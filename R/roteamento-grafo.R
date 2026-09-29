#' Constrói a rede navegável a partir de linhas de hidrovia (nodagem preservando direção)
#'
#' @description
#' Converte as linhas de hidrovia (tipicamente o retorno de \code{\link{busca_hidrovias_osm}})
#' em uma \pkg{sfnetworks} nodada: cada interseção entre `ways` vira um nó compartilhado,
#' preservando a ordem original dos vértices de cada `way` (necessária para inferir sentido de
#' fluxo — ver \code{\link{.rota_hidroviaria}}). Substitui o `sf::st_union() + sf::st_node()`
#' do rascunho original, que descartava os atributos (`osm_id`, direção) ao unir todas as
#' geometrias numa única `MULTILINESTRING` antes de nodar.
#'
#' Antes de montar a rede, as junções são consertadas por \code{.conecta_juncoes()} —
#' sem isso, costuras entre tiles do cache e afluentes que no OSM terminam a poucos metros
#' do rio principal viram redes desconectadas.
#'
#' @param sf_hidrovias sf (LINESTRING) — waterways, tipicamente de \code{\link{busca_hidrovias_osm}}.
#' @param crs_metrico Código EPSG métrico para cálculo de comprimento de aresta.
#' @param tolerancia_juncao_m Ver \code{.conecta_juncoes()}.
#'
#' @return Uma \code{sfnetwork} direcionada, subdividida nas interseções, com atributo de
#'   aresta `comprimento_m`.
#' @keywords internal
#' @noRd
.constroi_rede_hidroviaria <- function(sf_hidrovias, crs_metrico = 5880, tolerancia_juncao_m = 50) {

  hid_proj <- sf::st_transform(sf_hidrovias, crs_metrico)
  hid_proj <- hid_proj[!sf::st_is_empty(hid_proj), ]
  hid_proj <- .conecta_juncoes(hid_proj, tolerancia_juncao_m)

  net <- sfnetworks::as_sfnetwork(hid_proj, directed = TRUE)
  net <- tidygraph::convert(net, sfnetworks::to_spatial_subdivision, .clean = TRUE)

  net |>
    sfnetworks::activate("edges") |>
    dplyr::mutate(comprimento_m = as.numeric(sfnetworks::edge_length()))
}

#' Conserta junções quase-conectadas entre linhas de hidrovia
#'
#' @description
#' A nodagem de \code{.constroi_rede_hidroviaria()} só liga duas linhas quando elas
#' compartilham um vértice com coordenadas idênticas. Na prática isso falha em dois casos:
#' \itemize{
#'   \item \strong{Costura entre tiles}: o cache de \code{\link{busca_hidrovias_osm}} recorta
#'     cada via na borda do tile, e os dois pedaços podem terminar em coordenadas que diferem
#'     por arredondamento.
#'   \item \strong{Afluente solto}: no OSM, é comum um afluente terminar a alguns metros do
#'     rio principal (ou encostar no meio de um segmento dele sem vértice compartilhado).
#' }
#' Em dois passos, preservando a ordem dos vértices (e portanto o sentido de digitalização
#' usado para jusante/montante):
#' \enumerate{
#'   \item Extremidades de linhas a até `tolerancia_m` umas das outras são unificadas numa
#'     só coordenada.
#'   \item Cada extremidade que continua sem outra extremidade por perto e fica a até
#'     `tolerancia_m` do interior de outra linha é estendida até o ponto mais próximo dessa
#'     linha, e esse ponto é inserido como vértice nela (\code{sf::st_snap()}), para que a
#'     subdivisão da rede crie o nó de junção.
#' }
#'
#' @param linhas sf (LINESTRING) em CRS métrico.
#' @param tolerancia_m Distância máxima (m) para considerar duas linhas como conectadas.
#'   `0` desliga o conserto.
#'
#' @return `linhas` com as geometrias ajustadas (mesmas linhas e atributos).
#' @keywords internal
#' @noRd
.conecta_juncoes <- function(linhas, tolerancia_m) {

  if (tolerancia_m <= 0 || nrow(linhas) < 2) return(linhas)

  crs <- sf::st_crs(linhas)
  coords <- lapply(sf::st_geometry(linhas), function(g) unclass(g)[, 1:2, drop = FALSE])
  n <- length(coords)

  # Extremidades: 2 por linha (inicio, fim), na ordem linha1-inicio, linha1-fim, linha2-inicio...
  ext_linha <- rep(seq_len(n), each = 2)
  ext_fim <- rep(c(FALSE, TRUE), n)
  ext_xy <- t(vapply(seq_along(ext_linha), function(k) {
    m <- coords[[ext_linha[k]]]
    if (ext_fim[k]) m[nrow(m), ] else m[1, ]
  }, numeric(2)))
  ext_sf <- sf::st_sfc(lapply(seq_len(nrow(ext_xy)), function(k) sf::st_point(ext_xy[k, ])), crs = crs)

  # Passo 1: unifica extremidades próximas (componentes conexas do grafo "a até tolerancia_m").
  vizinhos <- sf::st_is_within_distance(ext_sf, ext_sf, dist = tolerancia_m)
  grupo <- igraph::components(igraph::graph_from_adj_list(unclass(vizinhos), mode = "all"))$membership
  representante <- match(grupo, grupo)
  ext_xy <- ext_xy[representante, , drop = FALSE]
  isolada <- tabulate(grupo)[grupo] == 1

  # Passo 2: extremidades isoladas perto do interior de outra linha.
  pontos_juncao <- list()
  linhas_alvo <- integer()
  if (any(isolada)) {
    geoms <- sf::st_geometry(linhas)
    candidatas <- sf::st_is_within_distance(ext_sf[isolada], geoms, dist = tolerancia_m)
    idx_isoladas <- which(isolada)
    for (j in seq_along(idx_isoladas)) {
      k <- idx_isoladas[j]
      alvos <- setdiff(candidatas[[j]], ext_linha[k])
      if (length(alvos) == 0) next
      ponto <- ext_sf[k]
      alvo <- alvos[which.min(as.numeric(sf::st_distance(ponto, geoms[alvos])))]
      p_xy <- sf::st_coordinates(sf::st_cast(sf::st_nearest_points(ponto, geoms[alvo]), "POINT"))[2, 1:2]
      ext_xy[k, ] <- p_xy
      pontos_juncao[[length(pontos_juncao) + 1]] <- sf::st_point(p_xy)
      linhas_alvo <- c(linhas_alvo, alvo)
    }
  }

  # Reescreve as extremidades de cada linha (estendendo quando a nova extremidade não
  # coincide com a original, para não deformar o último segmento).
  novas <- lapply(seq_len(n), function(i) {
    m <- coords[[i]]
    ini <- ext_xy[2 * i - 1, ]
    fim <- ext_xy[2 * i, ]
    if (!isTRUE(all.equal(ini, m[1, ], check.attributes = FALSE))) m <- rbind(ini, m)
    if (!isTRUE(all.equal(fim, m[nrow(m), ], check.attributes = FALSE))) m <- rbind(m, fim)
    sf::st_linestring(unname(m))
  })
  novas <- sf::st_sfc(novas, crs = crs)

  if (length(linhas_alvo) > 0) {
    linhas_alvo <- unique(linhas_alvo)
    alvo_pts <- sf::st_combine(sf::st_sfc(pontos_juncao, crs = crs))
    novas[linhas_alvo] <- sf::st_snap(novas[linhas_alvo], alvo_pts, tolerance = 1e-3)
  }

  sf::st_geometry(linhas) <- novas
  linhas
}

#' Insere pontos arbitrários como nós na rede (particionando a aresta mais próxima)
#'
#' @description
#' Encaixa `pontos_sf` na rede via \code{sfnetworks::st_network_blend()} — que já resolve
#' a projeção do ponto na aresta mais próxima e a partição da aresta em duas, o mesmo que o
#' rascunho original fazia manualmente com `st_nearest_feature()` + `st_nearest_points()`
#' (linhas 149-219 do arquivo antigo). Devolve, para cada ponto de entrada, o nó resultante e
#' a distância de acesso (a "última milha" a pé até a rede) — sem aplicar nenhum corte de
#' distância máxima aqui; isso é responsabilidade de quem chama (ver `max_caminhada_km` em
#' \code{\link{rotear_multimodal}}), para que a distância real fique sempre disponível para
#' diagnóstico mesmo quando o candidato acaba sendo descartado.
#'
#' @param net sfnetwork (retorno de \code{.constroi_rede_hidroviaria}).
#' @param pontos_sf sf (POINT), no mesmo CRS métrico de `net`.
#' @param tolerance_m Distância máxima (m) de busca para encaixe — deve ser generosa (maior
#'   que qualquer `max_caminhada_km` plausível) para não mascarar um ponto genuinamente longe
#'   da rede como "sem rede próxima" antes mesmo de medir a distância real.
#'
#' @return `list(net, idx_nos, dist_acesso_m)` — rede atualizada, índice do nó de cada ponto
#'   nessa rede atualizada e a distância (m) entre o ponto original e o nó de encaixe.
#' @keywords internal
#' @noRd
.insere_pontos_rede <- function(net, pontos_sf, tolerance_m) {

  net_blend <- sfnetworks::st_network_blend(net, pontos_sf, tolerance = tolerance_m)

  # st_network_blend() parte a aresta em duas mas copia os atributos da original para as
  # duas metades — sem recalcular, cada metade herdaria o comprimento da aresta inteira.
  net_blend <- net_blend |>
    sfnetworks::activate("edges") |>
    dplyr::mutate(comprimento_m = as.numeric(sfnetworks::edge_length()))

  nodes_sf <- net_blend |> sfnetworks::activate("nodes") |> sf::st_as_sf()

  idx_nos <- sf::st_nearest_feature(pontos_sf, nodes_sf)

  dist_acesso_m <- as.numeric(
    sf::st_distance(pontos_sf, nodes_sf[idx_nos, ], by_element = TRUE)
  )

  list(net = net_blend, idx_nos = idx_nos, dist_acesso_m = dist_acesso_m)
}

#' Opções de acesso a pé de cada ponto a cada componente conexa da rede ao alcance
#'
#' @description
#' Encaixar o ponto só no rio mais próximo falha quando esse rio é um fragmento isolado da
#' rede (comum no OSM da Amazônia: igarapé que termina num lago sem linha de centro, trecho
#' sem ligação mapeada) — o ponto fica preso num pedaço que não leva a lugar nenhum, mesmo
#' com o rio principal a uma caminhada curta. Aqui cada ponto ganha uma opção de acesso por
#' componente conexa da rede que tenha alguma aresta a até `max_dist_m`: o ponto mais
#' próximo dessa componente, inserido como nó (\code{.insere_pontos_rede}). Quem roteia
#' escolhe, por par, a melhor combinação entre componentes comuns à origem e ao destino.
#'
#' @param net sfnetwork (retorno de \code{.constroi_rede_hidroviaria}).
#' @param pontos_m sf (POINT) no mesmo CRS métrico de `net`.
#' @param max_dist_m Distância máxima (m) de caminhada até a rede.
#'
#' @return `list(net, opcoes, dist_min_km, geom_nos)`: a rede com os nós de acesso
#'   inseridos; `opcoes` (data.frame com `ponto`, `componente`, `no`, `acesso_km`, uma linha
#'   por ponto x componente ao alcance); a distância (km) de cada ponto à rede mais próxima
#'   (para diagnóstico quando não há opção); e as geometrias dos nós da rede final.
#' @keywords internal
#' @noRd
.acessos_por_componente <- function(net, pontos_m, max_dist_m) {

  edges <- net |> sfnetworks::activate("edges") |> sf::st_as_sf()
  componente_no <- igraph::components(net, mode = "weak")$membership
  componente_aresta <- componente_no[edges$from]
  geom_arestas <- sf::st_geometry(edges)
  geom_pontos <- sf::st_geometry(pontos_m)

  vizinhas <- sf::st_is_within_distance(geom_pontos, geom_arestas, dist = max_dist_m)

  opcoes <- list()
  projecoes <- list()
  for (k in seq_along(geom_pontos)) {
    cand <- vizinhas[[k]]
    if (length(cand) == 0) next
    d <- as.numeric(sf::st_distance(geom_pontos[k], geom_arestas[cand]))
    ordem <- order(d)
    cand <- cand[ordem]
    d <- d[ordem]
    primeira <- !duplicated(componente_aresta[cand])
    for (j in which(primeira)) {
      proj <- sf::st_cast(sf::st_nearest_points(geom_pontos[k], geom_arestas[cand[j]]), "POINT")[2]
      projecoes[[length(projecoes) + 1]] <- proj[[1]]
      opcoes[[length(opcoes) + 1]] <- data.frame(
        ponto = k, componente = componente_aresta[cand[j]], acesso_km = d[j] / 1000
      )
    }
  }

  dist_min_km <- as.numeric(sf::st_distance(
    geom_pontos, geom_arestas[sf::st_nearest_feature(geom_pontos, geom_arestas)], by_element = TRUE
  )) / 1000

  if (length(opcoes) == 0) {
    return(list(
      net = net,
      opcoes = data.frame(ponto = integer(), componente = integer(), no = integer(), acesso_km = numeric()),
      dist_min_km = dist_min_km,
      geom_nos = sf::st_geometry(net |> sfnetworks::activate("nodes") |> sf::st_as_sf())
    ))
  }

  opcoes <- do.call(rbind, opcoes)
  pontos_acesso <- sf::st_sf(geometry = sf::st_sfc(projecoes, crs = sf::st_crs(pontos_m)))

  # Os pontos de acesso já estão sobre as arestas: tolerância mínima só para absorver
  # arredondamento da projeção.
  blend <- .insere_pontos_rede(net, pontos_acesso, tolerance_m = 1)
  opcoes$no <- blend$idx_nos

  list(
    net = blend$net,
    opcoes = opcoes,
    dist_min_km = dist_min_km,
    geom_nos = sf::st_geometry(blend$net |> sfnetworks::activate("nodes") |> sf::st_as_sf())
  )
}

#' Grafo `igraph` ponderado por tempo a partir de uma rede hidroviária, com fluxo direcionado
#'
#' @description
#' Constrói uma vez o grafo `igraph` direcionado e ponderado por tempo a partir da
#' `sfnetwork` (já com os pontos de interesse inseridos via \code{.insere_pontos_rede}),
#' para ser reaproveitado por várias consultas de caminho mínimo
#' (\code{.caminhos_hidroviarios}) — no roteamento em lote, o mesmo grafo atende todos os
#' pares de uma região.
#'
#' Quando `direcionar_fluxo = TRUE`, cada aresta original gera duas arestas dirigidas no
#' grafo: uma no sentido em que o `way` foi digitalizado no OSM (assumido jusante, por
#' convenção de mapeamento) com peso `comprimento / vel_jusante_kmh`, e uma no sentido
#' inverso (montante) com peso `comprimento / vel_montante_kmh`. Isso é o que permite que a
#' mesma rede produza tempos diferentes para origem->destino e destino->origem.
#'
#' @param net sfnetwork já com os pontos de origem/destino inseridos.
#' @param vel_jusante_kmh,vel_montante_kmh Velocidades (km/h) a favor/contra a correnteza.
#' @param direcionar_fluxo Se `FALSE`, ignora `vel_montante_kmh` e usa `vel_jusante_kmh` nos
#'   dois sentidos (grafo efetivamente não-direcionado, igual ao comportamento do rascunho
#'   original).
#'
#' @return `list(g, arestas, geometrias)`: o grafo, a tabela de arestas dirigidas (na ordem
#'   das arestas de `g`) e as geometrias das arestas originais da rede.
#' @keywords internal
#' @noRd
.grafo_hidroviario <- function(
    net,
    vel_jusante_kmh = 20,
    vel_montante_kmh = 12,
    direcionar_fluxo = TRUE
) {

  edges_sf <- net |> sfnetworks::activate("edges") |> sf::st_as_sf()
  n_nos <- igraph::vcount(net)

  edges_df <- sf::st_drop_geometry(edges_sf)

  vel_inversa <- if (isTRUE(direcionar_fluxo)) vel_montante_kmh else vel_jusante_kmh
  sentidos <- if (isTRUE(direcionar_fluxo)) c("jusante", "montante") else c("sem_direcao", "sem_direcao")

  arestas <- rbind(
    data.frame(
      from = edges_df$from, to = edges_df$to,
      comprimento_m = edges_df$comprimento_m,
      peso_h = edges_df$comprimento_m / 1000 / vel_jusante_kmh,
      edge_idx = seq_len(nrow(edges_df)), sentido = sentidos[1]
    ),
    data.frame(
      from = edges_df$to, to = edges_df$from,
      comprimento_m = edges_df$comprimento_m,
      peso_h = edges_df$comprimento_m / 1000 / vel_inversa,
      edge_idx = seq_len(nrow(edges_df)), sentido = sentidos[2]
    )
  )

  g <- igraph::graph_from_data_frame(
    arestas,
    directed = TRUE,
    vertices = data.frame(name = seq_len(n_nos))
  )

  list(g = g, arestas = arestas, geometrias = sf::st_geometry(edges_sf))
}

#' Caminhos mínimos por tempo de um nó de origem para vários nós de destino
#'
#' @description
#' Um único Dijkstra (`igraph::shortest_paths()`, o mesmo motor já usado em
#' `.siorg_classifica_hierarquia()`) resolve todos os destinos de uma origem: o caminho sai
#' pronto e o tempo é a soma dos pesos. Destino inalcançável vira caminho vazio (com aviso
#' do igraph, silenciado aqui) e é devolvido como `viavel = FALSE`.
#'
#' @param grafo Retorno de \code{.grafo_hidroviario()}.
#' @param no_origem Índice do nó de origem.
#' @param nos_destino Índices dos nós de destino.
#'
#' @return Lista com um elemento por `nos_destino`, cada um
#'   `list(viavel, tempo_h, distancia_km, geometry)`.
#' @keywords internal
#' @noRd
.caminhos_hidroviarios <- function(grafo, no_origem, nos_destino) {

  crs <- sf::st_crs(grafo$geometrias)
  destinos_unicos <- unique(nos_destino)

  caminhos <- suppressWarnings(igraph::shortest_paths(
    grafo$g,
    from = as.character(no_origem),
    to = as.character(destinos_unicos),
    weights = igraph::E(grafo$g)$peso_h,
    mode = "out",
    output = "epath"
  ))$epath

  resultado <- lapply(seq_along(destinos_unicos), function(j) {
    edge_ids <- as.integer(caminhos[[j]])

    if (length(edge_ids) == 0) {
      if (no_origem != destinos_unicos[j]) {
        return(list(viavel = FALSE, tempo_h = NA_real_, distancia_km = NA_real_, geometry = NULL))
      }
      return(list(
        viavel = TRUE, tempo_h = 0, distancia_km = 0,
        geometry = sf::st_sfc(sf::st_linestring(), crs = crs)
      ))
    }

    trecho <- grafo$arestas[edge_ids, ]

    list(
      viavel = TRUE,
      tempo_h = sum(trecho$peso_h),
      distancia_km = sum(trecho$comprimento_m) / 1000,
      geometry = sf::st_sfc(sf::st_combine(grafo$geometrias[trecho$edge_idx]), crs = crs)
    )
  })

  resultado[match(nos_destino, destinos_unicos)]
}

#' Caminho mínimo por tempo entre dois nós de uma rede hidroviária, com fluxo direcionado
#'
#' Atalho de \code{.grafo_hidroviario()} + \code{.caminhos_hidroviarios()} para um único par.
#'
#' @return `list(viavel, tempo_h, distancia_km, geometry)`. `viavel = FALSE` quando os dois
#'   nós não estão no mesmo componente conectado da rede.
#' @keywords internal
#' @noRd
.rota_hidroviaria <- function(
    net,
    no_origem,
    no_destino,
    vel_jusante_kmh = 20,
    vel_montante_kmh = 12,
    direcionar_fluxo = TRUE
) {
  grafo <- .grafo_hidroviario(net, vel_jusante_kmh, vel_montante_kmh, direcionar_fluxo)
  .caminhos_hidroviarios(grafo, no_origem, no_destino)[[1]]
}

#' Valida (e opcionalmente corrige) o sentido de digitalização de `ways` por elevação
#'
#' @description
#' A convenção de digitalização do OSM (mapeadores desenham `waterway=river`/`stream` no
#' sentido nascente -> foz) é a base usada por \code{\link{.rota_hidroviaria}} para atribuir
#' jusante/montante, mas nem sempre é seguida à risca. Quando um modelo digital de elevação
#' (MDE) está disponível, esta função verifica se a elevação no primeiro vértice de cada `way`
#' é maior que no último (água desce); `ways` onde isso não vale são sinalizados como
#' provavelmente invertidos.
#'
#' É uma validação opcional (custo extra de amostragem de raster) — sem `mde`, o pacote confia
#' apenas na convenção de digitalização.
#'
#' @param sf_hidrovias sf (LINESTRING) no mesmo CRS de `mde`.
#' @param mde Um `terra::SpatRaster` de elevação cobrindo a área de `sf_hidrovias`.
#'
#' @return `sf_hidrovias` com uma coluna lógica adicional `fluxo_invertido` (`TRUE` quando a
#'   elevação sugere que o `way` foi digitalizado de jusante para montante).
#' @keywords internal
#' @noRd
.valida_direcao_fluxo_mde <- function(sf_hidrovias, mde) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("O pacote 'terra' e necessario para validar o sentido de fluxo por elevacao.", call. = FALSE)
  }

  primeiro_vertice <- sf::st_line_sample(sf_hidrovias, sample = 0)
  ultimo_vertice <- sf::st_line_sample(sf_hidrovias, sample = 1)

  elev_inicio <- terra::extract(mde, terra::vect(sf::st_transform(primeiro_vertice, terra::crs(mde))))[, 2]
  elev_fim <- terra::extract(mde, terra::vect(sf::st_transform(ultimo_vertice, terra::crs(mde))))[, 2]

  sf_hidrovias$fluxo_invertido <- !is.na(elev_inicio) & !is.na(elev_fim) & (elev_fim > elev_inicio)

  sf_hidrovias
}
