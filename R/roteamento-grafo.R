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
#' @param sf_hidrovias sf (LINESTRING) — waterways, tipicamente de \code{\link{busca_hidrovias_osm}}.
#' @param crs_metrico Código EPSG métrico para cálculo de comprimento de aresta.
#'
#' @return Uma \code{sfnetwork} direcionada, subdividida nas interseções, com atributo de
#'   aresta `comprimento_m`.
#' @keywords internal
#' @noRd
.constroi_rede_hidroviaria <- function(sf_hidrovias, crs_metrico = 5880) {

  hid_proj <- sf::st_transform(sf_hidrovias, crs_metrico)
  hid_proj <- hid_proj[!sf::st_is_empty(hid_proj), ]

  net <- sfnetworks::as_sfnetwork(hid_proj, directed = TRUE)
  net <- tidygraph::convert(net, sfnetworks::to_spatial_subdivision, .clean = TRUE)

  net |>
    sfnetworks::activate("edges") |>
    dplyr::mutate(comprimento_m = as.numeric(sfnetworks::edge_length()))
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

  nodes_sf <- net_blend |> sfnetworks::activate("nodes") |> sf::st_as_sf()

  idx_nos <- sf::st_nearest_feature(pontos_sf, nodes_sf)

  dist_acesso_m <- as.numeric(
    sf::st_distance(pontos_sf, nodes_sf[idx_nos, ], by_element = TRUE)
  )

  list(net = net_blend, idx_nos = idx_nos, dist_acesso_m = dist_acesso_m)
}

#' Caminho mínimo por tempo entre dois nós de uma rede hidroviária, com fluxo direcionado
#'
#' @description
#' Constrói um grafo `igraph` direcionado e ponderado por tempo a partir da `sfnetwork`
#' (já com os pontos de interesse inseridos via \code{.insere_pontos_rede}) e calcula o
#' caminho mínimo por Dijkstra — o mesmo motor (`igraph::shortest_paths()`) já usado em
#' `.siorg_classifica_hierarquia()` (`R/siorg-hierarquia.R`) para o grafo organizacional,
#' mantendo consistência de padrão dentro do pacote.
#'
#' Quando `direcionar_fluxo = TRUE`, cada aresta original gera duas arestas dirigidas no
#' grafo: uma no sentido em que o `way` foi digitalizado no OSM (assumido jusante, por
#' convenção de mapeamento) com peso `comprimento / vel_jusante_kmh`, e uma no sentido
#' inverso (montante) com peso `comprimento / vel_montante_kmh`. Isso é o que permite que a
#' mesma rede produza tempos diferentes para origem->destino e destino->origem.
#'
#' @param net sfnetwork já com os pontos de origem/destino inseridos.
#' @param no_origem,no_destino Índices de nó (na tabela de nós de `net`) de origem e destino.
#' @param vel_jusante_kmh,vel_montante_kmh Velocidades (km/h) a favor/contra a correnteza.
#' @param direcionar_fluxo Se `FALSE`, ignora `vel_montante_kmh` e usa `vel_jusante_kmh` nos
#'   dois sentidos (grafo efetivamente não-direcionado, igual ao comportamento do rascunho
#'   original).
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

  edges_sf <- net |> sfnetworks::activate("edges") |> sf::st_as_sf()
  nodes_sf <- net |> sfnetworks::activate("nodes") |> sf::st_as_sf()

  edges_df <- sf::st_drop_geometry(edges_sf)

  if (isTRUE(direcionar_fluxo)) {
    arestas <- rbind(
      data.frame(
        from = edges_df$from, to = edges_df$to,
        comprimento_m = edges_df$comprimento_m,
        peso_h = edges_df$comprimento_m / 1000 / vel_jusante_kmh,
        edge_idx = seq_len(nrow(edges_df)), sentido = "jusante"
      ),
      data.frame(
        from = edges_df$to, to = edges_df$from,
        comprimento_m = edges_df$comprimento_m,
        peso_h = edges_df$comprimento_m / 1000 / vel_montante_kmh,
        edge_idx = seq_len(nrow(edges_df)), sentido = "montante"
      )
    )
  } else {
    arestas <- rbind(
      data.frame(
        from = edges_df$from, to = edges_df$to,
        comprimento_m = edges_df$comprimento_m,
        peso_h = edges_df$comprimento_m / 1000 / vel_jusante_kmh,
        edge_idx = seq_len(nrow(edges_df)), sentido = "sem_direcao"
      ),
      data.frame(
        from = edges_df$to, to = edges_df$from,
        comprimento_m = edges_df$comprimento_m,
        peso_h = edges_df$comprimento_m / 1000 / vel_jusante_kmh,
        edge_idx = seq_len(nrow(edges_df)), sentido = "sem_direcao"
      )
    )
  }

  g <- igraph::graph_from_data_frame(
    arestas,
    directed = TRUE,
    vertices = data.frame(name = seq_len(nrow(nodes_sf)))
  )

  dist <- igraph::distances(
    g,
    v = as.character(no_origem),
    to = as.character(no_destino),
    weights = igraph::E(g)$peso_h,
    mode = "out"
  )[1, 1]

  if (!is.finite(dist)) {
    return(list(viavel = FALSE, tempo_h = NA_real_, distancia_km = NA_real_, geometry = NULL))
  }

  caminho <- igraph::shortest_paths(
    g,
    from = as.character(no_origem),
    to = as.character(no_destino),
    weights = igraph::E(g)$peso_h,
    mode = "out",
    output = "epath"
  )

  edge_ids <- as.integer(caminho$epath[[1]])

  if (length(edge_ids) == 0) {
    return(list(viavel = TRUE, tempo_h = dist, distancia_km = 0, geometry = sf::st_sfc(crs = sf::st_crs(edges_sf))))
  }

  trecho <- arestas[edge_ids, ]

  geom <- sf::st_combine(sf::st_geometry(edges_sf)[trecho$edge_idx])

  list(
    viavel = TRUE,
    tempo_h = sum(trecho$peso_h),
    distancia_km = sum(trecho$comprimento_m) / 1000,
    geometry = sf::st_sfc(geom, crs = sf::st_crs(edges_sf))
  )
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
