#' Padroniza N pontos de entrada (`sf`/`sfc` POINT, `matrix`/`data.frame` 2 colunas, ou
#' `c(lon, lat)`) como sf
#' @keywords internal
#' @noRd
.padroniza_pontos <- function(p) {

  if (inherits(p, "sf")) {
    if (is.na(sf::st_crs(p))) sf::st_crs(p) <- 4326
    return(sf::st_sf(geometry = sf::st_geometry(p)))
  }

  if (inherits(p, "sfc")) {
    if (is.na(sf::st_crs(p))) sf::st_crs(p) <- 4326
    return(sf::st_sf(geometry = p))
  }

  if (is.numeric(p) && length(p) == 2) {
    return(sf::st_sf(geometry = sf::st_sfc(sf::st_point(p), crs = 4326)))
  }

  if ((is.matrix(p) || is.data.frame(p)) && ncol(p) == 2) {
    pontos <- lapply(seq_len(nrow(p)), function(i) sf::st_point(as.numeric(p[i, ])))
    return(sf::st_sf(geometry = sf::st_sfc(pontos, crs = 4326)))
  }

  stop(
    "`origem`/`destino` devem ser um objeto sf/sfc POINT, matrix/data.frame com 2 colunas ",
    "(lon, lat), ou c(lon, lat).",
    call. = FALSE
  )
}

#' Padroniza um único ponto de entrada (`sf`/`sfc` POINT ou `c(lon, lat)`) como sf de 1 feature
#' @keywords internal
#' @noRd
.padroniza_ponto <- function(p) {
  out <- .padroniza_pontos(p)
  if (nrow(out) != 1) stop("`origem`/`destino` devem ter exatamente 1 feature.", call. = FALSE)
  out
}

#' Candidata "rodovia": roteamento via OSRM, com crítica ao snap do OSRM
#'
#' @description
#' O OSRM sempre encaixa (*snap*) o ponto pedido no nó mais próximo da malha rodoviária que
#' ele conhece, mesmo que esse nó esteja muito longe do ponto real — típico de localidade
#' isolada sem acesso rodoviário. Uma rota "encontrada" nessas condições pode ter uma razão
#' de desvio (distância da rota / distância em linha reta) aparentemente normal e ainda assim
#' não representar o trajeto real: ela simplesmente ignora o trecho entre o ponto pedido e o
#' nó da malha, que pode ser intransponível (ex. atravessa um rio sem ponte). Por isso a
#' distância de snap (`osrm::osrmNearest()$distance`, já em metros) é checada **antes** de
#' sequer chamar `osrmRoute()`, não apenas registrada depois.
#'
#' @keywords internal
#' @noRd
#' @importFrom sf st_sf st_geometry
.candidata_rodovia <- function(
    origem, destino, osrm_server, osrm_profile,
    max_snap_osrm_km, max_ratio_desvio, dist_linha_reta_km, verbose
) {

  snap_o <- osrm::osrmNearest(origem, osrm.server = osrm_server, osrm.profile = osrm_profile)
  snap_d <- osrm::osrmNearest(destino, osrm.server = osrm_server, osrm.profile = osrm_profile)

  snap_o_km <- snap_o$distance[1] / 1000
  snap_d_km <- snap_d$distance[1] / 1000

  if (snap_o_km > max_snap_osrm_km || snap_d_km > max_snap_osrm_km) {
    if (verbose) {
      message(
        "Candidata rodovia descartada: encaixe OSRM muito distante (origem ",
        round(snap_o_km, 1), " km, destino ", round(snap_d_km, 1), " km; limite ",
        max_snap_osrm_km, " km)."
      )
    }
    return(NULL)
  }

  rota <- osrm::osrmRoute(src = origem, dst = destino, osrm.server = osrm_server, osrm.profile = osrm_profile)

  if (is.null(rota) || nrow(rota) == 0) return(NULL)

  distancia_km <- rota$distance[1]
  tempo_h <- rota$duration[1] / 60

  razao_desvio <- if (dist_linha_reta_km > 0) distancia_km / dist_linha_reta_km else 1

  if (razao_desvio > max_ratio_desvio) {
    if (verbose) {
      message(
        "Candidata rodovia descartada: razao de desvio ", round(razao_desvio, 1),
        "x excede max_ratio_desvio (", max_ratio_desvio, "x)."
      )
    }
    return(NULL)
  }

  list(
    resumo = data.frame(modal = "rodovia", distancia_km = distancia_km, tempo_h = tempo_h),
    rota = sf::st_sf(modal = "rodovia", geometry = sf::st_geometry(rota)),
    tempo_total_h = tempo_h,
    distancia_total_km = distancia_km,
    confiabilidade = list(
      modal = "rodovia",
      snap_osrm_origem_km = snap_o_km,
      snap_osrm_destino_km = snap_d_km,
      razao_desvio = razao_desvio
    )
  )
}

#' Candidata "hidrovia + caminhada": Overpass -> grafo direcionado -> Dijkstra
#' @keywords internal
#' @noRd
#' @importFrom sf st_transform st_bbox st_sf st_sfc st_linestring st_coordinates st_crs
.candidata_hidrovia <- function(
    origem, destino, tipos_hidrovia, bbox_buffer_km, crs_metrico,
    vel_jusante_kmh, vel_montante_kmh, vel_caminhada_kmh,
    max_caminhada_km, direcionar_fluxo,
    cache_dir, cache_max_age_dias, verbose
) {

  pontos_4326 <- rbind(sf::st_transform(origem, 4326), sf::st_transform(destino, 4326))

  bbox_pontos <- sf::st_bbox(pontos_4326)
  buffer_deg <- bbox_buffer_km / 111

  bbox_busca <- c(
    bbox_pontos[["xmin"]] - buffer_deg, bbox_pontos[["ymin"]] - buffer_deg,
    bbox_pontos[["xmax"]] + buffer_deg, bbox_pontos[["ymax"]] + buffer_deg
  )

  sf_hid <- busca_hidrovias_osm(
    bbox_busca,
    tipos = tipos_hidrovia,
    cache_dir = cache_dir,
    cache_max_age_dias = cache_max_age_dias,
    verbose = verbose
  )

  if (is.null(sf_hid) || nrow(sf_hid) == 0) {
    if (verbose) message("Nenhuma waterway encontrada na area de busca.")
    return(NULL)
  }

  net <- .constroi_rede_hidroviaria(sf_hid, crs_metrico = crs_metrico)

  pontos_m <- sf::st_transform(pontos_4326, crs_metrico)

  # tolerancia de encaixe generosa: sempre medir a distancia real de acesso e so entao
  # decidir se ela viola max_caminhada_km, em vez de o encaixe falhar silenciosamente antes.
  tolerance_m <- max_caminhada_km * 1000 * 3

  blend <- .insere_pontos_rede(net, pontos_m, tolerance_m = tolerance_m)

  dist_acesso_origem_km <- blend$dist_acesso_m[1] / 1000
  dist_acesso_destino_km <- blend$dist_acesso_m[2] / 1000

  if (dist_acesso_origem_km > max_caminhada_km || dist_acesso_destino_km > max_caminhada_km) {
    if (verbose) {
      message(
        "Candidata hidrovia descartada: caminhada de acesso excede max_caminhada_km ",
        "(origem ", round(dist_acesso_origem_km, 1), " km, destino ",
        round(dist_acesso_destino_km, 1), " km; limite ", max_caminhada_km, " km)."
      )
    }
    return(NULL)
  }

  rota_h <- .rota_hidroviaria(
    blend$net, blend$idx_nos[1], blend$idx_nos[2],
    vel_jusante_kmh = vel_jusante_kmh,
    vel_montante_kmh = vel_montante_kmh,
    direcionar_fluxo = direcionar_fluxo
  )

  if (!rota_h$viavel) {
    if (verbose) {
      message("Candidata hidrovia descartada: origem e destino nao estao conectados pela rede de waterways encontrada.")
    }
    return(NULL)
  }

  nodes_sf <- blend$net |> sfnetworks::activate("nodes") |> sf::st_as_sf()

  linha_acesso <- sf::st_sfc(
    sf::st_linestring(rbind(
      sf::st_coordinates(pontos_m[1, ]),
      sf::st_coordinates(sf::st_geometry(nodes_sf)[[blend$idx_nos[1]]])
    )),
    crs = crs_metrico
  )

  linha_desembarque <- sf::st_sfc(
    sf::st_linestring(rbind(
      sf::st_coordinates(sf::st_geometry(nodes_sf)[[blend$idx_nos[2]]]),
      sf::st_coordinates(pontos_m[2, ])
    )),
    crs = crs_metrico
  )

  tempo_acesso_h <- dist_acesso_origem_km / vel_caminhada_kmh
  tempo_desembarque_h <- dist_acesso_destino_km / vel_caminhada_kmh

  resumo <- data.frame(
    modal = c("caminhada_acesso", "hidrovia", "caminhada_desembarque"),
    distancia_km = c(dist_acesso_origem_km, rota_h$distancia_km, dist_acesso_destino_km),
    tempo_h = c(tempo_acesso_h, rota_h$tempo_h, tempo_desembarque_h)
  )

  rota_sf <- sf::st_sf(
    modal = c("caminhada_acesso", "hidrovia", "caminhada_desembarque"),
    geometry = sf::st_sfc(
      list(linha_acesso[[1]], rota_h$geometry[[1]], linha_desembarque[[1]]),
      crs = crs_metrico
    )
  )
  rota_sf <- sf::st_transform(rota_sf, 4326)

  list(
    resumo = resumo,
    rota = rota_sf,
    tempo_total_h = sum(resumo$tempo_h),
    distancia_total_km = sum(resumo$distancia_km),
    confiabilidade = list(
      modal = "hidrovia",
      caminhada_acesso_km = dist_acesso_origem_km,
      caminhada_desembarque_km = dist_acesso_destino_km,
      n_waterways_na_area = nrow(sf_hid)
    )
  )
}

#' Roteamento ótimo multimodal entre dois pontos (rodovia via OSRM, hidrovia via OSM/Overpass)
#'
#' @description
#' Encontra a rota mais rápida entre `origem` e `destino` comparando duas candidatas
#' genéricas — não específicas de nenhum órgão ou recorte territorial:
#' \itemize{
#'   \item \strong{rodovia}: roteamento direto via API OSRM (\pkg{osrm}), com crítica ao
#'     comportamento de *snap* do OSRM (ver \code{max_snap_osrm_km} abaixo) e a um limite de
#'     razão de desvio (\code{max_ratio_desvio});
#'   \item \strong{hidrovia + caminhada}: waterways do OpenStreetMap (via
#'     \code{\link{busca_hidrovias_osm}}, com cache local em DuckDB) montadas em um grafo
#'     direcionado (jusante mais rápido que montante, por convenção de digitalização do OSM —
#'     ver \code{vel_hidrovia_jusante_kmh}/\code{vel_hidrovia_montante_kmh}), com trechos de
#'     caminhada de última milha entre os pontos pedidos e a rede navegável.
#' }
#' Foi desenhada para aproximar o roteamento até localidades isoladas da Amazônia, onde
#' nenhuma das duas fontes isoladamente é suficiente — mas os dois pontos de entrada podem ser
#' quaisquer dois pontos, em qualquer lugar.
#'
#' @param origem,destino Objeto `sf`/`sfc` POINT (1 feature) ou vetor `c(lon, lat)`.
#' @param osrm_server URL do servidor OSRM. Padrão: servidor público de demonstração — use
#'   apenas para prototipagem; para uso em lote (muitas chamadas), aponte para uma instância
#'   própria, já que o servidor público tem limite de requisições e seus termos de uso
#'   desaconselham uso em lote/produção.
#' @param osrm_profile Perfil de roteamento do OSRM (ex. `"driving"`, `"foot"` — depende dos
#'   perfis disponíveis no `osrm_server` usado).
#' @param vel_hidrovia_jusante_kmh,vel_hidrovia_montante_kmh Velocidade (km/h) de embarcação a
#'   favor/contra a correnteza, usada como peso das arestas do grafo hidroviário.
#' @param vel_caminhada_kmh Velocidade (km/h) de caminhada para os trechos de última milha.
#' @param max_caminhada_km Distância máxima (km) de caminhada de última milha aceitável; a
#'   candidata hidrovia é descartada se a origem ou o destino estiverem mais longe da rede
#'   navegável do que isso.
#' @param max_ratio_desvio Razão máxima aceitável entre a distância da rota rodoviária e a
#'   distância em linha reta entre origem e destino; acima disso, a candidata rodovia é
#'   descartada por implausibilidade.
#' @param max_snap_osrm_km Distância máxima (km) de encaixe (*snap*) do OSRM aceitável para
#'   origem/destino na malha rodoviária conhecida. Ver `@details`/`@description` acima — este
#'   é o principal resguardo contra rotas OSRM enganosas para localidades isoladas.
#' @param direcionar_fluxo Se `TRUE` (padrão), o grafo hidroviário é direcionado (jusante mais
#'   rápido que montante). Se `FALSE`, usa `vel_hidrovia_jusante_kmh` nos dois sentidos.
#' @param tipos_hidrovia Valores da tag OSM `waterway` a considerar navegáveis. Padrão:
#'   `c("river", "canal")`.
#' @param bbox_buffer_km Margem (km) ao redor do retângulo envolvente de origem/destino usada
#'   na busca de waterways no Overpass.
#' @param crs_metrico Código EPSG métrico usado internamente para cálculo de distâncias/grafo.
#' @param cache_dir Diretório do cache local (Overpass) em DuckDB.
#' @param cache_max_age_dias Idade máxima (dias) de dados em cache antes de rebuscar.
#' @param verbose Se `TRUE`, imprime mensagens de progresso e de descarte de candidatas.
#'
#' @return Uma lista com `resumo` (data.frame com `modal`/`distancia_km`/`tempo_h` por
#'   trecho), `rota` (sf LINESTRING, 1 linha por trecho), `tempo_total_h`,
#'   `distancia_total_km` e `confiabilidade` (lista com os indicadores de auditoria da
#'   candidata vencedora — ex. distâncias de snap do OSRM, mesmo quando a rota foi aceita).
#'   Retorna `NULL` (com aviso) se nenhuma candidata for viável.
#'
#' @export
#' @importFrom sf st_distance st_transform
rotear_multimodal <- function(
    origem,
    destino,
    osrm_server = "https://router.project-osrm.org/",
    osrm_profile = "driving",
    vel_hidrovia_jusante_kmh = 20,
    vel_hidrovia_montante_kmh = 12,
    vel_caminhada_kmh = 4.5,
    max_caminhada_km = 30,
    max_ratio_desvio = 3.0,
    max_snap_osrm_km = 5.0,
    direcionar_fluxo = TRUE,
    tipos_hidrovia = c("river", "canal"),
    bbox_buffer_km = 25,
    crs_metrico = 5880,
    cache_dir = tools::R_user_dir("indigenR", "cache"),
    cache_max_age_dias = 30,
    verbose = TRUE
) {

  origem <- .padroniza_ponto(origem)
  destino <- .padroniza_ponto(destino)

  dist_linha_reta_km <- as.numeric(
    sf::st_distance(sf::st_transform(origem, 4326), sf::st_transform(destino, 4326))
  ) / 1000

  candidata_rodovia <- tryCatch(
    .candidata_rodovia(
      origem, destino, osrm_server, osrm_profile,
      max_snap_osrm_km, max_ratio_desvio, dist_linha_reta_km, verbose
    ),
    error = function(e) {
      if (verbose) message("Candidata rodovia indisponivel: ", conditionMessage(e))
      NULL
    }
  )

  candidata_hidrovia <- tryCatch(
    .candidata_hidrovia(
      origem, destino, tipos_hidrovia, bbox_buffer_km, crs_metrico,
      vel_hidrovia_jusante_kmh, vel_hidrovia_montante_kmh, vel_caminhada_kmh,
      max_caminhada_km, direcionar_fluxo,
      cache_dir, cache_max_age_dias, verbose
    ),
    error = function(e) {
      if (verbose) message("Candidata hidrovia indisponivel: ", conditionMessage(e))
      NULL
    }
  )

  candidatas <- Filter(Negate(is.null), list(rodovia = candidata_rodovia, hidrovia = candidata_hidrovia))

  if (length(candidatas) == 0) {
    warning("Nenhuma rota viavel encontrada entre origem e destino.", call. = FALSE)
    return(NULL)
  }

  tempos <- vapply(candidatas, function(x) x$tempo_total_h, numeric(1))
  vencedora <- candidatas[[which.min(tempos)]]

  list(
    resumo = vencedora$resumo,
    rota = vencedora$rota,
    tempo_total_h = vencedora$tempo_total_h,
    distancia_total_km = vencedora$distancia_total_km,
    confiabilidade = vencedora$confiabilidade
  )
}

#' Triagem em lote de pares origem-destino via OSRM Table
#'
#' @description
#' Pré-filtro barato para lotes de roteamento: em vez de checar cada par origem-destino com
#' as chamadas não vetorizadas que `rotear_multimodal()` usa por trás (`osrm::osrmNearest()` e
#' `osrm::osrmRoute()`, uma requisição HTTP cada), consulta `osrm::osrmTable()` **uma única
#' vez** para o lote inteiro — a API de tabela do OSRM é genuinamente vetorizada — e aplica a
#' mesma crítica que `rotear_multimodal()` já usa para descartar uma rota rodoviária
#' implausível (distância de encaixe/*snap* e razão de desvio, ver `max_snap_osrm_km`/
#' `max_ratio_desvio`). O resultado marca, para cada par, se a rota rodoviária já é plausível
#' ou se o par é candidato ao roteamento multimodal completo (caro: `osrm::osrmRoute()` +
#' busca de hidrovia via Overpass/`sfnetworks`/`igraph`, uma chamada por par).
#'
#' Não chama `rotear_multimodal()` para os pares flagados — essa orquestração (rodar o
#' roteamento completo só para os candidatos, e obter a geometria da rota rodoviária via
#' `osrm::osrmRoute()` para os demais) fica para uma etapa seguinte.
#'
#' @param origens,destinos Objetos `sf`/`sfc` POINT, `matrix`/`data.frame` com 2 colunas
#'   (lon, lat), com o mesmo número de pares (1 linha/feature = 1 par origem-destino).
#' @param osrm_server,osrm_profile Ver `\link{rotear_multimodal}`.
#' @param max_snap_osrm_km,max_ratio_desvio Mesmos limiares/padrões usados pela candidata
#'   rodovia de `rotear_multimodal()` — ver `\link{rotear_multimodal}`.
#' @param max_pares Número máximo de pares por chamada. Origens e destinos são consultados
#'   juntos numa única chamada `osrm::osrmTable(loc = ...)` (não `src`/`dst` separados — alguns
#'   servidores, incluindo o público `router.project-osrm.org`, retornam uma matriz de zeros em
#'   vez de erro quando `src`/`dst` são passados como conjuntos separados), o que gera uma
#'   matriz `2n x 2n`, custo O(4n²); o servidor público de demonstração do OSRM não aceita mais
#'   de 10000 valores por requisição, daí o padrão de 50 pares (`(2*50)² = 10000`). Para lotes
#'   maiores, aponte `osrm_server` para uma instância própria e aumente `max_pares` de acordo
#'   com o `max-table-size` configurado nela.
#' @param verbose Se `TRUE` (padrão), exibe mensagens de progresso.
#'
#' @return Um `tibble`, 1 linha por par, com `par` (índice), `tempo_rodovia_h`,
#'   `distancia_rodovia_km` (estimativas do OSRM Table), `dist_linha_reta_km`, `razao_desvio`,
#'   `snap_origem_km`, `snap_destino_km` e `candidato_multimodal` (`TRUE` quando o par não
#'   passou na crítica — sem rota rodoviária, encaixe muito distante, ou desvio implausível —
#'   e portanto é candidato ao roteamento multimodal completo).
#'
#' @export
#' @importFrom sf st_transform st_coordinates st_distance st_geometry st_as_sf
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' triagem_rodovia_lote(
#'   origens = matrix(c(-60.0217, -3.1190, -60.1875, -3.2778), ncol = 2, byrow = TRUE),
#'   destinos = matrix(c(-58.4453, -3.1431, -60.6206, -3.2996), ncol = 2, byrow = TRUE)
#' )
#' }
triagem_rodovia_lote <- function(
    origens,
    destinos,
    osrm_server = "https://router.project-osrm.org/",
    osrm_profile = "driving",
    max_snap_osrm_km = 5.0,
    max_ratio_desvio = 3.0,
    max_pares = 50,
    verbose = TRUE
) {

  origens <- .padroniza_pontos(origens)
  destinos <- .padroniza_pontos(destinos)

  if (nrow(origens) != nrow(destinos)) {
    stop("`origens` e `destinos` devem ter o mesmo numero de pares.", call. = FALSE)
  }

  n <- nrow(origens)

  if (n > max_pares) {
    stop(
      "Lote de ", n, " pares excede max_pares (", max_pares, "). Isso geraria uma tabela OSRM ",
      2 * n, "x", 2 * n, " (", 4 * n * n, " valores) - acima do limite pratico do servidor ",
      "publico OSRM (10000 valores). Reduza o lote ou aponte 'osrm_server' para uma instancia ",
      "propria.",
      call. = FALSE
    )
  }

  origens_4326 <- sf::st_transform(origens, 4326)
  destinos_4326 <- sf::st_transform(destinos, 4326)

  if (verbose) message("Consultando osrmTable() para ", n, " pares...")

  # Consulta origens e destinos juntos num unico 'loc' (nao 'src'/'dst' separados): alguns
  # servidores OSRM, incluindo o publico router.project-osrm.org (o padrao de
  # rotear_multimodal()), respondem com uma matriz de zeros em vez de erro quando src/dst sao
  # passados como conjuntos separados - confirmado empiricamente. 'loc' unico e' suportado por
  # qualquer servidor com o servico de tabela habilitado, ao custo de uma matriz (2n)x(2n) em
  # vez de nxn (ver guarda de max_pares acima).
  todos_ll <- rbind(sf::st_coordinates(origens_4326), sf::st_coordinates(destinos_4326))

  # O mesmo servidor tambem responde com uma matriz de zeros (de novo, sem erro) quando o
  # lote tem coordenadas duplicadas/quase-duplicadas - comum em roteamento em lote de verdade
  # (ex. varias origens para o mesmo hub). Deduplicar antes de consultar evita o problema e
  # de quebra reduz o tamanho da tabela pedida.
  chave <- paste(round(todos_ll[, 1], 6), round(todos_ll[, 2], 6))
  pontos_unicos <- todos_ll[!duplicated(chave), , drop = FALSE]
  idx_unico <- match(chave, chave[!duplicated(chave)])

  if (verbose && nrow(pontos_unicos) < nrow(todos_ll)) {
    message(
      nrow(todos_ll) - nrow(pontos_unicos), " coordenada(s) duplicada(s) no lote - ",
      "consultando ", nrow(pontos_unicos), " pontos unicos."
    )
  }

  tab <- osrm::osrmTable(
    loc = pontos_unicos,
    measure = c("duration", "distance"),
    osrm.server = osrm_server, osrm.profile = osrm_profile
  )

  idx_origens <- idx_unico[seq_len(n)]
  idx_destinos <- idx_unico[n + seq_len(n)]

  tempo_rodovia_h <- tab$durations[cbind(idx_origens, idx_destinos)] / 60
  distancia_rodovia_km <- tab$distances[cbind(idx_origens, idx_destinos)] / 1000

  dist_linha_reta_km <- as.numeric(
    sf::st_distance(origens_4326, destinos_4326, by_element = TRUE)
  ) / 1000

  # Defesa extra: se mesmo assim vier tudo zero para pares com origem != destino (ex. servidor
  # com o servico de tabela mal configurado), falhar com uma mensagem clara em vez de devolver
  # uma triagem incorreta.
  pares_reais <- dist_linha_reta_km > 0
  if (any(pares_reais) && all(distancia_rodovia_km[pares_reais] == 0, na.rm = TRUE)) {
    stop(
      "osrm::osrmTable() retornou apenas zeros para todos os pares com origem != destino no ",
      "servidor OSRM usado (\"", osrm_server, "\", perfil \"", osrm_profile, "\"). Verifique se ",
      "o perfil e' valido para esse servidor ou tente outro servidor.",
      call. = FALSE
    )
  }

  # tab$sources/$destinations sao os pontos unicos de 'loc' (identicos entre si, em modo
  # loc) - reusar idx_origens/idx_destinos (o mesmo mapeamento p/ pontos duplicados) para
  # pegar os pontos de origem/destino correspondentes a cada par.
  snap_origem_km <- as.numeric(sf::st_distance(
    sf::st_geometry(origens_4326),
    sf::st_as_sf(tab$sources[idx_origens, ], coords = c("lon", "lat"), crs = 4326),
    by_element = TRUE
  )) / 1000

  snap_destino_km <- as.numeric(sf::st_distance(
    sf::st_geometry(destinos_4326),
    sf::st_as_sf(tab$destinations[idx_destinos, ], coords = c("lon", "lat"), crs = 4326),
    by_element = TRUE
  )) / 1000

  razao_desvio <- ifelse(
    dist_linha_reta_km > 0, distancia_rodovia_km / dist_linha_reta_km, 1
  )

  rodovia_plausivel <-
    !is.na(tempo_rodovia_h) &
    snap_origem_km <= max_snap_osrm_km &
    snap_destino_km <= max_snap_osrm_km &
    razao_desvio <= max_ratio_desvio

  tibble::tibble(
    par = seq_len(n),
    tempo_rodovia_h = tempo_rodovia_h,
    distancia_rodovia_km = distancia_rodovia_km,
    dist_linha_reta_km = dist_linha_reta_km,
    razao_desvio = razao_desvio,
    snap_origem_km = snap_origem_km,
    snap_destino_km = snap_destino_km,
    candidato_multimodal = !rodovia_plausivel
  )
}
