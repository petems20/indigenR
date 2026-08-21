#' Abre (e inicializa, se necessário) o cache DuckDB de hidrovias
#'
#' @param cache_dir Diretório onde o arquivo `hidrovias_cache.duckdb` é mantido.
#' @return Conexão DBI aberta. Chame `DBI::dbDisconnect(con, shutdown = TRUE)` ao final.
#' @keywords internal
#' @noRd
.overpass_cache_con <- function(cache_dir) {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), file.path(cache_dir, "hidrovias_cache.duckdb"))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tiles_hidrovias (
      tile_id VARCHAR PRIMARY KEY,
      tipos VARCHAR,
      criado_em TIMESTAMP,
      n_features INTEGER,
      sf_blob BLOB
    )
  ")

  con
}

#' Divide um bbox (lon/lat) em uma grade de tiles fixos
#'
#' @description
#' Consultas ao Overpass são cacheadas por tile, não pelo bbox exato pedido — como a mesma
#' região é tipicamente revisitada por chamadas próximas em um lote (vários pares de pontos
#' na mesma bacia), tiles de tamanho fixo maximizam o reaproveitamento do cache, o que uma
#' chave por bbox exato não faria.
#'
#' @param bbox `c(xmin, ymin, xmax, ymax)` em graus.
#' @param tile_deg Tamanho do lado do tile, em graus.
#' @return `data.frame` com uma linha por tile (`xmin`, `ymin`, `xmax`, `ymax`).
#' @keywords internal
#' @noRd
.tile_grid <- function(bbox, tile_deg = 0.5) {

  xs <- seq(floor(bbox[1] / tile_deg) * tile_deg, floor(bbox[3] / tile_deg) * tile_deg, by = tile_deg)
  ys <- seq(floor(bbox[2] / tile_deg) * tile_deg, floor(bbox[4] / tile_deg) * tile_deg, by = tile_deg)

  grade <- expand.grid(xmin = xs, ymin = ys)

  grade$xmax <- grade$xmin + tile_deg
  grade$ymax <- grade$ymin + tile_deg

  grade
}

#' Colunas padronizadas retidas para as linhas de hidrovia
#' @keywords internal
#' @noRd
.hidrovias_colunas <- function() {
  c("osm_id", "waterway", "name", "width", "boat", "motorboat", "draft")
}

#' Nomeia um bbox `c(xmin, ymin, xmax, ymax)` como `sf::st_crop()` exige
#'
#' `sf::st_crop()` silenciosamente falha (`!anyNA(x) não é TRUE`) se o bbox não tiver os
#' nomes `xmin`/`ymin`/`xmax`/`ymax` — o formato posicional usado pelo resto do pacote (e por
#' `osmdata::opq()`, que aceita as duas formas) não é suficiente.
#' @keywords internal
#' @noRd
.bbox_nomeado <- function(bbox) {
  stats::setNames(as.numeric(bbox), c("xmin", "ymin", "xmax", "ymax"))
}

#' Normaliza uma coleção sf para LINESTRING uniforme, explodindo MULTILINESTRING
#'
#' `sfnetworks::as_sfnetwork()` exige geometria uniforme e falha se houver uma mistura de
#' `LINESTRING`/`MULTILINESTRING` — o que acontece na prática, já que tanto o Overpass quanto
#' o `sf::st_crop()` (por tile e no bbox final) podem produzir `MULTILINESTRING` a partir de
#' uma via que é cortada em pedaços desconexos. Um `sf::st_cast(x, "LINESTRING")` direto sobre
#' geometria mista descarta partes de cada `MULTILINESTRING` silenciosamente; o cast em dois
#' passos (via `MULTILINESTRING` primeiro) explode cada uma corretamente em várias linhas.
#' @keywords internal
#' @noRd
#' @importFrom sf st_cast st_is_empty st_geometry_type
.normaliza_linhas <- function(x) {
  x <- suppressWarnings(sf::st_cast(sf::st_cast(x, "MULTILINESTRING"), "LINESTRING"))
  x[!sf::st_is_empty(x) & sf::st_geometry_type(x) == "LINESTRING", ]
}

#' Busca um único tile no Overpass, com retentativa e backoff exponencial
#'
#' @description
#' Segue o mesmo idioma de retentativa usado em `puxa_api_pncp()` (`R/api-pncp.R`), adaptado
#' para `osmdata`, que não tenta de novo sozinho e cujo backend (Overpass) frequentemente
#' responde com erro sob carga (HTTP 429/504). O resultado é recortado (`sf::st_crop()`) ao
#' bbox do tile: o Overpass devolve a geometria *inteira* de qualquer via que apenas toque o
#' bbox pedido (ex. o Rio Negro inteiro por tocar um tile pequeno), o que inflaria o cache e o
#' grafo se não for recortado.
#'
#' @keywords internal
#' @noRd
#' @importFrom sf st_crop st_sf st_sfc
.busca_tile_overpass <- function(tile_bbox, tipos, timeout_overpass = 120, max_tries = 5, verbose = TRUE) {

  vazio_df <- as.data.frame(
    stats::setNames(as.list(rep(NA_character_, length(.hidrovias_colunas()))), .hidrovias_colunas())
  )[0, ]

  vazio <- sf::st_sf(vazio_df, geometry = sf::st_sfc(crs = 4326))

  tentativa <- 1

  repeat {

    resultado <- tryCatch({
      q <- osmdata::opq(bbox = tile_bbox, timeout = timeout_overpass) |>
        osmdata::add_osm_feature(key = "waterway", value = tipos)
      osmdata::osmdata_sf(q)
    }, error = function(e) e)

    if (!inherits(resultado, "error")) break

    if (tentativa >= max_tries) {
      stop(
        "Falha ao consultar o Overpass apos ", tentativa, " tentativas.\n",
        conditionMessage(resultado),
        call. = FALSE
      )
    }

    wait_time <- 2 ^ (tentativa - 1)

    if (verbose) {
      message("Erro no Overpass (", conditionMessage(resultado), "). Nova tentativa em ", wait_time, "s...")
    }

    Sys.sleep(wait_time)

    tentativa <- tentativa + 1
  }

  linhas <- resultado$osm_lines

  if (is.null(linhas) || nrow(linhas) == 0) {
    return(vazio)
  }

  linhas <- linhas[, intersect(.hidrovias_colunas(), names(linhas))]

  faltantes <- setdiff(.hidrovias_colunas(), names(linhas))
  for (col in faltantes) linhas[[col]] <- NA_character_

  linhas <- linhas[, c(.hidrovias_colunas(), attr(linhas, "sf_column"))]

  suppressWarnings(
    sf::st_crop(linhas, .bbox_nomeado(tile_bbox))
  )
}

#' Waterways do OpenStreetMap para uma área, via Overpass, com cache local em DuckDB
#'
#' @description
#' Baixa `waterway=river/canal/...` do Overpass API (via \pkg{osmdata}) para o bbox informado,
#' substituindo fontes locais estáticas (ex. `targets::tar_read(mapa_hidrovias)`). A consulta é
#' dividida em tiles de tamanho fixo (`tile_deg`) e cada tile é cacheado individualmente em um
#' banco DuckDB local — chamadas subsequentes que caiam nos mesmos tiles (comum em uso em lote,
#' roteando muitos pares de pontos numa mesma bacia) não tornam a bater no Overpass, respeitando
#' a política de uso responsável do serviço público.
#'
#' @param bbox `c(xmin, ymin, xmax, ymax)` em graus (WGS84/EPSG:4326).
#' @param tipos Valores da tag `waterway` a incluir. Padrão: `c("river", "canal")`.
#' @param tile_deg Tamanho do lado do tile de cache, em graus. Padrão: `0.5` (~55 km).
#' @param cache_dir Diretório do cache DuckDB. Padrão: `tools::R_user_dir("indigenR", "cache")`.
#' @param cache_max_age_dias Idade máxima (dias) de um tile em cache antes de ser rebuscado.
#' @param timeout_overpass Timeout (segundos) repassado à consulta Overpass.
#' @param max_tries Máximo de tentativas por tile em caso de erro.
#' @param verbose Se `TRUE`, imprime mensagens de progresso.
#'
#' @return Objeto `sf` (LINESTRING, EPSG:4326) com as waterways da área, colunas
#'   `osm_id`, `waterway`, `name`, `width`, `boat`, `motorboat`, `draft`. A geometria é sempre
#'   normalizada para `LINESTRING` uniforme (`MULTILINESTRING` — comuns após o recorte por
#'   `sf::st_crop()` — são explodidas em várias linhas) antes de retornar, mesmo quando parte
#'   dos dados vem de um tile já cacheado de uma versão anterior desta função.
#'
#' @export
#' @importFrom sf st_crop st_as_sfc st_bbox
busca_hidrovias_osm <- function(
    bbox,
    tipos = c("river", "canal"),
    tile_deg = 0.5,
    cache_dir = tools::R_user_dir("indigenR", "cache"),
    cache_max_age_dias = 30,
    timeout_overpass = 120,
    max_tries = 5,
    verbose = TRUE
) {

  tipos_key <- paste(sort(tipos), collapse = "+")

  tiles <- .tile_grid(bbox, tile_deg = tile_deg)

  con <- .overpass_cache_con(cache_dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  idade_limite <- Sys.time() - as.difftime(cache_max_age_dias, units = "days")

  lista <- vector("list", nrow(tiles))

  for (i in seq_len(nrow(tiles))) {

    tb <- tiles[i, ]
    tile_id <- sprintf("%s|%.4f_%.4f_%.4f", tipos_key, tb$xmin, tb$ymin, tile_deg)

    cache_hit <- DBI::dbGetQuery(
      con,
      "SELECT criado_em, sf_blob FROM tiles_hidrovias WHERE tile_id = ?",
      params = list(tile_id)
    )

    if (nrow(cache_hit) == 1 && cache_hit$criado_em[[1]] >= idade_limite) {

      if (verbose) message("Tile ", tile_id, ": cache.")
      lista[[i]] <- unserialize(cache_hit$sf_blob[[1]])
      next
    }

    if (verbose) message("Tile ", tile_id, ": consultando Overpass...")

    tile_bbox <- c(tb$xmin, tb$ymin, tb$xmax, tb$ymax)

    dados_tile <- .busca_tile_overpass(
      tile_bbox,
      tipos = tipos,
      timeout_overpass = timeout_overpass,
      max_tries = max_tries,
      verbose = verbose
    )

    blob <- serialize(dados_tile, connection = NULL)

    DBI::dbExecute(con, "DELETE FROM tiles_hidrovias WHERE tile_id = ?", params = list(tile_id))

    DBI::dbExecute(
      con,
      "INSERT INTO tiles_hidrovias VALUES (?, ?, ?, ?, ?)",
      params = list(tile_id, tipos_key, Sys.time(), nrow(dados_tile), list(blob))
    )

    lista[[i]] <- dados_tile
  }

  combinado <- do.call(rbind, lista)

  if (nrow(combinado) == 0) {
    return(combinado)
  }

  combinado <- combinado[!duplicated(combinado$osm_id), ]

  .normaliza_linhas(suppressWarnings(
    sf::st_crop(combinado, .bbox_nomeado(bbox))
  ))
}
