#' URL do extrato OSM (OpenStreetMap France) de uma região ou estado do Brasil
#' @keywords internal
#' @noRd
.hidrovias_url <- function(regiao) {
  .hidrovias_valida_regiao(regiao)
  paste0(
    "https://download.openstreetmap.fr/extracts/south-america/brazil/", regiao, "-latest.osm.pbf"
  )
}

#' Valida o nome de região/estado do extrato (ex. "north", "north/amazonas")
#' @keywords internal
#' @noRd
.hidrovias_valida_regiao <- function(regiao) {
  if (length(regiao) != 1 || is.na(regiao) || !grepl("^[a-z-]+(/[a-z-]+)?$", regiao)) {
    stop(
      "'regiao' deve ser o nome de um extrato do OpenStreetMap France para o Brasil: uma ",
      "região (\"north\", \"northeast\", \"central-west\", \"southeast\", \"south\") ou ",
      "região/estado (ex. \"north/amazonas\").",
      call. = FALSE
    )
  }
  invisible(regiao)
}

#' Diretório das bases locais de hidrovias dentro do cache do pacote
#' @keywords internal
#' @noRd
.hidrovias_dir <- function(cache_dir) {
  file.path(cache_dir, "hidrovias")
}

#' Nome de arquivo (sem extensão) de uma região no cache: "north/amazonas" -> "north_amazonas"
#' @keywords internal
#' @noRd
.hidrovias_nome <- function(regiao) {
  gsub("/", "_", regiao, fixed = TRUE)
}

#' Baixa o extrato OSM para disco com retentativa e backoff exponencial
#'
#' Retenta em caso de erro de conexão, limite de requisições (HTTP 429) ou erro de
#' servidor (5xx); para em erro HTTP 4xx não recuperável (ex. região inexistente).
#'
#' @keywords internal
#' @noRd
.hidrovias_baixa_pbf <- function(url, destino, max_tries, verbose) {
  tentativa <- 1
  repeat {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::write_disk(destino, overwrite = TRUE),
        httr::timeout(1800),
        if (verbose) httr::progress()
      ),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      if (tentativa >= max_tries) {
        stop(
          "Falha de conexão ao baixar o extrato OSM após ", tentativa, " tentativas.\n",
          resp$message, call. = FALSE
        )
      }
      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) message("Erro de conexão. Nova tentativa em ", wait_time, " segundos...")
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }

    sc <- httr::status_code(resp)

    if (sc == 429 || sc >= 500) {
      if (tentativa >= max_tries) {
        stop(
          "Erro HTTP ", sc, " persistente ao baixar o extrato OSM após ", tentativa,
          " tentativas.", call. = FALSE
        )
      }
      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) {
        message(
          if (sc == 429) "Rate limit (429). " else paste0("Erro HTTP ", sc, ". "),
          "Nova tentativa em ", wait_time, " segundos..."
        )
      }
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }

    if (sc >= 400) {
      stop(
        "Erro HTTP ", sc, " ao baixar o extrato OSM (", url, "). Verifique o nome da região.",
        call. = FALSE
      )
    }

    return(invisible(destino))
  }
}

#' Extrai o valor de uma chave do campo `other_tags` (formato hstore) do driver OSM do GDAL
#' @keywords internal
#' @noRd
.tag_hstore <- function(other_tags, chave) {
  padrao <- paste0('"', chave, '"=>"([^"]*)"')
  m <- regmatches(other_tags, regexec(padrao, other_tags))
  vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
}

#' Baixa as hidrovias do OpenStreetMap de uma região do Brasil para o cache local
#'
#' @description
#' Alternativa local ao Overpass para \code{\link{busca_hidrovias_osm}} (e portanto para
#' \code{\link{rotear_multimodal}} e afins): baixa uma única vez o extrato OSM da região
#' (formato `.osm.pbf`, publicado diariamente pelo OpenStreetMap France), extrai as linhas
#' com `waterway` entre `tipos` e as guarda num GeoPackage com índice espacial, em
#' \code{<cache_dir>/hidrovias/<regiao>.gpkg}. O extrato é apagado ao final.
#'
#' Os servidores públicos do Overpass costumam ser lentos (minutos por tile) e instáveis
#' (HTTP 504, conexões derrubadas) — inviáveis para roteamento em lote. Com a base local,
#' a leitura de uma região inteira leva segundos (a região Norte, extrato de ~165 MB, tem
#' ~15 mil linhas de rio/canal).
#'
#' @details
#' Com uma base local no cache, \code{\link{busca_hidrovias_osm}} (com
#' `fonte = "auto"`, o padrão) passa a usá-la sempre que ela cobrir a área pedida. A base é
#' um retrato do OSM na data do download: use `atualizar = TRUE` para renová-la.
#'
#' A base só é registrada no cache (arquivo de metadados `<regiao>.csv`) depois de gravada
#' por inteiro, então um download interrompido não deixa uma base parcial visível.
#'
#' @param regiao Nome do extrato do OpenStreetMap France para o Brasil: uma região
#'   (`"north"`, `"northeast"`, `"central-west"`, `"southeast"`, `"south"`) ou um estado
#'   dentro dela (ex. `"north/amazonas"`, `"north/para"`) — ver
#'   \url{https://download.openstreetmap.fr/extracts/south-america/brazil/}.
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}; as bases ficam na subpasta `hidrovias`.
#' @param tipos Valores da tag `waterway` a extrair. Padrão: `c("river", "canal")`. Incluir
#'   `"stream"` (igarapés) aumenta muito a base.
#' @param atualizar Se `TRUE`, baixa e extrai de novo mesmo que a região já esteja no cache.
#' @param arquivo_pbf Caminho de um extrato `.osm.pbf` já baixado, para usar no lugar do
#'   download (ex. em máquina sem acesso ao OpenStreetMap France). O arquivo não é apagado.
#' @param url URL do extrato. Por padrão montada a partir de `regiao`.
#' @param max_tries Número máximo de tentativas do download.
#' @param verbose Se `TRUE`, exibe progresso.
#'
#' @return Invisivelmente, o caminho do GeoPackage no cache.
#'
#' @seealso \code{\link{hidrovias_bases_cache}}, \code{\link{hidrovias_limpa_cache}},
#'   \code{\link{busca_hidrovias_osm}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hidrovias_baixa_base("north")
#' rotear_multimodal(c(-64.81, -3.21), c(-64.71, -3.34))  # usa a base local
#' }
hidrovias_baixa_base <- function(regiao,
                                 cache_dir = tools::R_user_dir("indigenR", "cache"),
                                 tipos = c("river", "canal"),
                                 atualizar = FALSE,
                                 arquivo_pbf = NULL,
                                 url = .hidrovias_url(regiao),
                                 max_tries = 5,
                                 verbose = TRUE) {
  .hidrovias_valida_regiao(regiao)

  dir_base <- .hidrovias_dir(cache_dir)
  nome <- .hidrovias_nome(regiao)
  gpkg <- file.path(dir_base, paste0(nome, ".gpkg"))
  metadados <- file.path(dir_base, paste0(nome, ".csv"))

  if (!atualizar && file.exists(metadados)) {
    if (verbose) message("Hidrovias '", regiao, "' — já estão no cache: ", gpkg)
    return(invisible(gpkg))
  }

  dir.create(dir_base, recursive = TRUE, showWarnings = FALSE)
  dir_provisorio <- file.path(dir_base, paste0(".provisorio_", nome))
  unlink(dir_provisorio, recursive = TRUE)
  dir.create(dir_provisorio)
  on.exit(unlink(dir_provisorio, recursive = TRUE), add = TRUE)

  if (is.null(arquivo_pbf)) {
    arquivo_pbf <- file.path(dir_provisorio, "extrato.osm.pbf")
    if (verbose) message("Hidrovias '", regiao, "' — baixando ", url)
    .hidrovias_baixa_pbf(url, arquivo_pbf, max_tries = max_tries, verbose = verbose)
    url_origem <- url
  } else {
    if (!file.exists(arquivo_pbf)) stop("Arquivo nao encontrado: ", arquivo_pbf, call. = FALSE)
    url_origem <- normalizePath(arquivo_pbf, winslash = "/")
  }

  if (verbose) message("Hidrovias '", regiao, "' — extraindo waterway = ", paste(tipos, collapse = ", "))

  tipos_sql <- paste0("'", gsub("'", "''", tipos), "'", collapse = ", ")
  linhas <- tryCatch(
    sf::st_read(
      arquivo_pbf,
      query = paste0(
        "SELECT osm_id, waterway, name, other_tags FROM lines WHERE waterway IN (", tipos_sql, ")"
      ),
      quiet = TRUE
    ),
    error = function(e) {
      stop(
        "Nao foi possivel ler o extrato OSM (", conditionMessage(e), "). O arquivo baixado ",
        "pode estar incompleto ou nao ser um .osm.pbf.", call. = FALSE
      )
    }
  )

  for (chave in c("width", "boat", "motorboat", "draft")) {
    linhas[[chave]] <- .tag_hstore(linhas$other_tags, chave)
  }
  linhas <- linhas[, .hidrovias_colunas()]
  linhas <- linhas[!sf::st_is_empty(linhas), ]

  gpkg_provisorio <- file.path(dir_provisorio, "hidrovias.gpkg")
  sf::st_write(linhas, gpkg_provisorio, layer = "hidrovias", quiet = TRUE)

  caixa <- sf::st_bbox(linhas)
  meta <- data.frame(
    regiao = regiao,
    origem = url_origem,
    baixado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_linhas = nrow(linhas),
    tipos = paste(sort(tipos), collapse = ","),
    xmin = caixa[["xmin"]], ymin = caixa[["ymin"]], xmax = caixa[["xmax"]], ymax = caixa[["ymax"]]
  )

  unlink(metadados)
  if (!file.copy(gpkg_provisorio, gpkg, overwrite = TRUE)) {
    stop("Nao foi possivel gravar a base em ", gpkg, call. = FALSE)
  }
  meta_provisorio <- file.path(dir_provisorio, "meta.csv")
  utils::write.csv(meta, meta_provisorio, row.names = FALSE)
  file.copy(meta_provisorio, metadados, overwrite = TRUE)

  if (verbose) message("Hidrovias '", regiao, "' — ", nrow(linhas), " linhas no cache: ", gpkg)
  invisible(gpkg)
}

#' Lista as bases locais de hidrovias disponíveis no cache
#'
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
#'
#' @return Uma tibble, uma linha por região baixada com \code{\link{hidrovias_baixa_base}}:
#'   `regiao`, `origem` (URL ou arquivo usado), `baixado_em`, `n_linhas`, `tipos`
#'   (valores de `waterway` extraídos), o retângulo envolvente (`xmin`, `ymin`, `xmax`,
#'   `ymax`) e `caminho` do GeoPackage. Vazia se não houver nenhuma.
#'
#' @export
#'
#' @examples
#' hidrovias_bases_cache()
hidrovias_bases_cache <- function(cache_dir = tools::R_user_dir("indigenR", "cache")) {
  arquivos <- list.files(.hidrovias_dir(cache_dir), pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) {
    return(tibble::tibble(
      regiao = character(), origem = character(), baixado_em = character(),
      n_linhas = numeric(), tipos = character(), xmin = numeric(), ymin = numeric(),
      xmax = numeric(), ymax = numeric(), caminho = character()
    ))
  }
  meta <- do.call(rbind, lapply(arquivos, function(a) {
    utils::read.csv(a, stringsAsFactors = FALSE, colClasses = c(regiao = "character"))
  }))
  meta$caminho <- file.path(.hidrovias_dir(cache_dir), paste0(.hidrovias_nome(meta$regiao), ".gpkg"))
  meta <- meta[file.exists(meta$caminho), ]
  tibble::as_tibble(meta)
}

#' Remove bases locais de hidrovias do cache
#'
#' @param regiao Região(ões) a remover, como informadas em \code{\link{hidrovias_baixa_base}}.
#'   Se `NULL`, remove todas.
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
#' @param verbose Se `TRUE`, informa o que foi removido.
#'
#' @return Invisivelmente, as regiões removidas.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hidrovias_limpa_cache("north")
#' }
hidrovias_limpa_cache <- function(regiao = NULL,
                                  cache_dir = tools::R_user_dir("indigenR", "cache"),
                                  verbose = TRUE) {
  presentes <- hidrovias_bases_cache(cache_dir)$regiao
  alvo <- if (is.null(regiao)) presentes else intersect(regiao, presentes)
  for (r in alvo) {
    unlink(file.path(.hidrovias_dir(cache_dir), paste0(.hidrovias_nome(r), c(".csv", ".gpkg"))))
  }
  if (verbose) {
    message(
      if (length(alvo) == 0) "Hidrovias — nenhuma base removida do cache."
      else paste0("Hidrovias — removida(s) do cache: ", paste(alvo, collapse = ", "))
    )
  }
  invisible(alvo)
}

#' Hidrovias de uma área a partir das bases locais do cache
#'
#' @description
#' Lê, de cada base local cujo retângulo envolvente cruza `bbox` e que tenha todos os
#' `tipos` pedidos, só as linhas que cruzam `bbox` (filtro espacial do GeoPackage), junta as
#' bases (sem duplicar vias que aparecem em duas regiões vizinhas), recorta ao `bbox` e
#' normaliza para LINESTRING — o mesmo formato devolvido pela busca via Overpass.
#'
#' @return sf de linhas, ou `NULL` se nenhuma base local cobre a área.
#' @keywords internal
#' @noRd
.hidrovias_locais <- function(bbox, tipos, cache_dir) {
  bases <- hidrovias_bases_cache(cache_dir)
  if (nrow(bases) == 0) return(NULL)

  cruza <- bases$xmin <= bbox[3] & bases$xmax >= bbox[1] & bases$ymin <= bbox[4] & bases$ymax >= bbox[2]
  tem_tipos <- vapply(strsplit(bases$tipos, ","), function(t) all(tipos %in% t), logical(1))
  bases <- bases[cruza & tem_tipos, ]
  if (nrow(bases) == 0) return(NULL)

  caixa_wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(.bbox_nomeado(bbox), crs = 4326)))

  partes <- lapply(bases$caminho, function(caminho) {
    sf::st_read(caminho, layer = "hidrovias", wkt_filter = caixa_wkt, quiet = TRUE)
  })
  linhas <- do.call(rbind, partes)
  linhas <- linhas[linhas$waterway %in% tipos, ]
  if (nrow(linhas) == 0) return(linhas)

  linhas <- linhas[!duplicated(sf::st_as_binary(sf::st_geometry(linhas))), ]

  .normaliza_linhas(suppressWarnings(
    sf::st_crop(linhas, .bbox_nomeado(bbox))
  ))
}
