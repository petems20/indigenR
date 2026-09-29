#' Extrai o texto de uma exceção OWS (GeoServer) de uma resposta HTTP de erro
#' @keywords internal
#' @noRd
.geoserver_wfs_extrai_excecao <- function(resp) {
  txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  m <- regmatches(txt, regexpr("(?<=<ows:ExceptionText>).*?(?=</ows:ExceptionText>)", txt, perl = TRUE))
  if (length(m) == 0) NULL else m
}

#' Executa uma requisição HTTP a um GeoServer com retentativa e backoff exponencial
#'
#' Retenta em caso de erro de conexão, limite de requisições (HTTP 429) ou erro de
#' servidor (5xx, tipicamente transitório); para em erro HTTP 4xx não recuperável (ex:
#' 404), já que retentar não mudaria o resultado.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr GET timeout status_code content
.geoserver_wfs_com_retry <- function(url, contexto, max_tries, verbose) {
  tentativa <- 1
  repeat {
    resp <- tryCatch(httr::GET(url, httr::timeout(60)), error = function(e) e)

    if (inherits(resp, "error")) {
      if (tentativa >= max_tries) {
        stop(
          "Falha de conexão ao ", contexto, " após ", tentativa, " tentativas.\n",
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
          "Erro HTTP ", sc, " persistente ao ", contexto, " após ", tentativa, " tentativas.",
          call. = FALSE
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
      excecao <- .geoserver_wfs_extrai_excecao(resp)
      dica <- if (!is.null(excecao) && grepl("primary key", excecao, fixed = TRUE)) {
        " A camada não tem chave primária definida no GeoServer — informe 'sortby' com uma coluna existente para permitir paginação."
      } else {
        ""
      }
      stop(
        "Erro HTTP ", sc, " ao ", contexto, ".",
        if (!is.null(excecao)) paste0(" ", excecao) else "",
        dica,
        call. = FALSE
      )
    }

    return(resp)
  }
}

#' Baixa e lê uma única página de feições de uma camada WFS de um GeoServer
#'
#' @keywords internal
#' @noRd
#' @importFrom httr parse_url build_url content
#' @importFrom sf st_read
puxa_wfs <- function(url_base,
                      camada,
                      count,
                      start_index,
                      bbox = NULL,
                      cql_filter = NULL,
                      srsname = NULL,
                      sortby = NULL,
                      max_tries = 5,
                      verbose = TRUE) {

  query <- list(
    service = "WFS",
    version = "2.0.0",
    request = "GetFeature",
    typeNames = camada,
    outputFormat = "application/json",
    count = count,
    startIndex = start_index
  )

  if (!is.null(bbox)) query$bbox <- bbox
  if (!is.null(cql_filter)) query$cql_filter <- cql_filter
  if (!is.null(srsname)) query$srsName <- srsname
  if (!is.null(sortby)) query$sortBy <- sortby

  url_parsed <- httr::parse_url(url_base)
  url_parsed$query <- query
  url <- httr::build_url(url_parsed)

  if (verbose) {
    message("Buscando '", camada, "' — startIndex ", start_index, ", count ", count, "...")
  }

  resp <- .geoserver_wfs_com_retry(
    url = url,
    contexto = paste0("baixar a camada '", camada, "'"),
    max_tries = max_tries,
    verbose = verbose
  )

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")

  if (is.null(txt) || identical(txt, "") || nchar(txt) == 0) {
    return(NULL)
  }

  destino <- tempfile(fileext = ".geojson")
  on.exit(unlink(destino), add = TRUE)
  writeLines(txt, destino, useBytes = TRUE)

  pagina <- tryCatch(
    sf::st_read(destino, quiet = TRUE),
    error = function(e) {
      stop(
        "Falha ao interpretar a resposta GeoJSON da camada '", camada, "':\n",
        e$message, call. = FALSE
      )
    }
  )

  if (nrow(pagina) == 0) {
    return(NULL)
  }

  pagina
}

#' Extrai todas as feições de uma camada WFS de um GeoServer, com paginação
#'
#' @description
#' Motor genérico (não específico de nenhum órgão) para consumir camadas vetoriais de
#' qualquer GeoServer via WFS 2.0.0, paginando com \code{startIndex}/\code{count} até
#' esgotar as feições disponíveis. \code{\link{funai_importa_terras_indigenas}} e as
#' demais conveniências \code{funai_importa_*()} são wrappers desta função para as
#' camadas do GeoServer do SII da Funai.
#'
#' @param url_base \code{character}. URL do endpoint OWS do GeoServer (ex.:
#'   \code{"https://geoserver.funai.gov.br/geoserver/ows"}).
#' @param camada \code{character}. Nome completo da camada (\code{typeName}), incluindo
#'   o workspace (ex.: \code{"Funai:tis_poligonais"}).
#' @param bbox \code{character} opcional. Filtro espacial por caixa delimitadora, no
#'   formato aceito pelo WFS (\code{"xmin,ymin,xmax,ymax[,crs]"}).
#' @param cql_filter \code{character} opcional. Filtro CQL aplicado no servidor (ex.:
#'   \code{"uf = 'AM'"}).
#' @param crs Opcional. Se informado (ex.: \code{"EPSG:4326"} ou \code{4326}), reprojeta
#'   o resultado final com \code{\link[sf]{st_transform}}. Se \code{NULL} (padrão),
#'   mantém o sistema de referência nativo da camada.
#' @param sortby \code{character} opcional. Nome de uma coluna existente na camada para
#'   ordenar a paginação. Algumas camadas do GeoServer (tipicamente views sem chave
#'   primária definida) exigem esse parâmetro para paginar com \code{startIndex}/
#'   \code{count} — nesse caso a requisição falha com uma mensagem indicando "chave
#'   primária"; informe aqui qualquer coluna existente da camada (ver
#'   \code{\link[sf]{st_read}}/\code{DescribeFeatureType} do GeoServer para os nomes de
#'   coluna disponíveis) para resolver.
#' @param count \code{integer}. Tamanho de página (número de feições por requisição).
#' @param max_tries \code{integer}. Número de retentativas por requisição HTTP, em caso
#'   de erro de conexão, limite de requisições (HTTP 429) ou erro de servidor (5xx).
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#'
#' @return Um objeto \code{sf} com todas as feições da camada (ou filtradas por
#'   \code{bbox}/\code{cql_filter}, quando informados). O nome da camada fica em
#'   \code{attr(resultado, "camada")}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_wfs_completo(
#'   url_base = "https://geoserver.funai.gov.br/geoserver/ows",
#'   camada = "Funai:tis_cr"
#' )
#' }
#' @importFrom sf st_transform
puxa_wfs_completo <- function(url_base,
                               camada,
                               bbox = NULL,
                               cql_filter = NULL,
                               crs = NULL,
                               sortby = NULL,
                               count = 1000,
                               max_tries = 5,
                               verbose = TRUE) {

  lista_paginas <- list()
  start_index <- 0

  repeat {
    pagina <- puxa_wfs(
      url_base = url_base,
      camada = camada,
      count = count,
      start_index = start_index,
      bbox = bbox,
      cql_filter = cql_filter,
      sortby = sortby,
      max_tries = max_tries,
      verbose = verbose
    )

    if (is.null(pagina)) break

    lista_paginas[[length(lista_paginas) + 1]] <- pagina

    if (nrow(pagina) < count) break

    start_index <- start_index + count
  }

  if (length(lista_paginas) == 0) {
    warning("Nenhuma feição encontrada para a camada '", camada, "'.", call. = FALSE)
    return(NULL)
  }

  if (verbose) message("Consolidando resultados...")

  resultado <- if (length(lista_paginas) == 1) {
    lista_paginas[[1]]
  } else {
    do.call(rbind, lista_paginas)
  }

  if (!is.null(crs)) {
    resultado <- sf::st_transform(resultado, crs)
  }

  attr(resultado, "camada") <- camada

  resultado
}
