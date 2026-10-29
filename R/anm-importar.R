# Processos minerários da ANM (SIGMINE), pelo serviço ArcGIS REST público
# (geo.anm.gov.br/arcgis/rest/services/SIGMINE/dados_anm/MapServer). O download em lote de
# dadosabertos (app.anm.gov.br) não respondeu nos testes de 29/10/2026.

.anm_url_sigmine <- "https://geo.anm.gov.br/arcgis/rest/services/SIGMINE/dados_anm/MapServer"

#' Uma página de consulta a uma camada ArcGIS REST (GeoJSON), com retentativa
#'
#' @keywords internal
#' @noRd
.arcgis_pagina <- function(url_camada, parametros, contexto, max_tries = 5, verbose = TRUE) {
  tentativa <- 1
  repeat {
    resp <- tryCatch(
      httr::GET(paste0(url_camada, "/query"), query = parametros, httr::timeout(300),
                httr::user_agent("indigenR (R; httr)")),
      error = function(e) e
    )
    erro <- if (inherits(resp, "error")) resp$message else {
      sc <- httr::status_code(resp)
      if (sc == 429 || sc >= 500) paste("HTTP", sc) else NULL
    }
    if (!is.null(erro)) {
      if (tentativa >= max_tries) stop("Falha ao ", contexto, " após ", tentativa, " tentativas: ", erro, call. = FALSE)
      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) message(erro, ". Nova tentativa em ", wait_time, " segundos...")
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }
    if (httr::status_code(resp) >= 400) stop("Erro HTTP ", httr::status_code(resp), " ao ", contexto, ".", call. = FALSE)
    texto <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (grepl('^\\s*\\{\\s*"error"', texto)) stop("Erro do servidor ArcGIS ao ", contexto, ": ", substr(texto, 1, 300), call. = FALSE)
    return(texto)
  }
}

#' Todas as feições de uma camada ArcGIS REST (MapServer/FeatureServer), paginadas
#'
#' @description Consulta genérica a uma camada de serviço ArcGIS REST com
#' \code{resultOffset}/\code{resultRecordCount}, devolvendo \code{sf}. Serve a qualquer
#' serviço público ArcGIS que permita paginação.
#'
#' @param url_camada URL da camada (ex.: \code{".../MapServer/0"}).
#' @param where Filtro SQL do ArcGIS (padrão \code{"1=1"}, tudo).
#' @param campos Campos a trazer (\code{"*"} = todos).
#' @param crs EPSG do resultado (padrão 4326).
#' @param tamanho_pagina Registros por requisição (até o \code{maxRecordCount} do serviço).
#' @param max_tries,verbose Retentativas por requisição e mensagens de progresso.
#'
#' @return Objeto \code{sf}; \code{NULL} se nada casar com o filtro.
#' @export
#' @importFrom sf st_read
puxa_arcgis_completo <- function(url_camada, where = "1=1", campos = "*", crs = 4326, tamanho_pagina = 2000,
                                 max_tries = 5, verbose = TRUE) {
  paginas <- list()
  offset <- 0
  repeat {
    texto <- .arcgis_pagina(url_camada, list(
      where = where, outFields = campos, outSR = crs, f = "geojson", returnGeometry = "true",
      orderByFields = "OBJECTID", resultOffset = offset, resultRecordCount = tamanho_pagina
    ), contexto = paste("consultar", url_camada), max_tries = max_tries, verbose = verbose)
    pagina <- sf::st_read(texto, quiet = TRUE)
    if (nrow(pagina) == 0) break
    paginas[[length(paginas) + 1]] <- pagina
    if (verbose) message("ArcGIS: ", offset + nrow(pagina), " feições lidas...")
    if (nrow(pagina) < tamanho_pagina) break
    offset <- offset + tamanho_pagina
  }
  if (length(paginas) == 0) return(NULL)
  do.call(rbind, paginas)
}

#' Processos minerários ativos da ANM (SIGMINE)
#'
#' @description Baixa os polígonos dos processos minerários ativos do SIGMINE (Agência
#' Nacional de Mineração), com fase (requerimento de pesquisa, autorização de pesquisa,
#' lavra garimpeira, concessão de lavra...), substância, uso, titular, área e último evento.
#' Também dá acesso às outras camadas do serviço (áreas de bloqueio, reservas garimpeiras etc.).
#'
#' @param uf Sigla(s) de UF para filtrar no servidor (\code{NULL} = país inteiro, ~270 mil processos).
#' @param camada Índice da camada no serviço: 0 processos minerários ativos (padrão),
#'   1 áreas de proteção de fonte, 2 arrendamentos, 3 áreas de bloqueio, 4 reservas garimpeiras.
#' @param crs,max_tries,verbose Ver \code{\link{puxa_arcgis_completo}}.
#'
#' @return Objeto \code{sf} com os campos da camada (\code{PROCESSO}, \code{FASE},
#'   \code{SUBS}, \code{USO}, \code{NOME} (titular), \code{AREA_HA}, \code{UF},
#'   \code{ULT_EVENTO} etc.).
#' @export
#' @examples
#' \dontrun{
#' processos_rr <- anm_processos_minerarios(uf = "RR")
#' }
anm_processos_minerarios <- function(uf = NULL, camada = 0, crs = 4326, max_tries = 5, verbose = TRUE) {
  where <- if (is.null(uf)) "1=1" else sprintf("UF IN (%s)", paste0("'", toupper(uf), "'", collapse = ","))
  puxa_arcgis_completo(sprintf("%s/%d", .anm_url_sigmine, as.integer(camada)), where = where, crs = crs,
                       max_tries = max_tries, verbose = verbose)
}
