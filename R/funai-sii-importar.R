#' URL base do GeoServer do SII (Sistema de Informações Indígenas) da Funai
#' @keywords internal
#' @noRd
.funai_sii_url_base <- function() {
  "https://geoserver.funai.gov.br/geoserver/ows"
}

#' Importa as aldeias indígenas do SII da Funai
#'
#' Importa a camada de aldeias indígenas (pontos) do GeoServer do SII (Sistema de
#' Informações Indígenas) da Funai, via \code{\link{puxa_wfs_completo}}.
#'
#' @inheritParams puxa_wfs_completo
#' @param url_base URL do endpoint OWS do GeoServer da Funai.
#'
#' @return Um objeto \code{sf} de pontos, uma linha por aldeia, em EPSG:4674
#'   (SIRGAS2000), salvo se \code{crs} for informado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_importa_aldeias()
#' }
funai_importa_aldeias <- function(bbox = NULL,
                                   cql_filter = NULL,
                                   crs = NULL,
                                   count = 1000,
                                   max_tries = 5,
                                   verbose = TRUE,
                                   url_base = .funai_sii_url_base()) {
  puxa_wfs_completo(
    url_base = url_base,
    camada = "Funai:aldeias_pontos",
    bbox = bbox,
    cql_filter = cql_filter,
    crs = crs,
    sortby = "cod_aldeia",
    count = count,
    max_tries = max_tries,
    verbose = verbose
  )
}

#' Importa as terras indígenas do SII da Funai
#'
#' Importa a camada de terras indígenas (poligonais) do GeoServer do SII (Sistema de
#' Informações Indígenas) da Funai, via \code{\link{puxa_wfs_completo}}. Por padrão traz
#' a camada simples de poligonais; \code{portarias} e \code{amazonia_legal} selecionam
#' variantes com informação adicional.
#'
#' @inheritParams puxa_wfs_completo
#' @param portarias Se \code{TRUE}, usa a variante da camada com portarias e data de
#'   publicação. Não pode ser combinado com \code{amazonia_legal = TRUE}.
#' @param amazonia_legal Se \code{TRUE}, usa a camada restrita às terras indígenas da
#'   Amazônia Legal. Não pode ser combinado com \code{portarias = TRUE}.
#' @param url_base URL do endpoint OWS do GeoServer da Funai.
#'
#' @return Um objeto \code{sf} de polígonos, uma linha por terra indígena, em EPSG:4674
#'   (SIRGAS2000), salvo se \code{crs} for informado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_importa_terras_indigenas()
#' funai_importa_terras_indigenas(portarias = TRUE)
#' funai_importa_terras_indigenas(amazonia_legal = TRUE)
#' }
funai_importa_terras_indigenas <- function(portarias = FALSE,
                                            amazonia_legal = FALSE,
                                            bbox = NULL,
                                            cql_filter = NULL,
                                            crs = NULL,
                                            count = 1000,
                                            max_tries = 5,
                                            verbose = TRUE,
                                            url_base = .funai_sii_url_base()) {

  if (portarias && amazonia_legal) {
    stop(
      "'portarias' e 'amazonia_legal' não podem ser TRUE ao mesmo tempo — não existe ",
      "camada de terras indígenas da Amazônia Legal com portarias.",
      call. = FALSE
    )
  }

  if (amazonia_legal) {
    camada <- "Funai:tis_amazonia_legal_poligonais"
    sortby <- "terrai_codigo"
  } else if (portarias) {
    camada <- "Funai:tis_poligonais_portarias"
    sortby <- "terrai_codigo"
  } else {
    camada <- "Funai:tis_poligonais"
    sortby <- "gid"
  }

  puxa_wfs_completo(
    url_base = url_base,
    camada = camada,
    bbox = bbox,
    cql_filter = cql_filter,
    crs = crs,
    sortby = sortby,
    count = count,
    max_tries = max_tries,
    verbose = verbose
  )
}

#' Importa as terras indígenas em estudo do SII da Funai
#'
#' Importa a camada de terras indígenas em estudo (pontos) do GeoServer do SII (Sistema
#' de Informações Indígenas) da Funai, via \code{\link{puxa_wfs_completo}}.
#'
#' @inheritParams puxa_wfs_completo
#' @param portarias Se \code{TRUE}, usa a variante da camada com portarias e data de
#'   publicação.
#' @param url_base URL do endpoint OWS do GeoServer da Funai.
#'
#' @return Um objeto \code{sf} de pontos, uma linha por terra indígena em estudo, em
#'   EPSG:4674 (SIRGAS2000), salvo se \code{crs} for informado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_importa_tis_estudo()
#' funai_importa_tis_estudo(portarias = TRUE)
#' }
funai_importa_tis_estudo <- function(portarias = FALSE,
                                      bbox = NULL,
                                      cql_filter = NULL,
                                      crs = NULL,
                                      count = 1000,
                                      max_tries = 5,
                                      verbose = TRUE,
                                      url_base = .funai_sii_url_base()) {

  if (portarias) {
    camada <- "Funai:tis_pontos_portarias"
    sortby <- "terrai_codigo"
  } else {
    camada <- "Funai:tis_pontos"
    sortby <- "gid"
  }

  puxa_wfs_completo(
    url_base = url_base,
    camada = camada,
    bbox = bbox,
    cql_filter = cql_filter,
    crs = crs,
    sortby = sortby,
    count = count,
    max_tries = max_tries,
    verbose = verbose
  )
}

#' Importa a localização das Coordenações Regionais (CR) da Funai
#'
#' Importa a camada de Coordenações Regionais (pontos) do GeoServer do SII (Sistema de
#' Informações Indígenas) da Funai, via \code{\link{puxa_wfs_completo}}.
#'
#' @inheritParams puxa_wfs_completo
#' @param url_base URL do endpoint OWS do GeoServer da Funai.
#'
#' @return Um objeto \code{sf} de pontos, uma linha por Coordenação Regional, em
#'   EPSG:4674 (SIRGAS2000), salvo se \code{crs} for informado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_importa_coordenacoes_regionais()
#' }
funai_importa_coordenacoes_regionais <- function(bbox = NULL,
                                                  cql_filter = NULL,
                                                  crs = NULL,
                                                  count = 1000,
                                                  max_tries = 5,
                                                  verbose = TRUE,
                                                  url_base = .funai_sii_url_base()) {
  puxa_wfs_completo(
    url_base = url_base,
    camada = "Funai:tis_cr",
    bbox = bbox,
    cql_filter = cql_filter,
    crs = crs,
    count = count,
    max_tries = max_tries,
    verbose = verbose
  )
}

#' Importa a localização das Coordenações Técnicas Locais (CTL) da Funai
#'
#' Importa a camada de Coordenações Técnicas Locais (pontos) do GeoServer do SII
#' (Sistema de Informações Indígenas) da Funai, via \code{\link{puxa_wfs_completo}}.
#'
#' @inheritParams puxa_wfs_completo
#' @param url_base URL do endpoint OWS do GeoServer da Funai.
#'
#' @return Um objeto \code{sf} de pontos, uma linha por Coordenação Técnica Local, em
#'   EPSG:4674 (SIRGAS2000), salvo se \code{crs} for informado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_importa_coordenacoes_tecnicas_locais()
#' }
funai_importa_coordenacoes_tecnicas_locais <- function(bbox = NULL,
                                                         cql_filter = NULL,
                                                         crs = NULL,
                                                         count = 1000,
                                                         max_tries = 5,
                                                         verbose = TRUE,
                                                         url_base = .funai_sii_url_base()) {
  puxa_wfs_completo(
    url_base = url_base,
    camada = "Funai:tis_ctl",
    bbox = bbox,
    cql_filter = cql_filter,
    crs = crs,
    count = count,
    max_tries = max_tries,
    verbose = verbose
  )
}
