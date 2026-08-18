#' Geocodifica endereços de unidades a partir de um relatório SIORG
#'
#' Prepara os campos de endereço de um relatório de estrutura organizacional (qualquer
#' órgão) e realiza a geocodificação utilizando o pacote \pkg{geocodebr}.
#'
#' @param arq_siorg Data frame contendo os dados de estrutura organizacional do SIORG.
#'   Deve conter as colunas: \code{codigoUnidade}, \code{logradouro}, \code{numero},
#'   \code{cep}, \code{bairro}, \code{nm_mun} e \code{uf}.
#'
#' @return Um objeto espacial do tipo \code{sf} contendo os dados das unidades
#'   geocodificadas e suas respectivas coordenadas.
#'
#' @details
#' A função realiza as seguintes etapas:
#' \itemize{
#'   \item remove duplicidades pelo código da unidade;
#'   \item padroniza os campos de endereço;
#'   \item geocodifica, resolvendo possíveis empates.
#' }
#'
#' @seealso
#' \code{\link[geocodebr]{geocode}},
#' \code{\link[geocodebr]{definir_campos}}
#'
#' @importFrom dplyr distinct
#'
#' @examples
#' \dontrun{
#' dados <- funai_relatorio_estrutura()
#' plot(sf::st_geometry(dados))
#' }
#'
#' @export
geocodifica_enderecos <- function(arq_siorg) {

  siorg <- arq_siorg |>
    dplyr::distinct(codigoUnidade, .keep_all = TRUE)

  campos <- geocodebr::definir_campos(
    logradouro = "logradouro",
    numero = "numero",
    cep = "cep",
    localidade = "bairro",
    municipio = "nm_mun",
    estado = "uf"
  )

  cr_geocode <- geocodebr::geocode(
    enderecos = siorg,
    padronizar_enderecos = TRUE,
    campos_endereco = campos,
    resolver_empates = TRUE,
    resultado_sf = TRUE,
    verboso = TRUE
  )

  return(cr_geocode)
}
