#' Consulta completa de unidades organizacionais (UORGs) no SIORG
#'
#' Consulta a base de Estrutura Organizacional do Governo Federal
#' (\url{https://estruturaorganizacional.dados.gov.br}) e retorna informações detalhadas de
#' uma unidade organizacional e de toda a sua descendência, a partir do código da unidade
#' raiz. Funciona para qualquer órgão ou entidade do Poder Executivo federal cadastrado no
#' SIORG, não apenas a Funai — basta informar o \code{codigo} da unidade de interesse.
#'
#' @param codigo Código SIORG da unidade organizacional raiz a ser consultada.
#'
#' @return Um \code{tibble} com os dados das UORGs (a unidade informada e sua descendência),
#' incluindo informações de contato, endereço, atos normativos e classificações. Caso a
#' consulta não retorne resultados, retorna \code{NULL}.
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' \dontrun{
#' siorg_estrutura_completa("12345")
#' }
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate select across everything
#' @importFrom tidyr unnest unnest_wider
siorg_estrutura_completa <- function(codigo) {

  # Construir URL da API
  url <- paste0(
    "https://estruturaorganizacional.dados.gov.br/doc/estrutura-organizacional/completa?",
    "codigoPoder=1&codigoEsfera=1&codigoUnidade=", codigo
  )

  # Requisição HTTP segura
  res <- httr::GET(url)

  # Checar status da requisição
  if (httr::http_error(res)) {
    warning("Falha na requisição para o código ", codigo, ": HTTP ", httr::status_code(res))
    return(NULL)
  }

  # Extrair conteúdo JSON
  json_content <- httr::content(res, as = "text", encoding = "UTF-8")
  dados <- jsonlite::fromJSON(json_content)$unidades

  if (length(dados) == 0) {
    message("Nenhuma unidade encontrada para o código ", codigo)
    return(NULL)
  }

  # Limpeza e padronização dos códigos
  dados <- dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("codigo"),
        ~ sub(".*/", "", .)
      )
    ) |>
    tidyr::unnest(cols = c(contato, endereco))

  # Expansão de listas internas (telefone, email, atoNormativo)
  dados <- dados |>
    dplyr::select(dplyr::everything(), -site) |>
    tidyr::unnest_wider(telefone, names_sep = "_") |>
    tidyr::unnest_wider(email, names_sep = "_") |>
    tidyr::unnest_wider(atoNormativo, names_sep = "_")

  return(dados)
}
