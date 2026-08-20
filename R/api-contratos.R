#' Requisição de baixo nível para a API de Contratos (Compras.gov), com retentativa
#'
#' @keywords internal
#' @noRd
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
puxa_api_contratos <- function(url, verbose = TRUE, max_tries = 5) {
  tentativa <- 1
  repeat {
    if (verbose) message("Extraindo dados da URL: ", url)

    resp <- httr::GET(url)
    sc <- httr::status_code(resp)

    # --- Retry em caso de erro HTTP temporário ---
    if (sc >= 400) {
      if (tentativa >= max_tries) {
        stop("Erro HTTP: ", sc, " após ", tentativa, " tentativas.", call. = FALSE)
      } else {
        wait_time <- 2 ^ (tentativa - 1)
        if (verbose) message("Erro HTTP ", sc, ". Tentando novamente em ", wait_time, " segundos...")
        Sys.sleep(wait_time)
        tentativa <- tentativa + 1
        next
      }
    }

    # --- Sucesso ---
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    return(jsonlite::fromJSON(txt, flatten = TRUE))
  }
}

#' Extrai contratos de uma Unidade Gestora via Contratos.gov.br
#'
#' Consulta a API pública (sem autenticação) do Contratos.gov.br
#' (\url{https://contratos.comprasnet.gov.br}) para uma Unidade Gestora (UG/UASG), e
#' opcionalmente também seus contratos inativos. Diferente de
#' \code{\link{puxa_api_compras_completo}}/\code{\link{puxa_api_pncp_completo}}, esta
#' API não pagina e não recebe parâmetros de querystring — a UG é informada no próprio
#' caminho da URL, e cada chamada já retorna todos os contratos daquela unidade. Não é
#' específica da Funai: qualquer código de UG cadastrado no Contratos.gov.br pode ser
#' usado.
#'
#' @param unidade_codigo Código da Unidade Gestora (UG/UASG), como \code{character} ou
#'   \code{numeric} (ex: \code{"194005"}). Mesmo espaço de código usado no SIAFI.
#' @param incluir_inativos Se \code{TRUE} (padrão), também busca os contratos inativos
#'   da UG e empilha com os ativos, marcando a coluna \code{situacao_consulta}
#'   (\code{"ativo"}/\code{"inativo"}).
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#' @param max_tries Número de retentativas por requisição HTTP, em caso de erro.
#'
#' @return Um \code{tibble} com os contratos da UG informada (colunas achatadas via
#'   \code{jsonlite::fromJSON(..., flatten = TRUE)}), ou uma tibble de 0 linhas se a UG
#'   não tiver contratos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_completo("194005")
#' }
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
puxa_api_contratos_completo <- function(unidade_codigo, incluir_inativos = TRUE,
                                         verbose = TRUE, max_tries = 5) {

  base_url <- "https://contratos.comprasnet.gov.br/api/contrato"

  busca_situacao <- function(url, situacao) {
    dados <- puxa_api_contratos(url, verbose = verbose, max_tries = max_tries)

    if (length(dados) == 0) {
      return(tibble::tibble())
    }

    dados <- tibble::as_tibble(dados)
    dados$situacao_consulta <- situacao
    dados
  }

  ativos <- busca_situacao(
    paste0(base_url, "/ug/", unidade_codigo), "ativo"
  )

  if (!incluir_inativos) {
    return(ativos)
  }

  inativos <- busca_situacao(
    paste0(base_url, "/inativo/ug/", unidade_codigo), "inativo"
  )

  dplyr::bind_rows(ativos, inativos)
}
