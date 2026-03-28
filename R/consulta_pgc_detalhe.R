#' Consultar detalhes do PGC (Projeto de Gestão de Compras)
#'
#' Extrai informações detalhadas de itens do PGC via API.
#'
#' @param orgao Código ou sigla do órgão (REQUIRED)
#' @param anoPca Ano do PCA (REQUIRED)
#' @param delay_segundos Tempo de espera entre requisições (default 0.5s)
#'
#' @return Tibble com os detalhes do PGC ou objeto da API em caso de erro.
#' @export
#' @importFrom dplyr bind_rows mutate across ends_with
#' @importFrom tidyr unnest_wider
#' @importFrom lubridate ymd_hms
consultarPgcDetalhe <- function(orgao, anoPca, delay_segundos = 0.5) {

  # Validação de argumentos obrigatórios
  if (missing(orgao) || missing(anoPca)) {
    stop("'orgao' e 'anoPca' são obrigatórios.")
  }

  todos_dados <- list()
  pagina <- 1
  base_url <- "https://dadosabertos.compras.gov.br/modulo-pgc/1_consultarPgcDetalhe"

  repeat {

    url <- paste0(
      base_url,
      "?pagina=", pagina,
      "&tamanhoPagina=500",
      "&orgao=", orgao,
      "&anoPcaProjetoCompra=", anoPca
    )

    resultado <- puxa_api_compras(url)

    # Se houve erro HTTP ou resposta vazia
    if (inherits(resultado, "response") || length(resultado) == 0) {
      message(sprintf("Falha ou fim da extração na página %d", pagina))
      break
    }

    df <- tibble::tibble(registros = resultado$resultado) |> tidyr::unnest_wider(registros)

    # Conversão segura de datas
    df <- dplyr::mutate(
      df,
      dplyr::across(
        dplyr::ends_with("Data") | dplyr::ends_with("data"),
        ~ suppressWarnings(lubridate::ymd_hms(.x, tz = "UTC")),
        .names = "{.col}_dt"
      )
    )

    todos_dados[[pagina]] <- df

    # PAGINAÇÃO
    paginasRestantes <- resultado$paginasRestantes
    if (resultado$paginasRestantes == 0) break
    pagina <- pagina + 1
    Sys.sleep(delay_segundos)

  }

  dplyr::bind_rows(todos_dados)
}
