#' Consulta preços de materiais ou serviços no Compras.gov
#'
#' Esta função consulta a API de preços do Compras.gov, retornando histórico
#' de preços de materiais ou serviços, filtrando por estado, região e últimos X meses.
#'
#' @param codigo_item_catalogo Código do item no catálogo
#' @param estado Sigla do estado (opcional)
#' @param servico Se TRUE, consulta serviços (default FALSE)
#' @param regiao Região geográfica (opcional)
#' @param meses Quantidade de meses anteriores para filtrar resultados (default 12)
#' @param verbose Se TRUE, imprime mensagens de progresso (default TRUE)
#'
#' @return Data frame com os resultados filtrados
#' @export
#' @importFrom lubridate today `%m-%`
#' @importFrom dplyr bind_rows mutate filter left_join across contains
#' @importFrom tibble tribble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract
consultar_precos <- function(codigo_item_catalogo,
                             estado = NULL,
                             servico = FALSE,
                             regiao = NULL,
                             meses = 12,
                             verbose = TRUE) {

  # ---------------------------
  # Tabela de estados (pode virar dataset interno do pacote)
  # ---------------------------
  df_estados <- tibble::tribble(
    ~Sigla, ~Regiao,
    "AC", "Norte", "AL", "Nordeste", "AP", "Norte",
    "AM", "Norte", "BA", "Nordeste", "CE", "Nordeste",
    "DF", "Centro-Oeste", "ES", "Sudeste",
    "GO", "Centro-Oeste", "MA", "Nordeste",
    "MT", "Centro-Oeste", "MS", "Centro-Oeste",
    "MG", "Sudeste", "PA", "Norte",
    "PB", "Nordeste", "PR", "Sul",
    "PE", "Nordeste", "PI", "Nordeste",
    "RJ", "Sudeste", "RN", "Nordeste",
    "RS", "Sul", "RO", "Norte",
    "RR", "Norte", "SC", "Sul",
    "SP", "Sudeste", "SE", "Nordeste",
    "TO", "Norte"
  )

  # ---------------------------
  # URL base
  # ---------------------------
  base_url <- if (isTRUE(servico)) {
    "https://dadosabertos.compras.gov.br/modulo-pesquisa-preco/3_consultarServico"
  } else {
    "https://dadosabertos.compras.gov.br/modulo-pesquisa-preco/1_consultarMaterial"
  }

  resultados <- list()
  pagina <- 1

  data_ref <- lubridate::today()
  data_limite <- lubridate::today() %m-% base::months(meses)

  if (verbose) message("Iniciando busca...")

  while (data_ref > data_limite) {

    query <- list(
      codigoItemCatalogo = codigo_item_catalogo,
      tamanhoPagina = 100,
      dataResultado = "true",
      pagina = pagina
    )

    if (!is.null(estado)) query$estado <- estado

    if (verbose) message("Página: ", pagina)

    # ---------------------------
    # Chamada à API com retry e backoff exponencial
    # ---------------------------
    dados <- puxa_api_compras(
      httr::modify_url(base_url, query = query),
      verbose = verbose
    )

    if (is.null(dados$resultado) || length(dados$resultado) == 0) break

    if ("dataResultado" %in% names(dados$resultado)) {
      data_ref <- min(dados$resultado$dataResultado, na.rm = TRUE)
    } else {
      break
    }

    resultados[[pagina]] <- dados$resultado
    pagina <- pagina + 1

    Sys.sleep(0.5)
  }

  if (length(resultados) == 0) {
    if (verbose) message("Nenhum resultado encontrado.")
    return(data.frame())
  }

  tabela <- dplyr::bind_rows(resultados)

  # ---------------------------
  # Tratamento de datas
  # ---------------------------
  tabela <- tabela |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("data"), as.Date)
    ) |>
    dplyr::filter(dataResultado >= data_limite) |>
    dplyr::left_join(df_estados, by = c("estado" = "Sigla"))

  if (!is.null(regiao)) {
    tabela <- dplyr::filter(tabela, Regiao == regiao)
  }

  if (verbose) message("Registros finais: ", nrow(tabela))

  return(tabela)
}
