#' Unidades-raiz da hierarquia da Funai usadas por \code{funai_estrutura_completa()}
#'
#' Mapa de código SIORG -> sigla das unidades cuja descendência define o \code{nivel_1}
#' (diretoria) de cada unidade da Funai. É o padrão usado pelas funções \code{funai_*()};
#' para classificar a estrutura de outro órgão, informe seu próprio vetor de raízes via o
#' parâmetro \code{raizes} de \code{\link{.siorg_classifica_hierarquia}}.
#' @keywords internal
#' @noRd
.siorg_raizes_funai <- function() {
  c(
    "87501"  = "DAGES",
    "87876"  = "DPT",
    "505410" = "DHPS",
    "505790" = "DIGAT",
    "505890" = "DIDEM",
    "87887"  = "MNPI",
    "88031"  = "Audin",
    "107103" = "Ouvi",
    "88025"  = "PFE",
    "107102" = "Correg",
    "173"    = "Presidência"
  )
}

#' Classifica unidades organizacionais em níveis hierárquicos (nivel_1 / nivel_2)
#'
#' @description
#' Monta um grafo direcionado das unidades organizacionais (arestas
#' \code{codigoUnidadePai -> codigoUnidade}) e, para cada unidade, calcula:
#' \itemize{
#'   \item \code{nivel_1}: a qual unidade-raiz de \code{raizes} ela descende (ex: uma
#'     diretoria);
#'   \item \code{nivel_2}: a qual unidade filha direta de uma raiz ela descende (ex: uma
#'     coordenação-geral).
#' }
#' Não é específica da Funai: qualquer órgão cujos dados venham de
#' \code{\link{siorg_estrutura_completa}} pode ser classificado informando seu próprio
#' \code{raizes}.
#'
#' @param dados_api Data.frame com, no mínimo, \code{codigoUnidade} e
#'   \code{codigoUnidadePai} (formato retornado por \code{\link{siorg_estrutura_completa}}).
#' @param raizes Vetor nomeado (código SIORG = rótulo) com as unidades-raiz do nível 1.
#'   Padrão: as raízes da estrutura da Funai.
#'
#' @return Tibble com \code{codigoUnidade}, \code{nivel_1} e \code{nivel_2}, para uso em
#' \code{dplyr::left_join()} com os dados originais.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr distinct rename transmute filter arrange mutate full_join select where
#' @importFrom igraph graph_from_data_frame subcomponent V
#' @importFrom purrr imap_dfr
#' @importFrom stats setNames
.siorg_classifica_hierarquia <- function(dados_api, raizes = .siorg_raizes_funai()) {

  dados_dedup <- dados_api |>
    dplyr::distinct(codigoUnidade, .keep_all = TRUE)

  vertices <- dados_dedup |>
    dplyr::rename(name = codigoUnidade)

  # Remove colunas de lista/data.frame residuais (ex: campos ainda não desaninhados),
  # que fazem igraph::graph_from_data_frame() falhar.
  vertices_clean <- vertices |>
    dplyr::select(dplyr::where(~ !is.list(.))) |>
    dplyr::select(dplyr::where(~ !inherits(., "data.frame")))

  edges <- dados_dedup |>
    dplyr::filter(!is.na(codigoUnidadePai)) |>
    dplyr::transmute(from = codigoUnidadePai, to = codigoUnidade) |>
    dplyr::filter(from %in% vertices$name, to %in% vertices$name)

  g <- igraph::graph_from_data_frame(
    d = edges,
    directed = TRUE,
    vertices = vertices_clean
  )

  agrupamento_1 <- purrr::imap_dfr(raizes, function(nome, codigo) {
    if (codigo %in% igraph::V(g)$name) {
      desc <- igraph::subcomponent(g, v = codigo, mode = "out")
      data.frame(codigoUnidade = igraph::V(g)[desc]$name, nivel_1 = nome)
    }
  })

  coords <- with(
    dados_api |>
      dplyr::filter(codigoUnidadePai %in% names(raizes)) |>
      dplyr::distinct(codigoUnidade, sigla) |>
      dplyr::mutate(sigla = trimws(sigla)) |>
      dplyr::arrange(sigla),
    stats::setNames(sigla, codigoUnidade)
  )

  agrupamento_2 <- purrr::imap_dfr(coords, function(nome, codigo) {
    if (codigo %in% igraph::V(g)$name) {
      desc <- igraph::subcomponent(g, v = codigo, mode = "out")
      data.frame(codigoUnidade = igraph::V(g)[desc]$name, nivel_2 = nome)
    }
  })

  dplyr::full_join(
    agrupamento_1 |> dplyr::distinct(codigoUnidade, .keep_all = TRUE),
    agrupamento_2 |> dplyr::distinct(codigoUnidade, .keep_all = TRUE),
    by = "codigoUnidade"
  )
}

#' Estrutura organizacional completa da Funai, classificada por nível hierárquico
#'
#' Consulta a estrutura organizacional completa da Funai (código SIORG 173) e calcula,
#' internamente, a diretoria (\code{nivel_1}) e a coordenação (\code{nivel_2}) de cada
#' unidade.
#'
#' @return Um \code{tibble} com os dados das UORGs da Funai, incluindo informações de
#' contato, endereço, atos normativos e classificações.
#'
#' @details
#' Por compatibilidade com versões anteriores desta função, o \code{tibble} retornado
#' \strong{não} inclui as colunas \code{nivel_1}/\code{nivel_2} calculadas — use
#' \code{\link{prepara_dados_estrutura}} para obter os dados já com a classificação
#' hierárquica anexada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_estrutura_completa()
#' }
funai_estrutura_completa <- function() {

  codigo_funai <- 173

  dados <- siorg_estrutura_completa(codigo_funai)

  if (is.null(dados)) return(NULL)

  return(dados)
}
