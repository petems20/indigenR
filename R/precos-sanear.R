#' Saneia dados de preços unitários
#'
#' Esta função identifica e marca outliers usando o método IQR (interquartile range)
#' para cada unidade de fornecimento. Pode filtrar os outliers ou apenas marcar.
#'
#' @param dadosPrecos Data frame com colunas `precoUnitario` e `unidadeFornecimento`
#' @param marca_excluidos Se TRUE, apenas marca outliers com status e justificativa.
#'                        Se FALSE, remove outliers. Default FALSE.
#'
#' @return Data frame saneado, com outliers filtrados ou marcados.
#' @export
#' @importFrom dplyr group_by mutate ungroup filter case_when
#' @importFrom stats quantile
sanear_precos <- function(dadosPrecos, marca_excluidos = FALSE) {

  # ---------------------------
  # Cálculo do IQR por unidade
  # ---------------------------
  dados_saneados <- dadosPrecos |>
    dplyr::group_by(unidadeFornecimento) |>
    dplyr::mutate(
      Q1 = stats::quantile(precoUnitario, 0.25, na.rm = TRUE),
      Q3 = stats::quantile(precoUnitario, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      limite_inferior = Q1 - 1.5 * IQR,
      limite_superior = Q3 + 1.5 * IQR
    ) |>
    dplyr::ungroup()

  # ---------------------------
  # Filtra ou marca outliers
  # ---------------------------
  if (!marca_excluidos) {
    dados_saneados <- dados_saneados |>
      dplyr::filter(precoUnitario >= limite_inferior & precoUnitario <= limite_superior)
  } else {
    dados_saneados <- dados_saneados |>
      dplyr::mutate(
        Status = dplyr::case_when(
          precoUnitario < limite_inferior | precoUnitario > limite_superior ~ "Excluido",
          TRUE ~ "Incluído"
        ),
        Justificativa = dplyr::case_when(
          precoUnitario < limite_inferior ~ "Inexequível",
          precoUnitario > limite_superior ~ "Excessivamente Elevado",
          TRUE ~ NA_character_
        )
      )
  }

  return(dados_saneados)
}

#' Calcula o preço de referência (média ou mediana) a partir do coeficiente de variação
#'
#' @description
#' Implementa a regra de negócio única para escolha do preço de referência em pesquisas
#' de preços: usa a \strong{média} quando o coeficiente de variação (CV = desvio padrão /
#' média) é menor ou igual a 25%, e a \strong{mediana} caso contrário. Compartilhada por
#' todas as funções de análise de preços do pacote para evitar reimplementações divergentes
#' da mesma fórmula.
#'
#' @param precos Vetor numérico de preços unitários (já saneados).
#'
#' @return Lista com \code{media}, \code{mediana}, \code{desvio_padrao},
#'   \code{coef_variacao}, \code{preco_referencia} e \code{criterio_usado}.
#' @keywords internal
#' @noRd
#' @importFrom stats sd median
.calcula_preco_referencia <- function(precos) {

  media <- mean(precos, na.rm = TRUE)
  mediana <- stats::median(precos, na.rm = TRUE)
  desvio_padrao <- stats::sd(precos, na.rm = TRUE)

  coef_variacao <- if (!is.na(media) && media > 0) desvio_padrao / media else 0
  if (is.na(coef_variacao)) coef_variacao <- 0

  if (coef_variacao <= 0.25) {
    preco_referencia <- media
    criterio_usado <- "Média Aritmética (CV <= 25%)"
  } else {
    preco_referencia <- mediana
    criterio_usado <- "Mediana (CV > 25%)"
  }

  list(
    media = media,
    mediana = mediana,
    desvio_padrao = desvio_padrao,
    coef_variacao = coef_variacao,
    preco_referencia = preco_referencia,
    criterio_usado = criterio_usado
  )
}
