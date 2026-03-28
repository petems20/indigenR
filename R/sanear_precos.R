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
sanear_precos <- function(dadosPrecos, marca_excluidos = FALSE) {

  # ---------------------------
  # Cálculo do IQR por unidade
  # ---------------------------
  dados_saneados <- dadosPrecos |>
    dplyr::group_by(unidadeFornecimento) |>
    dplyr::mutate(
      Q1 = quantile(precoUnitario, 0.25, na.rm = TRUE),
      Q3 = quantile(precoUnitario, 0.75, na.rm = TRUE),
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

  # ---------------------------
  # Remove colunas temporárias do IQR
  # ---------------------------
  #dados_saneados <- dados_saneados |>
  #  dplyr::select(-Q1, -Q3, -IQR, -limite_inferior, -limite_superior)

  return(dados_saneados)
}
