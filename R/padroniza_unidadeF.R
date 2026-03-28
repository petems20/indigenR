#' Padroniza unidades de fornecimento e medida
#'
#' Esta função adapta os nomes e siglas das unidades de fornecimento,
#' garantindo consistência entre materiais e serviços.
#'
#' @param dadosBrutos Data frame com os dados brutos da API
#' @param servico Se TRUE, indica que os dados são de serviços (default FALSE)
#'
#' @return Data frame com colunas padronizadas:
#' \itemize{
#'   \item unidadeFornecimento
#'   \item demais colunas originais
#' }
#' @keywords internal
#' @importFrom dplyr mutate case_when select rename
#' @importFrom stringr str_trim
padroniza_unidadeF <- function(dadosBrutos, servico = FALSE) {

  if (servico) {
    dadosBrutos <- dadosBrutos |>
      dplyr::rename(nomeUnidadeFornecimento = nomeUnidadeMedida) |>
      dplyr::mutate(
        unidadeFornecimento = nomeUnidadeFornecimento
      )
  } else {
    dadosBrutos <- dadosBrutos |>
      dplyr::mutate(
        # Ajusta sigla da unidade de medida
        siglaUnidadeMedidaAdaptado = dplyr::case_when(
          nomeUnidadeFornecimento == "QUILOGRAMA" ~ "KG",
          is.na(siglaUnidadeMedida) ~ as.character(siglaUnidadeFornecimento),
          TRUE ~ as.character(siglaUnidadeMedida)
        ),

        # Ajusta capacidade da unidade (0 vira 1)
        capacidadeUnidadeFornecimentoAdaptado = dplyr::case_when(
          capacidadeUnidadeFornecimento == 0 | is.na(capacidadeUnidadeFornecimento) ~ 1,
          TRUE ~ capacidadeUnidadeFornecimento
        ),

        # Cria coluna final de unidade de fornecimento
        unidadeFornecimento = stringr::str_trim(
          paste(
            capacidadeUnidadeFornecimentoAdaptado,
            siglaUnidadeMedidaAdaptado
          )
        )
      ) |>
      dplyr::select(-siglaUnidadeMedidaAdaptado, -capacidadeUnidadeFornecimentoAdaptado)
  }

  return(dadosBrutos)
}
