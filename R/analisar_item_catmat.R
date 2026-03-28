#' Analisar preços de um item de material
#'
#' Esta função realiza a análise de preços históricos de um item do CATMAT,
#' realizando saneamento de outliers, padronização de unidades e cálculo de estatísticas.
#' Opcionalmente, salva os resultados em arquivos Excel organizados por classe.
#'
#' @param codigo_catmat Código do item CATMAT a ser analisado.
#' @param nome_do_responsavel Nome do responsável pela pesquisa. Padrão: "Nome".
#' @param pasta Caminho da pasta onde os arquivos serão salvos. Padrão: "PesquisadePrecos".
#' @param salvar Lógico. Se TRUE, salva os dados e resumo em arquivos Excel. Padrão: TRUE.
#' @param meses Número de meses anteriores a serem considerados na análise. Padrão: 12.
#' @param estado Filtrar por estado (opcional).
#' @param regiao Filtrar por região (opcional).
#' @param return_dados Lógico. Se TRUE, retorna uma lista com resumo, dados saneados e excluídos. Padrão: FALSE.
#' @param unidadeF Unidade de fornecimento a ser utilizada (opcional). Se NULL, utiliza a unidade predominante.
#'
#' @return Um tibble com o resumo do item ou, se \code{return_dados = TRUE},
#' uma lista contendo:
#' \itemize{
#'   \item \code{resumo}: resumo do item analisado
#'   \item \code{dadosSaneados}: preços saneados
#'   \item \code{dadosExcluidos}: preços excluídos
#' }
#'
#' @import dplyr
#' @import tibble
#' @import openxlsx
#' @importFrom stats mean median sd quantile
#' @export
#'
#' @examples
#' \dontrun{
#' analisar_item_material(123456, nome_do_responsavel = "Fulano", meses = 6)
#' }
analisar_item_material <- function(
    codigo_catmat,
    nome_do_responsavel = "Nome",
    pasta = "PesquisadePrecos",
    salvar = TRUE,
    meses = 12,
    estado = NULL,
    regiao = NULL,
    return_dados = FALSE,
    unidadeF = NULL
) {

  cat("\n--- Analisando o código CATMAT:", codigo_catmat, "---\n")

  tryCatch({

    # 1. Consultar preços
    dadosPrecos <- consultar_precos(
      codigo_item_catalogo = codigo_catmat,
      meses = meses,
      estado = estado,
      regiao = regiao
    )

    # Validação mínima de dados
    if (is.null(dadosPrecos) || nrow(dadosPrecos) < 3) {
      cat("Falha: Menos de 3 preços para o código", codigo_catmat, ". Pulando.\n")
      return(NULL)
    }

    # 2. Padronização da unidade de fornecimento
    dadosPrecos <- dadosPrecos %>%
      dplyr::mutate(
        unidadeFornecimento = trimws(paste0(
          dplyr::coalesce(as.character(nomeUnidadeFornecimento), ""),
          " ",
          dplyr::coalesce(as.character(capacidadeUnidadeFornecimento), ""),
          dplyr::coalesce(as.character(siglaUnidadeMedida), "")
        ))
      )

    # 3. Determinar unidade predominante
    if (is.null(unidadeF)) {
      unidadepredominante <- dadosPrecos %>%
        dplyr::group_by(unidadeFornecimento) %>%
        dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(N)) %>%
        dplyr::slice(1)
    } else {
      unidadepredominante <- dadosPrecos %>%
        dplyr::filter(unidadeFornecimento == unidadeF) %>%
        dplyr::group_by(unidadeFornecimento) %>%
        dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(N)) %>%
        dplyr::slice(1)
    }

    # Filtrar pela unidade predominante se tiver pelo menos 3 observações
    if (unidadepredominante$N >= 3) {
      dadosPrecos <- dadosPrecos %>%
        dplyr::filter(unidadeFornecimento == unidadepredominante$unidadeFornecimento)
      cat("Total de", nrow(dadosPrecos),
          "registros após padronização da unidade:", unidadepredominante$unidadeFornecimento, "\n")
    }

    # 4. Saneamento de outliers (IQR)
    preco <- dadosPrecos$precoUnitario
    Q1 <- stats::quantile(preco, 0.25, na.rm = TRUE)
    Q3 <- stats::quantile(preco, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    limite_inferior <- Q1 - 1.5 * IQR_val
    limite_superior <- Q3 + 1.5 * IQR_val

    dadosPrecosSaneados <- dadosPrecos %>%
      dplyr::filter(precoUnitario >= limite_inferior & precoUnitario <= limite_superior)

    precos_excluidos <- dplyr::anti_join(dadosPrecos, dadosPrecosSaneados) %>%
      dplyr::mutate(
        Justificativa = dplyr::if_else(precoUnitario <= limite_inferior,
                                       "Inexequível", "Excessivamente Elevado")
      )

    # Validação pós-saneamento
    if (nrow(dadosPrecosSaneados) < 3) {
      cat("Falha: Menos de 3 preços após saneamento para o código", codigo_catmat, "\n")
      return(NULL)
    }

    # 5. Estatísticas
    n_amostra <- nrow(dadosPrecosSaneados)
    media <- mean(dadosPrecosSaneados$precoUnitario, na.rm = TRUE)
    mediana <- stats::median(dadosPrecosSaneados$precoUnitario, na.rm = TRUE)
    desvio_padrao <- stats::sd(dadosPrecosSaneados$precoUnitario, na.rm = TRUE)
    coef_variacao <- ifelse(media > 0, desvio_padrao / media, 0)

    # 6. Determinar preço de referência
    if (coef_variacao <= 0.25) {
      preco_referencia <- media
      criterio_usado <- "Média Aritmética (CV <= 25%)"
    } else {
      preco_referencia <- mediana
      criterio_usado <- "Mediana (CV > 25%)"
    }

    # 7. Criar resumo
    resumo_item <- tibble::tibble(
      CATMAT = codigo_catmat,
      Descrição = dadosPrecos$descricaoItem[1],
      Grupo = dadosPrecos$nomeClasse[1],
      `N Amostral Total` = nrow(dadosPrecos),
      `N Preços Excluídos` = nrow(precos_excluidos),
      `N Amostra Saneada` = n_amostra,
      Média = round(media, 4),
      Mediana = round(mediana, 4),
      `Desvio Padrão` = round(desvio_padrao, 4),
      `Coeficiente de Variação (%)` = paste0(round(coef_variacao*100, 2), "%"),
      `Preço de Referência Estimado` = round(preco_referencia, 4),
      `Critério Utilizado` = criterio_usado,
      `Unidade padronizada?` = ifelse(unidadepredominante$N >= 3, "Sim", "Não"),
      `Unidade Predominante` = ifelse(unidadepredominante$N >= 3,
                                      unidadepredominante$unidadeFornecimento, NA)
    )

    # 8. Salvar em Excel
    if (salvar) {
      nomeclasse <- resumo_item$Grupo[1]
      dir.create(file.path(pasta, nomeclasse), showWarnings = FALSE, recursive = TRUE)
      arquivo <- file.path(pasta, nomeclasse, paste0(codigo_catmat, ".xlsx"))

      wb <- openxlsx::createWorkbook()

      # Informações
      openxlsx::addWorksheet(wb, "Informações")
      info_df <- data.frame(
        Campo = c("Responsável pela Pesquisa:", "Data e Hora da Pesquisa:"),
        Valor = c(nome_do_responsavel, format(Sys.time(), "%Y-%m-%d %H:%M"))
      )
      openxlsx::writeData(wb, "Informações", info_df, startRow = 1, colNames = FALSE)
      openxlsx::writeData(wb, "Informações", resumo_item, startRow = 4, colNames = TRUE)
      openxlsx::setColWidths(wb, "Informações", cols = 1:2, widths = "auto")

      # Preços Saneados
      openxlsx::addWorksheet(wb, "Preços Saneados")
      openxlsx::writeData(wb, "Preços Saneados", dadosPrecosSaneados)
      openxlsx::setColWidths(wb, "Preços Saneados", cols = 1:ncol(dadosPrecosSaneados), widths = "auto")

      # Preços Excluídos
      openxlsx::addWorksheet(wb, "Preços Excluídos")
      openxlsx::writeData(wb, "Preços Excluídos", precos_excluidos)
      openxlsx::setColWidths(wb, "Preços Excluídos", cols = 1:ncol(precos_excluidos), widths = "auto")

      openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
      cat("Arquivo salvo em:", arquivo, "\n")
    }

    cat("Sucesso: Análise do código", codigo_catmat, "concluída.\n")

    # 9. Retornos
    if (return_dados) {
      return(list(
        resumo = resumo_item,
        dadosSaneados = dadosPrecosSaneados,
        dadosExcluidos = precos_excluidos
      ))
    } else {
      return(resumo_item)
    }

  }, error = function(e) {
    cat("ERRO ao processar o código", codigo_catmat, ":", e$message, "\n")
    return(NULL)
  })
}
