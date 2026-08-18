#' Prepara arquivo Excel de resultados de pesquisa de preços
#'
#' @param dadosPrecos Data frame com resultados saneados (deve conter coluna 'Status')
#' @param nome_do_responsavel Nome do responsável pela pesquisa
#' @param pasta Diretório onde o arquivo será salvo
#'
#' @return Caminho completo do arquivo Excel gerado
#' @export
#' @import openxlsx
#' @importFrom dplyr is_grouped_df group_keys group_split
#' @importFrom purrr walk2
#' @importFrom glue glue
#' @importFrom tibble tibble
prepara_arquivo <- function(dadosPrecos, nome_do_responsavel = "Nome", pasta) {

  # ---- Trata data frames agrupados ----
  if (dplyr::is_grouped_df(dadosPrecos)) {
    grupos <- dplyr::group_keys(dadosPrecos)
    lista  <- dplyr::group_split(dadosPrecos)

    purrr::walk2(lista, grupos$unidadeFornecimento, function(df, key) {
      message("Gerando arquivo para grupo: ", key)
      prepara_arquivo(df, nome_do_responsavel, pasta)
    })
    return(invisible(NULL))
  }

  # ---- Variáveis principais ----
  codItem <- unique(dadosPrecos$codigoItemCatalogo)
  unidade <- trimws(dadosPrecos$unidadeFornecimento[1])

  # ---- Criação de estilos ----
  estilo_header <- openxlsx::createStyle(fgFill = "#DDDDDD", halign = "left", textDecoration = "bold")
  estilo_normal <- openxlsx::createStyle(numFmt = "GENERAL", halign = "left", valign = "center")
  estilo_percentual <- openxlsx::createStyle(numFmt = "0.00%", halign = "left", valign = "center")
  estilo_numero <- openxlsx::createStyle(numFmt = "0.00", halign = "left", valign = "center")
  estilo_moeda <- openxlsx::createStyle(numFmt = '"R$" #,##0.00', fgFill = "#DDDDDD",
                                        halign = "left", valign = "center", border = "Top", textDecoration = "bold")
  estilo_excluido <- openxlsx::createStyle(fontColour = "red")

  # ---- Cria workbook e planilhas ----
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Resumo")
  openxlsx::showGridLines(wb, sheet = "Resumo", showGridLines = FALSE)
  openxlsx::addWorksheet(wb, "Inciso I")
  openxlsx::addWorksheet(wb, "Inciso III")

  # ---- Dados ----
  openxlsx::writeDataTable(wb, sheet = "Inciso I", x = dadosPrecos, tableName = "dados", startCol = 1, startRow = 1)
  openxlsx::writeDataTable(wb, sheet = "Inciso III",
                           tibble::tibble(
                             link = NA_real_,
                             CATMAT = NA_integer_,
                             `Preço Unitário` = NA_real_,
                             `Data/Hora` = as.Date(NA)
                           ),
                           tableName = "dadosIII", startCol = 1, startRow = 1)

  # ---- Função auxiliar: número para letra de coluna ----
  num_para_col_letra <- function(n) {
    letra <- ""
    while (n > 0) {
      resto <- (n - 1) %% 26
      letra <- paste0(intToUtf8(65 + resto), letra)
      n <- floor((n - 1) / 26)
    }
    letra
  }

  # ---- Formatação condicional para excluídos ----
  col_status <- num_para_col_letra(which(names(dadosPrecos) == "Status"))
  openxlsx::conditionalFormatting(
    wb,
    sheet = "Inciso I",
    cols = 1:ncol(dadosPrecos),
    rows = 2:(nrow(dadosPrecos) + 1),
    rule = glue::glue('${col_status}2="Excluido"'),
    style = estilo_excluido,
    type = "expression"
  )

  # ---- Resumo ----
  linha_atual <- 1
  f_normal <- c(); f_header <- c(); f_numero <- c(); f_moeda <- c()

  # Responsável e Data/Hora
  openxlsx::writeData(wb, "Resumo", x = "Responsável pela Pesquisa:", startRow = linha_atual, startCol = 1)
  openxlsx::writeData(wb, "Resumo", x = nome_do_responsavel, startRow = linha_atual, startCol = 2)
  linha_atual <- linha_atual + 1
  openxlsx::writeData(wb, "Resumo", x = "Data e Hora da Pesquisa:", startRow = linha_atual, startCol = 1)
  openxlsx::writeData(wb, "Resumo", x = format(Sys.time(), "%Y-%m-%d %H:%M"), startRow = linha_atual, startCol = 2)
  linha_atual <- linha_atual + 2

  # Resumo da Pesquisa
  openxlsx::writeData(wb, "Resumo", x = "Resumo da Pesquisa", startRow = linha_atual, startCol = 1)
  f_header <- c(f_header, linha_atual)
  linha_atual <- linha_atual + 1

  # CATMAT
  openxlsx::writeData(wb, "Resumo", x = "CATMAT", startRow = linha_atual, startCol = 1)
  openxlsx::writeData(wb, "Resumo", x = codItem, startRow = linha_atual, startCol = 2)
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Unidade
  openxlsx::writeData(wb, "Resumo", x = "Unidade de Fornecimento", startRow = linha_atual, startCol = 1)
  openxlsx::writeData(wb, "Resumo", x = unique(dadosPrecos$unidadeFornecimento), startRow = linha_atual, startCol = 2)
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Descrição
  openxlsx::writeData(wb, "Resumo", x = "Descrição", startRow = linha_atual, startCol = 1)
  openxlsx::writeData(wb, "Resumo", x = unique(dadosPrecos$descricaoItem), startRow = linha_atual, startCol = 2)
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Classe
  openxlsx::writeData(wb, "Resumo", x = "Classe", startRow = linha_atual, startCol = 1)
  openxlsx::writeData(wb, "Resumo", x = unique(dadosPrecos$nomeClasse), startRow = linha_atual, startCol = 2)
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Itens amostra original
  openxlsx::writeData(wb, "Resumo", x = "Itens Amostra Original", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = '=ROWS(dados[idCompra])+COUNTIF(dadosIII[Preço Unitário], "<>")')
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Itens Excluídos
  openxlsx::writeData(wb, "Resumo", x = "Itens Excluídos", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = '=COUNTIF(dados[Status], "Excluido")')
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Itens Incluídos
  openxlsx::writeData(wb, "Resumo", x = "Itens Incluídos", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = '=COUNTIF(dados[Status], "Incluído")+COUNTIF(dadosIII[Preço Unitário], "<>")')
  f_normal <- c(f_normal, linha_atual)
  linha_atual <- linha_atual + 1

  # Média, Mediana, Desvio, CV, Preço referência, Preço final, Critério
  linha_media <- linha_atual
  openxlsx::writeData(wb, "Resumo", x = "Média", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = '=AVERAGE(IF(dados[Status]="Incluído", dados[precoUnitario], ""), dadosIII[Preço Unitário])', array = TRUE)
  f_numero <- c(f_numero, linha_atual)
  linha_atual <- linha_atual + 1

  linha_mediana <- linha_atual
  openxlsx::writeData(wb, "Resumo", x = "Mediana", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = '=MEDIAN(IF(dados[Status]="Incluído", dados[precoUnitario],), dadosIII[Preço Unitário])', array = TRUE)
  f_numero <- c(f_numero, linha_atual)
  linha_atual <- linha_atual + 1

  linha_desvpad <- linha_atual
  openxlsx::writeData(wb, "Resumo", x = "Desvio Padrão", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = '=IFERROR(STDEV(IF(dados[Status]="Incluído", dados[precoUnitario],""), dadosIII[Preço Unitário]), "")', array = TRUE)
  f_numero <- c(f_numero, linha_atual)
  linha_atual <- linha_atual + 1

  linha_cv <- linha_atual
  openxlsx::writeData(wb, "Resumo", x = "Coeficiente de Variação (%)", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = glue::glue('=IFERROR(B{linha_desvpad}/B{linha_media},"")'))
  openxlsx::addStyle(wb, "Resumo", style = estilo_percentual, rows = linha_atual, cols = 2, gridExpand = TRUE)
  linha_atual <- linha_atual + 1

  linha_precoref <- linha_atual
  openxlsx::writeData(wb, "Resumo", x = "Preço de Referência Estimado", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = glue::glue('=IF(B{linha_cv}>0.25, B{linha_mediana}, B{linha_media})'))
  f_moeda <- c(f_moeda, linha_atual)
  linha_atual <- linha_atual + 1

  openxlsx::writeData(wb, "Resumo", x = "Preço Final (+34,7%)", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = glue::glue('=B{linha_precoref}*1.347'))
  f_moeda <- c(f_moeda, linha_atual)
  linha_atual <- linha_atual + 1

  openxlsx::writeData(wb, "Resumo", x = "Critério Utilizado", startRow = linha_atual, startCol = 1)
  openxlsx::writeFormula(wb, "Resumo", startRow = linha_atual, startCol = 2,
                         x = glue::glue('=IF(B{linha_cv}>0.25,"Mediana (CV > 25%)","Média (CV <= 25%)")'))
  f_moeda <- c(f_moeda, linha_atual)

  # ---- Aplica estilos ----
  openxlsx::addStyle(wb, "Resumo", style = estilo_normal, rows = f_normal, cols = 2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Resumo", style = estilo_header, rows = f_header, cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Resumo", style = estilo_numero, rows = f_numero, cols = 2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Resumo", style = estilo_moeda, rows = f_moeda, cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Resumo", cols = 1:2, widths = "auto")

  # ---- Nome do arquivo e salvamento ----
  prefixo <- if (sum(dadosPrecos$Status == "Incluído") >= 3) "I1_" else "I13_"
  arquivo <- paste0(prefixo, codItem, "_", gsub(" ", "_", unidade), ".xlsx")
  if (!dir.exists(pasta)) dir.create(pasta, recursive = TRUE)
  openxlsx::saveWorkbook(wb, file.path(pasta, arquivo), overwrite = TRUE)

  message("Arquivo gerado: ", file.path(pasta, arquivo))
  return(file.path(pasta, arquivo))
}
