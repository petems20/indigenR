#' Analisar preços de um item de material (unidade de fornecimento única)
#'
#' Esta função realiza a análise de preços históricos de um item do CATMAT,
#' realizando saneamento de outliers, padronização de unidades e cálculo de estatísticas
#' para a unidade de fornecimento predominante. Opcionalmente, salva os resultados em
#' arquivo Excel. Para analisar múltiplas unidades de fornecimento de uma vez, veja
#' \code{\link{analisar_item_catmat_completo}}.
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
#' @importFrom dplyr mutate coalesce group_by summarise n arrange desc slice filter select
#' @importFrom tibble tibble
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

    # 4. Saneamento de outliers (IQR), via sanear_precos()
    saneamento <- sanear_precos(dadosPrecos, marca_excluidos = TRUE)

    dadosPrecosSaneados <- saneamento %>%
      dplyr::filter(Status == "Incluído") %>%
      dplyr::select(-Q1, -Q3, -IQR, -limite_inferior, -limite_superior, -Status, -Justificativa)

    precos_excluidos <- saneamento %>%
      dplyr::filter(Status == "Excluido") %>%
      dplyr::select(-Q1, -Q3, -IQR, -limite_inferior, -limite_superior, -Status)

    # Validação pós-saneamento
    if (nrow(dadosPrecosSaneados) < 3) {
      cat("Falha: Menos de 3 preços após saneamento para o código", codigo_catmat, "\n")
      return(NULL)
    }

    # 5. Estatísticas e preço de referência (média x mediana por CV)
    ref <- .calcula_preco_referencia(dadosPrecosSaneados$precoUnitario)
    n_amostra <- nrow(dadosPrecosSaneados)

    # 6. Criar resumo
    resumo_item <- tibble::tibble(
      CATMAT = codigo_catmat,
      Descrição = dadosPrecos$descricaoItem[1],
      Grupo = dadosPrecos$nomeClasse[1],
      `N Amostral Total` = nrow(dadosPrecos),
      `N Preços Excluídos` = nrow(precos_excluidos),
      `N Amostra Saneada` = n_amostra,
      Média = round(ref$media, 4),
      Mediana = round(ref$mediana, 4),
      `Desvio Padrão` = round(ref$desvio_padrao, 4),
      `Coeficiente de Variação (%)` = paste0(round(ref$coef_variacao * 100, 2), "%"),
      `Preço de Referência Estimado` = round(ref$preco_referencia, 4),
      `Critério Utilizado` = ref$criterio_usado,
      `Unidade padronizada?` = ifelse(unidadepredominante$N >= 3, "Sim", "Não"),
      `Unidade Predominante` = ifelse(unidadepredominante$N >= 3,
                                      unidadepredominante$unidadeFornecimento, NA)
    )

    # 7. Salvar em Excel
    if (salvar) {
      nomeclasse <- resumo_item$Grupo[1]
      dir.create(file.path(pasta, nomeclasse), showWarnings = FALSE, recursive = TRUE)
      arquivo <- file.path(pasta, nomeclasse, paste0(codigo_catmat, ".xlsx"))

      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, "Informações")
      info_df <- data.frame(
        Campo = c("Responsável pela Pesquisa:", "Data e Hora da Pesquisa:"),
        Valor = c(nome_do_responsavel, format(Sys.time(), "%Y-%m-%d %H:%M"))
      )
      openxlsx::writeData(wb, "Informações", info_df, startRow = 1, colNames = FALSE)
      openxlsx::writeData(wb, "Informações", resumo_item, startRow = 4, colNames = TRUE)
      openxlsx::setColWidths(wb, "Informações", cols = 1:2, widths = "auto")

      openxlsx::addWorksheet(wb, "Preços Saneados")
      openxlsx::writeData(wb, "Preços Saneados", dadosPrecosSaneados)
      openxlsx::setColWidths(wb, "Preços Saneados", cols = 1:ncol(dadosPrecosSaneados), widths = "auto")

      openxlsx::addWorksheet(wb, "Preços Excluídos")
      openxlsx::writeData(wb, "Preços Excluídos", precos_excluidos)
      openxlsx::setColWidths(wb, "Preços Excluídos", cols = 1:ncol(precos_excluidos), widths = "auto")

      openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
      cat("Arquivo salvo em:", arquivo, "\n")
    }

    cat("Sucesso: Análise do código", codigo_catmat, "concluída.\n")

    # 8. Retornos
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

#' Analisar preços de um item do CATMAT, em todas as unidades de fornecimento qualificadas
#'
#' Versão mais completa de \code{\link{analisar_item_material}}: em vez de considerar
#' apenas a unidade de fornecimento predominante, analisa \strong{todas} as unidades com
#' pelo menos 3 registros, suporta materiais e serviços, permite restringir a uma
#' modalidade de licitação preferida e ajusta automaticamente o período (3/6/9/12 meses)
#' para o menor intervalo com amostra suficiente.
#'
#' @param codigo_catmat Código do item no catálogo (CATMAT ou serviço).
#' @param nome_do_responsavel Nome do responsável pela pesquisa.
#' @param pasta Caminho da pasta onde os arquivos serão salvos, quando \code{salvar = TRUE}.
#' @param salvar Lógico. Se TRUE, salva os dados e resumo em arquivos Excel por unidade de fornecimento.
#' @param servico Se TRUE, consulta serviços em vez de materiais.
#' @param meses Número de meses anteriores considerados na busca inicial.
#' @param estado Filtrar por estado (opcional).
#' @param regiao Filtrar por região (opcional).
#' @param return_dados Lógico. Se TRUE, retorna também os dados saneados/excluídos de cada
#'   unidade de fornecimento (ver Value).
#' @param unidadeF Vetor de unidades de fornecimento a analisar (opcional). Se \code{NULL},
#'   usa todas as unidades com pelo menos 3 registros.
#' @param ajuste_periodo Lógico. Se TRUE, usa o menor período (3/6/9/12 meses) com amostra
#'   de pelo menos 3 preços válidos.
#' @param preferencia_modalidade Código da modalidade de licitação preferida (opcional);
#'   só é aplicado se restarem pelo menos 3 registros nessa modalidade.
#'
#' @return Um tibble com uma linha de resumo por unidade de fornecimento analisada ou,
#' se \code{return_dados = TRUE}, uma lista com:
#' \itemize{
#'   \item \code{resumo}: o mesmo tibble de resumo;
#'   \item \code{detalhes}: lista nomeada por unidade de fornecimento, cada elemento com
#'     \code{dadosSaneados} e \code{dadosExcluidos}.
#' }
#'
#' @importFrom dplyr rename mutate case_when select group_by summarise arrange desc filter
#'   pull first distinct if_else
#' @importFrom tibble tibble
#' @importFrom lubridate time_length interval today
#' @importFrom purrr map_int
#' @importFrom stringr str_trim
#' @export
analisar_item_catmat_completo <- function(
    codigo_catmat,
    nome_do_responsavel = "Nome",
    pasta = "PesquisadePrecos",
    salvar = FALSE,
    servico = FALSE,
    meses = 12,
    estado = NULL,
    regiao = NULL,
    return_dados = FALSE,
    unidadeF = NULL,
    ajuste_periodo = TRUE,
    preferencia_modalidade = NULL
) {

  tabela_modalidades <- data.frame(
    codigo = c(1, 2, 3, 4, 5, 6, 7, 12, 20, 22, 33, 44, 57),
    descricao = c(
      "CONVITE", "TOMADA DE PREÇOS", "CONCORRÊNCIA", "CONCORRÊNCIA INTERNACIONAL",
      "PREGÃO", "DISPENSA DE LICITAÇÃO", "INEXIGIBILIDADE DE LICITAÇÃO", "CREDENCIAMENTO",
      "CONCURSO", "TOMADA DE PREÇOS POR TÉCNICA E PREÇO", "CONCORRÊNCIA POR TÉCNICA E PREÇO",
      "CONCORRÊNCIA INTERNACIONAL POR TÉCNICA E PREÇO", "CONVÊNIO"
    ),
    stringsAsFactors = FALSE
  )

  resumo_completo <- tibble::tibble()
  detalhes_por_unidade <- list()

  cat("\n--- Analisando o código CATMAT:", codigo_catmat, "---\n")

  tryCatch({

    # 1. Busca os dados do item
    dadosBrutos <- consultar_precos(codigo_item_catalogo = codigo_catmat, meses = meses, servico = servico, estado = estado, regiao = regiao)

    if (is.null(dadosBrutos) || nrow(dadosBrutos) < 3) {
      cat("Falha: Foram encontrados menos de 3 preços para o código", codigo_catmat, ". Pulando para o próximo.\n")
      return(NULL)
    }

    if (servico) {
      dadosBrutos <- dadosBrutos |>
        dplyr::rename("nomeUnidadeFornecimento" = "nomeUnidadeMedida") |>
        dplyr::mutate(unidadeFornecimento = nomeUnidadeFornecimento)
    } else {
      dadosBrutos <- dadosBrutos |>
        dplyr::mutate(
          siglaUnidadeMedidaAdaptado = dplyr::case_when(
            nomeUnidadeFornecimento == 'QUILOGRAMA' ~ 'KG',
            is.na(siglaUnidadeMedida) ~ siglaUnidadeFornecimento,
            TRUE ~ siglaUnidadeMedida
          ),
          capacidadeUnidadeFornecimentoAdaptado = dplyr::case_when(
            capacidadeUnidadeFornecimento == 0 ~ 1,
            TRUE ~ capacidadeUnidadeFornecimento
          ),
          unidadeFornecimento = trimws(paste0(
            dplyr::if_else(is.na(capacidadeUnidadeFornecimentoAdaptado) | capacidadeUnidadeFornecimentoAdaptado == 0,
                    "", as.character(capacidadeUnidadeFornecimentoAdaptado)), " ",
            dplyr::if_else(is.na(siglaUnidadeMedidaAdaptado) | siglaUnidadeMedidaAdaptado == 0,
                    "", as.character(siglaUnidadeMedidaAdaptado))
          ))
        ) |>
        dplyr::select(-siglaUnidadeMedidaAdaptado, -capacidadeUnidadeFornecimentoAdaptado)
    }

    # OBTENDO UNIDADES DE FORNECIMENTO ----
    if (is.null(unidadeF)) {
      unidades_fornecimento <- dadosBrutos |>
        dplyr::group_by(unidadeFornecimento) |>
        dplyr::summarise(N = dplyr::n()) |>
        dplyr::arrange(dplyr::desc(N)) |>
        dplyr::filter(N >= 3) |>
        dplyr::select(unidadeFornecimento) |>
        dplyr::pull()
    } else {
      unidades_fornecimento <- unidadeF
    }

    # LOOP PARA CADA UNIDADE DE FORNECIMENTO IDENTIFICADA ----
    for (unidadepredominante in unidades_fornecimento) {

      dadosPrecos <- dadosBrutos |> dplyr::filter(unidadeFornecimento == unidadepredominante)
      cat("Total de", nrow(dadosPrecos), "registros após padronização da unidade de fornecimento predominante", paste0("(", stringr::str_trim(unidadepredominante), ").\n"))

      # AJUSTE PARA A MODALIDADE DE PREFERÊNCIA ----
      if (!is.null(preferencia_modalidade) && isTRUE(preferencia_modalidade %in% tabela_modalidades$codigo)) {
        n_modalidade_preferida <- dadosPrecos |> dplyr::filter(modalidade == preferencia_modalidade) |> dplyr::summarise(N = dplyr::n()) |> dplyr::pull(N)
        if (n_modalidade_preferida >= 3) {
          dadosPrecos <- dadosPrecos |> dplyr::filter(modalidade == preferencia_modalidade)
          cat("Total de", nrow(dadosPrecos), "registros após ajuste para modalidade preferida", paste0("(", tabela_modalidades$descricao[tabela_modalidades$codigo == preferencia_modalidade], ")"), ".\n")
          out_modalidade <- tabela_modalidades$descricao[tabela_modalidades$codigo == preferencia_modalidade]
        } else {
          out_modalidade <- "TODAS"
        }
      } else if (!is.null(preferencia_modalidade)) {
        out_modalidade <- "TODAS"
        cat("Insira um valor válido para a modalidade de preferência.\n", paste(tabela_modalidades$codigo, tabela_modalidades$descricao, '\n'))
      } else {
        out_modalidade <- "TODAS"
      }

      dadosPrecos <- dadosPrecos |>
        dplyr::mutate(
          meses = lubridate::time_length(lubridate::interval(dataResultado, lubridate::today()), "months"),
          periodo = dplyr::case_when(
            meses <= 3 ~ 3, meses <= 6 ~ 6, meses <= 9 ~ 9, meses <= 12 ~ 12,
            TRUE ~ ceiling(meses)
          )
        )

      # AJUSTE DE PERÍODO (MENOR PERÍODO VÁLIDO) ----
      if (ajuste_periodo) {
        tabela_periodos <- dadosPrecos |>
          dplyr::distinct(periodo) |>
          dplyr::mutate(N = purrr::map_int(periodo, ~ sum(dadosPrecos$meses <= .x))) |>
          dplyr::arrange(periodo) |>
          dplyr::filter(N >= 3)

        if (nrow(tabela_periodos) > 0) menor_periodo_valido <- min(tabela_periodos$periodo)

        dadosPrecos <- dadosPrecos |> dplyr::filter(periodo <= menor_periodo_valido)
        cat("Total de", nrow(dadosPrecos), "registros após ajuste de período", paste0("(", menor_periodo_valido, " meses)"), ".\n")
      } else {
        menor_periodo_valido <- max(dadosPrecos$periodo)
      }

      # SANEAMENTO DE OUTLIERS (IQR), via sanear_precos() ----
      saneamento <- sanear_precos(dadosPrecos, marca_excluidos = TRUE)

      dadosPrecosSaneados <- saneamento |>
        dplyr::filter(Status == "Incluído") |>
        dplyr::select(-Q1, -Q3, -IQR, -limite_inferior, -limite_superior, -Status, -Justificativa)

      precos_excluidos <- saneamento |>
        dplyr::filter(Status == "Excluido") |>
        dplyr::select(-Q1, -Q3, -IQR, -limite_inferior, -limite_superior, -Status)

      # Validação pós-saneamento (mantém o comportamento original: interrompe toda a
      # análise do CATMAT, mesmo que outras unidades já tenham sido processadas)
      if (nrow(dadosPrecosSaneados) < 3) {
        cat("Falha: Menos de 3 preços restantes após o saneamento para o código", codigo_catmat, ". Pulando.\n")
        return(NULL)
      }

      ref <- .calcula_preco_referencia(dadosPrecosSaneados$precoUnitario)
      n_amostra <- nrow(dadosPrecosSaneados)

      resumo_item <- tibble::tibble(
        `CATMAT` = codigo_catmat,
        `Descrição` = dplyr::first(dadosPrecos$descricaoItem),
        `Grupo` = if (servico) "Não se aplica" else dplyr::first(dadosPrecos$nomeClasse),
        `N Amostral Total` = nrow(dadosPrecos),
        `N Preços Excluídos` = nrow(precos_excluidos),
        `N Amostra Saneada` = n_amostra,
        `Média` = round(ref$media, 4),
        `Mediana` = round(ref$mediana, 4),
        `Desvio Padrão` = round(ref$desvio_padrao, 4),
        `Coeficiente de Variação (%)` = paste0(round(ref$coef_variacao * 100, 2), "%"),
        `Preço de Referência Estimado` = round(ref$preco_referencia, 4),
        `Critério Utilizado` = ref$criterio_usado,
        `Unidade de Fornecimento` = unidadepredominante,
        `Período` = paste(menor_periodo_valido, 'Meses'),
        `Modalidade` = out_modalidade
      )

      resumo_completo <- rbind(resumo_completo, resumo_item)
      detalhes_por_unidade[[unidadepredominante]] <- list(
        dadosSaneados = dadosPrecosSaneados,
        dadosExcluidos = precos_excluidos
      )

      # SALVANDO DADOS ----
      if (salvar) {
        nomeclasse <- dplyr::first(dadosPrecos$nomeClasse)
        if (!dir.exists(pasta)) dir.create(pasta)
        if (!dir.exists(file.path(pasta, nomeclasse))) dir.create(file.path(pasta, nomeclasse))

        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Informações")
        info_df <- data.frame(
          Campo = c("Responsável pela Pesquisa:", "Data e Hora da Pesquisa:"),
          Valor = c(nome_do_responsavel, as.character(format(Sys.time(), "%Y-%m-%d %H:%M")))
        )
        resumo_info <- tibble::tibble(
          `Resumo da Pesquisa` = c(
            "CATMAT", "Descrição", "Grupo", "N Amostral Total", "N Preços Excluídos",
            "N Amostra Saneada", "Média", "Mediana", "Desvio Padrão",
            "Coeficiente de Variação (%)", "Preço de Referência Estimado",
            "Critério Utilizado", "Unidade de Fornecimento", "Período", "Modalidade"
          ),
          ` ` = list(
            codigo_catmat,
            dplyr::first(dadosPrecos$descricaoItem),
            if (servico) "Não se aplica" else dplyr::first(dadosPrecos$nomeClasse),
            nrow(dadosPrecos),
            nrow(precos_excluidos),
            n_amostra,
            round(ref$media, 4),
            round(ref$mediana, 4),
            round(ref$desvio_padrao, 4),
            round(ref$coef_variacao * 100, 2),
            round(ref$preco_referencia, 4),
            ref$criterio_usado,
            unidadepredominante,
            paste(menor_periodo_valido, "Meses"),
            out_modalidade
          )
        )

        openxlsx::writeData(wb, sheet = "Informações", x = info_df, startRow = 1, colNames = FALSE)
        openxlsx::writeData(wb, sheet = "Informações", x = resumo_info, startRow = 4, colNames = TRUE)
        openxlsx::setColWidths(wb, sheet = "Informações", cols = 1:2, widths = "auto")

        openxlsx::addWorksheet(wb, "Preços Saneados")
        openxlsx::writeData(wb, sheet = "Preços Saneados", x = dadosPrecosSaneados)
        openxlsx::setColWidths(wb, sheet = "Preços Saneados", cols = 1:ncol(dadosPrecosSaneados), widths = "auto")

        openxlsx::addWorksheet(wb, "Preços Excluídos")
        openxlsx::writeData(wb, sheet = "Preços Excluídos", x = precos_excluidos)
        openxlsx::setColWidths(wb, sheet = "Preços Excluídos", cols = 1:ncol(precos_excluidos), widths = "auto")

        openxlsx::saveWorkbook(wb, file.path(pasta, nomeclasse, paste0(codigo_catmat, '_', gsub(" ", "_", unidadepredominante), '.xlsx')), overwrite = TRUE)
      }

      cat("Sucesso: Análise do código", codigo_catmat, "concluída.\n")
    }

  }, error = function(e) {
    cat("ERRO INESPERADO ao processar o código", codigo_catmat, ":", e$message, "\n")
    return(NULL)
  })

  if (return_dados) {
    return(list(resumo = resumo_completo, detalhes = detalhes_por_unidade))
  }

  return(resumo_completo)
}
