

regressao_estado <- function(catmat, meses = 12){
  t <- analisar_item_catmat(catmat, meses = meses, salvar = FALSE, return = TRUE)


  dadosPrecosSaneados <- mutate(dadosPrecosSaneados, value = 1) %>%
    pivot_wider(names_from=estado, values_from=value, values_fill = 0, names_prefix = "duf_")

  regressores <- grep("^duf_", names(dadosPrecosSaneados), value = TRUE)

  regressores <- regressores[-1]

  fml <- as.formula(
    paste("precoUnitario ~", paste(sprintf("`%s`", regressores), collapse = " + "))
  )

  # roda o modelo
  modelo <<- lm(fml, data = dadosPrecosSaneados)
  print(t)
  return(summary(modelo))
}

regressao_regiao <- function(catmat, meses = 12){
  t <- analisar_item_catmat(catmat, meses = meses, salvar = FALSE, return = TRUE)


  dadosPrecosSaneados <- mutate(dadosPrecosSaneados, value = 1) %>%
    pivot_wider(names_from=Região, values_from=value, values_fill = 0)

  regressores <- grep("^Região", names(dadosPrecosSaneados), value = TRUE)


  regressores <- regressores[!grepl("Sudeste", regressores)]

  fml <- as.formula(
    paste("precoUnitario ~", paste(sprintf("`%s`", regressores), collapse = " + "))
  )

  # roda o modelo
  modelo <<- lm(fml, data = dadosPrecosSaneados)
  print(t)
  return(summary(modelo))
}


buscar_dados_catmat <- function(catmats, max_tentativas = 5, tempo_base_espera = 1, pausa_entre_requests = 0.5, verbose = TRUE) {

  resultados_lista <- list() # Lista para armazenar os dataframes de cada CATMAT

  for (catmat in catmats) {

    if (verbose) {
      cat("------------------------------------\n")
      cat("Verificando CATMAT:", catmat, "\n")
    }

    tentativas <- 0
    sucesso <- FALSE

    # Loop de tentativas para o CATMAT atual
    repeat {
      tentativas <- tentativas + 1

      if (tentativas > max_tentativas) {
        if (verbose) cat("ERRO: Número máximo de tentativas atingido para o CATMAT", catmat, ". Pulando para o próximo.\n")
        break
      }

      query_params <- list(pagina = 1, tamanhoPagina = 10, codigoItem = catmat, bps = 'false')
      base_url <- "https://dadosabertos.compras.gov.br/modulo-material/4_consultarItemMaterial"

      if (verbose) cat("  Tentativa", tentativas, "...\n")

      # Usamos tryCatch para capturar erros de conexão que o GET pode gerar
      resposta <- tryCatch({
        GET(url = base_url, query = query_params, timeout(15)) # Adicionado timeout
      }, error = function(e) {
        if (verbose) cat("  ERRO DE CONEXÃO:", e$message, "\n")
        return(NULL) # Retorna NULL em caso de erro de conexão
      })

      if (is.null(resposta)) { # Se houve erro de conexão, tenta novamente
        Sys.sleep(tempo_base_espera * tentativas) # Espera um pouco antes de tentar de novo
        next
      }

      if (resposta$status_code == 200) {
        dadostemp <- fromJSON(content(resposta, "text", encoding = "UTF-8"))$resultado

        if (is.null(dadostemp) || (is.data.frame(dadostemp) && nrow(dadostemp) == 0)) {
          if (verbose) cat("  Sucesso: Nenhum resultado encontrado para o CATMAT", catmat, ".\n")
        } else {
          # Adiciona uma coluna com o código do catmat para referência
          resultados_lista[[as.character(catmat)]] <- dadostemp
          if (verbose) cat("  Sucesso: Dados do CATMAT", catmat, "coletados.\n")
        }

        sucesso <- TRUE
        break

      } else if (resposta$status_code == 429) {
        tempo_espera <- tempo_base_espera * (2 ^ (tentativas - 1))
        if (verbose) cat("  AVISO: Status 429 (Too Many Requests). Esperando", tempo_espera, "s para tentar novamente...\n")
        Sys.sleep(tempo_espera)

      } else {
        if (verbose) {
          cat("  ERRO: Status", resposta$status_code, "para o CATMAT", catmat, "\n")
          cat("  Este não é um erro recuperável. Pulando para o próximo CATMAT.\n")
        }
        break
      }
    }

    if (sucesso) {
      Sys.sleep(pausa_entre_requests)
    }
  }

  # --- Finalização ---
  if (length(resultados_lista) == 0) {
    if (verbose) warning("Nenhum dado foi retornado para os CATMATs fornecidos.", call. = FALSE)
    return(data.frame()) # Retorna um dataframe vazio para consistência de tipo
  }

  # Combina a lista de dataframes em um único, usando dplyr::bind_rows que é seguro
  df_completo <- dplyr::bind_rows(resultados_lista)

  if (verbose) {
    cat("------------------------------------\n")
    cat("Busca finalizada. Total de", nrow(df_completo), "registros retornados.\n")
  }

  return(df_completo)
}


analisar_item_catmat_completo <- function(codigo_catmat, nome_do_responsavel = "Pedro Henrique Medeiros dos Santos", pasta = "PesquisadePrecos", salvar = FALSE, servico = FALSE, meses = 12, estado = NULL, regiao = NULL, return = FALSE, unidadeF = NULL, ajuste_periodo = TRUE, preferencia_modalidade = NULL) {

  tabela_modalidades <- data.frame(
    codigo = c(
      1, 2, 3, 4, 5, 6, 7, 12,
      20, 22, 33, 44, 57
    ),
    descricao = c(
      "CONVITE",
      "TOMADA DE PREÇOS",
      "CONCORRÊNCIA",
      "CONCORRÊNCIA INTERNACIONAL",
      "PREGÃO",
      "DISPENSA DE LICITAÇÃO",
      "INEXIGIBILIDADE DE LICITAÇÃO",
      "CREDENCIAMENTO",
      "CONCURSO",
      "TOMADA DE PREÇOS POR TÉCNICA E PREÇO",
      "CONCORRÊNCIA POR TÉCNICA E PREÇO",
      "CONCORRÊNCIA INTERNACIONAL POR TÉCNICA E PREÇO",
      "CONVÊNIO"
    ),
    stringsAsFactors = FALSE
  )


  resumo_completo <- tibble()
  # codigo_catmat = 223575
  # nome_do_responsavel = "Pedro Henrique Medeiros dos Santos"
  # pasta = "PesquisadePrecos"
  # salvar = TRUE
  # servico = FALSE
  # meses = 12
  # estado = NULL
  # regiao = NULL
  # return = FALSE
  # unidadeF = NULL
  # preferencia_modalidade = NULL


  cat("\n--- Analisando o código CATMAT:", codigo_catmat, "---\n")

  # Bloco tryCatch para capturar erros. Se algo falhar, o loop não irá parar.
  tryCatch({

    # 1. Executa a função para buscar os dados do item
    dadosBrutos <- consultar_precos(codigo_item_catalogo = codigo_catmat, meses = meses, servico = servico, estado = estado, regiao = regiao)



    # Validação: Verifica se foram retornados dados e se há pelo menos 3 preços
    if (is.null(dadosBrutos) || nrow(dadosBrutos) < 3) {
      cat("Falha: Foram encontrados menos de 3 preços para o código", codigo_catmat, ". Pulando para o próximo.\n")
      return(NULL) # Retorna NULL se não houver dados suficientes
    }

    if(servico){
      dadosBrutos <- dadosBrutos |>
        rename("nomeUnidadeFornecimento" = "nomeUnidadeMedida") |>
        mutate(unidadeFornecimento = nomeUnidadeFornecimento)
    } else{
      dadosBrutos <- dadosBrutos |>
        mutate(
          siglaUnidadeMedidaAdaptado = case_when(

            nomeUnidadeFornecimento == 'QUILOGRAMA' ~ 'KG',
            is.na(siglaUnidadeMedida) ~ siglaUnidadeFornecimento,
            TRUE ~ siglaUnidadeMedida
          ),

          capacidadeUnidadeFornecimentoAdaptado = case_when(
            capacidadeUnidadeFornecimento == 0 ~ 1,
            TRUE ~ capacidadeUnidadeFornecimento
          ),


          unidadeFornecimento = trimws(paste0(
            # substitui NA e 0 por ""
            #if_else(is.na(nomeUnidadeFornecimento) | nomeUnidadeFornecimento == 0,
            #        "", as.character(nomeUnidadeFornecimento)), " ",
            if_else(is.na(capacidadeUnidadeFornecimentoAdaptado) | capacidadeUnidadeFornecimentoAdaptado == 0,
                    "", as.character(capacidadeUnidadeFornecimentoAdaptado))," ",
            if_else(is.na(siglaUnidadeMedidaAdaptado) | siglaUnidadeMedidaAdaptado == 0,
                    "", as.character(siglaUnidadeMedidaAdaptado))
          ))



        ) |> select(-siglaUnidadeMedidaAdaptado, capacidadeUnidadeFornecimentoAdaptado)


    }




    # OBTENDO UNIDADES DE FORNECIMENTO ----
    if(is.null(unidadeF)){
      unidades_fornecimento <- dadosBrutos |> group_by(unidadeFornecimento) |> summarise(N = n()) |> arrange(desc(N)) |> filter(N >= 3) |>  select(unidadeFornecimento) |> pull()
    } else{
      unidades_fornecimento <- unidadeF
    }


    # LOOP PARA CADA UNIDADE DE FORNECIMENTO IDENTIFICADA ----
    for(unidadepredominante in unidades_fornecimento){
      print(unidadepredominante)

      dadosPrecos <- dadosBrutos |> filter(unidadeFornecimento == unidadepredominante)
      cat("Total de", nrow(dadosPrecos), "registros após padronização da unidade de fornecimento predominante", paste0("(",str_trim(unidadepredominante),").\n"))

      info_unidadepadrao <- tibble(
        `Unidade de Fornecimento` =  unidadepredominante
      )


      # AJUSTANTO PARA A MODALIDADE DE PREFERÊNCIA ----
      if(!is.null(preferencia_modalidade) & isTRUE(preferencia_modalidade %in% tabela_modalidades$codigo)){
          n_modalidade_preferida = dadosPrecos |> filter(modalidade == preferencia_modalidade) |> summarise(N = n()) |> pull(N)
          if(n_modalidade_preferida >= 3){
            dadosPrecos <- dadosPrecos |> filter(modalidade == preferencia_modalidade)
            cat("Total de", nrow(dadosPrecos), "registros após ajuste para modalidade preferida",paste0("(",tabela_modalidades$descricao[tabela_modalidades$codigo==preferencia_modalidade],")"), ".\n")
            out_modalidade <- tabela_modalidades$descricao[tabela_modalidades$codigo == preferencia_modalidade]
          } else{
            out_modalidade <- "TODAS"
          }
      } else if(!is.null(preferencia_modalidade)) {
        out_modalidade <- "TODAS"
        cat("Insira um valor válido para a modalidade de preferência.\n", paste(tabela_modalidades$codigo, tabela_modalidades$descricao, '\n') )
      } else{
        out_modalidade <- "TODAS"
      }


      dadosPrecos <- dadosPrecos |>
        mutate(
          meses = time_length(interval(dataResultado, today()), "months"),
          periodo = case_when(
            meses <= 3 ~ 3,
            meses <= 6 ~ 6,
            meses <= 9 ~ 9,
            meses <= 12 ~ 12,
            TRUE ~ ceiling(meses)   # ou outro critério
          )
        )

      # AJUSTE DE PERÍODO (MENOR TRIMESTRE VÁLIDO) ----
      if(ajuste_periodo == TRUE){


        # Criar tabela com contagem acumulada por período
        tabela_periodos <- dadosPrecos |>
          distinct(periodo) |>                           # lista de períodos existentes
          mutate(
            N = map_int(periodo, ~ sum(dadosPrecos$meses <= .x))   # contagem acumulada
          ) |>
          arrange(periodo) |>
          filter(N >= 3)

        if(nrow(tabela_periodos) > 0) menor_periodo_valido <- min(tabela_periodos$periodo)

        # Filtrar o dataset final
        dadosPrecos <- dadosPrecos |> filter(periodo <= menor_periodo_valido)
        cat("Total de", nrow(dadosPrecos), "registros após ajuste de período",paste0("(",menor_periodo_valido," meses)"), ".\n")

      } else{
        menor_periodo_valido <- max(dadosPrecos$periodo)
      }



      # 2. Saneamento de Outliers (método IQR)
      Q1 <- quantile(dadosPrecos$precoUnitario, 0.25, na.rm = TRUE)
      Q3 <- quantile(dadosPrecos$precoUnitario, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      limite_inferior <- Q1 - 1.5 * IQR
      limite_superior <- Q3 + 1.5 * IQR



      if(return){
        dadosPrecosSaneados <<- dadosPrecos %>%
          filter(precoUnitario >= limite_inferior & precoUnitario <= limite_superior)
      } else {
        dadosPrecosSaneados <- dadosPrecos %>%
          filter(precoUnitario >= limite_inferior & precoUnitario <= limite_superior)
      }




      precos_excluidos <- anti_join(dadosPrecos, dadosPrecosSaneados, by = join_by(idCompra)) |>
        mutate(Justificativa = if_else(precoUnitario <= limite_inferior, "Inexequível", "Excessivamente Elevado"))

      # Validação pós-saneamento
      if (nrow(dadosPrecosSaneados) < 3) {
        cat("Falha: Menos de 3 preços restantes após o saneamento para o código", codigo_catmat, ". Pulando.\n")
        return(NULL)
      }


      # 3. Cálculos Estatísticos
      n_amostra <- nrow(dadosPrecosSaneados)
      media <- mean(dadosPrecosSaneados$precoUnitario, na.rm = TRUE)
      mediana <- median(dadosPrecosSaneados$precoUnitario, na.rm = TRUE)
      desvio_padrao <- sd(dadosPrecosSaneados$precoUnitario, na.rm = TRUE)

      coef_variacao <- if (!is.na(media) && media > 0) (desvio_padrao / media) else 0

      if (is.na(coef_variacao)) coef_variacao <- 0 # Evitar NA se o desvio padrão for NA

      # 4. Determinação do Preço de Referência
      if (coef_variacao <= 0.25) {
        preco_referencia <- media
        criterio_usado <- "Média Aritmética (CV <= 25%)"
      } else {
        preco_referencia <- mediana
        criterio_usado <- "Mediana (CV > 25%)"
      }


      # 5. Criação do Resumo Final
      resumo_item <- tibble(
        `CATMAT` = codigo_catmat,
        `Descrição` = first(dadosPrecos$descricaoItem), # Pega a primeira descrição
        `Grupo` = if(servico) "Não se aplica" else first(dadosPrecos$nomeClasse),
        `N Amostral Total` = nrow(dadosPrecos),
        `N Preços Excluídos` = nrow(precos_excluidos),
        `N Amostra Saneada` = n_amostra,
        `Média` = round(media, 4),
        `Mediana` = round(mediana, 4),
        `Desvio Padrão` = round(desvio_padrao, 4),
        `Coeficiente de Variação (%)` = paste0(round(coef_variacao * 100, 2), "%"),
        `Preço de Referência Estimado` = round(preco_referencia, 4),
        `Critério Utilizado` = criterio_usado,
        `Unidade de Fornecimento` = unidadepredominante,
        `Período` = paste(menor_periodo_valido, 'Meses'),
        `Modalidade` = out_modalidade
      )

      resumo_completo <- rbind(resumo_completo, resumo_item)

      # SALVANDO DADOS
      if(salvar == TRUE){
        nomeclasse <- first(dadosPrecos$nomeClasse)
        if(!file.exists(pasta)) dir.create(pasta)
        if(!file.exists(file.path(pasta, nomeclasse))) dir.create(file.path(pasta, nomeclasse))

        wb <- createWorkbook()

        addWorksheet(wb, "Informações")
        info_df <- data.frame(
          Campo = c("Responsável pela Pesquisa:", "Data e Hora da Pesquisa:"),
          Valor = c(nome_do_responsavel, as.character(format(Sys.time(), "%Y-%m-%d %H:%M")))
        )

        resumo_info <<- tibble(
          `Resumo da Pesquisa` = c(
            "CATMAT",
            "Descrição",
            "Grupo",
            "N Amostral Total",
            "N Preços Excluídos",
            "N Amostra Saneada",
            "Média",
            "Mediana",
            "Desvio Padrão",
            "Coeficiente de Variação (%)",
            "Preço de Referência Estimado",
            "Critério Utilizado",
            "Unidade de Fornecimento",
            "Período",
            "Modalidade"
          ),
          ` ` = list(
            codigo_catmat,                           # int/chr
            first(dadosPrecos$descricaoItem),        # chr
            if(servico) "Não se aplica" else first(dadosPrecos$nomeClasse),           # chr
            nrow(dadosPrecos),                       # int
            nrow(precos_excluidos),                  # int
            n_amostra,                               # int
            round(media, 4),                         # num
            round(mediana, 4),                       # num
            round(desvio_padrao, 4),                 # num
            round(coef_variacao * 100, 2),           # num
            round(preco_referencia, 4),              # num
            criterio_usado,                           # chr
            unidadepredominante,
            paste(menor_periodo_valido, "Meses"),
            out_modalidade
          )
        )

        writeData(
          wb,
          sheet = "Informações",
          x = info_df,
          startRow = 1,
          colNames = FALSE
        )

        writeData(
          wb,
          sheet = "Informações",
          x = resumo_info,
          startRow = 4,      # Começa a escrever na linha 4
          colNames = TRUE    # Escreve os nomes das colunas "Metrica" e "Valor"
        )
        setColWidths(wb, sheet = "Informações", cols = 1:2, widths = "auto")

        addWorksheet(wb, "Preços Saneados")
        writeData(wb, sheet = "Preços Saneados", x = dadosPrecosSaneados)
        setColWidths(wb, sheet = "Preços Saneados", cols = 1:ncol(dadosPrecosSaneados), widths = "auto")

        addWorksheet(wb, "Preços Excluídos")
        writeData(wb, sheet = "Preços Excluídos", x = precos_excluidos)
        setColWidths(wb, sheet = "Preços Excluídos", cols = 1:ncol(precos_excluidos), widths = "auto")

        saveWorkbook(wb, file.path(pasta, nomeclasse, paste0(codigo_catmat,'_',gsub(" ", "_", unidadepredominante), '.xlsx')), overwrite = TRUE)
      }


      cat("Sucesso: Análise do código", codigo_catmat, "concluída.\n")

      # GERANDO ARQUIVOS DA PESQUISA



    }

  }, error = function(e) {
    # Se ocorrer qualquer erro inesperado durante a análise
    cat("ERRO INESPERADO ao processar o código", codigo_catmat, ":", e$message, "\n")
    return(NULL) # Retorna NULL em caso de erro
  })

  return(resumo_completo)
}


regressao_estado <- function(catmat, meses = 12){
  t <- analisar_item_catmat(catmat, meses = meses, salvar = FALSE, return = TRUE)


  dadosPrecosSaneados <- mutate(dadosPrecosSaneados, value = 1) %>%
    pivot_wider(names_from=estado, values_from=value, values_fill = 0, names_prefix = "duf_")

  regressores <- grep("^duf_", names(dadosPrecosSaneados), value = TRUE)

  regressores <- regressores[-1]

  fml <- as.formula(
    paste("precoUnitario ~", paste(sprintf("`%s`", regressores), collapse = " + "))
  )

  # roda o modelo
  modelo <<- lm(fml, data = dadosPrecosSaneados)
  print(t)
  return(summary(modelo))
}


pesquisa_mista <- function(codigoItem, UnFor = NULL, meses = 12, choose = TRUE, nome_do_responsavel = 'Pedro Henrique Medeiros dos Santos',
                           pasta = 'AVN/tabelas/Pesquisa_Preços/Pesquisas'){

  #codigoItem = 389172 ; meses = 12

  cat("\nPreparando pesquisa mista para o item", codigoItem, "\n")
  dadosBrutos <- consultar_precos(codigo_item_catalogo = codigoItem, meses = meses)

  dadosPrecos <- padroniza_unidadeF(dadosBrutos)

  if(!is.null(UnFor) & any(UnFor %in% dadosPrecos$unidadeFornecimento)){
    dadosPrecos <- dadosPrecos |> filter(unidadeFornecimento == UnFor)
  } else if(choose){
    dadosPrecos <- dadosPrecos |>
      group_by(unidadeFornecimento) |>
      mutate(N = n()) |>
      ungroup() |>
      filter(N == max(N, na.rm = T)) |>
      select(-N)

    cat("\nFoi escolhida a unidade de fornecimento mais frequente:", unique(dadosPrecos$unidadeFornecimento), "\n")

  }

  dadosPrecosSaneados <- sanear_dados(dadosPrecos, marca_excluidos = T)

  dadosPrecosSaneados |> group_by(unidadeFornecimento) |> prepara_arquivo()

}

cria_resumo_pesquisa <- function(pasta = 'AVN/tabelas/Pesquisa_Preços/Pesquisas'){

  fs <- tibble(
    file = list.files(pasta, recursive = T, full.names = T, pattern = 'I(\\d+)_')
  )

  calcula_stats <- function(file){
    print(file)
    dados_resumo <- read.xlsx(file, sheet = 'Resumo', startRow = 5, rowNames = T, colNames = F) |> t()
    inciso_i <- read.xlsx(file, sheet = 'Inciso I')
    inciso_iii <- read.xlsx(file, sheet = 'Inciso III')

    resumo <- tibble(
      CATMAT = if(is_empty(unique(inciso_i$codigoItemCatalogo))) as.integer(dados_resumo[1]) else as.integer(unique(inciso_i$codigoItemCatalogo)),
      UnidadeFornecimento = if(is_empty(unique(inciso_i$unidadeFornecimento))) dados_resumo[2] else unique(inciso_i$unidadeFornecimento),
      Descrição = if(is_empty(unique(inciso_i$descricaoItem))) dados_resumo[3] else unique(inciso_i$descricaoItem),
      Grupo = if(is_empty(unique(inciso_i$nomeClasse))) dados_resumo[4] else unique(inciso_i$nomeClasse),
      `Itens Amostra Original` = length(inciso_i$precoUnitario) + length(inciso_iii$Preço.Unitário),
      `Itens Excluídos` = length(inciso_i |> filter(Status == 'Excluido') |> select(precoUnitario) |> pull()),
      `Itens Incluidos` = length(inciso_i |> filter(Status == 'Incluído') |> select(precoUnitario) |> pull()) + length(inciso_iii$Preço.Unitário),
      Média = mean(c(inciso_i[inciso_i$Status != 'Excluido', ]$precoUnitario, inciso_iii$Preço.Unitário), na.rm = T),
      Mediana = median(c(inciso_i[inciso_i$Status != 'Excluido', ]$precoUnitario, inciso_iii$Preço.Unitário), na.rm = T),
      `Desvio Padrão` = sd(c(inciso_i[inciso_i$Status != 'Excluido', ]$precoUnitario, inciso_iii$Preço.Unitário)),
      CV = `Desvio Padrão`/Média,
      `Preço de Referência` = if_else(CV > 0.25, Mediana, Média, missing = Média),
      `Preço Final` = `Preço de Referência`*1.347,
      Critério = if_else(CV > 0.25, "Mediana (CV > 25%)", "Média (CV <= 25%)", missing = "Média (CV <= 25%)")
    )

    return(resumo)

  }



  resumo <- map_dfr(fs$file, ~calcula_stats(.x))

  fwrite(resumo, file.path(pasta, 'Resumo.csv'), dec = ",", sep = ';')

  return(resumo)
}


cria_diretorio_negados <- function(pasta = 'AVN/tabelas/Pesquisa_Preços/Pesquisas'){
  fs <- tibble(
    file = list.files(pasta, recursive = TRUE, full.names = TRUE, pattern = 'I(\\d+)_')
  ) |>
    mutate(
      nome = basename(fs::path_ext_remove(file))
    ) |>
    extract(
      nome,
      into = c("CATMAT", "Unidade de Fornecimento"),
      # captura o código e junta quantidade + unidade como um segundo grupo
      regex = "I\\d+_([0-9]+)_([0-9\\.]+_[A-Za-z]+)$"
    ) |>
    mutate(
      # transforma "1_UN" → "1 UN"
      `Unidade de Fornecimento` = gsub("_", " ", `Unidade de Fornecimento`)
    )


  fs_negados <- inner_join(
    fs |> mutate(CATMAT = as.numeric(CATMAT)),
    negados |> select(CATMAT, Unidade.de.Fornecimento.Pesquisa.FUNAI),
    by = join_by(CATMAT == CATMAT, `Unidade de Fornecimento` == Unidade.de.Fornecimento.Pesquisa.FUNAI)
  ) |> distinct(file, .keep_all = T)

  destino <- file.path(pasta, 'Pesquisa_Itens_Negados')
  if(!dir.exists(destino)) dir.create(destino)

  for (arq in fs_negados$file) {
    file.copy(from = arq,
              to   = file.path(destino, basename(arq)),
              overwrite = TRUE)   # mude para FALSE se não quiser sobrescrever
  }
}












