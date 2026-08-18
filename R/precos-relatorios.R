#' Busca em lote a existência/descrição de itens de material no catálogo do Compras.gov
#'
#' Consulta, um a um, uma lista de códigos de item de material (CATMAT), com retentativa
#' e backoff exponencial em caso de limite de requisições (HTTP 429).
#'
#' @param catmats Vetor de códigos de item de material a consultar.
#' @param max_tentativas Número máximo de tentativas por CATMAT.
#' @param tempo_base_espera Tempo base (segundos) usado no backoff entre tentativas.
#' @param pausa_entre_requests Pausa (segundos) entre CATMATs consultados com sucesso.
#' @param verbose Se TRUE, imprime o progresso da busca.
#'
#' @return Data frame com os registros do catálogo encontrados para os CATMATs informados.
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom httr GET content timeout
#' @importFrom jsonlite fromJSON
buscar_dados_catmat <- function(catmats, max_tentativas = 5, tempo_base_espera = 1, pausa_entre_requests = 0.5, verbose = TRUE) {

  resultados_lista <- list()

  for (catmat in catmats) {

    if (verbose) {
      cat("------------------------------------\n")
      cat("Verificando CATMAT:", catmat, "\n")
    }

    tentativas <- 0
    sucesso <- FALSE

    repeat {
      tentativas <- tentativas + 1

      if (tentativas > max_tentativas) {
        if (verbose) cat("ERRO: Número máximo de tentativas atingido para o CATMAT", catmat, ". Pulando para o próximo.\n")
        break
      }

      query_params <- list(pagina = 1, tamanhoPagina = 10, codigoItem = catmat, bps = 'false')
      base_url <- "https://dadosabertos.compras.gov.br/modulo-material/4_consultarItemMaterial"

      if (verbose) cat("  Tentativa", tentativas, "...\n")

      resposta <- tryCatch({
        httr::GET(url = base_url, query = query_params, httr::timeout(15))
      }, error = function(e) {
        if (verbose) cat("  ERRO DE CONEXÃO:", e$message, "\n")
        return(NULL)
      })

      if (is.null(resposta)) {
        Sys.sleep(tempo_base_espera * tentativas)
        next
      }

      if (resposta$status_code == 200) {
        dadostemp <- jsonlite::fromJSON(httr::content(resposta, "text", encoding = "UTF-8"))$resultado

        if (is.null(dadostemp) || (is.data.frame(dadostemp) && nrow(dadostemp) == 0)) {
          if (verbose) cat("  Sucesso: Nenhum resultado encontrado para o CATMAT", catmat, ".\n")
        } else {
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

  if (length(resultados_lista) == 0) {
    if (verbose) warning("Nenhum dado foi retornado para os CATMATs fornecidos.", call. = FALSE)
    return(data.frame())
  }

  df_completo <- dplyr::bind_rows(resultados_lista)

  if (verbose) {
    cat("------------------------------------\n")
    cat("Busca finalizada. Total de", nrow(df_completo), "registros retornados.\n")
  }

  return(df_completo)
}

#' Pesquisa de preços de uma única unidade de fornecimento, direto para Excel
#'
#' Atalho que consulta preços de um item, padroniza a unidade de fornecimento, saneia
#' outliers via \code{\link{sanear_precos}} e grava o resultado com
#' \code{\link{prepara_arquivo}}, escolhendo (ou recebendo) uma única unidade de
#' fornecimento.
#'
#' @param codigoItem Código do item no catálogo (CATMAT).
#' @param UnFor Unidade de fornecimento a usar (opcional). Se \code{NULL} e
#'   \code{choose = TRUE}, usa a unidade mais frequente nos dados.
#' @param meses Número de meses anteriores considerados na busca.
#' @param choose Se TRUE (padrão) e \code{UnFor} não for informado ou não existir nos
#'   dados, escolhe automaticamente a unidade de fornecimento mais frequente.
#' @param nome_do_responsavel Nome do responsável pela pesquisa.
#' @param pasta Diretório onde o arquivo Excel será salvo.
#'
#' @return O caminho do arquivo Excel gerado (retorno de \code{\link{prepara_arquivo}}).
#' @export
#' @importFrom dplyr group_by mutate ungroup filter select n
pesquisa_mista <- function(codigoItem, UnFor = NULL, meses = 12, choose = TRUE,
                            nome_do_responsavel = "Nome",
                            pasta = "PesquisadePrecos") {

  cat("\nPreparando pesquisa mista para o item", codigoItem, "\n")
  dadosBrutos <- consultar_precos(codigo_item_catalogo = codigoItem, meses = meses)

  dadosPrecos <- padroniza_unidadeF(dadosBrutos)

  if (!is.null(UnFor) && any(UnFor %in% dadosPrecos$unidadeFornecimento)) {
    dadosPrecos <- dadosPrecos |> dplyr::filter(unidadeFornecimento == UnFor)
  } else if (choose) {
    dadosPrecos <- dadosPrecos |>
      dplyr::group_by(unidadeFornecimento) |>
      dplyr::mutate(N = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(N == max(N, na.rm = TRUE)) |>
      dplyr::select(-N)

    cat("\nFoi escolhida a unidade de fornecimento mais frequente:", unique(dadosPrecos$unidadeFornecimento), "\n")
  }

  dadosPrecosSaneados <- sanear_precos(dadosPrecos, marca_excluidos = TRUE)

  dadosPrecosSaneados |> dplyr::group_by(unidadeFornecimento) |> prepara_arquivo(pasta = pasta)
}

#' Consolida um resumo estatístico a partir de uma pasta de pesquisas de preços já geradas
#'
#' Percorre uma pasta em busca de planilhas geradas por \code{\link{prepara_arquivo}} ou
#' \code{\link{analisar_item_material}} (identificadas pelo padrão de nome \code{I<código>_...}),
#' recalcula estatísticas (média, mediana, desvio padrão, coeficiente de variação e preço de
#' referência) a partir das abas "Inciso I"/"Inciso III" de cada arquivo, e grava um CSV
#' consolidado (\code{Resumo.csv}, separador \code{;}, decimal \code{,}) na mesma pasta.
#'
#' @param pasta Diretório com os arquivos de pesquisa de preços já gerados.
#'
#' @return Tibble com uma linha de resumo por arquivo encontrado (o mesmo conteúdo gravado
#'   em \code{Resumo.csv}).
#' @export
#' @importFrom dplyr filter select pull
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr is_empty
#' @importFrom openxlsx read.xlsx
#' @importFrom readr write_csv2
cria_resumo_pesquisa <- function(pasta = "PesquisadePrecos") {

  fs_arquivos <- tibble::tibble(
    file = list.files(pasta, recursive = TRUE, full.names = TRUE, pattern = "I(\\d+)_")
  )

  calcula_stats <- function(file) {
    dados_resumo <- openxlsx::read.xlsx(file, sheet = "Resumo", startRow = 5, rowNames = TRUE, colNames = FALSE) |> t()
    inciso_i <- openxlsx::read.xlsx(file, sheet = "Inciso I")
    inciso_iii <- openxlsx::read.xlsx(file, sheet = "Inciso III")

    precos_validos <- c(
      inciso_i[inciso_i$Status != "Excluido", ]$precoUnitario,
      inciso_iii$Preço.Unitário
    )
    ref <- .calcula_preco_referencia(precos_validos)

    tibble::tibble(
      CATMAT = if (purrr::is_empty(unique(inciso_i$codigoItemCatalogo))) as.integer(dados_resumo[1]) else as.integer(unique(inciso_i$codigoItemCatalogo)),
      UnidadeFornecimento = if (purrr::is_empty(unique(inciso_i$unidadeFornecimento))) dados_resumo[2] else unique(inciso_i$unidadeFornecimento),
      Descrição = if (purrr::is_empty(unique(inciso_i$descricaoItem))) dados_resumo[3] else unique(inciso_i$descricaoItem),
      Grupo = if (purrr::is_empty(unique(inciso_i$nomeClasse))) dados_resumo[4] else unique(inciso_i$nomeClasse),
      `Itens Amostra Original` = length(inciso_i$precoUnitario) + length(inciso_iii$Preço.Unitário),
      `Itens Excluídos` = length(inciso_i |> dplyr::filter(Status == "Excluido") |> dplyr::select(precoUnitario) |> dplyr::pull()),
      `Itens Incluidos` = length(inciso_i |> dplyr::filter(Status == "Incluído") |> dplyr::select(precoUnitario) |> dplyr::pull()) + length(inciso_iii$Preço.Unitário),
      Média = round(ref$media, 4),
      Mediana = round(ref$mediana, 4),
      `Desvio Padrão` = round(ref$desvio_padrao, 4),
      CV = round(ref$coef_variacao, 4),
      `Preço de Referência` = round(ref$preco_referencia, 4),
      `Preço Final` = round(ref$preco_referencia * 1.347, 4),
      Critério = ref$criterio_usado
    )
  }

  resumo <- purrr::map_dfr(fs_arquivos$file, calcula_stats)

  readr::write_csv2(resumo, file.path(pasta, "Resumo.csv"))

  return(resumo)
}

#' Regressão do preço unitário sobre indicadoras de estado (UF)
#'
#' Reanalisa um CATMAT com \code{\link{analisar_item_catmat_completo}} e ajusta uma
#' regressão linear do preço unitário saneado contra variáveis indicadoras (dummies) de
#' estado (UF), útil para investigar variação regional de preços.
#'
#' @param catmat Código do item no catálogo (CATMAT).
#' @param meses Número de meses anteriores considerados na análise.
#'
#' @return Objeto \code{summary.lm} do modelo ajustado.
#' @export
#' @importFrom dplyr bind_rows mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stats as.formula lm
regressao_estado <- function(catmat, meses = 12) {

  resultado <- analisar_item_catmat_completo(catmat, meses = meses, salvar = FALSE, return_dados = TRUE)

  dados <- dplyr::bind_rows(lapply(resultado$detalhes, `[[`, "dadosSaneados")) |>
    dplyr::mutate(value = 1) |>
    tidyr::pivot_wider(names_from = estado, values_from = value, values_fill = 0, names_prefix = "duf_")

  regressores <- grep("^duf_", names(dados), value = TRUE)
  regressores <- regressores[-1]

  formula_modelo <- stats::as.formula(
    paste("precoUnitario ~", paste(sprintf("`%s`", regressores), collapse = " + "))
  )

  modelo <- stats::lm(formula_modelo, data = dados)
  print(resultado$resumo)
  summary(modelo)
}

#' Regressão do preço unitário sobre indicadoras de região
#'
#' Reanalisa um CATMAT com \code{\link{analisar_item_catmat_completo}} e ajusta uma
#' regressão linear do preço unitário saneado contra variáveis indicadoras (dummies) de
#' região geográfica (categoria de referência: Sudeste).
#'
#' @param catmat Código do item no catálogo (CATMAT).
#' @param meses Número de meses anteriores considerados na análise.
#'
#' @return Objeto \code{summary.lm} do modelo ajustado.
#' @export
#' @importFrom dplyr bind_rows mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stats as.formula lm
regressao_regiao <- function(catmat, meses = 12) {

  resultado <- analisar_item_catmat_completo(catmat, meses = meses, salvar = FALSE, return_dados = TRUE)

  dados <- dplyr::bind_rows(lapply(resultado$detalhes, `[[`, "dadosSaneados")) |>
    dplyr::mutate(value = 1) |>
    tidyr::pivot_wider(names_from = Regiao, values_from = value, values_fill = 0, names_prefix = "regiao_")

  regressores <- grep("^regiao_", names(dados), value = TRUE)
  regressores <- regressores[!grepl("Sudeste", regressores)]

  formula_modelo <- stats::as.formula(
    paste("precoUnitario ~", paste(sprintf("`%s`", regressores), collapse = " + "))
  )

  modelo <- stats::lm(formula_modelo, data = dados)
  print(resultado$resumo)
  summary(modelo)
}
