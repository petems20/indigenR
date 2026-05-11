#' Extrai todos os dados paginados da API de Compras usando lista de parâmetros
#'
#' @description
#' Utiliza a função \code{puxa_api_compras()} para iterar sobre todas as páginas de
#' um endpoint da API de Compras. A URL final é construída dinamicamente a partir
#' de uma lista de parâmetros fornecida pelo usuário. Caso um dos parâmetros seja
#' um vetor (ex: múltiplas modalidades), a função itera automaticamente sobre ele.
#'
#' @param url_base \code{character}. A URL base do endpoint da API.
#' @param parametros \code{list}. Lista nomeada contendo os parâmetros da requisição.
#'   (ex: \code{list(orgao = "00059311000126", codigoModalidade = 1:13)}). Suporta
#'   no máximo um parâmetro com múltiplos valores (vetor) por vez.
#' @param verbose \code{logical}. Se \code{TRUE}, imprime mensagens de
#'   progresso no console. Padrão: \code{TRUE}.
#' @param max_tries \code{integer}. Número máximo de tentativas de requisição
#'   em caso de erro. Padrão: 5.
#'
#' @return Um \code{data.frame} com todos os registros combinados.
#'
#' @importFrom httr parse_url build_url
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @examples
#' \dontrun{
#' meus_filtros <- list(
#'   orgaoEntidadeCnpj = "00059311000126",
#'   codigoModalidade = 1:13 # Iteração automática habilitada!
#' )
#' dados <- puxa_api_compras_completo(
#'   url_base = "https://dadosabertos.compras.gov.br/modulo-contratacoes/1_consultarContratacoes_PNCP_14133",
#'   parametros = meus_filtros
#' )
#' }
puxa_api_compras_completo <- function(
    url_base,
    parametros = list(),
    verbose = TRUE,
    max_tries = 5
) {

  # 1. VERIFICAÇÃO DE VETORES NA LISTA DE PARÂMETROS
  tamanhos_params <- sapply(parametros, length)
  params_vetor <- names(tamanhos_params[tamanhos_params > 1])

  # Trava de segurança: restringe a apenas 1 parâmetro vetorizado
  if (length(params_vetor) > 1) {
    stop(sprintf(
      "A iteração automática suporta apenas um parâmetro por vez. Foram encontrados múltiplos: %s",
      paste(params_vetor, collapse = ", ")
    ))
  }

  # 2. SE HOUVER UM VETOR, FAZ A ITERAÇÃO RECURSIVA
  if (length(params_vetor) == 1) {
    param_alvo <- params_vetor[1]
    valores_alvo <- parametros[[param_alvo]]

    if (verbose) {
      cat(sprintf("\n=== Iniciando iteração sobre o parâmetro '%s' (%d valores) ===\n",
                  param_alvo, length(valores_alvo)))
    }

    lista_dfs <- lapply(valores_alvo, function(valor_atual) {
      if (verbose) cat(sprintf("\n-> Extraindo para %s = %s\n", param_alvo, as.character(valor_atual)))

      # Cria uma cópia da lista de parâmetros, isolando o valor atual da iteração
      params_iteracao <- parametros
      params_iteracao[[param_alvo]] <- valor_atual

      # Chama a própria função recursivamente (agora sem vetores, caindo no bloco de paginação)
      puxa_api_compras_completo(
        url_base = url_base,
        parametros = params_iteracao,
        verbose = verbose,
        max_tries = max_tries
      )
    })

    if (verbose) cat(sprintf("\n=== Empilhando resultados da iteração sobre '%s' ===\n", param_alvo))
    return(dplyr::bind_rows(lista_dfs))
  }

  # 3. BLOCO DE PAGINAÇÃO PADRÃO (Executado quando a lista só tem valores únicos)

  # Define tamanho da página para 500 se o usuário não tiver especificado
  if (is.null(parametros$tamanhoPagina)) {
    parametros$tamanhoPagina <- 500
  }

  # Garante que a primeira requisição comece sempre na página 1
  parametros$pagina <- 1

  # Faz o parse da URL e injeta a lista de parâmetros
  url_parsed <- httr::parse_url(url_base)
  url_parsed$query <- parametros

  url_inicial <- httr::build_url(url_parsed)

  if (verbose) cat("Buscando página 1...\n")
  primeira_requisicao <- puxa_api_compras(url_inicial, verbose = verbose, max_tries = max_tries)

  # Validação do retorno inicial
  if (is.null(primeira_requisicao) || is.null(primeira_requisicao$resultado) || length(primeira_requisicao$resultado) == 0) {
    warning("Falha na requisição inicial ou nenhum dado retornado para os parâmetros atuais.")
    return(NULL) # Retorna NULL silenciosamente para não quebrar o bind_rows da etapa recursiva
  }

  lista_resultados <- list()
  lista_resultados[[1]] <- primeira_requisicao$resultado

  total_paginas <- primeira_requisicao$totalPaginas

  # Condição de saída antecipada
  if (is.null(total_paginas) || total_paginas <= 1) {
    if (verbose) cat("Extração concluída. Apenas 1 página de dados encontrada.\n")
    return(primeira_requisicao$resultado)
  }

  if (verbose) cat(sprintf("Total de páginas identificadas: %d. Iniciando loop...\n", total_paginas))

  # Loop pelas páginas restantes
  for (pg in 2:total_paginas) {
    url_parsed$query$pagina <- pg
    url_atual <- httr::build_url(url_parsed)

    if (verbose) cat(sprintf("Buscando página %d de %d...\n", pg, total_paginas))
    req_atual <- puxa_api_compras(url_atual, verbose = verbose, max_tries = max_tries)

    if (!is.null(req_atual) && !is.null(req_atual$resultado) && length(req_atual$resultado) > 0) {
      lista_resultados[[pg]] <- req_atual$resultado
    } else {
      warning(sprintf("Falha ou retorno vazio na página %d. Pulando para a próxima...", pg))
    }
  }

  if (verbose) cat("Consolidando dados paginados...\n")
  dados_finais <- dplyr::bind_rows(lista_resultados)

  return(dados_finais)
}
