#' Requisição de baixo nível para a API do PNCP, com retentativa
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
puxa_api_pncp <- function(
    url,
    verbose = TRUE,
    max_tries = 5
) {

  tentativa <- 1

  repeat {

    if (verbose) {
      message("Extraindo dados da URL: ", url)
    }

    resp <- tryCatch(
      httr::GET(url),
      error = function(e) e
    )

    # ERRO DE CONEXÃO

    if (inherits(resp, "error")) {

      if (tentativa >= max_tries) {

        stop(
          "Falha de conexão após ",
          tentativa,
          " tentativas.\n",
          resp$message,
          call. = FALSE
        )
      }

      wait_time <- 2 ^ (tentativa - 1)

      if (verbose) {
        message(
          "Erro de conexão. Nova tentativa em ",
          wait_time,
          " segundos..."
        )
      }

      Sys.sleep(wait_time)

      tentativa <- tentativa + 1

      next
    }

    sc <- httr::status_code(resp)

    # 204 = SEM CONTEÚDO

    if (sc == 204) {

      if (verbose) {
        message("Resposta 204 (sem conteúdo).")
      }

      return(list(
        ok = TRUE,
        status_code = 204L,
        content = NULL
      ))
    }

    # ERROS HTTP

    if (sc >= 400) {

      if (tentativa >= max_tries) {

        stop(
          "Erro HTTP ",
          sc,
          " após ",
          tentativa,
          " tentativas.",
          call. = FALSE
        )
      }

      wait_time <- 2 ^ (tentativa - 1)

      if (verbose) {
        message(
          "Erro HTTP ",
          sc,
          ". Tentando novamente em ",
          wait_time,
          " segundos..."
        )
      }

      Sys.sleep(wait_time)

      tentativa <- tentativa + 1

      next
    }

    # SUCESSO COM CONTEÚDO

    txt <- httr::content(
      resp,
      as = "text",
      encoding = "UTF-8"
    )

    # conteúdo vazio mesmo sem 204
    if (
      is.null(txt) ||
      identical(txt, "") ||
      nchar(txt) == 0
    ) {

      return(list(
        ok = TRUE,
        status_code = sc,
        content = NULL
      ))
    }

    # parse robusto
    parsed <- tryCatch(

      jsonlite::fromJSON(
        txt,
        flatten = TRUE
      ),

      error = function(e) {

        warning(
          "Falha ao interpretar JSON:\n",
          e$message,
          call. = FALSE
        )

        NULL
      }
    )

    return(list(
      ok = TRUE,
      status_code = sc,
      content = parsed
    ))
  }
}

#' Extrai todos os dados paginados da API PNCP
#'
#' @param url_base URL com placeholders compatíveis com glue
#' @param parametros lista nomeada de parâmetros
#' @param verbose lógico
#' @param max_tries máximo de tentativas
#'
#' @return data.frame
#'
#' @importFrom glue glue_data
#' @importFrom httr parse_url build_url
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_match_all
#'
#' @export
puxa_api_pncp_completo <- function(
    url_base,
    parametros = list(),
    verbose = TRUE,
    max_tries = 5
) {

  # Identifica parâmetros vetorizados ----

  tamanhos_params <- sapply(parametros, length)

  params_vetor <- names(tamanhos_params[tamanhos_params > 1])

  if (length(params_vetor) > 1) {

    stop(
      paste0(
        "A função suporta apenas um parâmetro vetorizado. ",
        "Encontrados: ",
        paste(params_vetor, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Iteração recursiva ----

  if (length(params_vetor) == 1) {

    param_alvo <- params_vetor[1]

    valores_alvo <- parametros[[param_alvo]]

    if (verbose) {

      message(
        "Iterando sobre ",
        param_alvo,
        " (",
        length(valores_alvo),
        " valores)"
      )
    }

    lista_dfs <- lapply(valores_alvo, function(valor_atual) {

      if (verbose) {
        message(
          "-> ",
          param_alvo,
          " = ",
          valor_atual
        )
      }

      params_iteracao <- parametros

      params_iteracao[[param_alvo]] <- valor_atual

      puxa_api_pncp_completo(
        url_base = url_base,
        parametros = params_iteracao,
        verbose = verbose,
        max_tries = max_tries
      )
    })

    return(dplyr::bind_rows(lista_dfs))
  }

  # Interpolação da URL ----

  url_interpolada <- glue::glue_data(
    .x = parametros,
    url_base
  )

  url_interpolada <- as.character(url_interpolada)

  # Remove parâmetros usados no path ----

  placeholders <- stringr::str_match_all(
    url_base,
    "\\{([^}]+)\\}"
  )[[1]][, 2]

  query_params <- parametros[
    !names(parametros) %in% placeholders
  ]

  # Configuração de paginação ----

  if (is.null(query_params$tamanhoPagina)) {
    query_params$tamanhoPagina <- 500
  }

  url_parsed <- httr::parse_url(url_interpolada)

  lista_resultados <- list()

  pagina_atual <- 1

  repeat {

    # Monta URL da página ----

    query_params$pagina <- pagina_atual

    url_parsed$query <- query_params

    url_atual <- httr::build_url(url_parsed)

    if (verbose) {
      message("Buscando página ", pagina_atual)
    }

    # Requisição ----

    req <- tryCatch(

      puxa_api_pncp(
        url = url_atual,
        verbose = FALSE,
        max_tries = max_tries
      ),

      error = function(e) NULL
    )

    # Falha ou fim da paginação ----

    if (is.null(req) || is.null(req$content)) {

      if (verbose) {
        message("Fim da paginação.")
      }

      break
    }

    # Dados da página ----

    dados <- req$content

    # Página vazia ----

    vazio <- FALSE

    if (is.null(dados)) {

      vazio <- TRUE

    } else if (is.data.frame(dados) && nrow(dados) == 0) {

      vazio <- TRUE

    } else if (is.list(dados) && length(dados) == 0) {

      vazio <- TRUE
    }

    if (vazio) {

      if (verbose) {
        message("Página vazia.")
      }

      break
    }

    # Acumula resultados ----

    lista_resultados[[length(lista_resultados) + 1]] <- dados

    pagina_atual <- pagina_atual + 1
  }

  # Consolidação final ----

  if (length(lista_resultados) == 0) {

    warning(
      "Nenhum dado encontrado.",
      call. = FALSE
    )

    return(NULL)
  }

  if (verbose) {
    message("Consolidando resultados...")
  }

  dplyr::bind_rows(lista_resultados)
}
