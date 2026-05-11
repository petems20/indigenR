#' Requisição robusta para API PNCP
#'
#' @keywords internal
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
