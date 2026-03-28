#' @keywords internal
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract
puxa_api_compras <- function(url, verbose = TRUE, max_tries = 5) {
  tentativa <- 1
  repeat {
    if (verbose) cat("Extraindo dados da URL: ", url,'\n')

    res <- httr::GET(url)
    sc <- httr::status_code(res)

    # --- Rate limit ---
    if (sc == 429) {
      body <- httr::content(res, "text", encoding = "UTF-8")
      msg <- jsonlite::fromJSON(body)$message
      segundos <- as.integer(stringr::str_extract(msg, "(?<=Try again in )\\d+(?= seconds)"))
      wait_time <- ifelse(is.na(segundos), 5, segundos)
      if (verbose) cat("Rate limit. Esperando ", wait_time, " segundos...\n")
      Sys.sleep(wait_time + 1)
      next
    }

    if (sc >= 400) {
      cat(sprintf("Falha na requisição. Status HTTP: %d", sc), '\n')
      return(res)  # Retorna o objeto httr com erro
    }

    res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)

    if (!is.null(res$resultado) && length(res$resultado) > 0) return(res)
    else return(res)

    # Retry com backoff exponencial
    tentativa <- tentativa + 1
    if (tentativa > max_tries) {
      warning("Máximo de tentativas atingido para URL: ", url)
      return(NULL)
    }
    Sys.sleep(2 ^ (tentativa - 1))
  }
}

