#' @keywords internal
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
puxa_api_contratos <- function(url, verbose = TRUE, max_tries = 5) {
  tentativa <- 1
  repeat {
    if (verbose) message("Extraindo dados da URL: ", url)

    resp <- httr::GET(url)
    sc <- httr::status_code(resp)

    # --- Retry em caso de erro HTTP temporário ---
    if (sc >= 400) {
      if (tentativa >= max_tries) {
        stop("Erro HTTP: ", sc, " após ", tentativa, " tentativas.", call. = FALSE)
      } else {
        wait_time <- 2 ^ (tentativa - 1)
        if (verbose) message("Erro HTTP ", sc, ". Tentando novamente em ", wait_time, " segundos...")
        Sys.sleep(wait_time)
        tentativa <- tentativa + 1
        next
      }
    }

    # --- Sucesso ---
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    res <- jsonlite::fromJSON(txt, flatten = TRUE)
    return(res)
  }
}
