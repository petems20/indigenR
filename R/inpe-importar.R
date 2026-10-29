# Dados de monitoramento ambiental do INPE: focos de calor (BDQueimadas), alertas DETER e
# desmatamento anual PRODES (TerraBrasilis). Fontes públicas, sem autenticação.

.inpe_url_focos <- "https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv"
.inpe_url_terrabrasilis <- "https://terrabrasilis.dpi.inpe.br/geoserver"

#' Baixa um arquivo para disco com retentativa e backoff exponencial
#'
#' Repete em erro de conexão, limite de requisições (HTTP 429) e erro de servidor (5xx);
#' para em erro HTTP 4xx não recuperável.
#'
#' @keywords internal
#' @noRd
.inpe_baixa_arquivo <- function(url, destino, contexto, max_tries = 5, verbose = TRUE) {
  tentativa <- 1
  repeat {
    resp <- tryCatch(
      httr::GET(url, httr::write_disk(destino, overwrite = TRUE), httr::timeout(1200),
                httr::user_agent("indigenR (R; httr)")),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      if (tentativa >= max_tries) {
        stop("Falha de conexão ao ", contexto, " após ", tentativa, " tentativas.\n", resp$message, call. = FALSE)
      }
      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) message("Erro de conexão. Nova tentativa em ", wait_time, " segundos...")
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }
    sc <- httr::status_code(resp)
    if (sc == 429 || sc >= 500) {
      if (tentativa >= max_tries) {
        stop("Erro HTTP ", sc, " persistente ao ", contexto, " após ", tentativa, " tentativas.", call. = FALSE)
      }
      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) message("Erro HTTP ", sc, ". Nova tentativa em ", wait_time, " segundos...")
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }
    if (sc >= 400) stop("Erro HTTP ", sc, " ao ", contexto, " (", url, ").", call. = FALSE)
    return(invisible(destino))
  }
}

#' Focos de calor do INPE (BDQueimadas) de um ano ou de um mês
#'
#' @description Baixa os focos de calor detectados por todos os satélites monitorados pelo
#' INPE no Brasil: o arquivo anual (\code{periodo = "AAAA"}, anos fechados) ou o mensal
#' (\code{periodo = "AAAAMM"}, inclusive o mês corrente, atualizado diariamente pelo INPE).
#' Os dois formatos têm colunas diferentes na origem; o resultado vem padronizado.
#'
#' Foco de calor não é incêndio confirmado, e o mesmo fogo aparece em vários satélites. Para
#' séries comparáveis entre anos, o INPE usa o satélite de referência (\code{"AQUA_M-T"});
#' filtre por \code{satelite} conforme o uso.
#'
#' @param periodo \code{character}. Ano (\code{"2025"}) ou ano e mês (\code{"202610"}).
#' @param max_tries Número de retentativas por requisição HTTP.
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#'
#' @return \code{tibble} com uma linha por foco: \code{data_hora} (UTC), \code{satelite},
#'   \code{latitude}, \code{longitude}, \code{pais}, \code{estado}, \code{municipio},
#'   \code{codigo_municipio} (só nos arquivos mensais; \code{NA} nos anuais), \code{bioma},
#'   \code{numero_dias_sem_chuva}, \code{precipitacao}, \code{risco_fogo} e \code{frp}
#'   (potência radiativa do fogo). O período fica em \code{attr(resultado, "periodo")}.
#'
#' @export
#' @examples
#' \dontrun{
#' focos_2025 <- inpe_focos_calor("2025")
#' focos_out_2026 <- inpe_focos_calor("202610")
#' }
#' @importFrom data.table fread
#' @importFrom tibble as_tibble tibble
inpe_focos_calor <- function(periodo, max_tries = 5, verbose = TRUE) {
  periodo <- as.character(periodo)
  if (!grepl("^[0-9]{4}([0-9]{2})?$", periodo)) stop("periodo deve ser 'AAAA' ou 'AAAAMM'.", call. = FALSE)
  anual <- nchar(periodo) == 4
  pasta <- tempfile("focos_")
  dir.create(pasta)
  on.exit(unlink(pasta, recursive = TRUE), add = TRUE)

  if (anual) {
    url <- sprintf("%s/anual/Brasil_todos_sats/focos_br_todos-sats_%s.zip", .inpe_url_focos, periodo)
    zip <- file.path(pasta, "focos.zip")
    if (verbose) message("Baixando focos de calor de ", periodo, " (arquivo anual)...")
    .inpe_baixa_arquivo(url, zip, paste("baixar os focos de calor de", periodo), max_tries, verbose)
    csv <- grep("\\.csv$", utils::unzip(zip, exdir = pasta), value = TRUE)[1]
  } else {
    url <- sprintf("%s/mensal/Brasil/focos_mensal_br_%s.csv", .inpe_url_focos, periodo)
    csv <- file.path(pasta, "focos.csv")
    if (verbose) message("Baixando focos de calor de ", periodo, " (arquivo mensal)...")
    .inpe_baixa_arquivo(url, csv, paste("baixar os focos de calor de", periodo), max_tries, verbose)
  }

  d <- data.table::fread(csv, colClasses = "character", encoding = "UTF-8", showProgress = FALSE)
  num <- function(x) suppressWarnings(as.numeric(x))
  col <- function(...) {
    for (nome in c(...)) if (nome %in% names(d)) return(d[[nome]])
    rep(NA_character_, nrow(d))
  }
  resultado <- tibble::tibble(
    data_hora = as.POSIXct(col("data_pas", "data_hora_gmt"), tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
    satelite = col("satelite"),
    latitude = num(col("latitude", "lat")),
    longitude = num(col("longitude", "lon")),
    pais = col("pais"),
    estado = col("estado"),
    municipio = col("municipio"),
    codigo_municipio = col("municipio_id"),
    bioma = col("bioma"),
    numero_dias_sem_chuva = num(col("numero_dias_sem_chuva")),
    precipitacao = num(col("precipitacao")),
    risco_fogo = num(col("risco_fogo")),
    frp = num(col("frp"))
  )
  attr(resultado, "periodo") <- periodo
  resultado
}

#' Camada WFS do TerraBrasilis por produto e bioma
#'
#' @keywords internal
#' @noRd
.inpe_camada <- function(produto, bioma) {
  camadas <- list(
    deter = c(amazonia = "deter-amz:deter_amz", cerrado = "deter-cerrado-nb:deter_cerrado"),
    prodes = c(amazonia = "prodes-legal-amz:yearly_deforestation", cerrado = "prodes-cerrado-nb:yearly_deforestation",
               caatinga = "prodes-caatinga-nb:yearly_deforestation", mata_atlantica = "prodes-mata-atlantica-nb:yearly_deforestation",
               pampa = "prodes-pampa-nb:yearly_deforestation", pantanal = "prodes-pantanal-nb:yearly_deforestation")
  )[[produto]]
  if (!bioma %in% names(camadas)) {
    stop("Bioma '", bioma, "' sem ", toupper(produto), ". Disponíveis: ", paste(names(camadas), collapse = ", "), call. = FALSE)
  }
  camada <- camadas[[bioma]]
  list(url = sprintf("%s/%s/ows", .inpe_url_terrabrasilis, sub(":.*$", "", camada)), camada = camada)
}

#' Alertas de desmatamento e degradação do DETER (INPE) num período
#'
#' @description Baixa os polígonos de alerta do DETER (detecção quase diária de alteração
#' na cobertura florestal) do TerraBrasilis, por bioma, entre duas datas de observação
#' (\code{view_date}). Classes de alerta: desmatamento (corte raso, com vegetação),
#' degradação, cicatriz de queimada, corte seletivo, mineração (na Amazônia) etc. —
#' alerta não é área oficial de desmatamento (a oficial é o PRODES, ver
#' \code{\link{inpe_prodes}}).
#'
#' @param bioma \code{"amazonia"} ou \code{"cerrado"} (os biomas com DETER).
#' @param data_inicio,data_fim Datas (\code{Date} ou \code{"AAAA-MM-DD"}); o filtro é
#'   \code{data_inicio <= view_date < data_fim}. \code{data_fim = NULL} = até hoje.
#' @param crs Sistema de referência do resultado (padrão 4326, WGS 84; a camada vem em SIRGAS 2000).
#' @param count Feições por requisição.
#' @param max_tries,verbose Ver \code{\link{puxa_wfs_completo}}.
#'
#' @return Objeto \code{sf} com os campos da camada (variam entre biomas; a classe vem em
#'   \code{classname}) mais \code{bioma}; \code{NULL} se não houver alertas no período.
#'
#' @export
#' @examples
#' \dontrun{
#' deter_2025 <- inpe_deter("amazonia", "2025-01-01", "2026-01-01")
#' }
inpe_deter <- function(bioma = c("amazonia", "cerrado"), data_inicio, data_fim = NULL, crs = 4326,
                       count = 10000, max_tries = 5, verbose = TRUE) {
  bioma <- match.arg(bioma)
  origem <- .inpe_camada("deter", bioma)
  filtro <- sprintf("view_date >= '%s'", as.Date(data_inicio))
  if (!is.null(data_fim)) filtro <- sprintf("%s AND view_date < '%s'", filtro, as.Date(data_fim))
  res <- puxa_wfs_completo(origem$url, origem$camada, cql_filter = filtro, crs = crs, count = count,
                           max_tries = max_tries, verbose = verbose)
  if (is.null(res)) return(NULL)
  res$bioma <- bioma
  res
}

#' Desmatamento anual do PRODES (INPE) num ano
#'
#' @description Baixa os polígonos de desmatamento anual do PRODES (dado oficial de
#' desmatamento do Brasil) do TerraBrasilis, por bioma e ano. O "ano PRODES" é o período de
#' referência do bioma (na Amazônia, de agosto do ano anterior a julho do ano informado).
#'
#' @param bioma \code{"amazonia"} (Amazônia Legal), \code{"cerrado"}, \code{"caatinga"},
#'   \code{"mata_atlantica"}, \code{"pampa"} ou \code{"pantanal"}.
#' @param ano Ano PRODES (\code{integer}).
#' @param crs,count,max_tries,verbose Ver \code{\link{inpe_deter}}.
#'
#' @return Objeto \code{sf} com os campos da camada (a área vem em \code{area_km}, km²) mais
#'   \code{bioma}; \code{NULL} se o ano não tiver dados no bioma.
#'
#' @export
#' @examples
#' \dontrun{
#' prodes_2024 <- inpe_prodes("amazonia", 2024)
#' }
inpe_prodes <- function(bioma = c("amazonia", "cerrado", "caatinga", "mata_atlantica", "pampa", "pantanal"),
                        ano, crs = 4326, count = 10000, max_tries = 5, verbose = TRUE) {
  bioma <- match.arg(bioma)
  origem <- .inpe_camada("prodes", bioma)
  res <- puxa_wfs_completo(origem$url, origem$camada, cql_filter = sprintf("year = %d", as.integer(ano)), crs = crs,
                           count = count, max_tries = max_tries, verbose = verbose)
  if (is.null(res)) return(NULL)
  res$bioma <- bioma
  res
}
