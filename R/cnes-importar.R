#' Monta a URL do zip mensal da base nacional do CNES para uma competência
#' @keywords internal
#' @noRd
.cnes_url_base <- function(competencia) {
  if (length(competencia) != 1 || !grepl("^\\d{6}$", competencia)) {
    stop("'competencia' deve ser uma string AAAAMM (ex: \"202607\").", call. = FALSE)
  }
  paste0(
    "https://cnes.datasus.gov.br/EstatisticasServlet?path=BASE_DE_DADOS_CNES_",
    competencia, ".ZIP"
  )
}

#' Baixa o zip do CNES para disco com retentativa e backoff exponencial
#'
#' Retenta em caso de erro de conexão, limite de requisições (HTTP 429) ou erro de
#' servidor (5xx); para em erro HTTP 4xx não recuperável. O timeout é longo porque o zip
#' tem ~735MB.
#'
#' @keywords internal
#' @noRd
.cnes_baixa_zip <- function(url, destino, max_tries, verbose) {
  tentativa <- 1
  repeat {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::write_disk(destino, overwrite = TRUE),
        httr::timeout(1200),
        if (verbose) httr::progress()
      ),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      if (tentativa >= max_tries) {
        stop(
          "Falha de conexão ao baixar o zip do CNES após ", tentativa, " tentativas.\n",
          resp$message, call. = FALSE
        )
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
        stop(
          "Erro HTTP ", sc, " persistente ao baixar o zip do CNES após ", tentativa,
          " tentativas.", call. = FALSE
        )
      }
      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) {
        message(
          if (sc == 429) "Rate limit (429). " else paste0("Erro HTTP ", sc, ". "),
          "Nova tentativa em ", wait_time, " segundos..."
        )
      }
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }

    if (sc >= 400) {
      stop(
        "Erro HTTP ", sc, " ao baixar o zip do CNES. Verifique se a competência existe.",
        call. = FALSE
      )
    }

    return(invisible(destino))
  }
}

#' Importa os estabelecimentos de saúde indígena do CNES
#'
#' @description
#' Baixa o zip mensal completo da base nacional do CNES (~735MB compactado, ~3GB
#' descompactado em ~109 tabelas), extrai apenas \code{tbEstabelecimento},
#' \code{rlEstabPoloAldeia} e \code{tbPoloBase}, e filtra os estabelecimentos por
#' \code{TP_UNIDADE}. O padrão, \code{"72"}, é "UNIDADE DE ATENCAO A SAUDE INDIGENA"
#' (código de \code{tbTipoUnidade<AAAAMM>.csv}, o mesmo usado pelo CnesWeb público) e
#' cobre toda a rede SASISUS (DSEI, Polo Base, UBSI, CASAI) — não há subtipo próprio de
#' saúde indígena em \code{tbSubTipo}. Testado contra a base de 2026-07: 1.572
#' estabelecimentos.
#'
#' A filtragem é feita via DuckDB diretamente sobre os CSVs (sem carregar o
#' \code{tbEstabelecimento} de ~300MB inteiro em R). Arquivo zip e tabelas extraídas
#' ficam em diretório temporário e são apagados ao final.
#'
#' @details
#' Particularidades da base:
#' \itemize{
#'   \item \code{tbEstabelecimento} tem duas colunas parecidas: \code{TP_UNIDADE} (a
#'     correta, populada) e \code{CO_TIPO_UNIDADE} (NULL em todas as linhas testadas).
#'   \item Os CSVs vêm em latin-1, não UTF-8.
#'   \item \code{CO_UNIDADE} de \code{rlEstabPoloAldeia} não é o CNES puro: é
#'     \code{CO_MUNICIPIO_GESTOR} (6 dígitos) + \code{CO_CNES} (7 dígitos) concatenados
#'     (não documentado no dicionário oficial). A função devolve só os 7 dígitos finais
#'     como \code{co_cnes}.
#'   \item Não há alias confirmado para "competência mais recente": \code{competencia}
#'     precisa ser atualizada manualmente conforme o DATASUS publica novos meses.
#' }
#'
#' @param competencia Competência (mês de referência) da base, string \code{"AAAAMM"}.
#' @param url URL do zip. Por padrão montada a partir de \code{competencia}.
#' @param tp_unidade Código(s) de \code{TP_UNIDADE} a manter. Padrão \code{"72"}
#'   (unidade de atenção à saúde indígena).
#' @param max_tries Número máximo de tentativas do download.
#' @param verbose Se \code{TRUE}, exibe progresso e mensagens de retentativa.
#'
#' @return Uma lista nomeada com três tibbles (todas as colunas como texto, exceto
#'   \code{latitude}/\code{longitude}):
#'   \describe{
#'     \item{estabelecimentos}{Estabelecimentos com o \code{TP_UNIDADE} informado e com
#'       latitude/longitude válidas.}
#'     \item{rl_polo_aldeia}{Relação estabelecimento-polo base (\code{co_cnes},
#'       \code{co_polo_base}), só linhas com polo base preenchido.}
#'     \item{polo_base}{Cadastro de polos base do SASISUS (\code{co_polo_base},
#'       \code{nm_polo_base}).}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cnes <- cnes_importa_saude_indigena("202607")
#' cnes$estabelecimentos
#' }
cnes_importa_saude_indigena <- function(competencia = "202607",
                                         url = .cnes_url_base(competencia),
                                         tp_unidade = "72",
                                         max_tries = 5,
                                         verbose = TRUE) {

  dir_tmp <- tempfile("cnes_")
  dir.create(dir_tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)

  zip_path <- file.path(dir_tmp, "cnes_base.zip")
  if (verbose) message("CNES — baixando ", url)
  .cnes_baixa_zip(url, zip_path, max_tries = max_tries, verbose = verbose)

  arquivos_zip <- utils::unzip(zip_path, list = TRUE)$Name
  localiza <- function(padrao) {
    arquivos_zip[grepl(padrao, basename(arquivos_zip), ignore.case = TRUE)]
  }
  arq_estab <- localiza("^tbEstabelecimento.*\\.csv$")
  arq_rl <- localiza("^rlEstabPoloAldeia.*\\.csv$")
  arq_polo <- localiza("^tbPoloBase.*\\.csv$")
  if (length(arq_estab) != 1 || length(arq_rl) != 1 || length(arq_polo) != 1) {
    stop(
      "Tabela de estabelecimentos, rlEstabPoloAldeia ou tbPoloBase não encontrada ",
      "(ou ambígua) no zip do CNES.", call. = FALSE
    )
  }

  unzip_dir <- file.path(dir_tmp, "extraido")
  utils::unzip(
    zip_path,
    files = c(arq_estab, arq_rl, arq_polo),
    exdir = unzip_dir,
    junkpaths = TRUE
  )
  # O zip não é mais necessário — libera ~735MB antes de consultar os CSVs.
  unlink(zip_path)

  caminho <- function(arq) {
    normalizePath(file.path(unzip_dir, basename(arq)), winslash = "/", mustWork = TRUE)
  }
  caminho_estab <- caminho(arq_estab)
  caminho_rl <- caminho(arq_rl)
  caminho_polo <- caminho(arq_polo)

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (verbose) message("CNES — filtrando estabelecimentos (TP_UNIDADE: ", paste(tp_unidade, collapse = ", "), ")")

  estab <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT
      e.CO_CNES, e.NO_FANTASIA, e.NO_RAZAO_SOCIAL,
      e.CO_ESTADO_GESTOR, e.CO_MUNICIPIO_GESTOR,
      e.NO_LOGRADOURO, e.NU_ENDERECO, e.NO_BAIRRO, e.CO_CEP,
      e.NU_TELEFONE, e.NO_EMAIL, e.TP_GESTAO, e.CO_MOTIVO_DESAB,
      e.TP_UNIDADE, e.CO_TIPO_ESTABELECIMENTO,
      TRY_CAST(e.NU_LATITUDE AS DOUBLE) AS latitude,
      TRY_CAST(e.NU_LONGITUDE AS DOUBLE) AS longitude
    FROM read_csv({caminho_estab}, delim = ';', header = true, all_varchar = true, encoding = 'latin-1') e
    WHERE e.TP_UNIDADE IN ({tp_unidade*})
  ", .con = con))
  estab <- estab[!is.na(estab$latitude) & !is.na(estab$longitude), ]

  rl_polo_aldeia <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT RIGHT(CO_UNIDADE, 7) AS co_cnes, CO_POLOBASE AS co_polo_base
    FROM read_csv({caminho_rl}, delim = ';', header = true, all_varchar = true, encoding = 'latin-1')
    WHERE CO_POLOBASE IS NOT NULL
  ", .con = con))

  polo_base <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT CO_SEQ_POLO_BASE AS co_polo_base, DS_POLO_BASE AS nm_polo_base
    FROM read_csv({caminho_polo}, delim = ';', header = true, all_varchar = true, encoding = 'latin-1')
  ", .con = con))

  if (verbose) message("CNES — estabelecimentos com coordenadas: ", nrow(estab))

  list(
    estabelecimentos = tibble::as_tibble(estab),
    rl_polo_aldeia = tibble::as_tibble(rl_polo_aldeia),
    polo_base = tibble::as_tibble(polo_base)
  )
}
