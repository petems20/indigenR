#' Monta a URL do zip mensal da base nacional do CNES para uma competência
#' @keywords internal
#' @noRd
.cnes_url_base <- function(competencia) {
  .cnes_valida_competencia(competencia)
  paste0(
    "https://cnes.datasus.gov.br/EstatisticasServlet?path=BASE_DE_DADOS_CNES_",
    competencia, ".ZIP"
  )
}

#' Valida que a competência é uma única string AAAAMM
#' @keywords internal
#' @noRd
.cnes_valida_competencia <- function(competencia) {
  if (length(competencia) != 1 || is.na(competencia) || !grepl("^\\d{6}$", competencia)) {
    stop("'competencia' deve ser uma string AAAAMM (ex: \"202607\").", call. = FALSE)
  }
  invisible(competencia)
}

#' Diretório raiz do cache do CNES dentro do cache do pacote
#' @keywords internal
#' @noRd
.cnes_dir <- function(cache_dir) {
  file.path(cache_dir, "cnes")
}

#' Nome do arquivo de metadados que marca uma competência como completa no cache
#' @keywords internal
#' @noRd
.cnes_arquivo_metadados <- "_tabelas.csv"

#' Resolve a competência: a informada ou, se NULL, a mais recente do cache
#' @keywords internal
#' @noRd
.cnes_resolve_competencia <- function(competencia, cache_dir) {
  if (!is.null(competencia)) {
    return(.cnes_valida_competencia(competencia))
  }
  disponiveis <- cnes_competencias_cache(cache_dir)
  if (length(disponiveis) == 0) {
    stop(
      "Nenhuma competência do CNES no cache. Informe 'competencia' (ex: \"202607\") ",
      "para baixar a base.", call. = FALSE
    )
  }
  disponiveis[length(disponiveis)]
}

#' Normaliza o nome de uma tabela do CNES: tira extensão e sufixo de competência
#'
#' \code{"tbEstabelecimento202607.csv"} vira \code{"tbEstabelecimento"}.
#'
#' @keywords internal
#' @noRd
.cnes_nome_tabela <- function(arquivo) {
  base <- sub("\\.(csv|parquet)$", "", basename(arquivo), ignore.case = TRUE)
  sub("\\d{6}$", "", base)
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

#' Converte um CSV do CNES (latin-1, separado por ";") em parquet via DuckDB
#'
#' Todas as colunas são gravadas como texto, preservando zeros à esquerda de códigos.
#' \code{skip = 0} desliga a detecção automática de linhas iniciais a pular: sem ele, o
#' DuckDB pode tomar uma linha de dados como cabeçalho e descartar as anteriores em
#' silêncio quando o arquivo tem linhas com número irregular de colunas — com ele, isso
#' vira erro. Devolve o número de linhas gravadas.
#'
#' @keywords internal
#' @noRd
.cnes_csv_para_parquet <- function(con, csv, parquet) {
  DBI::dbExecute(con, glue::glue_sql("
    COPY (
      SELECT * FROM read_csv(
        {csv},
        delim = ';', header = true, skip = 0, all_varchar = true, encoding = 'latin-1'
      )
    ) TO {parquet} (FORMAT parquet, COMPRESSION zstd)
  ", .con = con))
  DBI::dbGetQuery(
    con,
    glue::glue_sql("SELECT count(*) AS n FROM read_parquet({parquet})", .con = con)
  )$n
}

#' Baixa a base nacional do CNES de uma competência para o cache local em parquet
#'
#' @description
#' Baixa o zip mensal completo da base nacional do CNES (~735MB compactado, ~3GB
#' descompactado em ~109 tabelas) e converte cada tabela CSV em um arquivo parquet no
#' cache local, em \code{<cache_dir>/cnes/<AAAAMM>/<tabela>.parquet}. O nome da tabela
#' perde o sufixo de competência (\code{tbEstabelecimento202607.csv} vira
#' \code{tbEstabelecimento}).
#'
#' O download é feito uma única vez por competência: chamadas seguintes (inclusive
#' indiretas, via \code{\link{cnes_tabela}} ou \code{\link{cnes_importa_saude_indigena}})
#' encontram a base no cache e não acessam a rede. O zip e os CSVs são apagados ao final;
#' só os parquets ficam no disco. As tabelas são descompactadas e convertidas uma a uma,
#' então o espaço livre necessário durante o processo é o zip mais a maior tabela
#' descompactada, além dos parquets já gravados.
#'
#' @details
#' Particularidades da base:
#' \itemize{
#'   \item Os CSVs vêm em latin-1 e separados por \code{";"}. Todas as colunas são
#'     gravadas como texto no parquet (preserva zeros à esquerda de códigos); converta
#'     tipos numéricos na leitura, se necessário.
#'   \item Não há alias confirmado para "competência mais recente" no DATASUS:
#'     \code{competencia} precisa ser informada conforme novos meses são publicados.
#'   \item Uma tabela que falhe na conversão não interrompe as demais: ela é omitida do
#'     cache e listada em um aviso ao final.
#' }
#'
#' A competência só é registrada no cache depois que todas as tabelas foram processadas
#' (o arquivo de metadados \code{_tabelas.csv} marca a conclusão). Um download
#' interrompido não deixa uma competência incompleta visível no cache.
#'
#' @param competencia Competência (mês de referência) da base, string \code{"AAAAMM"}.
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}; a base fica na subpasta \code{cnes}.
#' @param atualizar Se \code{TRUE}, baixa e converte de novo mesmo que a competência já
#'   esteja no cache.
#' @param url URL do zip. Por padrão montada a partir de \code{competencia}.
#' @param max_tries Número máximo de tentativas do download.
#' @param verbose Se \code{TRUE}, exibe progresso e mensagens de retentativa.
#'
#' @return Invisivelmente, o caminho do diretório da competência no cache.
#'
#' @seealso \code{\link{cnes_tabelas}}, \code{\link{cnes_tabela}},
#'   \code{\link{cnes_limpa_cache}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cnes_baixa_base("202607")
#' cnes_tabelas("202607")
#' }
cnes_baixa_base <- function(competencia,
                            cache_dir = tools::R_user_dir("indigenR", "cache"),
                            atualizar = FALSE,
                            url = .cnes_url_base(competencia),
                            max_tries = 5,
                            verbose = TRUE) {
  .cnes_valida_competencia(competencia)

  dir_final <- file.path(.cnes_dir(cache_dir), competencia)
  if (!atualizar && competencia %in% cnes_competencias_cache(cache_dir)) {
    if (verbose) message("CNES ", competencia, " — já está no cache: ", dir_final)
    return(invisible(dir_final))
  }

  # Tudo é montado em um diretório provisório no mesmo disco do cache e só renomeado
  # para o destino final no fim — um erro no meio do caminho não deixa cache parcial.
  dir_provisorio <- file.path(.cnes_dir(cache_dir), paste0(".provisorio_", competencia))
  unlink(dir_provisorio, recursive = TRUE)
  dir.create(dir_provisorio, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir_provisorio, recursive = TRUE), add = TRUE)

  zip_path <- file.path(dir_provisorio, "cnes_base.zip")
  if (verbose) message("CNES ", competencia, " — baixando ", url)
  .cnes_baixa_zip(url, zip_path, max_tries = max_tries, verbose = verbose)

  arquivos_zip <- tryCatch(
    utils::unzip(zip_path, list = TRUE)$Name,
    error = function(e) {
      stop(
        "O arquivo baixado não é um zip válido — a competência ", competencia,
        " pode não estar publicada no DATASUS.", call. = FALSE
      )
    }
  )
  arquivos_csv <- arquivos_zip[grepl("\\.csv$", arquivos_zip, ignore.case = TRUE)]
  if (length(arquivos_csv) == 0) {
    stop("Nenhuma tabela CSV encontrada no zip do CNES.", call. = FALSE)
  }

  tabelas <- .cnes_nome_tabela(arquivos_csv)
  # Se a remoção do sufixo gerar nomes repetidos, mantém o nome original do arquivo.
  repetidas <- tabelas %in% tabelas[duplicated(tabelas)]
  tabelas[repetidas] <- sub("\\.csv$", "", basename(arquivos_csv[repetidas]), ignore.case = TRUE)

  dir_csv <- file.path(dir_provisorio, "csv")
  dir_parquet <- file.path(dir_provisorio, "parquet")
  dir.create(dir_parquet, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  linhas <- rep(NA_real_, length(arquivos_csv))
  falhas <- character()

  for (i in seq_along(arquivos_csv)) {
    if (verbose) {
      message("CNES ", competencia, " — convertendo ", i, "/", length(arquivos_csv), ": ", tabelas[i])
    }
    utils::unzip(zip_path, files = arquivos_csv[i], exdir = dir_csv, junkpaths = TRUE)
    csv <- normalizePath(file.path(dir_csv, basename(arquivos_csv[i])), winslash = "/", mustWork = TRUE)
    parquet <- file.path(normalizePath(dir_parquet, winslash = "/"), paste0(tabelas[i], ".parquet"))

    n <- tryCatch(
      .cnes_csv_para_parquet(con, csv, parquet),
      error = function(e) {
        falhas[[length(falhas) + 1]] <<- paste0(tabelas[i], " (", conditionMessage(e), ")")
        unlink(parquet)
        NA_real_
      }
    )
    linhas[i] <- n
    unlink(csv)
  }

  # O zip não é mais necessário — libera ~735MB antes de mover os parquets.
  unlink(zip_path)

  ok <- !is.na(linhas)
  metadados <- data.frame(
    tabela = tabelas[ok],
    arquivo_origem = basename(arquivos_csv[ok]),
    linhas = linhas[ok],
    stringsAsFactors = FALSE
  )
  utils::write.csv(metadados, file.path(dir_parquet, .cnes_arquivo_metadados), row.names = FALSE)

  unlink(dir_final, recursive = TRUE)
  if (!file.rename(dir_parquet, dir_final)) {
    stop("Não foi possível mover a base convertida para ", dir_final, call. = FALSE)
  }

  if (length(falhas) > 0) {
    warning(
      length(falhas), " tabela(s) do CNES não puderam ser convertidas e ficaram fora do cache:\n",
      paste0("  - ", falhas, collapse = "\n"),
      call. = FALSE
    )
  }
  if (verbose) message("CNES ", competencia, " — ", sum(ok), " tabelas no cache: ", dir_final)

  invisible(dir_final)
}

#' Lista as competências do CNES disponíveis no cache local
#'
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
#'
#' @return Vetor de caracteres com as competências (\code{"AAAAMM"}) completas no cache,
#'   em ordem crescente. Vazio se não houver nenhuma.
#'
#' @export
#'
#' @examples
#' cnes_competencias_cache()
cnes_competencias_cache <- function(cache_dir = tools::R_user_dir("indigenR", "cache")) {
  dirs <- list.dirs(.cnes_dir(cache_dir), full.names = FALSE, recursive = FALSE)
  dirs <- dirs[grepl("^\\d{6}$", dirs)]
  completos <- file.exists(file.path(.cnes_dir(cache_dir), dirs, .cnes_arquivo_metadados))
  sort(dirs[completos])
}

#' Lista as tabelas do CNES disponíveis no cache para uma competência
#'
#' @param competencia Competência \code{"AAAAMM"}. Se \code{NULL}, usa a mais recente do
#'   cache. Não dispara download: se a competência não estiver no cache, use
#'   \code{\link{cnes_baixa_base}} antes.
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
#'
#' @return Uma tibble com uma linha por tabela: \code{tabela} (nome usado em
#'   \code{\link{cnes_tabela}}), \code{arquivo_origem} (nome do CSV no zip),
#'   \code{linhas}, \code{tamanho_mb} (do parquet) e \code{caminho} (do parquet, para
#'   leitura direta com DuckDB ou arrow, se preferir).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cnes_tabelas("202607")
#' }
cnes_tabelas <- function(competencia = NULL,
                         cache_dir = tools::R_user_dir("indigenR", "cache")) {
  competencia <- .cnes_resolve_competencia(competencia, cache_dir)
  if (!competencia %in% cnes_competencias_cache(cache_dir)) {
    stop(
      "Competência ", competencia, " do CNES não está no cache. ",
      "Baixe com cnes_baixa_base(\"", competencia, "\").", call. = FALSE
    )
  }

  dir_comp <- file.path(.cnes_dir(cache_dir), competencia)
  metadados <- utils::read.csv(
    file.path(dir_comp, .cnes_arquivo_metadados),
    colClasses = c("character", "character", "numeric")
  )
  caminho <- normalizePath(file.path(dir_comp, paste0(metadados$tabela, ".parquet")), winslash = "/")

  tibble::tibble(
    tabela = metadados$tabela,
    arquivo_origem = metadados$arquivo_origem,
    linhas = metadados$linhas,
    tamanho_mb = round(file.size(caminho) / 1024^2, 2),
    caminho = caminho
  )
}

#' Caminho do parquet de uma tabela do CNES no cache, baixando a base se necessário
#'
#' Aceita o nome com ou sem sufixo de competência/extensão e sem diferenciar
#' maiúsculas de minúsculas.
#'
#' @keywords internal
#' @noRd
.cnes_caminho_tabela <- function(tabela, competencia, cache_dir, verbose) {
  if (length(tabela) != 1 || is.na(tabela)) {
    stop("'tabela' deve ser um único nome de tabela.", call. = FALSE)
  }
  if (!is.null(competencia) && !competencia %in% cnes_competencias_cache(cache_dir)) {
    cnes_baixa_base(competencia, cache_dir = cache_dir, verbose = verbose)
  }
  tabs <- cnes_tabelas(competencia, cache_dir = cache_dir)

  idx <- which(tolower(tabs$tabela) == tolower(.cnes_nome_tabela(tabela)))
  if (length(idx) != 1) {
    parecidas <- tabs$tabela[agrepl(.cnes_nome_tabela(tabela), tabs$tabela, ignore.case = TRUE)]
    stop(
      "Tabela '", tabela, "' não encontrada no cache do CNES.",
      if (length(parecidas) > 0) paste0(" Você quis dizer: ", paste(parecidas, collapse = ", "), "?"),
      " Veja cnes_tabelas() para a lista completa.",
      call. = FALSE
    )
  }
  tabs$caminho[[idx]]
}

#' Lê uma tabela do CNES do cache local
#'
#' @description
#' Devolve para a memória só a tabela pedida (e, opcionalmente, só algumas colunas e
#' linhas) da base do CNES em cache. Se \code{competencia} for informada e ainda não
#' estiver no cache, baixa a base inteira antes com \code{\link{cnes_baixa_base}} — as
#' próximas leituras de qualquer tabela dessa competência são locais.
#'
#' Seleção de colunas e filtro são aplicados pelo DuckDB sobre o parquet, antes de os
#' dados chegarem ao R: útil para tabelas grandes como \code{tbEstabelecimento}.
#'
#' @param tabela Nome da tabela, como listado em \code{\link{cnes_tabelas}} (ex:
#'   \code{"tbEstabelecimento"}). Também aceita o nome original do CSV
#'   (\code{"tbEstabelecimento202607.csv"}); maiúsculas e minúsculas são indiferentes.
#' @param competencia Competência \code{"AAAAMM"}. Se \code{NULL}, usa a mais recente do
#'   cache.
#' @param colunas Vetor com os nomes das colunas a trazer. \code{NULL} traz todas.
#' @param filtro Lista nomeada de filtros de igualdade, combinados com E: cada elemento
#'   mantém as linhas cuja coluna está entre os valores informados (ex:
#'   \code{list(TP_UNIDADE = "72", CO_ESTADO_GESTOR = c("13", "14"))}). Valores são
#'   comparados como texto.
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
#' @param verbose Se \code{TRUE}, exibe mensagens de progresso (inclusive do download,
#'   se houver).
#'
#' @return Uma tibble com todas as colunas como texto.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Baixa a base de 2026-07 (uma vez) e lê só os estabelecimentos de saúde indígena
#' estab <- cnes_tabela(
#'   "tbEstabelecimento", "202607",
#'   colunas = c("CO_CNES", "NO_FANTASIA", "CO_MUNICIPIO_GESTOR"),
#'   filtro = list(TP_UNIDADE = "72")
#' )
#'
#' # Tabela de domínio, da competência mais recente do cache
#' cnes_tabela("tbTipoUnidade")
#' }
cnes_tabela <- function(tabela,
                        competencia = NULL,
                        colunas = NULL,
                        filtro = NULL,
                        cache_dir = tools::R_user_dir("indigenR", "cache"),
                        verbose = TRUE) {
  if (!is.null(filtro) && (!is.list(filtro) || is.null(names(filtro)) || any(names(filtro) == ""))) {
    stop("'filtro' deve ser uma lista nomeada (ex: list(TP_UNIDADE = \"72\")).", call. = FALSE)
  }
  caminho <- .cnes_caminho_tabela(tabela, competencia, cache_dir, verbose)

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  existentes <- names(DBI::dbGetQuery(
    con, glue::glue_sql("SELECT * FROM read_parquet({caminho}) LIMIT 0", .con = con)
  ))
  desconhecidas <- setdiff(c(colunas, names(filtro)), existentes)
  if (length(desconhecidas) > 0) {
    stop(
      "Coluna(s) inexistente(s) em ", tabela, ": ", paste(desconhecidas, collapse = ", "),
      ".\nColunas disponíveis: ", paste(existentes, collapse = ", "), call. = FALSE
    )
  }

  selecao <- if (is.null(colunas)) DBI::SQL("*") else glue::glue_sql("{`colunas`*}", .con = con)
  condicoes <- if (length(filtro) == 0) {
    DBI::SQL("TRUE")
  } else {
    partes <- lapply(names(filtro), function(col) {
      valores <- as.character(filtro[[col]])
      glue::glue_sql("{`col`} IN ({valores*})", .con = con)
    })
    glue::glue_sql_collapse(partes, sep = " AND ")
  }

  dados <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT {selecao} FROM read_parquet({caminho}) WHERE {condicoes}",
    .con = con
  ))
  tibble::as_tibble(dados)
}

#' Remove competências do CNES do cache local
#'
#' @param competencia Competência(s) \code{"AAAAMM"} a remover. Se \code{NULL}, remove
#'   todo o cache do CNES (inclusive restos de downloads interrompidos).
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
#' @param verbose Se \code{TRUE}, informa o que foi removido.
#'
#' @return Invisivelmente, as competências removidas.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cnes_limpa_cache("202606")
#' }
cnes_limpa_cache <- function(competencia = NULL,
                             cache_dir = tools::R_user_dir("indigenR", "cache"),
                             verbose = TRUE) {
  presentes <- cnes_competencias_cache(cache_dir)

  if (is.null(competencia)) {
    unlink(.cnes_dir(cache_dir), recursive = TRUE)
    removidas <- presentes
  } else {
    for (comp in competencia) .cnes_valida_competencia(comp)
    removidas <- intersect(competencia, presentes)
    unlink(file.path(.cnes_dir(cache_dir), competencia), recursive = TRUE)
  }

  if (verbose) {
    message(
      if (length(removidas) == 0) "CNES — nenhuma competência removida do cache."
      else paste0("CNES — removida(s) do cache: ", paste(removidas, collapse = ", "))
    )
  }
  invisible(removidas)
}
