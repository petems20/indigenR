#' Importa os estabelecimentos de saúde indígena do CNES
#'
#' @description
#' Lê do cache local do CNES (ver \code{\link{cnes_baixa_base}}) as tabelas
#' \code{tbEstabelecimento}, \code{rlEstabPoloAldeia} e \code{tbPoloBase}, e filtra os
#' estabelecimentos por \code{TP_UNIDADE}. Se a competência ainda não estiver no cache, a
#' base inteira é baixada antes (uma vez só); as chamadas seguintes são locais.
#'
#' O padrão de \code{tp_unidade}, \code{"72"}, é "UNIDADE DE ATENCAO A SAUDE INDIGENA"
#' (código de \code{tbTipoUnidade}, o mesmo usado pelo CnesWeb público) e cobre toda a
#' rede SASISUS (DSEI, Polo Base, UBSI, CASAI) — não há subtipo próprio de saúde indígena
#' em \code{tbSubTipo}. Testado contra a base de 2026-07: 1.572 estabelecimentos.
#'
#' A filtragem é feita via DuckDB diretamente sobre os parquets do cache (sem carregar o
#' \code{tbEstabelecimento} inteiro em R).
#'
#' @details
#' Particularidades da base:
#' \itemize{
#'   \item \code{tbEstabelecimento} tem duas colunas parecidas: \code{TP_UNIDADE} (a
#'     correta, populada) e \code{CO_TIPO_UNIDADE} (NULL em todas as linhas testadas).
#'   \item \code{CO_UNIDADE} de \code{rlEstabPoloAldeia} não é o CNES puro: é
#'     \code{CO_MUNICIPIO_GESTOR} (6 dígitos) + \code{CO_CNES} (7 dígitos) concatenados
#'     (não documentado no dicionário oficial). A função devolve só os 7 dígitos finais
#'     como \code{co_cnes}.
#' }
#'
#' @param competencia Competência (mês de referência) da base, string \code{"AAAAMM"}.
#'   Se \code{NULL}, usa a mais recente do cache.
#' @param tp_unidade Código(s) de \code{TP_UNIDADE} a manter. Padrão \code{"72"}
#'   (unidade de atenção à saúde indígena).
#' @param cache_dir Diretório de cache do pacote. Padrão:
#'   \code{tools::R_user_dir("indigenR", "cache")}.
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
#' @seealso \code{\link{cnes_tabela}} para ler qualquer outra tabela do CNES.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cnes <- cnes_importa_saude_indigena("202607")
#' cnes$estabelecimentos
#' }
cnes_importa_saude_indigena <- function(competencia = NULL,
                                         tp_unidade = "72",
                                         cache_dir = tools::R_user_dir("indigenR", "cache"),
                                         verbose = TRUE) {

  caminho_estab <- .cnes_caminho_tabela("tbEstabelecimento", competencia, cache_dir, verbose)
  caminho_rl <- .cnes_caminho_tabela("rlEstabPoloAldeia", competencia, cache_dir, verbose)
  caminho_polo <- .cnes_caminho_tabela("tbPoloBase", competencia, cache_dir, verbose)

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
    FROM read_parquet({caminho_estab}) e
    WHERE e.TP_UNIDADE IN ({tp_unidade*})
  ", .con = con))
  estab <- estab[!is.na(estab$latitude) & !is.na(estab$longitude), ]

  rl_polo_aldeia <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT RIGHT(CO_UNIDADE, 7) AS co_cnes, CO_POLOBASE AS co_polo_base
    FROM read_parquet({caminho_rl})
    WHERE CO_POLOBASE IS NOT NULL
  ", .con = con))

  polo_base <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT CO_SEQ_POLO_BASE AS co_polo_base, DS_POLO_BASE AS nm_polo_base
    FROM read_parquet({caminho_polo})
  ", .con = con))

  if (verbose) message("CNES — estabelecimentos com coordenadas: ", nrow(estab))

  list(
    estabelecimentos = tibble::as_tibble(estab),
    rl_polo_aldeia = tibble::as_tibble(rl_polo_aldeia),
    polo_base = tibble::as_tibble(polo_base)
  )
}
