#' Executa uma requisição HTTP do VIS DATA com retentativa e backoff exponencial
#'
#' Retenta em caso de erro de conexão, limite de requisições (HTTP 429) ou erro de
#' servidor (5xx, tipicamente transitório); para em erro HTTP 4xx não recuperável (ex:
#' 404), já que retentar não mudaria o resultado.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr status_code
.visdata_com_retry <- function(fazer_requisicao, contexto, max_tries, verbose) {
  tentativa <- 1
  repeat {
    resp <- tryCatch(fazer_requisicao(), error = function(e) e)

    if (inherits(resp, "error")) {
      if (tentativa >= max_tries) {
        stop(
          "Falha de conexão ao ", contexto, " após ", tentativa, " tentativas.\n",
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
          "Erro HTTP ", sc, " persistente ao ", contexto, " após ", tentativa, " tentativas.",
          call. = FALSE
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
      stop("Erro HTTP ", sc, " ao ", contexto, ".", call. = FALSE)
    }

    return(resp)
  }
}

#' Monta os parâmetros de uma coluna do DataTables (server-side processing) do VIS DATA
#' @keywords internal
#' @noRd
.visdata_coluna_datatable <- function(indice, nome) {
  setNames(
    list(indice, nome, "true", "true", "", "false"),
    sprintf(
      c(
        "columns[%d][data]",
        "columns[%d][name]",
        "columns[%d][searchable]",
        "columns[%d][orderable]",
        "columns[%d][search][value]",
        "columns[%d][search][regex]"
      ),
      indice
    )
  )
}

#' Extrai o token de sessão "rqprocess" da página do VIS DATA
#' @keywords internal
#' @noRd
.visdata_extrai_token <- function(html, nome_indicador) {
  m <- regmatches(
    html,
    regexpr("rqprocess[\"']?\\s*[:=]\\s*[\"']?[a-f0-9]{32}", html, perl = TRUE)
  )

  if (length(m) == 0) {
    stop(
      "Não encontrei o token de sessão ('rqprocess') para o indicador '", nome_indicador,
      "'. O endpoint do VIS DATA pode ter mudado — verifique 'q_param' e 'base_url'.",
      call. = FALSE
    )
  }

  regmatches(m, regexpr("[a-f0-9]{32}$", m, perl = TRUE))
}

#' Consulta um indicador municipal do VIS DATA 3 (endpoint interno de DataTables)
#'
#' @description
#' Réplica a chamada AJAX server-side do DataTables usada pela própria página do VIS
#' DATA 3: um \code{GET} inicial na página (com o \code{q_param} do indicador) para obter
#' o token de sessão \code{rqprocess}, seguido de um \code{POST} pedindo todas as linhas
#' de uma vez (\code{length} maior que o total de municípios).
#'
#' @keywords internal
#' @noRd
#' @importFrom httr GET POST user_agent timeout content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
.visdata_consulta_indicador <- function(q_param, nomes_indicadores, base_url, max_tries, verbose) {

  ua <- httr::user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
  )

  rotulo <- paste(nomes_indicadores, collapse = ", ")

  if (verbose) message("VIS DATA — consultando indicador(es): ", rotulo)

  pagina <- .visdata_com_retry(
    function() {
      httr::GET(
        base_url,
        query = list(`q[]` = q_param, ultdisp = 1, ag = "m"),
        httr::timeout(30),
        ua
      )
    },
    contexto = paste0("obter o token de sessão do indicador '", rotulo, "'"),
    max_tries = max_tries,
    verbose = verbose
  )

  html <- httr::content(pagina, as = "text", encoding = "UTF-8")
  token <- .visdata_extrai_token(html, rotulo)

  # O `q_param` já determina, do lado do servidor, quantas e quais colunas de
  # indicador a consulta traz (ex: uma consulta de "pessoas por raça/cor" traz as 6
  # categorias juntas, não só a que interessa) — as definições de coluna abaixo não
  # filtram o conteúdo, só descrevem ao DataTables o que esperar. `nomes_indicadores`
  # precisa ter um nome por coluna de indicador realmente devolvida, na mesma ordem;
  # a checagem de largura logo abaixo detecta um descompasso em vez de nomear colunas
  # erradas silenciosamente.
  colunas_indicadores <- unlist(
    lapply(seq_along(nomes_indicadores), function(i) {
      .visdata_coluna_datatable(3 + i, nomes_indicadores[i])
    }),
    recursive = FALSE
  )

  body <- c(
    list(draw = 1),
    .visdata_coluna_datatable(0, "codigo"),
    .visdata_coluna_datatable(1, "nome"),
    .visdata_coluna_datatable(2, "sigla"),
    .visdata_coluna_datatable(3, "mes_ano_formatado"),
    colunas_indicadores,
    list(
      `order[0][column]` = 2,
      `order[0][dir]` = "asc",
      `order[1][column]` = 0,
      `order[1][dir]` = "asc",
      start = 0,
      length = 10000,
      `search[value]` = "",
      `search[regex]` = "false"
    )
  )

  resp <- .visdata_com_retry(
    function() {
      httr::POST(
        base_url,
        query = list(
          `q[]` = q_param, ultdisp = 1, ag = "m", wt = "json",
          tp_funcao_consulta = 0, rqprocess = token
        ),
        body = body,
        encode = "form",
        httr::timeout(30),
        httr::add_headers(`X-Requested-With` = "XMLHttpRequest"),
        ua
      )
    },
    contexto = paste0("baixar os dados do indicador '", rotulo, "'"),
    max_tries = max_tries,
    verbose = verbose
  )

  json <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

  if (verbose) message("  Registros: ", json$recordsTotal)

  df <- as.data.frame(json$data, stringsAsFactors = FALSE)

  n_esperado <- 4 + length(nomes_indicadores)
  if (ncol(df) != n_esperado) {
    stop(
      "A resposta do VIS DATA para '", rotulo, "' trouxe ", ncol(df), " coluna(s), mas ",
      "'nomes_indicadores' (", length(nomes_indicadores), " nome(s)) esperava ", n_esperado,
      " no total (4 colunas fixas + indicadores). A consulta subjacente a este 'q_param' ",
      "pode trazer um conjunto de colunas diferente do esperado — reinspecione a resposta ",
      "AJAX no navegador (aba Rede/Network) e ajuste 'nomes_indicadores' para bater com a ",
      "ordem realmente devolvida.",
      call. = FALSE
    )
  }

  names(df) <- c("codigo_ibge", "municipio", "uf", "referencia", nomes_indicadores)

  # Números vêm formatados em pt-BR (ex: "1.234"); remove o separador de milhar antes
  # de converter — não há casas decimais nesses indicadores (contagens).
  for (nm in nomes_indicadores) {
    df[[nm]] <- as.numeric(gsub(".", "", df[[nm]], fixed = TRUE))
  }

  tibble::as_tibble(df)
}

#' Importa um indicador municipal do VIS DATA (Cadastro Único / MDS)
#'
#' @description
#' Consulta o endpoint interno de DataTables do VIS DATA 3
#' (\url{https://aplicacoes.cidadania.gov.br/vis/data3/v.php}), do Ministério do
#' Desenvolvimento e Assistência Social, e retorna, para todos os municípios do Brasil,
#' um indicador do Cadastro Único para Programas Sociais na última referência disponível.
#' Não é específica de nenhum indicador em particular — o parâmetro \code{q_param} é o
#' token de consulta que o próprio VIS DATA gera para a combinação de indicador/filtros
#' escolhida na interface (ver \strong{Como obter q_param} abaixo).
#' \code{\link{visdata_importa_racor_indigena}} é a conveniência que já traz prontos os
#' dois indicadores de raça/cor indígena do Cadastro Único.
#'
#' @section Como obter q_param:
#' O VIS DATA não documenta esse parâmetro publicamente. Para obter o \code{q_param} de
#' um indicador: abra \url{https://aplicacoes.cidadania.gov.br/vis/data3/v.php} no
#' navegador, monte a consulta desejada na interface (nível "por município", indicador de
#' interesse) e inspecione, nas ferramentas de desenvolvedor (aba Rede/Network), a query
#' string da própria página — o valor do parâmetro \code{q[]} é o \code{q_param}. É um
#' endpoint interno (não documentado) e pode mudar sem aviso; se parar de funcionar,
#' repita essa inspeção.
#'
#' @param q_param \code{character}. Token de consulta do indicador, gerado pelo VIS DATA
#'   (ver \strong{Como obter q_param}). Se copiado já com caracteres percent-encoded
#'   (ex: contém \code{\%2F}), decodifique antes com \code{\link{URLdecode}}.
#' @param nomes_indicadores \code{character}. Um nome por coluna de indicador realmente
#'   devolvida pela consulta, na mesma ordem em que aparecem na resposta (ex:
#'   \code{"n_pess_indigena_cad"} para um único indicador; um vetor com vários nomes se
#'   o \code{q_param} corresponder a uma consulta que já agrupa várias categorias,
#'   como as 6 categorias de raça/cor de uma consulta "pessoas por raça/cor" — nesse
#'   caso o servidor devolve todas juntas, independente de quantas colunas se peça). Uma
#'   quantidade de nomes que não bate com o número de colunas devolvidas gera erro, em
#'   vez de nomear colunas erradas silenciosamente.
#' @param base_url URL base do endpoint do VIS DATA 3.
#' @param max_tries Número de retentativas por requisição HTTP, em caso de erro de
#'   conexão, limite de requisições (HTTP 429) ou erro de servidor (5xx).
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#'
#' @return Um \code{tibble} com uma linha por município: \code{codigo_ibge},
#'   \code{municipio}, \code{uf}, \code{referencia} (mês/ano da última publicação
#'   disponível) e uma coluna por indicador, nomeadas conforme \code{nomes_indicadores}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' visdata_importa_indicador(
#'   q_param = "oNOcls...",
#'   nomes_indicadores = "n_pess_indigena_cad"
#' )
#' }
visdata_importa_indicador <- function(q_param,
                                       nomes_indicadores,
                                       base_url = "https://aplicacoes.cidadania.gov.br/vis/data3/v.php",
                                       max_tries = 5,
                                       verbose = TRUE) {
  .visdata_consulta_indicador(
    q_param = q_param,
    nomes_indicadores = nomes_indicadores,
    base_url = base_url,
    max_tries = max_tries,
    verbose = verbose
  )
}

#' Importa dados de raça/cor indígena do Cadastro Único (VIS DATA), por município
#'
#' @description
#' Conveniência sobre \code{\link{visdata_importa_indicador}} que já traz prontos os dois
#' indicadores municipais de raça/cor indígena do Cadastro Único para Programas Sociais
#' (CadÚnico), unidos em um único \code{tibble}: número de famílias com responsável
#' familiar de raça/cor indígena (\code{n_fam_indigena_cad}) e número de pessoas de
#' raça/cor indígena (\code{n_pess_indigena_cad}), ambos na última referência disponível.
#' A união dos dois indicadores é feita por \code{codigo_ibge} (identificador estável),
#' não pelo nome do município.
#'
#' @param max_tries Número de retentativas por requisição HTTP — ver
#'   \code{\link{visdata_importa_indicador}}.
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#'
#' @return Um \code{tibble} com uma linha por município: \code{codigo_ibge},
#'   \code{municipio}, \code{uf}, \code{referencia}, \code{n_fam_indigena_cad} e
#'   \code{n_pess_indigena_cad}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' visdata_importa_racor_indigena()
#' }
#' @importFrom dplyr left_join select
visdata_importa_racor_indigena <- function(max_tries = 5, verbose = TRUE) {

  # Tokens de consulta gerados pela interface do VIS DATA (ver `@section Como obter
  # q_param` em `visdata_importa_indicador()`) para os indicadores municipais "Famílias
  # com responsável familiar de raça/cor indígena" e "Pessoas de raça/cor indígena" do
  # Cadastro Único. O segundo já foi copiado percent-encoded e por isso passa por
  # URLdecode() aqui — repassá-lo sem decodificar faria o GET/POST codificá-lo de novo,
  # invalidando o token.
  q_param_fam <- "oNOclsLerpibuKep3bV+fmhe05Kv2rmg2a19ZW51ZXKmaX6JaV2Jl2CadWCNrMmTbXmUoNqmrLelkbbElM54mb7nwJl3rpam7J6IiZ2Ow9SYpXim0ujJep21+Ofloq7BV3bFxfYXpJjL3MBUpbaoneuiwa+qTcXQU62el77uwaaraPjU56KwvbOdkt2v5ng="

  q_param_pes <- URLdecode(
    "oNOclsLerpibuKep3bWEf7Ne09Gv17lljax%2FYWyAYmqqdH9%2BaWOEkWuXbWTZ6aykobuomdurrryajrbElM54mb7nwJl3rpam7J6IiZ2Ow9SYpXim0uhwopu4mq3smL3AnKG4wJbLoW7D3LmnoYObm%2BWssolyk7jNps94btDwuleqp6Wf7Kysr6SOycafy5yWvt%2BImp20qJ%2B0n666qpKSnJnLqabCtoinsbVYqNipssGqjMfCpc6eksDcsW%2BiqaGt3nSzr6OgvJxu0J6f0OCIb6%2B9ol3nmL2zqqC2yqHOpprC6a6Tn6mZdd%2BaucGcaL3Cn92ibpjhrqCvrXB17K66caWMx8am3Zylvt6uk6%2BtoqzerL2tmo67nJnLqabCtrOVqLuadbSfrrqqkpKcpt%2Bqr67wrqKwsZmb3Z5tspxNx8am3ayU0Juvpp22mJvsWba8qpDJyqfLsFPL6m13nayWre2rvG76x8XKltlghNLcu6ilrJae3lmxs1edvNSm2Z6mfeu%2FmbCpqFrip8CxqZbLwqaKq6J9vq6YnbuprOhZEOillrrQVruylMvvtpidrJpa3Z5tvpygytCU3V2Uyty%2FmaipqFrip8CxqZbLwqaKq6J9vq6YnbuprOhZEOillrrQVruylMvvtpidrJpa3Z5tvpygytCU3V2jvu2xla9onqjsnL%2B3q47KgaHZXXa%2B366nsLqkWjzTu7eanHqyqMurp8bfrpihaJmfmamywaqcuNRT06uXICi0maqpqFrip8CxqZbLwqaKq6J9vq6YnbuprOhZEOillrrQVruylMvvtpidrJpa3Z5tvpygytCU3V2mwuhtnaqupKzmmhD1%2BtDGgabZn6XCm7%2BV%2F%2B%2BWadyov26gm8rEpdOxlNCbu6Nci5ae2qzBwKZNGvuh06Ci2euIsLjEcA%3D%3D"
  )

  # A consulta "pessoas" traz sempre as 6 categorias de raça/cor juntas (o servidor não
  # filtra por coluna pedida — ver nota em `.visdata_consulta_indicador()`); a ordem
  # abaixo (branca, preta, amarela, parda, indígena, sem resposta) foi confirmada
  # inspecionando a resposta real do endpoint para este `q_param`.
  nomes_pes <- c(
    "n_pess_branca_cad", "n_pess_preta_cad", "n_pess_amarela_cad",
    "n_pess_parda_cad", "n_pess_indigena_cad", "n_pess_raca_semresp_cad"
  )

  dados_fam <- visdata_importa_indicador(
    q_param = q_param_fam,
    nomes_indicadores = "n_fam_indigena_cad",
    max_tries = max_tries,
    verbose = verbose
  )

  dados_pes <- visdata_importa_indicador(
    q_param = q_param_pes,
    nomes_indicadores = nomes_pes,
    max_tries = max_tries,
    verbose = verbose
  )

  dplyr::left_join(
    dados_fam,
    dplyr::select(dados_pes, codigo_ibge, n_pess_indigena_cad),
    by = "codigo_ibge"
  )
}
