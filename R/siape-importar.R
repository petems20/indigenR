#' Nomes de coluna do arquivo Cadastro.csv do SIAPE
#'
#' Layout fixo (posicional) usado pelo Portal da Transparência no ZIP mensal de
#' "Servidores SIAPE" para o arquivo \code{Cadastro.csv}. Não é específico de nenhum
#' órgão — é o esquema do próprio arquivo.
#' @keywords internal
#' @noRd
.siape_colunas_cadastro <- function() {
  c(
    "id_servidor_portal",
    "nome",
    "cpf",
    "matricula",
    "descricao_cargo",
    "classe_cargo",
    "referencia_cargo",
    "padrao_cargo",
    "nivel_cargo",
    "sigla_funcao",
    "nivel_funcao",
    "funcao",
    "codigo_atividade",
    "atividade",
    "opcao_parcial",
    "cod_uorg_lotacao",
    "uorg_lotacao",
    "cod_org_lotacao",
    "org_lotacao",
    "cod_orgsup_lotacao",
    "orgsup_lotacao",
    "cod_uorg_exercicio",
    "uorg_exercicio",
    "cod_org_exercicio",
    "org_exercicio",
    "cod_orgsup_exercicio",
    "orgsup_exercicio",
    "cod_tipo_vinculo",
    "tipo_vinculo",
    "situacao_vinculo",
    "data_inicio_afastamento",
    "data_termino_afastamento",
    "regime_juridico",
    "jornada_trabalho",
    "data_ingresso_cargo_funcao",
    "data_nomeacao_cargo_funcao",
    "data_ingresso_orgao",
    "documento_ingresso_servico_publico",
    "data_diploma_ingresso_servico_publico",
    "diploma_ingresso_cargo_funcao",
    "diploma_ingresso_orgao",
    "diploma_ingresso_servico_publico",
    "uf_exercicio"
  )
}

#' Normaliza um anomês para o formato AAAAMM usado pelo Portal da Transparência
#'
#' @param anomes Um dos formatos: string de 6 dígitos (\code{"202510"}), string
#'   \code{"AAAA-MM"}/\code{"AAAA/MM"}, objeto \code{Date}/\code{POSIXct}, ou numérico
#'   (\code{202510}).
#'
#' @return String de 6 dígitos no formato \code{AAAAMM}.
#'
#' @keywords internal
#' @noRd
#' @importFrom lubridate year month
#' @importFrom stringr str_detect str_remove_all
.siape_normaliza_anomes <- function(anomes) {

  if (inherits(anomes, "Date") || inherits(anomes, "POSIXct")) {
    return(sprintf("%04d%02d", lubridate::year(anomes), lubridate::month(anomes)))
  }

  anomes_chr <- as.character(anomes)

  if (stringr::str_detect(anomes_chr, "^\\d{6}$")) {
    return(anomes_chr)
  }

  if (stringr::str_detect(anomes_chr, "^\\d{4}[-/]\\d{2}$")) {
    return(stringr::str_remove_all(anomes_chr, "[-/]"))
  }

  stop(
    "Formato de 'anomes' não reconhecido: '", anomes_chr, "'. ",
    "Use 'AAAAMM' (ex: '202510'), 'AAAA-MM', 'AAAA/MM', ou um objeto Date.",
    call. = FALSE
  )
}

#' Localiza o anomês mais recente disponível para download no Portal da Transparência
#'
#' Sonda, mês a mês a partir do mês corrente, a URL de download dos dados de "Servidores
#' SIAPE" até encontrar a primeira publicação disponível.
#'
#' @param max_tentativas_anomes Quantos meses, no máximo, retroceder a partir do mês
#'   corrente antes de desistir.
#' @param max_tries Número de tentativas de retentativa (retry) por sondagem, em caso de
#'   erro de conexão/HTTP.
#' @param verbose Se \code{TRUE}, exibe mensagens de progresso.
#'
#' @return String de 6 dígitos (\code{AAAAMM}) do anomês mais recente disponível.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr HEAD GET status_code headers timeout
#' @importFrom lubridate today `%m-%`
.siape_localiza_anomes_disponivel <- function(max_tentativas_anomes = 12,
                                               max_tries = 5,
                                               verbose = TRUE) {

  usa_head <- TRUE

  for (i in 0:(max_tentativas_anomes - 1)) {

    candidato <- .siape_normaliza_anomes(lubridate::today() %m-% months(i))
    url <- paste0(
      "https://portaldatransparencia.gov.br/download-de-dados/servidores/",
      candidato, "_Servidores_SIAPE"
    )

    if (verbose) {
      message("Verificando disponibilidade do anomês ", candidato, "...")
    }

    resp <- if (usa_head) {
      tryCatch(httr::HEAD(url, httr::timeout(15)), error = function(e) e)
    } else {
      tryCatch(httr::GET(url, httr::timeout(15)), error = function(e) e)
    }

    if (inherits(resp, "error")) {
      if (usa_head) {
        usa_head <- FALSE
        resp <- tryCatch(httr::GET(url, httr::timeout(15)), error = function(e) e)
      }
      if (inherits(resp, "error")) {
        next
      }
    }

    content_type <- httr::headers(resp)[["content-type"]]
    is_hit <- httr::status_code(resp) == 200 &&
      !is.null(content_type) &&
      stringr::str_detect(tolower(content_type), "zip|octet-stream")

    if (is_hit) {
      if (verbose) {
        message("Anomês mais recente disponível: ", candidato)
      }
      return(candidato)
    }
  }

  stop(
    "Nenhum anomês disponível encontrado nos últimos ", max_tentativas_anomes,
    " meses. Informe 'anomes' manualmente.",
    call. = FALSE
  )
}

#' Baixa, extrai e lê o Cadastro.csv de um anomês do SIAPE, com retentativa
#'
#' @keywords internal
#' @noRd
#' @importFrom httr GET status_code timeout write_disk
#' @importFrom readr read_delim locale cols
.siape_baixa_extrai_le <- function(anomes, max_tries = 5, verbose = TRUE) {

  url <- paste0(
    "https://portaldatransparencia.gov.br/download-de-dados/servidores/",
    anomes, "_Servidores_SIAPE"
  )

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)

  extract_dir <- tempfile()
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)

  tentativa <- 1

  repeat {

    if (verbose) {
      message("Baixando dados do SIAPE (anomês ", anomes, ")...")
    }

    resp <- tryCatch(
      httr::GET(url, httr::timeout(60), httr::write_disk(zip_path, overwrite = TRUE)),
      error = function(e) e
    )

    if (inherits(resp, "error")) {

      if (tentativa >= max_tries) {
        stop(
          "Falha de conexão ao baixar o anomês ", anomes, " após ",
          tentativa, " tentativas.\n", resp$message,
          call. = FALSE
        )
      }

      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) {
        message("Erro de conexão. Nova tentativa em ", wait_time, " segundos...")
      }
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }

    sc <- httr::status_code(resp)

    if (sc >= 400) {

      if (tentativa >= max_tries) {
        stop(
          "Erro HTTP ", sc, " ao baixar o anomês ", anomes, " após ",
          tentativa, " tentativas.",
          call. = FALSE
        )
      }

      wait_time <- 2 ^ (tentativa - 1)
      if (verbose) {
        message("Erro HTTP ", sc, ". Nova tentativa em ", wait_time, " segundos...")
      }
      Sys.sleep(wait_time)
      tentativa <- tentativa + 1
      next
    }

    break
  }

  if (verbose) message("Download concluído. Extraindo arquivos...")
  unzip(zip_path, exdir = extract_dir)

  csv_file <- list.files(
    extract_dir, pattern = "Cadastro\\.csv$", full.names = TRUE, recursive = TRUE
  )

  if (length(csv_file) == 0) {
    stop("Arquivo de cadastro não encontrado no ZIP do anomês ", anomes, ".", call. = FALSE)
  }
  if (length(csv_file) > 1) {
    warning(
      "Mais de um arquivo Cadastro.csv encontrado no ZIP: ",
      paste(csv_file, collapse = ", "), ". Usando o primeiro."
    )
    csv_file <- csv_file[1]
  }

  if (verbose) message("Lendo arquivo CSV...")
  df <- readr::read_delim(
    csv_file,
    delim = ";",
    locale = readr::locale(encoding = "latin1"),
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )

  cols <- .siape_colunas_cadastro()

  if (ncol(df) != length(cols)) {
    stop(
      "O arquivo Cadastro.csv do anomês ", anomes, " tem ", ncol(df),
      " colunas; eram esperadas ", length(cols),
      ". O layout do Portal da Transparência pode ter mudado.",
      call. = FALSE
    )
  }

  colnames(df) <- cols

  df
}

#' Importa dados de servidores do SIAPE do Portal da Transparência
#'
#' Baixa o arquivo mensal "Cadastro de Servidores SIAPE" do Portal da Transparência
#' (\url{https://portaldatransparencia.gov.br/download-de-dados/servidores}), extrai o
#' \code{Cadastro.csv} e retorna os dados como \code{tibble}. Quando \code{anomes} não é
#' informado, detecta automaticamente o anomês mais recente disponível para download.
#' Não é específica da Funai: qualquer código de órgão (\code{COD_ORG_EXERCICIO}) pode ser
#' usado para filtrar — \code{\link{funai_importa_siape}} é a conveniência que já informa
#' o código da Funai.
#'
#' @param anomes Anomês desejado (\code{"AAAAMM"}, \code{"AAAA-MM"}, \code{Date}, ou
#'   numérico). Se \code{NULL} (padrão), detecta automaticamente o anomês mais recente
#'   disponível.
#' @param codigo_orgao Código do órgão de exercício (\code{cod_org_exercicio}) para
#'   filtrar os servidores. Se \code{NULL} (padrão), nenhum filtro é aplicado e a base
#'   completa do funcionalismo público federal é retornada (arquivo grande).
#' @param destino Caminho de um arquivo \code{.csv} onde salvar o resultado. Se
#'   \code{NULL} (padrão), o resultado apenas é retornado, sem gravar em disco.
#' @param max_tentativas_anomes Quando \code{anomes} não é informado, quantos meses, no
#'   máximo, retroceder a partir do mês corrente ao procurar a publicação mais recente.
#' @param max_tries Número de retentativas por requisição HTTP, em caso de erro de
#'   conexão ou HTTP.
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#'
#' @return Um \code{tibble} com os dados de servidores (todas as colunas como
#'   \code{character}, para preservar zeros à esquerda em campos como CPF e matrícula).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Anomês mais recente, todos os órgãos
#' siape_importa_dados()
#'
#' # Anomês específico, filtrado para um órgão
#' siape_importa_dados(anomes = "202509", codigo_orgao = 30202)
#' }
#' @importFrom dplyr filter
#' @importFrom readr write_csv
siape_importa_dados <- function(anomes = NULL,
                                 codigo_orgao = NULL,
                                 destino = NULL,
                                 max_tentativas_anomes = 12,
                                 max_tries = 5,
                                 verbose = TRUE) {

  anomes_final <- if (is.null(anomes)) {
    .siape_localiza_anomes_disponivel(max_tentativas_anomes, max_tries, verbose)
  } else {
    .siape_normaliza_anomes(anomes)
  }

  if (verbose) message("Importando SIAPE — anomês: ", anomes_final)

  dados <- .siape_baixa_extrai_le(anomes_final, max_tries = max_tries, verbose = verbose)

  if (!is.null(codigo_orgao)) {
    dados <- dplyr::filter(dados, cod_org_exercicio == as.character(codigo_orgao))
  } else if (verbose) {
    message("Nenhum 'codigo_orgao' informado: retornando todos os servidores federais.")
  }

  if (!is.null(destino)) {
    pasta_destino <- dirname(destino)
    if (!dir.exists(pasta_destino)) dir.create(pasta_destino, recursive = TRUE)
    readr::write_csv(dados, destino)
    if (verbose) message("Arquivo salvo em: ", destino)
  }

  dados
}

#' Importa dados de servidores da Funai no SIAPE
#'
#' Conveniência sobre \code{\link{siape_importa_dados}} que já filtra pelo código de
#' órgão de exercício da Funai (30202).
#'
#' @param anomes Anomês desejado. Se \code{NULL} (padrão), detecta automaticamente o
#'   anomês mais recente disponível — ver \code{\link{siape_importa_dados}}.
#' @param destino Caminho de um arquivo \code{.csv} onde salvar o resultado. Se
#'   \code{NULL} (padrão), o resultado apenas é retornado, sem gravar em disco.
#'
#' @return Um \code{tibble} com os dados dos servidores da Funai.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funai_importa_siape()
#' }
funai_importa_siape <- function(anomes = NULL, destino = NULL) {

  codigo_funai_siape <- 30202

  siape_importa_dados(
    anomes = anomes,
    codigo_orgao = codigo_funai_siape,
    destino = destino
  )
}
