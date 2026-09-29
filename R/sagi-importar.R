#' Executa uma requisição HTTP à API da SAGI/MDS com retentativa e backoff exponencial
#'
#' Retenta em caso de erro de conexão, limite de requisições (HTTP 429) ou erro de
#' servidor (5xx); para em erro HTTP 4xx não recuperável.
#'
#' @keywords internal
#' @noRd
.sagi_com_retry <- function(fazer_requisicao, contexto, max_tries, verbose) {
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

#' Importa equipamentos socioassistenciais da API da SAGI/MDS
#'
#' @description
#' Consulta a API de equipamentos da SAGI (Secretaria de Avaliação, Gestão da Informação
#' e Cadastro Único, MDS) — um único endpoint Solr para CRAS, Unidades de Acolhimento e
#' demais equipamentos, que muda só pelo filtro \code{tipo_equipamento} — e devolve os
#' equipamentos como pontos.
#'
#' O campo \code{georef_location} chega como uma única string \code{"lat\\,lon"} (a API
#' escapa a vírgula interna com uma barra invertida literal); a função remove o escape
#' e separa em \code{latitude}/\code{longitude} numéricas. Equipamentos sem coordenada
#' válida são descartados.
#'
#' @param tipo_equipamento Tipo de equipamento, exatamente como cadastrado na API (ex:
#'   \code{"CRAS"}). Um único valor.
#' @param url_base URL do endpoint de equipamentos da SAGI.
#' @param max_tries Número máximo de tentativas da requisição.
#' @param verbose Se \code{TRUE}, exibe mensagens de progresso e retentativa.
#'
#' @return Um objeto \code{sf} de pontos em EPSG:4326, uma linha por equipamento, com as
#'   colunas originais da API mais \code{latitude} e \code{longitude}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cras <- sagi_importa_equipamentos("CRAS")
#' }
sagi_importa_equipamentos <- function(tipo_equipamento,
                                      url_base = "https://aplicacoes.mds.gov.br/sagi/servicos/equipamentos",
                                      max_tries = 5,
                                      verbose = TRUE) {

  if (length(tipo_equipamento) != 1) {
    stop("'tipo_equipamento' deve ter um único valor.", call. = FALSE)
  }

  campos <- c(
    "id_equipamento", "ibge", "uf", "cidade", "nome", "responsavel", "telefone",
    "email", "situacao", "orgao_gestor", "endereco", "numero", "complemento",
    "referencia", "bairro", "cep", "georef_location", "data_atualizacao"
  )

  if (verbose) message("SAGI — consultando equipamentos: ", tipo_equipamento)

  resp <- .sagi_com_retry(
    function() {
      httr::GET(
        url_base,
        query = list(
          q = "*:*",
          fq = glue::glue('tipo_equipamento:"{tipo_equipamento}"'),
          wt = "csv",
          fl = paste(campos, collapse = ","),
          rows = "999999999"
        ),
        httr::timeout(120)
      )
    },
    contexto = paste0("consultar equipamentos '", tipo_equipamento, "'"),
    max_tries = max_tries,
    verbose = verbose
  )

  df <- readr::read_csv(
    I(httr::content(resp, as = "text", encoding = "UTF-8")),
    col_types = readr::cols(.default = readr::col_character())
  )

  if (nrow(df) == 0) {
    stop("Nenhum equipamento encontrado para tipo_equipamento = '", tipo_equipamento, "'.", call. = FALSE)
  }

  # georef_location chega como "lat\,lon" — a barra invertida precisa sair antes do
  # split, senão sobra um "\" no fim da latitude e as.numeric() vira NA.
  partes <- strsplit(gsub("\\\\", "", df$georef_location), ",", fixed = TRUE)
  df$latitude <- suppressWarnings(as.numeric(trimws(vapply(partes, `[`, character(1), 1))))
  df$longitude <- suppressWarnings(as.numeric(trimws(vapply(partes, `[`, character(1), 2))))

  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]

  if (verbose) message("  Equipamentos com coordenadas: ", nrow(df))

  sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
}
