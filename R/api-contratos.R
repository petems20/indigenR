#' Requisição de baixo nível para a API de Contratos (Compras.gov), com retentativa
#'
#' @keywords internal
#' @noRd
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
    return(jsonlite::fromJSON(txt, flatten = TRUE))
  }
}

#' Extrai contratos de uma Unidade Gestora via Contratos.gov.br
#'
#' Consulta a API pública (sem autenticação) do Contratos.gov.br
#' (\url{https://contratos.comprasnet.gov.br}) para uma Unidade Gestora (UG/UASG), e
#' opcionalmente também seus contratos inativos. Diferente de
#' \code{\link{puxa_api_compras_completo}}/\code{\link{puxa_api_pncp_completo}}, esta
#' API não pagina e não recebe parâmetros de querystring — a UG é informada no próprio
#' caminho da URL, e cada chamada já retorna todos os contratos daquela unidade. Não é
#' específica da Funai: qualquer código de UG cadastrado no Contratos.gov.br pode ser
#' usado.
#'
#' @param unidade_codigo Código da Unidade Gestora (UG/UASG), como \code{character} ou
#'   \code{numeric} (ex: \code{"194005"}). Mesmo espaço de código usado no SIAFI.
#' @param incluir_inativos Se \code{TRUE} (padrão), também busca os contratos inativos
#'   da UG e empilha com os ativos, marcando a coluna \code{situacao_consulta}
#'   (\code{"ativo"}/\code{"inativo"}).
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#' @param max_tries Número de retentativas por requisição HTTP, em caso de erro.
#'
#' @return Um \code{tibble} com os contratos da UG informada (colunas achatadas via
#'   \code{jsonlite::fromJSON(..., flatten = TRUE)}), ou uma tibble de 0 linhas se a UG
#'   não tiver contratos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_completo("194005")
#' }
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
puxa_api_contratos_completo <- function(unidade_codigo, incluir_inativos = TRUE,
                                         verbose = TRUE, max_tries = 5) {

  base_url <- "https://contratos.comprasnet.gov.br/api/contrato"

  busca_situacao <- function(url, situacao) {
    dados <- puxa_api_contratos(url, verbose = verbose, max_tries = max_tries)

    if (length(dados) == 0) {
      return(tibble::tibble())
    }

    dados <- tibble::as_tibble(dados)
    dados$situacao_consulta <- situacao
    dados
  }

  ativos <- busca_situacao(
    paste0(base_url, "/ug/", unidade_codigo), "ativo"
  )

  if (!incluir_inativos) {
    return(ativos)
  }

  inativos <- busca_situacao(
    paste0(base_url, "/inativo/ug/", unidade_codigo), "inativo"
  )

  dplyr::bind_rows(ativos, inativos)
}

#' Busca um sub-recurso de um contrato específico via Contratos.gov.br, com retentativa
#'
#' @keywords internal
#' @noRd
#' @importFrom tibble tibble as_tibble
.puxa_api_contratos_recurso <- function(contrato_id, recurso, verbose, max_tries) {
  url <- paste0(
    "https://contratos.comprasnet.gov.br/api/contrato/", contrato_id, "/", recurso
  )

  dados <- puxa_api_contratos(url, verbose = verbose, max_tries = max_tries)

  if (length(dados) == 0) {
    return(tibble::tibble())
  }

  dados <- tibble::as_tibble(dados)
  # `empenhos` não traz `contrato_id` no próprio registro (só `terceirizados` e
  # `historico` trazem) — atribuído aqui sempre, sobrescrevendo se já vier da API, para
  # que as três funções tenham essa coluna de forma uniforme e confiável.
  dados$contrato_id <- as.integer(contrato_id)
  dados
}

#' Extrai os IDs de contrato de uma ou mais Unidades Gestoras via Contratos.gov.br
#'
#' @keywords internal
#' @noRd
.puxa_api_contratos_ids <- function(unidade_codigo, incluir_inativos, verbose, max_tries) {
  contratos <- dplyr::bind_rows(lapply(
    unique(as.character(unidade_codigo)),
    function(ug) {
      puxa_api_contratos_completo(
        ug, incluir_inativos = incluir_inativos, verbose = verbose, max_tries = max_tries
      )
    }
  ))

  if (nrow(contratos) == 0 || !"id" %in% names(contratos)) {
    return(integer(0))
  }

  unique(contratos$id)
}

#' Busca um sub-recurso de todos os contratos de uma ou mais UGs via Contratos.gov.br
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
.puxa_api_contratos_recurso_completo <- function(unidade_codigo, recurso, incluir_inativos,
                                                  verbose, max_tries) {
  ids <- .puxa_api_contratos_ids(unidade_codigo, incluir_inativos, verbose, max_tries)

  if (length(ids) == 0) {
    if (verbose) message("Nenhum contrato encontrado para a(s) UG(s) informada(s).")
    return(tibble::tibble())
  }

  if (verbose) {
    message(length(ids), " contrato(s) encontrado(s). Buscando '", recurso, "' de cada um...")
  }

  dplyr::bind_rows(lapply(ids, function(id) {
    if (verbose) message("  Contrato ", id, "...")
    .puxa_api_contratos_recurso(id, recurso, verbose = FALSE, max_tries = max_tries)
  }))
}

#' Extrai os empenhos de um contrato via Contratos.gov.br
#'
#' @description
#' Consulta o endpoint público (sem autenticação) \code{/api/contrato/{contrato_id}/empenhos}
#' do Contratos.gov.br. Não é específica de nenhum órgão ou UG — qualquer \code{contrato_id}
#' válido nesse sistema pode ser usado. \code{\link{puxa_api_contratos_empenhos_completo}} é a
#' conveniência que já busca os empenhos de todos os contratos de uma ou mais UGs.
#'
#' @param contrato_id ID do contrato no Contratos.gov.br (coluna \code{id} retornada por
#'   \code{\link{puxa_api_contratos_completo}}).
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#' @param max_tries Número de retentativas por requisição HTTP, em caso de erro.
#'
#' @return Um \code{tibble} com um empenho por linha (colunas achatadas via
#'   \code{jsonlite::fromJSON(..., flatten = TRUE)}), incluindo a coluna \code{contrato_id};
#'   uma tibble de 0 linhas se o contrato não tiver empenhos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_empenhos(15590)
#' }
puxa_api_contratos_empenhos <- function(contrato_id, verbose = TRUE, max_tries = 5) {
  .puxa_api_contratos_recurso(contrato_id, "empenhos", verbose, max_tries)
}

#' Extrai os empenhos de todos os contratos de uma ou mais UGs via Contratos.gov.br
#'
#' @description
#' Conveniência sobre \code{\link{puxa_api_contratos_empenhos}}: busca a lista de contratos de
#' cada Unidade Gestora informada (via \code{\link{puxa_api_contratos_completo}}) e, para cada
#' contrato encontrado, busca seus empenhos — empilhando tudo em um único \code{tibble}.
#'
#' Não há um endpoint público por \strong{órgão}: \code{/api/v1/contrato/orgao/{orgao}} existe,
#' mas exige autenticação (confirmado: retorna HTTP 401 "Authorization Token not found" sem
#' token) — fora de escopo deste pacote por ora (mesma decisão já tomada no pipeline de
#' contratos do \code{relatorio_unidade}). Para importar "todos os empenhos de um órgão", informe
#' o vetor de códigos de UG daquele órgão em \code{unidade_codigo} (ex: obtido de uma listagem
#' própria de UGs do órgão, já que o Contratos.gov.br não expõe essa listagem sem login).
#'
#' @param unidade_codigo Um ou mais códigos de Unidade Gestora (UG/UASG) — ver
#'   \code{\link{puxa_api_contratos_completo}}.
#' @param incluir_inativos Se \code{TRUE}, também considera os contratos inativos de cada UG. Por
#'   padrão (\code{FALSE}), busca apenas os contratos ativos.
#' @param verbose Se \code{TRUE} (padrão), exibe mensagens de progresso.
#' @param max_tries Número de retentativas por requisição HTTP, em caso de erro.
#'
#' @return Um \code{tibble} com todos os empenhos de todos os contratos encontrados, incluindo a
#'   coluna \code{contrato_id}; uma tibble de 0 linhas se nenhuma UG tiver contratos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_empenhos_completo("194005")
#' puxa_api_contratos_empenhos_completo(c("194005", "194006"))
#' }
puxa_api_contratos_empenhos_completo <- function(unidade_codigo, incluir_inativos = FALSE,
                                                  verbose = TRUE, max_tries = 5) {
  .puxa_api_contratos_recurso_completo(
    unidade_codigo, "empenhos", incluir_inativos, verbose, max_tries
  )
}

#' Extrai os terceirizados de um contrato via Contratos.gov.br
#'
#' @description
#' Consulta o endpoint público (sem autenticação) \code{/api/contrato/{contrato_id}/terceirizados}
#' do Contratos.gov.br — trabalhadores terceirizados alocados no contrato (função, jornada,
#' salário, custo, situação). Não é específica de nenhum órgão ou UG.
#' \code{\link{puxa_api_contratos_terceirizados_completo}} é a conveniência que já busca os
#' terceirizados de todos os contratos de uma ou mais UGs.
#'
#' @inheritParams puxa_api_contratos_empenhos
#'
#' @return Um \code{tibble} com um terceirizado por linha, incluindo a coluna \code{contrato_id};
#'   uma tibble de 0 linhas se o contrato não tiver terceirizados.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_terceirizados(15590)
#' }
puxa_api_contratos_terceirizados <- function(contrato_id, verbose = TRUE, max_tries = 5) {
  .puxa_api_contratos_recurso(contrato_id, "terceirizados", verbose, max_tries)
}

#' Extrai os terceirizados de todos os contratos de uma ou mais UGs via Contratos.gov.br
#'
#' @description
#' Conveniência sobre \code{\link{puxa_api_contratos_terceirizados}} — mesmo padrão de
#' \code{\link{puxa_api_contratos_empenhos_completo}}: uma linha por UG informada vira uma
#' listagem de contratos, e cada contrato tem seus terceirizados buscados e empilhados.
#'
#' @inheritParams puxa_api_contratos_empenhos_completo
#'
#' @return Um \code{tibble} com todos os terceirizados de todos os contratos encontrados,
#'   incluindo a coluna \code{contrato_id}; uma tibble de 0 linhas se nenhuma UG tiver contratos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_terceirizados_completo("194005")
#' }
puxa_api_contratos_terceirizados_completo <- function(unidade_codigo, incluir_inativos = FALSE,
                                                        verbose = TRUE, max_tries = 5) {
  .puxa_api_contratos_recurso_completo(
    unidade_codigo, "terceirizados", incluir_inativos, verbose, max_tries
  )
}

#' Extrai o histórico (aditivos, apostilamentos, reajustes) de um contrato via Contratos.gov.br
#'
#' @description
#' Consulta o endpoint público (sem autenticação) \code{/api/contrato/{contrato_id}/historico}
#' do Contratos.gov.br: uma linha por alteração formal do contrato (termo aditivo, apostilamento,
#' repactuação, etc.), com uma descrição em texto livre (\code{observacao}) e os timestamps
#' \code{criado_em}/\code{alterado_em} de quando aquele registro de histórico foi
#' criado/alterado no sistema. É o único sinal público de "o que mudou e quando" neste sistema —
#' ver \strong{Cache incremental} em \code{\link{puxa_api_contratos_historico_completo}}.
#'
#' @inheritParams puxa_api_contratos_empenhos
#'
#' @return Um \code{tibble} com um evento de histórico por linha, incluindo a coluna
#'   \code{contrato_id}; uma tibble de 0 linhas se o contrato não tiver histórico registrado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_historico(15590)
#' }
puxa_api_contratos_historico <- function(contrato_id, verbose = TRUE, max_tries = 5) {
  .puxa_api_contratos_recurso(contrato_id, "historico", verbose, max_tries)
}

#' Extrai o histórico de todos os contratos de uma ou mais UGs via Contratos.gov.br
#'
#' @description
#' Conveniência sobre \code{\link{puxa_api_contratos_historico}} — mesmo padrão de
#' \code{\link{puxa_api_contratos_empenhos_completo}}.
#'
#' @section Cache incremental:
#' O Contratos.gov.br expõe, para praticamente todo sub-recurso (contrato, itens, empenhos,
#' terceirizados, histórico, ocorrências, publicações, garantias, cronogramas, despesas
#' acessórias, arquivos, responsáveis, prepostos, faturas), um endpoint em lote
#' \code{/api/v1/contrato/<recurso>?dt_alteracao_min=...&dt_alteracao_max=...} que devolveria,
#' numa única chamada e para \strong{todos os órgãos de uma vez}, tudo que foi alterado num
#' período — o desenho ideal para atualização incremental. Na investigação para esta função,
#' confirmei ao vivo que \strong{todo} endpoint sob \code{/api/v1/contrato/...} (incluindo esses
#' de \code{dt_alteracao_min}/\code{dt_alteracao_max}, e a listagem de contratos por órgão,
#' \code{/api/v1/contrato/orgao/{orgao}}) exige autenticação — sem token, todos retornam HTTP 401
#' (\code{"Authorization Token not found"}). Não há, portanto, um endpoint público de "o que
#' mudou desde X" que cubra várias UGs numa chamada só (mesma constatação já registrada no
#' pipeline de contratos do \code{relatorio_unidade}, que também deixou autenticação fora de
#' escopo).
#'
#' O endpoint público mais próximo disso é justamente \code{/api/contrato/{contrato_id}/historico}
#' (\code{\link{puxa_api_contratos_historico}}): por ser por contrato, não permite descobrir
#' contratos novos nem detectar mudanças de todos os contratos de uma UG numa chamada só, mas dá
#' um sinal barato (\code{criado_em}/\code{alterado_em} por evento) de que \emph{um contrato já
#' conhecido} mudou, sem precisar buscar de novo seus empenhos/terceirizados completos para
#' descobrir isso. Estratégia de cache incremental viável sem autenticação, para um chamador que
#' queira implementá-la:
#' \enumerate{
#'   \item Importação inicial: \code{puxa_api_contratos_completo()} para listar os contratos das
#'     UGs de interesse, e as funções \verb{_completo()} deste arquivo para os sub-recursos.
#'   \item A cada atualização: primeiro re-chama \code{puxa_api_contratos_completo()} (barato, não
#'     pagina) para pegar contratos novos, que entram na base normalmente.
#'   \item Para os contratos já conhecidos, chama \code{puxa_api_contratos_historico()} e compara
#'     a contagem de linhas (ou o \code{alterado_em} mais recente) com o que já está no cache — só
#'     os contratos cujo histórico mudou precisam ter empenhos/terceirizados buscados de novo.
#' }
#' Nenhuma dessas etapas é implementada aqui — este pacote só busca dados; o cache/upsert em si é
#' responsabilidade do projeto consumidor (ver \code{atualizacao.R}/\code{mescla_incremento_cache()}
#' no \code{relatorio_unidade}, que já segue esse padrão de upsert por \code{id}).
#'
#' @inheritParams puxa_api_contratos_empenhos_completo
#'
#' @return Um \code{tibble} com todos os eventos de histórico de todos os contratos encontrados,
#'   incluindo a coluna \code{contrato_id}; uma tibble de 0 linhas se nenhuma UG tiver contratos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' puxa_api_contratos_historico_completo("194005")
#' }
puxa_api_contratos_historico_completo <- function(unidade_codigo, incluir_inativos = TRUE,
                                                    verbose = TRUE, max_tries = 5) {
  .puxa_api_contratos_recurso_completo(
    unidade_codigo, "historico", incluir_inativos, verbose, max_tries
  )
}
