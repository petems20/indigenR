#' Requisição POST para a API do SIORG Cidadão
#'
#' @description Função interna auxiliar para enviar requisições POST formatadas para o
#' endpoint da API do SIORG Cidadão. Garante o envio estrito do JSON mapeando valores nulos.
#'
#' @param endpoint \code{character}. Caminho do endpoint da API (ex: "/relatorio-dinamico/gerar").
#' @param body \code{list}. Lista nomeada contendo o payload da requisição. O padrão é uma lista vazia.
#'
#' @return Um objeto de resposta HTTP contendo o retorno da API.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr POST add_headers stop_for_status
#' @importFrom jsonlite toJSON
siorg_post <- function(endpoint, body = list()) {

  url <- paste0(
    "https://siorg.gov.br",
    "/siorg-cidadao-webapp/api",
    endpoint
  )

  json_body <- jsonlite::toJSON(
    body,
    auto_unbox = TRUE,
    null = "null",
    pretty = FALSE
  )

  res <- httr::POST(
    url,
    body = json_body,
    encode = "raw",
    httr::add_headers(
      Accept = "*/*",
      `Content-Type` = "application/json",
      Origin = "https://siorg.gov.br",
      Referer = "https://siorg.gov.br/siorg-cidadao-webapp/resources/app/relatorio-dinamico.html",
      `User-Agent` = "Mozilla/5.0"
    )
  )

  httr::stop_for_status(res)

  return(res)
}

#' Estrutura organizacional da Funai, com classificação hierárquica e geocodificação
#'
#' Consulta a estrutura organizacional completa da Funai via
#' \code{\link{siorg_estrutura_completa}}, classifica cada unidade em \code{nivel_1}
#' (diretoria) e \code{nivel_2} (coordenação), junta a divisão territorial do IBGE (para
#' obter nome do município e UF a partir do código de município) e geocodifica os
#' endereços das unidades via \code{\link{geocodifica_enderecos}}.
#'
#' @param dados Data.frame originado do relatório dinâmico do SIORG (precisa ter a coluna
#'   \code{Código Unidade}) — normalmente o retorno de \code{\link{funai_relatorio_estrutura}}.
#'
#' @return Um \code{data.frame} combinando o relatório dinâmico informado com a
#' classificação hierárquica, dados territoriais do IBGE e coordenadas geográficas.
#'
#' @importFrom httr GET http_error status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate across starts_with distinct rename transmute filter left_join
#'   arrange select join_by
#' @importFrom tidyr unnest
#' @importFrom readxl read_xls
#' @import sf
#' @export
prepara_dados_estrutura <- function(dados) {

  codigo_funai <- 173

  dados_api <- siorg_estrutura_completa(codigo_funai)

  niveis <- .siorg_classifica_hierarquia(dados_api)

  # GEOCODIFICANDO ENDEREÇOS ----

  url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2025/DTB_2025.zip"

  tmp <- tempfile(fileext = ".zip")
  download.file(url, tmp, mode = "wb")

  dir <- tempfile()
  dir.create(dir)
  unzip(tmp, exdir = dir)

  municipios_ibge <- readxl::read_xls(
    list.files(dir, pattern = "\\MUNICIPIOS.xls$", full.names = TRUE),
    skip = 6
  )

  dados_api <- dados_api |>
    dplyr::left_join(
      municipios_ibge |>
        dplyr::transmute(
          cd_mun = as.integer(`Código Município Completo`),
          nm_mun = `Nome_Município`,
          sigla_uf = `UF`
        ),
      by = dplyr::join_by(municipio == cd_mun)
    )

  dados_api <- geocodifica_enderecos(dados_api)

  dados_final_geocodificado <- dados_api |>
    dplyr::select(
      "Cód. Unidade Superior" = codigoUnidadePai,
      codigoUnidade,
      "Competência" = competencia,
      "Logradouro" = logradouro,
      "Número" = numero,
      "Complemento" = complemento,
      "CEP" = cep,
      "Bairro" = bairro,
      "Município" = nm_mun,
      "UF" = uf,
      "Horário de Funcionamento" = horarioDeFuncionamento,
      geometry
    )

  # UNIÃO COM O RELATÓRIO DINÂMICO INFORMADO ----

  dados_join <- dados |>
    dplyr::mutate(codigoUnidade = as.character(`Código Unidade`)) |>
    dplyr::left_join(niveis, by = "codigoUnidade")

  dados_final <- dados_final_geocodificado |>
    dplyr::left_join(dados_join, by = "codigoUnidade") |>
    dplyr::select(-codigoUnidade)

  return(dados_final)
}

#' Relatório Dinâmico de Estrutura da Funai
#'
#' @description Extrai o relatório dinâmico completo das unidades organizacionais da Funai
#' diretamente da API do SIORG e o enriquece via \code{\link{prepara_dados_estrutura}}
#' (classificação hierárquica, dados territoriais e geocodificação). O \code{payload} segue
#' estritamente a validação do backend do SIORG Cidadão para evitar erros de requisição
#' (HTTP 400).
#'
#' @return Um \code{data.frame} contendo as informações tabulares do relatório dinâmico,
#' já combinadas com a classificação hierárquica e a geocodificação das unidades.
#'
#' @importFrom httr content
#' @export
funai_relatorio_estrutura <- function() {

  # A lista de campos deve ser estritamente mapeada aos parâmetros do backend
  campos <- list(
    nivelHierarquico = TRUE,
    tipoUnidade = TRUE,
    denominacaoUnidade = TRUE,
    codigoUnidade = TRUE,
    siglaUnidade = TRUE,
    categoriaUnidade = TRUE,
    orgaoEntidade = TRUE,
    competencia = FALSE,
    finalidade = FALSE,
    esfera = TRUE,
    poder = TRUE,
    naturezaJuridica = TRUE,
    subnaturezaJuridica = TRUE,
    missao = FALSE,
    objetivoEstrategico = FALSE,
    endereco = FALSE,
    cep = FALSE,
    municipio = FALSE,
    uf = FALSE,
    pais = FALSE,
    horarioFuncionamento = FALSE,
    telefone = FALSE,
    email = FALSE,
    site = FALSE,
    areaAtuacao = FALSE,
    tipoCargo = TRUE,
    denominacaoCargo = TRUE,
    complementoDenominacaoCargo = TRUE,
    categoriaCargo = TRUE,
    nivelCargo = TRUE,
    quantidade = TRUE,
    autoridadeCargo = TRUE,
    dataLimiteCargo = TRUE,
    temporarioCargo = TRUE,
    mobilidade = TRUE,
    permiteCargosValor = TRUE,
    ordenacao = "HIERARQUICO"
  )

  payload <- list(
    filtro = list(
      origemRelatorio = "VIVA",
      dataReferenciaHistorico = NULL,
      idProposta = NULL,
      idUnidade = 165,
      tiposUnidade = list(),
      denominacaoUnidade = NULL,
      siglaUnidade = NULL,
      idsCategoriaUnidade = list(),
      competencia = NULL,
      finalidade = NULL,
      idsEsfera = list(),
      idsPoder = list(),
      idsNaturezaJuridica = list(),
      idsSubnaturezaJuridica = list(),
      permiteCargosValor = NULL,
      regulamentoEspecifico = NULL,
      missao = NULL,
      objetivoEstrategico = NULL,
      areaAtuacao = NULL,
      enderecos = list(),
      cargos = list(),
      cargoTemp = NULL,
      dataInicioCargoTemp = NULL,
      dataFimCargoTemp = NULL,
      ehAutoridadeCargo = NULL,
      mobilidades = list(),
      idsDenominacaoCargo = list()
    ),
    campos = campos,
    agrupamentos = NULL,
    formatoSaida = "CSV"
  )

  res <- siorg_post("/relatorio-dinamico/gerar", payload)

  txt <- httr::content(res, "text", encoding = "UTF-8")

  dados <- read.csv(
    text = txt,
    sep = ",",
    quote = "\"",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  dados <- prepara_dados_estrutura(dados)

  return(dados)
}
