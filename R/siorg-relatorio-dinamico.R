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

#' Catálogo de referência dos níveis de cargos comissionados do SIORG (CCE, FCE, ...)
#'
#' @description Consulta o catálogo de níveis de cargos comissionados do SIORG Cidadão
#' (endpoint `cargos-comissionados/cargos`) — uma referência genérica de valores em R$ e
#' pontos por nível, sem vínculo a nenhum órgão específico (o endpoint não recebe
#' parâmetro de órgão). Usado para enriquecer os cargos retornados pelo relatório
#' dinâmico (ver \code{\link{prepara_dados_estrutura}}) com valor e pontuação por
#' unidade, sem precisar de uma segunda cópia dessa lógica em cada projeto consumidor.
#'
#' @param siglas Vetor de siglas de cargo comissionado a consultar. Padrão:
#'   \code{c("CCE", "FCE")}.
#'
#' @return Tibble com uma linha por nível de cargo: \code{sigla}, \code{categoria}
#'   (character), \code{nivel} (character, 2 dígitos, zero-padded), \code{cargo_label}
#'   (ex: \code{"FCE 1.05"}), \code{valor} (R$ do nível) e \code{cceUnitario}
#'   (pontos/unidades do nível — ex: FCE categoria 1 nível 09 tem \code{cceUnitario =
#'   1.0} exatamente, o ponto de referência da escala).
#'
#' @export
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate
siorg_catalogo_cargos_comissionados <- function(siglas = c("CCE", "FCE")) {

  buscar <- function(sigla) {
    url <- paste0(
      "https://siorg.gov.br/siorg-cidadao-webapp/api/cargos-comissionados/cargos?sigla=",
      sigla
    )
    res <- httr::GET(url)
    httr::stop_for_status(res)
    jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  }

  dplyr::bind_rows(lapply(siglas, buscar)) |>
    dplyr::mutate(
      categoria = trimws(as.character(categoria)),
      nivel = formatC(as.integer(nivel), width = 2, flag = "0"),
      cargo_label = paste0(sigla, " ", categoria, ".", nivel)
    )
}

#' Estrutura organizacional da Funai, com classificação hierárquica e geocodificação
#'
#' Consulta a estrutura organizacional completa da Funai via
#' \code{\link{siorg_estrutura_completa}}, classifica cada unidade em \code{nivel_1}
#' (diretoria) e \code{nivel_2} (coordenação), junta a divisão territorial do IBGE (para
#' obter nome do município e UF a partir do código de município) e geocodifica os
#' endereços das unidades via \code{\link{geocodifica_enderecos}}.
#'
#' @param dados Data.frame originado do relatório dinâmico do SIORG (precisa ter as colunas
#'   \code{Código Unidade}, \code{Tipo do Cargo}, \code{Categoria} e \code{Nível}) —
#'   normalmente o retorno de \code{\link{funai_relatorio_estrutura}}.
#'
#' @return Um \code{data.frame} combinando o relatório dinâmico informado com a
#' classificação hierárquica, dados territoriais do IBGE, coordenadas geográficas e,
#' para cada linha de cargo comissionado (CCE/FCE), o valor unitário em R$ e a
#' pontuação unitária do nível (\code{Valor Unitário Cargo (R$)}, \code{Pontos
#' Unitário Cargo} — via \code{\link{siorg_catalogo_cargos_comissionados}}; \code{NA}
#' nas linhas que não são de cargo comissionado).
#'
#' @importFrom httr GET http_error status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate across starts_with distinct rename transmute filter left_join
#'   arrange select join_by if_else
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

  # PONTOS/VALOR UNITÁRIO DOS CARGOS COMISSIONADOS (CCE/FCE) ----
  # Catálogo agnóstico de órgão (`siorg_catalogo_cargos_comissionados()`, mesmo
  # endpoint sem parâmetro de órgão) — junta valor em R$ e pontos por nível direto nas
  # linhas de cargo do relatório, para que qualquer projeto consumidor (ex. cálculo de
  # pontuação de CCE/FCE por unidade) não precise buscar e casar esse catálogo de novo.
  # Linhas sem cargo (Tipo do Cargo/Categoria/Nível vazios) ficam com NA nas duas
  # colunas novas, sem afetar o restante do relatório.
  catalogo_cargos <- siorg_catalogo_cargos_comissionados()

  dados_join <- dados_join |>
    dplyr::mutate(
      .sigla_cargo = trimws(`Tipo do Cargo`),
      .categoria_cargo = trimws(as.character(Categoria)),
      .nivel_int = suppressWarnings(as.integer(`Nível`)),
      .nivel_cargo = dplyr::if_else(
        is.na(.nivel_int), NA_character_, formatC(.nivel_int, width = 2, flag = "0")
      )
    ) |>
    dplyr::left_join(
      catalogo_cargos |>
        dplyr::transmute(
          .sigla_cargo = sigla,
          .categoria_cargo = categoria,
          .nivel_cargo = nivel,
          `Valor Unitário Cargo (R$)` = valor,
          `Pontos Unitário Cargo` = cceUnitario
        ),
      by = c(".sigla_cargo", ".categoria_cargo", ".nivel_cargo")
    ) |>
    dplyr::select(-.sigla_cargo, -.categoria_cargo, -.nivel_int, -.nivel_cargo)

  dados_final <- dados_final_geocodificado |>
    dplyr::left_join(dados_join, by = "codigoUnidade") |>
    dplyr::select(-codigoUnidade)

  return(dados_final)
}

#' Relatório Dinâmico de Estrutura da Funai
#'
#' @description Extrai o relatório dinâmico completo das unidades organizacionais da Funai
#' diretamente da API do SIORG e o enriquece via \code{\link{prepara_dados_estrutura}}
#' (classificação hierárquica, dados territoriais, geocodificação e valor/pontuação dos
#' cargos comissionados CCE/FCE). O \code{payload} segue estritamente a validação do
#' backend do SIORG Cidadão para evitar erros de requisição (HTTP 400).
#'
#' @return Um \code{data.frame} contendo as informações tabulares do relatório dinâmico,
#' já combinadas com a classificação hierárquica, a geocodificação das unidades e, para
#' cada linha de cargo comissionado, valor em R$ e pontuação unitária (ver
#' \code{\link{prepara_dados_estrutura}}).
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
