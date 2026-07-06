#' Consulta completa de unidades organizacionais (UORGs)
#'
#' Esta função consulta a base de Estrutura Organizacional do Governo Federal e retorna
#' informações detalhadas de uma unidade organizacional (UORG) a partir do seu código.
#'
#' @param codigo Código da unidade organizacional a ser consultada.
#'
#' @return Um \code{tibble} com os dados da UORG, incluindo informações de contato, endereço,
#' atos normativos e classificações. Caso a consulta não retorne resultados, retorna \code{NULL}.
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' \dontrun{
#' consulta_uorgs_completo("12345")
#' }
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate select across everything
#' @importFrom tidyr unnest unnest_wider
siorg_estrutura_completa <- function(codigo) {

  # Lista de pacotes necessários
  pacotes_necessarios <- c("httr", "jsonlite", "dplyr", "tidyr")

  # Instala apenas os que ainda não estão instalados
  pacotes_faltando <- pacotes_necessarios[!pacotes_necessarios %in% rownames(installed.packages())]
  if(length(pacotes_faltando)) {
    install.packages(pacotes_faltando)
  }

  # Construir URL da API
  url <- paste0(
    "https://estruturaorganizacional.dados.gov.br/doc/estrutura-organizacional/completa?",
    "codigoPoder=1&codigoEsfera=1&codigoUnidade=", codigo
  )

  # Requisição HTTP segura
  res <- httr::GET(url)

  # Checar status da requisição
  if (httr::http_error(res)) {
    warning("Falha na requisição para o código ", codigo, ": HTTP ", httr::status_code(res))
    return(NULL)
  }

  # Extrair conteúdo JSON
  json_content <- httr::content(res, as = "text", encoding = "UTF-8")
  dados <- jsonlite::fromJSON(json_content)$unidades

  if (length(dados) == 0) {
    message("Nenhuma unidade encontrada para o código ", codigo)
    return(NULL)
  }

  # Limpeza e padronização dos códigos
  dados <- dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("codigo"),
        ~ sub(".*/", "", .)
      )
    ) |>
    tidyr::unnest(cols = c(contato, endereco))

  # Expansão de listas internas (telefone, email, atoNormativo)
  dados <- dados |>
    dplyr::select(dplyr::everything(), -site) |>
    tidyr::unnest_wider(telefone, names_sep = "_") |>
    tidyr::unnest_wider(email, names_sep = "_") |>
    tidyr::unnest_wider(atoNormativo, names_sep = "_")

  return(dados)
}

#' Consulta completa personalizada de unidades organizacionais (UORGs)
#'
#' Esta função consulta a base de Estrutura Organizacional do Governo Federal da Funai e retorna
#' informações detalhadas de uma unidade organizacional (UORG) a partir do seu código, além de níveis de desagregação específicos.
#'
#'
#' @return Um \code{tibble} com os dados da UORG, incluindo informações de contato, endereço,
#' atos normativos e classificações. Caso a consulta não retorne resultados, retorna \code{NULL}.
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' \dontrun{
#' consulta_uorgs_completo("12345")
#' }
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate select across everything
#' @importFrom tidyr unnest unnest_wider
funai_estrutura_completa <- function() {

  codigo = 173

  # Lista de pacotes necessários
  pacotes_necessarios <- c("httr", "jsonlite", "dplyr", "tidyr", "igraph")

  # Instala apenas os que ainda não estão instalados
  pacotes_faltando <- pacotes_necessarios[!pacotes_necessarios %in% rownames(installed.packages())]
  if(length(pacotes_faltando)) {
    install.packages(pacotes_faltando)
  }

  # Construir URL da API
  url <- paste0(
    "https://estruturaorganizacional.dados.gov.br/doc/estrutura-organizacional/completa?",
    "codigoPoder=1&codigoEsfera=1&codigoUnidade=", codigo
  )

  # Requisição HTTP segura
  res <- httr::GET(url)

  # Checar status da requisição
  if (httr::http_error(res)) {
    warning("Falha na requisição para o código ", codigo, ": HTTP ", httr::status_code(res))
    return(NULL)
  }

  # Extrair conteúdo JSON
  json_content <- httr::content(res, as = "text", encoding = "UTF-8")
  dados <- jsonlite::fromJSON(json_content)$unidades

  if (length(dados) == 0) {
    message("Nenhuma unidade encontrada para o código ", codigo)
    return(NULL)
  }

  # Limpeza e padronização dos códigos
  dados <- dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("codigo"),
        ~ sub(".*/", "", .)
      )
    ) |>
    tidyr::unnest(cols = c(contato, endereco))

  # Expansão de listas internas (telefone, email, atoNormativo)
  dados <- dados |>
    dplyr::select(dplyr::everything(), -site) |>
    tidyr::unnest_wider(telefone, names_sep = "_") |>
    tidyr::unnest_wider(email, names_sep = "_") |>
    tidyr::unnest_wider(atoNormativo, names_sep = "_")

  dados_dedup <- dados |>
    dplyr::distinct(codigoUnidade, .keep_all = TRUE)

  vertices <- dados_dedup |>
    dplyr::rename(name = codigoUnidade)

  edges <- dados_dedup |>
    dplyr::filter(!is.na(codigoUnidadePai)) |>
    dplyr::transmute(
      from = codigoUnidadePai,
      to = codigoUnidade
    ) |>
    dplyr::filter(
      from %in% vertices$name,
      to %in% vertices$name
    )

  g <- igraph::graph_from_data_frame(
    d = edges,
    directed = TRUE,
    vertices = vertices
  )

  raiz <- V(g)[degree(g, mode = "in") == 0]

  diretorias <- neighbors(g, raiz, mode = "out")

  agrupamento <- data.frame(
    codigoUnidade = character(),
    diretoria = character()
  )

  for (d in diretorias) {

    desc <- subcomponent(g, d, mode = "out")

    agrupamento <- rbind(
      agrupamento,
      data.frame(
        codigoUnidade = V(g)[desc]$name,
        diretoria = V(g)[d]$name
      )
    )
  }

  dados_final <- dados |>
    dplyr::left_join(agrupamento, by = "codigoUnidade")

  return(dados)
}
