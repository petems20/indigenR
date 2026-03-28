#' Atualiza repositório local de dados PNCP e SIORG
#'
#' Faz download de dados das APIs do PNCP (PCA, Contratações, Contratos)
#' e do SIORG, consolida e salva localmente, mantendo log de atualização.
#'
#' @param anos vetor de anos para atualização PNCP
#' @param codigos_uorg vetor de códigos SIORG a consultar
#' @param cnpj string. CNPJ padrão a ser usado nas consultas
#' @param caminho_repositorio string. Caminho para salvar os dados
#' @param delay_segundos numérico. Pausa entre requisições
#' @param formato "rds" ou "csv"
#' @param log_dir string. Pasta para salvar logs
#'
#' @return Lista com dados atualizados e resumo de logs
#' @export
atualizar_repositorio <- function(
    anos,
    codigos_uorg,
    cnpj = "00059311000126",
    caminho_repositorio = "dados",
    delay_segundos = 0.1,
    formato = c("rds", "csv"),
    log_dir = "logs"
){
  formato <- match.arg(formato)

  pacotes <- c("httr", "jsonlite", "dplyr", "tidyr", "purrr", "tibble", "stringr", "lubridate")
  faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
  if(length(faltando)) install.packages(faltando)

  dir.create(caminho_repositorio, recursive = TRUE, showWarnings = FALSE)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  log_file <- file.path(log_dir, paste0("log_atualizacao_", Sys.Date(), ".csv"))
  logs <- tibble::tibble(
    data = Sys.time(),
    fonte = character(),
    status = character(),
    mensagem = character()
  )

  # --- 1. PNCP PCA ---
  pca <- tryCatch({
    resultado <- fxObterPcaAnoCompleto(anos = anos, cnpj = cnpj, delay_segundos = delay_segundos)
    logs <- logs |> add_row(data = Sys.time(), fonte = "PNCP PCA", status = "Sucesso",
                            mensagem = paste0(nrow(resultado), " linhas"))
    resultado
  }, error = function(e){
    logs <<- logs |> add_row(data = Sys.time(), fonte = "PNCP PCA", status = "Falha", mensagem = e$message)
    NULL
  })

  # --- 2. PNCP Contratações ---
  contratacoes <- tryCatch({
    resultado <- fxObterContratacoesPNCP(anos = anos, cnpj = cnpj, delay_segundos = delay_segundos)
    logs <- logs |> add_row(data = Sys.time(), fonte = "PNCP Contratações", status = "Sucesso",
                            mensagem = paste0(nrow(resultado), " linhas"))
    resultado
  }, error = function(e){
    logs <<- logs |> add_row(data = Sys.time(), fonte = "PNCP Contratações", status = "Falha", mensagem = e$message)
    NULL
  })

  # --- 3. PNCP Contratos ---
  contratos <- tryCatch({
    resultado <- fxObterContratosPNCP(anos = anos, cnpj = cnpj, delay_segundos = delay_segundos)
    logs <- logs |> add_row(data = Sys.time(), fonte = "PNCP Contratos", status = "Sucesso",
                            mensagem = paste0(nrow(resultado), " linhas"))
    resultado
  }, error = function(e){
    logs <<- logs |> add_row(data = Sys.time(), fonte = "PNCP Contratos", status = "Falha", mensagem = e$message)
    NULL
  })

  # --- 4. SIORG ---
  uorgs <- tryCatch({
    resultado <- map_dfr(codigos_uorg, consulta_uorgs_completo)
    logs <- logs |> add_row(data = Sys.time(), fonte = "SIORG", status = "Sucesso",
                            mensagem = paste0(nrow(resultado), " linhas"))
    resultado
  }, error = function(e){
    logs <<- logs |> add_row(data = Sys.time(), fonte = "SIORG", status = "Falha", mensagem = e$message)
    NULL
  })

  # --- Salva arquivos ---
  arquivos <- list(
    pca = file.path(caminho_repositorio, paste0("pca.", formato)),
    contratacoes = file.path(caminho_repositorio, paste0("contratacoes.", formato)),
    contratos = file.path(caminho_repositorio, paste0("contratos.", formato)),
    uorgs = file.path(caminho_repositorio, paste0("uorgs.", formato))
  )

  salvar <- function(dados, arquivo){
    if(is.null(dados)) return()
    if(formato == "rds") saveRDS(dados, arquivo) else readr::write_csv(dados, arquivo)
  }

  salvar(pca, arquivos$pca)
  salvar(contratacoes, arquivos$contratacoes)
  salvar(contratos, arquivos$contratos)
  salvar(uorgs, arquivos$uorgs)

  # Salva log
  if(file.exists(log_file)){
    log_ant <- read.csv(log_file)
    logs <- rbind(log_ant, logs)
  }
  write.csv(logs, log_file, row.names = FALSE)

  return(list(pca = pca, contratacoes = contratacoes, contratos = contratos, uorgs = uorgs, logs = logs))
}
