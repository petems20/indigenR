# Validação do roteamento multimodal contra a REGIC 2018 (IBGE, rotas 2021).
#
# A REGIC traz ~71 mil rotas entre municípios com modal (Rodoviário, Hidroviário,
# Hidro-Rodoviário, Aéreo), km e minutos. Os tempos hidroviários da REGIC são modelados a
# 20 km/h fixos (sem correnteza) — então esta validação mede distância fluvial, conexão da
# rede e escolha de modal, não velocidade de embarcação. Os tempos rodoviários vêm de uma
# rede viária com velocidade por tipo de via, e servem para comparar com o OSRM.
#
# Não faz parte do pacote instalado (^dev$ está no .Rbuildignore). Rode interativamente:
#   source("dev/validacao-regic.R")
# Parâmetros no topo; resultados vão para `dir_saida`.

suppressPackageStartupMessages({
  library(sf)
  library(sfnetworks)
})

devtools::load_all(quiet = TRUE)

# ---------------------------------------------------------------------------
# Parâmetros
# ---------------------------------------------------------------------------
dir_saida <- file.path(tools::R_user_dir("indigenR", "cache"), "validacao-regic")
url_regic <- paste0(
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/",
  "regioes_de_influencia_das_cidades/Regioes_de_influencia_das_cidades_2018_Resultados_definitivos/",
  "base_vetorial/REGIC2018_Rotas2021.zip"
)
ufs_amazonia <- c("AM", "PA", "AC", "RR", "AP", "RO")
n_por_modal <- c(Rodoviário = 30, Hidroviário = 30)
km_max_hidro <- 300          # pares fluviais mais longos custam muitos tiles do Overpass
semente <- 2026
# O overpass-api.de principal tem derrubado conexões de ambientes em nuvem; o espelho abaixo
# responde (lento, com 504 ocasionais — a retentativa de busca_hidrovias_osm() cobre).
overpass_url <- "https://maps.mail.ru/osm/tools/overpass/api/interpreter"

dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)
osmdata::set_overpass_url(overpass_url)

# ---------------------------------------------------------------------------
# 1. Rotas da REGIC na Amazônia, com coordenadas de origem/destino
# ---------------------------------------------------------------------------
# O shapefile tem 1,6 GB de geometria (o traçado de cada rota); só lemos as linhas da região
# e guardamos o primeiro/último vértice como origem/destino (sedes municipais).
arq_rds <- file.path(dir_saida, "regic_amazonia.rds")

if (!file.exists(arq_rds)) {
  zip <- file.path(dir_saida, "REGIC2018_Rotas2021.zip")
  if (!file.exists(zip)) {
    message("Baixando a REGIC (~750 MB)...")
    httr::GET(url_regic, httr::write_disk(zip, overwrite = TRUE), httr::timeout(1800), httr::progress())
  }
  utils::unzip(zip, exdir = dir_saida)
  ufs_sql <- paste0("'", ufs_amazonia, "'", collapse = ",")
  rotas <- sf::st_read(
    file.path(dir_saida, "REGIC2018_Rotas2021.shp"),
    query = paste0(
      "SELECT Cod_O, Nome_O, Cod_UF_O, Cod_D, Nome_D, Cod_UF_D, Modal, km, minutos, kmh ",
      "FROM REGIC2018_Rotas2021 WHERE Cod_UF_O IN (", ufs_sql, ") AND Cod_UF_D IN (", ufs_sql, ")"
    ),
    quiet = TRUE
  )
  extremo <- function(g, ultimo) {
    m <- sf::st_coordinates(g)
    m[if (ultimo) nrow(m) else 1, 1:2]
  }
  ini <- t(vapply(sf::st_geometry(rotas), extremo, numeric(2), ultimo = FALSE))
  fim <- t(vapply(sf::st_geometry(rotas), extremo, numeric(2), ultimo = TRUE))
  rotas <- sf::st_drop_geometry(rotas)
  rotas$lon_o <- ini[, 1]; rotas$lat_o <- ini[, 2]
  rotas$lon_d <- fim[, 1]; rotas$lat_d <- fim[, 2]
  saveRDS(rotas, arq_rds)
}

rotas <- readRDS(arq_rds)

# ---------------------------------------------------------------------------
# 2. Amostra por modal
# ---------------------------------------------------------------------------
set.seed(semente)
amostra <- do.call(rbind, lapply(names(n_por_modal), function(modal) {
  x <- rotas[rotas$Modal == modal, ]
  if (modal == "Hidroviário") x <- x[x$km <= km_max_hidro, ]
  x[sample(nrow(x), min(n_por_modal[[modal]], nrow(x))), ]
}))
message("Amostra: ", paste(names(table(amostra$Modal)), table(amostra$Modal), collapse = ", "))

# ---------------------------------------------------------------------------
# 3. Roteamento em lote
# ---------------------------------------------------------------------------
t0 <- Sys.time()
lote <- rotear_multimodal_lote(
  origens = as.matrix(amostra[, c("lon_o", "lat_o")]),
  destinos = as.matrix(amostra[, c("lon_d", "lat_d")]),
  verbose = TRUE
)
duracao_min <- as.numeric(Sys.time() - t0, units = "mins")

resultado <- cbind(amostra, lote$resumo[, -1])
utils::write.csv(resultado, file.path(dir_saida, "validacao_regic.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Resumo
# ---------------------------------------------------------------------------
cat(sprintf("\nRoteamento de %d pares em %.1f min\n", nrow(resultado), duracao_min))

modal_regic <- ifelse(resultado$Modal == "Hidroviário", "hidrovia", "rodovia")
cat("\nModal previsto x modal REGIC:\n")
print(table(REGIC = resultado$Modal, previsto = ifelse(is.na(resultado$modal), "sem rota", resultado$modal)))
cat(sprintf("Acerto de modal: %.0f%%\n", 100 * mean(modal_regic == resultado$modal, na.rm = TRUE)))

quantis <- function(x) round(stats::quantile(x, c(.1, .25, .5, .75, .9), na.rm = TRUE), 2)

h <- resultado[resultado$Modal == "Hidroviário", ]
cat("\nHIDROVIÁRIO\n")
cat(sprintf("  com candidata hidrovia viável: %d de %d\n", sum(!is.na(h$distancia_hidrovia_km)), nrow(h)))
cat("  razão km fluvial (rede OSM, sem caminhadas) / km REGIC:\n")
km_fluvial <- h$distancia_hidrovia_km - h$caminhada_acesso_km - h$caminhada_desembarque_km
print(quantis(km_fluvial / h$km))
cat("  motivos de descarte da hidrovia:\n")
print(table(sub(" \\(.*", "", h$motivo_hidrovia[!is.na(h$motivo_hidrovia)])))

r <- resultado[resultado$Modal == "Rodoviário", ]
cat("\nRODOVIÁRIO\n")
cat(sprintf("  com candidata rodovia viável: %d de %d\n", sum(!is.na(r$tempo_rodovia_h)), nrow(r)))
cat("  razão km OSRM / km REGIC:\n"); print(quantis(r$distancia_rodovia_km / r$km))
cat("  razão tempo OSRM / tempo REGIC:\n"); print(quantis(r$tempo_rodovia_h * 60 / r$minutos))

message("\nResultados por par: ", file.path(dir_saida, "validacao_regic.csv"))
