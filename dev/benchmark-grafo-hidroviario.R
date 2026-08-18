# Benchmark de backend para o grafo hidroviario de rotear_multimodal().
#
# Decide, com numeros medidos (nao suposicao), se vale a pena usar sfnetworks+tidygraph
# (a escolha ja feita em R/roteamento-grafo.R) ou reimplementar manualmente com sf+igraph
# puro (o padrao ja usado em .siorg_classifica_hierarquia(), sem essas 2 dependencias a mais).
# Compara tambem sf::st_nearest_feature() contra a extensao spatial do DuckDB para snapping
# em lote — ver observacao sobre a extensao spatial no final do arquivo.
#
# Nao faz parte do pacote instalado (^dev$ esta no .Rbuildignore). Rode interativamente:
#   source("dev/benchmark-grafo-hidroviario.R")

suppressPackageStartupMessages({
  library(sf)
  library(sfnetworks)
  library(tidygraph)
  library(igraph)
})

devtools::load_all(quiet = TRUE)

# bench::mark() trava (bug de despacho `[[<-.sf` dentro do tibble interno do bench, nesta
# combinacao de versoes de bench/sf/sfnetworks) quando a expressao medida toca objetos sf
# dentro de uma sessao com sfnetworks carregado — em vez de perseguir o bug de uma dependencia
# dev-only, medimos tempo/memoria manualmente com gc()+system.time(), que e robusto.
medir <- function(f, repeticoes = 5) {
  tempos <- numeric(repeticoes)
  mem_antes <- gc(reset = TRUE, full = TRUE)
  for (i in seq_len(repeticoes)) {
    t0 <- Sys.time()
    f()
    tempos[i] <- as.numeric(Sys.time() - t0, units = "secs")
  }
  mem_depois <- gc(full = TRUE)
  list(
    min_s = min(tempos),
    mediana_s = stats::median(tempos),
    mem_alocada_mb = sum(mem_depois[, 2]) - sum(mem_antes[, 2])
  )
}

# ---------------------------------------------------------------------------
# 1. Rede hidroviaria sintetica, em escala realista (bacia de porte medio)
# ---------------------------------------------------------------------------
# Arvore fractal simples imitando uma bacia: 1 rio principal + afluentes recursivos.
# Gera ~n_segmentos LINESTRINGs num CRS metrico (5880), cobrindo ~200km x 200km.

gera_bacia_sintetica <- function(n_niveis = 8, seed = 42) {

  set.seed(seed)

  segmentos <- vector("list", 0)

  cresce <- function(x0, y0, ang, comprimento, nivel) {

    if (nivel > n_niveis || comprimento < 500) return(invisible())

    x1 <- x0 + comprimento * cos(ang)
    y1 <- y0 + comprimento * sin(ang)

    segmentos[[length(segmentos) + 1]] <<- st_linestring(rbind(c(x0, y0), c(x1, y1)))

    n_filhos <- if (nivel <= 2) 2 else sample(1:2, 1)

    for (i in seq_len(n_filhos)) {
      novo_ang <- ang + runif(1, -0.9, 0.9)
      cresce(x1, y1, novo_ang, comprimento * runif(1, 0.55, 0.75), nivel + 1)
    }
  }

  cresce(0, 0, pi / 2, 40000, 1)

  st_sf(
    osm_id = as.character(seq_along(segmentos)),
    waterway = "river",
    geometry = st_sfc(segmentos, crs = 5880)
  )
}

bacia <- gera_bacia_sintetica()
cat("Segmentos gerados:", nrow(bacia), "\n")

set.seed(7)
pontos_teste <- st_sf(
  id = seq_len(200),
  geometry = st_sfc(
    lapply(seq_len(200), function(i) st_point(c(runif(1, -20000, 20000), runif(1, 0, 300000)))),
    crs = 5880
  )
)

# ---------------------------------------------------------------------------
# 2a. Backend sfnetworks + tidygraph (o escolhido em R/roteamento-grafo.R)
# ---------------------------------------------------------------------------
rodada_sfnetworks <- function() {
  net <- .constroi_rede_hidroviaria(bacia, crs_metrico = 5880)
  blend <- .insere_pontos_rede(net, pontos_teste[1:2, ], tolerance_m = 50000)
  resultado <- .rota_hidroviaria(blend$net, blend$idx_nos[1], blend$idx_nos[2])
  # bench::mark() guarda o valor de retorno num list-column; devolver objetos sf/sfc
  # aninhados ali dispara um bug de despacho de metodo em [[<-.sf dentro do tibble interno
  # do bench — devolvemos so um escalar, o que basta para medir tempo/memoria.
  invisible(isTRUE(resultado$viavel))
}

# ---------------------------------------------------------------------------
# 2b. Backend manual sf + igraph puro (reimplementando o que st_network_blend faz)
# ---------------------------------------------------------------------------
# Nodagem manual: uniao + st_node (o metodo do rascunho original) + snapping por
# st_nearest_feature/st_nearest_points (tambem do rascunho original) em vez de blend().
rodada_sf_igraph_manual <- function() {

  noded <- bacia |> st_union() |> st_node() |> st_cast("LINESTRING") |> st_sf()

  idx_prox <- st_nearest_feature(pontos_teste[1:2, ], noded)

  linhas_proj <- st_sfc(
    lapply(seq_len(2), function(i) {
      st_nearest_points(st_geometry(pontos_teste[1:2, ])[i], st_geometry(noded)[idx_prox[i]])[[1]]
    }),
    crs = st_crs(noded)
  )

  # nota: montar o grafo ponderado a partir de `noded` exige reconstruir nos/arestas na mao
  # (o que sfnetworks::as_sfnetwork ja faz); para fins de benchmark, medimos so o custo da
  # nodagem + snapping manual (a parte que st_network_blend substitui), que e o gargalo real.
  invisible(length(linhas_proj))
}

cat("\n--- Benchmark: nodagem + snapping (sfnetworks/blend vs sf+igraph manual) ---\n")
m_sfnetworks <- medir(rodada_sfnetworks)
m_sf_manual  <- medir(rodada_sf_igraph_manual)
cat(sprintf(
  "sfnetworks : min %.3fs | mediana %.3fs | mem %.1f MB\n",
  m_sfnetworks$min_s, m_sfnetworks$mediana_s, m_sfnetworks$mem_alocada_mb
))
cat(sprintf(
  "sf_manual  : min %.3fs | mediana %.3fs | mem %.1f MB\n",
  m_sf_manual$min_s, m_sf_manual$mediana_s, m_sf_manual$mem_alocada_mb
))

# ---------------------------------------------------------------------------
# 3. sf::st_nearest_feature em lote vs DuckDB spatial (KNN)
# ---------------------------------------------------------------------------
cat("\n--- Testando disponibilidade da extensao 'spatial' do DuckDB ---\n")

con <- DBI::dbConnect(duckdb::duckdb())
duckdb_spatial_ok <- tryCatch({
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  TRUE
}, error = function(e) {
  message("DuckDB spatial indisponivel neste ambiente: ", conditionMessage(e))
  FALSE
})
DBI::dbDisconnect(con, shutdown = TRUE)

if (duckdb_spatial_ok) {

  noded <- bacia |> st_union() |> st_node() |> st_cast("LINESTRING") |> st_sf()

  m_snap <- medir(function() sf::st_nearest_feature(pontos_teste, noded))
  cat(sprintf(
    "sf::st_nearest_feature: min %.3fs | mediana %.3fs | mem %.1f MB\n",
    m_snap$min_s, m_snap$mediana_s, m_snap$mem_alocada_mb
  ))
  cat("DuckDB spatial disponivel — implementar comparacao KNN antes de adotar como padrao.\n")

} else {
  cat(
    "DuckDB spatial NAO pode ser instalado neste ambiente (extensions.duckdb.org retornou\n",
    "HTTP 403 no teste realizado em 2026-08-18 — provavel bloqueio de rede/proxy corporativo).\n",
    "Decisao: manter sf::st_nearest_feature() como backend de snapping; usar DuckDB apenas\n",
    "como armazenamento do cache (tabelas/BLOB), que nao depende da extensao spatial — ja\n",
    "implementado em R/roteamento-hidrovias.R (.overpass_cache_con()).\n",
    sep = ""
  )
}

# ---------------------------------------------------------------------------
# Decisao registrada (rodado em 2026-08-18, bacia sintetica de 100 segmentos, 5 repeticoes):
#
#   sfnetworks : min 0.201s | mediana 0.202s | mem 6.3 MB
#   sf_manual  : min 0.040s | mediana 0.041s | mem 0.1 MB   (so nodagem+snapping — NAO inclui
#                montar o grafo ponderado igraph::graph_from_data_frame() na mao, que
#                sfnetworks ja devolve pronto; o numero manual esta subestimado)
#
# - Backend do grafo: mesmo com esse overhead medido (~5x mais lento, ~60x mais memoria
#   nesta escala pequena), mantemos sfnetworks + tidygraph. Motivos: (1) o usuario autorizou
#   explicitamente novas dependencias para este modulo; (2) o numero "manual" acima e parcial
#   (falta a construcao do grafo ponderado, que sfnetworks da de graca); (3) em escala real
#   (milhares de segmentos, nao 100) o overhead fixo do sfnetworks tende a diluir frente ao
#   custo de nodagem/roteamento em si — reexecutar este benchmark com uma bacia maior antes
#   de trocar de backend em producao, nao decidir so por este numero pequeno.
# - DuckDB: usado so como armazenamento de cache (BLOB/tabela), nao como motor espacial —
#   a extensao 'spatial' nao instalou neste ambiente (ver acima). Se em outro ambiente ela
#   instalar E o benchmark acima mostrar ganho expressivo (>2x) no snapping em lote,
#   considerar introduzir DuckDB spatial como acelerador opcional (com fallback automatico
#   para sf, igual ja fazemos aqui) — nao como dependencia obrigatoria.
# ---------------------------------------------------------------------------
