# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`indigenR` is an R package with utilities for consuming Brazilian federal government
"sistemas estruturantes" — procurement/price-research APIs (Compras.gov, PNCP), the
organizational-structure API (SIORG), and geocoding of unit addresses. It supports
statistical price research under Brazilian procurement law (Leis 8.666 and 14.133) and
maps an org chart as a graph. It was originally built for Funai (Fundação Nacional dos
Povos Indígenas) and still ships Funai-specific convenience wrappers (`funai_*()`), but
the underlying engine (`siorg_estrutura_completa()`, `puxa_api_pncp_completo()`,
`puxa_api_compras_completo()`, the internal hierarchy-classification helper) is generic —
it works for any federal agency registered in these systems. Prefer keeping new
functionality agency-agnostic (parameters, not hardcoded agency codes) unless it's
explicitly a Funai-specific convenience wrapper.

## Commands

Standard R package using `renv` for dependency isolation and `roxygen2` for documentation.
There is no test suite (no `tests/` directory) and no lint config — use the R session directly.

```r
# Restore dependencies from renv.lock (first time / after pulling changes)
renv::restore()

# Load the package for interactive development (equivalent of library(indigenR) but from source)
devtools::load_all()

# Regenerate NAMESPACE and man/*.Rd from roxygen2 comments after editing R/ files
devtools::document()

# Full package check (also picks up missing/undeclared imports)
devtools::check()

# Install the package into the local library
devtools::install()
```

Roxygen version pinned in `DESCRIPTION` is 7.3.3 — run `devtools::document()` after any change to a
function's roxygen block, not just when adding new exported functions, since `NAMESPACE` and `man/`
are generated artifacts that must stay in sync with the source.

Note: the project's `renv` library (`renv/renv/library/...`) has been seen with corrupted/unreadable
package `.rds` files (likely a OneDrive sync artifact, since this repo lives in a OneDrive-synced
folder) — if `devtools`/`roxygen2` fail to load, try `renv::restore()` first before assuming a real
package problem.

## Architecture

### Three API integration stacks, same shape

Each external government API has its own **low-level single-request function** (retry/backoff logic,
internal, `@noRd`) and a **paginating "completo" wrapper** (exported) that drives it across pages
and optionally iterates over a vectorized parameter:

- Compras.gov: `puxa_api_compras()` → `puxa_api_compras_completo()` (both in `R/api-compras.R`).
  `R/api-compras-pgc.R` (`consultarPgcDetalhe()`) reuses the same low-level helper for the PGC module.
  Takes `url_base` + a named `parametros` list; query string is built with `httr::parse_url()`/`build_url()`.
- PNCP: `puxa_api_pncp()` → `puxa_api_pncp_completo()` (`R/api-pncp.R`). `url_base` supports
  `glue`-style `{placeholder}` path interpolation from `parametros` before the remaining params
  become the query string.
- Contratos: `puxa_api_contratos()` (`R/api-contratos.R`), same retry pattern, simpler (no exported
  pagination wrapper yet).

**Both `*_completo()` wrappers cap vectorized parameters at one per call** — if more than one list
element in `parametros` has length > 1, they `stop()` immediately. When one parameter is a vector, the
wrapper recurses once per value and `dplyr::bind_rows()`s the results instead of looping internally.

**Resilience contract**: every `httr::GET`/`POST` call site wraps in retry logic, uses a request
timeout, and on HTTP 429 sleeps with exponential backoff (`2^(tentativa-1)` seconds) before retrying.
Preserve this pattern in any new API function. The three low-level retry functions are intentionally
*not* unified into one shared helper — they differ enough in connection-error handling and return
shape that merging them would require touching all three `_completo()`/caller sites; if you do
consolidate them later, keep each one's specific behavior (e.g. Compras' 429 handler reads the
suggested wait time from the response body).

### Org-chart graph (SIORG) — `R/siorg-api.R`, `R/siorg-hierarquia.R`, `R/siorg-relatorio-dinamico.R`

`siorg_estrutura_completa(codigo)` (`R/siorg-api.R`) is the generic, agency-agnostic fetch: given any
SIORG unit code, returns that unit and its full descendancy as a flat tibble.

`.siorg_classifica_hierarquia(dados_api, raizes)` (`R/siorg-hierarquia.R`, internal) is the single
shared implementation of the org-chart graph logic: builds an `igraph` directed graph from
`codigoUnidadePai -> codigoUnidade` edges, then uses `igraph::subcomponent(..., mode = "out")` from a
`raizes` table (SIORG code -> label) to compute full descendancy and tag every unit with `nivel_1`
(e.g. directorate) and `nivel_2` (e.g. coordination, derived from units whose parent is one of the
`raizes`). `raizes` defaults to Funai's roots (`.siorg_raizes_funai()`) but accepts any agency's own
root table — this is the extension point for supporting another agency's hierarchy. Previously this
graph-building logic was duplicated three times (in `funai_estrutura_completa()`,
`prepara_dados_estrutura()`, and a loose untracked root script); it now lives in one place.

`funai_estrutura_completa()` (`R/siorg-hierarquia.R`) is a thin Funai-specific wrapper around
`siorg_estrutura_completa(173)`. For backward compatibility it does **not** attach the `nivel_1`/`nivel_2`
columns to its return value (this mirrors a pre-existing quirk in the function, not a design choice —
see the code comment). Use `prepara_dados_estrutura()` (`R/siorg-relatorio-dinamico.R`) for the current,
complete pipeline: hierarchy classification + IBGE municipality join + geocoding via
`geocodifica_enderecos()` (`R/geocodifica.R`, built on `geocodebr`).

`siorg_post()` (internal, `R/siorg-relatorio-dinamico.R`) POSTs a hand-built JSON payload to the
authenticated-looking `siorg.gov.br/siorg-cidadao-webapp` "relatório dinâmico" endpoint with spoofed
browser headers — the `payload$campos`/`payload$filtro` shape must match the backend's validation
exactly or it 400s. `funai_relatorio_estrutura()` drives it with Funai's own filter (`idUnidade = 165`).

### Price research (Pesquisa de Preços) pipeline

Flow: `consultar_precos()` (`R/precos-consultar.R`, paginated Compras.gov price history for one
CATMAT/service code) → unit-of-supply standardization → `sanear_precos()` (`R/precos-sanear.R`, IQR
outlier detection per `unidadeFornecimento` group, can filter or just mark with `Status`/`Justificativa`)
→ `.calcula_preco_referencia()` (internal, `R/precos-sanear.R`, the mean-vs-median-by-CV rule, shared by
every function below) → `prepara_arquivo()` (`R/precos-arquivo.R`, writes a formatted Excel workbook
with live formulas, sheets "Resumo"/"Inciso I"/"Inciso III").

`R/precos-analise.R` has the two higher-level analysis entry points:
- `analisar_item_material()`: single predominant unit of supply, simpler contract.
- `analisar_item_catmat_completo()`: every unit of supply with >= 3 records, handles services,
  bidding-modality preference, and automatic period narrowing.

`R/precos-relatorios.R` has batch/reporting helpers built on top: `pesquisa_mista()`,
`buscar_dados_catmat()`, `cria_resumo_pesquisa()`, `regressao_estado()`, `regressao_regiao()`.

**Known inconsistency, left as-is**: `analisar_item_material()`'s inline unit-of-supply formula
(`nomeUnidadeFornecimento + capacidade + siglaUnidadeMedida`) differs from `padroniza_unidadeF()`'s and
`analisar_item_catmat_completo()`'s formula (which special-cases `"QUILOGRAMA" -> "KG"` and treats
capacity `0` as `1`). Unifying them would silently change the supply-unit grouping — and therefore the
sanitized sample and reference price — of any research already run through `analisar_item_material()`.
If you need to fix this, do it as a deliberate, flagged change, not incidentally.

Business rules:
- Outlier removal: strictly IQR method (`Q1/Q3 ± 1.5*IQR`), implemented once in `sanear_precos()`.
- Reference price: **mean** if coefficient of variation (CV = sd/mean) `<= 25%`, else **median** —
  implemented once in `.calcula_preco_referencia()` (`R/precos-sanear.R`).
- Period: use the smallest time window (3/6/9/12 months) that yields at least 3 valid prices
  (`analisar_item_catmat_completo()`'s `ajuste_periodo` argument).

## Conventions

- Use `tidyverse` style (`dplyr`, `tidyr`, `purrr`, `stringr`) for data manipulation.
- `snake_case` for functions and variables — except `consultarPgcDetalhe()`, which is camelCase and
  kept that way because it's part of the public API (renaming would break callers).
- Always qualify non-base functions with `package::function()` (e.g. `dplyr::filter`, `httr::GET`).
  `DESCRIPTION` Imports and every `pkg::` reference in `R/` are kept in exact sync — if you add a new
  `pkg::fn()` call, add `pkg` to `DESCRIPTION` Imports too (or it'll fail `devtools::check()`).
- Every exported function needs full roxygen2 docs (`@title`/`@param`/`@return`/`@export`). Internal
  helpers (not exported) use `@keywords internal` + `@noRd` so they don't generate a `man/*.Rd` page —
  keep this pattern for new internal helpers rather than letting them accumulate undocumented or,
  conversely, cluttering `man/` with non-public topics.
- Keep pagination/retry/looping logic isolated in small helpers rather than inlined in business logic
  (the `puxa_api_*` / `puxa_api_*_completo` split is the existing template to follow).
- File naming in `R/` is `<domain>-<role>.R` (e.g. `precos-sanear.R`, `siorg-hierarquia.R`) rather than
  one file per function — R packages can't use subdirectories inside `R/` (`R CMD INSTALL` won't source
  them), so this prefix convention is the way file organization is expressed here.

## Repo layout notes

- `renv/`, `renv.lock`, and `*.Rproj` are excluded from the built package via `.Rbuildignore`; `logs/`
  and `AVN/` are excluded via both `.gitignore` and `.Rbuildignore` (local/output directories, not
  package source — `atualizar_repositorio()`, the only function that wrote to `logs/`, was removed as
  dead code, so that directory shouldn't reappear except as ad hoc local output).
