#' Padroniza N pontos de entrada (`sf`/`sfc` POINT, `matrix`/`data.frame` 2 colunas, ou
#' `c(lon, lat)`) como sf
#' @keywords internal
#' @noRd
.padroniza_pontos <- function(p) {

  if (inherits(p, "sf")) {
    if (is.na(sf::st_crs(p))) sf::st_crs(p) <- 4326
    return(sf::st_sf(geometry = sf::st_geometry(p)))
  }

  if (inherits(p, "sfc")) {
    if (is.na(sf::st_crs(p))) sf::st_crs(p) <- 4326
    return(sf::st_sf(geometry = p))
  }

  if (is.numeric(p) && length(p) == 2) {
    return(sf::st_sf(geometry = sf::st_sfc(sf::st_point(p), crs = 4326)))
  }

  if ((is.matrix(p) || is.data.frame(p)) && ncol(p) == 2) {
    pontos <- lapply(seq_len(nrow(p)), function(i) sf::st_point(as.numeric(p[i, ])))
    return(sf::st_sf(geometry = sf::st_sfc(pontos, crs = 4326)))
  }

  stop(
    "`origem`/`destino` devem ser um objeto sf/sfc POINT, matrix/data.frame com 2 colunas ",
    "(lon, lat), ou c(lon, lat).",
    call. = FALSE
  )
}

#' Padroniza um único ponto de entrada (`sf`/`sfc` POINT ou `c(lon, lat)`) como sf de 1 feature
#' @keywords internal
#' @noRd
.padroniza_ponto <- function(p) {
  out <- .padroniza_pontos(p)
  if (nrow(out) != 1) stop("`origem`/`destino` devem ter exatamente 1 feature.", call. = FALSE)
  out
}

#' Executa uma chamada ao OSRM com retentativa e backoff exponencial
#'
#' @description
#' O pacote \pkg{osrm} não tenta de novo sozinho, e o servidor público de demonstração
#' responde com erro com frequência sob carga. Segue o contrato de resiliência do pacote
#' (backoff `2^(tentativa-1)` segundos), mas não retenta erros do próprio OSRM que não são
#' transitórios (ex. `NoRoute`, `NoSegment`, `TooBig`): repetir a mesma consulta daria o
#' mesmo resultado.
#'
#' @keywords internal
#' @noRd
.osrm_com_retry <- function(chamada, contexto, max_tries, verbose) {
  tentativa <- 1
  repeat {
    resultado <- tryCatch(chamada(), error = function(e) e)

    if (!inherits(resultado, "error")) return(resultado)

    permanente <- grepl(
      "NoRoute|NoSegment|NoTable|NoMatch|TooBig|InvalidQuery|InvalidValue|InvalidOptions|InvalidUrl|InvalidService",
      conditionMessage(resultado)
    )

    if (permanente || tentativa >= max_tries) {
      stop(
        "Falha ao ", contexto, if (!permanente) paste0(" apos ", tentativa, " tentativas"), ".\n",
        conditionMessage(resultado),
        call. = FALSE
      )
    }

    wait_time <- 2 ^ (tentativa - 1)
    if (verbose) {
      message("Erro no OSRM (", conditionMessage(resultado), "). Nova tentativa em ", wait_time, "s...")
    }
    Sys.sleep(wait_time)
    tentativa <- tentativa + 1
  }
}

#' Motivo de descarte da candidata rodovia a partir de uma linha da triagem OSRM Table
#'
#' @description
#' O OSRM sempre encaixa (*snap*) o ponto pedido no nó mais próximo da malha rodoviária que
#' ele conhece, mesmo que esse nó esteja muito longe do ponto real — típico de localidade
#' isolada sem acesso rodoviário. Uma rota "encontrada" nessas condições pode ter uma razão
#' de desvio (distância da rota / distância em linha reta) aparentemente normal e ainda assim
#' não representar o trajeto real: ela simplesmente ignora o trecho entre o ponto pedido e o
#' nó da malha, que pode ser intransponível (ex. atravessa um rio sem ponte). Por isso a
#' distância de snap é checada antes da razão de desvio, e antes de pedir a geometria da rota.
#'
#' @return `NA_character_` se a rota é plausível; senão, o motivo do descarte.
#' @keywords internal
#' @noRd
.motivo_rodovia <- function(tri, max_snap_osrm_km, max_ratio_desvio) {
  if (is.na(tri$tempo_rodovia_h)) {
    return("sem rota rodoviaria no OSRM entre origem e destino")
  }
  if (tri$snap_origem_km > max_snap_osrm_km || tri$snap_destino_km > max_snap_osrm_km) {
    return(paste0(
      "encaixe OSRM muito distante (origem ", round(tri$snap_origem_km, 1), " km, destino ",
      round(tri$snap_destino_km, 1), " km; limite ", max_snap_osrm_km, " km)"
    ))
  }
  if (tri$razao_desvio > max_ratio_desvio) {
    return(paste0(
      "razao de desvio ", round(tri$razao_desvio, 1), "x excede max_ratio_desvio (",
      max_ratio_desvio, "x)"
    ))
  }
  NA_character_
}

#' Candidatas "rodovia" (OSRM) para vários pares origem-destino
#'
#' @description
#' Uma única consulta \code{osrm::osrmTable()} (via \code{\link{triagem_rodovia_lote}},
#' dividida em blocos de `max_pares`) dá tempo, distância e distâncias de encaixe de todos
#' os pares; a crítica de snap/desvio é aplicada a cada par. Só quando `geometria = TRUE`
#' é feita uma chamada \code{osrm::osrmRoute()} por par plausível, para obter o traçado.
#'
#' @return Lista com um elemento por par: `list(candidata, motivo)`. `candidata` é `NULL`
#'   quando descartada (e `motivo` diz por quê).
#' @keywords internal
#' @noRd
.rodovia_pares <- function(
    origens, destinos, osrm_server, osrm_profile,
    max_snap_osrm_km, max_ratio_desvio, max_pares, geometria, max_tries, verbose
) {

  tri <- triagem_rodovia_lote(
    origens, destinos,
    osrm_server = osrm_server, osrm_profile = osrm_profile,
    max_snap_osrm_km = max_snap_osrm_km, max_ratio_desvio = max_ratio_desvio,
    max_pares = max_pares, max_tries = max_tries, verbose = verbose
  )

  origens_4326 <- sf::st_transform(origens, 4326)
  destinos_4326 <- sf::st_transform(destinos, 4326)

  lapply(seq_len(nrow(tri)), function(i) {

    t <- tri[i, ]
    motivo <- .motivo_rodovia(t, max_snap_osrm_km, max_ratio_desvio)
    if (!is.na(motivo)) return(list(candidata = NULL, motivo = motivo))

    distancia_km <- t$distancia_rodovia_km
    tempo_h <- t$tempo_rodovia_h
    rota_sf <- NULL

    if (geometria) {
      rota <- tryCatch(
        .osrm_com_retry(
          function() {
            osrm::osrmRoute(
              src = origens_4326[i, ], dst = destinos_4326[i, ],
              osrm.server = osrm_server, osrm.profile = osrm_profile
            )
          },
          contexto = "obter a rota rodoviaria no OSRM", max_tries = max_tries, verbose = verbose
        ),
        error = function(e) e
      )
      if (inherits(rota, "error")) {
        return(list(candidata = NULL, motivo = conditionMessage(rota)))
      }
      if (is.null(rota) || nrow(rota) == 0) {
        return(list(candidata = NULL, motivo = "sem rota rodoviaria no OSRM entre origem e destino"))
      }
      distancia_km <- rota$distance[1]
      tempo_h <- rota$duration[1] / 60
      rota_sf <- sf::st_sf(modal = "rodovia", geometry = sf::st_geometry(rota))
    }

    razao_desvio <- if (t$dist_linha_reta_km > 0) distancia_km / t$dist_linha_reta_km else 1

    list(
      candidata = list(
        resumo = data.frame(modal = "rodovia", distancia_km = distancia_km, tempo_h = tempo_h),
        rota = rota_sf,
        tempo_total_h = tempo_h,
        distancia_total_km = distancia_km,
        confiabilidade = list(
          modal = "rodovia",
          snap_osrm_origem_km = t$snap_origem_km,
          snap_osrm_destino_km = t$snap_destino_km,
          razao_desvio = razao_desvio
        )
      ),
      motivo = NA_character_
    )
  })
}

#' Agrupa pares origem-destino próximos para compartilhar uma mesma rede hidroviária
#'
#' @description
#' Montar a rede (\code{.constroi_rede_hidroviaria}) é a etapa cara do roteamento
#' hidroviário; pares de uma mesma região podem compartilhar uma rede só. O agrupamento é
#' guloso: cada par (retângulo envolvente de origem/destino + margem) entra no primeiro
#' grupo cujo retângulo, somado ao dele, não passe de `max_area_deg2` graus quadrados — um
#' teto para que pares espalhados (ex. todo o país) não gerem uma única rede gigantesca.
#'
#' @return Lista de vetores de índices de pares, um por grupo.
#' @keywords internal
#' @noRd
.agrupa_pares <- function(origens_4326, destinos_4326, buffer_km, max_area_deg2 = 4) {

  xy_o <- sf::st_coordinates(origens_4326)
  xy_d <- sf::st_coordinates(destinos_4326)
  buffer_deg <- buffer_km / 111

  caixas <- cbind(
    xmin = pmin(xy_o[, 1], xy_d[, 1]) - buffer_deg,
    ymin = pmin(xy_o[, 2], xy_d[, 2]) - buffer_deg,
    xmax = pmax(xy_o[, 1], xy_d[, 1]) + buffer_deg,
    ymax = pmax(xy_o[, 2], xy_d[, 2]) + buffer_deg
  )

  grupos <- list()
  caixas_grupo <- list()

  for (i in order(caixas[, "xmin"], caixas[, "ymin"])) {
    alocado <- FALSE
    for (g in seq_along(grupos)) {
      uniao <- c(
        min(caixas_grupo[[g]][1], caixas[i, 1]), min(caixas_grupo[[g]][2], caixas[i, 2]),
        max(caixas_grupo[[g]][3], caixas[i, 3]), max(caixas_grupo[[g]][4], caixas[i, 4])
      )
      if ((uniao[3] - uniao[1]) * (uniao[4] - uniao[2]) <= max_area_deg2) {
        grupos[[g]] <- c(grupos[[g]], i)
        caixas_grupo[[g]] <- uniao
        alocado <- TRUE
        break
      }
    }
    if (!alocado) {
      grupos[[length(grupos) + 1]] <- i
      caixas_grupo[[length(caixas_grupo) + 1]] <- unname(caixas[i, ])
    }
  }

  lapply(grupos, sort)
}

#' Monta a candidata "hidrovia + caminhada" de um par a partir do caminho na rede
#' @keywords internal
#' @noRd
.monta_candidata_hidrovia <- function(
    ponto_origem_m, ponto_destino_m, no_origem_geom, no_destino_geom,
    acesso_origem_km, acesso_destino_km, caminho, vel_caminhada_kmh, crs_metrico, n_waterways
) {

  linha_acesso <- sf::st_linestring(rbind(
    sf::st_coordinates(ponto_origem_m)[1, 1:2], sf::st_coordinates(no_origem_geom)[1, 1:2]
  ))
  linha_desembarque <- sf::st_linestring(rbind(
    sf::st_coordinates(no_destino_geom)[1, 1:2], sf::st_coordinates(ponto_destino_m)[1, 1:2]
  ))

  resumo <- data.frame(
    modal = c("caminhada_acesso", "hidrovia", "caminhada_desembarque"),
    distancia_km = c(acesso_origem_km, caminho$distancia_km, acesso_destino_km),
    tempo_h = c(
      acesso_origem_km / vel_caminhada_kmh, caminho$tempo_h, acesso_destino_km / vel_caminhada_kmh
    )
  )

  rota_sf <- sf::st_sf(
    modal = resumo$modal,
    geometry = sf::st_sfc(
      list(linha_acesso, caminho$geometry[[1]], linha_desembarque),
      crs = crs_metrico
    )
  )

  list(
    resumo = resumo,
    rota = sf::st_transform(rota_sf, 4326),
    tempo_total_h = sum(resumo$tempo_h),
    distancia_total_km = sum(resumo$distancia_km),
    confiabilidade = list(
      modal = "hidrovia",
      caminhada_acesso_km = acesso_origem_km,
      caminhada_desembarque_km = acesso_destino_km,
      n_waterways_na_area = n_waterways
    )
  )
}

#' Candidatas "hidrovia + caminhada" para um grupo de pares que compartilha uma rede
#'
#' @description
#' Busca as waterways do retângulo envolvente de todos os pontos do grupo (+ margem), monta
#' a rede e o grafo uma única vez e liga cada ponto a cada componente conexa da rede ao
#' alcance da caminhada (\code{.acessos_por_componente}). Para cada par vale a combinação
#' de acessos (na mesma componente) com o menor tempo total; roda um Dijkstra por nó de
#' acesso de origem (\code{.caminhos_hidroviarios}), cobrindo todos os destinos dele.
#'
#' @return Lista com um elemento por par: `list(candidata, motivo, nao_conectado)`.
#'   `nao_conectado = TRUE` marca os pares que podem se resolver com uma área de busca maior.
#' @keywords internal
#' @noRd
.hidrovia_grupo <- function(
    origens_4326, destinos_4326, buffer_km, tipos_hidrovia, crs_metrico, tolerancia_juncao_m,
    vel_jusante_kmh, vel_montante_kmh, vel_caminhada_kmh, max_caminhada_km, direcionar_fluxo,
    cache_dir, cache_max_age_dias, verbose
) {

  n <- nrow(origens_4326)
  descarte <- function(motivo, nao_conectado = FALSE) {
    list(candidata = NULL, motivo = motivo, nao_conectado = nao_conectado)
  }

  # Pontos distintos do grupo (várias origens para o mesmo destino, por ex., são comuns).
  todos_ll <- rbind(sf::st_coordinates(origens_4326), sf::st_coordinates(destinos_4326))[, 1:2, drop = FALSE]
  chave <- paste(round(todos_ll[, 1], 7), round(todos_ll[, 2], 7))
  unicos <- !duplicated(chave)
  idx_ponto <- match(chave, chave[unicos])
  idx_origem <- idx_ponto[seq_len(n)]
  idx_destino <- idx_ponto[n + seq_len(n)]

  pontos_4326 <- sf::st_sf(geometry = sf::st_sfc(
    lapply(which(unicos), function(k) sf::st_point(todos_ll[k, ])), crs = 4326
  ))

  bbox_pontos <- sf::st_bbox(pontos_4326)
  buffer_deg <- buffer_km / 111
  bbox_busca <- c(
    bbox_pontos[["xmin"]] - buffer_deg, bbox_pontos[["ymin"]] - buffer_deg,
    bbox_pontos[["xmax"]] + buffer_deg, bbox_pontos[["ymax"]] + buffer_deg
  )

  sf_hid <- busca_hidrovias_osm(
    bbox_busca,
    tipos = tipos_hidrovia,
    cache_dir = cache_dir,
    cache_max_age_dias = cache_max_age_dias,
    verbose = verbose
  )

  if (is.null(sf_hid) || nrow(sf_hid) == 0) {
    return(rep(list(descarte("nenhuma waterway encontrada na area de busca")), n))
  }

  net <- .constroi_rede_hidroviaria(sf_hid, crs_metrico = crs_metrico, tolerancia_juncao_m = tolerancia_juncao_m)
  pontos_m <- sf::st_transform(pontos_4326, crs_metrico)

  acessos <- .acessos_por_componente(net, pontos_m, max_dist_m = max_caminhada_km * 1000)
  opcoes <- acessos$opcoes

  grafo <- .grafo_hidroviario(
    acessos$net,
    vel_jusante_kmh = vel_jusante_kmh,
    vel_montante_kmh = vel_montante_kmh,
    direcionar_fluxo = direcionar_fluxo
  )

  resultado <- vector("list", n)

  # Combinações (par, acesso de origem, acesso de destino) na mesma componente conexa.
  combinacoes <- list()
  for (i in seq_len(n)) {
    op_o <- opcoes[opcoes$ponto == idx_origem[i], ]
    op_d <- opcoes[opcoes$ponto == idx_destino[i], ]

    if (nrow(op_o) == 0 || nrow(op_d) == 0) {
      resultado[[i]] <- descarte(paste0(
        "caminhada de acesso excede max_caminhada_km (origem ",
        round(acessos$dist_min_km[idx_origem[i]], 1), " km, destino ",
        round(acessos$dist_min_km[idx_destino[i]], 1), " km; limite ", max_caminhada_km, " km)"
      ))
      next
    }

    comb <- merge(op_o, op_d, by = "componente", suffixes = c("_o", "_d"))
    if (nrow(comb) == 0) {
      resultado[[i]] <- descarte(
        paste0(
          "origem e destino nao estao conectados pela rede de waterways encontrada ",
          "(margem de busca de ", buffer_km, " km)"
        ),
        nao_conectado = TRUE
      )
      next
    }
    comb$par <- i
    combinacoes[[length(combinacoes) + 1]] <- comb
  }

  if (length(combinacoes) == 0) return(resultado)
  combinacoes <- do.call(rbind, combinacoes)

  # Um Dijkstra por nó de acesso de origem, cobrindo todos os destinos que dependem dele.
  caminhos <- vector("list", nrow(combinacoes))
  for (no_o in unique(combinacoes$no_o)) {
    linhas <- which(combinacoes$no_o == no_o)
    caminhos[linhas] <- .caminhos_hidroviarios(grafo, no_o, combinacoes$no_d[linhas])
  }
  combinacoes$tempo_total_h <- vapply(seq_len(nrow(combinacoes)), function(r) {
    if (!caminhos[[r]]$viavel) return(Inf)
    (combinacoes$acesso_km_o[r] + combinacoes$acesso_km_d[r]) / vel_caminhada_kmh + caminhos[[r]]$tempo_h
  }, numeric(1))

  for (i in unique(combinacoes$par)) {
    linhas <- which(combinacoes$par == i)
    r <- linhas[which.min(combinacoes$tempo_total_h[linhas])]
    resultado[[i]] <- list(
      candidata = .monta_candidata_hidrovia(
        pontos_m[idx_origem[i], ], pontos_m[idx_destino[i], ],
        acessos$geom_nos[combinacoes$no_o[r]], acessos$geom_nos[combinacoes$no_d[r]],
        combinacoes$acesso_km_o[r], combinacoes$acesso_km_d[r], caminhos[[r]],
        vel_caminhada_kmh, crs_metrico, nrow(sf_hid)
      ),
      motivo = NA_character_,
      nao_conectado = FALSE
    )
  }

  resultado
}

#' Candidatas "hidrovia + caminhada" para vários pares, ampliando a busca quando preciso
#'
#' @description
#' Um rio que faz uma volta grande para fora do retângulo origem/destino é cortado pela
#' área de busca, e os dois pontos parecem desconectados. Os pares nessa situação são
#' refeitos com a margem dobrada, até `bbox_buffer_max_km`. A margem inicial nunca é menor
#' que `max_caminhada_km` — senão um rio dentro do limite de caminhada, mas fora da margem,
#' nem seria buscado.
#'
#' @return Lista com um elemento por par: `list(candidata, motivo)`.
#' @keywords internal
#' @noRd
.hidrovia_pares <- function(
    origens, destinos, tipos_hidrovia, bbox_buffer_km, bbox_buffer_max_km, crs_metrico,
    tolerancia_juncao_m, vel_jusante_kmh, vel_montante_kmh, vel_caminhada_kmh,
    max_caminhada_km, direcionar_fluxo, cache_dir, cache_max_age_dias, verbose
) {

  origens_4326 <- sf::st_transform(origens, 4326)
  destinos_4326 <- sf::st_transform(destinos, 4326)
  n <- nrow(origens_4326)

  buffer_inicial <- max(bbox_buffer_km, max_caminhada_km)
  margens <- buffer_inicial
  while (utils::tail(margens, 1) * 2 <= bbox_buffer_max_km) {
    margens <- c(margens, utils::tail(margens, 1) * 2)
  }
  if (utils::tail(margens, 1) < bbox_buffer_max_km) margens <- c(margens, bbox_buffer_max_km)

  resultado <- vector("list", n)
  pendentes <- seq_len(n)

  for (m in seq_along(margens)) {
    if (length(pendentes) == 0) break
    margem <- margens[m]
    if (m > 1 && verbose) {
      message(
        length(pendentes), " par(es) sem conexao pela rede hidroviaria - ampliando a margem de ",
        "busca para ", margem, " km..."
      )
    }

    grupos <- .agrupa_pares(origens_4326[pendentes, ], destinos_4326[pendentes, ], margem)
    nao_conectados <- integer()

    for (grp in grupos) {
      idx <- pendentes[grp]
      res <- tryCatch(
        .hidrovia_grupo(
          origens_4326[idx, ], destinos_4326[idx, ], margem, tipos_hidrovia, crs_metrico,
          tolerancia_juncao_m, vel_jusante_kmh, vel_montante_kmh, vel_caminhada_kmh,
          max_caminhada_km, direcionar_fluxo, cache_dir, cache_max_age_dias, verbose
        ),
        error = function(e) {
          rep(list(list(
            candidata = NULL, motivo = paste0("indisponivel: ", conditionMessage(e)), nao_conectado = FALSE
          )), length(idx))
        }
      )
      resultado[idx] <- res
      nao_conectados <- c(nao_conectados, idx[vapply(res, function(r) isTRUE(r$nao_conectado), logical(1))])
    }

    pendentes <- nao_conectados
  }

  lapply(resultado, function(r) list(candidata = r$candidata, motivo = r$motivo))
}

#' Roteia vários pares origem-destino: candidatas rodovia e hidrovia de cada par
#'
#' Motor comum de \code{\link{rotear_multimodal}}, \code{\link{rotear_multimodal_lote}} e
#' \code{\link{rotear_mais_proximo}}.
#'
#' @return Lista com um elemento por par: `list(rodovia, hidrovia)`, cada um
#'   `list(candidata, motivo)`.
#' @keywords internal
#' @noRd
.rotear_pares <- function(origens, destinos, p, geometria) {

  n <- nrow(origens)

  rodovia <- tryCatch(
    .rodovia_pares(
      origens, destinos, p$osrm_server, p$osrm_profile,
      p$max_snap_osrm_km, p$max_ratio_desvio, p$max_pares, geometria, p$max_tries, p$verbose
    ),
    error = function(e) {
      rep(list(list(candidata = NULL, motivo = paste0("indisponivel: ", conditionMessage(e)))), n)
    }
  )

  hidrovia <- .hidrovia_pares(
    origens, destinos, p$tipos_hidrovia, p$bbox_buffer_km, p$bbox_buffer_max_km, p$crs_metrico,
    p$tolerancia_juncao_m, p$vel_hidrovia_jusante_kmh, p$vel_hidrovia_montante_kmh,
    p$vel_caminhada_kmh, p$max_caminhada_km, p$direcionar_fluxo,
    p$cache_dir, p$cache_max_age_dias, p$verbose
  )

  lapply(seq_len(n), function(i) list(rodovia = rodovia[[i]], hidrovia = hidrovia[[i]]))
}

#' Candidata vencedora (menor tempo total) de um par, ou `NULL`
#' @keywords internal
#' @noRd
.vencedora <- function(par) {
  candidatas <- Filter(Negate(is.null), list(
    rodovia = par$rodovia$candidata, hidrovia = par$hidrovia$candidata
  ))
  if (length(candidatas) == 0) return(NULL)
  candidatas[[which.min(vapply(candidatas, function(x) x$tempo_total_h, numeric(1)))]]
}

#' Resume os resultados de vários pares numa tibble (1 linha por par) + rotas opcionais
#' @keywords internal
#' @noRd
.resume_pares <- function(resultados, origens, destinos, geometria) {

  dist_linha_reta_km <- as.numeric(sf::st_distance(
    sf::st_transform(origens, 4326), sf::st_transform(destinos, 4326), by_element = TRUE
  )) / 1000

  campo <- function(x, nome) if (is.null(x)) NA_real_ else x[[nome]]

  linhas <- lapply(seq_along(resultados), function(i) {
    r <- resultados[[i]]
    rod <- r$rodovia$candidata
    hid <- r$hidrovia$candidata
    venc <- .vencedora(r)
    tibble::tibble(
      par = i,
      modal = if (is.null(venc)) NA_character_ else venc$confiabilidade$modal,
      tempo_total_h = campo(venc, "tempo_total_h"),
      distancia_total_km = campo(venc, "distancia_total_km"),
      dist_linha_reta_km = dist_linha_reta_km[i],
      tempo_rodovia_h = campo(rod, "tempo_total_h"),
      distancia_rodovia_km = campo(rod, "distancia_total_km"),
      tempo_hidrovia_h = campo(hid, "tempo_total_h"),
      distancia_hidrovia_km = campo(hid, "distancia_total_km"),
      caminhada_acesso_km = if (is.null(hid)) NA_real_ else hid$confiabilidade$caminhada_acesso_km,
      caminhada_desembarque_km = if (is.null(hid)) NA_real_ else hid$confiabilidade$caminhada_desembarque_km,
      motivo_rodovia = r$rodovia$motivo,
      motivo_hidrovia = r$hidrovia$motivo
    )
  })
  resumo <- do.call(rbind, linhas)

  rotas <- NULL
  if (geometria) {
    partes <- lapply(seq_along(resultados), function(i) {
      venc <- .vencedora(resultados[[i]])
      if (is.null(venc) || is.null(venc$rota)) return(NULL)
      rota <- venc$rota
      rota$par <- i
      rota[, c("par", "modal")]
    })
    partes <- Filter(Negate(is.null), partes)
    if (length(partes) > 0) rotas <- do.call(rbind, partes)
  }

  list(resumo = resumo, rotas = rotas)
}

#' Roteamento ótimo multimodal entre dois pontos (rodovia via OSRM, hidrovia via OSM/Overpass)
#'
#' @description
#' Encontra a rota mais rápida entre `origem` e `destino` comparando duas candidatas
#' genéricas — não específicas de nenhum órgão ou recorte territorial:
#' \itemize{
#'   \item \strong{rodovia}: roteamento via API OSRM (\pkg{osrm}), com crítica ao
#'     comportamento de *snap* do OSRM (ver \code{max_snap_osrm_km} abaixo) e a um limite de
#'     razão de desvio (\code{max_ratio_desvio});
#'   \item \strong{hidrovia + caminhada}: waterways do OpenStreetMap (via
#'     \code{\link{busca_hidrovias_osm}}, com cache local em DuckDB) montadas em um grafo
#'     direcionado (jusante mais rápido que montante, por convenção de digitalização do OSM —
#'     ver \code{vel_hidrovia_jusante_kmh}/\code{vel_hidrovia_montante_kmh}), com trechos de
#'     caminhada de última milha entre os pontos pedidos e a rede navegável.
#' }
#' Foi desenhada para aproximar o roteamento até localidades isoladas da Amazônia, onde
#' nenhuma das duas fontes isoladamente é suficiente — mas os dois pontos de entrada podem ser
#' quaisquer dois pontos, em qualquer lugar. Para muitos pares, use
#' \code{\link{rotear_multimodal_lote}}; para achar o destino mais próximo entre vários
#' candidatos, \code{\link{rotear_mais_proximo}}.
#'
#' @details
#' Robustez da rede hidroviária:
#' \itemize{
#'   \item Linhas que no OSM quase se encontram (costuras entre tiles do cache, afluentes que
#'     terminam a poucos metros do rio principal) são ligadas quando a distância é de até
#'     `tolerancia_juncao_m`.
#'   \item Se origem e destino não estão conectados pela rede encontrada, a busca é refeita
#'     com a margem dobrada (até `bbox_buffer_max_km`), para o caso de o rio sair do
#'     retângulo de busca inicial.
#' }
#'
#' @param origem,destino Objeto `sf`/`sfc` POINT (1 feature) ou vetor `c(lon, lat)`.
#' @param osrm_server URL do servidor OSRM. Padrão: servidor público de demonstração — use
#'   apenas para prototipagem; para uso em lote (muitas chamadas), aponte para uma instância
#'   própria, já que o servidor público tem limite de requisições e seus termos de uso
#'   desaconselham uso em lote/produção.
#' @param osrm_profile Perfil de roteamento do OSRM (ex. `"driving"`, `"foot"` — depende dos
#'   perfis disponíveis no `osrm_server` usado).
#' @param vel_hidrovia_jusante_kmh,vel_hidrovia_montante_kmh Velocidade (km/h) de embarcação a
#'   favor/contra a correnteza, usada como peso das arestas do grafo hidroviário.
#' @param vel_caminhada_kmh Velocidade (km/h) de caminhada para os trechos de última milha.
#' @param max_caminhada_km Distância máxima (km) de caminhada de última milha aceitável; a
#'   candidata hidrovia é descartada se a origem ou o destino estiverem mais longe da rede
#'   navegável do que isso.
#' @param max_ratio_desvio Razão máxima aceitável entre a distância da rota rodoviária e a
#'   distância em linha reta entre origem e destino; acima disso, a candidata rodovia é
#'   descartada por implausibilidade.
#' @param max_snap_osrm_km Distância máxima (km) de encaixe (*snap*) do OSRM aceitável para
#'   origem/destino na malha rodoviária conhecida — o principal resguardo contra rotas OSRM
#'   enganosas para localidades isoladas (o OSRM encaixa o ponto na estrada mais próxima,
#'   por mais longe que ela esteja, e ignora esse trecho).
#' @param direcionar_fluxo Se `TRUE` (padrão), o grafo hidroviário é direcionado (jusante mais
#'   rápido que montante). Se `FALSE`, usa `vel_hidrovia_jusante_kmh` nos dois sentidos.
#' @param tipos_hidrovia Valores da tag OSM `waterway` a considerar navegáveis. Padrão:
#'   `c("river", "canal")`.
#' @param bbox_buffer_km Margem (km) ao redor do retângulo envolvente de origem/destino usada
#'   na busca de waterways no Overpass. Nunca menor que `max_caminhada_km`.
#' @param bbox_buffer_max_km Margem máxima (km) até a qual a busca é ampliada (dobrando a
#'   margem a cada tentativa) quando origem e destino não se conectam pela rede encontrada.
#'   Use o mesmo valor de `bbox_buffer_km` para desligar a ampliação.
#' @param tolerancia_juncao_m Distância máxima (m) para ligar linhas de hidrovia que quase se
#'   encontram. `0` desliga o conserto.
#' @param crs_metrico Código EPSG métrico usado internamente para cálculo de distâncias/grafo.
#' @param cache_dir Diretório do cache local (Overpass) em DuckDB.
#' @param cache_max_age_dias Idade máxima (dias) de dados em cache antes de rebuscar.
#' @param max_tries Número máximo de tentativas de cada chamada ao OSRM.
#' @param verbose Se `TRUE`, imprime mensagens de progresso e de descarte de candidatas.
#'
#' @return Uma lista com `resumo` (data.frame com `modal`/`distancia_km`/`tempo_h` por
#'   trecho), `rota` (sf LINESTRING, 1 linha por trecho), `tempo_total_h`,
#'   `distancia_total_km` e `confiabilidade` (lista com os indicadores de auditoria da
#'   candidata vencedora — ex. distâncias de snap do OSRM, mesmo quando a rota foi aceita).
#'   Retorna `NULL` (com aviso) se nenhuma candidata for viável.
#'
#' @export
#' @importFrom sf st_distance st_transform
rotear_multimodal <- function(
    origem,
    destino,
    osrm_server = "https://router.project-osrm.org/",
    osrm_profile = "driving",
    vel_hidrovia_jusante_kmh = 20,
    vel_hidrovia_montante_kmh = 12,
    vel_caminhada_kmh = 4.5,
    max_caminhada_km = 30,
    max_ratio_desvio = 3.0,
    max_snap_osrm_km = 5.0,
    direcionar_fluxo = TRUE,
    tipos_hidrovia = c("river", "canal"),
    bbox_buffer_km = 25,
    bbox_buffer_max_km = 100,
    tolerancia_juncao_m = 50,
    crs_metrico = 5880,
    cache_dir = tools::R_user_dir("indigenR", "cache"),
    cache_max_age_dias = 30,
    max_tries = 5,
    verbose = TRUE
) {

  origem <- .padroniza_ponto(origem)
  destino <- .padroniza_ponto(destino)

  parametros <- mget(setdiff(names(formals()), c("origem", "destino")))
  parametros$max_pares <- 50

  par <- .rotear_pares(origem, destino, parametros, geometria = TRUE)[[1]]

  if (verbose) {
    for (modal in c("rodovia", "hidrovia")) {
      motivo <- par[[modal]]$motivo
      if (!is.na(motivo)) message("Candidata ", modal, " descartada: ", motivo, ".")
    }
  }

  vencedora <- .vencedora(par)

  if (is.null(vencedora)) {
    warning("Nenhuma rota viavel encontrada entre origem e destino.", call. = FALSE)
    return(NULL)
  }

  list(
    resumo = vencedora$resumo,
    rota = vencedora$rota,
    tempo_total_h = vencedora$tempo_total_h,
    distancia_total_km = vencedora$distancia_total_km,
    confiabilidade = vencedora$confiabilidade
  )
}

#' Roteamento multimodal em lote (vários pares origem-destino)
#'
#' @description
#' Mesmo roteamento de \code{\link{rotear_multimodal}}, para vários pares de uma vez, com o
#' custo repartido:
#' \itemize{
#'   \item \strong{rodovia}: uma consulta \code{osrm::osrmTable()} por bloco de `max_pares`
#'     pares (via \code{\link{triagem_rodovia_lote}}) em vez de três chamadas por par; o
#'     traçado (\code{osrm::osrmRoute()}, uma chamada por par) só é pedido com
#'     `geometria = TRUE`, e só para pares cuja rota rodoviária é plausível.
#'   \item \strong{hidrovia}: pares próximos são agrupados e compartilham a mesma rede
#'     hidroviária (montada uma vez por grupo) e um Dijkstra por origem distinta.
#' }
#'
#' @param origens,destinos Objetos `sf`/`sfc` POINT, `matrix`/`data.frame` com 2 colunas
#'   (lon, lat), com o mesmo número de pares (1 linha/feature = 1 par origem-destino).
#' @inheritParams rotear_multimodal
#' @param max_pares Pares por consulta ao OSRM Table — ver \code{\link{triagem_rodovia_lote}}.
#' @param geometria Se `TRUE`, devolve também o traçado da rota vencedora de cada par
#'   (custa uma chamada \code{osrm::osrmRoute()} por par com rota rodoviária plausível).
#'
#' @return Uma lista com:
#'   \describe{
#'     \item{resumo}{Tibble, 1 linha por par: `par`, `modal` vencedor (`"rodovia"`,
#'       `"hidrovia"` ou `NA` se nenhum), `tempo_total_h`, `distancia_total_km`,
#'       `dist_linha_reta_km`, tempo/distância de cada candidata (`tempo_rodovia_h`,
#'       `distancia_rodovia_km`, `tempo_hidrovia_h`, `distancia_hidrovia_km` — a da hidrovia
#'       inclui as caminhadas), `caminhada_acesso_km`, `caminhada_desembarque_km` e
#'       `motivo_rodovia`/`motivo_hidrovia` (por que a candidata foi descartada; `NA` se
#'       não foi).}
#'     \item{rotas}{Com `geometria = TRUE`, sf LINESTRING (EPSG:4326) com os trechos da rota
#'       vencedora de cada par (colunas `par`, `modal`); senão `NULL`.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lote <- rotear_multimodal_lote(
#'   origens = matrix(c(-60.0217, -3.1190, -60.1875, -3.2778), ncol = 2, byrow = TRUE),
#'   destinos = matrix(c(-58.4453, -3.1431, -60.6206, -3.2996), ncol = 2, byrow = TRUE)
#' )
#' lote$resumo
#' }
rotear_multimodal_lote <- function(
    origens,
    destinos,
    osrm_server = "https://router.project-osrm.org/",
    osrm_profile = "driving",
    vel_hidrovia_jusante_kmh = 20,
    vel_hidrovia_montante_kmh = 12,
    vel_caminhada_kmh = 4.5,
    max_caminhada_km = 30,
    max_ratio_desvio = 3.0,
    max_snap_osrm_km = 5.0,
    direcionar_fluxo = TRUE,
    tipos_hidrovia = c("river", "canal"),
    bbox_buffer_km = 25,
    bbox_buffer_max_km = 100,
    tolerancia_juncao_m = 50,
    crs_metrico = 5880,
    cache_dir = tools::R_user_dir("indigenR", "cache"),
    cache_max_age_dias = 30,
    max_pares = 50,
    max_tries = 5,
    geometria = FALSE,
    verbose = TRUE
) {

  origens <- .padroniza_pontos(origens)
  destinos <- .padroniza_pontos(destinos)

  if (nrow(origens) != nrow(destinos)) {
    stop("`origens` e `destinos` devem ter o mesmo numero de pares.", call. = FALSE)
  }

  parametros <- mget(setdiff(names(formals()), c("origens", "destinos", "geometria")))

  resultados <- .rotear_pares(origens, destinos, parametros, geometria = geometria)
  .resume_pares(resultados, origens, destinos, geometria)
}

#' Destino mais próximo de um ponto, em tempo de deslocamento multimodal
#'
#' @description
#' Dado um ponto de `origem` e um conjunto de `candidatos` (ex. unidades da Funai, polos
#' base, sedes municipais), ordena os candidatos pelo tempo de deslocamento multimodal
#' (\code{\link{rotear_multimodal}}) a partir da origem.
#'
#' Rotear até todos os candidatos seria caro e desnecessário: primeiro são escolhidos os
#' `k` candidatos mais próximos em linha reta, e só esses são roteados
#' (\code{\link{rotear_multimodal_lote}}, compartilhando a rede hidroviária). Se o candidato
#' mais rápido pode estar além dos `k` mais próximos em linha reta (ex. rede de rios muito
#' sinuosa), aumente `k`.
#'
#' @param origem Objeto `sf`/`sfc` POINT (1 feature) ou vetor `c(lon, lat)`.
#' @param candidatos Objeto `sf` POINT (os atributos são devolvidos junto no resultado),
#'   `sfc`, ou `matrix`/`data.frame` com 2 colunas (lon, lat).
#' @param k Número de candidatos mais próximos em linha reta a rotear.
#' @inheritParams rotear_multimodal_lote
#' @param geometria Se `TRUE` (padrão), devolve também o traçado das rotas.
#'
#' @return Uma lista com:
#'   \describe{
#'     \item{resumo}{Tibble com os `k` candidatos roteados, do mais rápido para o mais
#'       lento (sem rota viável por último): `candidato` (índice da linha em `candidatos`),
#'       os atributos de `candidatos` (se for `sf`) e as colunas de resultado de
#'       \code{\link{rotear_multimodal_lote}} (exceto `par`).}
#'     \item{rotas}{Com `geometria = TRUE`, sf com os trechos da rota vencedora até cada
#'       candidato (coluna `candidato` no lugar de `par`); senão `NULL`.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Unidade mais próxima (em tempo) de uma aldeia, entre as unidades cadastradas
#' mais_proxima <- rotear_mais_proximo(c(-67.08, -0.13), unidades_sf, k = 5)
#' mais_proxima$resumo[1, ]
#' }
rotear_mais_proximo <- function(
    origem,
    candidatos,
    k = 5,
    osrm_server = "https://router.project-osrm.org/",
    osrm_profile = "driving",
    vel_hidrovia_jusante_kmh = 20,
    vel_hidrovia_montante_kmh = 12,
    vel_caminhada_kmh = 4.5,
    max_caminhada_km = 30,
    max_ratio_desvio = 3.0,
    max_snap_osrm_km = 5.0,
    direcionar_fluxo = TRUE,
    tipos_hidrovia = c("river", "canal"),
    bbox_buffer_km = 25,
    bbox_buffer_max_km = 100,
    tolerancia_juncao_m = 50,
    crs_metrico = 5880,
    cache_dir = tools::R_user_dir("indigenR", "cache"),
    cache_max_age_dias = 30,
    max_pares = 50,
    max_tries = 5,
    geometria = TRUE,
    verbose = TRUE
) {

  origem <- .padroniza_ponto(origem)
  atributos <- if (inherits(candidatos, "sf")) sf::st_drop_geometry(candidatos) else NULL
  pontos <- .padroniza_pontos(candidatos)

  if (nrow(pontos) == 0) stop("`candidatos` esta vazio.", call. = FALSE)

  linha_reta_km <- as.numeric(sf::st_distance(
    sf::st_transform(origem, 4326), sf::st_transform(pontos, 4326)
  )[1, ]) / 1000
  escolhidos <- order(linha_reta_km)[seq_len(min(k, nrow(pontos)))]

  if (verbose) {
    message(
      "Roteando ate os ", length(escolhidos), " candidato(s) mais proximo(s) em linha reta ",
      "(de ", nrow(pontos), ")..."
    )
  }

  parametros <- mget(setdiff(names(formals()), c("origem", "candidatos", "k", "geometria")))

  origens <- origem[rep(1, length(escolhidos)), ]
  destinos <- pontos[escolhidos, ]
  resultados <- .rotear_pares(origens, destinos, parametros, geometria = geometria)
  lote <- .resume_pares(resultados, origens, destinos, geometria)

  resumo <- lote$resumo
  resumo$par <- NULL
  resumo <- tibble::add_column(resumo, candidato = escolhidos, .before = 1)
  if (!is.null(atributos)) {
    resumo <- tibble::as_tibble(cbind(
      resumo["candidato"], atributos[escolhidos, , drop = FALSE], resumo[setdiff(names(resumo), "candidato")]
    ))
  }
  ordem <- order(resumo$tempo_total_h, na.last = TRUE)
  resumo <- resumo[ordem, ]

  rotas <- lote$rotas
  if (!is.null(rotas)) {
    rotas$candidato <- escolhidos[rotas$par]
    rotas <- rotas[, c("candidato", "modal")]
  }

  list(resumo = resumo, rotas = rotas)
}

#' Triagem de um bloco de pares via uma única chamada OSRM Table
#' @keywords internal
#' @noRd
.triagem_rodovia_bloco <- function(
    origens_4326, destinos_4326, osrm_server, osrm_profile,
    max_snap_osrm_km, max_ratio_desvio, max_tries, verbose
) {

  n <- nrow(origens_4326)

  # Consulta origens e destinos juntos num unico 'loc' (nao 'src'/'dst' separados): alguns
  # servidores OSRM, incluindo o publico router.project-osrm.org (o padrao de
  # rotear_multimodal()), respondem com uma matriz de zeros em vez de erro quando src/dst sao
  # passados como conjuntos separados - confirmado empiricamente. 'loc' unico e' suportado por
  # qualquer servidor com o servico de tabela habilitado, ao custo de uma matriz (2n)x(2n) em
  # vez de nxn (ver max_pares).
  todos_ll <- rbind(sf::st_coordinates(origens_4326), sf::st_coordinates(destinos_4326))

  # O mesmo servidor tambem responde com uma matriz de zeros (de novo, sem erro) quando o
  # lote tem coordenadas duplicadas/quase-duplicadas - comum em roteamento em lote de verdade
  # (ex. varias origens para o mesmo hub). Deduplicar antes de consultar evita o problema e
  # de quebra reduz o tamanho da tabela pedida.
  chave <- paste(round(todos_ll[, 1], 6), round(todos_ll[, 2], 6))
  pontos_unicos <- todos_ll[!duplicated(chave), , drop = FALSE]
  idx_unico <- match(chave, chave[!duplicated(chave)])

  if (verbose && nrow(pontos_unicos) < nrow(todos_ll)) {
    message(
      nrow(todos_ll) - nrow(pontos_unicos), " coordenada(s) duplicada(s) no lote - ",
      "consultando ", nrow(pontos_unicos), " pontos unicos."
    )
  }

  tab <- .osrm_com_retry(
    function() {
      osrm::osrmTable(
        loc = pontos_unicos,
        measure = c("duration", "distance"),
        osrm.server = osrm_server, osrm.profile = osrm_profile
      )
    },
    contexto = "consultar a tabela de tempos do OSRM", max_tries = max_tries, verbose = verbose
  )

  idx_origens <- idx_unico[seq_len(n)]
  idx_destinos <- idx_unico[n + seq_len(n)]

  tempo_rodovia_h <- tab$durations[cbind(idx_origens, idx_destinos)] / 60
  distancia_rodovia_km <- tab$distances[cbind(idx_origens, idx_destinos)] / 1000

  dist_linha_reta_km <- as.numeric(
    sf::st_distance(origens_4326, destinos_4326, by_element = TRUE)
  ) / 1000

  # tab$sources/$destinations sao os pontos unicos de 'loc' (identicos entre si, em modo
  # loc) - reusar idx_origens/idx_destinos (o mesmo mapeamento p/ pontos duplicados) para
  # pegar os pontos de origem/destino correspondentes a cada par.
  snap_origem_km <- as.numeric(sf::st_distance(
    sf::st_geometry(origens_4326),
    sf::st_as_sf(tab$sources[idx_origens, ], coords = c("lon", "lat"), crs = 4326),
    by_element = TRUE
  )) / 1000

  snap_destino_km <- as.numeric(sf::st_distance(
    sf::st_geometry(destinos_4326),
    sf::st_as_sf(tab$destinations[idx_destinos, ], coords = c("lon", "lat"), crs = 4326),
    by_element = TRUE
  )) / 1000

  # Defesa extra: se mesmo assim vier tudo zero para pares que deveriam ter rota (origem !=
  # destino e os dois pontos encaixados perto da malha), o servidor esta respondendo errado
  # (ex. servico de tabela mal configurado) - falhar com mensagem clara em vez de devolver uma
  # triagem incorreta. Pares com encaixe distante ficam de fora: dois pontos isolados podem
  # ser encaixados no mesmo trecho de estrada e ter distancia zero legitimamente (a critica
  # de snap os descarta depois).
  pares_reais <- dist_linha_reta_km > 0 &
    snap_origem_km <= max_snap_osrm_km & snap_destino_km <= max_snap_osrm_km
  # Pares sem rota voltam NA (null no OSRM) e ficam fora: all() de um vetor vazio é TRUE.
  distancias_reais <- distancia_rodovia_km[pares_reais & !is.na(distancia_rodovia_km)]
  if (length(distancias_reais) > 0 && all(distancias_reais == 0)) {
    stop(
      "osrm::osrmTable() retornou apenas zeros para todos os pares com origem != destino no ",
      "servidor OSRM usado (\"", osrm_server, "\", perfil \"", osrm_profile, "\"). Verifique se ",
      "o perfil e' valido para esse servidor ou tente outro servidor.",
      call. = FALSE
    )
  }

  razao_desvio <- ifelse(
    dist_linha_reta_km > 0, distancia_rodovia_km / dist_linha_reta_km, 1
  )

  rodovia_plausivel <-
    !is.na(tempo_rodovia_h) &
    snap_origem_km <= max_snap_osrm_km &
    snap_destino_km <= max_snap_osrm_km &
    razao_desvio <= max_ratio_desvio

  tibble::tibble(
    par = seq_len(n),
    tempo_rodovia_h = tempo_rodovia_h,
    distancia_rodovia_km = distancia_rodovia_km,
    dist_linha_reta_km = dist_linha_reta_km,
    razao_desvio = razao_desvio,
    snap_origem_km = snap_origem_km,
    snap_destino_km = snap_destino_km,
    candidato_multimodal = !rodovia_plausivel
  )
}

#' Triagem em lote de pares origem-destino via OSRM Table
#'
#' @description
#' Pré-filtro barato para lotes de roteamento: em vez de checar cada par origem-destino com
#' chamadas não vetorizadas (`osrm::osrmNearest()`/`osrm::osrmRoute()`, uma requisição HTTP
#' cada), consulta `osrm::osrmTable()` **uma única vez por bloco** de `max_pares` pares — a
#' API de tabela do OSRM é genuinamente vetorizada — e aplica a crítica que
#' `rotear_multimodal()` usa para descartar uma rota rodoviária implausível (distância de
#' encaixe/*snap* e razão de desvio, ver `max_snap_osrm_km`/`max_ratio_desvio`). O resultado
#' marca, para cada par, se a rota rodoviária já é plausível ou se o par é candidato ao
#' roteamento multimodal completo.
#'
#' Para rotear os pares de fato (inclusive a candidata hidrovia), use
#' \code{\link{rotear_multimodal_lote}}, que usa esta triagem internamente.
#'
#' @param origens,destinos Objetos `sf`/`sfc` POINT, `matrix`/`data.frame` com 2 colunas
#'   (lon, lat), com o mesmo número de pares (1 linha/feature = 1 par origem-destino).
#' @param osrm_server,osrm_profile Ver `\link{rotear_multimodal}`.
#' @param max_snap_osrm_km,max_ratio_desvio Mesmos limiares/padrões usados pela candidata
#'   rodovia de `rotear_multimodal()` — ver `\link{rotear_multimodal}`.
#' @param max_pares Número máximo de pares por chamada; lotes maiores são divididos em
#'   blocos automaticamente. Origens e destinos são consultados juntos numa única chamada
#'   `osrm::osrmTable(loc = ...)` (não `src`/`dst` separados — alguns servidores, incluindo o
#'   público `router.project-osrm.org`, retornam uma matriz de zeros em vez de erro quando
#'   `src`/`dst` são passados como conjuntos separados), o que gera uma matriz `2n x 2n`,
#'   custo O(4n²); o servidor público de demonstração do OSRM não aceita mais de 10000
#'   valores por requisição, daí o padrão de 50 pares (`(2*50)² = 10000`). Com uma instância
#'   própria, aumente `max_pares` de acordo com o `max-table-size` configurado nela.
#' @param max_tries Número máximo de tentativas de cada chamada ao OSRM.
#' @param verbose Se `TRUE` (padrão), exibe mensagens de progresso.
#'
#' @return Um `tibble`, 1 linha por par, com `par` (índice), `tempo_rodovia_h`,
#'   `distancia_rodovia_km` (estimativas do OSRM Table), `dist_linha_reta_km`, `razao_desvio`,
#'   `snap_origem_km`, `snap_destino_km` e `candidato_multimodal` (`TRUE` quando o par não
#'   passou na crítica — sem rota rodoviária, encaixe muito distante, ou desvio implausível —
#'   e portanto é candidato ao roteamento multimodal completo).
#'
#' @export
#' @importFrom sf st_transform st_coordinates st_distance st_geometry st_as_sf
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' triagem_rodovia_lote(
#'   origens = matrix(c(-60.0217, -3.1190, -60.1875, -3.2778), ncol = 2, byrow = TRUE),
#'   destinos = matrix(c(-58.4453, -3.1431, -60.6206, -3.2996), ncol = 2, byrow = TRUE)
#' )
#' }
triagem_rodovia_lote <- function(
    origens,
    destinos,
    osrm_server = "https://router.project-osrm.org/",
    osrm_profile = "driving",
    max_snap_osrm_km = 5.0,
    max_ratio_desvio = 3.0,
    max_pares = 50,
    verbose = TRUE,
    max_tries = 5
) {

  origens <- .padroniza_pontos(origens)
  destinos <- .padroniza_pontos(destinos)

  if (nrow(origens) != nrow(destinos)) {
    stop("`origens` e `destinos` devem ter o mesmo numero de pares.", call. = FALSE)
  }

  n <- nrow(origens)
  origens_4326 <- sf::st_transform(origens, 4326)
  destinos_4326 <- sf::st_transform(destinos, 4326)

  blocos <- split(seq_len(n), ceiling(seq_len(n) / max_pares))

  if (verbose) {
    message(
      "Consultando osrmTable() para ", n, " pares",
      if (length(blocos) > 1) paste0(" em ", length(blocos), " blocos de ate ", max_pares),
      "..."
    )
  }

  partes <- lapply(blocos, function(idx) {
    parte <- .triagem_rodovia_bloco(
      origens_4326[idx, ], destinos_4326[idx, ], osrm_server, osrm_profile,
      max_snap_osrm_km, max_ratio_desvio, max_tries, verbose
    )
    parte$par <- idx
    parte
  })

  do.call(rbind, unname(partes))
}
