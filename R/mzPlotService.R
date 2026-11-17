# File: mzPlotService.R
# Funzioni generalizzate per Plot e Table
# utilizzabili da tutti i modelli di meta-analisi
# ---------------------------------------------------------------

#' Arricchisce il dataset con yi, vi, fit, resid per ordinamento
#'
#' @param dat Dataset processato (dataProc)
#' @param escalcResults Risultati di escalc (con yi, vi)
#' @param subgroupResults Risultati analisi sottogruppi (opzionale)
#' @param modelResults Risultati modello principale
#' @return Dataset arricchito con colonne aggiuntive
mzGetSubgrEnrichedData <- function(dat, escalcResults, subgroupResults = NULL, modelResults = NULL) {
    # 1. Aggiungi yi e vi da escalcResults
    if (!is.null(escalcResults) && nrow(dat) == nrow(escalcResults)) {
        dat$yi <- escalcResults$yi
        dat$vi <- escalcResults$vi
    }

    # 2. Aggiungi fit e resid se necessario per ordinamento
    # Prova a ottenere il modello metareg da subgroupResults
    res <- NULL
    if (!is.null(subgroupResults) && !is.null(subgroupResults$metareg)) {
        res <- subgroupResults$metareg
    } else {
        # Fallback al modello principale
        res <- modelResults
    }

    if (!is.null(res) && !jmvcore::isError(res)) {
        tryCatch(
            {
                # Mappa residui/fitted alle righe corrette
                # L'ordine è preservato se non sono stati eliminati NA
                dat$fit <- fitted(res)
                dat$resid <- residuals(res)
                dat$abs.resid <- abs(dat$resid)

                try(
                    {
                        rstd <- rstandard(res)$z
                        dat$rstandard <- rstd
                        dat$abs.rstandard <- abs(rstd)
                    },
                    silent = TRUE
                )
            },
            error = function(e) {
                dat$fit <- NA
                dat$resid <- NA
                dat$abs.resid <- NA
            }
        )
    } else {
        dat$fit <- NA
        dat$resid <- NA
        dat$abs.resid <- NA
    }

    return(dat)
}


#' Ordina Forest Table con sottogruppo
#'
#' Usa la stessa logica del Forest Plot e inverte l'ordine dei gruppi
#' per compensare il rendering Bottom-Up del plot vs Top-Down della tabella
#'
#' @param studies Dataframe degli studi da ordinare
#' @param dataProc Dataset processato
#' @param escalcResults Risultati escalc
#' @param subgroupResults Risultati sottogruppi
#' @param order_var Variabile di ordinamento (yi, fit, vi, resid, etc.)
#' @return Dataframe studies ordinato
mzSortForestTableWithSubgroup <- function(studies, dataProc, escalcResults, subgroupResults, order_var) {
    # 1. Prepara dataset arricchito (con fit, resid, etc.)
    dat_enriched <- mzGetSubgrEnrichedData(dataProc, escalcResults, subgroupResults)

    # Aggiungi ID originale per tracciare le righe (evita problemi con nomi duplicati)
    dat_enriched$orig_id <- seq_len(nrow(dat_enriched))

    # 2. Calcola ordinamento usando la stessa funzione del Forest Plot
    pos <- mzForestCalcRowsSubgr(
        dat_enriched,
        "subgroup", # Nome variabile sottogruppo interno
        order_var
    )

    dat_ordered <- pos$dat_ordered

    # 3. IMPORTANTE: Inverte l'ordine dei GRUPPI (non degli studi)
    # Il FP usa rendering dal basso verso l'alto (ultimo gruppo in alto)
    # La FT usa visualizzazione dall'alto verso il basso (primo gruppo in alto)
    # Quindi si deve invertire l'ordine dei blocchi di gruppo

    group_levels <- levels(as.factor(dat_ordered$subgroup))
    dat_reordered_list <- list()

    for (lvl in rev(group_levels)) {
        grp_data <- dat_ordered[dat_ordered$subgroup == lvl, ]
        dat_reordered_list[[lvl]] <- grp_data
    }

    dat_ordered <- do.call(rbind, dat_reordered_list)

    # 4. Riordina gli studi per corrispondere a dat_ordered
    # Usa orig_id per l'ordinamento sicuro (evita match su nomi duplicati)
    if ("orig_id" %in% names(dat_ordered)) {
        studies <- studies[dat_ordered$orig_id, ]
    } else {
        # Fallback (non dovrebbe accadere)
        studies$temp_order <- match(studies$Study, dat_ordered$slab)
        studies <- studies[order(studies$temp_order), ]
        studies$temp_order <- NULL
    }

    return(studies)
}

#' Ordina Forest Table senza sottogruppo
#'
#' IMPORTANTE: La logica di ordinamento è DIVERSA rispetto al caso con subgroup
#' Per elementi 1-2 (obs, fit): ordinamento uguale tra FT e FP
#' Per elementi 3-7 (prec, resid, rstandard, abs.resid, abs.rstandard):
#' ordinamento INVERTITO rispetto alla logica con subgroup
#'
#' @param studies Dataframe degli studi da ordinare
#' @param dataProc Dataset processato
#' @param escalcResults Risultati escalc
#' @param modelResults Risultati modello principale
#' @param order_var Variabile di ordinamento
#' @return Dataframe studies ordinato
mzSortForestTableNoSubgroup <- function(studies, dataProc, escalcResults, modelResults, order_var) {
    # Usa mzGetSubgrEnrichedData per ottenere i valori
    dat_enriched <- mzGetSubgrEnrichedData(dataProc, escalcResults, NULL, modelResults)

    if (!is.null(order_var) && order_var %in% names(dat_enriched)) {
        # Allineamento sicuro: assumiamo che studies e dat_enriched siano nello stesso ordine iniziale
        # (dato che provengono dalla stessa pipeline di estrazione)
        # NON usare match() su Study/slab perché fallisce con nomi duplicati

        # Recupera i valori di ordinamento corretti per ogni studio (per posizione)
        studies$sort_val <- dat_enriched[[order_var]]

        # Applica ordinamento:
        # CORREZIONE: Per elementi 3-7, si inverte la logica rispetto al caso con subgroup
        # - yi, fit: ASCENDENTE (valori piccoli in alto) - funziona correttamente
        # - vi, resid, abs.resid, etc.: ASCENDENTE (valori piccoli in alto) - invertito rispetto a prima

        decreasing <- FALSE
        if (order_var %in% c("yi", "fit")) {
            # Elementi 1-2: ASCENDENTE (funziona correttamente)
            decreasing <- FALSE
        } else if (order_var %in% c("vi", "resid", "abs.resid", "rstandard", "abs.rstandard")) {
            # Elementi 3-7: ASCENDENTE (invertito rispetto alla vecchia logica)
            # Prima era decreasing=TRUE, ora è FALSE
            decreasing <- FALSE
        }

        studies <- studies[order(studies$sort_val, decreasing = decreasing), ]
        studies$sort_val <- NULL
    }

    return(studies)
}


#' Calcola posizioni righe per sottogruppi con ordinamento interno
#'
#' @param dat Dataset arricchito
#' @param group_var Nome variabile di raggruppamento
#' @param order_var Variabile di ordinamento (opzionale)
#' @return Lista con rows, text_y, poly_y, labels, rStudy, dat_ordered
mzForestCalcRowsSubgr <- function(dat, group_var, order_var = NULL) {
    # Converti in factor la variabile di raggruppamento mantenendo l'ordine di apparizione
    dat[[group_var]] <- factor(dat[[group_var]], levels = unique(dat[[group_var]]))
    levels_order <- levels(dat[[group_var]])
    nlevel <- length(levels_order)

    # Ordina DENTRO ogni gruppo se order_var è specificato
    if (!is.null(order_var) && order_var %in% names(dat)) {
        # Usa una colonna temporanea per i valori di ordinamento
        dat$temp_sort_val <- NA

        for (grp in levels_order) {
            idx <- which(dat[[group_var]] == grp)
            # Per il plot (Bottom-Up), usiamo l'ordine inverso (Descending) dei valori
            # così i valori più piccoli (es. alta precisione) finiscono in alto (ultime righe processate)
            # NOTA: Invertiamo per tutte le metriche di residuo/varianza per avere i valori più grandi in alto.
            # Per 'yi' (obs) e 'fit', manteniamo l'ordine naturale (Ascendente -> Piccoli in alto).

            force_descending <- c("vi", "resid", "abs.resid", "rstandard", "abs.rstandard")

            if (order_var %in% force_descending) {
                dat$temp_sort_val[idx] <- -dat[[order_var]][idx]
            } else {
                dat$temp_sort_val[idx] <- dat[[order_var]][idx]
            }
        }

        # Ordina per gruppo e poi per valore di ordinamento
        dat <- dat[order(dat[[group_var]], dat$temp_sort_val), ]
        dat$temp_sort_val <- NULL # Rimuovi colonna temporanea
    } else {
        # Ordina solo per gruppo
        dat <- dat[order(dat[[group_var]]), ]
    }

    # Conta studi per livello
    studies_per_level <- as.numeric(table(factor(dat[[group_var]], levels = levels_order)))

    # Inizializza
    rows_list <- list()
    text_y <- numeric(nlevel)
    poly_y <- numeric(nlevel)

    # Calcola rStudy
    # Per ogni sottogruppo: 1 riga header + n studi + 1 riga Modello e diamante + 1 riga spacer
    # L'ultimo gruppo non ha spacer, quindi: total_studies + nlevel * 4 - 1
    total_studies <- sum(studies_per_level)
    rStudy <- total_studies + nlevel * 4 - 1

    # Partenza dall'alto (riga rStudy) e ordine inverso
    current_row <- rStudy

    # Processa gruppi in ordine inverso (ultimo alfabetico va in alto)
    for (i in nlevel:1) {
        n_studies <- studies_per_level[i]

        # Testo sottogruppo (riga più alta del blocco)
        text_y[i] <- current_row

        # Righe per studi (sotto il testo, scendendo)
        rows_list[[i]] <- (current_row - 1):(current_row - n_studies)

        # Poly sotto gli studi
        poly_y[i] <- current_row - n_studies - 1

        # Prossimo gruppo: scendi di n_studies + 4 (header + studi + diamante + stats + spacer)
        # Ma l'ultimo gruppo (i=1) non ha spacer, quindi scende solo di n_studies + 3
        if (i > 1) {
            current_row <- current_row - n_studies - 4
        }
    }

    # Combina rows
    rows <- unlist(rows_list)

    result <- list(
        rows = rows,
        text_y = text_y,
        poly_y = poly_y,
        labels = levels_order,
        rStudy = rStudy,
        dat_ordered = dat
    )

    return(result)
}


#' Aggiorna stato del Forest Plot con logica consapevole della cache
#'
#' Chiamare da .run() DOPO mzUpdateForestPlot() (che gestisce le dimensioni).
#' Triggera la render function in entrambi i cicli (statistico ed estetico)
#' tramite setState(), ma su ciclo estetico la render function trova xlim/ylim
#' fissi nella sizeCache e li usa per mantenere le dimensioni stabili.
#'
#' @param image           Elemento Image di jamovi (results$mzForestPlot).
#' @param sizeCache       Elemento Group con clearWith: [] per le dimensioni.
#' @param model           Oggetto rma/rma.mv principale.
#' @param subgroupResults Lista risultati sottogruppi. NULL se nessun subgroup.
#' @param options         Oggetto self$options.
#' @param rob Giudizi classificati preparati durante .run(), quando data e' disponibile.
#' @noRd
mzUpdateCachedForestPlot <- function(image, sizeCache, model, subgroupResults,
                                     options, rob=NULL, data=NULL) {
  
  # Guard: plot non visibile (binding jamovi su 'visible:' in .r.yaml,
  # cioe' forestPlotMode == "posLorT") o modello non disponibile.
  if (is.null(image) || !isTRUE(image$visible)) {
    return()
  }
  if (is.null(model)) {
    return()
  }
  
  # Costruiamo un "renderState" che include il modello statistico e le opzioni
  # estetiche del grafico. Se uno qualsiasi di questi cambia, identical() fallisce
  # e forza il re-render. Questo risolve il bug dove modifiche a showweights
  # o shadeRows venivano ignorate perche il modello restava identico.
    predictionStyle <- if ("forestPredictionStyle" %in% options$names)
        options$forestPredictionStyle else "line2"
    if (identical(predictionStyle,"line"))predictionStyle <- "line2"
    previousStyle <- if (!is.null(sizeCache))sizeCache$state$predictionStyle else NULL
    if (is.null(previousStyle))previousStyle <- image$state$opts$forestPredictionStyle
    if (is.null(previousStyle))previousStyle <- "line"
    # clearWith puo' avere gia' svuotato Image: conserva lo stile nella size cache.
    # Lo stile cambia il numero di righe, non il fit: invalida solo la geometria.
    if (!identical(previousStyle,predictionStyle) && !is.null(sizeCache)) {
        cache <- sizeCache$state
        if (is.null(cache)) cache <- list()
        # Le dimensioni in pixel servono al ripristino Image: elimina solo gli assi.
        cache[c("xlim", "ylim", "alim", "layoutSig")] <- NULL
        cache$predictionStyle <- predictionStyle
        sizeCache$setState(cache)
    }
    renderState <- list(
        model = model,
        # Input puliti correnti: il fit riutilizzato puo' precedere la scelta Subgroup.
        # Snapshot per il render separato, senza accesso al dataset grezzo.
        data = data,
        # Preparato in .run(), quando il dataset e' disponibile, non nel render Image.
        rob = rob,
        opts  = list(
            subgroup = options$subgroup,
            addpred = options$addpred,
            forestPredictionStyle = predictionStyle,
            forestRobShow = if ('forestRobShow' %in% options$names) options$forestRobShow else NULL,
            robDomains = if ('robDomains' %in% options$names) options$robDomains else NULL,
            addfit = options$addfit,
            showweights = options$showweights,
            forestOrder = options$forestOrder,
            ordSubgr = options$ordSubgr,
            showBackTransform = options$showBackTransform,
            steps = options$steps,
            xAxisTitle = options$xAxisTitle,
            shadeRows = options$shadeRows,
            mainTitle = options$mainTitle,
            pchForest = options$pchForest,
            forestPlotWidth = options$forestPlotWidth,
            forestPlotHeight = options$forestPlotHeight
        )
    )
  
  
  # setState invalida filePath.
  if (!identical(image$state, renderState)) {
    jinfo("[IMG/FOREST] invalidate", .loc = "mzPlotService.R:339")
    image$setState(renderState)
    
  } else {
    jinfo("[IMG/FOREST] reuse", .loc = "mzPlotService.R:343")
  }
}

#' Aggiorna stato del Funnel Plot (Classic) con logica consapevole della cache
#'
#' Analogo a mzUpdateCachedForestPlot ma per il Funnel Plot Classic
#' (type: Image). A differenza del Forest non servono xlim/ylim in cache:
#' le dimensioni sono fisse e gestite a parte da mzUpdateFixedPlot()
#' (mzCacheImage.R). Qui si gestisce solo il trigger del render tramite
#' setState(), con lo stesso guard su image$visible (== funnelPlotMode ==
#' "posLorT" nel .r.yaml) usato dal Forest, per evitare setState()
#' ridondanti quando il plot Classic non e' quello attivo nel tree-state.
#'
#' Chiamare da .run() DOPO mzUpdateFixedPlot() (che gestisce le dimensioni).
#'
#' @param image    Elemento Image di jamovi (results$mzFunnelPlot).
#' @param model    Oggetto rma/rma.mv principale.
#' @param options  Oggetto self$options.
#' @noRd
mzUpdateCachedFunnelPlot <- function(image, model, options) {
  
  # Guard: plot non visibile (binding jamovi su 'visible:' in .r.yaml,
  # cioe' funnelPlotMode == "posLorT") o modello non disponibile.
  if (is.null(image) || !isTRUE(image$visible)) {
    return()
  }
  if (is.null(model)) {
    return()
  }
  
  # Costruiamo un "renderState" che include il modello statistico e le
  # opzioni estetiche del grafico, stessa logica di mzUpdateCachedForestPlot:
  # se una qualsiasi cambia, identical() fallisce e forza il re-render anche
  # quando il modello resta lo stesso oggetto.
  renderState <- list(
    model = model,
    opts  = list(
      yaxis             = options$yaxis,
      yaxisInv          = options$yaxisInv,
      enhanceFunnel     = options$enhanceFunnel,
      showBackTransform = options$showBackTransform,
      modelmea          = options$modelmea,
      funnelPlotWidth   = options$funnelPlotWidth,
      funnelPlotHeight  = options$funnelPlotHeight
    )
  )
  
  
  if (!identical(image$state, renderState)) {
    jinfo("[IMG/FUNNEL] invalidate", .loc = "mzPlotService.R:393")
    image$setState(renderState)
    
  } else {
    jinfo("[IMG/FUNNEL] reuse", .loc = "mzPlotService.R:397")
  }
}

# Statistiche p-curve su test Wald z bilaterali, indipendenti dal rendering.
.mzPcurveStats <- function(yi, vi) {
    if (length(yi) != length(vi) || any(!is.finite(yi)) ||
        any(!is.finite(vi) | vi <= 0))
        stop('P-curve requires finite effects and strictly positive sampling variances.')
    sei <- sqrt(vi)
    k <- length(yi)

    # Z e p-values
    zi <- abs(yi / sei)
    pi <- 2 * pnorm(-zi)
    pi <- pmin(pmax(pi, 2.2e-16), 1 - 2.2e-16)

    ksig <- sum(pi < 0.05, na.rm = TRUE)
    khalf <- sum(pi < 0.025, na.rm = TRUE)

    if (ksig < 3) {
        return()
    }

    # Funzioni helper
    getncp <- function(df, power) {
        xc <- qchisq(0.95, df = df)
        f <- function(ncp) pchisq(xc, df = df, ncp = ncp) - (1 - power)
        uniroot(f, c(0, 1000))$root
    }

    stouffer <- function(pp) {
        pp <- pp[is.finite(pp)]
        if (!length(pp)) return(NA_real_)
        sum(qnorm(pp)) / sqrt(length(pp))
    }

    pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")

    cleanp <- function(p) {
        if (is.na(p)) {
            return("N/A")
        }
        if (p < 0.0001) {
            return("< .0001")
        }
        if (p > 0.9999) {
            return("> .9999")
        }
        paste0("= ", substr(round(p, 4), 2, 6))
    }

    # Calcoli pp-values
    df1 <- rep(1, k)
    value <- zi^2

    ppr <- ifelse(pi < 0.05, 20 * pi, NA)
    ppr <- pmin(pmax(ppr, 2.2e-16), 1 - 2.2e-16)

    ppr_half <- ifelse(pi < 0.025, 40 * pi, NA)
    ppr_half <- pmin(pmax(ppr_half, 2.2e-16), 1 - 2.2e-16)

    ncp33 <- rep(getncp(1, 1 / 3), k)

    pp33 <- ifelse(pi < 0.05, 3 * (pchisq(value, df = df1, ncp = ncp33) - 2 / 3), NA)
    pp33 <- pmin(pmax(pp33, 2.2e-16), 1 - 2.2e-16)

    prop33_025 <- mean(ifelse(pi < 0.05, 1 - pchisq(qchisq(0.975, df = df1), df = df1, ncp = ncp33), NA), na.rm = TRUE)
    # La curva half condiziona una sola volta su p < .025 (correzione ufficiale 4.10).
    prop25 <- prop33_025

    pp33_half <- ifelse(pi < 0.025, (1 / prop25) * (pchisq(value, df = df1, ncp = ncp33) - (1 - prop25)), NA)
    pp33_half <- pmin(pmax(pp33_half, 2.2e-16), 1 - 2.2e-16)

    # Stouffer
    Zppr <- stouffer(ppr)
    Zpp33 <- stouffer(pp33)
    Zppr_half <- stouffer(ppr_half)
    Zpp33_half <- stouffer(pp33_half)

    p_Zppr <- pnorm(Zppr)
    p_Zpp33 <- pnorm(Zpp33)
    p_Zppr_half <- pnorm(Zppr_half)
    p_Zpp33_half <- pnorm(Zpp33_half)

    # Binomial
    prop25_obs <- sum(pi < 0.025) / sum(pi < 0.05)
    binom_r <- 1 - pbinom(prop25_obs * ksig - 1, ksig, 0.5)

    binom_33 <- NA
    if (requireNamespace("poibin", quietly = TRUE)) {
        pp025 <- ifelse(pi < 0.05, 3 * (1 - pchisq(qchisq(0.975, df = df1), df = df1, ncp = ncp33)), NA)
        pp025 <- pp025[!is.na(pp025)]
        if (length(pp025) > 0) {
            binom_33 <- poibin::ppoibin(sum(pi < 0.025), pp025)
        }
    }

    # Power
    powerfit <- function(pwr) {
        ncp_est <- rep(getncp(1, pwr), k)
        pp_est <- ifelse(pi < 0.05, (pchisq(value, df = df1, ncp = ncp_est) - (1 - pwr)) / pwr, NA)
        pp_est <- pmin(pmax(pp_est, 2.2e-16), 1 - 2.2e-16)
        stouffer(pp_est)
    }

    fit <- sapply(5:99 / 100, function(p) abs(powerfit(p)))
    hat <- (which.min(fit) + 4) / 100

    get_pct <- function(pct) {
        z <- qnorm(pct)
        f <- function(pwr) powerfit(pwr) - z
        uniroot(f, c(0.0501, 0.99))$root
    }

    p05 <- pnorm(powerfit(0.051))
    p99 <- pnorm(powerfit(0.99))

    lb <- if (p05 <= 0.95) 0.05 else if (p99 >= 0.95) 0.99 else get_pct(0.95)
    ub <- if (p05 <= 0.05) 0.05 else if (p99 >= 0.05) 0.99 else get_pct(0.05)

    # Curve
    prop33f <- function(pc) mean(ifelse(pi < 0.05, 1 - pchisq(qchisq(1 - pc, df = df1), df = df1, ncp = ncp33), NA), na.rm = TRUE) * 3

    green <- c(
        prop33f(0.01),
        prop33f(0.02) - prop33f(0.01),
        prop33f(0.03) - prop33f(0.02),
        prop33f(0.04) - prop33f(0.03),
        1 - prop33f(0.04)
    ) * 100

    ps <- ceiling(pi[pi < 0.05] * 100) / 100
    blue <- sapply(c(0.01, 0.02, 0.03, 0.04, 0.05), function(i) sum(ps == i) / ksig * 100)

    x <- c(0.01, 0.02, 0.03, 0.04, 0.05)

    list(k=k,
         ksig=ksig,
         khalf=khalf,
         pi=pi,
         ppr=ppr,
         ppr_half=ppr_half,
         pp33=pp33,
         pp33_half=pp33_half,
         p_Zppr=p_Zppr,
         p_Zpp33=p_Zpp33,
         p_Zppr_half=p_Zppr_half,
         p_Zpp33_half=p_Zpp33_half,
         binom_r=binom_r,
         binom_33=binom_33,
         hat=hat,
         lb=lb,
         ub=ub,
         x=x,
         blue=blue,
         green=green,
         pct=pct,
         cleanp=cleanp)
}

# Render separato dai calcoli: un caso non applicabile produce un'immagine esplicativa.
mzPcurvePlot <- function(res, self) {
    if (!inherits(res, c('rma', 'rma.uni'))) return(invisible(NULL))
    stats <- .mzPcurveStats(res$yi, res$vi)
    if (is.null(stats))
        return(mzDiagnosticMessage(.("P-curve requires at least three statistically significant study results (p < .05).")))
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar), add=TRUE)
    with(stats, {
    # PLOT con xlim ottimizzato
    par(mar = c(5.5, 5.5, 1, 1), family = "sans")

    plot(x, blue,
        type = "n", xlim = c(0.008, 0.053), ylim = c(0, 108),
        xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", main = ""
    )

    # Assi L-shaped
    axis(1,
        at = x, labels = c(".01", ".02", ".03", ".04", ".05"),
        lwd = 1.3, lwd.ticks = 1.2, col = "gray20", col.ticks = "gray20", cex.axis = 1.1
    )
    mtext(expression(italic("p") * "-value"), side = 1, line = 3, cex = 1.2, font = 2)

    axis(2,
        at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
        las = 1, lwd = 1.3, lwd.ticks = 1.2, col = "gray20", col.ticks = "gray20", cex.axis = 1.1
    )
    mtext("Percentage of test results", side = 2, line = 3.8, cex = 1.2, font = 2)

    # Linea rossa PUNTEGGIATA con label "20%" tra asse Y e tick .01
    segments(0.01, 20, x[5], 20, col = "#E74C3C", lwd = 2.8, lty = 3)
    text(0.0081, 20, "20%", adj = 0, cex = 0.75, col = "#E74C3C", font = 2)

    lines(x, green, col = "#27AE60", lwd = 3, lty = 2)
    lines(x, blue, col = "#3498DB", lwd = 3.8, lty = 1)
    points(x, blue, pch = 21, bg = "#3498DB", col = "white", cex = 1.5, lwd = 2)

    # PERCENTUALI SU TUTTI I PUNTI
    for (i in 1:5) {
        text(x[i], blue[i] + 5, paste0(round(blue[i]), "%"),
            col = "gray10", cex = 0.95, font = 2
        )
    }

    # LEGENDA
    lx <- 0.018
    ly <- 102
    lh <- 4.2
    ll <- 0.005

    # 1. P-curve osservata
    segments(lx, ly, lx + ll, ly, col = "#3498DB", lwd = 3.8, lty = 1)
    text(lx + ll + 0.001, ly,
        expression(bold("Observed") * " " * bolditalic("p") * bold("-curve")),
        adj = 0, cex = 0.92, col = "gray10"
    )
    text(lx + ll + 0.001, ly - lh,
        sprintf("Power estimate: %s, CI(%s, %s)", pct(hat), pct(lb), pct(ub)),
        adj = 0, cex = 0.8, col = "black"
    )

    # 2. Null of no effect
    y2 <- ly - 2 * lh
    segments(lx, y2, lx + ll, y2, col = "#E74C3C", lwd = 2.8, lty = 3)
    text(lx + ll + 0.001, y2,
        expression(bold("Null of no effect")),
        adj = 0, cex = 0.92, col = "gray10"
    )

    p_half <- if (khalf > 0) cleanp(p_Zppr_half) else "N/A"
    text(lx + ll + 0.001, y2 - lh,
        bquote("Tests for right-skewness: " * italic(p)[Full] * " " * .(cleanp(p_Zppr)) * ", " * italic(p)[Half] * " " * .(p_half)),
        adj = 0, cex = 0.8, col = "black"
    )

    # 3. Null of 33% power
    y3 <- y2 - 2 * lh
    segments(lx, y3, lx + ll, y3, col = "#27AE60", lwd = 3, lty = 2)
    text(lx + ll + 0.001, y3,
        expression(bold("Null of 33% power")),
        adj = 0, cex = 0.92, col = "gray10"
    )

    p_flat_h <- if (khalf > 0) cleanp(p_Zpp33_half) else "> .9999"
    p_binom <- if (!is.na(binom_33)) cleanp(binom_33) else "> .9999"
    text(lx + ll + 0.001, y3 - lh,
        bquote("Tests for flatness: " * italic(p)[Full] * " " * .(cleanp(p_Zpp33)) * ", " * italic(p)[half] * " " * .(p_flat_h) * ", " * italic(p)[Binomial] * " " * .(p_binom)),
        adj = 0, cex = 0.8, col = "black"
    )

    # Note in basso - più compatte e professionali
    note <- bquote("Note: The observed " * italic(p) * "-curve includes " * .(ksig) *
        " statistically significant (" * italic(p) * " < .05) results, of which " *
        .(khalf) * " are " * italic(p) * " < .025.")
    mtext(note, side = 1, line = 3.8, cex = 0.72, adj = 0, col = "gray25")

    kns <- k - ksig
    if (kns == 0) {
        ns <- "There were no non-significant results entered."
    } else if (kns == 1) {
        ns <- bquote("There was one additional result entered but excluded from " *
            italic(p) * "-curve because it was " * italic(p) * " > .05.")
    } else {
        ns <- bquote("There were " * .(kns) * " additional results entered but excluded from " *
            italic(p) * "-curve because they were " * italic(p) * " > .05.")
    }
    mtext(ns, side = 1, line = 4.5, cex = 0.72, adj = 0, col = "gray25")
    })
    invisible(TRUE)
}

# In base al nome del pch passato recupera il suo valore numerico per il forest
mzGetPchValue <- function(pchName) {
    map <- c(
        eSquare   = 0,
        eCircle   = 1,
        eTriangle = 2,
        eDiamond  = 5,
        star      = 8,
        bSquare   = 15,
        bCircle   = 16,
        bTriangle = 17,
        bDiamond  = 18)
    
    map[[pchName]] %||% 15
}

# Riferimenti descrittivi sulla scala di analisi, non stime aggiustate.
.mzClassicForestReferences <- function(fit) {
    if (isTRUE(fit$int.only)) return(NULL)
    keep <- is.finite(fit$yi) & is.finite(fit$vi) & fit$vi > 0
    yi <- fit$yi[keep]; vi <- fit$vi[keep]
    if (!length(yi)) return(NULL)
    # Pesi proporzionali a 1/vi, normalizzati per evitare overflow.
    w <- min(vi) / vi
    centre <- sum(w * yi) / sum(w)
    spread <- sqrt(sum(w * (yi - centre)^2) / sum(w))
    list(values=c(centre-spread,centre,centre+spread),
         colours=c('#98A2B3','#3E6DA9','#98A2B3'),
         notes=c(
           'Blue: inverse-variance weighted mean; grey: mean +/- weighted SD (effects on the analysis scale).',
           'Descriptive only: not moderator-adjusted effects, confidence limits or prediction limits.'))
}

# mzForestPlotAll - Forest Plot Generalizzato con Sottogruppi
#
# Funzione per creare forest plot semplici o con sottogruppi dinamici
# Compatibile con tutti i modelli di meta-analisi (correlation, dichotomous, etc.)

mzForestPlotAll <- function(plotData, self = NULL,
                            subgroupVar = NULL, subgroupVarName = NULL, subgroupFont = 4, cex = 0.85,
                            ilab = NULL, ilabLab = NULL, ilabStat = NULL, ilab.xpos = NULL,
                            xlim = NULL, ylim = NULL, at = NULL, alim = NULL, olim = NULL,
                            atransf = NULL, targs = NULL,
                            header = "Study",
                            topHeader = NULL,
                            topHeaderXpos = NULL,
                            subTitMain = NULL,
                            spaceTop = 4,
                            spaceBottom = 3,
                            spaceGap = 1,
                            spaceLabel = 1,
                            spaceDiamond = 1,
                            rowHT = -2.0,
                            spaceHetero = NULL,
                            diamondLabel = NULL,
                            topLabel = NULL,
                            reverseOrder = FALSE,
                            orderWithin = NULL,
                            colshade = NULL,
                            xlimLine = FALSE,
                            rob = NULL,
                            ...) {
    # Un coefficiente di regressione, anche unico e senza intercetta, non è un pooled globale.
    hasGlobalSummary <- isTRUE(plotData$int.only)
    # Opzione condivisa: gli stili estesi restano una presentazione del fit nativo.
    predictionStyle <- "line"
    if (!is.null(self) && "forestPredictionStyle" %in% self$options$names)
        predictionStyle <- self$options$forestPredictionStyle
    if (!is.null(self) && identical(predictionStyle,"line"))predictionStyle <- "line2"
    if (!predictionStyle %in% c("line","line2","diamond","bar","shade","dist"))
        stop("Unknown Classic prediction style.",call.=FALSE)
    predictionEnabled <- hasGlobalSummary && plotData$k >= 2L &&
        !mzEstimatorId(plotData) %in% c("FE","EE","CE") && !is.null(self) &&
        isTRUE(self$options$addpred) && isTRUE(self$options$addfit)
    extendedPrediction <- predictionEnabled && predictionStyle != "line"
    if (extendedPrediction) {
        # Una riga predittiva separata, piu' spazio per densita' e annotazioni.
        spaceBottom <- max(spaceBottom,5)
        spaceDiamond <- max(spaceDiamond,3)
        rowHT <- min(rowHT,-4)
    }
    references <- .mzClassicForestReferences(plotData)
    footerLayout <- NULL
    # Il disegno nativo puo' avvenire dopo il rilascio del dataset: usa solo lo stato.
    robLayout <- if(is.null(rob))NULL else .mzClassicRobLayout(self$options)
    robGeometry <- NULL
    restoreMargins <- graphics::par('mar')
    originalMargins <- restoreMargins
    if (!is.null(robLayout)) {
        lineIn <- graphics::par('cin')[2]*graphics::par('mex')
        originalMargins[4] <- originalMargins[4]+robLayout$widthIn/lineIn
        originalMargins[1] <- originalMargins[1]+robLayout$heightIn/lineIn
        graphics::par(mar=originalMargins)
    }
    if (!hasGlobalSummary || !is.null(robLayout))
        on.exit(graphics::par(mar=restoreMargins),add=TRUE)
    footerPlan <- NULL
    groupHeaderShading <- NULL
    # Larghezze fisiche: non riduce il corpo per fare entrare le annotazioni.
    wrapMeasured <- function(text,width,font=1) {
        lines <- character()
        for (paragraph in text) {
            words <- strsplit(paragraph,' ',fixed=TRUE)[[1]]
            line <- ''
            for (word in words) {
                candidate <- if(nzchar(line))paste(line,word) else word
                if(nzchar(line) && graphics::strwidth(candidate,units='inches',cex=cex,font=font)>width) {
                    lines <- c(lines,line);line <- word
                } else line <- candidate
            }
            if(nzchar(line))lines <- c(lines,line)
        }
        lines
    }
    # Prefisso in grassetto e continuazioni allineate al testo, con misura mista.
    wrapReferenceNotes <- function(width) {
        out <- list(prefix=character(),body=character(),indent=numeric())
        if(is.null(references))return(out)
        for(paragraph in references$notes) {
            prefix <- if(startsWith(paragraph,'Blue:'))'Blue:' else
                if(startsWith(paragraph,'Descriptive only:'))'Descriptive only:' else ''
            body <- if(nzchar(prefix))trimws(substring(paragraph,nchar(prefix)+1L)) else paragraph
            indent <- if(nzchar(prefix))graphics::strwidth(prefix,units='inches',cex=cex,font=2)+
                graphics::strwidth(' ',units='inches',cex=cex,font=1) else 0
            lines <- wrapMeasured(body,width-indent)
            out$prefix <- c(out$prefix,if(length(lines))c(prefix,rep('',length(lines)-1L))else character())
            out$body <- c(out$body,lines)
            out$indent <- c(out$indent,rep(indent,length(lines)))
        }
        out
    }
    prepareReferences <- function(args) {
        if (hasGlobalSummary) {
            # Una cache proveniente dal modello con moderatori non taglia il pooled.
            args$ylim[1] <- min(args$ylim[1],-spaceBottom,rowHT-.5)
            return(args)
        }
        args$refline <- NA_real_
        args$ylim[1] <- -.25
        lineIn <- graphics::par('cin')[2]*graphics::par('mex')
        effectiveMargins <- pmax(originalMargins-c(0,3,1,1),0)
        widthIn <- graphics::par('fin')[1]-sum(effectiveMargins[c(2,4)])*lineIn
        gapIn <- graphics::strheight('Mg',units='inches',cex=cex)*.65
        textHeight <- max(graphics::strheight('Mg',units='inches',cex=cex,font=1),
                          graphics::strheight('Mg',units='inches',cex=cex,font=2))
        leading <- max(textHeight*1.45,.12)
        notes <- wrapReferenceNotes(widthIn-.15)$body
        # Stima prudente per il margine; il disegno usa poi le misure effettive.
        stats <- sprintf('tau2=%.3f; I2=%.1f%%; Q(df=%d)=%.2f; p=%.4f',
            plotData$tau2,plotData$I2,plotData$k-plotData$p,plotData$QE,plotData$QEp)
        statsLines <- length(wrapMeasured(stats,max(.6,widthIn*.20)))+
            length(wrapMeasured('Residual heterogeneity',max(.6,widthIn*.20),2))
        labelCex <- if(is.null(args$cex.lab))cex else args$cex.lab
        axisCex <- if(is.null(args$cex.axis))cex else args$cex.axis
        # Usa il generatore di etichette di metafor5.3-4, senza duplicare la sua mappa.
        axisTitle <- if(is.null(args$xlab))getFromNamespace('.setlab','metafor')(
            plotData$measure,deparse(if(is.null(args$transf))FALSE else args$transf),
            deparse(if(is.null(args$atransf))FALSE else args$atransf),gentype=1) else args$xlab
        titleFonts <- rep_len(if(is.null(args$xlabfont))1L else args$xlabfont,length(axisTitle))
        labelHeight <- max(graphics::strheight(axisTitle,units='inches',cex=labelCex,font=titleFonts))
        axisDepth <- max((graphics::par('mgp')[1]-.5)*lineIn+labelHeight,
                         graphics::par('mgp')[2]*lineIn+
                         graphics::strheight('Mg',units='inches',cex=axisCex))+.25*lineIn
        reserve <- max(axisDepth,statsLines*leading)+gapIn+length(notes)*leading+.15
        margins <- originalMargins;margins[1] <- reserve/lineIn+
            if(is.null(robLayout))0 else robLayout$heightIn/lineIn
        graphics::par(mar=margins)
        footerPlan <<- list(lineIn=lineIn,leading=leading,height=textHeight,
            axisDepth=axisDepth,gap=gapIn,axisCex=axisCex,title=axisTitle,titleFonts=titleFonts,
            titleAdj=args$xlabadj)
        # Il titolo viene disegnato una sola volta, nel punto medio misurato.
        args$xlab <- ''
        args
    }
    callForest <- function(args) {
        # metafor ripristina mar all'uscita: riprende qui la regione del suo disegno.
        actualMargins <- pmax(graphics::par('mar')-c(0,3,1,1),0)
        if (extendedPrediction) {
            args$predstyle <- predictionStyle
            # La densita' nativa occupa quasi una riga: lascia respiro al diamante.
            # Il quarto efac cambia solo l'altezza del disegno predittivo.
            if (identical(predictionStyle,"dist")) {
                efac <- args$efac
                if (is.null(efac)) efac <- rep(1,4)
                if (length(efac)==1L) efac <- rep(efac,4)
                if (length(efac)==2L) efac <- efac[c(1,1,2,2)]
                if (length(efac)==3L) efac <- efac[c(1:3,3)]
                efac[4] <- efac[4] * .65
                args$efac <- efac
            }
            # Le dimensioni memorizzate con Line non possono tagliare le nuove righe.
            args$ylim[1] <- min(args$ylim[1],-spaceBottom)
            if (!is.null(subgroupVar))args$ylim[2] <- max(args$ylim[2],totRow+spaceTop)
        }
        # Colore nativo esplicito: l'estensione ROB usa lo stesso sfondo del forest.
        if (!is.null(args$shade) && is.null(args$colshade))
            args$colshade <- getFromNamespace('.coladj','metafor')(
                graphics::par(c('bg','fg')),dark=.1,light=-.1)
        result <- do.call(forest,args)
        result$mzShading <- .mzClassicForestShading(plotData,args,result)
        result$mzHeaderShading <- groupHeaderShading
        if (!is.null(groupHeaderShading))
            graphics::rect(result$xlim[1],groupHeaderShading$rows-.5,
                result$xlim[2],groupHeaderShading$rows+.5,
                col=groupHeaderShading$col,border=NA)

        if(!hasGlobalSummary)graphics::par(mar=actualMargins)
        result
    }
    drawReferences <- function(tmp) {
        if (is.null(references)) return(invisible(NULL))
        order <- c(1L,3L,2L)
        graphics::segments(x0=references$values[order],x1=references$values[order],
            y0=0,y1=tmp$ylim[2]-(spaceTop-1),col=references$colours[order],
            lty='dotted',lwd=c(.8,.8,1.4))
    }
    drawModeratorFooter <- function(tmp,summary) {
        x <- min(tmp$textpos)+graphics::strwidth('M',cex=cex)*.5
        left <- graphics::grconvertX(x,'user','inches')
        bottom <- graphics::grconvertY(graphics::par('usr')[3],'user','inches')
        top <- graphics::grconvertY(0,'user','inches')
        # Riserva anche la meta' sinistra delle etichette delle tacche.
        tickValues <- tmp$at
        if(is.function(atransf))tickValues <- if(is.null(targs))sapply(tickValues,atransf) else sapply(tickValues,atransf,targs)
        tickLabels <- format(tickValues,digits=8,trim=TRUE)
        halfTick <- max(graphics::strwidth(tickLabels,units='inches',cex=footerPlan$axisCex))*.5
        axisLeft <- graphics::grconvertX(min(tmp$at),'user','inches')-halfTick
        right <- axisLeft-footerPlan$gap
        width <- right-left
        if(width<=graphics::strwidth('heterogeneity',units='inches',cex=cex))
            stop('The Classic forest is too narrow for readable residual heterogeneity; increase the figure width.',call.=FALSE)
        header <- wrapMeasured('Residual heterogeneity',width,2)
        stats <- wrapMeasured(sub('^Overall: *','',summary),width)
        labels <- c(header,stats)
        fonts <- c(rep(2L,length(header)),rep(1L,length(stats)))
        centres <- top-footerPlan$height/2-(seq_along(labels)-1)*footerPlan$leading
        graphics::text(x=x,y=graphics::grconvertY(centres,'inches','user'),labels=labels,
            adj=c(0,.5),cex=cex,col='#111111',font=fonts,xpd=TRUE)
        residualBottom <- min(centres)-footerPlan$height/2
        richNotes <- wrapReferenceNotes(graphics::par('pin')[1]-
            (left-graphics::grconvertX(graphics::par('usr')[1],'user','inches'))-.08)
        notes <- ifelse(nzchar(richNotes$prefix),paste(richNotes$prefix,richNotes$body),richNotes$body)
        noteTop <- min(bottom-footerPlan$axisDepth,residualBottom)-footerPlan$gap
        # Centro verticale nello spazio libero dopo le etichette delle tacche.
        tickBottom <- bottom-(graphics::par('mgp')[2]+.35)*footerPlan$lineIn-
            graphics::strheight('Mg',units='inches',cex=tmp$cex.axis)/2
        titleHeight <- max(graphics::strheight(footerPlan$title,units='inches',cex=tmp$cex.lab,
            font=footerPlan$titleFonts))
        titleCentre <- (tickBottom+noteTop)/2
        if(tickBottom-noteTop<=titleHeight)
            stop('The Classic forest needs more space for its axis title and notes; increase the figure height.',call.=FALSE)
        titleX <- if(length(footerPlan$title)==1L)mean(range(tmp$at)) else
            if(length(footerPlan$title)==2L)range(tmp$at) else c(min(tmp$at),mean(range(tmp$at)),max(tmp$at))
        titleAdj <- if(is.null(footerPlan$titleAdj))rep(.5,length(titleX)) else
            rep_len(footerPlan$titleAdj,length(titleX))
        titleAdj[is.na(titleAdj)] <- .5
        for(i in seq_along(titleX))graphics::text(x=titleX[i],
            y=graphics::grconvertY(titleCentre,'inches','user'),labels=footerPlan$title[i],
            adj=c(titleAdj[i],.5),cex=tmp$cex.lab,font=footerPlan$titleFonts[i],xpd=TRUE)
        noteCentres <- noteTop-footerPlan$height/2-(seq_along(notes)-1)*footerPlan$leading
        noteBottom <- if(length(notes))min(noteCentres)-footerPlan$height/2 else noteTop
        figureBottom <- graphics::par('fig')[3]*grDevices::dev.size('in')[2]
        if(noteBottom<figureBottom+.05)
            stop('The Classic forest is too small for readable footer notes; increase the figure width or height.',call.=FALSE)
        if(length(notes)) {
            first <- nzchar(richNotes$prefix)
            graphics::text(x=x,y=graphics::grconvertY(noteCentres[first],'inches','user'),
                labels=richNotes$prefix[first],adj=c(0,.5),cex=cex,col='#111111',font=2,xpd=TRUE)
            graphics::text(x=graphics::grconvertX(left+richNotes$indent,'inches','user'),
                y=graphics::grconvertY(noteCentres,'inches','user'),labels=richNotes$body,
                adj=c(0,.5),cex=cex,col='#111111',font=1,xpd=TRUE)
        }
        widths <- vapply(seq_along(labels),function(i)graphics::strwidth(labels[i],units='inches',cex=cex,font=fonts[i]),numeric(1))
        footerLayout <<- list(heterogeneity=list(label='Residual heterogeneity',row=0,left=left,
            right=left+max(widths),top=top,bottom=residualBottom),
            axis=list(left=axisLeft,bottom=titleCentre-titleHeight/2,tickBottom=tickBottom),
            title=list(text=footerPlan$title,centre=titleCentre,height=titleHeight,
                gapAbove=tickBottom-(titleCentre+titleHeight/2),
                gapBelow=(titleCentre-titleHeight/2)-noteTop),
            notes=list(top=noteTop,bottom=noteBottom,lines=notes,cex=cex),figureBottom=figureBottom)
        invisible(NULL)
    }
    # 1A. Helper per disegnare header superiori con linee di separazione
    drawTopHeaders <- function(topHeader, topHeaderXpos, ilab.xpos, totRow, cex) {
        if (!is.null(topHeader) && !is.null(topHeaderXpos)) {
            text(
                x = topHeaderXpos, y = totRow + 3,
                labels = topHeader, cex = cex, font = 2, xpd = TRUE
            )

            # Linee di separazione tra gruppi di colonne
            if (!is.null(ilab.xpos)) {
                nTop <- length(topHeader)
                nIlab <- length(ilab.xpos)
                colsPerHead <- nIlab / nTop

                if (colsPerHead == as.integer(colsPerHead)) {
                    # Calcolo gap minimo tra gruppi adiacenti per determinare segOffset dinamico
                    minGap <- Inf
                    if (nTop > 1) {
                        for (i in 1:(nTop - 1)) {
                            idxCurrent <- ((i - 1) * colsPerHead + 1):(i * colsPerHead)
                            idxNext <- (i * colsPerHead + 1):((i + 1) * colsPerHead)

                            # Distanza tra fine del corrente e inizio del successivo
                            currentEnd <- max(ilab.xpos[idxCurrent])
                            nextStart <- min(ilab.xpos[idxNext])
                            gap <- nextStart - currentEnd
                            if (gap < minGap) minGap <- gap
                        }
                    }

                    # Definisci offset segmento: metà del gap meno un margine, con limiti di sicurezza
                    if (is.infinite(minGap)) {
                        segOffset <- 0.5 # Default se un solo gruppo
                    } else {
                        segOffset <- (minGap / 2) - 0.1
                        # Limita a range ragionevole [0.1, 0.5]
                        if (segOffset < 0.1) segOffset <- 0.1
                        if (segOffset > 0.5) segOffset <- 0.5
                    }

                    for (i in 1:nTop) {
                        idx <- ((i - 1) * colsPerHead + 1):(i * colsPerHead)
                        x0Seg <- min(ilab.xpos[idx]) - segOffset
                        x1Seg <- max(ilab.xpos[idx]) + segOffset
                        segments(x0 = x0Seg, y0 = totRow + 2.5, x1 = x1Seg, y1 = totRow + 2.5, lwd = 1.2, xpd = TRUE)
                    }
                }
            }
        }
    }

    # 1B. Helper per disegnare righine di separazione sopra le statistiche summary dei sottogruppi
    # (Stesso stile segmented dei topHeaders)
    drawSummarySeparators <- function(topHeader, ilab.xpos, rowY, cex, nIlabCols = NULL, xlimLine, xlim) {
        # Se xlimLine è TRUE, disegna una linea tratteggiata da xlim[1] a xlim[2]
        if (isTRUE(xlimLine)) {
            segments(
                x0 = xlim[1], y0 = rowY,
                x1 = xlim[2], y1 = rowY,
                lwd = 0.8, lty = 2, col = "darkblue"
            )
            return()
        }

        if (!is.null(ilab.xpos) && length(ilab.xpos) > 0) {
            # Calcola segOffset dinamicamente basandosi sulla spaziatura tra colonne
            # ilab.xpos rappresenta il CENTRO delle colonne
            # La metà della distanza tra due colonne copre: metà col iniziale + metà col finale
            # (2 col = 1 gap, 3 col = 2 gap, etc.)

            nCols <- length(ilab.xpos)

            if (nCols == 1) {
                # Una sola colonna: usa un offset fisso piccolo
                segOffset <- 0.1
            } else {
                # Ordina le posizioni e calcola la spaziatura tra colonne consecutive
                sortedPos <- sort(ilab.xpos)
                colSpacing <- sortedPos[2] - sortedPos[1] # Spaziatura tra centri colonne

                # Usa la METÀ della spaziatura come offset
                # Questo copre esattamente metà prima colonna + metà ultima colonna
                segOffset <- abs(colSpacing) / 2

                # Limiti di sicurezza: minimo 0.02, massimo 0.5
                if (segOffset < 0.02) segOffset <- 0.02
                if (segOffset > 0.5) segOffset <- 0.5
            }

            if (!is.null(topHeader)) {
                # Con topHeader: segmenti raggruppati
                nTop <- length(topHeader)
                nIlab <- length(ilab.xpos)
                colsPerHead <- nIlab / nTop
                if (colsPerHead == as.integer(colsPerHead)) {
                    minGap <- Inf
                    if (nTop > 1) {
                        for (h in 1:(nTop - 1)) {
                            idxC <- ((h - 1) * colsPerHead + 1):(h * colsPerHead)
                            idxN <- (h * colsPerHead + 1):((h + 1) * colsPerHead)
                            gap <- min(ilab.xpos[idxN]) - max(ilab.xpos[idxC])
                            if (gap < minGap) minGap <- gap
                        }
                    }
                    if (!is.infinite(minGap)) {
                        segOffset <- (minGap / 2) - 0.1
                        if (segOffset < 0.1) segOffset <- 0.1
                        if (segOffset > 0.5) segOffset <- 0.5
                    }
                    for (h in 1:nTop) {
                        idxH <- ((h - 1) * colsPerHead + 1):(h * colsPerHead)
                        segments(
                            x0 = min(ilab.xpos[idxH]) - segOffset, y0 = rowY,
                            x1 = max(ilab.xpos[idxH]) + segOffset, y1 = rowY,
                            lwd = 0.8, col = "darkblue"
                        )
                    }
                    return()
                }
            }
            # Senza topHeader, UN SOLO segmento da prima a ultima colonna ilab
            # Usa segOffset calcolato dinamicamente
            segments(
                x0 = min(ilab.xpos) - segOffset, y0 = rowY,
                x1 = max(ilab.xpos) + segOffset, y1 = rowY,
                lwd = 0.8, col = "darkblue"
            )
        }
    }

    # 2. Forest Plot Semplice (senza sottogruppi)
    if (is.null(subgroupVar)) {
        totRowSimple <- plotData$k
        ylimSimple <- c(-spaceBottom, totRowSimple + spaceTop)

        # Costruzione argomenti forest()
        forestArgs <- list(
            x = plotData, top = spaceTop,
            cex = cex, header = header
        )
        
        # Applica ylim dalla cache se fornito, altrimenti usa ylim empirico
        if (!is.null(ylim)) {
            forestArgs$ylim <- ylim
        } else {
            forestArgs$ylim <- ylimSimple
        }

        # Gestione shade per forest plot semplice
        if (!is.null(self) && isTRUE(self$options$shadeRows)) {
            forestArgs$shade <- TRUE
        }

        if (!is.null(xlim)) forestArgs$xlim <- xlim
        if (!is.null(at)) forestArgs$at <- at
        if (!is.null(alim)) forestArgs$alim <- alim
        if (!is.null(olim)) forestArgs$olim <- olim
        if (!is.null(atransf)) forestArgs$atransf <- atransf
        if (!is.null(targs)) forestArgs$targs <- targs
        if (!is.null(orderWithin)) forestArgs$order <- orderWithin

        # Aggiunta colonne extra con ilab
        if (!is.null(ilab)) {
            forestArgs$ilab <- ilab
            if (!is.null(ilabLab)) forestArgs$ilab.lab <- ilabLab
            if (!is.null(ilab.xpos)) forestArgs$ilab.xpos <- ilab.xpos
        }

        forestArgs <- c(forestArgs, list(...))
        if (identical(mzEstimatorId(plotData), "SJ2") && is.null(forestArgs$mlab)) forestArgs$mlab <- "RE Model [SJ2]"
        if (!hasGlobalSummary) {
            forestArgs$addfit <- FALSE
            forestArgs$addpred <- FALSE
        }
        forestArgs <- prepareReferences(forestArgs)
        tmp <- callForest(forestArgs)
        drawReferences(tmp)

        # Se ilab.xpos non era fornito ma ilab sì, recuperalo dal plot

        # Se ilab.xpos non era fornito ma ilab sì, recuperalo dal plot
        if (is.null(ilab.xpos) && !is.null(ilab) && !is.null(tmp$ilab.xpos)) {
            ilab.xpos <- tmp$ilab.xpos
        }

        # Se topHeader è fornito ma topHeaderXpos è NULL, calcolalo automaticamente
        if (!is.null(topHeader) && is.null(topHeaderXpos) && !is.null(ilab.xpos)) {
            nTop <- length(topHeader)
            nIlab <- length(ilab.xpos)
            colsPerHead <- nIlab / nTop
            if (colsPerHead == as.integer(colsPerHead)) {
                topHeaderXpos <- numeric(nTop)
                for (i in 1:nTop) {
                    idx <- ((i - 1) * colsPerHead + 1):(i * colsPerHead)
                    topHeaderXpos[i] <- mean(ilab.xpos[idx])
                }
            }
        }


        # Header superiori
        drawTopHeaders(topHeader, topHeaderXpos, ilab.xpos, totRowSimple, cex)

        # Sottotitolo principale (subTitMain)
        if (!is.null(subTitMain)) {
            text(
                x = mean(tmp$xlim), y = totRowSimple + spaceTop + 0.2,
                labels = subTitMain, cex = cex * 0.9, font = 4, xpd = TRUE
            )
        }

        # Determina se disegnare il diamante overall (e quindi le statistiche totali)
        shouldDrawDiamondOverall <- hasGlobalSummary
        # Cerca addfit in diversi posti
        if (hasGlobalSummary && !is.null(self) && !is.null(self$options$addfit)) {
            shouldDrawDiamondOverall <- isTRUE(self$options$addfit)
        } else if (hasGlobalSummary && !is.null(list(...)$addfit)) {
            shouldDrawDiamondOverall <- isTRUE(list(...)$addfit)
        }


        # Statistiche per colonne extra di ilab (totali/somme in riga -1) - SOLO SE disegniamo il modello
        if (shouldDrawDiamondOverall && !is.null(ilabStat) && !is.null(ilab) && !is.null(ilab.xpos)) {
            ilabMat <- as.matrix(ilab)
            for (cIdx in seq_along(ilabStat)) {
                valToPrint <- NULL
                statItem <- ilabStat[[cIdx]]

                # FIX: Salta se FALSE
                if (identical(statItem, FALSE)) {
                    next
                }

                if (is.function(statItem)) {
                    vals <- suppressWarnings(as.numeric(ilabMat[, cIdx]))
                    if (!all(is.na(vals))) {
                        valToPrint <- statItem(vals, na.rm = TRUE)
                    }
                } else if (is.list(statItem) && !is.null(statItem$overall)) {
                    # Supporto struttura custom {overall: val, groups: c(val, val)}
                    valToPrint <- statItem$overall
                } else if (is.vector(statItem) && length(statItem) == 1) {
                    # Valore scalare diretto
                    valToPrint <- statItem
                }

                if (!is.null(valToPrint)) {
                    # Formattazione intelligente
                    if (is.numeric(valToPrint)) {
                        # Se intero (es. somma N), 0 decimali. Se float (es. mean/sd), 2 decimali.
                        if (valToPrint %% 1 == 0) {
                            label <- formatC(valToPrint, format = "f", digits = 0)
                        } else {
                            label <- formatC(valToPrint, format = "f", digits = 2)
                        }
                    } else {
                        label <- valToPrint
                    }

                    text(
                        x = ilab.xpos[cIdx], y = -1, labels = label,
                        cex = cex * 0.75, font = 4
                    )
                }
            }
        }

        # Statistiche eterogeneità overall (riga -2.0)
        # Formato p-value
        pText <- if (plotData$QEp < 0.0001) "p < 0.0001" else paste0("p=", formatC(plotData$QEp, digits = 3, format = "f"))

        # Costruisci testo: tau² solo se non FE
        if (plotData$method == "FE") {
            hText <- paste0(
                "Overall: I\u00b2=", formatC(plotData$I2, digits = 1, format = "f"), "%; ",
                "Q(df=", plotData$k - plotData$p, ")=", formatC(plotData$QE, digits = 2, format = "f"), ", ", pText
            )
        } else {
            hText <- paste0(
                "Overall: tau\u00b2=", formatC(plotData$tau2, digits = 3, format = "f"), "; ",
                "I\u00b2=", formatC(plotData$I2, digits = 1, format = "f"), "%; ",
                "Q(df=", plotData$k - plotData$p, ")=", formatC(plotData$QE, digits = 2, format = "f"), ", ", pText
            )
        }
        # Posizione x per testo a sinistra (fix per testo tagliato: allineamento agli studi)
        # Usiamo tmp$textpos per allinearci esattamente all'inizio del testo degli studi.
        # Questa è la logica che funziona con xlim manuali e dovrebbe funzionare sempre.
        if (!is.null(tmp$textpos)) {
            if (is.matrix(tmp$textpos)) {
                xPos <- min(tmp$textpos[, 1], na.rm = TRUE)
            } else {
                xPos <- min(tmp$textpos, na.rm = TRUE)
            }
        } else if (!is.null(tmp$xlim)) {
            xPos <- tmp$xlim[1]
        } else {
            xPos <- xlim[1]
        }

        # pos=4: scrive a destra della coordinata x (offset standard)
        # xpd=TRUE: disabilita il clipping alla plot region, permettendo al testo di essere visualizzato
        #           anche se topHeader (che usa xpd=TRUE) ha alterato i margini percepiti o il clipping.
        if(hasGlobalSummary)text(x = xPos, y = rowHT, labels = hText, cex = cex * 0.85, pos = 4, font = 4, xpd = TRUE)
        else drawModeratorFooter(tmp,hText)

        # Con moderatori non aggiunge una riga Summary priva di effetto globale.

        robGeometry <- .mzDrawClassicRob(rob,robLayout,plotData,forestArgs,tmp,cex,spaceTop,
            overallY=if(hasGlobalSummary)rowHT else NULL,footer=footerLayout)
        return(invisible(list(tmp = tmp,footer=footerLayout,rob=robGeometry)))
    }

    ## -------------- 3. Forest Plot con Sottogruppi -------------- ##

    # Determina spaceHetero automaticamente (RE=1, FE=0)
    if (is.null(spaceHetero)) {
        reMethods <- c("DL", "HE", "HS", "SJ", "SJ2", "ML", "REML", "EB", "PM", "FE")
        spaceHetero <- if (plotData$method %in% reMethods) 1 else 0
    }

    # Estrazione nome e livelli variabile sottogruppo
    if (!is.null(self) && !is.null(self$options$subgroup)) {
        # Priorità: estrae da self$options$subgroup se disponibile
        subVarName <- self$options$subgroup
    } else if (!is.null(subgroupVarName)) {
        # Usa il nome esplicito se fornito
        subVarName <- subgroupVarName
    } else {
        # Fallback: estrae dal parametro (può restituire "subgroupVar")
        subVarName <- sub("^.*\\$", "", deparse(substitute(subgroupVar)))
    }
    subVarData <- subgroupVar

    subLevels <- if (is.factor(subVarData)) {
        levels(droplevels(subVarData))
    } else {
        sort(unique(subVarData))
    }

    nSub <- length(subLevels)
    # La tabella conserva gia' i fit dei sottogruppi, indipendenti dal disegno.
    cachedSubgroups <- if (is.null(self)) NULL else self$subgroupResults
    studiesPerGr <- vapply(subLevels, function(lv) sum(subVarData == lv), integer(1))

    # Ordinamento: subgruppo + eventuale ordinamento interno
    subOrder <- as.numeric(factor(subVarData, levels = subLevels))

    if (!is.null(orderWithin)) {
        withinOrder <- numeric(plotData$k)
        for (i in seq_len(nSub)) {
            idx <- which(subVarData == subLevels[i])
            vals <- switch(orderWithin,
                "prec" = 1 / plotData$vi[idx],
                "obs" = plotData$yi[idx],
                "fit" = fitted(plotData)[idx],
                "resid" = residuals(plotData)[idx],
                "rstandard" = rstandard(plotData)$z[idx],
                "abs.resid" = abs(residuals(plotData)[idx]),
                "abs.rstandard" = abs(rstandard(plotData)$z[idx])
            )
            withinOrder[idx] <- rank(vals, ties.method = "first")
        }
        orderVar <- subOrder * 1000 + withinOrder
    } else {
        orderVar <- subOrder
    }

    # Calcolo righe totali e posizioni
    extraPerGr <- spaceDiamond + spaceHetero + spaceLabel + spaceGap
    totRow <- plotData$k + (extraPerGr * nSub) - spaceGap

    rowsList <- vector("list", nSub)
    shadeRows <- diamondRows <- heteroRows <- integer(nSub)

    if (!reverseOrder) {
        # Bottom-to-top
        curr <- 1
        for (i in seq_len(nSub)) {
            if (spaceHetero > 0) {
                heteroRows[i] <- curr
                curr <- curr + spaceHetero
            }
            if (spaceDiamond > 0) {
                diamondRows[i] <- curr + if(extendedPrediction)2 else 0
                curr <- curr + spaceDiamond
            }
            rowsList[[i]] <- curr:(curr + studiesPerGr[i] - 1)
            curr <- curr + studiesPerGr[i] + spaceLabel
            shadeRows[i] <- curr - 1
            if (i < nSub) curr <- curr + spaceGap
        }
    } else {
        # Top-to-bottom
        curr <- totRow
        for (i in seq_len(nSub)) {
            shadeRows[i] <- curr
            curr <- curr - spaceLabel
            rowsList[[i]] <- curr:(curr - studiesPerGr[i] + 1)
            curr <- curr - studiesPerGr[i]
            if (spaceDiamond > 0) {
                diamondRows[i] <- curr
                curr <- curr - spaceDiamond
            }
            if (spaceHetero > 0) {
                heteroRows[i] <- curr
                curr <- curr - spaceHetero
            }
            if (i < nSub) curr <- curr - spaceGap
        }
    }

    # Costruzione argomenti forest()
    colshadeVal <- if (is.null(colshade)) {
        if (reverseOrder) "#FFF3E0" else "#E3F2FD"
    } else {
        colshade
    }

    forestArgs <- list(
        x = plotData, order = orderVar, rows = unlist(rowsList),
        shade = shadeRows, colshade = colshadeVal, top = spaceTop,
        cex = cex, header = header
    )

    # Le intestazioni mantengono il proprio colore anche con zebratura degli studi.
    if (!is.null(self) && isTRUE(self$options$shadeRows)) {
        groupHeaderShading <- list(rows=shadeRows,col=colshadeVal)
        forestArgs$shade <- TRUE
        forestArgs$colshade <- NULL
    }

    # Applica ylim dalla cache se fornito, altrimenti usa ylim empirico
    if (!is.null(ylim)) {
        forestArgs$ylim <- ylim
    } else {
        forestArgs$ylim <- c(-spaceBottom, totRow + spaceTop)
    }

    if (!is.null(xlim)) forestArgs$xlim <- xlim
    if (!is.null(at)) forestArgs$at <- at
    if (!is.null(alim)) forestArgs$alim <- alim
    if (!is.null(olim)) forestArgs$olim <- olim
    if (!is.null(atransf)) forestArgs$atransf <- atransf

    if (!is.null(ilab)) {
        forestArgs$ilab <- ilab
        forestArgs$ilab.lab <- ilabLab
        forestArgs$ilab.xpos <- ilab.xpos
    }

    forestArgs <- c(forestArgs, list(...))
    if (identical(mzEstimatorId(plotData), "SJ2") && is.null(forestArgs$mlab)) forestArgs$mlab <- "RE Model [SJ2]"
    if (!hasGlobalSummary) {
        forestArgs$addfit <- FALSE
        forestArgs$addpred <- FALSE
    }
    forestArgs <- prepareReferences(forestArgs)
        tmp <- callForest(forestArgs)
        drawReferences(tmp)

    # Se ilab.xpos non era fornito ma ilab sì, recuperalo dal plot

    # Se ilab.xpos non era fornito ma ilab sì, recuperalo dal plot
    if (is.null(ilab.xpos) && !is.null(ilab) && !is.null(tmp$ilab.xpos)) {
        ilab.xpos <- tmp$ilab.xpos
    }

    # Se topHeader è fornito ma topHeaderXpos è NULL, calcolalo automaticamente
    if (!is.null(topHeader) && is.null(topHeaderXpos) && !is.null(ilab.xpos)) {
        nTop <- length(topHeader)
        nIlab <- length(ilab.xpos)
        colsPerHead <- nIlab / nTop
        if (colsPerHead == as.integer(colsPerHead)) {
            topHeaderXpos <- numeric(nTop)
            for (i in 1:nTop) {
                idx <- ((i - 1) * colsPerHead + 1):(i * colsPerHead)
                topHeaderXpos[i] <- mean(ilab.xpos[idx])
            }
        }
    }

    # Posizione x per testo a sinistra
    xPos <- if (is.null(xlim)) par("usr")[1] else xlim[1]

    # Aggiungi elementi per ogni sottogruppo
    for (i in seq_along(subLevels)) {
        # Label sottogruppo (riga shade)
        text(xPos, shadeRows[i], paste0(subVarName, ": ", subLevels[i]),
            font = subgroupFont, cex = cex, pos = 4
        )

        # Calcola meta-analisi sottogruppo
        idx <- which(subVarData == subLevels[i])
        if (length(idx) > 1) {
            if (!is.null(self)) {
                # Nessuna stima nel render jamovi: riusa il modello che alimenta la tabella.
                resSub <- cachedSubgroups[[as.character(subLevels[i])]]
            } else {
                # Mantiene l'uso autonomo della funzione R, senza un'analisi jamovi.
                resSub <- tryCatch(
                    mzFitRma(plotData$yi[idx], plotData$vi[idx], method=mzEstimatorId(plotData),
                        test=plotData$test, level=100*(1-plotData$level)),
                    error=function(e)NULL)
            }

            if (!is.null(resSub)) {
                # Determina label diamante
                if (is.null(ilabStat)) {
                    modName <- if (plotData$method == "FE") {
                        "FE Model"
                    } else {
                        paste0("RE Model (", mzEstimatorId(plotData), ")")
                    }
                    labelDiam <- if (!is.null(diamondLabel)) diamondLabel else modName
                } else {
                    labelDiam <- if (!is.null(diamondLabel)) diamondLabel else "Summary"
                    # Assicuriamoci che l'etichetta sia in grassetto corsivo se usiamo addpoly più avanti
                }

                # Diamante + statistiche ilab
                # Controllo addfit: se FALSE non disegna il diamante ma lascia lo spazio
                # Se ci sono moderatori (p > 1), non disegna il diamante
                # p > 1 indica che il modello RMA contiene almeno un moderatore oltre all'intercetta
                # In questo caso nascondiamo i diamanti per coerenza visiva con il modello overall
                shouldDrawDiamond <- TRUE

                if (!is.null(plotData$p) && plotData$p > 1) {
                    # Se ci sono moderatori, nascondiamo i diamanti di default
                    shouldDrawDiamond <- FALSE
                } else if (!is.null(self) && !is.null(self$options$addfit)) {
                    shouldDrawDiamond <- isTRUE(self$options$addfit)
                } else if (!is.null(list(...)$addfit)) {
                    shouldDrawDiamond <- isTRUE(list(...)$addfit)
                }


                if (spaceDiamond > 0) {
                    # Disegna l'etichetta del summary (es. "Summary" o "RE Model")
                    text(xPos, diamondRows[i], labelDiam, pos = 4, font = 4, cex = cex)

                    # Disegna il diamante (e intervallo di predizione) solo se richiesto (es. no moderatori)
                    if (shouldDrawDiamond) {
                        # Gestione addpred (intervallo di predizione)
                        shouldDrawPred <- FALSE
                        if (!is.null(self) && !is.null(self$options$addpred)) {
                            shouldDrawPred <- isTRUE(self$options$addpred)
                        } else if (!is.null(list(...)$addpred)) {
                            shouldDrawPred <- isTRUE(list(...)$addpred)
                        }

                        # Colore basato sullo shade degli headers
                        subCol <- colshadeVal

                        # Disegna intervallo di predizione se richiesto
                        if (shouldDrawPred && !extendedPrediction) {
                            preds <- predict(resSub)
                            segments(
                                x0 = preds$pi.lb, y0 = diamondRows[i],
                                x1 = preds$pi.ub, y1 = diamondRows[i],
                                lwd = 1.2, lty = 2, col = "black"
                            )
                            segments(
                                x0 = preds$pi.lb, y0 = diamondRows[i] - 0.2,
                                x1 = preds$pi.lb, y1 = diamondRows[i] + 0.2,
                                lwd = 1.2, lty = 2, col = "black"
                            )
                            segments(
                                x0 = preds$pi.ub, y0 = diamondRows[i] - 0.2,
                                x1 = preds$pi.ub, y1 = diamondRows[i] + 0.2,
                                lwd = 1.2, lty = 2, col = "black"
                            )
                        }

                        # Disegna diamante senza etichetta (già disegnata sopra)
                        if (extendedPrediction && resSub$k >= 2L) {
                            # Il renderer metafor aggiunge limiti, forma e annotazioni coerenti.
                            addpoly(resSub, row=c(diamondRows[i],diamondRows[i]-1),
                                cex=cex,atransf=atransf,targs=targs,
                                mlab=c("","Prediction interval"),addpred=TRUE,
                                predstyle=predictionStyle,col=subCol,border="black")
                        } else addpoly(resSub,
                            row = diamondRows[i], cex = cex,
                            atransf = atransf, mlab = "",
                            col = subCol, border = "black"
                        )
                    }

                    # Statistiche ilab per sottogruppo: sempre mostrate se presenti
                    if (!is.null(ilabStat) && !is.null(ilab) && !is.null(ilab.xpos)) {
                        nIlabCols <- if (!is.null(ilabLab)) length(ilabLab) else ncol(as.matrix(ilab))
                        ilabXposOnly <- if (nIlabCols <= length(ilab.xpos)) ilab.xpos[1:nIlabCols] else ilab.xpos
                        drawSummarySeparators(topHeader, ilabXposOnly, diamondRows[i] + 0.4, cex, nIlabCols, xlimLine, tmp$xlim)

                        ilabMat <- as.matrix(ilab)
                        for (col in seq_along(ilabStat)) {
                            valToPrint <- NULL
                            statItem <- ilabStat[[col]]

                            if (identical(statItem, FALSE)) next

                            if (is.function(statItem)) {
                                vals <- suppressWarnings(as.numeric(ilabMat[idx, col]))
                                if (!all(is.na(vals))) {
                                    valToPrint <- statItem(vals, na.rm = TRUE)
                                }
                            } else if (is.list(statItem) && !is.null(statItem$groups)) {
                                if (i <= length(statItem$groups)) {
                                    valToPrint <- statItem$groups[i]
                                }
                            }

                            if (!is.null(valToPrint)) {
                                if (is.numeric(valToPrint)) {
                                    label <- if (valToPrint %% 1 == 0) formatC(valToPrint, format = "f", digits = 0) else formatC(valToPrint, format = "f", digits = 2)
                                } else {
                                    label <- valToPrint
                                }
                                text(ilab.xpos[col], diamondRows[i], label, cex = cex * 0.75, font = 4)
                            }
                        }
                    }
                }


                # Statistiche eterogeneità sottogruppo
                if (spaceHetero > 0) {
                    # Formato p-value
                    pText <- if (resSub$QEp < 0.0001) "p < 0.0001" else sprintf("p=%.3f", resSub$QEp)

                    # Costruisci testo: tau² solo se non FE
                    if (plotData$method == "FE") {
                        hetText <- sprintf(
                            "I\u00b2=%.1f%%; Q(df=%d)=%.2f, %s",
                            resSub$I2, resSub$k - resSub$p, resSub$QE, pText
                        )
                    } else {
                        hetText <- sprintf(
                            "tau\u00b2=%.3f; I\u00b2=%.1f%%; Q(df=%d)=%.2f, %s",
                            resSub$tau2, resSub$I2, resSub$k - resSub$p, resSub$QE, pText
                        )
                    }
                    text(xPos, heteroRows[i], hetText,
                        cex = cex * 0.8, pos = 4, font = 4
                    )
                }
            }
        }
    }


    # Determina se disegnare il diamante overall (e quindi le statistiche totali)
    # Seguiamo la stessa logica dei sottogruppi: nascondiamo se ci sono moderatori (p > 1)
    shouldDrawDiamondOverall <- TRUE

    if (!hasGlobalSummary) {
        # Se ci sono moderatori, nascondiamo gli elementi del summary di default
        shouldDrawDiamondOverall <- FALSE
    } else if (!is.null(self) && !is.null(self$options$addfit)) {
        shouldDrawDiamondOverall <- isTRUE(self$options$addfit)
    } else if (!is.null(list(...)$addfit)) {
        shouldDrawDiamondOverall <- isTRUE(list(...)$addfit)
    }


    # Nessuna etichetta Summary globale nei modelli con moderatori.

    # Statistiche ilab totali (riga -1) - Sempre mostrate se presenti
    if (hasGlobalSummary && !is.null(ilabStat) && !is.null(ilab) && !is.null(ilab.xpos)) {
        ilabMat <- as.matrix(ilab)
        for (col in seq_along(ilabStat)) {
            valToPrint <- NULL
            statItem <- ilabStat[[col]]

            # FIX: Salta se FALSE
            if (identical(statItem, FALSE)) {
                next
            }

            if (is.function(statItem)) {
                vals <- suppressWarnings(as.numeric(ilabMat[, col]))
                if (!all(is.na(vals))) {
                    valToPrint <- statItem(vals, na.rm = TRUE)
                }
            } else if (is.list(statItem) && !is.null(statItem$overall)) {
                valToPrint <- statItem$overall
            } else if (is.vector(statItem) && length(statItem) == 1) {
                valToPrint <- statItem
            }

            if (!is.null(valToPrint)) {
                if (is.numeric(valToPrint)) {
                    label <- if (valToPrint %% 1 == 0) formatC(valToPrint, format = "f", digits = 0) else formatC(valToPrint, format = "f", digits = 2)
                } else {
                    label <- valToPrint
                }
                text(ilab.xpos[col], -1, label, cex = cex * 0.75, font = 4)
            }
        }
    }

    # Statistiche eterogeneità overall (riga -2.0)
    # Formato p-value
    pText <- if (plotData$QEp < 0.0001) "p < 0.0001" else sprintf("p=%.3f", plotData$QEp)

    # Costruisci testo: tau² solo se non FE
    if (plotData$method == "FE") {
        hetText <- sprintf(
            "Overall: I\u00b2=%.1f%%; Q(df=%d)=%.2f, %s",
            plotData$I2, plotData$k - plotData$p, plotData$QE, pText
        )
    } else {
        hetText <- sprintf(
            "Overall: tau\u00b2=%.3f; I\u00b2=%.1f%%; Q(df=%d)=%.2f, %s",
            plotData$tau2, plotData$I2, plotData$k - plotData$p, plotData$QE, pText
        )
    }
    # Posizione x per testo a sinistra (fix per testo tagliato)
    # Uniformiamo alla logica del plot semplice usando tmp$textpos
    xPosOverall <- tmp$textpos[1]
    if (!is.null(tmp$textpos) && is.matrix(tmp$textpos)) {
        xPosOverall <- min(tmp$textpos[, 1], na.rm = TRUE)
    }

    if(hasGlobalSummary)text(xPosOverall, y = rowHT, labels = hetText, cex = cex * 0.8, pos = 4, font = 4, xpd = TRUE)
    else drawModeratorFooter(tmp,hetText)



    # Header superiori
    drawTopHeaders(topHeader, topHeaderXpos, ilab.xpos, totRow, cex)

    # Sottotitolo principale (subTitMain)
    if (!is.null(subTitMain)) {
        text(
            x = mean(tmp$xlim), y = totRow + spaceTop + 0.2,
            labels = subTitMain, cex = cex * 0.9, font = 4, xpd = TRUE
        )
    }

    robGeometry <- .mzDrawClassicRob(rob,robLayout,plotData,forestArgs,tmp,cex,spaceTop,
            overallY=if(hasGlobalSummary)rowHT else NULL,footer=footerLayout)
    invisible(list(tmp = tmp, totRow = totRow,footer=footerLayout,rob=robGeometry))
}
