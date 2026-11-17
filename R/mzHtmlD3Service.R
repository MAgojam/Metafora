# mzHtmlD3Service.R

# ──────────────────────────────────────────────────────────────────────────────
# Servizio grafico D3.js per i result type:Html di Metafora.
# Usa esclusivamente D3.js (inst/extdata/d3.min.js) per entrambi i grafici.
#
# Funzioni pubbliche:
#   mzD3RenderForest(self, res)   — forest plot su outD3ForestPlot
#   mzD3RenderFunnel(self, res)   — funnel plot su outD3FunnelPlot

# Il contesto esplicito consente .() anche fuori dai metodi dell'analisi.
.mzD3InteractionTexts <- function(jamovi_self) {
    self <- jamovi_self
    if (!is.function(self$options$translate)) {
        self <- list(options=list(translate=function(text,n=1) text))
    }
    list(
        "Low Risk"=.("Low Risk"),
        "Some Concerns / Unclear"=.("Some Concerns / Unclear"),
        "High Risk"=.("High Risk"),
        "Missing / Unrecognized"=.("Missing / Unrecognized"),
        "Switch to Rainforest view"=.("Switch to Rainforest view"),
        "Switch to classic Forest Plot view"=.("Switch to classic Forest Plot view"),
        "Study"=.("Study"), "Summary"=.("Summary"), "Pooled"=.("Pooled"),
        "Standard Error"=.("Standard Error"),
        "Standard Error (analysis scale)"=.("Standard Error (analysis scale)"),
        "Study Omitted"=.("Study Omitted"), "Power"=.("Power"),
        "Funnel Plot"=.("Funnel Plot"), "Legend"=.("Legend"),
        "Null effect"=.("Null effect"),
        "Residual"=.("Residual"),
        "Power color palette"=.("Power color palette"),
        "Red-Yellow-Green"=.("Red-Yellow-Green"),
        "Missing subgroup"=.("Missing subgroup"),
        "All Subgroups"=.("All Subgroups"),
        "All studies"=.("All studies"),
        "Study effects shown; no overall effect for this meta-regression."=.("Study effects shown; no overall effect for this meta-regression."),
        "Null region"=.("Null region"),
        "Pseudo confidence region"=.("Pseudo confidence region"),
        "Zero residual"=.("Zero residual"),
        "Reference used for the pseudo confidence region."=.("Reference used for the pseudo confidence region."),
        "Outside the pseudo confidence region; not an outlier diagnosis."=.("Outside the pseudo confidence region; not an outlier diagnosis."),
        "Model residuals shown, as in metafor; no overall pooled effect for this meta-regression."=
            .("Model residuals shown, as in metafor; no overall pooled effect for this meta-regression."),
        "Null contour"=.("Null contour"),
        "Pooled effect"=.("Pooled effect"),
        "Subgroup"=.("Subgroup"),
        "Original Pooled Estimate"=.("Original Pooled Estimate"),
        "Original subgroup estimate"=.("Original subgroup estimate"),
        "Global pooled estimate across all studies."=.("Global pooled estimate across all studies."),
        "Reference used for the significance contours."=.("Reference used for the significance contours."),
        "Estimated from the observed studies."=.("Estimated from the observed studies."),
        "Trim-and-Fill adjusted effect"=.("Trim-and-Fill adjusted effect"),
        "Sensitivity analysis; imputed studies are not observed evidence."=.("Sensitivity analysis; imputed studies are not observed evidence."),
        "Outside the null region; not an outlier diagnosis."=.("Outside the null region; not an outlier diagnosis."),
        "Click or press Enter to pin this explanation."=.("Click or press Enter to pin this explanation."),
        "Pinned. Click again or press Escape to release."=.("Pinned. Click again or press Escape to release."),
        "Two-sided normal-test significance boundary around the null effect."=.("Two-sided normal-test significance boundary around the null effect."),
        "Power conditional on the global pooled effect, not subgroup-specific power."=.("Power conditional on the global pooled effect, not subgroup-specific power."),
        "Prediction interval"=.("Prediction interval"),
        "Prediction interval: true effect in a new study; requires at least two remaining studies."=
            .("Prediction interval: true effect in a new study; requires at least two remaining studies."),
        "Range for the true effect in a new study under this model."=.("Range for the true effect in a new study under this model.")
    )
}
#
# Opzioni forest rispettate:
#   modelmea, showBackTransform, level
#   shadeRows       — righe alternate con sfondo grigio chiaro
#   addpred         — banda Prediction Interval (se TRUE)
#   showweights     — colonna Peso% (se FALSE la nasconde)
#   mainTitle       — titolo sopra il grafico
#   xAxisTitle      — label asse X custom
#   forestOrder     — ordinamento studi
#   pchForest       — stile punto (bSquare/bCircle/bDiamond/eSquare/eCircle/eDiamond)
#   subgroup        — separatori di gruppo con label
#
# Prerequisiti:
#   inst/extdata/d3.min.js
#   inst/extdata/metaforad3.min.js
#   inst/extdata/metaforad3.min.css
# ──────────────────────────────────────────────────────────────────────────────


# ── HELPER INTERNI ────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (is.null(a)) b else a

# Identita delle righe prima degli ordinamenti di presentazione. I nomi del
# dataset possono essere ripetuti o normalizzati da metafor: non sono chiavi.
# Le row.names del data frame processato conservano anche i buchi da na.omit.
.mzD3StudyIds <- function(res) {
    ids <- res$ids %||% seq_along(res$slab)
    sourceRows <- if (is.data.frame(res$data)) rownames(res$data) else NULL
    rows <- if (length(sourceRows) && all(ids %in% seq_along(sourceRows))) sourceRows[ids] else as.character(ids)
    stopifnot(length(rows) == length(res$slab), !anyNA(rows), !anyDuplicated(rows))
    paste0('study-row:', rows)
}


mzHtmlD3Service <- R6::R6Class(
    "mzHtmlD3Service",
    
    # ── METODI PUBBLICI ───────────────────────────────────────────────────────
    public = list(
        
        #' Renderizza il Forest Plot D3 nel result outD3ForestPlot
        #' @param jamovi_self Istanza R6 del modello Jamovi (il `self` dal .run del backend)
        #' @param res         Oggetto rma da metafor::rma()
        renderForest = function(jamovi_self, res,
                                ilab = NULL, ilab_names = NULL, ilab_groups = NULL,
                                ilab_totals = NULL,
                                rob_domains = NULL, rob_domain_names = NULL) {
            
            if (is.null(jamovi_self$results$outD3ForestPlot)) return(invisible(NULL))
            if (!isTRUE(jamovi_self$results$outD3ForestPlot$visible)) return(invisible(NULL))
            
            tryCatch({
                # Unico percorso ROB per i modelli che espongono il supplier.
                robResult <- .mzCollectRobDomains(jamovi_self)
                rob_domains <- if (!is.null(robResult)) robResult$values else NULL
                rob_domain_names <- if (!is.null(robResult)) robResult$names else NULL
                private$.analysisScope <- private$jsAnalysisScope(jamovi_self)
                mi     <- private$jsGetMeasureInfo(jamovi_self$options$tabbedControlModelType,
                                                   jamovi_self$options$modelmea,
                                                   isTRUE(jamovi_self$options$showBackTransform))
                
                level  <- if (!is.null(jamovi_self$options$level)) jamovi_self$options$level / 100 else 0.95
                
                ciMult <- qnorm(1 - (1 - level) / 2)
                
                sortIdx  <- private$jsSortStudies(res, jamovi_self$options$forestOrder %||% "None")
                
                subgrVec <- if (!is.null(jamovi_self$options$subgroup) &&
                                "subgroup" %in% names(jamovi_self$dataProc))
                    
                    jamovi_self$dataProc$subgroup else NULL

                json <- private$d3BuildForestJson(res, mi, ciMult, sortIdx,
                                                  isTRUE(jamovi_self$options$addpred), subgrVec,
                                                  ilab, ilab_names, ilab_groups, ilab_totals,
                                                  rob_domains, rob_domain_names,
                                                  subgroupResults = if (!is.null(subgrVec)) jamovi_self$subgroupResults else NULL)

                # alim = limiti asse valori calcolati da metafor::forest() (Classic)
                alimMetafor <- tryCatch({
                    sc <- jamovi_self$results$mzForestPlotSizeCache$state
                    if (!is.null(sc) && !is.null(sc$alim)) sc$alim else NULL
                }, error = function(e) NULL)
                
                opts <- list(
                    i18n        = .mzD3InteractionTexts(jamovi_self),
                    addfit      = isTRUE(jamovi_self$options$addfit),
                    shadeRows   = isTRUE(jamovi_self$options$shadeRows),
                    showweights = isTRUE(jamovi_self$options$showweights),
                    mainTitle   = jamovi_self$options$mainTitle  %||% "",
                    xAxisTitle  = jamovi_self$options$xAxisTitle %||% "",
                    pchForest   = jamovi_self$options$pchForest  %||% "bSquare",
                    ci_level    = round(level * 100),
                    ciMult      = ciMult,
                    steps       = as.integer(jamovi_self$options$steps %||% 5L),
                    alim        = alimMetafor
                )
                
                d3_js       <- private$jsReadLib("d3.min.js")
                metafora_js <- private$jsReadLib("metaforad3.min.js")
                
                # Titolo (letto dal result item, cosi' segue eventuali modifiche
                # future al .r.yaml) e chiave di persistenza dello stato
                # espanso/collassato dell'accordion. La chiave NON include
                # res$method (a differenza del canale cross-filter): deve
                # restare stabile su modelest/modelmea/level/opzioni estetiche,
                # che innescano clearWith ma non devono mai resettare la scelta
                # dell'utente. Cambia solo se cambiano gli studi (slab).
                title <- jamovi_self$results$outD3ForestPlot$title %||% "Forest Plot"
                key   <- private$jsD3PlotAccordionKey(res, "forest")
                
                nativeSvg <- inherits(jamovi_self$results$outD3ForestPlot, "Svg")
                doc   <- private$d3BuildForestDoc(d3_js, metafora_js, json, opts, title, key, nativeSvg)

                # Larghezza e altezza minima SVG calcolate dinamicamente con formule speculari a forest_plot.js
                ilabW   <- if (!is.null(ilab_names)) length(ilab_names) * 55L else 0L
                wtW     <- if (isTRUE(opts$showweights)) (40L + 14L) else 0L
                robW    <- if (!is.null(rob_domain_names) && length(rob_domain_names) > 0) (10L + length(rob_domain_names) * .mzRobColumnWidth(rob_domain_names)) else 0L
                rightW  <- 4L + wtW + 110L + robW
                forestW <- 30L + 150L + ilabW + (14L + 280L) + rightW + 10L

                # Calcolo altezza iniziale stimata per evitare collassi/salti visivi (flicker/tremore)
                nStudies <- length(res$yi)
                hasSubgr <- !is.null(subgrVec) && length(subgrVec) > 0
                sgExtraRows <- 0
                if (hasSubgr) {
                    nSg <- length(unique(as.character(subgrVec)))
                    hasHeteroSubgr <- isTRUE(opts$addfit) && !is.null(res$QE)
                    rowsPerSg <- 1L + as.integer(isTRUE(opts$addfit)) + (if (hasHeteroSubgr) 1L else 0L)
                    sgExtraRows <- nSg * rowsPerSg + max(0, nSg - 1) * 0.4
                }
                hasIlabGrp <- !is.null(ilab_groups) && length(ilab_groups) > 0
                hasRob     <- !is.null(rob_domain_names) && length(rob_domain_names) > 0
                hasGlobal <- isTRUE(res$int.only) && isTRUE(opts$addfit)
                hasHetero  <- hasGlobal && !is.null(res$QE) && !is.null(res$I2)
                tailFactor <- if (!hasGlobal) 1.8 else if (hasHetero) 4.2 else 3.5
                mTop       <- (if (nzchar(jamovi_self$options$mainTitle %||% "")) 52L else 46L) +
                              (if (hasIlabGrp) 14L else 0L) +
                              (if (hasRob) 26L else 0L)
                mBot       <- 50L
                svgH       <- as.integer(ceiling(mTop + (nStudies + sgExtraRows + tailFactor) * 28L + mBot))
                forestH    <- svgH + 82L

                forestContent <- if (nativeSvg) doc else private$jsIframeWrap(doc,
                    minWidth = forestW, height = forestH, key = key, stablePlot = TRUE,
                    exportSelf=jamovi_self,exportKind='forest')
                # Svg views may already exist when the first computed result
                # arrives. jamovi 28.14 loads setScripts only in the view's
                # constructor, so late assets are not requested. The native
                # document carries its scripts in execution order instead.
                if (nativeSvg) jamovi_self$results$outD3ForestPlot$setScripts(character())
                jinfo("[D3/FOREST] publish", "studies =", nStudies, "ROB =", length(rob_domain_names), ui_log_obj=jamovi_self$results$debugLog, .loc = "mzHtmlD3Service.R:217")
                jamovi_self$results$outD3ForestPlot$setContent(forestContent)
            },
            error = function(e) {
                mark("[D3] failed", conditionMessage(e), ui_log_obj=jamovi_self$results$debugLog, .loc = "mzHtmlD3Service.R:221")
                jamovi_self$results$outD3ForestPlot$setContent(
                    paste0("<p style='color:#c00;font-family:monospace'>",
                           "Forest Plot error: ", htmlSpecialChars(conditionMessage(e)), "</p>")
                )
            })
        },
        
        #' Renderizza il Funnel Plot D3 nel result outD3FunnelPlot
        #' @param jamovi_self Istanza R6 del modello Jamovi
        #' @param res         Oggetto rma da metafor::rma()
        renderFunnel = function(jamovi_self, res) {
            
            if (is.null(jamovi_self$results$outD3FunnelPlot)) return(invisible(NULL))
            if (!isTRUE(jamovi_self$results$outD3FunnelPlot$visible)) return(invisible(NULL))
            
            tryCatch({
                private$.analysisScope <- private$jsAnalysisScope(jamovi_self)
                mi      <- private$jsGetMeasureInfo(jamovi_self$options$tabbedControlModelType,
                                                    jamovi_self$options$modelmea,
                                                    isTRUE(jamovi_self$options$showBackTransform))
                
                # Coerente con mzMetaforService.R::mzFunnelPlot — il Sunset e'
                # attivo solo quando yaxis == "sei", indipendentemente dal
                # checkbox sunsetFunnel (stessa gate della versione Classic).
                sunset  <- isTRUE(jamovi_self$options$sunsetFunnel) &&
                    identical(jamovi_self$options$yaxis, "sei") && isTRUE(res$int.only)
                
                enhance <- isTRUE(jamovi_self$options$enhanceFunnel)
                
                subgrVec <- if (!is.null(jamovi_self$options$subgroup) &&
                                "subgroup" %in% names(jamovi_self$dataProc))
                    
                    jamovi_self$dataProc$subgroup else NULL
                
                json  <- private$d3BuildFunnelJson(res, mi,
                                                   sunset      = sunset,
                                                   enhance     = enhance,
                                                   subgroupVec = subgrVec,
                                                   sig_level   = 0.05,
                                                   yaxis       = jamovi_self$options$yaxis %||% "sei",
                                                   yaxisInv    = isTRUE(jamovi_self$options$yaxisInv))
                
                d3_js       <- private$jsReadLib("d3.min.js")
                metafora_js <- private$jsReadLib("metaforad3.min.js")
                
                title <- jamovi_self$results$outD3FunnelPlot$title %||%
                    (if (sunset) "Sunset Plot" else "Funnel Plot")
                
                key   <- private$jsD3PlotAccordionKey(res, if (sunset) "sunset" else "funnel")
                
                doc <- if (sunset)
                    private$d3BuildSunsetDoc(d3_js, metafora_js, json, title, key, .mzD3InteractionTexts(jamovi_self))
                else
                    private$d3BuildFunnelDoc(d3_js, metafora_js, json, title, key, .mzD3InteractionTexts(jamovi_self))
                
                funnelW <- if (sunset) 620L else 580L
                funnelH <- if (sunset) 540L else 500L
                jinfo("[D3/FUNNEL] publish", "sunset =", sunset, ui_log_obj=jamovi_self$results$debugLog, .loc = "mzHtmlD3Service.R:279")
                jamovi_self$results$outD3FunnelPlot$setContent(private$jsIframeWrap(doc,
                    minWidth = funnelW, height = funnelH, key = key,
                    stablePlot = TRUE,exportSelf=jamovi_self,exportKind='funnel'))
            },
            error = function(e) {
                mark("[D3] failed", conditionMessage(e), ui_log_obj=jamovi_self$results$debugLog, .loc = "mzHtmlD3Service.R:285")
                jamovi_self$results$outD3FunnelPlot$setContent(
                    paste0("<p style='color:#c00;font-family:monospace'>",
                           "Funnel Plot error: ", htmlSpecialChars(conditionMessage(e)), "</p>")
                )
            })
        },
        
        #' Renderizza il Leave-One-Out Plot D3 nel result item indicato
        #' @param jamovi_self Istanza R6 del modello Jamovi
        #' @param htmlResult  Result item Html target (condiviso con la modalita'
        #'                    Tabella, va passato esplicitamente — non cercato
        #'                    per nome fisso come renderForest/renderFunnel)
        #' @param res         Oggetto rma da metafor::rma()
        #' @param l1o         data.frame da MetaforServiceClass$mzComputeLeave1Out()
        forestTableAnalysisScope = function(jamovi_self) private$jsAnalysisScope(jamovi_self),

        # Forest tables share the isolated/stable document, not plot geometry.
        forestTableHtml = function(jamovi_self, payload, kind="forest-table", renderer="renderForestTable") {
            private$.analysisScope <- private$jsAnalysisScope(jamovi_self)
            key <- private$jsD3PlotAccordionKey(jamovi_self$modelResults,kind)
            payload$state_key <- key
            payload$xfilter_channel <- private$jsXfilterChannelId(jamovi_self$modelResults)
            json <- jsonlite::toJSON(payload,auto_unbox=TRUE,na='null',null='null',digits=NA)
            doc <- private$d3BuildLeave1OutDoc(private$jsReadLib('d3.min.js'),
                private$jsReadLib('metaforad3.min.js'),json,payload$title,key,
                payload$i18n,renderer=renderer,
                bodyHtml="<div id='loo' class='mz-ft-root'></div>")
            parts <- private$jsIframeWrap(doc,minWidth=780L,height=500L,key=key,stablePlot=TRUE,asParts=TRUE)
            parts$analysisScope <- private$.analysisScope
            parts
        },

        renderCumulative = function(jamovi_self, result) {
            output <- jamovi_self$results$outD3CumulativePlot
            if (is.null(output) || !isTRUE(output$visible)) return(invisible(NULL))
            private$.analysisScope <- private$jsAnalysisScope(jamovi_self)
            # La chiave usa il modello completo: cambiare ordinamento non resetta l'accordion.
            key <- private$jsD3PlotAccordionKey(jamovi_self$modelResults,'cumulative')
            payload <- mzCumulativePayload(jamovi_self,result)
            json <- jsonlite::toJSON(payload,auto_unbox=TRUE,na='null',null='null',digits=NA)
            doc <- private$d3BuildLeave1OutDoc(private$jsReadLib('d3.min.js'),
                private$jsReadLib('metaforad3.min.js'),json,output$title,key,
                payload$i18n,renderer='renderCumulative')
            jinfo('[D3/CUM] publish','steps =',length(result$estimate), .loc = "mzHtmlD3Service.R:329")
            output$setContent(private$jsIframeWrap(doc,minWidth=780L,
                height=as.integer(max(420,180+(length(result$estimate)+length(result$.mzGroups)*2)*28)),key=key,stablePlot=TRUE,
                exportSelf=jamovi_self,exportKind='cumulative'))
        },

        renderLeave1Out = function(jamovi_self, htmlResult, res, l1o) {
            if (is.null(htmlResult)) return(invisible(NULL))
            if (!isTRUE(htmlResult$visible)) return(invisible(NULL))
            
            tryCatch({
                private$.analysisScope <- private$jsAnalysisScope(jamovi_self)
                mi <- private$jsGetMeasureInfo(jamovi_self$options$tabbedControlModelType,
                                               jamovi_self$options$modelmea,
                                               isTRUE(jamovi_self$options$showBackTransform))
                
                ciLevelPct <- if (!is.null(jamovi_self$options$level)) round(jamovi_self$options$level) else 95
                sortOrder  <- jamovi_self$options$leave1outOrder %||% "none"
                
                json <- private$d3BuildLeave1OutJson(res, l1o, mi, ciLevelPct, sortOrder, jamovi_self)
                d3_js       <- private$jsReadLib("d3.min.js")
                metafora_js <- private$jsReadLib("metaforad3.min.js")
                
                title <- htmlResult$title %||% "Leave-One-Out"
                key   <- private$jsD3PlotAccordionKey(res, "leave1out")
                
                doc <- private$d3BuildLeave1OutDoc(d3_js, metafora_js, json, title, key, .mzD3InteractionTexts(jamovi_self))
                
                l1oH <- as.integer(ceiling(48L + length(res$yi) * 24L + 80L))
                htmlResult$setContent(private$jsIframeWrap(doc, minWidth = 620L, height = l1oH, key = key, stablePlot = TRUE,
                    exportSelf=jamovi_self,exportKind='leave1out'))
            },
            error = function(e) {
                mark("[D3] failed", conditionMessage(e), ui_log_obj=jamovi_self$results$debugLog, .loc = "mzHtmlD3Service.R:362")
                htmlResult$setContent(
                    paste0("<p style='color:#c00;font-family:monospace'>",
                           "Leave-One-Out Plot error: ", htmlSpecialChars(conditionMessage(e)), "</p>")
                )
            })
        }
    ),
    
    # ── METODI PRIVATI (Helper interni) ───────────────────────────────────────
    private = list(
        .analysisScope = "",

        # Una chiave di sessione per analisi, indipendente dai dati e dallo stimatore.
        jsAnalysisScope = function(jamovi_self) {
            item <- jamovi_self$results$d3SessionScope
            if (is.null(item)) return("")
            if (is.null(item$state)) item$setState(basename(tempfile("mz-d3-")))
            # analysisId distingue anche una copia dell'analisi che eredita lo stato.
            paste0(as.character(item$state), "-", jamovi_self$analysisId %||% "standalone")
        },
        
        jsReadLib = function(filename) {
            path <- system.file("extdata", filename, package = "Metafora")
            if (nchar(path) == 0L)
                stop(paste0("[mzHtmlD3Service] Plot library not found in inst/extdata/: ", filename))
            paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        },
        
        jsEscapeSrcdoc = function(doc) {
            doc <- gsub("&", "&amp;",  doc, fixed = TRUE)
            doc <- gsub('"', "&quot;", doc, fixed = TRUE)
            doc
        },
        
        #' Genera un ID canale stabile per il cross-filtering Forest<->Funnel.
        #' Checksum di scope di sessione+slab+method: solo la stessa analisi
        #' condivide il canale, anche se altre analisi usano gli stessi studi;
        #' cosi' i due iframe si sincronizzano via BroadcastChannel senza
        #' dover passare dal parent frame di jamovi.
        #' 
        #' Uso la funzione generale .mzPolyHash() che è nel file mzHtmlService.R
        jsXfilterChannelId = function(res) {
            key <- jsonlite::toJSON(list(scope=private$.analysisScope,ids=.mzD3StudyIds(res),
                labels=as.character(res$slab),method=mzEstimatorId(res)),auto_unbox=TRUE)
            paste0("mz-xfilter-id2-", private$.analysisScope, "-", .mzPolyHash(key))
        },
        
        #' Genera una chiave stabile per la persistenza (lato client) dello
        #' stato espanso/collassato dell'accordion D3, isolato per analisi. A differenza di
        #' jsXfilterChannelId, NON include res$method: deve restare
        #' invariata su modelest/modelmea/level/opzioni estetiche — che
        #' innescano clearWith sul result item ma non devono mai resettare
        #' la scelta dell'utente. Cambia solo se cambiano gli studi (slab),
        #' cosa che rende legittimo un reset (e' di fatto un plot diverso).
        #' 
        #' Uso la funzione generale .mzPolyHash() che è nel file mzHtmlService.R
        jsD3PlotAccordionKey = function(res, suffix) {
            key <- paste0(private$.analysisScope, "|", paste(as.character(res$slab), collapse = "|"))
            paste0("mz-d3plot-accordion-", private$.analysisScope, "-", .mzPolyHash(key), "-", suffix)
        },
        
        #' Avvolge il markup del body del grafico in un accordion +/-
        #' self-contained (icona/animazione in d3_common.css, persistenza
        #' stato in d3_common.js — vedi setupD3PlotAccordion()). Aperto di
        #' default al primo render; il toggle successivo e' governato solo
        #' dal click dell'utente (vedi jsIframeWrap).
        d3PlotAccordionWrap = function(bodyHtml, title, key) {
            paste0(
                '<details class="mz-d3plot-accordion" data-d3plot-accordion-key="', key, '">',
                '<summary class="mz-d3plot-accordion-summary">',
                '<span class="mz-d3plot-accordion-icon-container">',
                '<svg class="mz-d3plot-accordion-icon" viewBox="0 0 12 12">',
                '<line class="mz-d3plot-accordion-icon-h" x1="1" y1="6" x2="11" y2="6"></line>',
                '<line class="mz-d3plot-accordion-icon-v" x1="6" y1="1" x2="6" y2="11"></line>',
                '</svg>',
                '</span>',
                '<span class="mz-d3plot-accordion-title">', title, '</span>',
                '</summary>',
                '<div class="mz-d3plot-accordion-body">', bodyHtml, '</div>',
                '</details>'
            )
        },
        
        jsIframeWrap = function(doc, minWidth = 1000L, height = 500L, key = "", stablePlot = FALSE, asParts = FALSE,
                                exportSelf=NULL,exportKind=NULL) {
            exportConfig <- if (!is.null(exportSelf)) .mzRegisterD3Export(exportSelf,exportKind,doc) else NULL
            exportBridge <- !is.null(exportConfig)
            doc <- private$jsEscapeSrcdoc(doc)
            # Il renderer Html di jamovi esegue gli script clonandoli nel head:
            # currentScript.previousElementSibling NON identifica il frame.
            # Il percorso condiviso dei grafici D3 trova il frame per chiave e
            # protegge questa sola istanza Html dai render di contenuto identico
            # (es. risultati IN_PROGRESS e COMPLETE della stessa run).
            # Non modifica il prototipo jamovi; update continua ad aggiornare il
            # modello. A pannello collassato conserva il frame e rinvia il
            # contenuto aggiornato alla riapertura, usando l'ultimo modello.
            # Cambi di grafico/analisi ed errori passano al render originale.
            # Leave-One-Out usa la stessa protezione, anche passando alla tabella.
            pre_size_js <- if (nzchar(key)) paste0(
                "(function(){",
                  "try{",
                    "if(!window.__mzD3PlotCache)window.__mzD3PlotCache={};",
                    "if(!window.__mzD3PlotAccordionState)window.__mzD3PlotAccordionState={};",
                    "var k=", shQuote(key), ";",
                    if (stablePlot) paste0(
                      "var f=document.querySelector('iframe[data-mz-stable-plot=\"'+k+'\"]');",
                      "if(!f)return;",
                      "var host=f.closest('.jmv-results-html');",
                      "if(host&&typeof host.render==='function'&&host.model&&host.model.attributes&&host.__mzD3RenderGuard!==2){",
                        "var originalRender=host.__mzD3OriginalRender||host.render;",
                        "host.__mzD3OriginalRender=originalRender;",
                        "var renderedContent=host.model.attributes.element.content;",
                        "host.__mzD3RenderGuard=2;",
                        "host.render=function(){",
                          "var next=this.model.attributes.element.content;",
                          "var current=this.querySelector('iframe[data-mz-stable-plot]');",
                          "if(next===renderedContent&&current){this.__mzD3PendingRender=false;return;}",
                          "var frameKey=current&&current.getAttribute('data-mz-stable-plot');",
                          if (!exportBridge) paste0(
                          "if(current&&window.__mzD3PlotAccordionState[frameKey]===false&&typeof next==='string'&&next.indexOf('data-mz-stable-plot=\"'+frameKey+'\"')!==-1){",
                            "this.__mzD3PendingRender=true;return;",
                          "}"),
                          "this.__mzD3PendingRender=false;",
                          "renderedContent=next;",
                          "return originalRender.apply(this,arguments);",
                        "};",
                      "}",
                      "if(!window.__mzD3DeferredAccordionListener){",
                        "window.__mzD3DeferredAccordionListener=true;",
                        "window.addEventListener('message',function(e){",
                          "if(!e.data||e.data.type!=='mzD3PlotAccordionState'||e.data.open!==true)return;",
                          "var registry=window.__mzD3PlotFrameRegistry;",
                          "var frame=registry&&e.source&&registry.get(e.source);",
                          "if(!frame||!frame.isConnected||e.data.key!==frame.getAttribute('data-mz-plot-key'))return;",
                          "window.__mzD3PlotAccordionState[e.data.key]=true;",
                          "var host=frame.closest('.jmv-results-html');",
                          "if(host&&host.__mzD3PendingRender){host.__mzD3PendingRender=false;host.render();}",
                        "});",
                      "}") else "var f=document.currentScript.previousElementSibling;",
                    "var c=window.__mzD3PlotCache[k];",
                    "if(c&&c.h>0){f.style.height=c.h+'px';}",
                    "if(c&&c.w>0){f.style.minWidth=c.w+'px';}",
                  "}catch(e){}",
                "})()"
            ) else ""
            hostStyle <- if (exportBridge) '' else '<style>.jmv-results-html:has(.metafora-d3-output) { width: max-content !important; }</style>'
            initScript <- if (!exportBridge && nzchar(pre_size_js)) paste0('<script>', pre_size_js, '</script>') else ''
            result <- paste0(
                if (!asParts) hostStyle else '',
                '<div class="metafora-d3-output">',
                '<iframe srcdoc="', doc, '" ',
                if (stablePlot) paste0('data-mz-stable-plot="', key, '" ') else '',
                if (exportBridge) paste0('data-mz-export-token="',exportConfig$token,'" ') else '',
                'style="min-width:', minWidth, 'px;height:', height, 'px;width:100%;border:none;display:block;overflow:hidden;" ',
                'data-mz-plot-key="', key, '" ',
                'scrolling="no" ',
                'onload="',
                if (exportBridge) "if(!this.closest('.jmv-results-html')){this.style.display='none';return;} " else '',
                'var frame = this; ',
                if (exportBridge) paste0(private$jsEscapeSrcdoc(pre_size_js), '; ') else '',
                'if (!window.__mzD3PlotAccordionState) window.__mzD3PlotAccordionState = {}; ',
                'if (!window.__mzD3PlotCache) window.__mzD3PlotCache = {}; ',
                'try { ',
                '  var det = frame.contentDocument && frame.contentDocument.querySelector(\'.mz-d3plot-accordion\'); ',
                '  if (det && det.dataset && det.dataset.d3plotAccordionKey) { ',
                '    var k = det.dataset.d3plotAccordionKey; ',
                '    if (window.__mzD3PlotAccordionState[k] === false) { ',
                '      det.open = false; ',
                '      var collH = det.getBoundingClientRect().height || 40; ',
                '      frame.style.height = collH + \'px\'; ',
                '      window.__mzD3PlotCache[k] = { h: collH, w: parseInt(frame.style.minWidth) || 0 }; ',
                '    } ',
                '  } ',
                '} catch (err) {} ',
                "if (!window.__mzD3PlotFrameRegistry) { ",
                " window.__mzD3PlotFrameRegistry = new WeakMap(); ",
                " window.addEventListener('message', function(e) { ",
                "  var frame = e.source && window.__mzD3PlotFrameRegistry.get(e.source); ",
                "  if (!frame || !frame.isConnected || !e.data) return; ",
                "  var key = frame.getAttribute('data-mz-plot-key') || ''; ",
                "  if (e.data.key !== key) return; ",
                "  if (e.data.type === 'mzResize') { ",
                "   if (!Number.isFinite(e.data.w) || !Number.isFinite(e.data.h) || e.data.w <= 0 || e.data.h <= 0) return; ",
                "   frame.style.minWidth = e.data.w + 'px'; frame.style.height = e.data.h + 'px'; ",
                "   if (key) window.__mzD3PlotCache[key] = { h: e.data.h, w: e.data.w }; ",
                "  } else if (e.data.type === 'mzD3PlotAccordionState' && typeof e.data.open === 'boolean') { ",
                "   window.__mzD3PlotAccordionState[key] = e.data.open; ",
                "   if (!e.data.open) window.__mzD3PlotCache[key] = { h: frame.getBoundingClientRect().height || 40, w: parseInt(frame.style.minWidth) || 0 }; ",
                "  } else if (e.data.type === 'mzD3PlotAccordionRequestState' && Object.prototype.hasOwnProperty.call(window.__mzD3PlotAccordionState, key)) { ",
                "   frame.contentWindow.postMessage({type: 'mzD3PlotAccordionRestore', key: key, open: window.__mzD3PlotAccordionState[key]}, '*'); ",
                "  } ",
                " }); ",
                "} ",
                "window.__mzD3PlotFrameRegistry.set(frame.contentWindow, frame); ",
                "frame.contentWindow.postMessage({type:'mzD3PlotRequestSize',key:frame.getAttribute('data-mz-plot-key')||''},'*'); ",
                if (exportBridge) paste0('frame.contentWindow.MetaforaD3.installImageExport(frame,',
                    private$jsEscapeSrcdoc(jsonlite::toJSON(exportConfig,auto_unbox=TRUE)),'); ') else '',
                '">',
                '</iframe>',
                if (!asParts) initScript else '',
                '</div>'
            )
            if (asParts) list(body=result,head=paste0(hostStyle,initScript)) else result
        },
        
        #' Wrapper D3: delega a .mzGetBackTransform() (mzMetaforService.R, fonte
        #' unica) e adatta il risultato al formato consumato da renderForest/
        #' renderFunnel/renderLeave1Out. Il campo "transf" (funzione R) non e'
        #' mai consumato lato D3 (il client applica applyTransf() in JS in base
        #' a transf_type) - esposto solo per continuita' d'interfaccia.
        jsGetMeasureInfo = function(modelType, modelmea, showBackTransform) {
            bt <- .mzGetBackTransform(modelType, modelmea, showBackTransform, ni = NULL)
            raw <- .mzGetBackTransform(modelType, modelmea, FALSE, ni = NULL)
            list(
                transf      = bt$atransf %||% identity,
                transf_type = bt$transf_type,
                xlab        = bt$xlab,
                xlab_short  = bt$xlab_short,
                null_value  = bt$null_value,
                null_value_raw = raw$null_value
            )
        },
        
        jsSortStudies = function(res, forestOrder) {
            n <- length(res$yi)
            if (is.null(forestOrder) || forestOrder == "None") return(seq_len(n))
            vals <- switch(forestOrder,
                           "obs"           = as.numeric(res$yi),
                           "prec"          = as.numeric(res$vi),
                           "fit"           = tryCatch(as.numeric(fitted(res)),      error = function(e) as.numeric(res$yi)),
                           "resid"         = tryCatch(as.numeric(residuals(res)),   error = function(e) rep(0, n)),
                           "rstandard"     = tryCatch(as.numeric(rstandard(res)$z), error = function(e) rep(0, n)),
                           "abs.resid"     = tryCatch(abs(as.numeric(residuals(res))), error = function(e) rep(0, n)),
                           "abs.rstandard" = tryCatch(abs(as.numeric(rstandard(res)$z)), error = function(e) rep(0, n)),
                           NULL
            )
            if (is.null(vals)) return(seq_len(n))
            order(vals)
        },

        d3BuildForestJson = function(res, mi, ciMult, sortIdx,
                                     addpred, subgroupVec,
                                     ilab = NULL, ilab_names = NULL, ilab_groups = NULL,
                                     ilab_totals = NULL,
                                     rob_domains = NULL, rob_domain_names = NULL,
                                     subgroupResults = NULL) {
            
            .calc_ilab_total <- function(col_idx, row_idx, tot_type, ilab_list) {
                vals <- as.numeric(ilab_list[[col_idx]][row_idx])
                if (tot_type == "skip") return("")
                if (tot_type == "median") return(round(median(vals, na.rm = TRUE), 2))
                if (tot_type %in% c("wmean","pooledsd")) {
                    # Usa il campione del gruppo corretto (N1 oppure N2).
                    preceding <- which(ilab_totals=="sum" & seq_along(ilab_totals)<=col_idx)
                    n_col_idx <- if(length(preceding))tail(preceding,1L)else which(ilab_totals=="sum")[1L]
                    if (!is.na(n_col_idx)) {
                        n_vals <- as.numeric(ilab_list[[n_col_idx]][row_idx])
                        value <- mzForestAggregate(vals,n_vals,tot_type)
                        return(if(is.finite(value))round(value,2)else "")
                    }
                    return("")
                }
                if (tot_type == "prop") {
                    n_col_idx <- which(ilab_totals == "sum")
                    n_col_idx <- n_col_idx[n_col_idx > col_idx][1]
                    if (!is.na(n_col_idx)) {
                        n_vals <- as.numeric(ilab_list[[n_col_idx]][row_idx])
                        tot_n  <- sum(n_vals, na.rm = TRUE)
                        if (tot_n > 0) return(round(sum(vals, na.rm = TRUE) / tot_n, 3))
                    }
                    return(round(mean(vals, na.rm = TRUE), 3))
                }
                v <- sum(vals, na.rm = TRUE)
                if (v == round(v)) return(as.integer(v)) else return(round(v, 2))
            }
            
            if (is.null(ilab_totals) && !is.null(ilab))
                ilab_totals <- rep("sum", length(ilab))
            n      <- length(sortIdx)
            
            es    <- as.numeric(res$yi)[sortIdx]
            se    <- sqrt(as.numeric(res$vi))[sortIdx]
            ci_lo <- es - ciMult * se
            ci_hi <- es + ciMult * se
            
            labels  <- as.character(res$slab)[sortIdx]
            studyIds <- .mzD3StudyIds(res)
            
            if (!is.null(ilab)) {
                ilab_sorted <- lapply(ilab, function(col) col[sortIdx])
            } else {
                ilab_sorted <- NULL
            }
            
            if (!is.null(rob_domains)) {
                rob_sorted <- lapply(rob_domains, function(col) col[sortIdx])
            } else {
                rob_sorted <- NULL
            }
            
            if (!is.null(subgroupVec)) {
                sg_vals  <- as.character(subgroupVec[sortIdx])
                sg_order <- order(match(sg_vals, unique(sg_vals)), seq_len(n))
                es      <- es[sg_order]
                ci_lo   <- ci_lo[sg_order]
                ci_hi   <- ci_hi[sg_order]
                labels  <- labels[sg_order]
                sg_vals <- sg_vals[sg_order]
                sortIdx_used <- sortIdx[sg_order]
                if (!is.null(ilab_sorted))
                    ilab_sorted <- lapply(ilab_sorted, function(col) col[sg_order])
                if (!is.null(rob_sorted))
                    rob_sorted <- lapply(rob_sorted, function(col) col[sg_order])
            } else {
                sg_vals      <- NULL
                sortIdx_used <- sortIdx
            }
            
            wts_raw_all <- weights(res)
            max_wt      <- max(wts_raw_all, na.rm = TRUE)
            
            wts_raw_final <- wts_raw_all[sortIdx]
            if (!is.null(sg_vals)) wts_raw_final <- wts_raw_final[sg_order]
            
            total_w     <- sum(wts_raw_all)
            wts_pct     <- if (total_w > 0) round(wts_raw_final / total_w * 100, 2) else rep(0, n)
            
            plim_lo <- 0.5
            plim_hi <- 3.0
            sqrt_w  <- sqrt(wts_raw_all / max_wt)
            rng     <- max(sqrt_w) - min(sqrt_w)
            psize_01_all <- if (rng > .Machine$double.eps^0.5) {
                (sqrt_w - min(sqrt_w)) / rng
            } else {
                rep(1, length(sqrt_w))
            }
            psize_scaled_all <- psize_01_all * (plim_hi - plim_lo) / plim_hi + plim_lo / plim_hi
            psize_final <- psize_scaled_all[sortIdx]
            if (!is.null(sg_vals)) psize_final <- psize_final[sg_order]
            
            studies_list <- lapply(seq_len(n), function(i) {
                entry <- list(
                    id         = studyIds[sortIdx_used[i]],
                    label      = labels[i],
                    es         = round(es[i],    4),
                    ci_lo      = round(ci_lo[i], 4),
                    ci_hi      = round(ci_hi[i], 4),
                    weight     = wts_pct[i],
                    weight_raw = round(wts_raw_final[i], 4),
                    psize      = round(psize_final[i], 4)
                )

                if (!is.null(sg_vals)) entry$subgroup <- sg_vals[i]
                if (!is.null(ilab_sorted)) entry$ilab <- lapply(ilab_sorted, function(col) col[i])
                # unname() e' essenziale: rob_sorted e' una lista NOMINATA (nomi dominio).
                # jsonlite::toJSON serializza una lista nominata come oggetto JSON {}
                # anche con auto_unbox=TRUE, rompendo s.rob.forEach() lato D3 - che itera
                # per indice posizionale in sync con rob_domain_names, non per nome dominio.
                if (!is.null(rob_sorted)) entry$rob <- unname(lapply(rob_sorted, function(col) col[i]))
                entry
            })
            
            pooled_label <- if (res$method %in% c("FE", "EE")) "FE Model"
            else if (res$method == "CE") "CE Model"
            else paste0("RE Model [", mzEstimatorId(res), "]")
            
            # Anche un modello senza intercetta può avere un solo coefficiente:
            # int.only distingue un vero effetto globale da un coefficiente di regressione.
            has_pooled <- isTRUE(res$int.only)
            pooled_list <- list(
                label  = pooled_label,
                es     = if (has_pooled) round(as.numeric(res$beta), 4) else NULL,
                ci_lo  = if (has_pooled) round(res$ci.lb, 4) else NULL,
                ci_hi  = if (has_pooled) round(res$ci.ub, 4) else NULL,
                weight = 100
            )
            
            if (has_pooled && isTRUE(addpred)) {
                pred <- tryCatch(predict(res), error = function(e) NULL)
                if (!is.null(pred) && !is.null(pred$pi.lb)) {
                    pooled_list$pi_lo <- round(as.numeric(pred$pi.lb), 4)
                    pooled_list$pi_hi <- round(as.numeric(pred$pi.ub), 4)
                }
            }
            
            if (!is.null(res$QE) && !is.null(res$I2)) {
                pooled_list$hetero <- list(
                    tau2 = round(if (!is.null(res$tau2) && !is.na(res$tau2)) res$tau2 else 0, 3),
                    i2   = round(res$I2, 1),
                    QE   = round(res$QE, 2),
                    QEdf = as.integer(res$k - res$p),
                    QEp  = res$QEp
                )
            }
            
            if (!is.null(ilab) && !is.null(ilab_totals)) {
                all_idx <- seq_len(n)
                pooled_list$ilab <- lapply(seq_along(ilab), function(ci) {
                    .calc_ilab_total(ci, all_idx, ilab_totals[ci], ilab_sorted)
                })
            }
            
            subgroup_summaries <- NULL
            if (!is.null(sg_vals)) {
                sg_unique <- unique(sg_vals)
                yi_all <- as.numeric(res$yi)
                vi_all <- as.numeric(res$vi)
                
                subgroup_summaries <- lapply(sg_unique, function(sg) {
                    grp_orig_idx <- which(as.character(subgroupVec) == sg)
                    if (length(grp_orig_idx) < 2) return(NULL)
                    
                    yi_g <- yi_all[grp_orig_idx]
                    vi_g <- vi_all[grp_orig_idx]
                    
                    # Riusa il fit del backend: stessa inferenza e nessun ricalcolo estetico.
                    # Il fallback mantiene il servizio utilizzabile anche da chiamanti autonomi.
                    sg_res <- if (!is.null(subgroupResults)) subgroupResults[[sg]] else tryCatch(
                        mzFitRma(yi = yi_g, vi = vi_g, method = mzEstimatorId(res),
                                     test = res$test, level = 100 * (1 - res$level)),
                        error = function(e) NULL
                    )
                    
                    if (is.null(sg_res)) return(NULL)
                    
                    sg_entry <- list(
                        subgroup = sg,
                        label    = "Summary",
                        es       = round(as.numeric(sg_res$beta), 4),
                        ci_lo    = round(sg_res$ci.lb, 4),
                        ci_hi    = round(sg_res$ci.ub, 4)
                    )
                    
                    if (isTRUE(addpred)) {
                        sg_pred <- tryCatch(predict(sg_res), error = function(e) NULL)
                        if (!is.null(sg_pred) && !is.null(sg_pred$pi.lb)) {
                            sg_entry$pi_lo <- round(as.numeric(sg_pred$pi.lb), 4)
                            sg_entry$pi_hi <- round(as.numeric(sg_pred$pi.ub), 4)
                        }
                    }
                    
                    if (!is.null(sg_res$QE) && !is.null(sg_res$I2)) {
                        sg_entry$hetero <- list(
                            tau2 = round(if (!is.null(sg_res$tau2) && !is.na(sg_res$tau2)) sg_res$tau2 else 0, 3),
                            i2   = round(sg_res$I2, 1),
                            QE   = round(sg_res$QE, 2),
                            QEdf = as.integer(sg_res$k - sg_res$p),
                            QEp  = sg_res$QEp
                        )
                    }
                    
                    if (!is.null(ilab_sorted) && !is.null(ilab_totals)) {
                        grp_sorted_idx <- which(sg_vals == sg)
                        sg_entry$ilab <- lapply(seq_along(ilab_sorted), function(ci) {
                            .calc_ilab_total(ci, grp_sorted_idx, ilab_totals[ci], ilab_sorted)
                        })
                    }
                    
                    sg_entry
                })
                subgroup_summaries <- Filter(Negate(is.null), subgroup_summaries)
                if (length(subgroup_summaries) == 0) subgroup_summaries <- NULL
            }
            
            top_list <- list(studies = studies_list, pooled = pooled_list, has_pooled = has_pooled,
                             measure = mi$xlab, measure_short = mi$xlab_short %||% mi$xlab,
                             transf_type = mi$transf_type %||% "identity",
                             xfilter_channel = private$jsXfilterChannelId(res))
            
            if (!is.null(ilab_names))        top_list$ilab_names        <- as.list(ilab_names)
            if (!is.null(ilab_groups))       top_list$ilab_groups       <- ilab_groups
            if (!is.null(subgroup_summaries)) top_list$subgroup_summaries <- subgroup_summaries
            if (!is.null(rob_domain_names))   top_list$rob_domain_names  <- as.list(rob_domain_names)
            if (!is.null(rob_domain_names))   top_list$rob_column_width <- .mzRobColumnWidth(rob_domain_names)

            jsonlite::toJSON(top_list, auto_unbox = TRUE)
        },
        
        d3BuildForestDoc = function(d3_js, metafora_js, json_data, opts, title, key, nativeSvg = FALSE) {
            opts_json <- jsonlite::toJSON(opts, auto_unbox = TRUE)
            
            metafora_css <- private$jsReadLib("metaforad3.min.css")
            
            payload <- paste0("window.plotData = ", json_data, ";\n",
                              "window.plotData.config = ", opts_json, ";\n")
            payload <- gsub("</", "<\\/", payload, fixed=TRUE)
            
            body <- paste0(
                "<div id='forest-wrap'>",
                "  <div class='zoom-row'>",
                "    <div class='zoom-slider-container'>",
                "      <input type='range' id='zoomZ' class='thin-slider' min='0.5' max='2.5' step='0.05' value='1.0'>",
                "      <div id='lblZ' class='thumb-lbl'>Z</div>",
                "    </div>",
                "    <button id='rfToggle' class='rf-toggle-btn' title='Passa a vista Rainforest'>RF</button>",
                "    <select id='sgFilter' class='filter-select'></select>",
                "  </div>",
                "  <div id='fp'></div>",
                "</div>"
            )

            if (nativeSvg) {
                data <- jsonlite::fromJSON(json_data, simplifyVector = FALSE)
                data$config <- opts
                args <- paste(jsonlite::toJSON(key,auto_unbox=TRUE),
                    jsonlite::toJSON(data,auto_unbox=TRUE,digits=NA),
                    jsonlite::toJSON(metafora_css,auto_unbox=TRUE),sep=",")
                script <- gsub("</", "<\\/", paste0("MetaforaD3.mountNative(",args,");"),fixed=TRUE)
                token <- digest::digest(paste0(body,title,args),algo="sha256",serialize=FALSE)
                return(paste0('<div class="mz-native-d3" data-mz-native-key="',key,
                    '" data-mz-render-token="',token,'">',
                    private$d3PlotAccordionWrap(body,title,key),'</div>',
                    '<script>',d3_js,'</script>',
                    '<script>',metafora_js,'</script>',
                    '<script>',script,'</script>'))
            }
            
            # restore_js: posto DOPO d3PlotAccordionWrap cosi' <details> esiste
            # nel DOM quando lo script gira. <details> parte SENZA 'open';
            # lo script lo apre se lo stato e' 'aperto' o sconosciuto (primo
            # render). Se lo stato era 'chiuso', non succede nulla -> zero salto.
            restore_js <- paste0(
                "<script>",
                "try{",
                  "var _k=", shQuote(key), ";",
                  "var _p=window.parent;",
                  "var _st=_p&&_p.__mzD3PlotAccordionState?_p.__mzD3PlotAccordionState[_k]:undefined;",
                  "if(_st!==false){",
                    "var _d=document.querySelector('.mz-d3plot-accordion');",
                    "if(_d)_d.open=true;",
                  "}",
                "}catch(e){",
                  "var _d2=document.querySelector('.mz-d3plot-accordion');",
                  "if(_d2)_d2.open=true;",
                "}",
                "</script>"
            )
            
            paste0(
                "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
                "<style>", metafora_css, "</style>",
                "</head><body>",
                private$d3PlotAccordionWrap(body, title, key),
                restore_js,
                "<script>", d3_js, "</script>",
                "<script>", metafora_js, "</script>",
                "<script>", payload, "MetaforaD3.renderForest('#fp', window.plotData);", "</script>",
                "</body></html>"
            )
        },
        
        calcPowerBands = function(res, sig_level = 0.05, n_steps = 60) {
            se          <- sqrt(as.numeric(res$vi))
            seMax       <- max(se) * 1.15
            true_effect <- as.numeric(res$beta)
            
            yseq  <- seq(0, seMax, length.out = 2000)
            power <- (1 - pnorm(qnorm(1 - sig_level/2) * yseq, abs(true_effect), yseq)) +
                pnorm(qnorm(sig_level/2) * yseq, abs(true_effect), yseq)
            power[yseq == 0] <- if (abs(true_effect) > 0) 1 else sig_level
            
            se_breaks <- seq(0, seMax, length.out = n_steps + 1)
            
            lapply(seq_len(n_steps), function(i) {
                se_mid <- (se_breaks[i] + se_breaks[i + 1]) / 2
                pwr    <- power[which.min(abs(yseq - se_mid))]
                list(
                    y_lo  = round(se_breaks[i],     6),
                    y_hi  = round(se_breaks[i + 1], 6),
                    power = round(pwr, 4) 
                )
            })
        },
        
        calcPowerStats = function(res, sig_level = 0.05) {
            es          <- as.numeric(res$yi)
            se          <- sqrt(as.numeric(res$vi))
            true_effect <- as.numeric(res$beta)
            k           <- length(es)
            
            study_power <- (1 - pnorm(qnorm(1 - sig_level/2) * se, abs(true_effect), se)) +
                pnorm(qnorm(sig_level/2) * se, abs(true_effect), se)
            med_power  <- round(median(study_power) * 100, 1)
            expected   <- round(sum(study_power), 2)
            observed   <- sum(2 * (1 - pnorm(abs(es / se))) <= sig_level)
            
            c2   <- (observed - expected)^2 / expected +
                (observed - expected)^2 / max(k - expected, 0.001)
            p_tes <- round(1 - pchisq(c2, df = 1), 3)
            r_idx <- max(0, min(1, 2 * median(study_power) - observed / k))
            
            dpower <- function(target) {
                tryCatch(
                    round(uniroot(function(d) {
                        median((1 - pnorm(qnorm(1 - sig_level/2)*se, abs(d), se)) +
                                   pnorm(qnorm(sig_level/2)*se, abs(d), se)) - target
                    }, lower=0, upper=10, extendInt="upX", tol=0.001, maxiter=2000)$root, 2),
                    error = function(e) NA
                )
            }
            d33 <- dpower(0.33)
            d66 <- dpower(0.66)
            
            list(
                med_power = paste0(med_power, "%"),
                d33       = if (is.na(d33)) "n.a." else as.character(d33),
                d66       = if (is.na(d66)) "n.a." else as.character(d66),
                expected  = expected,
                observed  = observed,
                p_tes     = if (p_tes == 0) "< 0.001" else as.character(p_tes),
                r_index   = paste0(round(r_idx * 100, 1), "%"),
                sig_level = sig_level
            )
        },
        
        d3BuildFunnelJson = function(res, mi, sunset = FALSE,
                                     enhance = FALSE, subgroupVec = NULL,
                                     sig_level = 0.05, yaxis = "sei", yaxisInv = FALSE) {
            coordinates<-mzFunnelCoordinates(res,yaxis,yaxisInv)
            es<-coordinates$es;se<-coordinates$se;ni_vec<-coordinates$ni
            labels<-coordinates$labels;studyIds<-coordinates$ids
            
            # Inversione applicabile a tutti gli assi tranne "lni" — stessa gate
            # di mzMetaforService.R::mzFunnelPlot (yaxisTrans <- paste0(yaxis,"nv")).
            invert <- isTRUE(yaxisInv) && yaxis != "lni"
            
            yVals <- switch(yaxis,
                            "sei"    = se,
                            "vi"     = se^2,
                            "ni"     = ni_vec,
                            "sqrtni" = if (!is.null(ni_vec)) sqrt(ni_vec) else NULL,
                            "lni"    = if (!is.null(ni_vec)) log(ni_vec) else NULL,
                            se
            )
            if (is.null(yVals))stop('Sample-size axes require sample sizes stored in the fitted model.')
            if (invert) {
                yVals <- ifelse(yVals > 0, 1 / yVals, NA_real_)
            }
            
            yLabel <- switch(yaxis,
                             "sei"    = if (invert) "Precision (1/SE)" else "Standard Error",
                             "vi"     = if (invert) "Precision (1/Variance)" else "Sampling Variance",
                             "ni"     = if (invert) "Precision (1/N)" else "Sample Size",
                             "sqrtni" = if (invert) "Precision (1/\u221aN)" else "\u221a Sample Size",
                             "lni"    = "Log Sample Size",
                             "Standard Error"
            )
            
            # Contorni di significativita' e Trim&Fill richiedono una SE ben
            # definita in funzione della posizione sull'asse Y: disponibili solo
            # per sei/vi (diretti o invertiti in precisione).
            supportsContours <- yaxis %in% c("sei", "vi")
            
            studies_list <- lapply(seq_len(length(es)), function(i) {
                entry <- list(id = studyIds[i], label = labels[i], es = es[i],
                              se = se[i], y = yVals[i])
                if (!is.null(subgroupVec))
                    entry$subgroup <- as.character(subgroupVec[coordinates$indices[i]])
                entry
            })
            
            has_pooled <- isTRUE(res$int.only)
            pooled_es <- if (has_pooled) as.numeric(res$beta) else NULL
            # Stime, SE e contorni restano sulla scala del fit. Si trasformano solo le etichette.
            null_val  <- mi$null_value_raw %||% mi$null_value
            
            model_title <- if (!is.null(res$method)) {
                if (!has_pooled) "Meta-Regression Model"
                else if (res$method %in% c("FE", "EE", "CE")) "Fixed-Effect Model"
                else paste0("Random-Effect Model [", mzEstimatorId(res), "]")
            } else {
                "Funnel Plot"
            }
            
            base <- list(
                studies    = studies_list,
                pooled     = list(es = if (has_pooled) pooled_es else NULL),
                has_pooled = has_pooled,
                measure    = if(coordinates$residuals)'Residual Value'else mi$xlab,
                transf_type = if(coordinates$residuals)'identity'else mi$transf_type %||% "identity",
                null_value = null_val,
                funnel_center = coordinates$center,
                residuals = coordinates$residuals,
                y_limits = coordinates$y_limits,
                ci_level = 100 * (1-(res$level %||% .05)),
                ci_critical = qnorm(1-(res$level %||% .05)/2),
                enhance    = enhance,
                sunset     = sunset,
                model_title= model_title,
                y_axis_base       = yaxis,
                y_axis_inverted   = invert,
                y_label           = if (!identical(mi$transf_type, "identity") && yaxis %in% c("sei", "vi")) paste0(yLabel, " (analysis scale)") else yLabel,
                supports_contours = supportsContours,
                xfilter_channel   = private$jsXfilterChannelId(res)
            )
            
            # Il funnel del fit osservato non aggiunge studi imputati: come funnel(res).
            # Trim-and-Fill resta un'analisi distinta in Publication Bias.
            if (sunset && has_pooled) {
                base$power_bands <- private$calcPowerBands(res, sig_level)
                base$power_stats <- private$calcPowerStats(res, sig_level)
                # La scala cromatica usa potenze effettive, non altezze normalizzate.
                curve_se <- seq(0, max(se) * 1.15, length.out=129L)
                normal_power <- function(s) {
                    z <- qnorm(1-sig_level/2)
                    p <- pnorm(abs(pooled_es)/s-z) + pnorm(-abs(pooled_es)/s-z)
                    p[s==0] <- if (pooled_es==0) sig_level else 1
                    p
                }
                base$power_curve <- lapply(seq_along(curve_se),function(i) list(se=curve_se[i],power=normal_power(curve_se[i])))
                powers <- normal_power(se)
                for (i in seq_along(base$studies)) base$studies[[i]]$power <- powers[i]
            }
            
            jsonlite::toJSON(base, auto_unbox = TRUE, digits = NA)
        },
        
        d3BuildFunnelDoc = function(d3_js, metafora_js, json_data, title, key, i18n = list()) {
            metafora_css <- private$jsReadLib("metaforad3.min.css")
            
            payload <- paste0("window.plotData = ", json_data, ";\nwindow.plotData.i18n = Object.assign({}, window.plotData.i18n || {}, ", jsonlite::toJSON(i18n,auto_unbox=TRUE), ");\n")
            payload <- gsub("</", "<\\/", payload, fixed=TRUE)
            
            restore_js <- paste0(
                "<script>",
                "try{",
                  "var _k=", shQuote(key), ";",
                  "var _p=window.parent;",
                  "var _st=_p&&_p.__mzD3PlotAccordionState?_p.__mzD3PlotAccordionState[_k]:undefined;",
                  "if(_st!==false){",
                    "var _d=document.querySelector('.mz-d3plot-accordion');",
                    "if(_d)_d.open=true;",
                  "}",
                "}catch(e){",
                  "var _d2=document.querySelector('.mz-d3plot-accordion');",
                  "if(_d2)_d2.open=true;",
                "}",
                "</script>"
            )
            
            body <- paste0(
                "<div id='funnel-wrap'>",
                "  <div class='zoom-row'>",
                "    <input type='range' id='zoomZ' class='thin-slider' min='0.5' max='2.5' step='0.05' value='1.0'>",
                "    <div id='lblZ' class='thumb-lbl'>Z</div>",
                "  </div>",
                "  <div id='fn'></div>",
                "</div>"
            )
            
            paste0(
                "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
                "<style>", metafora_css, "</style></head><body>",
                private$d3PlotAccordionWrap(body, title, key),
                restore_js,
                "<script>", d3_js, "</script>",
                "<script>", metafora_js, "</script>",
                "<script>", payload, "MetaforaD3.renderFunnel('#fn', window.plotData);", "</script>",
                "</body></html>"
            )
        },
        
        d3BuildSunsetDoc = function(d3_js, metafora_js, json_data, title, key, i18n = list()) {
            metafora_css <- private$jsReadLib("metaforad3.min.css")
            
            payload <- paste0("window.plotData = ", json_data, ";\nwindow.plotData.i18n = Object.assign({}, window.plotData.i18n || {}, ", jsonlite::toJSON(i18n,auto_unbox=TRUE), ");\n")
            payload <- gsub("</", "<\\/", payload, fixed=TRUE)
            
            restore_js <- paste0(
                "<script>",
                "try{",
                  "var _k=", shQuote(key), ";",
                  "var _p=window.parent;",
                  "var _st=_p&&_p.__mzD3PlotAccordionState?_p.__mzD3PlotAccordionState[_k]:undefined;",
                  "if(_st!==false){",
                    "var _d=document.querySelector('.mz-d3plot-accordion');",
                    "if(_d)_d.open=true;",
                  "}",
                "}catch(e){",
                  "var _d2=document.querySelector('.mz-d3plot-accordion');",
                  "if(_d2)_d2.open=true;",
                "}",
                "</script>"
            )
            
            body <- paste0(
                "<div id='funnel-wrap'>",
                "  <div class='zoom-row'>",
                "    <input type='range' id='zoomZ' class='thin-slider' min='0.5' max='2.5' step='0.05' value='1.0'>",
                "    <div id='lblZ' class='thumb-lbl'>Z</div>",
                "  </div>",
                "  <div id='fn'></div>",
                "</div>"
            )
            
            paste0(
                "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
                "<style>", metafora_css, "</style></head><body>",
                private$d3PlotAccordionWrap(body, title, key),
                restore_js,
                "<script>", d3_js, "</script>",
                "<script>", metafora_js, "</script>",
                "<script>", payload, "MetaforaD3.renderSunset('#fn', window.plotData);", "</script>",
                "</body></html>"
            )
        },
        
        #' Costruisce il payload JSON per il Leave-One-Out Plot.
        #' Nessuna dipendenza da jsSortStudies: la semantica di ordinamento
        #' e' diversa (basata su delta_estimate, non su fitted/residuals del
        #' modello pieno).
        d3BuildLeave1OutJson = function(res, l1o, mi, ciLevelPct, sortOrder = "none", self=NULL) {
            reverseGroups <- !is.null(self) && isTRUE(self$options$leave1outGroupOrder)
            shadeRows <- is.null(self) || is.null(self$options$leave1outShade) || isTRUE(self$options$leave1outShade)
            self <- .mzTranslationSelf(options=if(is.null(self))NULL else self$options)
            n <- nrow(l1o)
            l1oOrd <- .mzSortLeaveOneOut(l1o,sortOrder,reverseGroups)
            groups <- attr(l1oOrd,'mz_loo_groups',exact=TRUE)
            ordIdx <- attr(l1oOrd,'mz_loo_indices',exact=TRUE)
            studyIds <- if('study_id' %in% names(l1oOrd))l1oOrd$study_id else .mzD3StudyIds(res)[ordIdx]

            remaining <- rep(res$k-1L,n)
            if(!is.null(groups))for(g in groups)remaining[seq.int(g$start,g$end)] <- g$end-g$start
            studies_list <- lapply(seq_len(n), function(i) {
                heterogeneity <- remaining[i]>=2L
                entry <- list(
                    id          = studyIds[i],
                    label       = l1oOrd$slab[i],
                    es          = round(l1oOrd$estimate[i], 4),
                    ci_lo       = round(l1oOrd$ci.lb[i], 4),
                    ci_hi       = round(l1oOrd$ci.ub[i], 4),
                    influential = isTRUE(l1oOrd$influential[i]),
                    pval = l1oOrd$pval[i],
                    tau2 = if(heterogeneity)l1oOrd$tau2[i] else NA_real_,
                    tau = if(heterogeneity)sqrt(l1oOrd$tau2[i]) else NA_real_,
                    I2 = if(heterogeneity)l1oOrd$I2[i] else NA_real_,
                    pi_lo = if('pi.lb'%in%names(l1oOrd))l1oOrd$pi.lb[i]else NULL,
                    pi_hi = if('pi.ub'%in%names(l1oOrd))l1oOrd$pi.ub[i]else NULL
                )
                if (heterogeneity && !is.na(l1oOrd$delta_tau2[i])) entry$delta_tau2 <- round(l1oOrd$delta_tau2[i], 4)
                if (heterogeneity && !is.na(l1oOrd$delta_I2[i]))   entry$delta_i2   <- round(l1oOrd$delta_I2[i], 2)
                entry
            })
            
            pooled_list <- list(
                label = .("Original Pooled Estimate"),
                es    = round(as.numeric(res$beta), 4),
                ci_lo = round(res$ci.lb, 4),
                ci_hi = round(res$ci.ub, 4)
            )
            pooled_list <- c(pooled_list,.mzLooPooledStats(res))
            pooled_list <- c(pooled_list,attr(l1o,'mz_loo_pooled_prediction',exact=TRUE))
            modelLabel <- .mzSensitivityModelLabel(self,res)
            top_list <- list(
                model_label = modelLabel,
                shade = shadeRows,
                prediction = 'pi.lb'%in%names(l1o),
                studies         = studies_list,
                pooled          = pooled_list,
                common_effect = mzEstimatorId(res)%in%c('FE','EE','CE'),
                single_study_omissions = !is.null(groups) && any(vapply(groups,function(g)g$end-g$start+1L==2L,logical(1))),
                groups          = if(is.null(groups))NULL else lapply(groups,function(g)
                    list(label=g$label,start=g$start,end=g$end,pooled=g$pooled)),
                measure         = mi$xlab,
                measure_short   = mi$xlab_short %||% mi$xlab,
                transf_type     = mi$transf_type %||% "identity",
                ci_level        = ciLevelPct,
                xfilter_channel = private$jsXfilterChannelId(res),
                i18n = c(.mzD3InteractionTexts(self),list(
                    'p-value'=.('p-value'),
                    'Test of the effect against the null'=.('Test of the effect against the null'),
                    'Between-study variance on the analysis scale'=.('Between-study variance on the analysis scale'),
                    'Between-study standard deviation on the analysis scale'=.('Between-study standard deviation on the analysis scale'),
                    'Heterogeneity as a percentage'=.('Heterogeneity as a percentage'),
                    'Not estimable'=.('Not estimable'),
                    'Original subgroup estimate'=.('Original subgroup estimate'),
                    'Common-effect model: tau-squared and tau are not estimated.'=
                        .('Common-effect model: tau-squared and tau are not estimated.'),
                    'With one study remaining, metafor uses z inference; heterogeneity is not estimable.'=
                        .('With one study remaining, metafor uses z inference; heterogeneity is not estimable.'),
                    'Each study is omitted and compared within its own subgroup.'=
                        .('Each study is omitted and compared within its own subgroup.'),
                    'At least two studies per subgroup are required; non-estimable results are shown as missing.'=
                        .('At least two studies per subgroup are required; non-estimable results are shown as missing.')))
            )
            
            jsonlite::toJSON(top_list, auto_unbox = TRUE, na = "null", digits = NA)
        },
        
        d3BuildLeave1OutDoc = function(d3_js, metafora_js, json_data, title, key, i18n = list(), renderer = 'renderLeave1Out', bodyHtml = NULL) {
            metafora_css <- private$jsReadLib("metaforad3.min.css")
            
            payload <- paste0("window.plotData = ", json_data, ";\nwindow.plotData.i18n = Object.assign({}, window.plotData.i18n || {}, ", jsonlite::toJSON(i18n,auto_unbox=TRUE), ");\n")
            payload <- gsub("</", "<\\/", payload, fixed=TRUE)
            
            restore_js <- paste0(
                "<script>",
                "try{",
                  "var _k=", shQuote(key), ";",
                  "var _p=window.parent;",
                  "var _st=_p&&_p.__mzD3PlotAccordionState?_p.__mzD3PlotAccordionState[_k]:undefined;",
                  "if(_st!==false){",
                    "var _d=document.querySelector('.mz-d3plot-accordion');",
                    "if(_d)_d.open=true;",
                  "}",
                "}catch(e){",
                  "var _d2=document.querySelector('.mz-d3plot-accordion');",
                  "if(_d2)_d2.open=true;",
                "}",
                "</script>"
            )
            
            body <- if (!is.null(bodyHtml)) bodyHtml else paste0(
                "<div id='loo-wrap'>",
                "  <div class='zoom-row'>",
                "    <div class='zoom-slider-container'>",
                "      <input type='range' id='zoomZ' class='thin-slider' min='0.5' max='2.5' step='0.05' value='1.0'>",
                "      <div id='lblZ' class='thumb-lbl'>Z</div>",
                "    </div>",
                "  </div>",
                "  <div id='loo'></div>",
                "</div>"
            )
            
            paste0(
                "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
                "<style>", metafora_css, "</style>",
                "</head><body>",
                private$d3PlotAccordionWrap(body, title, key),
                restore_js,
                "<script>", d3_js, "</script>",
                "<script>", metafora_js, "</script>",
                "<script>", payload, "MetaforaD3.", renderer, "('#loo', window.plotData);", "</script>",
                "</body></html>"
            )
        }
    )
)
