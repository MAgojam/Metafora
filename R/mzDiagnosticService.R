# Servizi condivisi per dimensioni e cache dei grafici diagnostici Classic.
# Le dimensioni nominali mantengono i valori iniziali già usati dal modulo.
.mzDiagnosticSpecs <- list(
    mzInfDiagPlot = list(show="infDiagPlotShow", w=600, h=500, bx=0, by=0),
    mzInfQQPlot = list(show="infQQPlotShow", w=600, h=500, bx=90, by=132),
    mzLhPlot = list(show="lhPlotShow", w=600, h=450, bx=90, by=132),
    mzTostPlot = list(show="tostPlotShow", w=600, h=450, bx=90, by=132),
    mzScatterPlot = list(show="scatterPlotShow", w=600, h=600, bx=0, by=0),
    mzLabbePlot = list(show="labbePlotShow", w=600, h=600, bx=90, by=132),
    mzRadialPlot = list(show="radialPlotShow", w=600, h=500, bx=90, by=132),
    mzBaujatPlot = list(show="baujatPlotShow", w=600, h=500, bx=90, by=132),
    mzGoshPlot = list(show="goshPlotShow", w=600, h=500, bx=0, by=0),
    mzPcurvePlot = list(show="pcurvePlotShow", w=NULL, h=NULL)
)

# Nei grafici singoli B riserva lo spazio di riferimento per gli assi
# (circa 1.24 x 1.84 pollici a 72 dpi, margini base R a 12 pt).
# Influence, Scatter e GOSH gestiscono internamente più pannelli:
# non si inventa per loro un'unica fascia fissa esterna al layout.
.mzSetDiagnosticSize <- function(image, spec) {
    if (is.null(image) || is.null(spec$w) || is.null(spec$h)) return(invisible(NULL))
    if (is.function(image$setSize2)) {
        image$setSize2(spec$w - spec$bx, spec$h - spec$by, spec$bx, spec$by)
    } else {
        image$setSize(spec$w, spec$h)
    }
}

# Non azzera lo stato: il ripristino Protobuf decide quali risultati sono validi.
mzInitPlots <- function(self) {
    if (missing(self) || is.null(self)) return(invisible(NULL))
    for (name in names(.mzDiagnosticSpecs)) {
        .mzSetDiagnosticSize(self$results[[name]], .mzDiagnosticSpecs[[name]])
    }
}

mzUpdatePlots <- function(self) {
    if (missing(self) || is.null(self)) return(invisible(NULL))
    model <- self$modelResults
    if (is.null(model) || jmvcore::isError(model)) return(invisible(NULL))
    for (name in names(.mzDiagnosticSpecs)) {
        spec <- .mzDiagnosticSpecs[[name]]
        image <- self$results[[name]]
        if (is.null(image) || !isTRUE(self$options[[spec$show]])) next
        if (!identical(image$state, model)) image$setState(model)
        .mzSetDiagnosticSize(image, spec)
    }
}

# value è valutato soltanto su cache miss; clearWith governa l'invalidazione.
.mzCachedDiagnostic <- function(element, value) {
    if (is.null(element)) return(value)
    if (!is.null(element$state)) {
        jinfo("[CACHE/DIAG] hit", .loc = "mzDiagnosticService.R:54")
        return(element$state)
    }
    jinfo("[CACHE/DIAG] miss", .loc = "mzDiagnosticService.R:57")
    result <- value
    element$setState(result)
    result
}

mzDiagnosticInfluence <- function(self, model) {
    .mzCachedDiagnostic(self$results$influenceResults, mzInfluence(model))
}

mzDiagnosticGosh <- function(self, model) {
    .mzCachedDiagnostic(self$results$goshResults,
                        mzGosh(model, subsets=self$options$goshPlotSubsets, progbar=FALSE))
}

# Seleziona coefficienti numerici reali, evitando l'intercetta e i termini fattoriali.
# Il contesto self esplicito permette la traduzione dei testi del grafico.
mzDiagnosticMessage <- function(message) {
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar),add=TRUE)
    graphics::par(mfrow=c(1,1),mar=c(1,1,1,1))
    graphics::plot.new()
    graphics::text(.5,.5,paste(strwrap(message,width=60),collapse='\n'),
                   cex=.95,col='#5B6573')
    TRUE
}

mzDiagnosticScatterRender <- function(self,image) {
    tryCatch(mzDiagnosticScatter(self,image),error=function(e) {
        mark("[SCATTER] failed", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzDiagnosticService.R:86")
        mzDiagnosticMessage(.("The scatter/bubble plot could not be rendered. Check the selected moderators and plot settings."))
    })
}

mzDiagnosticScatter <- function(self, image) {
    model <- image$state
    if (is.null(model) || !inherits(model, "rma") || is.null(model$formula.mods))
        return(mzDiagnosticMessage(.("Scatter/bubble plots require a fitted model with at least one continuous moderator.")))
    data <- self$dataProc
    variables <- unique(unlist(self$options$moderator, use.names=FALSE))
    variables <- variables[vapply(variables, function(name) {
        name %in% names(data) && is.numeric(data[[name]]) &&
            any(rownames(model$beta) %in% c(name, paste0("`", name, "`")))
    }, logical(1))]
    if (length(variables) == 0L)
        return(mzDiagnosticMessage(.("Scatter/bubble plots require at least one continuous moderator with an estimable coefficient.")))
    jinfo("[SCATTER] render", "panels =", length(variables), ui_log_obj=self$results$debugLog, .loc = "mzDiagnosticService.R:103")
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar), add=TRUE)
    graphics::par(mfrow=c(ceiling(length(variables)/2), min(2L,length(variables))),
                  mar=c(5.1,4.1,3.1,2.1))
    transform <- .mzGetBackTransform(self$options$tabbedControlModelType,
                                    self$options$modelmea,isTRUE(self$options$showBackTransform),ni=data$ni)
    for (name in variables) {
        index <- which(rownames(model$beta) %in% c(name,paste0("`",name,"`")))[1L]
        metafor::regplot(model, mod=index,
                        ci=isTRUE(self$options$scatterPlotCI), pi=isTRUE(self$options$scatterPlotPI),
                        legend=isTRUE(self$options$scatterPlotLegend), grid=isTRUE(self$options$scatterPlotGrid),
                        label=if(isTRUE(self$options$scatterPlotLabel)) "all" else FALSE,
                        psize=if(isTRUE(self$options$scatterPlotScale)) "seinv" else 1,
                        transf=transform$atransf,targs=transform$targs,
                        xlab=name,ylab=self$options$translate(transform$xlab),
                        col="#3E6DA9",bg="#3E6DA966",shade=c("#3E6DA933","#E8735233"),
                        col.lab="#3E6DA9",font.lab=3)
        graphics::title(main=sprintf(.("Moderator: %s"),name),col.main="#3E6DA9",cex.main=1.1)
    }
    TRUE
}
