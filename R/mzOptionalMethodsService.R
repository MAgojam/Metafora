# Orchestrazione condivisa dei metodi opzionali dei sei modelli.
# I renderer HTML restano separati dai contratti delle librerie statistiche.

.mzComputeTOST <- function(self) {
    if (!isTRUE(self$options$showTost)) return(invisible(NULL))
    res <- self$modelResults
    if (is.null(res) || jmvcore::isError(res)) return(invisible(NULL))
    jinfo("[OPT/TOST] render", .loc = "mzOptionalMethodsService.R:8")
    .mzSetHtmlResult(self, self$results$outHtmlTost, 'tost', mzHtmlTost(
        self=self, res=res, lower=self$options$tostLower,
        upper=self$options$tostUpper, alpha=self$options$tostAlpha))
}

.mzComputeTES <- function(self) {
    if (!isTRUE(self$options$showTes)) return(invisible(NULL))
    res <- self$modelResults
    if (is.null(res) || jmvcore::isError(res)) return(invisible(NULL))
    jinfo("[OPT/TES] render", .loc = "mzOptionalMethodsService.R:18")
    .mzSetHtmlResult(self, self$results$outHtmlTes, 'tes', mzHtmlTes(
        self=self, res=res, H0=self$options$tesH0,
        alternative=self$options$tesAlternative, alpha=self$options$tesAlpha,
        test=self$options$tesTest))
}

# Conserva soglie, gestione degli errori e contesto di traduzione dei backend.
.mzFitSelectionModel <- function(self) {
    if (!isTRUE(self$options$showSelmodel)) return(NULL)
    res <- self$modelResults
    if (is.null(res) || jmvcore::isError(res)) return(NULL)
    jinfo("[OPT/SEL] fit", .loc = "mzOptionalMethodsService.R:30")
    selected <- try(if (self$options$selModelType == "stepfun") {
        metafor::selmodel(res, type=self$options$selModelType,
                         steps=c(0.01, 0.025, 0.05, 1))
    } else {
        metafor::selmodel(res, type=self$options$selModelType)
    }, silent=TRUE)
    if (is.null(selected) || jmvcore::isError(selected)) {
        mark("[OPT/SEL] failed", list(result_class=class(selected)), .loc = "mzOptionalMethodsService.R:38")
        jmvcore::reject(
            .("selection model '{smt}' did not converge with the current data."),
            code="", smt=self$options$selModelType)
        return(NULL)
    }
    selected
}

# Correlation usa ri/ni: non sostituirli con gli effetti trasformati del modello.
.mzComputePuniform <- function(self) {
    data <- self$dataProc
    if (is.null(data) || nrow(data) == 0) return(invisible(NULL))
    args <- if (identical(self$options$tabbedControlModelType, "correlation")) {
        list(ri=data$ri, ni=data$ni)
    } else {
        list(yi=self$effectsize, vi=self$variance)
    }
    args <- c(args, list(side=self$options$puniformSide,
                         method=self$options$methodPunif,
                         alpha=self$options$alphaPunif))
    jinfo("[OPT/PUNIF] compute", .loc = "mzOptionalMethodsService.R:59")
    result <- try(do.call(puniform::puniform, args), silent=TRUE)
    if (inherits(result, "try-error")) {
        message <- attr(result, "condition")$message
        if (is.null(message)) message <- as.character(result)
        mark("[OPT/PUNIF] failed", message, .loc = "mzOptionalMethodsService.R:64")
        result <- list(error=TRUE, message=message)
    }
    .mzSetHtmlResult(self, self$results$outHtmlPuniform, 'puniform', mzHtmlPuniform(result, self$options))
}
