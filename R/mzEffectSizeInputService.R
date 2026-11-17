# Input alternativi del modello Generic Effect Sizes.
# I CI devono essere Wald normali e gia sulla stessa scala di yi.
mzEffectSizeInputVars <- function(options) {
    if (identical(options$meaType, 'cimt')) c('slab', 'yi', 'ciLower', 'ciUpper')
    else c('slab', 'yi', 'vi')
}

mzEffectSizeInputVariance <- function(data, options, self=NULL) {
    self <- .mzTranslationSelf(self, options)
    if (identical(options$meaType, 'cimt')) {
        level <- options$inputCiLevel
        if (length(level) != 1L || !is.finite(level) || level < 50 || level > 99.9)
            stop(.("Input CI (%) must be between 50 and 99.9."), call.=FALSE)
        if (any(!is.finite(data$yi) | !is.finite(data$ciLower) | !is.finite(data$ciUpper)))
            stop(.("Effect sizes and input CI limits must be finite."), call.=FALSE)
        if (any(data$ciLower >= data$ciUpper))
            stop(.("Each input CI lower limit must be smaller than its upper limit."), call.=FALSE)
        if (any(data$yi < data$ciLower | data$yi > data$ciUpper))
            stop(.("Each effect size must lie within its input confidence interval."), call.=FALSE)
        z <- stats::qnorm((1 + level / 100) / 2)
        data$vi <- ((data$ciUpper - data$ciLower) / (2 * z))^2
        if (any(!is.finite(data$vi) | data$vi <= 0))
            stop(.("Input confidence intervals must yield finite, positive sampling variances."), call.=FALSE)
        jinfo("[INPUT/CI] variance derived", .loc = "mzEffectSizeInputService.R:24")
    } else if (identical(options$meaType, 'seimt')) {
        if (any(!is.finite(data$vi) | data$vi <= 0))
            stop(.("Standard errors must be finite and positive."), call.=FALSE)
        data$vi <- data$vi^2
    }
    data
}

mzEffectSizeInputNote <- function(self, table) {
    ciMode <- identical(self$options$meaType, 'cimt')
    note <- if (ciMode) jmvcore::format(.("Sampling variances are approximated from {level}% input CIs assuming normal-based Wald intervals on the supplied effect-size scale. The output CI level is set separately."),
        level=self$options$inputCiLevel) else NULL
    table$setNote('inputCI', note, init=FALSE)
    invisible(NULL)
}
