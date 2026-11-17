# Il PI del prefisso richiede il suo fit, non i soli riassunti di cumul().
.mzCumulativePrediction <- function(fit,result,orderKey,groups=NULL) {
    out <- unclass(result)
    n <- length(out$estimate)
    out$pi.lb <- out$pi.ub <- rep(NA_real_,n)
    sections <- out$.mzGroups
    if(is.null(sections))sections <- list(list(label=NULL,start=1L,end=n))
    for(section in sections) {
        indices <- if(is.null(groups))seq_along(fit$yi.f)else which(as.character(groups)==section$label)
        indices <- indices[order(orderKey[indices])]
        for(position in seq.int(section$start,section$end)) {
            # cumul omette i passi delle righe NA: order conserva il rango
            # del vero prefisso, che puo differire dalla posizione visualizzata.
            step <- if(is.null(out$order))position-section$start+1L else out$order[position]
            if(!is.finite(step)||step<2L||step>length(indices)||!is.finite(out$estimate[position]))next
            prefix <- indices[seq_len(step)]
            refit <- tryCatch(.mzFitSensitivitySubset(fit,prefix),error=function(e)NULL)
            prediction <- .mzSensitivityPrediction(refit)
            out$pi.lb[position] <- prediction$pi_lo
            out$pi.ub[position] <- prediction$pi_hi
        }
    }
    out$.mzPooledPrediction <- .mzSensitivityPrediction(fit)
    class(out) <- class(result)
    out
}

.mzCumulativeDisplayResult <- function(self) {
    cache <- self$results$cumulativeResults$state
    if(!is.list(cache)||is.null(cache$result))return(NULL)
    prediction <- self$results$cumulativePredictionCache$state
    result <- if(isTRUE(self$options$cumulativePrediction)&&is.list(prediction)&&
        identical(prediction$signature,cache$signature))prediction$result else cache$result
    .mzOrderCumulativeGroups(result,self$options$cumulativeGroupOrder)
}

.mzSensitivityModelLabel <- function(self,fit) {
    method <- mzEstimatorId(fit)
    if(!is.function(self$options$translate))self<-list(options=list(translate=function(text,n=1)text))
    paste0(if(method%in%c('FE','EE','CE')).('Common-effect model')else .('Random-effects model'),
        ' [',method,']')
}
