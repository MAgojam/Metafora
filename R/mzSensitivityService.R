# Rifit condivisi delle analisi di sensibilita sulla scala numerica del modello.
.mzFitSensitivitySubset <- function(fit,idx) {
    args <- list(yi=fit$yi.f[idx],vi=fit$vi.f[idx],slab=fit$slab[idx],
        measure=fit$measure,method=mzEstimatorId(fit),test=fit$test,
        level=fit$level,weighted=fit$weighted,control=fit$control)
    if(!is.null(fit$weights.f))args$weights <- fit$weights.f[idx]
    if(isTRUE(fit$tau2.fix))args$tau2 <- fit$tau2
    suppressWarnings(do.call(mzFitRma,args))
}

# Ordinamento interno condiviso fra tabella HTML e grafico D3 LOO.
.mzSortLeaveOneOut <- function(data,order='none',reverseGroups=FALSE) {
    sortPart <- function(idx) {
        x <- data[idx,,drop=FALSE]
        key <- switch(order,shift=-abs(x$delta_estimate),estimate=x$estimate,
            pval=x$pval,study=x$slab,seq_along(idx))
        idx[base::order(key,seq_along(idx),na.last=TRUE)]
    }
    groups <- attr(data,'mz_loo_groups',exact=TRUE)
    if(!is.null(groups)) {
        labels <- vapply(groups,function(g)enc2utf8(g$label),character(1))
        groups <- groups[base::order(labels,decreasing=isTRUE(reverseGroups),method='radix')]
    }
    idx <- if(is.null(groups))sortPart(seq_len(nrow(data))) else
        unlist(lapply(groups,function(g)sortPart(seq.int(g$start,g$end))),use.names=FALSE)
    out <- data[idx,,drop=FALSE]
    if(!is.null(groups)) {
        offset <- 0L
        groups <- lapply(groups,function(g) {
            size <- g$end-g$start+1L
            g$start <- offset+1L;g$end <- offset+size;offset <<- offset+size
            g
        })
    }
    attr(out,'mz_loo_groups') <- groups
    attr(out,'mz_loo_indices') <- idx
    out
}

.mzLeaveOneOutResults <- function(self,service) {
    fit <- self$modelResults
    if(is.null(fit) || !inherits(fit,'rma') || !isTRUE(fit$int.only))return(NULL)
    if(length(self$options$subgroup)==0L)return(service$mzComputeLeave1Out(fit))
    groups <- self$dataProc[['subgroup']]
    if(length(groups)!=fit$k.all || anyNA(groups))stop('Leave-one-out subgroups are misaligned.')
    labels <- sort(unique(as.character(groups)),method='radix')
    ids <- .mzD3StudyIds(fit)
    parts <- vector('list',length(labels));panels <- vector('list',length(labels));offset <- 0L
    for(j in seq_along(labels)) {
        idx <- which(as.character(groups)==labels[j]);subfit <- NULL;error <- NULL
        part <- tryCatch({
            subfit <- .mzFitSensitivitySubset(fit,idx)
            if(length(idx)<2L)stop('At least two studies are required within a subgroup.')
            value <- service$mzComputeLeave1Out(subfit)
            if(is.null(value) || nrow(value)!=length(idx))stop('Native leave-one-out calculation failed.')
            value
        },error=function(e) {error <<- conditionMessage(e);NULL})
        if(is.null(part)) {
            mark('[LOO/SG] unavailable',labels[j],error, .loc = "mzSensitivityService.R:59")
            fields <- c('estimate','se','stat','pval','ci.lb','ci.ub','tau2','I2','H2','QE','QEp',
                        'delta_estimate','influential','delta_tau2','delta_I2')
            part <- as.data.frame(setNames(rep(list(rep(NA_real_,length(idx))),length(fields)),fields))
            part$slab <- as.character(fit$slab[idx])
            part$influential <- rep(NA,length(idx))
        }
        part$subgroup <- labels[j];part$study_id <- ids[idx]
        pooled <- if(is.null(subfit))list(es=NA_real_,ci_lo=NA_real_,ci_hi=NA_real_) else
            list(es=as.numeric(subfit$beta),ci_lo=as.numeric(subfit$ci.lb),ci_hi=as.numeric(subfit$ci.ub))
        panels[[j]] <- list(label=labels[j],start=offset+1L,end=offset+nrow(part),pooled=c(pooled,.mzLooPooledStats(subfit)),error=error)
        parts[[j]] <- part;offset <- offset+nrow(part)
    }
    out <- do.call(rbind,parts);rownames(out) <- NULL
    attr(out,'mz_loo_groups') <- panels
    out
}

# Statistiche del fit originale per le righe pooled LOO; eterogeneita non
# stimabile con un solo studio. Nessuna back-trasformazione di tau/tau2.
.mzLooPooledStats <- function(fit) {
    if(is.null(fit))return(as.list(setNames(rep(NA_real_,10),c('pval','tau2','tau','I2','se','stat','H2','Q','Qp','k'))))
    tau2 <- if(fit$k<2L||mzEstimatorId(fit)%in%c('FE','EE','CE'))NA_real_ else as.numeric(fit$tau2)
    list(pval=as.numeric(fit$pval),tau2=tau2,tau=sqrt(tau2),
         I2=if(fit$k<2L)NA_real_ else as.numeric(fit$I2),
         se=as.numeric(fit$se),stat=as.numeric(fit$zval),
         H2=if(fit$k<2L)NA_real_ else as.numeric(fit$H2),
         Q=if(fit$k<2L)NA_real_ else as.numeric(fit$QE),
         Qp=if(fit$k<2L)NA_real_ else as.numeric(fit$QEp),k=fit$k)
}


# Compatibilita headless delle opzioni precedenti; il frontend migra e azzera
# leave1outMode alla prima apertura, cosi entrambe le nuove scelte possono essere spente.
.mzLeaveOneOutViews <- function(options) {
    legacy <- options$leave1outMode
    list(plot=.mzLeaveOneOutPlotMode(options)!='posOff',
         table=isTRUE(options$leave1outTableShow)||identical(legacy,'posLorT'))
}

.mzLeaveOneOutPlotMode <- function(options) {
    mode <- options$leave1outPlotMode
    if(length(mode)==1L&&mode%in%c('posLorT','posRorB'))return(mode)
    if(isTRUE(options$leave1outPlotShow)||identical(options$leave1outMode,'posRorB'))'posRorB'else'posOff'
}

# Cache persistente indipendente da visualizzazione, ordinamento e zebra.
.mzCachedLeaveOneOut <- function(self,service) {
    fit<-self$modelResults
    if(is.null(fit)||!inherits(fit,'rma')||!isTRUE(fit$int.only))return(NULL)
    groups<-if(length(self$options$subgroup))self$dataProc[['subgroup']]else NULL
    signature<-digest::digest(list(version=1L,measure=fit$measure,yi=fit$yi.f,vi=fit$vi.f,
        slab=fit$slab,ids=.mzD3StudyIds(fit),notna=fit$not.na,subset=fit$subset,
        weights=fit$weights.f,method=mzEstimatorId(fit),weighted=fit$weighted,
        test=fit$test,level=fit$level,tauFixed=fit$tau2.fix,
        tau=if(isTRUE(fit$tau2.fix))fit$tau2 else NULL,control=fit$control,groups=groups),
        algo='sha256',serializeVersion=2L)
    output<-self$results$leave1outResultsCache
    cache<-output$state
    if(is.list(cache)&&identical(cache$signature,signature))result<-cache$result else {
        result<-.mzLeaveOneOutResults(self,service)
        if(!is.null(result))output$setState(list(version=1L,signature=signature,result=result))
    }
    if(!is.null(result)&&isTRUE(self$options$leave1outPrediction)&&
       !mzEstimatorId(fit)%in%c('FE','EE','CE')) {
        predictionCache<-self$results$leave1outPredictionCache
        saved<-predictionCache$state
        if(!is.list(saved)||!identical(saved$signature,signature)||!identical(saved$version,1L)) {
            saved<-list(version=1L,signature=signature,
                result=.mzLeaveOneOutPrediction(self,fit,result))
            predictionCache$setState(saved)
        }
        result<-saved$result
    }
    # Uniforma anche le vecchie cache FE: zero nel fit e un vincolo, non una stima.
    if(!is.null(result)&&mzEstimatorId(fit)%in%c('FE','EE','CE')) {
        result$tau2<-rep(NA_real_,nrow(result))
        panels<-attr(result,'mz_loo_groups',exact=TRUE)
        if(!is.null(panels)) {
            panels<-lapply(panels,function(panel) {
                panel$pooled$tau2<-NA_real_;panel$pooled$tau<-NA_real_;panel
            })
            attr(result,'mz_loo_groups')<-panels
        }
    }
    result
}

# Intervallo nativo per il vero effetto in un nuovo studio, senza nuova varianza
# campionaria. Non sostituire con CI; con un solo studio tau2 non e stimabile.
.mzSensitivityPrediction <- function(fit) {
    missing<-list(pi_lo=NA_real_,pi_hi=NA_real_)
    if(is.null(fit)||fit$k<2L||mzEstimatorId(fit)%in%c('FE','EE','CE'))return(missing)
    value<-tryCatch(suppressWarnings(stats::predict(fit)),error=function(e)NULL)
    if(is.null(value)||length(value$pi.lb)!=1L||length(value$pi.ub)!=1L||
       !all(is.finite(c(value$pi.lb,value$pi.ub))))return(missing)
    list(pi_lo=as.numeric(value$pi.lb),pi_hi=as.numeric(value$pi.ub))
}

.mzLeaveOneOutPrediction <- function(self,fit,result) {
    result$pi.lb<-result$pi.ub<-rep(NA_real_,nrow(result))
    panels<-attr(result,'mz_loo_groups',exact=TRUE)
    groups<-if(length(self$options$subgroup))self$dataProc[['subgroup']]else NULL
    ids<-.mzD3StudyIds(fit)
    sections<-if(is.null(panels))list(list(label=NULL,start=1L,end=nrow(result)))else panels
    for(j in seq_along(sections)) {
        g<-sections[[j]]
        idx<-if(is.null(groups))seq_along(fit$yi.f)else which(as.character(groups)==g$label)
        original<-if(is.null(groups))fit else tryCatch(.mzFitSensitivitySubset(fit,idx),error=function(e)NULL)
        for(pos in seq.int(g$start,g$end)) {
            omitted<-if('study_id'%in%names(result))match(result$study_id[pos],ids)else pos
            if(is.na(omitted)||!omitted%in%idx||!is.finite(result$estimate[pos]))next
            remaining<-setdiff(idx,omitted)
            if(sum(fit$not.na[remaining])<2L)next
            refit<-tryCatch(.mzFitSensitivitySubset(fit,remaining),error=function(e)NULL)
            p<-.mzSensitivityPrediction(refit)
            result$pi.lb[pos]<-p$pi_lo;result$pi.ub[pos]<-p$pi_hi
        }
        p<-.mzSensitivityPrediction(original)
        if(!is.null(panels))panels[[j]]$pooled<-c(panels[[j]]$pooled,p)
        else attr(result,'mz_loo_pooled_prediction')<-p
    }
    attr(result,'mz_loo_groups')<-panels
    result
}

mzRunLeaveOneOut <- function(self) {
    views<-.mzLeaveOneOutViews(self$options)
    plot<-self$results$outD3Leave1OutPlot;table<-self$results$outHtmlLeave1Out
    classic<-self$results$leave1outClassicPlot
    plot$setVisible(FALSE);table$setVisible(FALSE);classic$setVisible(FALSE)
    if(!views$plot&&!views$table)return(invisible(NULL))
    fit<-self$modelResults
    if(is.null(fit)||jmvcore::isError(fit)||!isTRUE(fit$int.only))return(invisible(NULL))
    result<-self$leave1outResults
    if(is.null(result))return(invisible(NULL))
    mode<-.mzLeaveOneOutPlotMode(self$options)
    plot$setVisible(mode=='posRorB');table$setVisible(views$table);classic$setVisible(mode=='posLorT')
    if(mode=='posRorB')mzHtmlD3Service$new()$renderLeave1Out(self,plot,fit,result)
    if(mode=='posLorT') {
        prepared<-.mzPrepareLeaveOneOutClassic(self,result,fit)
        self$results$leave1outClassicCache$setState(prepared)
        classic$setSize(1040,as.integer(max(480,180+length(prepared$rows$es)*25)))
    }
    if(views$table) {
        built<-mzBuildLeaveOneOutTable(self,result,fit,self$options$leave1outOrder)
        if(!table$isFilled()||!identical(table$content,built$html))table$setContent(built$html)
    }
    invisible(NULL)
}
