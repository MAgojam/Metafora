# Cumulativa condivisa: risultati sulla scala del modello, presentazione separata.
# La funzione pura è verificabile contro metafor senza dipendere da jamovi.
mzComputeCumulative <- function(fit, orderKey) {
    if (!inherits(fit,'rma.uni') || !isTRUE(fit$int.only))
        stop('Cumulative analysis requires a model without moderators.')
    if (length(orderKey)!=fit$k.all || anyNA(orderKey))
        stop('Cumulative ordering must identify every row of the fitted dataset.')
    if (identical(mzEstimatorId(fit),'SJ2')) {
        method <- utils::getS3method('cumul','rma.uni',envir=asNamespace('metafor'))
        result <- do.call(mzSJ2RefitMethod(method),
            list(x=fit,order=orderKey,collapse=FALSE,progbar=FALSE))
    } else {
        result <- do.call(metafor::cumul,
            list(x=fit,order=orderKey,collapse=FALSE,progbar=FALSE))
    }
    # La tabella/grafico non hanno bisogno del dataset originale contenuto in cumul.
    result$data <- NULL
    result
}

# Ogni gruppo riparte da k=1 e usa yi/vi sulla stessa scala del fit completo.
# Nessuna dipendenza dalle call originarie: ordinamento e pesi restano allineati.
.mzComputeGroupedCumulative <- function(fit, values, groups, decreasing=FALSE) {
    if (length(groups)!=fit$k.all || anyNA(groups))
        stop('Cumulative subgroups are unavailable or misaligned.')
    labels <- unique(as.character(groups))
    parts <- vector('list',length(labels)); sections <- vector('list',length(labels))
    offset <- 0L
    for (j in seq_along(labels)) {
        idx <- which(as.character(groups)==labels[j])
        key <- mzCumulativeOrder(list(k.all=length(idx)),values[idx],decreasing)
        failure <- NULL
        part <- tryCatch({
            subfit <- .mzFitSensitivitySubset(fit,idx)
            mzComputeCumulative(subfit,key)
        },error=function(e) {
            failure <<- conditionMessage(e)
            ordered <- idx[order(key)]; n <- length(ordered)
            empty <- list(k=seq_len(n),estimate=rep(NA_real_,n),se=rep(NA_real_,n),
                pval=rep(NA_real_,n),ci.lb=rep(NA_real_,n),ci.ub=rep(NA_real_,n),
                tau2=rep(NA_real_,n),I2=rep(NA_real_,n),slab=fit$slab[ordered],
                ids=seq_len(n),slab.null=FALSE,transf=FALSE,level=fit$level,
                measure=fit$measure,test=fit$test)
            empty[[if(fit$test=='z')'zval' else 'tval']] <- rep(NA_real_,n)
            class(empty) <- c('list.rma','cumul.rma'); empty
        })
        n <- length(part$estimate)
        sections[[j]] <- list(label=labels[j],start=offset+1L,end=offset+n,
            final=as.numeric(tail(part$estimate,1L)),error=failure)
        parts[[j]] <- part; offset <- offset+n
    }
    # list.rma ridefinisce $<- per modifiche ai dati: assemblare come lista semplice.
    resultClass <- class(parts[[1L]])
    result <- unclass(parts[[1L]])
    fields <- c('k','estimate','se','zval','tval','pval','ci.lb','ci.ub','Q','Qp',
                'tau2','I2','H2','slab','ids','order')
    for (field in fields) {
        if (all(vapply(parts,function(p)is.null(p[[field]]),logical(1)))) next
        result[[field]] <- unlist(lapply(parts,function(p) {
            if(is.null(p[[field]]))rep(NA_real_,length(p$estimate)) else p[[field]]
        }),use.names=FALSE)
    }
    result$data <- NULL
    result$.mzPooled <- as.numeric(fit$beta)
    result$.mzGroups <- sections
    result$.mzStep <- unlist(lapply(parts,function(p)seq_along(p$estimate)),use.names=FALSE)
    result$.mzSubgroup <- rep(labels,vapply(parts,function(p)length(p$estimate),integer(1)))
    class(result) <- resultClass
    result
}

# Ordine delle sole sezioni: riusa i risultati nativi, senza nuovi fit o prefissi.
.mzOrderCumulativeGroups <- function(result, reverse=FALSE) {
    groups <- result$.mzGroups
    if(length(groups)<2L)return(result)
    labels <- vapply(groups,`[[`,character(1),'label')
    sections <- groups[order(enc2utf8(labels),decreasing=isTRUE(reverse),method='radix')]
    indices <- unlist(lapply(sections,function(g)seq.int(g$start,g$end)),use.names=FALSE)
    out <- unclass(result)
    for(field in c('k','estimate','se','zval','tval','pval','ci.lb','ci.ub','Q','Qp',
                  'tau2','I2','H2','slab','ids','order','.mzStep','.mzSubgroup','pi.lb','pi.ub'))
        if(!is.null(out[[field]]))out[[field]] <- out[[field]][indices]
    offset <- 0L
    out$.mzGroups <- lapply(sections,function(g) {
        n <- g$end-g$start+1L;g$start <- offset+1L;g$end <- offset+n
        offset <<- offset+n;g
    })
    class(out) <- class(result)
    out
}

# Non eliminare studi con ordinamento mancante: sono aggiunti in coda,
# mantenendo l'ordine originale in caso di parità anche in direzione decrescente.
mzCumulativeOrder <- function(fit,values=NULL,decreasing=FALSE) {
    if (is.null(values)) values <- seq_len(fit$k.all)
    if (length(values)!=fit$k.all) stop('Cumulative ordering is not aligned with the model rows.')
    key <- xtfrm(values)
    if (decreasing) key <- -key
    indices <- order(is.na(key),key,seq_along(key),na.last=TRUE)
    ranks <- integer(length(indices));ranks[indices] <- seq_along(indices)
    ranks
}

mzInitCumulative <- function(self) {
    # Notice è un oggetto runtime: lo schema YAML installato non lo dichiara.
    if (is.null(self$results$items$cumulativeNotice)) {
        notice <- jmvcore::Notice$new(options=self$options,name='cumulativeNotice',
            type=jmvcore::NoticeType$WARNING,visible=FALSE,clearWith=character())
        self$results$insert(match('cumulativeTable',self$results$itemNames),notice)
    }
}

mzCumulativeMeasure <- function(self) {
    .mzGetBackTransform(self$options$tabbedControlModelType,self$options$modelmea,
        isTRUE(self$options$showBackTransform))
}

mzCumulativeNotice <- function(self,text='') {
    notice <- self$results$items$cumulativeNotice
    notice$setContent(text)
    notice$setVisible(nzchar(text))
}

# La cache conserva solo il derivato numerico. Tutto il lavoro con i dati
# originali avviene durante .run; il callback Image usa soltanto questa cache.
mzRunCumulative <- function(self) {
    active <- isTRUE(self$options$cumulativeTableShow) ||
        !identical(self$options$cumulativePlotMode,'posOff')
    mzCumulativeNotice(self)
    mzCumulativeVisibility(self,FALSE)
    if (!active) return(invisible(NULL))
    tryCatch({
        fit <- self$modelResults
        if (is.null(fit) || jmvcore::isError(fit)) return(invisible(NULL))
        if (!isTRUE(fit$int.only)) {
            mzCumulativeNotice(self,.("Cumulative analysis is available only without moderators."))
            return(invisible(NULL))
        }
        kind <- self$options$cumulativeOrder
        missingOrder <- 0L
        values <- switch(kind,original=seq_len(fit$k.all),se=sqrt(fit$vi.f),
            study=fit$slab,variable={
                variable <- self$options$cumulativeSortVariable
                if (is.null(variable) || !nzchar(variable)) {
                    mzCumulativeNotice(self,.("Select a variable to order the cumulative analysis."))
                    return(invisible(NULL))
                }
                source <- self$data
                rows <- match(rownames(self$dataProc),rownames(source))
                if (is.null(source[[variable]]) || anyNA(rows))
                    stop('Cumulative sorting variable is unavailable or misaligned.')
                source[[variable]][rows]
            })
        if (is.null(values)) stop('Unknown cumulative ordering option.')
        missingOrder <- sum(is.na(values))
        orderKey <- mzCumulativeOrder(fit,values,self$options$cumulativeDirection=='desc')
        groups <- if(length(self$options$subgroup)>0L && nzchar(self$options$subgroup))
            self$dataProc[['subgroup']] else NULL
        if(length(self$options$subgroup)>0L && is.null(groups))
            stop('Cumulative subgroup variable is unavailable.')
        signature <- digest::digest(list(fit$measure,fit$yi.f,fit$vi.f,fit$slab,fit$not.na,
            fit$subset,fit$weights.f,mzEstimatorId(fit),fit$weighted,fit$test,fit$level,
            fit$tau2.fix,if(isTRUE(fit$tau2.fix))fit$tau2 else NULL,fit$control,orderKey,
            groups,self$options$subgroup,groupVersion=2L),
            algo='sha256',serializeVersion=2L)
        cache <- self$results$cumulativeResults$state
        if (!is.list(cache) || !identical(cache$signature,signature)) {
            jinfo('[CUM] compute','studies =',fit$k, .loc = "mzCumulativeService.R:168")
            result <- if(is.null(groups))mzComputeCumulative(fit,orderKey) else
                .mzComputeGroupedCumulative(fit,values,groups,self$options$cumulativeDirection=='desc')
            cache <- list(version=1L,signature=signature,result=result)
            self$results$cumulativeResults$setState(cache)
        } else jinfo('[CUM] cache hit', .loc = "mzCumulativeService.R:173")
        if(isTRUE(self$options$cumulativePrediction)&&!mzEstimatorId(fit)%in%c('FE','EE','CE')) {
            saved <- self$results$cumulativePredictionCache$state
            if(!is.list(saved)||!identical(saved$signature,signature)) {
                saved <- list(signature=signature,result=.mzCumulativePrediction(fit,cache$result,orderKey,groups))
                self$results$cumulativePredictionCache$setState(saved)
            }
        }
        result <- .mzCumulativeDisplayResult(self)
        mzCumulativeVisibility(self,TRUE)
        notes <- character()
        if (!is.null(result$.mzGroups)) {
            failed <- vapply(result$.mzGroups,function(g)!is.null(g$error),logical(1))
            if(any(failed)) {
                for(g in result$.mzGroups[failed]) mark('[CUM/SG] failed',g$label,g$error, .loc = "mzCumulativeService.R:187")
                notes <- c(notes,.("Some subgroups could not be estimated with the selected method; their steps are shown as missing."))
            }
        }
        if (missingOrder>0L) notes <- c(notes,.("Missing ordering values are placed last; no studies are removed for this reason."))
        if (any(!is.finite(result$estimate) | !is.finite(result$ci.lb) | !is.finite(result$ci.ub))) notes <- c(notes,
            .("Some cumulative steps could not be estimated with the selected method. Their results are shown as missing."))
        if (length(notes)) mzCumulativeNotice(self,paste(notes,collapse=' '))
        if (isTRUE(self$options$cumulativeTableShow)) mzPopulateCumulativeTable(self,result)
        if (identical(self$options$cumulativePlotMode,'posLorT')) {
            mzUpdateImageSize(self$results$cumulativePlot,result,self$options,
                self$results$cumulativePlotSizeCache,
                function(model,options)list(w=1040L,h=as.integer(max(420,180+(length(model$estimate)+length(model$.mzGroups)*2)*23))))
        } else if (identical(self$options$cumulativePlotMode,'posRorB')) {
            mzHtmlD3Service$new()$renderCumulative(self,result)
        }
    },error=function(e) {
        mzCumulativeVisibility(self,FALSE)
        mark('[CUM] failed',.mzLogErrorText(e), .loc = "mzCumulativeService.R:205")
        mzCumulativeNotice(self,.("Cumulative analysis could not be computed. Check the selected ordering variable and model settings."))
    })
    invisible(NULL)
}

# Nasconde anche risultati precedenti se il nuovo ordine non è utilizzabile.
mzCumulativeVisibility <- function(self,ready) {
    self$results$cumulativeTable$setVisible(ready && isTRUE(self$options$cumulativeTableShow))
    self$results$cumulativePlot$setVisible(ready && identical(self$options$cumulativePlotMode,'posLorT'))
    self$results$outD3CumulativePlot$setVisible(ready && identical(self$options$cumulativePlotMode,'posRorB'))
}

mzPopulateCumulativeTable <- function(self,result) {
    built <- mzBuildCumulativeTable(self,result)
    output <- self$results$cumulativeTable
    if(!output$isFilled() || !identical(output$content,built$html))output$setContent(built$html)
}

# Statistiche sulla scala del fit, condivise da tabella e due grafici.
# Con k=1 il motore produce un effetto, ma non una stima di eterogeneita.
.mzCumulativeStatistics <- function(result) {
    n <- length(result$estimate)
    num <- function(x)if(is.null(x))rep(NA_real_,n)else as.numeric(x)
    tau2 <- num(result$tau2)
    out <- data.frame(p=num(result$pval),tau2=tau2,tau=sqrt(tau2),i2=num(result$I2))
    out[num(result$k)<2L,c('tau2','tau','i2')] <- NA_real_
    out
}

mzRenderCumulativeClassic <- function(self,image) {
    cache <- self$results$cumulativeResults$state
    if (!is.list(cache) || is.null(cache$result) || length(self$options$moderator)>0L)
        return(FALSE)
    tryCatch({
        measure <- mzCumulativeMeasure(self)
        result <- .mzCumulativeDisplayResult(self)
        statistics <- .mzCumulativeStatistics(result)
        commonEffect <- mzEstimatorId(self$modelResults)%in%c('FE','EE','CE')
        # Riserva una riga per la nota FE, ripristinando i margini del device.
        if(commonEffect) {
            margins<-graphics::par('mar');on.exit(graphics::par(mar=margins),add=TRUE)
            graphics::par(mar=margins+c(1,0,0,0))
        }
        trans <- function(x)if(is.function(measure$atransf))measure$atransf(x)else x
        lo <- trans(result$ci.lb);hi <- trans(result$ci.ub)
        effect <- ifelse(is.finite(result$estimate),paste0(
            vapply(trans(result$estimate),.mzSensitivityFormat,character(1),id='Estimate'),' [',
            vapply(pmin(lo,hi),.mzSensitivityFormat,character(1),id='Lower'),', ',
            vapply(pmax(lo,hi),.mzSensitivityFormat,character(1),id='Upper'),']'),.('Not estimable'))
        columns <- cbind(effect,
            vapply(statistics$p,.mzSensitivityFormat,character(1),id='p'),
            vapply(statistics$i2,.mzSensitivityFormat,character(1),id='I2'),
            vapply(statistics$tau,.mzSensitivityFormat,character(1),id='Tau'),
            vapply(statistics$tau2,.mzSensitivityFormat,character(1),id='Tau2'))
        labels <- list(paste0(measure$xlab_short,' [',self$options$level,'% CI]'),
            .('p-value'),quote(I^2~'(%)'),quote(tau),quote(tau^2))
        widths <- c(68,62,64,64)
        if(commonEffect) {columns<-columns[,1:3,drop=FALSE];labels<-labels[1:3];widths<-widths[1:2]}
        # Coordinate ricavate dalle stesse proporzioni delle colonne D3.
        bounds <- c(result$ci.lb,result$ci.ub,result$pi.lb,result$pi.ub);bounds <- bounds[is.finite(bounds)]
        if(!length(bounds))return(FALSE)
        axis <- range(pretty(range(bounds),n=4))
        unit <- diff(axis)/310
        labelWidth <- min(320,max(210,nchar(as.character(result$slab))*6+42))
        columnEnds <- 188+c(0,cumsum(widths))
        limits <- c(axis[1]-unit*(labelWidth+20),axis[2]+unit*(tail(columnEnds,1)+20))
        args <- list(x=result,xlab=measure$xlab,col='#3e6da9',cex=.8,
            refline=if(is.null(result$.mzPooled))as.numeric(tail(result$estimate,1L)) else result$.mzPooled,
            annotate=FALSE,header=.('Study Added'),alim=axis,xlim=limits,
            ilab=columns,ilab.xpos=axis[2]+unit*columnEnds,ilab.pos=2,
            ilab.lab=as.expression(labels),
            atransf=if(is.function(measure$atransf))measure$atransf else FALSE)
        # FALSE scalare attiva zebra; NULL esplicito viene rifiutato da .getx.
        # Omettere shade lascia al metodo nativo il default senza sfondi.
        if(isTRUE(self$options$cumulativeShade))args$shade <- 'zebra'
        sections <- result$.mzGroups
        if(length(sections)) {
            positions <- numeric(length(result$estimate)); headers <- numeric(length(sections)); cursor <- 0
            for(j in seq_along(sections)) {
                g <- sections[[j]]; headers[j] <- cursor+1; cursor <- cursor+1.5
                idx <- g$start:g$end; positions[idx] <- cursor+seq_along(idx)
                cursor <- max(positions[idx])+.7
            }
            args$rows <- cursor+1-positions
            args$ylim <- c(0,cursor+3)
        }
        # Mantiene le righe non stimabili e l'allineamento delle sezioni.
        previous <- options(na.action='na.pass');on.exit(options(previous),add=TRUE)
        plotted <- do.call(metafor::forest,args)
        if(length(sections)) for(j in seq_along(sections))
            graphics::text(plotted$xlim[1],cursor+1-headers[j],
                labels=sections[[j]]$label,pos=4,font=2,col='#3e6da9',cex=.85)
        # Il metodo nativo ordina internamente le righe al contrario.
        y <- if(length(sections))args$rows else rev(seq_along(result$estimate))
        if(!is.null(result$pi.lb)) {
            valid <- is.finite(result$pi.lb)&is.finite(result$pi.ub)
            graphics::segments(result$pi.lb[valid],y[valid]-.24,result$pi.ub[valid],y[valid]-.24,
                col='#507980',lwd=2)
            a<-trans(result$pi.lb[valid]);b<-trans(result$pi.ub[valid])
            piText <- paste0('PI [',vapply(pmin(a,b),.mzSensitivityFormat,character(1),id='Lower'),
                ', ',vapply(pmax(a,b),.mzSensitivityFormat,character(1),id='Upper'),']')
            graphics::text(args$ilab.xpos[1],y[valid]-.3,piText,pos=2,col='#507980',cex=.65)
        }
        graphics::mtext(.mzSensitivityModelLabel(self,self$modelResults),side=1,line=if(commonEffect)3.5 else 4,adj=0,cex=.75,font=2)
        if(commonEffect)graphics::mtext(.('Common-effect model: tau-squared and tau are not estimated.'),
            side=1,line=4.5,adj=0,cex=.65,col='#666666')
        TRUE
    },error=function(e){mark('[CUM] classic failed',.mzLogErrorText(e), .loc = "mzCumulativeService.R:313");FALSE})
}

# Geometria sempre sulla scala del modello; trasformare solo testi e tick,
# come nel Forest esistente. In particolare PFT rimane sempre trasformato.
mzCumulativePayload <- function(self,result) {
    measure <- mzCumulativeMeasure(self)
    value <- function(x,i) if(is.null(x))NA_real_ else as.numeric(x[i])
    statistics <- .mzCumulativeStatistics(result)
    steps <- lapply(seq_along(result$estimate),function(i)list(
        step=if(is.null(result$.mzStep))i else result$.mzStep[i],
        label=as.character(result$slab[i]),k=value(result$k,i),
        es=value(result$estimate,i),ci_lo=value(result$ci.lb,i),ci_hi=value(result$ci.ub,i),
        p=statistics$p[i],tau2=statistics$tau2[i],tau=statistics$tau[i],
        i2=statistics$i2[i],pi_lo=value(result$pi.lb,i),pi_hi=value(result$pi.ub,i)))
    fit <- self$modelResults
    list(steps=steps,ci_level=self$options$level,shade=isTRUE(self$options$cumulativeShade),
        measure=measure$xlab,measure_short=measure$xlab_short,transf_type=measure$transf_type,
        final=as.numeric(fit$beta),
        model_label=.mzSensitivityModelLabel(self,fit),prediction=!is.null(result$pi.lb),
        common_effect=mzEstimatorId(fit)%in%c('FE','EE','CE'),
        groups=if(is.null(result$.mzGroups))NULL else lapply(result$.mzGroups,function(g)
            list(label=g$label,start=g$start,end=g$end,final=g$final)),
        null_value=measure$null_value,
        i18n=c(.mzD3InteractionTexts(self),list(
            'Study Added'=.('Study Added'),'Step'=.('Step'),
            'Cumulative estimate'=.('Cumulative estimate'),
            'Not estimable'=.('Not estimable'),
            'Common-effect model: tau-squared and tau are not estimated.'=
                .('Common-effect model: tau-squared and tau are not estimated.'),
            'p-value'=.('p-value'),
            'Test of the effect against the null'=.('Test of the effect against the null'),
            'Between-study variance on the analysis scale'=.('Between-study variance on the analysis scale'),
            'Between-study standard deviation on the analysis scale'=.('Between-study standard deviation on the analysis scale'),
            'Heterogeneity as a percentage'=.('Heterogeneity as a percentage'),
            'Prediction interval for the true effect in a new study.'=.('Prediction interval for the true effect in a new study.'),
            'Final subgroup estimate'=.('Final subgroup estimate'),
            'Cumulative sequences are fitted separately within each subgroup.'=
                .('Cumulative sequences are fitted separately within each subgroup.'),
            'Final pooled estimate'=.('Final pooled estimate'),
            'Each row adds one study. This is not a sequential stopping test.'=
                .('Each row adds one study. This is not a sequential stopping test.'))))
}
