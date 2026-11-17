# Probabilita' predittive: soglia naturale, calcolo nativo metafor e stato Image.
# Il fit principale e' gia' disponibile: non si ristima nel servizio o nel disegno.
mzPredictiveProbabilityScale <- function(type,measure) {
    identityScale <- function(label)list(label=label,forward=identity,inverse=identity,
        domain=function(x)TRUE)
    ratio <- function(label)list(label=label,forward=log,inverse=exp,
        domain=function(x)x>0)
    bounded <- function(label,forward=identity,inverse=identity,lower=0,upper=1)
        list(label=label,forward=forward,inverse=inverse,domain=function(x)x>lower && x<upper)
    if(type=='dichotomous')return(switch(measure,
        RR=ratio('Risk ratio'),OR=ratio('Odds ratio'),
        RD=identityScale('Risk difference'),AS=identityScale('Arcsine difference (radians)'),
        stop('This effect measure is not supported for predictive probabilities.',call.=FALSE)))
    if(type=='meandiff')return(switch(measure,
        ROM=ratio('Ratio of means'),MD=identityScale('Mean difference'),
        SMD=,SMDH=,SMD1=,SMD1H=identityScale('Standardized mean difference'),
        stop('This effect measure is not supported for predictive probabilities.',call.=FALSE)))
    if(type=='correlation')return(if(measure=='ZCOR')
        bounded('Correlation',metafor::transf.rtoz,metafor::transf.ztor,-1,1) else
        bounded('Correlation',lower=-1,upper=1))
    if(type=='alpha')return(switch(measure,
        ARAW=bounded("Cronbach's alpha"),
        AHW=bounded("Cronbach's alpha",metafor::transf.ahw,metafor::transf.iahw),
        ABT=bounded("Cronbach's alpha",metafor::transf.abt,metafor::transf.iabt),
        stop('This alpha measure is not supported for predictive probabilities.',call.=FALSE)))
    if(type=='proportion')return(switch(measure,
        PR=bounded('Proportion'),PLN=bounded('Proportion',log,exp),
        PLO=bounded('Proportion',metafor::transf.logit,metafor::transf.ilogit),
        PRZ=bounded('Proportion',qnorm,pnorm),
        PAS=bounded('Proportion',metafor::transf.arcsin,metafor::transf.iarcsin),
        PFT=stop('Predictive probabilities on the proportion scale are unavailable for PFT: its inverse requires a reference sample size. Select another proportion measure.',call.=FALSE),
        stop('This proportion measure is not supported for predictive probabilities.',call.=FALSE)))
    identityScale('Effect size (input scale)')
}

mzComputePredictiveProbability <- function(fit,type,measure,threshold,direction) {
    if(!inherits(fit,'rma.uni') || !isTRUE(fit$int.only))
        stop('Predictive probabilities currently require a random-effects model without moderators.',call.=FALSE)
    if(fit$method %in% c('FE','EE','CE'))
        stop('Select a random-effects estimator to obtain predictive probabilities.',call.=FALSE)
    if(fit$k<2L)stop('At least two included studies are required.',call.=FALSE)
    if(length(threshold)!=1L || !is.numeric(threshold) || !is.finite(threshold))
        stop('Enter a finite numeric effect threshold.',call.=FALSE)
    if(!direction %in% c('below','above'))stop('Choose Below threshold or Above threshold.',call.=FALSE)
    scale <- mzPredictiveProbabilityScale(type,measure)
    if(!scale$domain(threshold))stop(paste0('The threshold is outside the supported range for ',scale$label,
        '. Ratios must be positive; correlations must be between -1 and 1; proportions and alpha must be between 0 and 1 (endpoints excluded).'),call.=FALSE)
    if(measure=='RD' && abs(threshold)>1)stop('Risk-difference thresholds must be between -1 and 1.',call.=FALSE)
    if(measure=='AS' && abs(threshold)>pi/2)stop('Arcsine-difference thresholds must be between -pi/2 and pi/2.',call.=FALSE)
    cutoff <- scale$forward(threshold)
    if(!is.finite(cutoff))stop('The threshold cannot be transformed to the fitted analysis scale.',call.=FALSE)
    operator <- if(direction=='below')'<' else '>'
    # Nessun testo utente viene valutato: prob contiene solo operatore e numero validati.
    probArg <- paste0(operator,formatC(cutoff,digits=17,format='g',decimal.mark='.'))
    pred <- stats::predict(fit,prob=probArg)
    if(length(pred$prob)!=1L || !is.finite(pred$prob) ||
       length(pred$pi.se)!=1L || !is.finite(pred$pi.se) || pred$pi.se<=0)
        stop('A finite predictive distribution could not be obtained from this model.',call.=FALSE)
    centre <- as.numeric(pred$pred);spread <- as.numeric(pred$pi.se)
    isT <- identical(pred$pi.dist,'t')
    quantile <- if(isT)function(p)qt(p,df=pred$pi.ddf) else qnorm
    density <- if(isT)function(x)dt(x,df=pred$pi.ddf) else dnorm
    limits <- centre+spread*quantile(c(.005,.995))
    if(any(!is.finite(limits)) || diff(limits)<=0)
        stop('The predictive distribution has no finite plotting range.',call.=FALSE)
    x <- sort(unique(c(seq(limits[1],limits[2],length.out=1001),centre,
        if(cutoff>=limits[1] && cutoff<=limits[2])cutoff else numeric())))
    y <- density((x-centre)/spread)/spread
    ticks <- pretty(limits,n=6);ticks <- ticks[ticks>=limits[1] & ticks<=limits[2]]
    tickLabels <- format(signif(scale$inverse(ticks),3),trim=TRUE)
    list(ok=TRUE,k=fit$k,method=fit$method,test=fit$test,measure=measure,
         label=scale$label,threshold=threshold,direction=direction,cutoff=cutoff,
         probability=as.numeric(pred$prob),centre=centre,spread=spread,
         distribution=pred$pi.dist,df=pred$pi.ddf,x=x,y=y,limits=limits,
         ticks=ticks,tickLabels=tickLabels,
         transformed=!isTRUE(all.equal(cutoff,threshold)) || measure %in% c('RR','OR','ROM','ZCOR','PLN','PLO','PRZ','PAS','AHW','ABT'))
}

mzUpdatePredictiveProbability <- function(self,fit) {
    opt <- self$options
    # Come LOO: le selezioni si conservano, ma con moderatori non si pubblicano risultati.
    hasModerators <- length(opt$moderator)>0L
    self$results$predProbTable$setVisible(isTRUE(opt$predProbTableShow) && !hasModerators)
    self$results$mzPredProbPlot$setVisible(isTRUE(opt$predProbPlotShow) && !hasModerators)
    if(!isTRUE(opt$predProbTableShow) && !isTRUE(opt$predProbPlotShow))return(invisible(NULL))
    # Solo gli input che determinano probabilita'/curva, non le preferenze estetiche.
    key <- list(version=1L,fit=fit[c('b','vb','tau2','k','p','test','ddf','method')],
        type=opt$tabbedControlModelType,measure=opt$modelmea,
        threshold=opt$predProbThreshold,direction=opt$predProbDirection,
        moderators=opt$moderator)
    cache <- self$results$predProbResults$state
    if(is.list(cache) && identical(cache$key,key))result <- cache$result else {
        result <- tryCatch(mzComputePredictiveProbability(fit,opt$tabbedControlModelType,
            opt$modelmea,opt$predProbThreshold,opt$predProbDirection),
            error=function(e)list(ok=FALSE,message=conditionMessage(e)))
        self$results$predProbResults$setState(list(key=key,result=result))
    }
    if(hasModerators) {
        if(isTRUE(opt$predProbPlotShow))self$results$mzPredProbPlot$setState(result)
        return(invisible(result))
    }
    table <- self$results$predProbTable
    values <- list(k=fit$k,measure=opt$modelmea,threshold=opt$predProbThreshold,
        direction=if(opt$predProbDirection=='below')'Below' else 'Above',
        probability=if(isTRUE(result$ok))100*result$probability else NA_real_)
    if(isTRUE(opt$predProbTableShow)) {
        mzSetTableRowIfChanged(table,values,rowNo=1)
        table$setNote('meaning',if(isTRUE(result$ok))paste0(
            'Threshold on the ',result$label,' scale. Model-based probability for the true effect of a new study from the same population, not a probability about the pooled mean. The native metafor predictive distribution includes uncertainty in the estimated mean. Subgroup selection affects the included cases; this result describes the overall model, not separate subgroup predictions.') else result$message,init=FALSE)
    }
    image <- self$results$mzPredProbPlot
    if(isTRUE(opt$predProbPlotShow) && !identical(image$state,result))image$setState(result)
    invisible(result)
}

mzRenderPredictiveProbability <- function(image) {
    state <- image$state
    old <- graphics::par(mar=c(6.2,4.7,4.5,1.3));on.exit(graphics::par(old),add=TRUE)
    if(!isTRUE(state$ok)) {
        graphics::plot.new()
        message <- if(is.null(state$message))'Run the analysis to prepare predictive probabilities.' else state$message
        graphics::text(.5,.5,paste(strwrap(message,width=76),collapse='\n'),cex=1,col='#222222')
        return(TRUE)
    }
    graphics::plot(state$x,state$y,type='n',xlab='',ylab='Predictive density (analysis scale)',
        xaxt='n',ylim=c(0,max(state$y)*1.18),bty='l',main='Predictive probability')
    graphics::abline(h=pretty(c(0,max(state$y))),col='#e4eaf2',lwd=.7)
    selected <- if(state$direction=='below')state$x<=state$cutoff else state$x>=state$cutoff
    if(any(selected))graphics::polygon(c(min(state$x[selected]),state$x[selected],max(state$x[selected])),
        c(0,state$y[selected],0),col='#3e6da94d',border=NA)
    graphics::lines(state$x,state$y,col='#3e6da9',lwd=2)
    graphics::axis(1,at=state$ticks,labels=state$tickLabels)
    graphics::mtext(paste0(state$label,if(state$transformed)' (transformed analysis axis)'else ''),1,line=2.4)
    shown <- max(state$limits[1],min(state$limits[2],state$cutoff))
    graphics::abline(v=shown,col='#ed7546',lwd=2,lty=2)
    operator <- if(state$direction=='below')'<' else '>'
    title <- sprintf('P(true effect %s %s) = %.1f%%',operator,
        format(signif(state$threshold,5),trim=TRUE),100*state$probability)
    graphics::mtext(title,3,line=.45,col='#3e6da9',font=2)
    note <- paste0(state$method,'; k = ',state$k,'; ',
        if(state$distribution=='t')paste0('t predictive distribution (df = ',format(state$df),').')else 'Normal predictive distribution.',
        ' Curve displays the central 99%; the probability uses the full distribution.',
        if(shown!=state$cutoff)' Threshold lies beyond the displayed range.'else '')
    graphics::mtext(paste(strwrap(note,width=100),collapse='\n'),1,line=4.5,cex=.95,col='#222222')
    TRUE
}
