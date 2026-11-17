# Test nativo hettest: nessun surrogato Q e nessuna ristima del fit principale.
.mzComputeHetTest <- function(fit,iterations=1000L,seed=20261115L) {
    if(!inherits(fit,'rma.uni') || !isTRUE(fit$int.only) || !fit$method %in% c('ML','REML'))
        stop('Heteroscedasticity testing requires ML or REML without moderators.',call.=FALSE)
    if(fit$k<2L)stop('At least two included studies are required for heteroscedasticity testing.',call.=FALSE)
    if(length(iterations)!=1L || !is.numeric(iterations) || !is.finite(iterations) ||
       iterations!=floor(iterations) || iterations<100 || iterations>100000)
        stop('Bootstrap samples must be a whole number between 100 and 100000.',call.=FALSE)
    # Il seed del test non altera la sequenza casuale delle altre analisi.
    hadSeed <- exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
    if(hadSeed)oldSeed <- get('.Random.seed',envir=.GlobalEnv)
    on.exit({if(hadSeed)assign('.Random.seed',oldSeed,envir=.GlobalEnv)
             else if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv)},add=TRUE)
    warnings <- character()
    result <- withCallingHandlers(metafor::hettest(fit,test='score',boot=as.integer(iterations),
        seed=as.integer(seed),progbar=FALSE),warning=function(w) {
            warnings <<- c(warnings,conditionMessage(w));invokeRestart('muffleWarning')
        })
    if(length(result$pval)!=1L || !is.finite(result$pval) || !is.finite(result$statistic[1]))
        stop('The native heteroscedasticity test did not return a finite statistic and p-value.',call.=FALSE)
    valid <- sum(!is.na(result$statistic.boot[,1]))
    if(valid<1L)stop('No usable bootstrap iterations were obtained.',call.=FALSE)
    list(ok=TRUE,k=fit$k,method=result$method,test='Score',statistic=result$statistic[1],
         df=result$df[1],p=result$pval[1],valid=valid,iterations=result$iter,seed=seed,
         warnings=unique(warnings))
}

mzRunHeteroscedasticityTest <- function(self,fit,service=NULL) {
    table <- self$results$hetTestTable
    supported <- isTRUE(fit$int.only) && fit$method %in% c('ML','REML') && length(self$options$moderator)==0L
    table$setVisible(FALSE)
    if(!isTRUE(self$options$showHetTest) || !supported)return(invisible(NULL))
    # La chiave contiene gli input numerici completi: non basta il solo pooled effect.
    key <- list(version=2L,fit=fit[c('yi','vi','b','tau2','k','int.only','method')],
                iterations=self$options$hetTestBoot,seed=20261115L)
    cache <- self$results$hetTestResults$state
    fresh <- !is.list(cache) || !identical(cache$key,key)
    if(fresh) {
        result <- tryCatch(.mzComputeHetTest(fit,iterations=key$iterations),error=function(e)list(ok=FALSE,message=conditionMessage(e)))
        self$results$hetTestResults$setState(list(key=key,result=result))
    } else result <- cache$result
    if(!isTRUE(result$ok)) {
        if(fresh && !is.null(service))service$mzTriggerToast(self$results$toastTrigger,
            message=result$message,type='warning',duration=8000,theme='light',title='Heteroscedasticity test')
        return(invisible(result))
    }
    table$setVisible(TRUE)
    mzSetTableRowIfChanged(table,list(k=result$k,method=result$method,test=result$test,
        statistic=result$statistic,df=result$df,p=result$p,
        bootstrap=paste(result$valid,'/',result$iterations)),rowNo=1)
    note <- paste0('H0: a common true-effect heterogeneity variance across studies (homoscedasticity). ',
        'This is not the Q test for zero heterogeneity. Native metafor Score test; parametric bootstrap with ',
        result$iterations,' iterations (successful/total shown), seed ',result$seed,
        '. The bootstrap p-value has Monte Carlo uncertainty. One overall test, not separate subgroup tests.',
        if(length(result$warnings))paste0(' Native warning: ',paste(result$warnings,collapse='; '))else '')
    table$setNote('method',note,init=FALSE)
    invisible(result)
}
