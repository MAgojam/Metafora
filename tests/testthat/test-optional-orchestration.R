# Confronta il percorso backend con i renderer sui risultati nativi del modello.
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name, 'preserves TOST and TES parameters in shared orchestration'), {
        analysis <- make_analysis(name,extra=list(
            showTost=TRUE, tostLower=-.2, tostUpper=.3, tostAlpha=.04,
            showTes=TRUE, tesH0=.1, tesAlpha=.04, tesAlternative='two.sided',
            level=90))
        ns <- asNamespace('Metafora')
        fit <- analysis$modelResults
        expected <- get('mzHtmlTost',ns)(fit,lower=-.2,upper=.3,alpha=.04,self=analysis)
        expect_match(analysis$results$outHtmlTost$content,gsub('(?s)<style>.*?</style>','',expected,perl=TRUE),fixed=TRUE)
        expected <- get('mzHtmlTes',ns)(fit,H0=.1,alternative='two.sided',alpha=.04,
                               test=analysis$options$tesTest,self=analysis)
        expect_match(analysis$results$outHtmlTes$content,gsub('(?s)<style>.*?</style>','',expected,perl=TRUE),fixed=TRUE)
        # metafor memorizza alpha, non la percentuale del livello di confidenza.
        expect_equal(fit$level,.10)
    })
})

test_that('optional services do not evaluate an inactive or unavailable model', {
    ns <- asNamespace('Metafora')
    unavailable <- list(options=list(showTost=FALSE,showTes=FALSE,showSelmodel=FALSE))
    expect_null(get('.mzComputeTOST',ns)(unavailable))
    expect_null(get('.mzComputeTES',ns)(unavailable))
    expect_null(get('.mzFitSelectionModel',ns)(unavailable))
    unavailable$options <- list(showTost=TRUE,showTes=TRUE,showSelmodel=TRUE)
    expect_null(get('.mzComputeTOST',ns)(unavailable))
    expect_null(get('.mzComputeTES',ns)(unavailable))
    expect_null(get('.mzFitSelectionModel',ns)(unavailable))
})

test_that('selection orchestration preserves step thresholds and rejects failed fits', {
    fit <- metafor::rma(yi=model_data()$yi,vi=model_data()$vi)
    seen <- list()
    local_mocked_bindings(selmodel=function(x,type,...) {
        seen <<- c(list(x=x,type=type),list(...))
        list(marker='selected')
    },.package='metafor')
    run <- get('.mzFitSelectionModel',asNamespace('Metafora'))
    self <- list(modelResults=fit,options=list(showSelmodel=TRUE,
        selModelType='stepfun',translate=function(text,n=1)text))
    expect_identical(run(self),list(marker='selected'))
    expect_identical(seen$x,fit)
    expect_equal(seen$steps,c(.01,.025,.05,1))
    self$options$selModelType <- 'halfnorm'
    run(self)
    expect_null(seen$steps)
    expect_identical(seen$type,'halfnorm')
    local_mocked_bindings(selmodel=function(...)stop('controlled failure'),.package='metafor')
    expect_error(run(self),'did not converge')
})

test_that('p-uniform failures remain escaped and do not become silent blank output', {
    local_mocked_bindings(puniform=function(...)stop('<script>failure</script>'),.package='puniform')
    analysis <- make_analysis('mzDichotomous',extra=list(showPuniform=TRUE))
    output <- analysis$results$outHtmlPuniform$content
    expect_match(output,'&lt;script&gt;failure&lt;/script&gt;',fixed=TRUE)
    expect_false(grepl('<script>failure',output,fixed=TRUE))
})
