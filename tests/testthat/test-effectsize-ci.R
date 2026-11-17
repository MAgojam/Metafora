ci_data <- function(level=95) {
    d <- model_data()
    width <- stats::qnorm((1+level/100)/2)*sqrt(d$vi)
    d$lower <- d$yi-width
    d$upper <- d$yi+width
    d
}
ci_options <- function(...) utils::modifyList(list(meaType='cimt',
    ciLower='lower',ciUpper='upper',vi=NULL),list(...))

test_that('CI input matches variance and SE without requiring a variance column', {
    d <- ci_data()
    variance <- make_analysis('mzEffectSizes',data=d)
    ci <- make_analysis('mzEffectSizes',data=d,extra=ci_options())
    seData <- d;seData$vi <- sqrt(d$vi)
    se <- make_analysis('mzEffectSizes',data=seData,extra=list(meaType='seimt'))
    expect_true(ci$ready)
    expect_equal(ci$variance,d$vi,tolerance=1e-12,ignore_attr=TRUE)
    for(fit in list(ci$modelResults,se$modelResults)) {
        expect_equal(fit$beta,variance$modelResults$beta,tolerance=1e-10)
        expect_equal(fit$ci.lb,variance$modelResults$ci.lb,tolerance=1e-10)
        expect_equal(fit$tau2,variance$modelResults$tau2,tolerance=1e-10)
    }
    d$vi <- NULL
    without <- make_analysis('mzEffectSizes',data=d,extra=ci_options())
    expect_equal(without$modelResults$beta,variance$modelResults$beta,tolerance=1e-10)
})

test_that('input CI level and output CI level have separate effects', {
    d <- ci_data(90)
    a <- make_analysis('mzEffectSizes',data=d,extra=ci_options(inputCiLevel=90,level=95))
    b <- make_analysis('mzEffectSizes',data=d,extra=ci_options(inputCiLevel=90,level=99))
    expect_equal(a$variance,d$vi,tolerance=1e-12,ignore_attr=TRUE)
    expect_equal(a$variance,b$variance)
    expect_equal(a$modelResults$beta,b$modelResults$beta)
    expect_lt(b$modelResults$ci.lb,a$modelResults$ci.lb)
    c <- make_analysis('mzEffectSizes',data=d,extra=ci_options(inputCiLevel=99),
        previous=snapshot(a),changes='inputCiLevel')
    expected <- ((d$upper-d$lower)/(2*qnorm(.995)))^2
    expect_equal(c$variance,expected,tolerance=1e-12,ignore_attr=TRUE)
    expect_false(isTRUE(all.equal(c$modelResults$se,a$modelResults$se)))
})

test_that('CI changes invalidate derived results including cumulative and influence caches', {
    d <- ci_data()
    opts <- ci_options(cumulativeTableShow=TRUE,leave1outTableShow=TRUE)
    a <- make_analysis('mzEffectSizes',data=d,extra=opts)
    before <- snapshot(a)
    d$upper <- d$upper+.3;d$lower <- d$lower-.3
    b <- make_analysis('mzEffectSizes',data=d,extra=opts,previous=before,variables=c('lower','upper'))
    expect_gt(b$modelResults$se,a$modelResults$se)
    expect_equal(tail(b$results$cumulativeResults$state$result$ci.ub,1),b$modelResults$ci.ub,
        tolerance=1e-10,ignore_attr=TRUE)
    expect_false(isTRUE(all.equal(a$results$cumulativeResults$state$result$ci.ub,
        b$results$cumulativeResults$state$result$ci.ub)))
    expect_false(isTRUE(all.equal(a$leave1outResults$ci.ub,b$leave1outResults$ci.ub)))
})

test_that('CI input works with subgroup, moderator and complete-case filtering', {
    d <- ci_data();d$lower[2] <- NA_real_
    ci <- make_analysis('mzEffectSizes',data=d,extra=ci_options(subgroup='group',moderator='mod'))
    expected <- metafor::rma(yi=d$yi[-2],vi=d$vi[-2],mods=~mod,
        data=d[-2,],method='REML',test='z',level=95)
    expect_equal(ci$modelResults$beta,expected$beta,tolerance=1e-10)
    expect_equal(ci$modelResults$k,12L)
    expect_equal(rownames(ci$dataProc),as.character(setdiff(seq_len(13),2)))
    subgroup <- make_analysis('mzEffectSizes',data=d,extra=ci_options(subgroup='group'))
    expect_gt(subgroup$results$subgroupResults$rowCount,0)
})

test_that('CI mode requires both limits and preserves the normal default mode', {
    ns <- asNamespace('Metafora')
    expect_identical(get('mzEffectSizesOptions',ns)$new()$meaType,'vimt')
    for(missing in c('ciLower','ciUpper')) {
        opts <- ci_options();opts[missing] <- list(NULL)
        a <- make_analysis('mzEffectSizes',data=ci_data(),extra=opts,run=FALSE)
        expect_false(a$ready)
        expect_true(a$results$instructions$visible)
    }
    # Hidden CI columns do not enter complete-case selection in Variance mode.
    d <- ci_data();d$lower[] <- NA_real_
    a <- make_analysis('mzEffectSizes',data=d,extra=list(ciLower='lower',ciUpper='upper'))
    expect_equal(a$modelResults$k,13L)
})

test_that('CI validation rejects reversed, infinite and incompatible bounds', {
    fn <- get('mzEffectSizeInputVariance',asNamespace('Metafora'))
    opts <- list(meaType='cimt',inputCiLevel=95)
    d <- data.frame(yi=.5,ciLower=0,ciUpper=1)
    expect_error(fn(transform(d,ciUpper=0),opts),'smaller')
    expect_error(fn(transform(d,ciLower=Inf),opts),'finite')
    expect_error(fn(transform(d,yi=2),opts),'within')
    expect_error(fn(d,list(meaType='cimt',inputCiLevel=100)),'between')
    expect_error(fn(data.frame(yi=.5,vi=-1),list(meaType='seimt')),'positive')
})

test_that('aesthetic reload restores CI fit without refitting', {
    d <- ci_data()
    a <- make_analysis('mzEffectSizes',data=d,extra=ci_options())
    local_mocked_bindings(mzFitRma=function(...)stop('Unexpected refit'),.package='Metafora')
    b <- make_analysis('mzEffectSizes',data=d,extra=ci_options(shadeRows=TRUE),
        previous=snapshot(a),changes='shadeRows')
    expect_equal(b$modelResults$beta,a$modelResults$beta)
    expect_true(b$results$textRICH$isFilled())
})

test_that('mode switches preserve old inputs and remove or restore the CI note', {
    d <- ci_data()
    opts <- list(meaType='cimt',ciLower='lower',ciUpper='upper')
    a <- make_analysis('mzEffectSizes',data=d,extra=opts)
    expect_match(a$results$textRICH$notes$inputCI$note,'95% input CIs',fixed=TRUE)
    b <- make_analysis('mzEffectSizes',data=d,extra=utils::modifyList(opts,list(meaType='vimt')),
        previous=snapshot(a),changes='meaType')
    expect_null(b$results$textRICH$notes$inputCI)
    expect_identical(b$options$ciLower,'lower')
    expect_equal(b$variance,d$vi,ignore_attr=TRUE)
    c <- make_analysis('mzEffectSizes',data=d,extra=opts,previous=snapshot(b),changes='meaType')
    expect_match(c$results$textRICH$notes$inputCI$note,'95% input CIs',fixed=TRUE)
    expect_equal(c$modelResults$beta,b$modelResults$beta,tolerance=1e-10)
})

test_that('CI mode feeds the Forest table and Interactive plots without vi selected', {
    a <- make_analysis('mzEffectSizes',data=ci_data(),extra=ci_options(
        forestTableShow=TRUE,forestPlotMode='posRorB',funnelPlotMode='posRorB',
        cumulativePlotMode='posRorB',showHEC=TRUE))
    expect_null(a$options$vi)
    expect_false(grepl('Error generating table:',a$results$outHtmlTable$content,fixed=TRUE))
    expect_match(a$results$outHtmlTable$content,'>Var<span',fixed=TRUE)
    expect_match(a$results$outHtmlTable$content,'>SE<span',fixed=TRUE)
    for(name in c('outD3ForestPlot','outD3FunnelPlot','outD3CumulativePlot')) {
        if (inherits(a$results[[name]],'Svg'))
            expect_match(a$results[[name]]$content,'<div class="mz-native-d3"',fixed=TRUE)
        else expect_match(a$results[[name]]$content,'data-mz-stable-plot=',fixed=TRUE)
        expect_false(grepl('Plot error:',a$results[[name]]$content,fixed=TRUE))
    }
    expect_gt(a$results$heterogeneityComparison$rowCount,0)
})
