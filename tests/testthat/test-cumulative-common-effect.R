test_that('FE cumulative hides unestimated tau without changing prefix inference', {
    f<-function(n)get(n,asNamespace('Metafora'))
    for(model in names(model_specs))for(grouped in c(FALSE,TRUE)) {
        opts<-list(modelest='FE',cumulativePlotMode='posLorT',cumulativeTableShow=TRUE)
        if(grouped)opts$subgroup<-'group'
        a<-make_analysis(model,extra=opts)
        cache<-a$results$cumulativeResults$state
        result<-f('.mzCumulativeDisplayResult')(a)
        payload<-f('mzCumulativePayload')(a,result)
        table<-f('mzBuildCumulativeTable')(a,result)
        expect_true(payload$common_effect)
        expect_false(any(c('Tau','Tau2')%in%names(table$data)))
        expect_true(all(vapply(payload$steps,function(s)is.na(s$tau)&&is.na(s$tau2),logical(1))))
        expect_equal(vapply(payload$steps,`[[`,numeric(1),'i2'),table$data$I2)
        expect_equal(vapply(payload$steps,`[[`,numeric(1),'p'),as.numeric(result$pval))
        expect_match(table$html,'Common-effect model: tau-squared and tau are not estimated.',fixed=TRUE)
        nativeForest<-metafor::forest;captured<-NULL
        path<-tempfile(fileext='.pdf');grDevices::cairo_pdf(path,width=13,height=10)
        margins<-par('mar')
        tryCatch(testthat::with_mocked_bindings({
            expect_true(f('mzRenderCumulativeClassic')(a,NULL))
            expect_equal(par('mar'),margins)
        },forest=function(...) {captured<<-list(...);do.call(nativeForest,captured)},.package='metafor'),
            finally={dev.off();unlink(path)})
        expect_equal(ncol(captured$ilab),3L)
        expect_length(captured$ilab.lab,3L)
        expect_identical(a$results$cumulativeResults$state,cache)
        b<-testthat::with_mocked_bindings(make_analysis(model,
            extra=utils::modifyList(opts,list(cumulativePlotMode='posRorB')),
            previous=snapshot(a),changes='cumulativePlotMode'),
            mzComputeCumulative=function(...)stop('Unexpected refit'),.package='Metafora')
        expect_true(sens_payload(b$results$outD3CumulativePlot)$common_effect)
        expect_identical(b$results$cumulativeResults$state,cache)
    }
})
