test_that('common-effect LOO keeps native inference and consistently omits unestimated tau', {
    f<-function(n)get(n,asNamespace('Metafora'))
    for(model in names(model_specs))for(grouped in c(FALSE,TRUE)) {
        opts<-list(modelest='FE',leave1outPlotMode='posLorT',leave1outTableShow=TRUE)
        if(grouped)opts$subgroup<-'group'
        a<-make_analysis(model,extra=opts)
        r<-a$leave1outResults
        expect_true(all(is.na(r$tau2)))
        classic<-a$results$leave1outClassicCache$state
        expect_true(classic$common_effect)
        expect_true(all(is.na(classic$rows$tau2)))
        expect_true(all(is.na(classic$rows$tau)))
        built<-f('mzBuildLeaveOneOutTable')(a)
        expect_false(any(c('Tau2','Tau')%in%names(built$data)))
        expect_false(any(vapply(built$payload$columns,function(col)col$id%in%c('Tau2','Tau'),logical(1))))
        expect_match(built$html,'Common-effect model: tau-squared and tau are not estimated.',fixed=TRUE)
        expect_true(any(is.finite(classic$rows$i2)))
        if(!grouped) {
            native<-metafor::leave1out(a$modelResults)
            expect_equal(r$estimate,as.numeric(native$estimate))
            expect_equal(r$ci.lb,as.numeric(native$ci.lb))
            expect_equal(r$ci.ub,as.numeric(native$ci.ub))
            expect_equal(r$I2,as.numeric(native$I2))
        }
        path<-tempfile(fileext='.pdf');grDevices::pdf(path,width=13,height=10)
        tryCatch(expect_true(f('mzRenderLeaveOneOutClassic')(a,NULL)),
            finally={grDevices::dev.off();unlink(path)})
        b<-make_analysis(model,extra=utils::modifyList(opts,list(leave1outPlotMode='posRorB')),
            previous=snapshot(a),changes='leave1outPlotMode')
        payload<-sens_payload(b$results$outD3Leave1OutPlot)
        expect_true(payload$common_effect)
        expect_true(all(vapply(payload$studies,function(s)is.null(s$tau2)&&is.null(s$tau),logical(1))))
        if(grouped)expect_true(all(vapply(payload$groups,function(g)is.null(g$pooled$tau2)&&is.null(g$pooled$tau),logical(1))))
        else expect_null(payload$pooled$tau2)
    }
})

test_that('LOO pooled tau remains estimated for RE and unavailable for common-effect variants', {
    f<-get('.mzLooPooledStats',asNamespace('Metafora'))
    d<-model_data()
    for(method in c('FE','EE','CE','REML')) {
        fit<-metafor::rma(d$yi,d$vi,method=method)
        values<-f(fit)
        if(method=='REML')expect_equal(values$tau2,as.numeric(fit$tau2))
        else {expect_true(is.na(values$tau2));expect_true(is.na(values$tau))}
        expect_equal(values$I2,as.numeric(fit$I2))
    }
})
