cum_fun <- function(name)get(name,asNamespace('Metafora'))
cum_state <- function(a)a$results$cumulativeResults$state$result
cum_values <- function(table,column)sens_values(table,column)

for(model in names(model_specs)) local({
    name <- model
    test_that(paste(name,'cumulative steps match explicit native refits'),{
        a <- make_analysis(name,extra=list(cumulativeTableShow=TRUE,cumulativeOrder='se',
            cumulativeDirection='desc'))
        fit <- a$modelResults;r <- cum_state(a)
        expect_s3_class(r,'cumul.rma')
        expect_length(r$estimate,fit$k)
        expect_null(r$data)
        ordered <- order(sqrt(fit$vi.f),decreasing=TRUE)
        for(i in seq_along(ordered)) {
            idx <- ordered[seq_len(i)]
            native <- metafor::rma(yi=fit$yi.f[idx],vi=fit$vi.f[idx],method=fit$method,
                test=fit$test,level=fit$level)
            expect_equal(r$estimate[i],as.numeric(native$beta),tolerance=1e-9,ignore_attr=TRUE)
            expect_equal(r$ci.lb[i],native$ci.lb,tolerance=1e-9,ignore_attr=TRUE)
            expect_equal(r$ci.ub[i],native$ci.ub,tolerance=1e-9,ignore_attr=TRUE)
        }
        expect_equal(tail(r$estimate,1),as.numeric(fit$beta),ignore_attr=TRUE)
        expect_equal(cum_values(a$results$cumulativeTable,'estimate'),as.numeric(r$estimate))
        expect_false(a$results$items$cumulativeNotice$visible)
    })

    test_that(paste(name,'cumulative table survives aesthetic reload without refitting'),{
        a <- make_analysis(name,extra=list(cumulativeTableShow=TRUE))
        prior <- snapshot(a)
        local_mocked_bindings(mzComputeCumulative=function(...)stop('Unexpected cumulative refit'),.package='Metafora')
        b <- make_analysis(name,extra=list(cumulativeTableShow=TRUE,cumulativeShade=TRUE),
            previous=prior,changes='cumulativeShade')
        expect_equal(cum_state(b),cum_state(a))
        expect_equal(sens_count(b$results$cumulativeTable),sens_count(a$results$cumulativeTable))
        expect_equal(cum_values(b$results$cumulativeTable,'estimate'),cum_values(a$results$cumulativeTable,'estimate'))
        expect_false(b$results$items$cumulativeNotice$visible)
        # Cambiare solo il disegno conserva i valori della tabella HTML.
        expect_true(b$results$cumulativeTable$isFilled())
    })

    test_that(paste(name,'renders a restored Classic image without reading the dataset'),{
        spec <- model_specs[[name]]
        extra <- list(cumulativePlotMode='posLorT',cumulativeShade=TRUE)
        a <- make_analysis(name,extra=extra)
        args <- utils::modifyList(c(list(slab='slab',modelmea=spec$measure),as.list(spec$vars)),extra)
        options <- do.call(cum_fun(paste0(name,'Options'))$new,args)
        b <- cum_fun(paste0(name,'Class'))$new(options=options,data=NULL)
        data <- model_data()
        b$.setReadDatasetHeaderSource(function(vars)data[0,intersect(unlist(vars),names(data)),drop=FALSE])
        b$.setReadDatasetSource(function(...)stop('Classic must use only the restored cache'))
        b$init();b$results$fromProtoBuf(snapshot(a),character(),character());b$postInit()
        directory <- tempfile('cumulative_image_');dir.create(directory)
        on.exit(unlink(directory,recursive=TRUE),add=TRUE)
        b$.setResourcesPathSource(function(name,suffix)list(rootPath=directory,relPath=paste0(name,'.',suffix)))
        expect_false(b$results$cumulativePlot$requiresData)
        expect_true(b$.createImage('.mzCumulativePlot',b$results$cumulativePlot))
        expect_false(is.null(b$results$cumulativePlot$filePath))
        if(!is.null(b$results$cumulativePlot$filePath))
            expect_gt(file.info(file.path(directory,b$results$cumulativePlot$filePath))$size,1000)
    })
})

test_that('cumulative sort variable preserves missing values and aligns complete cases',{
    data <- model_data();data$year <- c(2005,2001,2001,NA,2009,2002,2004,2003,NA,2007,2006,2008,2000)
    data$ai[2] <- NA_real_
    a <- make_analysis('mzDichotomous',data=data,extra=list(cumulativeTableShow=TRUE,
        cumulativeOrder='variable',cumulativeSortVariable='year'))
    processed <- a$dataProc
    sorted <- order(data$year[match(rownames(processed),rownames(data))],na.last=TRUE)
    expect_equal(cum_state(a)$slab,as.character(processed$slab[sorted]))
    expect_equal(tail(cum_state(a)$k,1),a$modelResults$k)
    expect_true(a$results$items$cumulativeNotice$visible)
    b <- make_analysis('mzDichotomous',data=data,extra=list(cumulativeTableShow=TRUE,
        cumulativeOrder='variable',cumulativeSortVariable='year',cumulativeDirection='desc'))
    sorted <- order(-data$year[match(rownames(processed),rownames(data))],seq_len(nrow(processed)),na.last=TRUE)
    expect_equal(cum_state(b)$slab,as.character(processed$slab[sorted]))
})

test_that('cumulative preserves factor level ordering and stable ties',{
    a <- make_analysis('mzDichotomous')
    values <- factor(c('b','a','b',rep('c',10)),levels=c('c','b','a'),ordered=TRUE)
    key <- cum_fun('mzCumulativeOrder')(a$modelResults,values)
    expect_equal(order(key),order(values,seq_along(values)))
    key <- cum_fun('mzCumulativeOrder')(a$modelResults,values,TRUE)
    expect_equal(order(key),order(-xtfrm(values),seq_along(values)))
})

test_that('editing the ordering variable changes prefixes but not the fitted model',{
    data <- model_data();data$year <- seq_len(nrow(data))
    extra <- list(cumulativeTableShow=TRUE,cumulativeOrder='variable',cumulativeSortVariable='year')
    a <- make_analysis('mzDichotomous',data=data,extra=extra)
    data$year <- rev(data$year)
    b <- make_analysis('mzDichotomous',data=data,extra=extra,previous=snapshot(a),variables='year')
    expect_equal(b$modelResults$beta,a$modelResults$beta)
    expect_equal(cum_state(b)$slab,rev(cum_state(a)$slab))
    expect_false(identical(cum_state(b)$estimate,cum_state(a)$estimate))
    expect_equal(tail(cum_state(b)$estimate,1),tail(cum_state(a)$estimate,1))
})

test_that('cumulative missing selection and moderator gates hide stale output',{
    a <- make_analysis('mzDichotomous',extra=list(cumulativeTableShow=TRUE,
        cumulativePlotMode='posRorB',cumulativeOrder='variable'))
    expect_true(a$results$items$cumulativeNotice$visible)
    expect_false(a$results$cumulativeTable$visible)
    expect_false(a$results$outD3CumulativePlot$visible)
    a <- make_analysis('mzDichotomous',extra=list(cumulativeTableShow=TRUE,
        cumulativePlotMode='posRorB',moderator='mod'))
    expect_null(a$results$cumulativeResults$state)
    expect_true(a$results$items$cumulativeNotice$visible)
    expect_false(a$results$cumulativeTable$visible)
    expect_false(a$results$outD3CumulativePlot$visible)
})

test_that('cumulative respects all inference types, levels and the SJ2 initialization',{
    for(method in c('FE','DL','REML','SJ2')) for(test in if(method=='FE')'z' else c('z','t','knha','adhoc')) {
        a <- make_analysis('mzEffectSizes',extra=list(modelest=method,testType=test,level=99,
            cumulativeTableShow=TRUE,cumulativeOrder='se'))
        r <- cum_state(a);f <- a$modelResults
        expect_equal(tail(r$estimate,1),as.numeric(f$beta),ignore_attr=TRUE)
        expect_equal(tail(r$ci.lb,1),f$ci.lb,ignore_attr=TRUE)
        expect_equal(tail(r$ci.ub,1),f$ci.ub,ignore_attr=TRUE)
        if(method=='SJ2') {
            idx <- order(sqrt(f$vi.f))
            for(i in 2:length(idx)) {
                q <- cum_fun('mzFitRma')(yi=f$yi.f[idx[1:i]],vi=f$vi.f[idx[1:i]],
                    method='SJ2',test=f$test,level=f$level)
                expect_equal(r$tau2[i],q$tau2,ignore_attr=TRUE)
            }
        }
    }
})

test_that('cumulative display transforms estimates without changing inference or PFT scale',{
    for(pair in list(c('mzDichotomous','OR'),c('mzCorrelations','ZCOR'),c('mzProportions','PLO'),
        c('mzProportions','PFT'),c('mzAlphas','AHW'))) {
        a <- make_analysis(pair[1],extra=list(modelmea=pair[2],showBackTransform=TRUE,
            cumulativeTableShow=TRUE,cumulativePlotMode='posRorB'))
        r <- cum_state(a);m <- cum_fun('mzCumulativeMeasure')(a)
        expected <- if(is.function(m$atransf))m$atransf(r$estimate) else r$estimate
        expect_equal(cum_values(a$results$cumulativeTable,'estimate'),as.numeric(expected))
        payload <- cum_fun('mzCumulativePayload')(a,r)
        expect_equal(vapply(payload$steps,`[[`,numeric(1),'es'),as.numeric(r$estimate))
        expect_match(a$results$outD3CumulativePlot$content,'data-mz-stable-plot=',fixed=TRUE)
        if(pair[2]=='PFT')expect_identical(payload$transf_type,'identity')
    }
})

test_that('cumulative is lazy when disabled and classic renders without original data',{
    local_mocked_bindings(mzComputeCumulative=function(...)stop('Should remain lazy'),.package='Metafora')
    a <- make_analysis('mzDichotomous')
    expect_null(a$results$cumulativeResults$state)
})

test_that('cumulative classic uses cached result after dataset is released',{
    a <- make_analysis('mzDichotomous',extra=list(cumulativeTableShow=TRUE,cumulativePlotMode='posLorT'))
    path <- tempfile(fileext='.pdf');grDevices::cairo_pdf(path,width=10,height=8)
    on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
    a$.__enclos_env__$private$.data <- NULL
    expect_true(cum_fun('mzRenderCumulativeClassic')(a,a$results$cumulativePlot))
})
