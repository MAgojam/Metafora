sg_fun <- function(name)get(name,asNamespace('Metafora'))
sg_state <- function(a)a$results$cumulativeResults$state$result
for (model in names(model_specs)) local({
    name <- model
    test_that(paste(name,'subgroup cumulative prefixes match independent native fits'),{
        d <- model_data();d$year <- c(2002,2001,NA,2001,2007,2003,2004,2005,2006,2002,NA,2000,2008)
        a <- make_analysis(name,data=d,extra=list(subgroup='group',cumulativeTableShow=TRUE,
            cumulativeOrder='variable',cumulativeSortVariable='year',cumulativeDirection='desc',level=99))
        r <- sg_state(a);f <- a$modelResults
        expect_length(r$.mzGroups,2L)
        for(g in r$.mzGroups) {
            idx <- which(as.character(a$dataProc$subgroup)==g$label)
            years <- d$year[match(rownames(a$dataProc)[idx],rownames(d))]
            idx <- idx[order(is.na(years),-years,seq_along(years),na.last=TRUE)]
            positions <- g$start:g$end
            expect_equal(r$slab[positions],as.character(f$slab[idx]))
            expect_equal(r$k[positions],seq_along(idx))
            expect_equal(r$.mzStep[positions],seq_along(idx))
            for(i in seq_along(idx)) {
                selected <- idx[seq_len(i)]
                q <- suppressWarnings(metafor::rma(yi=f$yi.f[selected],vi=f$vi.f[selected],
                    method=f$method,test=f$test,level=f$level))
                position <- positions[i]
                for(pair in list(c('estimate','beta'),c('se','se'),c('ci.lb','ci.lb'),
                    c('ci.ub','ci.ub'),c('zval','zval'),c('pval','pval'),
                    c('tau2','tau2'),c('I2','I2'),c('Q','QE')))
                    expect_equal(as.numeric(r[[pair[1]]][position]),as.numeric(q[[pair[2]]]),tolerance=1e-9)
            }
            expect_equal(g$final,as.numeric(a$subgroupResults[[g$label]]$beta),tolerance=1e-9)
        }
        expect_true(sens_payload(a$results$cumulativeTable)$grouped)
        expect_equal(unique(sens_values(a$results$cumulativeTable,'subgroup')),c('A','Z'))
        expect_equal(sens_count(a$results$cumulativeTable),nrow(a$dataProc))
        payload <- sg_fun('mzCumulativePayload')(a,r)
        expect_equal(payload$final,as.numeric(f$beta))
        expect_equal(r$.mzPooled,as.numeric(f$beta))
        expect_length(payload$groups,2L)
        # Classic deve funzionare anche dopo che jamovi rilascia il dataset.
        path <- tempfile(fileext='.pdf');grDevices::cairo_pdf(path,width=10,height=8)
        on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
        a$.__enclos_env__$private$.data <- NULL
        expect_true(sg_fun('mzRenderCumulativeClassic')(a,a$results$cumulativePlot))
        captured <- NULL
        testthat::with_mocked_bindings({
            expect_true(sg_fun('mzRenderCumulativeClassic')(a,a$results$cumulativePlot))
        },forest=function(...) {
            captured <<- list(...)$refline
            list(xlim=c(-5,5))
        },.package='metafor')
        expect_equal(captured,as.numeric(f$beta))
    })
    test_that(paste(name,'subgroup cache survives styling and removing subgroup restores overall'),{
        opts <- list(subgroup='group',cumulativeTableShow=TRUE)
        a <- make_analysis(name,extra=opts)
        prior <- snapshot(a)
        b <- testthat::with_mocked_bindings(make_analysis(name,extra=c(opts,list(cumulativeShade=TRUE)),previous=prior,changes='cumulativeShade'),
            .mzComputeGroupedCumulative=function(...)stop('Unexpected grouped refit'),.package='Metafora')
        expect_true(b$results$cumulativeTable$visible)
        expect_false(b$results$items$cumulativeNotice$visible)
        expect_equal(sg_state(a),sg_state(b))
        c <- make_analysis(name,extra=list(cumulativeTableShow=TRUE),previous=snapshot(b),changes='subgroup')
        expect_null(sg_state(c)$.mzGroups)
        expect_false(sens_payload(c$results$cumulativeTable)$grouped)
        expect_true(all(sens_values(c$results$cumulativeTable,'subgroup')==''))
        expect_equal(as.numeric(tail(sg_state(c)$estimate,1)),as.numeric(c$modelResults$beta))
    })
})

test_that('subgroup cumulative supports native inference variants and SJ2 per prefix',{
    for(method in c('FE','DL','REML','SJ2')) for(test in if(method=='FE')'z' else c('z','t','knha','adhoc')) {
        a <- make_analysis('mzEffectSizes',extra=list(subgroup='group',cumulativeTableShow=TRUE,
            modelest=method,testType=test,level=90,cumulativeOrder='se'))
        r <- sg_state(a);f <- a$modelResults
        for(g in r$.mzGroups) {
            idx <- which(as.character(a$dataProc$subgroup)==g$label)
            idx <- idx[order(sqrt(f$vi.f[idx]),seq_along(idx))]
            for(i in seq_along(idx)) {
                j <- g$start+i-1L;take <- idx[seq_len(i)]
                q <- suppressWarnings(sg_fun('mzFitRma')(yi=f$yi.f[take],vi=f$vi.f[take],
                    method=method,test=test,level=90))
                stat <- if(test=='z')'zval' else 'tval'
                for(pair in list(c('estimate','beta'),c('se','se'),c('ci.lb','ci.lb'),
                    c('ci.ub','ci.ub'),c(stat,'zval'),c('pval','pval'),c('I2','I2'),c('Q','QE')))
                    expect_equal(as.numeric(r[[pair[1]]][j]),as.numeric(q[[pair[2]]]),tolerance=1e-8)
                if(method!='FE')expect_equal(as.numeric(r$tau2[j]),as.numeric(q$tau2),tolerance=1e-8)
            }
        }
    }
})

test_that('subgroup editing, singleton groups, missing data and hostile labels remain aligned',{
    d <- model_data();d$ai[2] <- NA_real_
    d$group <- as.character(d$group);d$group[1] <- '</script><img src=x>'
    opts <- list(subgroup='group',cumulativeTableShow=TRUE,cumulativePlotMode='posRorB')
    a <- make_analysis('mzDichotomous',data=d,extra=opts)
    r <- sg_state(a)
    expect_length(r$estimate,nrow(a$dataProc))
    expect_equal(r$.mzGroups[[1]]$start,r$.mzGroups[[1]]$end)
    expect_true(is.finite(r$estimate[1]))
    expect_false(a$results$items$cumulativeNotice$visible)
    d$group <- rep(c('One','Two','Three'),length.out=nrow(d))
    b <- make_analysis('mzDichotomous',data=d,extra=opts,previous=snapshot(a),variables='group')
    expect_equal(vapply(sg_state(b)$.mzGroups,`[[`,character(1),'label'),unique(as.character(b$dataProc$subgroup)))
    expect_false(identical(r$estimate,sg_state(b)$estimate))
})


test_that('grouped display transformations never alter native cumulative inference',{
    for(pair in list(c('mzCorrelations','ZCOR'),c('mzProportions','PLO'),c('mzProportions','PFT'),
                    c('mzDichotomous','OR'),c('mzAlphas','AHW'))) {
        a <- make_analysis(pair[1],extra=list(subgroup='group',modelmea=pair[2],
            cumulativeTableShow=TRUE,showBackTransform=TRUE))
        r <- sg_fun('.mzOrderCumulativeGroups')(sg_state(a));m <- sg_fun('mzCumulativeMeasure')(a)
        expected <- if(is.function(m$atransf))m$atransf(r$estimate) else r$estimate
        cells <- sens_values(a$results$cumulativeTable,'estimate')
        expect_equal(cells,as.numeric(expected))
        payload <- sg_fun('mzCumulativePayload')(a,r)
        expect_equal(vapply(payload$steps,`[[`,numeric(1),'es'),as.numeric(r$estimate))
        if(pair[2]=='PFT')expect_identical(payload$transf_type,'identity')
    }
})

test_that('a failed subgroup stays explicit and cannot contaminate the other group',{
    a <- make_analysis('mzEffectSizes');fit <- a$modelResults
    native <- sg_fun('mzComputeCumulative')
    testthat::local_mocked_bindings(mzComputeCumulative=function(fit,orderKey) {
        if(fit$slab[1]=='Study 1')stop('Synthetic subgroup failure')
        native(fit,orderKey)
    },.package='Metafora')
    r <- sg_fun('.mzComputeGroupedCumulative')(fit,seq_len(fit$k.all),model_data()$group)
    expect_match(r$.mzGroups[[1]]$error,'Synthetic subgroup failure')
    expect_true(all(is.na(r$estimate[1:7])))
    expect_null(r$.mzGroups[[2]]$error)
    expect_true(all(is.finite(r$estimate[8:13])))
})


for(model in names(model_specs))local({
    name <- model
    test_that(paste(name,'group-order toggle changes only presentation and reuses native fits'),{
        opts <- list(subgroup='group',cumulativeTableShow=TRUE,cumulativePlotMode='posRorB')
        a <- make_analysis(name,extra=opts)
        r <- sg_state(a)
        b <- testthat::with_mocked_bindings(
            make_analysis(name,extra=c(opts,list(cumulativeGroupOrder=TRUE)),
                previous=snapshot(a),changes='cumulativeGroupOrder'),
            .mzComputeGroupedCumulative=function(...)stop('Unexpected group refit'),
            mzComputeCumulative=function(...)stop('Unexpected native refit'),.package='Metafora')
        expect_false(b$results$items$cumulativeNotice$visible)
        expect_true(b$results$cumulativeTable$visible)
        expect_identical(sg_state(b),r)
        table <- b$results$cumulativeTable
        values <- function(col,mode)sens_values(table,col,mode)
        expect_equal(values('subgroup',character(1)),c(rep('Z',7),rep('A',6)))
        expect_equal(values('step',numeric(1)),c(1:7,1:6))
        expect_equal(values('k',numeric(1)),c(1:7,1:6))
        expect_equal(values('estimate',numeric(1)),as.numeric(r$estimate))
        ordered <- sg_fun('.mzOrderCumulativeGroups')(r,TRUE)
        expect_equal(ordered$.mzPooled,r$.mzPooled)
        expect_equal(vapply(ordered$.mzGroups,`[[`,character(1),'label'),c('Z','A'))
        expect_equal(vapply(ordered$.mzGroups,`[[`,integer(1),'start'),c(1L,8L))
        captured <- NULL
        path <- tempfile(fileext='.pdf');grDevices::cairo_pdf(path)
        on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
        graphics::plot.new();graphics::plot.window(xlim=c(-5,5),ylim=c(0,30))
        testthat::with_mocked_bindings({
            expect_true(sg_fun('mzRenderCumulativeClassic')(b,b$results$cumulativePlot))
        },forest=function(...) {
            captured <<- list(...)$x$.mzGroups
            list(xlim=c(-5,5))
        },.package='metafor')
        expect_equal(vapply(captured,`[[`,character(1),'label'),c('Z','A'))
        c <- make_analysis(name,extra=opts,previous=snapshot(b),changes='cumulativeGroupOrder')
        expect_identical(sg_state(c),r)
        expect_equal(sens_values(c$results$cumulativeTable,'subgroup')[1],'A')
    })
})


test_that('alloc-style subgroup names are alphabetical and toggling never changes within-group steps',{
    d <- model_data();d$alloc <- rep(c('random','alternate','systematic'),length.out=nrow(d))
    opts <- list(subgroup='alloc',cumulativeTableShow=TRUE)
    a <- make_analysis('mzDichotomous',data=d,extra=opts)
    r <- sg_state(a)
    asc <- sg_fun('.mzOrderCumulativeGroups')(r,FALSE)
    desc <- sg_fun('.mzOrderCumulativeGroups')(r,TRUE)
    expect_equal(vapply(asc$.mzGroups,`[[`,character(1),'label'),c('alternate','random','systematic'))
    expect_equal(vapply(desc$.mzGroups,`[[`,character(1),'label'),c('systematic','random','alternate'))
    expect_equal(sens_values(a$results$cumulativeTable,'subgroup')[1],'alternate')
    for(label in unique(d$alloc)) {
        x<-which(asc$.mzSubgroup==label);y<-which(desc$.mzSubgroup==label)
        expect_equal(asc$estimate[x],desc$estimate[y])
        expect_equal(asc$slab[x],desc$slab[y])
        expect_equal(asc$.mzStep[x],desc$.mzStep[y])
    }
})
