test_that('cumulative heterogeneity is missing for single-study steps and is never transformed', {
    stats <- get('.mzCumulativeStatistics',asNamespace('Metafora'))
    r <- list(estimate=c(.1,.2,.3),k=c(1,2,3),pval=c(.01,NA,.03),
              tau2=c(0,NA,.25),I2=c(0,NA,50))
    s <- stats(r)
    expect_equal(s$p,r$pval)
    expect_equal(s$tau2,c(NA,NA,.25))
    expect_equal(s$tau,c(NA,NA,.5))
    expect_equal(s$i2,c(NA,NA,50))
    r$tau2 <- NULL
    expect_true(all(is.na(stats(r)$tau2)))
})

for(model in names(model_specs))local({
    name <- model
    test_that(paste(name,'cumulative Classic and D3 statistics match the table in both group modes'), {
        f <- function(n)get(n,asNamespace('Metafora'))
        nativeForest <- metafor::forest
        for(grouped in c(FALSE,TRUE)) {
            opts <- list(cumulativeTableShow=TRUE,cumulativePlotMode='posLorT',
                showBackTransform=TRUE,cumulativeGroupOrder=TRUE,level=99)
            if(grouped)opts$subgroup <- 'group'
            a <- make_analysis(name,extra=opts)
            cache <- a$results$cumulativeResults$state
            r <- f('.mzOrderCumulativeGroups')(cache$result,TRUE)
            table <- f('mzBuildCumulativeTable')(a,r)$data
            payload <- f('mzCumulativePayload')(a,r)
            for(pair in list(c('p','p'),c('tau2','Tau2'),c('tau','Tau'),c('i2','I2'))) {
                values <- vapply(payload$steps,`[[`,numeric(1),pair[1])
                expect_equal(values,table[[pair[2]]],info=paste(name,grouped,pair[1]))
            }
            expect_equal(vapply(payload$steps,`[[`,numeric(1),'p'),as.numeric(r$pval))
            captured <- NULL
            path <- tempfile(fileext='.svg');grDevices::svg(path,width=13,height=10)
            previous <- getOption('na.action')
            tryCatch({
                testthat::with_mocked_bindings({
                    expect_true(f('mzRenderCumulativeClassic')(a,a$results$cumulativePlot))
                },forest=function(...) {
                    captured <<- list(...)
                    do.call(nativeForest,captured)
                },.package='metafor')
            },finally=grDevices::dev.off())
            expect_identical(getOption('na.action'),previous)
            svg <- paste(readLines(path,warn=FALSE),collapse='\n');unlink(path)
            expect_match(svg,'<svg',fixed=TRUE)
            expect_identical(dim(captured$ilab),c(nrow(table),5L))
            for(j in seq_along(c('p','I2','Tau','Tau2'))) {
                column <- c('p','I2','Tau','Tau2')[j]
                expected <- vapply(table[[column]],f('.mzSensitivityFormat'),character(1),id=column)
                expect_equal(unname(captured$ilab[,j+1L]),unname(expected))
            }
            expect_length(captured$ilab.lab,5L)
            expect_true(all(diff(captured$ilab.xpos)>0))
            expect_gt(captured$xlim[2],max(captured$ilab.xpos))
            expect_identical(a$results$cumulativeResults$state,cache)
        }
    })
})
