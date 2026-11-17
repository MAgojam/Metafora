test_that('Classic cumulative really draws shading only when requested, with and without groups', {
    nativeForest <- metafor::forest
    for(model in names(model_specs)) for(grouped in c(FALSE,TRUE)) {
        options <- list(cumulativePlotMode='posLorT',cumulativeShade=TRUE)
        if(grouped)options$subgroup <- 'group'
        analysis <- make_analysis(model,extra=options)
        original <- analysis$results$cumulativeResults$state
        render <- function(shade) {
            # Change only the presentation option; the numerical cache is reused.
            display <- testthat::with_mocked_bindings(
                make_analysis(model,extra=utils::modifyList(options,list(cumulativeShade=shade)),
                    previous=snapshot(analysis),changes='cumulativeShade'),
                mzComputeCumulative=function(...)stop('Unexpected cumulative refit'),
                .mzComputeGroupedCumulative=function(...)stop('Unexpected subgroup refit'),
                .package='Metafora')
            expect_identical(display$results$cumulativeResults$state,original)
            path <- tempfile(fileext='.svg')
            grDevices::svg(path,width=10,height=10)
            tryCatch({
                ok <- testthat::with_mocked_bindings(
                    get('mzRenderCumulativeClassic',asNamespace('Metafora'))(
                        display,display$results$cumulativePlot),
                    forest=function(...) {
                        args <- list(...)
                        # A distinctive test-only fill identifies actual native rectangles.
                        args$colshade <- '#00FF00'
                        do.call(nativeForest,args)
                    },.package='metafor')
                expect_true(ok,info=paste(model,grouped,shade))
            },finally=grDevices::dev.off())
            svg <- paste(readLines(path,warn=FALSE),collapse='\n');unlink(path)
            artifactDir <- Sys.getenv('MZ_CUMULATIVE_SHADE_ARTIFACTS')
            if(nzchar(artifactDir))writeLines(svg,file.path(artifactDir,
                paste0(model,'_groups_',grouped,'_shade_',shade,'.svg')))
            sum(grepl('fill[=:]"?rgb\\(0%,[[:space:]]*100%,[[:space:]]*0%\\)',
                strsplit(svg,'\n',fixed=TRUE)[[1]]))
        }
        expect_gt(render(TRUE),0,label=paste(model,grouped,'enabled'))
        expect_equal(render(FALSE),0,info=paste(model,grouped,'disabled'))
        expect_gt(render(TRUE),0,label=paste(model,grouped,'re-enabled'))
        expect_identical(analysis$results$cumulativeResults$state,original)
    }
})
