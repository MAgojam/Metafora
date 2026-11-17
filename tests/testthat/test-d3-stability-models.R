# Gli snapshot identici devono preservare il frame in tutti i modelli.
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'uses stable wrappers for Forest Funnel Sunset and Leave-One-Out'), {
        analysis <- make_analysis(name)
        changedAnalysis <- make_analysis(name,extra=list(modelest='FE'))
        service <- get('mzHtmlD3Service',asNamespace('Metafora'))$new()
        for (kind in c('forest','funnel','sunset','leave1out')) {
            output <- new.env(parent=emptyenv())
            output$visible <- TRUE;output$title <- paste(kind,'Plot');output$content <- ''
            output$setContent <- function(value) output$content <- value
            output$isFilled <- function()nzchar(output$content)
            opts <- list(tabbedControlModelType=analysis$options$tabbedControlModelType,
                modelmea=analysis$options$modelmea,level=95,showBackTransform=FALSE,
                yaxis='sei',yaxisInv=FALSE,sunsetFunnel=kind=='sunset',enhanceFunnel=FALSE,
                forestOrder='fit',addfit=TRUE,addpred=FALSE,shadeRows=FALSE,showweights=FALSE,
                mainTitle='',xAxisTitle='',pchForest='bSquare',steps=5,forestPlotMode='posRorB')
            fake <- list(options=opts,dataProc=analysis$dataProc,
                results=list(outD3ForestPlot=output,outD3FunnelPlot=output,
                    mzForestPlotSizeCache=list(state=NULL)))
            for (variant in c('base','changed')) {
                fit <- analysis$modelResults
                if (variant=='changed') {
                    fit <- changedAnalysis$modelResults
                    fake$options$shadeRows <- TRUE
                    fake$options$enhanceFunnel <- TRUE
                }
                if (kind=='forest') {
                    service$renderForest(fake,fit)
                } else if (kind=='leave1out') {
                    l1o <- get('MetaforServiceClass',asNamespace('Metafora'))$new()$mzComputeLeave1Out(fit)
                    service$renderLeave1Out(fake,output,fit,l1o)
                } else service$renderFunnel(fake,fit)
                expect_match(output$content,'data-mz-stable-plot=',fixed=TRUE)
                expect_match(output$content,'__mzD3RenderGuard',fixed=TRUE)
                expect_match(output$content,'__mzD3PendingRender',fixed=TRUE)
                expect_false(grepl('Plot error:',output$content,fixed=TRUE))
                export <- Sys.getenv('METAFORA_EXPORT_D3_STABILITY')
                if (nzchar(export)) {
                    dir.create(export,recursive=TRUE,showWarnings=FALSE)
                    writeLines(enc2utf8(output$content),file.path(export,paste(name,kind,variant,sep='_')),
                        useBytes=TRUE)
                }
            }
        }
    })
})
