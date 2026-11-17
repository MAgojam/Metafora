# La migrazione conserva i risultati statistici e disattiva le catture Html obsolete.
for (model in names(model_specs)) local({
    name <- model
    test_that(paste(name,'native Forest updates presentation without refitting'), {
        base <- list(forestPlotMode='posRorB',subgroup='group')
        a <- make_analysis(name,extra=base)
        expect_s3_class(a$results$outD3ForestPlot,'Svg')
        expect_match(a$results$outD3ForestPlot$content,'<div class="mz-native-d3"',fixed=TRUE)
        expect_length(a$results$outD3ForestPlot$scripts,0L)
        expect_identical(a$results$outD3ForestPlot$asProtoBuf(status=3L)$svg$content,
                         a$results$outD3ForestPlot$content)
        b <- with_mocked_bindings(
            make_analysis(name,extra=utils::modifyList(base,list(shadeRows=TRUE,ordSubgr=TRUE)),
                          previous=snapshot(a),changes=c('shadeRows','ordSubgr')),
            rma=function(...)stop('Unexpected statistical refit'),
            rma.uni=function(...)stop('Unexpected statistical refit'),.package='metafor')
        expect_equal(b$modelResults$beta,a$modelResults$beta)
        expect_false(identical(b$results$outD3ForestPlot$content,a$results$outD3ForestPlot$content))
        expect_false(b$results$outD3ForestPlotExport$visible)
    })
    test_that(paste(name,'native Forest ignores a valid legacy HTML snapshot'), {
        bytes <- png::writePNG(array(1,dim=c(64,96,4)),target=raw())
        encoded <- jsonlite::toJSON(list(token='legacy',width=96,height=64,
            png=jsonlite::base64_enc(bytes)),auto_unbox=TRUE)
        a <- make_analysis(name,extra=list(forestPlotMode='posRorB',d3ExportForest=encoded))
        before <- a$results$outD3ForestPlot$content
        a$results$d3ExportMeta$setState(list(forest='legacy'))
        a$results$outD3ForestPlotExport$setVisible(TRUE)
        get('.mzApplyD3Exports',asNamespace('Metafora'))(a)
        expect_false(a$results$outD3ForestPlotExport$visible)
        expect_identical(a$results$outD3ForestPlot$content,before)
    })
    test_that(paste(name,'Forest switch keeps Classic Off and Interactive distinct'), {
        for(mode in c('posLorT','posOff','posRorB')) {
            a <- make_analysis(name,extra=list(forestPlotMode=mode))
            expect_identical(a$results$outD3ForestPlot$visible,mode=='posRorB')
            expect_identical(a$results$mzForestPlot$visible,mode=='posLorT')
        }
    })
})
