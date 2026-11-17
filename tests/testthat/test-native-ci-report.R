# Cambio CI con stato reale su disco e confronto nativo delle opzioni.
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'updates numeric CI and report through native load'), {
        data <- model_data();spec <- model_specs[[name]];ns <- asNamespace('Metafora')
        state <- tempfile(fileext='.pb');on.exit(unlink(state),add=TRUE)
        previous <- NULL;bounds <- list()
        for (coverage in c(90,99,95)) {
            args <- c(list(slab='slab',modelmea=spec$measure,level=coverage,showTextRepo=TRUE),as.list(spec$vars))
            options <- options_with_protobuf(do.call(get(paste0(name,'Options'),ns)$new,args))
            a <- get(paste0(name,'Class'),ns)$new(options=options,data=NULL,datasetId='native-ci',analysisId=1L)
            a$.setReadDatasetHeaderSource(function(vars)data[0,intersect(unlist(vars),names(data)),drop=FALSE])
            a$.setReadDatasetSource(function(vars)data[,intersect(unlist(vars),names(data)),drop=FALSE])
            a$.setStatePathSource(function()state)
            if (!is.null(previous)) expect_identical(options$compProtoBuf(previous$options),'level')
            a$init();a$.load();a$postInit()
            expect_null(a$results$textRICH$state)
            a$run()
            expected <- do.call(metafor::rma,native_args(name,data,extra=list(level=coverage)))
            expect_equal(a$modelResults$level,1-coverage/100)
            table <- a$results$textRICH$asDF
            expect_equal(table$CILow[1],as.numeric(expected$ci.lb),tolerance=1e-12)
            expect_equal(table$CIHigh[1],as.numeric(expected$ci.ub),tolerance=1e-12)
            report <- a$results$OutputHtml$content
            expect_match(report,paste0(coverage,'%'),fixed=TRUE)
            digits <- get('.mz_get_digits',ns)(expected)
            for (limit in c(expected$ci.lb,expected$ci.ub))
                expect_match(report,get('.mz_fmtx',ns)(limit,digits$ci),fixed=TRUE)
            bounds[[as.character(coverage)]] <- c(table$CILow[1],table$CIHigh[1])
            previous <- a$asProtoBuf(final=TRUE);a$.save()
        }
        expect_lt(bounds[['99']][1],bounds[['90']][1])
        expect_gt(bounds[['99']][2],bounds[['90']][2])
    })
})
