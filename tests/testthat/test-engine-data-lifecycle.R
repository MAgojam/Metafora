make_engine_analysis <- function(name,data=model_data(),extra=list(),previous=NULL,changes=character(),variables=character()) {
    spec <- model_specs[[name]];ns <- asNamespace('Metafora')
    args <- utils::modifyList(c(list(slab='slab',modelmea=spec$measure),as.list(spec$vars)),extra)
    options <- do.call(get(paste0(name,'Options'),ns)$new,args)
    a <- get(paste0(name,'Class'),ns)$new(options=options,data=NULL)
    # Il motore fornisce prima intestazioni senza righe, poi i dati effettivi.
    a$.setReadDatasetHeaderSource(function(vars)data[0,intersect(unlist(vars),names(data)),drop=FALSE])
    a$.setReadDatasetSource(function(vars)data[,intersect(unlist(vars),names(data)),drop=FALSE])
    a$init()
    if (!is.null(previous)) a$results$fromProtoBuf(previous,changes,variables)
    a$postInit();a$run()
    a
}
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'uses full engine data after header-only initialization'), {
        a <- make_engine_analysis(name,extra=list(distabPBA=TRUE))
        expect_equal(a$modelResults$k,13L)
        expected <- make_analysis(name,extra=list(distabPBA=TRUE))
        expect_equal(a$modelResults$beta,expected$modelResults$beta)
        b <- make_engine_analysis(name,extra=list(distabPBA=TRUE,subgroup='group'),
            previous=snapshot(a),changes='subgroup')
        expect_equal(b$modelResults$k,13L)
        expect_equal(b$results$textRICH$asDF,a$results$textRICH$asDF)
        back <- make_engine_analysis(name,extra=list(distabPBA=TRUE),
            previous=snapshot(b),changes='subgroup')
        expect_equal(back$modelResults$k,13L)
        data <- model_data();data$group[1] <- NA
        changed <- make_engine_analysis(name,data=data,extra=list(distabPBA=TRUE,subgroup='group'),
            previous=snapshot(b),variables='group')
        expect_equal(changed$modelResults$k,12L)
    })
})

test_that('BCG loads correctly through the native header and full-data callbacks', {
    data <- as.data.frame(metadat::dat.bcg)
    extra <- list(slab='author',ai='tpos',bi='tneg',ci='cpos',di='cneg',modelmea='OR',distabPBA=TRUE)
    a <- make_engine_analysis('mzDichotomous',data=data,extra=extra)
    expected <- make_analysis('mzDichotomous',data=data,extra=extra)
    expect_equal(a$modelResults$k,13L)
    expect_equal(a$modelResults$beta,expected$modelResults$beta)
    b <- make_engine_analysis('mzDichotomous',data=data,
        extra=c(extra,list(subgroup='alloc')),
        previous=snapshot(a),changes='subgroup')
    expect_equal(b$modelResults$k,13L)
    expect_equal(b$results$tabPubBias$asDF,a$results$tabPubBias$asDF)
})
