# Richiesta Image separata da .run(): dataProc non è già popolato nell'istanza.
render_restored_scatter <- function(name,moderators,back_transform=FALSE,data=model_data()) {
    spec <- model_specs[[name]];ns <- asNamespace('Metafora')
    extra <- list(moderator=moderators,scatterPlotShow=TRUE,showBackTransform=back_transform)
    a <- make_analysis(name,data=data,extra=extra)
    args <- utils::modifyList(c(list(slab='slab',modelmea=spec$measure),as.list(spec$vars)),extra)
    options <- do.call(get(paste0(name,'Options'),ns)$new,args)
    b <- get(paste0(name,'Class'),ns)$new(options=options,data=NULL)
    reads <- 0L
    b$.setReadDatasetHeaderSource(function(vars)data[0,intersect(unlist(vars),names(data)),drop=FALSE])
    b$.setReadDatasetSource(function(vars) {
        reads <<- reads+1L
        data[,intersect(unlist(vars),names(data)),drop=FALSE]
    })
    b$init();b$results$fromProtoBuf(snapshot(a),character(),character());b$postInit()
    directory <- tempfile('metafora_scatter_');dir.create(directory)
    on.exit(unlink(directory,recursive=TRUE),add=TRUE)
    b$.setResourcesPathSource(function(name,suffix)list(rootPath=directory,relPath=paste0(name,'.',suffix)))
    image <- b$results$mzScatterPlot
    expect_true(image$requiresData)
    expect_true(b$.createImage('.mzScatterPlot',image))
    expect_equal(reads,1L)
    expect_false(is.null(image$filePath))
    if (!is.null(image$filePath)) expect_gt(file.info(file.path(directory,image$filePath))$size,1000)
}

for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'completes Scatter images for continuous and categorical moderators'), {
        messages <- character();original <- get('mzDiagnosticMessage',asNamespace('Metafora'))
        local_mocked_bindings(mzDiagnosticMessage=function(message) {
            messages <<- c(messages,message);original(message)
        },.package='Metafora')
        render_restored_scatter(name,list('mod'))
        expect_length(messages,0L)
        render_restored_scatter(name,list('group'))
        expect_length(messages,1L)
        expect_match(messages[1],'continuous moderator')
        messages <- character()
        render_restored_scatter(name,list('mod','group'),back_transform=TRUE)
        expect_length(messages,0L)
        coded <- model_data()
        coded$group_n <- factor(as.integer(coded$group))
        attr(coded$group_n,'values') <- c(1,2)
        messages <- character()
        render_restored_scatter(name,list('group_n'),data=coded)
        expect_length(messages,1L)
        expect_match(messages[1],'continuous moderator')
        messages <- character()
        render_restored_scatter(name,list('mod','group_n'),data=coded)
        expect_length(messages,0L)
    })
})

test_that('a Scatter renderer error produces an explanatory image instead of a pending placeholder', {
    local_mocked_bindings(regplot=function(...)stop('Controlled renderer failure'),.package='metafor')
    render_restored_scatter('mzDichotomous',list('mod'))
})
