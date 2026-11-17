for(model in c('mzDichotomous','mzMeanDifferences','mzEffectSizes'))local({name <- model
 test_that(paste(name,'Classic ROB survives native run release render restore and saveAs'), {
  data <- model_data()
  for(i in 1:6)data[[paste0('D',i)]] <- rep(c('Low','High','Some concerns',NA),length.out=nrow(data))
  selected <- paste0('D',1:6)
  for(grouped in c(FALSE,TRUE))for(style in c('line2','diamond','bar','shade','dist')) {
   extra <- list(forestPlotMode='posLorT',forestRobShow=TRUE,robDomains=selected,
                 forestPredictionStyle=style,modelest='REML',addfit=TRUE,addpred=TRUE)
   if(grouped)extra$subgroup <- 'group'
   run <- classic_rob_engine(name,data,extra)
   a <- run$analysis
   expect_null(a$data)
   expect_false(a$results$mzForestPlot$requiresData)
   expect_identical(a$results$mzForestPlot$state$rob$names,selected)
   expect_length(a$results$mzForestPlot$state$rob$values,6L)
   prior <- snapshot(a)
   directory <- tempfile('classic_rob_engine_');dir.create(directory)
   tryCatch({
    a$.setResourcesPathSource(function(name,suffix)list(rootPath=directory,relPath=paste0('run.',suffix)))
    reads <- run$reads()
    fitCalls <- 0L
    with_mocked_bindings(
     expect_true(a$.createImage('.mzForestPlot',a$results$mzForestPlot)),
     .mzCollectRobDomains=function(...)stop('Renderer must not collect raw ROB data'),
     mzFitRma=function(...) {fitCalls <<- fitCalls+1L;stop('Renderer must not fit statistical models')},.package='Metafora')
    expect_equal(run$reads(),reads)
    expect_null(a$data)
    expect_gt(file.info(file.path(directory,a$results$mzForestPlot$filePath))$size,1000)
    restored <- classic_rob_engine(name,data,extra,previous=prior,run=FALSE)
    b <- restored$analysis
    expect_null(b$data)
    b$.setReadDatasetSource(function(...)stop('Restored Image must not read the dataset'))
    b$.setResourcesPathSource(function(name,suffix)list(rootPath=directory,relPath=paste0('restored.',suffix)))
    with_mocked_bindings({
     expect_true(b$.createImage('.mzForestPlot',b$results$mzForestPlot))
     expect_true(b$results$mzForestPlot$saveAs(file.path(directory,'export.png')))
    },.mzCollectRobDomains=function(...)stop('Renderer must not collect raw ROB data'),
     mzFitRma=function(...) {fitCalls <<- fitCalls+1L;stop('Renderer must not fit statistical models')},.package='Metafora')
    expect_equal(fitCalls,0L)
    expect_equal(restored$reads(),0L)
    expect_gt(file.info(file.path(directory,'export.png'))$size,1000)
    expect_s4_class(snapshot(b),'Message')
   },finally={
    stopifnot(startsWith(normalizePath(directory,winslash='/'),normalizePath(tempdir(),winslash='/')))
    unlink(directory,recursive=TRUE)
   })
  }
 })
 test_that(paste(name,'Classic ROB toggle uses engine callbacks without statistical refits'), {
  data <- model_data();data$D1 <- rep(c('Low','High',NA),length.out=nrow(data))
  base <- list(forestPlotMode='posLorT',subgroup='group',forestPredictionStyle='shade')
  a <- classic_rob_engine(name,data,base)$analysis
  b <- with_mocked_bindings(classic_rob_engine(name,data,
   c(base,list(forestRobShow=TRUE,robDomains='D1')),previous=snapshot(a),
   changes=c('forestRobShow','robDomains'))$analysis,
   rma=function(...)stop('Unexpected ROB refit'),rma.uni=function(...)stop('Unexpected ROB refit'),.package='metafor')
  expect_null(b$data)
  expect_equal(b$modelResults$b,a$modelResults$b)
  expect_identical(b$results$mzForestPlot$state$rob$names,'D1')
  off <- with_mocked_bindings(classic_rob_engine(name,data,base,previous=snapshot(b),
   changes=c('forestRobShow','robDomains'))$analysis,
   rma=function(...)stop('Unexpected ROB refit'),rma.uni=function(...)stop('Unexpected ROB refit'),.package='metafor')
  expect_equal(off$modelResults$b,a$modelResults$b)
  expect_equal(off$results$mzForestPlot$size,a$results$mzForestPlot$size)
  expect_null(off$results$mzForestPlot$state$rob)
  expect_identical(b$results$mzForestPlot$state$rob$values$D1,
    get('MetaforServiceClass',asNamespace('Metafora'))$new()$mzClassifyRobDomains(
      data[rownames(b$dataProc),'D1',drop=FALSE])$values$D1)
 })
})
# Il riuso dei fit non deve cambiare i riepiloghi che prima il renderer ristimava.
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'Classic subgroup cache matches the previous yi vi fit without render refits'), {
  ns <- asNamespace('Metafora')
  for(test in c('z','knha')) {
   a <- make_analysis(name,extra=list(forestPlotMode='posLorT',subgroup='group',testType=test,level=90))
   groups <- levels(droplevels(a$dataProc$subgroup))
   for(group in groups) {
    index <- which(a$dataProc$subgroup==group)
    oldFit <- get('mzFitRma',ns)(a$modelResults$yi[index],a$modelResults$vi[index],
                method='REML',test=test,level=90)
    cached <- a$subgroupResults[[group]]
    expect_equal(cached$b,oldFit$b,ignore_attr=TRUE)
    expect_equal(cached$se,oldFit$se)
    expect_equal(cached$ci.lb,oldFit$ci.lb)
    expect_equal(cached$ci.ub,oldFit$ci.ub)
    expect_equal(predict(cached)$pi.lb,predict(oldFit)$pi.lb)
    expect_equal(predict(cached)$pi.ub,predict(oldFit)$pi.ub)
   }
   file <- tempfile(fileext='.pdf');grDevices::pdf(file,width=15,height=15)
   calls <- 0L
   tryCatch(with_mocked_bindings(
    expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
    mzFitRma=function(...) {calls <<- calls+1L;stop('No fit allowed in the renderer')},.package='Metafora'),
    finally={grDevices::dev.off();unlink(file)})
   expect_equal(calls,0L)
  }
 })
})
