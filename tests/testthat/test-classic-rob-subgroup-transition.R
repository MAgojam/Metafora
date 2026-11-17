# Cambi di subgroup dopo un Image gia' disegnato: callback reali e dati rilasciati.
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'Classic updates group layout after adding changing and removing subgroup'), {
  ns <- asNamespace('Metafora');d <- model_data()
  d$group2 <- factor(rep(c('First','Second','Third'),length.out=nrow(d)))
  d$D1 <- rep(c('Low','High','Some concerns'),length.out=nrow(d))
  robs <- name %in% c('mzDichotomous','mzMeanDifferences','mzEffectSizes')
  base <- list(forestPlotMode='posLorT',addfit=TRUE,addpred=TRUE,forestPredictionStyle='dist',shadeRows=TRUE)
  if(robs)base <- c(base,list(forestRobShow=TRUE,robDomains='D1'))
  prior <- NULL
  directory <- tempfile('group_transition_');dir.create(directory)
  on.exit({
   stopifnot(startsWith(normalizePath(directory,winslash='/'),normalizePath(tempdir(),winslash='/')))
   unlink(directory,recursive=TRUE)
  })
  step <- 0L
  for(group in list(NULL,'group','group2',NULL,'group')) {
   step <- step+1L;extra <- c(base,list(subgroup=group))
   a <- classic_rob_engine(name,d,extra,previous=prior,changes=if(step==1L)character()else'subgroup')$analysis
   expect_null(a$data)
   if(robs && !is.null(group))expect_equal(a$results$mzForestPlot$state$data$subgroup,d[[group]])
   result <- NULL;native <- get('mzForestPlotAll',ns);calls <- 0L
   a$.setResourcesPathSource(function(n,suffix)list(rootPath=directory,relPath=paste0(step,'.',suffix)))
   with_mocked_bindings(
    with_mocked_bindings(expect_true(a$.createImage('.mzForestPlot',a$results$mzForestPlot)),
      mzForestPlotAll=function(...) {result <<- native(...);result},.package='Metafora'),
    mzFitRma=function(...) {calls <<- calls+1L;stop('No statistical fit during drawing')},.package='Metafora')
   expect_identical(calls,0L)
   expect_false(is.null(result))
   expect_identical(is.null(result$totRow),is.null(group))
   fresh <- make_analysis(name,data=d,extra=extra)
   expect_equal(a$modelResults$b,fresh$modelResults$b)
   if(!is.null(group)) {
    expect_equal(length(setdiff(names(a$subgroupResults),'metareg')),nlevels(d[[group]]))
    expect_gt(result$totRow,nrow(d))
   }
   prior <- snapshot(a)
   if(robs) {
    restored <- classic_rob_engine(name,d,extra,previous=prior,run=FALSE)$analysis
    restored$.setResourcesPathSource(function(n,suffix)list(rootPath=directory,relPath=paste0('restore',step,'.',suffix)))
    file <- tempfile(fileext='.pdf');grDevices::pdf(file,width=15,height=16)
    result <- NULL
    tryCatch(with_mocked_bindings(
      expect_true(restored$.__enclos_env__$private$.mzForestPlot(restored$results$mzForestPlot)),
      mzForestPlotAll=function(...) {result <<- native(...);result},.package='Metafora'),
      finally={grDevices::dev.off();unlink(file)})
    expect_identical(is.null(result$totRow),is.null(group))
   }
  }
 })
})
