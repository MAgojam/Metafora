for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'LOO Classic shares exact omission and pooled values and survives data release'),{
  f<-function(n)get(n,asNamespace('Metafora'))
  for(grouped in c(FALSE,TRUE)) {
   opts<-list(leave1outPlotMode='posLorT',leave1outTableShow=TRUE,leave1outPrediction=TRUE,
       showBackTransform=TRUE,leave1outGroupOrder=TRUE)
   if(grouped)opts$subgroup<-'group'
   a<-make_analysis(name,extra=opts)
   expect_true(a$results$leave1outClassicPlot$visible)
   expect_false(a$results$outD3Leave1OutPlot$visible)
   expect_true(a$results$outHtmlLeave1Out$visible)
   state<-a$results$leave1outClassicCache$state
   table<-f('mzBuildLeaveOneOutTable')(a)$data
   m<-f('mzCumulativeMeasure')(a);trans<-function(x)if(is.function(m$atransf))m$atransf(x)else x
   expect_equal(trans(state$rows$es),table$Estimate)
   expect_equal(state$rows$p,table$p)
   expect_equal(state$rows$tau2,table$Tau2)
   expect_equal(state$rows$i2,table$I2)
   expect_true(match('I2',names(table))<match('Tau',names(table)))
   expect_true(match('Tau',names(table))<match('Tau2',names(table)))
   expect_equal(pmin(trans(state$rows$pi_lo),trans(state$rows$pi_hi)),table$PredictionLower)
   a$.__enclos_env__$private$.data<-NULL
   path<-tempfile(fileext='.pdf');grDevices::pdf(path,width=13,height=10)
   previous<-getOption('na.action')
   tryCatch(expect_true(f('mzRenderLeaveOneOutClassic')(a,a$results$leave1outClassicPlot)),
      finally={grDevices::dev.off();unlink(path)})
   expect_identical(getOption('na.action'),previous)
   b<-testthat::with_mocked_bindings(make_analysis(name,extra=utils::modifyList(opts,
      list(leave1outPlotMode='posRorB')),previous=snapshot(a),changes='leave1outPlotMode'),
      .mzLeaveOneOutResults=function(...)stop('Unexpected base refit'),
      .mzLeaveOneOutPrediction=function(...)stop('Unexpected PI refit'),.package='Metafora')
   expect_false(b$results$leave1outClassicPlot$visible)
   expect_true(b$results$outD3Leave1OutPlot$visible)
   expect_true(b$results$outHtmlLeave1Out$visible)
   expect_identical(b$results$leave1outResultsCache$state,a$results$leave1outResultsCache$state)
  }
 })
})
