# Viste LOO: stessa inferenza, cache indipendente dagli oggetti visivi.
for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'LOO views are independent and presentation restores without refitting'),{
  a<-make_analysis(name,extra=list(subgroup='group',leave1outPlotShow=TRUE,leave1outTableShow=TRUE))
  expect_true(a$results$outD3Leave1OutPlot$visible);expect_true(a$results$outHtmlLeave1Out$visible)
  original<-a$leave1outResults;pb<-snapshot(a)
  testthat::local_mocked_bindings(.mzLeaveOneOutResults=function(...)stop('Unexpected LOO refit'),.package='Metafora')
  for(plot in c(FALSE,TRUE))for(table in c(FALSE,TRUE)) {
   opts<-list(subgroup='group',leave1outPlotShow=plot,leave1outTableShow=table,
              leave1outShade=FALSE,leave1outOrder='estimate',leave1outGroupOrder=TRUE)
   b<-make_analysis(name,extra=opts,previous=pb,
      changes=c('leave1outPlotShow','leave1outTableShow','leave1outShade','leave1outOrder','leave1outGroupOrder'))
   expect_identical(b$results$outD3Leave1OutPlot$visible,plot)
   expect_identical(b$results$outHtmlLeave1Out$visible,table)
   expect_equal(b$leave1outResults,original)
   if(table) {
    expect_false(sens_payload(b$results$outHtmlLeave1Out)$shade)
    expect_identical(unique(vapply(sens_payload(b$results$outHtmlLeave1Out)$rows,`[[`,character(1),'group')),c('Z','A'))
   }
   if(plot) {
    payload<-sens_payload(b$results$outD3Leave1OutPlot)
    expect_false(payload$shade);expect_length(payload$studies,13L)
   }
  }
 })
 test_that(paste(name,'legacy LOO snapshots preserve their selected view'),{
  folder<-testthat::test_path('fixtures','legacy-loo')
  for(mode in c('posLorT','posRorB')) {
   previous<-RProtoBuf::read(jamovi.coms.ResultsElement,file.path(folder,paste0(name,'_',mode,'.pb')))
   a<-make_analysis(name,extra=list(subgroup='group',leave1outMode=mode),previous=previous)
   expect_identical(a$results$outD3Leave1OutPlot$visible,mode=='posRorB')
   expect_identical(a$results$outHtmlLeave1Out$visible,mode=='posLorT')
   out<-if(mode=='posRorB')a$results$outD3Leave1OutPlot else a$results$outHtmlLeave1Out
   expect_true(nzchar(out$content));expect_length(a$leave1outResults$estimate,13L)
  }
 })
})
test_that('LOO cache invalidates statistical settings and row identity',{
 a<-make_analysis('mzDichotomous',extra=list(leave1outPlotShow=TRUE))
 b<-make_analysis('mzDichotomous',extra=list(leave1outPlotShow=TRUE,level=99),
    previous=snapshot(a),changes='level')
 expect_false(identical(a$results$leave1outResultsCache$state$signature,b$results$leave1outResultsCache$state$signature))
 expect_equal(a$leave1outResults$estimate,b$leave1outResults$estimate)
 expect_true(all(b$leave1outResults$ci.lb<a$leave1outResults$ci.lb))
 d<-model_data();d$ai[1]<-d$ai[1]+20
 c<-make_analysis('mzDichotomous',data=d,extra=list(leave1outPlotShow=TRUE),previous=snapshot(a),variables='ai')
 expect_false(identical(a$results$leave1outResultsCache$state$signature,c$results$leave1outResultsCache$state$signature))
 expect_false(isTRUE(all.equal(a$leave1outResults$estimate,c$leave1outResults$estimate)))
})
