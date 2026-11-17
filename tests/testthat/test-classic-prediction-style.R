# Prove sul renderer nativo: tutti i modelli, stili, sottogruppi e cache di righe.
for(model in names(model_specs))local({name<-model
 test_that(paste(name,'Classic prediction styles retain estimates and render native prediction rows'),{
  ns<-asNamespace('Metafora');styles<-c('line2','diamond','bar','shade','dist')
  file<-tempfile(fileext='.pdf');pdf(file,width=13,height=13)
  tryCatch(for(grouped in c(FALSE,TRUE))for(reverse in c(FALSE,TRUE)) {
   base<-list(modelest='REML',forestPlotMode='posLorT',addfit=TRUE,addpred=TRUE,ordSubgr=reverse)
   if(grouped)base$subgroup<-'group'
   reference<-make_analysis(name,extra=base)$modelResults
   for(style in styles) {
    a<-make_analysis(name,extra=c(base,list(forestPredictionStyle=style)))
    captured<-NULL;result<-NULL;polys<-list();native<-metafor::forest;poly<-metafor::addpoly
    with_mocked_bindings(
     expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
     forest=function(...) {captured<<-list(...);result<<-do.call(native,captured);result},
     addpoly=function(...) {args<-list(...);polys[[length(polys)+1L]]<<-args;do.call(poly,args)},
     .package='Metafora')
    expect_equal(a$modelResults$b,reference$b)
    expect_equal(a$modelResults$se,reference$se)
    expect_equal(predict(a$modelResults)$pi.lb,predict(reference)$pi.lb)
    expect_equal(predict(a$modelResults)$pi.ub,predict(reference)$pi.ub)
    if(style=='line')expect_null(captured$predstyle)else {
     expect_identical(captured$predstyle,style)
     if(style=='dist') {
      expect_equal(captured$efac,c(1,1,1,.65))
     } else expect_null(captured$efac)
     expect_lte(captured$ylim[1],-5)
     if(grouped) {
      expect_length(polys,2L)
      for(args in polys) {
       expect_identical(args$predstyle,style)
       expect_equal(diff(args$row),-1)
       expect_true(isTRUE(args$addpred))
      }
     }
    }
   }
  },finally={dev.off();unlink(file)})
 })
 test_that(paste(name,'extended prediction styles remain inactive for FE, moderators or hidden prediction'),{
  ns<-asNamespace('Metafora');native<-metafor::forest
  cases<-list(list(modelest='FE'),list(moderator='mod'),list(addpred=FALSE),list(addfit=FALSE))
  file<-tempfile(fileext='.pdf');pdf(file,width=13,height=13)
  tryCatch(for(extra in cases) {
   opts<-modifyList(list(forestPlotMode='posLorT',addfit=TRUE,addpred=TRUE,
    forestPredictionStyle='dist'),extra)
   a<-make_analysis(name,extra=opts);args<-NULL
   with_mocked_bindings(
    expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
    forest=function(...) {args<<-list(...);do.call(native,args)},.package='Metafora')
   expect_null(args$predstyle)
  },finally={dev.off();unlink(file)})
 })
})
# Il nuovo controllo deve invalidare il file e la sola cache di geometria.
test_that('Classic prediction style changes survive native restore and invalidate geometry only',{
 for(name in names(model_specs)) {
  base<-list(forestPlotMode='posLorT',modelest='REML',addfit=TRUE,addpred=TRUE)
  a<-make_analysis(name,extra=base)
  a$results$mzForestPlotSizeCache$setState(list(xlim=c(-10,10),ylim=c(-3,20)))
  b<-with_mocked_bindings(
   make_analysis(name,extra=c(base,list(forestPredictionStyle='dist')),
    previous=snapshot(a),changes='forestPredictionStyle'),
   rma=function(...)stop('Unexpected statistical refit'),
   rma.uni=function(...)stop('Unexpected statistical refit'),.package='metafor')
  expect_identical(b$results$mzForestPlot$state$opts$forestPredictionStyle,'dist')
  expect_null(b$results$mzForestPlotSizeCache$state$xlim)
  expect_identical(b$results$mzForestPlotSizeCache$state$predictionStyle,'dist')
  expect_equal(b$modelResults$b,a$modelResults$b)
  file<-tempfile(fileext='.png')
  tryCatch({
   expect_true(b$results$mzForestPlot$saveAs(file))
   expect_gt(file.info(file)$size,1000)
  },finally=unlink(file))
  c<-make_analysis(name,extra=base,previous=snapshot(b),changes='forestPredictionStyle')
  expect_identical(c$results$mzForestPlot$state$opts$forestPredictionStyle,'line2')
  expect_null(c$results$mzForestPlotSizeCache$state$xlim)
  expect_identical(c$results$mzForestPlotSizeCache$state$predictionStyle,'line2')
 }
})
# La voce Line non e' piu' offerta: i vecchi file locali usano la variante con limiti.
test_that('legacy Line selections restore as Line with limits without changing the model',{
 for(name in names(model_specs)) {
  base<-list(forestPlotMode='posLorT',modelest='REML',addfit=TRUE,addpred=TRUE)
  a<-make_analysis(name,extra=c(base,list(forestPredictionStyle='line')))
  expect_identical(a$results$mzForestPlot$state$opts$forestPredictionStyle,'line2')
  b<-make_analysis(name,extra=base)
  expect_identical(b$options$forestPredictionStyle,'line2')
  expect_equal(a$modelResults$b,b$modelResults$b)
  expect_equal(a$modelResults$se,b$modelResults$se)
 }
})
