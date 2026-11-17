pred_fun<-function(name)get(name,asNamespace('Metafora'))
for(model in names(model_specs))local({
 name<-model
 for(grouped in c(FALSE,TRUE))local({
  sg<-grouped
  test_that(paste(name,sg,'LOO prediction limits match independent native refits'),{
   opts<-list(leave1outPrediction=TRUE,leave1outTableShow=TRUE,level=90)
   if(sg)opts$subgroup<-'group'
   a<-make_analysis(name,extra=opts);fit<-a$modelResults;r<-a$leave1outResults
   expect_true(all(c('pi.lb','pi.ub')%in%names(r)))
   labels<-if(sg)sort(unique(as.character(a$dataProc$subgroup)))else ''
   offset<-0L
   for(label in labels) {
    idx<-if(sg)which(as.character(a$dataProc$subgroup)==label)else seq_along(fit$yi.f)
    for(i in seq_along(idx)) {
     keep<-idx[-i]
     native<-metafor::rma(yi=fit$yi.f[keep],vi=fit$vi.f[keep],measure=fit$measure,
       method=fit$method,test=fit$test,level=fit$level)
     p<-predict(native)
     expect_equal(r$pi.lb[offset+i],as.numeric(p$pi.lb),tolerance=1e-10)
     expect_equal(r$pi.ub[offset+i],as.numeric(p$pi.ub),tolerance=1e-10)
     expect_equal(r$estimate[offset+i],as.numeric(native$beta),tolerance=1e-10)
    }
    offset<-offset+length(idx)
   }
   table<-pred_fun('mzBuildLeaveOneOutTable')(a)
   expect_true(all(c('PredictionLower','PredictionUpper')%in%names(table$data)))
   expect_match(table$html,'90% PI',fixed=TRUE)
   prior<-snapshot(a)
   local_mocked_bindings(.mzLeaveOneOutResults=function(...)stop('Unexpected base refit'),
    .mzLeaveOneOutPrediction=function(...)stop('Unexpected PI refit'),.package='Metafora')
   b<-make_analysis(name,extra=c(opts,list(leave1outShade=FALSE)),previous=prior,changes='leave1outShade')
   expect_identical(b$leave1outResults,r)
   off<-utils::modifyList(opts,list(leave1outPrediction=FALSE))
   c<-make_analysis(name,extra=off,previous=snapshot(b),changes='leave1outPrediction')
   expect_false('pi.lb'%in%names(c$leave1outResults))
   expect_false('PredictionLower'%in%names(pred_fun('mzBuildLeaveOneOutTable')(c)$data))
   back<-make_analysis(name,extra=opts,previous=snapshot(c),changes='leave1outPrediction')
   expect_identical(back$leave1outResults,r)
  })
 })
})

test_that('PI follows native inference variants, SJ2, small groups and excludes FE',{
 for(method in c('FE','REML','SJ2'))for(test in if(method=='FE')'z'else c('z','t','knha','adhoc')) {
  opts<-list(leave1outPrediction=TRUE,leave1outTableShow=TRUE,modelest=method,testType=test,level=99)
  a<-make_analysis('mzEffectSizes',extra=opts);r<-a$leave1outResults;f<-a$modelResults
  if(method=='FE') {expect_false('pi.lb'%in%names(r));next}
  keep<-seq_len(f$k)[-1]
  args<-list(yi=f$yi.f[keep],vi=f$vi.f[keep],method=if(method=='SJ2')'SJ' else method,test=test,level=99)
  if(method=='SJ2')args$control<-list(tau2.init=metafor::rma(yi=args$yi,vi=args$vi,method='HE')$tau2)
  native<-do.call(metafor::rma,args);p<-predict(native)
  expect_equal(r$pi.lb[1],as.numeric(p$pi.lb),tolerance=1e-10)
  expect_equal(r$pi.ub[1],as.numeric(p$pi.ub),tolerance=1e-10)
 }
 d<-model_data();d$group<-c('pair','pair',rep('other',10),'single')
 a<-make_analysis('mzDichotomous',data=d,extra=list(subgroup='group',leave1outPrediction=TRUE,leave1outTableShow=TRUE))
 r<-a$leave1outResults
 expect_true(all(is.na(r$pi.lb[r$subgroup%in%c('pair','single')])))
 pair<-Filter(function(g)g$label=='pair',attr(r,'mz_loo_groups'))[[1]]
 expect_true(is.finite(pair$pooled$pi_lo))
 expect_true(all(is.finite(r$pi.lb[r$subgroup=='other'])))
 # Il livello CI invalida entrambe le cache numeriche.
 b<-make_analysis('mzDichotomous',data=d,extra=list(subgroup='group',leave1outPrediction=TRUE,
  leave1outTableShow=TRUE,level=99),previous=snapshot(a),changes='level')
 expect_false(identical(a$results$leave1outPredictionCache$state$signature,b$results$leave1outPredictionCache$state$signature))
 expect_true(all(b$leave1outResults$pi.lb[b$leave1outResults$subgroup=='other']<r$pi.lb[r$subgroup=='other']))
})

test_that('displayed PI limits transform without changing analysis-scale inference',{
 for(spec in list(c('mzDichotomous','RR'),c('mzCorrelations','ZCOR'),c('mzProportions','PLO'),c('mzAlphas','AHW'))) {
  name<-spec[1];opts<-list(modelmea=spec[2],leave1outPrediction=TRUE,leave1outTableShow=TRUE)
  a<-make_analysis(name,extra=opts)
  b<-make_analysis(name,extra=c(opts,list(showBackTransform=TRUE)),previous=snapshot(a),changes='showBackTransform')
  expect_equal(b$leave1outResults,a$leave1outResults)
  m<-pred_fun('mzCumulativeMeasure')(b);t<-pred_fun('mzBuildLeaveOneOutTable')(b)$data
  expect_equal(t$PredictionLower[1],min(m$atransf(a$leave1outResults$pi.lb[1]),m$atransf(a$leave1outResults$pi.ub[1])))
  expect_equal(t$PredictionUpper[1],max(m$atransf(a$leave1outResults$pi.lb[1]),m$atransf(a$leave1outResults$pi.ub[1])))
 }
})
