for(model in names(model_specs))local({
 name<-model
 for(grouped in c(FALSE,TRUE))local({
  sg<-grouped
  test_that(paste(name,sg,'cumulative PI matches independent prefix refits and is cached separately'),{
   f<-function(n)get(n,asNamespace('Metafora'))
   opts<-list(cumulativeTableShow=TRUE,cumulativePrediction=TRUE,cumulativeOrder='se',
     cumulativeDirection='desc',level=90,showBackTransform=TRUE)
   if(sg)opts$subgroup<-'group'
   a<-make_analysis(name,extra=opts);fit<-a$modelResults
   r<-f('.mzCumulativeDisplayResult')(a)
   base<-a$results$cumulativeResults$state
   expect_null(base$result$pi.lb)
   groups<-if(sg)a$dataProc$subgroup else rep('',fit$k.all)
   labels<-if(sg)sort(unique(as.character(groups)))else ''
   offset<-0L
   for(label in labels) {
    idx<-which(as.character(groups)==label);idx<-idx[order(sqrt(fit$vi.f[idx]),decreasing=TRUE)]
    for(step in seq_along(idx)) {
     i<-offset+step
     if(step<2L) {expect_true(is.na(r$pi.lb[i]));expect_true(is.na(r$pi.ub[i]));next}
     take<-idx[seq_len(step)]
     native<-metafor::rma(yi=fit$yi.f[take],vi=fit$vi.f[take],method=fit$method,test=fit$test,level=fit$level)
     p<-predict(native)
     expect_equal(r$pi.lb[i],as.numeric(p$pi.lb),tolerance=1e-9)
     expect_equal(r$pi.ub[i],as.numeric(p$pi.ub),tolerance=1e-9)
     expect_equal(r$estimate[i],as.numeric(native$beta),tolerance=1e-9,ignore_attr=TRUE)
    }
    offset<-offset+length(idx)
   }
   table<-f('mzBuildCumulativeTable')(a,r)
   expect_true(all(c('PredictionLower','PredictionUpper')%in%names(table$data)))
   m<-f('mzCumulativeMeasure')(a);trans<-function(x)if(is.function(m$atransf))m$atransf(x)else x
   expect_equal(table$data$PredictionLower,pmin(trans(r$pi.lb),trans(r$pi.ub)))
   saved<-a$results$cumulativePredictionCache$state
   testthat::local_mocked_bindings(.mzCumulativePrediction=function(...)stop('Unexpected PI refit'),
     mzComputeCumulative=function(...)stop('Unexpected base refit'),
     .mzComputeGroupedCumulative=function(...)stop('Unexpected group refit'),.package='Metafora')
   b<-make_analysis(name,extra=utils::modifyList(opts,list(cumulativeShade=TRUE,cumulativeGroupOrder=TRUE)),
      previous=snapshot(a),changes=c('cumulativeShade','cumulativeGroupOrder'))
   expect_identical(b$results$cumulativeResults$state,base)
   expect_identical(b$results$cumulativePredictionCache$state,saved)
   off<-make_analysis(name,extra=utils::modifyList(opts,list(cumulativePrediction=FALSE)),
      previous=snapshot(b),changes='cumulativePrediction')
   expect_null(f('.mzCumulativeDisplayResult')(off)$pi.lb)
   on<-make_analysis(name,extra=opts,previous=snapshot(off),changes='cumulativePrediction')
   expect_identical(on$results$cumulativePredictionCache$state,saved)
  })
 })
})

test_that('FE cumulative never fabricates prediction intervals',{
 a<-make_analysis('mzDichotomous',extra=list(modelest='FE',cumulativeTableShow=TRUE,cumulativePrediction=TRUE))
 expect_null(a$results$cumulativePredictionCache$state)
 expect_null(get('.mzCumulativeDisplayResult',asNamespace('Metafora'))(a)$pi.lb)
})

test_that('prediction prefixes remain aligned when native cumul omits an NA step',{
 f<-function(n)get(n,asNamespace('Metafora'))
 yi<-c(.1,NA,.4,.2,.5,.7);vi<-rep(.1,6)
 fit<-suppressWarnings(metafor::rma(yi=yi,vi=vi))
 base<-f('mzComputeCumulative')(fit,seq_along(yi))
 result<-f('.mzCumulativePrediction')(fit,base,seq_along(yi))
 expect_length(result$pi.lb,length(base$estimate))
 for(i in seq_along(base$estimate)) {
  idx<-seq_len(base$order[i]);native<-suppressWarnings(metafor::rma(yi=yi[idx],vi=vi[idx]))
  if(native$k<2L)expect_true(is.na(result$pi.lb[i]))else {
   expected<-predict(native)
   expect_equal(result$pi.lb[i],as.numeric(expected$pi.lb),tolerance=1e-9)
   expect_equal(result$pi.ub[i],as.numeric(expected$pi.ub),tolerance=1e-9)
  }
 }
})

test_that('cumulative PI retains the selected inference and PFT stays on its transformed scale',{
 f<-function(n)get(n,asNamespace('Metafora'))
 for(test in c('t','knha','adhoc')) {
  a<-make_analysis('mzEffectSizes',extra=list(cumulativeTableShow=TRUE,cumulativePrediction=TRUE,
    testType=test,level=99))
  fit<-a$modelResults;r<-f('.mzCumulativeDisplayResult')(a)
  for(step in 2:fit$k) {
   native<-metafor::rma(yi=fit$yi.f[seq_len(step)],vi=fit$vi.f[seq_len(step)],
     method=fit$method,test=test,level=99)
   p<-predict(native)
   expect_equal(r$pi.lb[step],as.numeric(p$pi.lb),tolerance=1e-9)
   expect_equal(r$pi.ub[step],as.numeric(p$pi.ub),tolerance=1e-9)
  }
 }
 a<-make_analysis('mzProportions',extra=list(modelmea='PFT',cumulativeTableShow=TRUE,
   cumulativePrediction=TRUE,showBackTransform=TRUE))
 r<-f('.mzCumulativeDisplayResult')(a)
 expect_identical(f('mzCumulativeMeasure')(a)$transf_type,'identity')
 expect_equal(f('mzBuildCumulativeTable')(a,r)$data$PredictionLower,r$pi.lb)
})
