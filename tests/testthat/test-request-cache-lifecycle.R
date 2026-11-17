for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'failed escalc is not retried by subgroup enrichment'),{
  a<-make_analysis(name,run=FALSE)
  fit<-a$modelResults
  p<-a$.__enclos_env__$private;original<-p$.computeEscalc;calls<-0L
  unlockBinding('.computeEscalc',p)
  p$.computeEscalc<-function() {calls<<-calls+1L;stop('Original escalc failure')}
  lockBinding('.computeEscalc',p)
  on.exit({unlockBinding('.computeEscalc',p);p$.computeEscalc<-original;lockBinding('.computeEscalc',p)},add=TRUE)
  expect_error(a$escalcResults,'Original escalc failure')
  expect_null(a$escalcResults)
  expect_no_error(p$.getSubgrEnrichedData(a$dataProc))
  expect_equal(calls,1L)
  expect_identical(a$modelResults,fit)
 })
 test_that(paste(name,'new request recovers after a failed main calculation'),{
  original<-get('mzGetCachedModel',asNamespace('Metafora'));calls<-0L
  local_mocked_bindings(mzGetCachedModel=function(...) {
   calls<<-calls+1L
   if(calls==1L)stop('Original transient failure')
   original(...)
  },.package='Metafora')
  a<-make_analysis(name,run=FALSE)
  expect_error(a$modelResults,'Original transient failure')
  expect_null(a$modelResults)
  prior<-snapshot(a)
  b<-make_analysis(name,run=FALSE,previous=prior)
  expect_s3_class(b$modelResults,'rma')
  expect_true(all(is.finite(b$modelResults$beta)))
  expect_equal(calls,2L)
  expect_identical(b$.__enclos_env__$private$.requestCalculations$.modelResults$status,'ready')
 })
})
