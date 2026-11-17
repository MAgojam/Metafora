test_that('request memo distinguishes success, empty, failure and reentrant access', {
 memo <- get('mzRequestCalculation',asNamespace('Metafora'))
 owner <- new.env(parent=emptyenv());owner$.requestCalculations<-NULL;owner$.value<-NULL
 calls<-0L
 expect_equal(memo(owner,'.value',{calls<-calls+1L;42}),42)
 expect_equal(memo(owner,'.value',stop('Must stay lazy')),42)
 expect_equal(calls,1L)
 expect_identical(owner$.requestCalculations$.value$status,'ready')
 for(kind in c('empty','failed','reentrant')) {
  owner<-new.env(parent=emptyenv());owner$.requestCalculations<-NULL;owner$.value<-NULL
  calls<-0L
  compute<-function() {
   calls<<-calls+1L
   if(kind=='failed') {
    owner$.value<-list(partial=TRUE)
    stop(structure(list(message='Original failure',call=NULL),
     class=c('mz_test_failure','error','condition')))
   }
   if(kind=='reentrant')expect_null(memo(owner,'.value',stop('Recursive retry')))
   NULL
  }
  if(kind=='failed')expect_error(memo(owner,'.value',compute()),class='mz_test_failure')
  else expect_null(memo(owner,'.value',compute()))
  expect_null(memo(owner,'.value',compute()))
  expect_equal(calls,1L)
  expect_identical(owner$.requestCalculations$.value$status,if(kind=='failed')'failed' else 'empty')
  # Una nuova istanza non eredita fallimenti o risultati vuoti.
  fresh<-new.env(parent=emptyenv());fresh$.requestCalculations<-NULL;fresh$.value<-NULL
  expect_equal(memo(fresh,'.value',7),7)
 }
})

for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'failed main fit is attempted once and is not persisted'),{
  calls<-0L
  local_mocked_bindings(mzGetCachedModel=function(...) {
   calls<<-calls+1L
   stop(structure(list(message='Original fit failure',call=NULL),
    class=c('mz_test_failure','error','condition')))
  },.package='Metafora')
  a<-make_analysis(name,run=FALSE)
  expect_error(a$modelResults,class='mz_test_failure')
  expect_null(a$modelResults);expect_null(a$modelResults)
  expect_equal(calls,1L)
  expect_null(a$results$textRICH$state)
  expect_identical(a$.__enclos_env__$private$.requestCalculations$.modelResults$status,'failed')
  b<-make_analysis(name,run=FALSE)
  expect_error(b$modelResults,class='mz_test_failure')
  expect_equal(calls,2L)
 })
 test_that(paste(name,'null main fit is remembered within its request'),{
  calls<-0L
  local_mocked_bindings(mzGetCachedModel=function(...) {calls<<-calls+1L;NULL},.package='Metafora')
  a<-make_analysis(name,run=FALSE)
  expect_null(a$modelResults);expect_null(a$modelResults)
  expect_equal(calls,1L)
  expect_identical(a$.__enclos_env__$private$.requestCalculations$.modelResults$status,'empty')
 })
 for(binding in c('subgroupResults','leave1outResults'))local({
  target<-binding
  test_that(paste(name,target,'failure is attempted once without replacing valid global cache'),{
   a<-make_analysis(name,run=FALSE,extra=list(subgroup='group'))
   fit<-a$modelResults
   calls<-0L
   replacement<-function(...) {calls<<-calls+1L;stop('Original sensitivity failure')}
   mock<-setNames(list(replacement),if(target=='subgroupResults')'mzGetCachedSubgroups' else '.mzCachedLeaveOneOut')
   do.call(local_mocked_bindings,c(mock,list(.package='Metafora',.env=environment())))
   expect_error(a[[target]],'Original sensitivity failure')
   expect_null(a[[target]]);expect_null(a[[target]])
   expect_equal(calls,1L)
   expect_identical(a$modelResults,fit)
   expect_identical(a$results$textRICH$state,fit)
  })
 })
})
