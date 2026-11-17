# Confronto col bootstrap nativo, cache indipendente dal fit e ripristino RNG.
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'hettest matches native Score bootstrap and preserves RNG'), {
  compute <- get('.mzComputeHetTest',asNamespace('Metafora'))
  set.seed(317);before <- .Random.seed
  a <- make_analysis(name,extra=list(showHetTest=TRUE))
  expect_identical(.Random.seed,before)
  expect_true(a$results$hetTestTable$visible)
  state <- a$results$hetTestResults$state$result
  expect_true(state$ok);expect_equal(state$iterations,1000)
  native <- metafor::hettest(a$modelResults,test='score',boot=1000L,seed=20261115L,progbar=FALSE)
  expect_equal(state$statistic,native$statistic[1])
  expect_equal(state$df,native$df[1]);expect_equal(state$p,native$pval[1])
  expect_equal(state$valid,sum(!is.na(native$statistic.boot[,1])))
  expect_equal(a$results$hetTestTable$getCell(col='p',rowNo=1)$value,native$pval[1])
  # Off/on, ripristino e CI non ripetono il bootstrap.
  blocked <- function(...)stop('Unexpected hettest recomputation')
  off <- with_mocked_bindings(make_analysis(name,extra=list(showHetTest=FALSE),
       previous=snapshot(a),changes='showHetTest'),.mzComputeHetTest=blocked,.package='Metafora')
  expect_false(off$results$hetTestTable$visible)
  on <- with_mocked_bindings(make_analysis(name,extra=list(showHetTest=TRUE),
       previous=snapshot(off),changes='showHetTest'),.mzComputeHetTest=blocked,.package='Metafora')
  expect_true(on$results$hetTestTable$visible)
  expect_equal(on$results$hetTestResults$state$result$p,state$p)
  ci <- with_mocked_bindings(make_analysis(name,extra=list(showHetTest=TRUE,level=90),
       previous=snapshot(on),changes='level'),.mzComputeHetTest=blocked,.package='Metafora')
  expect_equal(ci$results$hetTestResults$state$result$p,state$p)
  # Il checkbox non ristima il modello principale gia disponibile.
  noFit <- function(...)stop('Unexpected main model refit')
  b <- make_analysis(name)
  reuse <- with_mocked_bindings(make_analysis(name,extra=list(showHetTest=TRUE),
       previous=snapshot(b),changes='showHetTest'),rma=noFit,rma.uni=noFit,.package='metafor')
  expect_equal(reuse$modelResults$b,b$modelResults$b)
 })
})

test_that('ML is supported, input changes invalidate hettest, and unsupported models stay hidden', {
 compute <- get('.mzComputeHetTest',asNamespace('Metafora'))
 ml <- make_analysis('mzDichotomous',extra=list(modelest='ML',showHetTest=TRUE))
 native <- metafor::hettest(ml$modelResults,test='score',boot=1000L,seed=20261115L,progbar=FALSE)
 expect_equal(ml$results$hetTestResults$state$result$p,native$pval[1])
 for(extra in list(list(modelest='DL'),list(modelest='FE'),list(moderator='mod'))) {
  a <- with_mocked_bindings(make_analysis('mzDichotomous',extra=utils::modifyList(list(showHetTest=TRUE),extra)),
       .mzComputeHetTest=function(...)stop('Unsupported test must not run'),.package='Metafora')
  expect_false(a$results$hetTestTable$visible)
  expect_null(a$results$hetTestResults$state)
 }
 a <- make_analysis('mzEffectSizes',extra=list(showHetTest=TRUE))
 d <- model_data();d$vi[1] <- d$vi[1]*1.7
 b <- make_analysis('mzEffectSizes',data=d,extra=list(showHetTest=TRUE),previous=snapshot(a),variables='vi')
 expect_false(identical(a$results$hetTestResults$state$key,b$results$hetTestResults$state$key))
 expect_equal(b$results$hetTestResults$state$key$fit$vi[1],d$vi[1])
 expect_error(compute(metafor::rma(yi=.3,vi=.1)))
 # Il bootstrap non deve lasciare un seed se prima non esisteva.
 if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv)
 invisible(compute(a$modelResults));expect_false(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))
})

test_that('Native hettest failure does not stop the main analysis or publish NaN', {
 a <- with_mocked_bindings(make_analysis('mzDichotomous',extra=list(showHetTest=TRUE)),
       .mzComputeHetTest=function(...)stop('Native bootstrap failed'),.package='Metafora')
 expect_true(inherits(a$modelResults,'rma'))
 expect_false(a$results$hetTestTable$visible)
 expect_false(a$results$hetTestResults$state$result$ok)
 expect_match(a$results$hetTestResults$state$result$message,'Native bootstrap failed')
})

# Il cambio B invalida solo il bootstrap, senza ristimare il modello principale.
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'uses configured bootstrap samples with native results and cached restoration'), {
  a <- make_analysis(name,extra=list(showHetTest=TRUE))
  noFit <- function(...)stop('Changing bootstrap samples must not refit the main model')
  b <- with_mocked_bindings(make_analysis(name,extra=list(showHetTest=TRUE,hetTestBoot=2000),
       previous=snapshot(a),changes='hetTestBoot'),rma=noFit,rma.uni=noFit,.package='metafor')
  state <- b$results$hetTestResults$state$result
  native <- metafor::hettest(b$modelResults,test='score',boot=2000L,seed=20261115L,progbar=FALSE)
  expect_true(state$ok);expect_equal(state$iterations,2000)
  expect_equal(state$p,native$pval[1]);expect_equal(state$statistic,native$statistic[1])
  expect_equal(state$valid,sum(!is.na(native$statistic.boot[,1])))
  expect_equal(b$results$hetTestResults$state$key$iterations,2000)
  expect_equal(b$results$hetTestTable$getCell(col='bootstrap',rowNo=1)$value,
      paste(sum(!is.na(native$statistic.boot[,1])),'/',2000))
  expect_equal(b$modelResults$b,a$modelResults$b)
  cached <- with_mocked_bindings(make_analysis(name,extra=list(showHetTest=TRUE,hetTestBoot=2000),
       previous=snapshot(b)),.mzComputeHetTest=function(...)stop('Bootstrap must be cached'),.package='Metafora')
  expect_equal(cached$results$hetTestResults$state$result$p,state$p)
  off <- make_analysis(name,extra=list(hetTestBoot=5000),previous=snapshot(cached),changes=c('showHetTest','hetTestBoot'))
  expect_false(off$results$hetTestTable$visible)
  expect_equal(off$options$hetTestBoot,5000)
 })
})

test_that('Bootstrap count validation rejects non-integers and invalid ranges without changing RNG', {
 compute <- get('.mzComputeHetTest',asNamespace('Metafora'))
 fit <- make_analysis('mzDichotomous')$modelResults
 set.seed(77);before <- .Random.seed
 for(b in list(NULL,NA_real_,Inf,99,100001,1000.5,'5000',c(1000,2000)))
  expect_error(compute(fit,iterations=b),'whole number')
 expect_identical(.Random.seed,before)
 large <- make_analysis('mzDichotomous',extra=list(showHetTest=TRUE,hetTestBoot=5000))
 expect_equal(large$results$hetTestResults$state$result$iterations,5000)
 expect_equal(large$results$hetTestTable$getCell(col='bootstrap',rowNo=1)$value,
    paste(large$results$hetTestResults$state$result$valid,'/',5000))
})
