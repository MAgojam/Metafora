# Le probabilita' e la densita' seguono la stessa distribuzione nativa di metafor.
pred_defaults <- c(mzDichotomous=1,mzMeanDifferences=0,mzEffectSizes=0,
                   mzCorrelations=0,mzAlphas=.8,mzProportions=.5)
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'predictive probabilities match metafor and survive engine restore'), {
  ns <- asNamespace('Metafora');d <- model_data()
  for(test in c('z','knha')) {
   extra <- list(predProbTableShow=TRUE,predProbPlotShow=TRUE,
                 predProbThreshold=unname(pred_defaults[name]),testType=test)
   a <- classic_rob_engine(name,d,extra)$analysis
   expect_null(a$data)
   state <- a$results$mzPredProbPlot$state
   expect_true(state$ok)
   expected <- stats::predict(a$modelResults,prob=paste0('<',format(state$cutoff,digits=17,scientific=TRUE,decimal.mark='.')))
   expect_equal(state$probability,as.numeric(expected$prob),tolerance=1e-12)
   expect_equal(state$spread,as.numeric(expected$pi.se))
   expect_identical(state$distribution,expected$pi.dist)
   mass <- sum(diff(state$x)*(head(state$y,-1)+tail(state$y,-1))/2)
   expect_equal(mass,.99,tolerance=1e-4)
   expect_equal(a$results$predProbTable$getCell(col='probability',rowNo=1)$value,100*state$probability)
   directory <- tempfile('predprob_');dir.create(directory)
   tryCatch({
    a$.setResourcesPathSource(function(n,suffix)list(rootPath=directory,relPath=paste0('prediction.',suffix)))
    expect_true(a$.createImage('.mzPredProbPlot',a$results$mzPredProbPlot))
    restored <- classic_rob_engine(name,d,extra,previous=snapshot(a),run=FALSE)$analysis
    expect_null(restored$data)
    expect_equal(restored$results$mzPredProbPlot$state,state)
    expect_true(restored$results$mzPredProbPlot$saveAs(file.path(directory,'restored.png')))
    expect_gt(file.info(file.path(directory,'restored.png'))$size,1000)
   },finally={
    stopifnot(startsWith(normalizePath(directory,winslash='/'),normalizePath(tempdir(),winslash='/')))
    unlink(directory,recursive=TRUE)
   })
  }
 })
 test_that(paste(name,'threshold direction and visibility reuse the main fit'), {
  d <- model_data();extra <- list(predProbTableShow=TRUE,predProbThreshold=unname(pred_defaults[name]))
  a <- classic_rob_engine(name,d,extra)$analysis
  calls <- 0L;failFit <- function(...) {calls <<- calls+1L;stop('Unexpected statistical refit')}
  above <- extra;above$predProbDirection <- 'above'
  b <- with_mocked_bindings(classic_rob_engine(name,d,above,previous=snapshot(a),changes='predProbDirection')$analysis,
      rma=failFit,rma.uni=failFit,.package='metafor')
  expect_identical(calls,0L)
  expect_equal(a$results$predProbResults$state$result$probability+b$results$predProbResults$state$result$probability,1,tolerance=1e-12)
  above$predProbThreshold <- above$predProbThreshold+.01
  c <- with_mocked_bindings(classic_rob_engine(name,d,above,previous=snapshot(b),changes='predProbThreshold')$analysis,
      rma=failFit,rma.uni=failFit,.package='metafor')
  expect_identical(calls,0L)
  expect_equal(c$modelResults$b,a$modelResults$b)
  # Attivare il disegno riusa anche la probabilita' gia' preparata per la tabella.
  above$predProbPlotShow <- TRUE
  reused <- with_mocked_bindings(classic_rob_engine(name,d,above,previous=snapshot(c),changes='predProbPlotShow')$analysis,
      mzComputePredictiveProbability=function(...)stop('Unnecessary probability recomputation'),.package='Metafora')
  expect_true(reused$results$mzPredProbPlot$state$ok)
 })
})

test_that('Natural thresholds convert through the native monotone transformations', {
 ns <- asNamespace('Metafora');scaleFn <- get('mzPredictiveProbabilityScale',ns)
 cases <- list(c('dichotomous','RR','.8'),c('dichotomous','OR','1.2'),c('meandiff','ROM','1.1'),
   c('correlation','ZCOR','.3'),c('proportion','PLN','.2'),c('proportion','PLO','.2'),
   c('proportion','PRZ','.2'),c('proportion','PAS','.2'),c('alpha','AHW','.8'),c('alpha','ABT','.8'))
 for(case in cases) {
  scale <- scaleFn(case[1],case[2]);threshold <- as.numeric(case[3])
  expect_true(scale$domain(threshold))
  expect_equal(as.numeric(scale$inverse(scale$forward(threshold))),threshold,tolerance=1e-12)
 }
 expect_error(scaleFn('proportion','PFT'),'reference sample size')
})

test_that('Unsupported contexts explain the limitation without stopping other results', {
 ns <- asNamespace('Metafora');compute <- get('mzComputePredictiveProbability',ns)
 base <- list(predProbTableShow=TRUE,predProbPlotShow=TRUE,predProbThreshold=1)
 for(extra in list(list(modelest='FE'),list(moderator='mod'),list(predProbThreshold=0))) {
  a <- make_analysis('mzDichotomous',extra=utils::modifyList(base,extra))
  expect_true(inherits(a$modelResults,'rma'))
  expect_false(a$results$mzPredProbPlot$state$ok)
  expect_true(nzchar(a$results$mzPredProbPlot$state$message))
 }
 a <- make_analysis('mzProportions',extra=list(modelmea='PFT',predProbTableShow=TRUE,predProbPlotShow=TRUE))
 expect_false(a$results$mzPredProbPlot$state$ok)
 expect_match(a$results$mzPredProbPlot$state$message,'reference sample size')
 fit <- make_analysis('mzDichotomous')$modelResults
 expect_error(compute(fit,'dichotomous','RR','1;stop(1)','below'),'numeric')
 expect_error(compute(fit,'dichotomous','RR',Inf,'below'),'finite')
 expect_error(compute(fit,'dichotomous','RR',1,'invalid'),'Choose')
})

# Verifica sul fit reale, non soltanto sulle funzioni di trasformazione isolate.
test_that('Every supported transformed measure agrees with native predict probabilities', {
 ns <- asNamespace('Metafora')
 compute <- get('mzComputePredictiveProbability',ns)
 cases <- list(
  c('mzDichotomous','RR','.8'),c('mzDichotomous','OR','1.2'),c('mzDichotomous','RD','0'),c('mzDichotomous','AS','0'),
  c('mzMeanDifferences','MD','0'),c('mzMeanDifferences','SMD','0'),c('mzMeanDifferences','SMDH','0'),
  c('mzMeanDifferences','SMD1','0'),c('mzMeanDifferences','SMD1H','0'),c('mzMeanDifferences','ROM','1.1'),
  c('mzCorrelations','COR','.3'),c('mzCorrelations','UCOR','.3'),c('mzCorrelations','ZCOR','.3'),
  c('mzAlphas','ARAW','.8'),c('mzAlphas','AHW','.8'),c('mzAlphas','ABT','.8'),
  c('mzProportions','PR','.2'),c('mzProportions','PLN','.2'),c('mzProportions','PLO','.2'),
  c('mzProportions','PRZ','.2'),c('mzProportions','PAS','.2'),c('mzEffectSizes','GEN','0'))
 for(case in cases) {
  a <- make_analysis(case[1],extra=list(modelmea=case[2]))
  b <- compute(a$modelResults,a$options$tabbedControlModelType,case[2],as.numeric(case[3]),'above')
  expected <- predict(a$modelResults,prob=paste0('>',format(b$cutoff,digits=17,scientific=TRUE,decimal.mark='.')))
  expect_true(b$ok)
  expect_equal(b$probability,as.numeric(expected$prob),tolerance=1e-12)
  expect_true(all(is.finite(b$x)))
  expect_true(all(is.finite(b$y)))
 }
})

# Visibilita nativa e riattivazione: non una sostituzione del risultato con testo/NaN.
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'hides predictive outputs with moderators and restores selected views'), {
  options <- list(predProbTableShow=TRUE,predProbPlotShow=TRUE,
                  predProbThreshold=unname(pred_defaults[name]))
  a <- make_analysis(name,extra=options)
  expect_true(a$results$predProbTable$visible)
  expect_true(a$results$mzPredProbPlot$visible)
  b <- make_analysis(name,extra=utils::modifyList(options,list(moderator='mod')),
        previous=snapshot(a),changes='moderator')
  expect_false(b$results$predProbTable$visible)
  expect_false(b$results$mzPredProbPlot$visible)
  expect_true(b$options$predProbTableShow)
  expect_true(b$options$predProbPlotShow)
  c <- make_analysis(name,extra=options,previous=snapshot(b),changes='moderator')
  expect_true(c$results$predProbTable$visible)
  expect_true(c$results$mzPredProbPlot$visible)
  expect_true(c$results$mzPredProbPlot$state$ok)
  expect_equal(c$results$mzPredProbPlot$state$probability,a$results$mzPredProbPlot$state$probability)
 })
})
