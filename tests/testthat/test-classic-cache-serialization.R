# Riproduce i cicli del motore: un file Image renderizzato viene ripristinato.
# saveAs disegna; .setPath registra il risultato come fa .createImage nel motore.
for (model in names(model_specs)) local({name <- model
 test_that(paste(name, 'Classic subgroup transitions retain serializable image dimensions'), {
  engine <- function(extra, previous=NULL, changes=character()) {
   a <- make_analysis(name, extra=extra, previous=previous, changes=changes)
   private <- a$.__enclos_env__$private
   private$.analysisId <- 1L
   private$.datasetId <- 'test'
   private$.weightsStatus <- get('weightsStatus',asNamespace('jmvcore'))$NOT_APPLICABLE
   a
  }
  render <- function(a, file) {
   expect_true(a$results$mzForestPlot$saveAs(file))
   a$results$mzForestPlot$.setPath(file)
   expect_true(a$results$mzForestPlot$isFilled())
  }
  check <- function(a) {
   size <- a$results$mzForestPlot$size
   expect_true(all(is.finite(unlist(size))))
   expect_gt(size$width, 0)
   expect_gt(size$height, 0)
   cache <- a$results$mzForestPlotSizeCache$state
   expect_equal(cache$w, size$width)
   expect_equal(cache$h, size$height)
   expect_type(a$serialize(createErrorOnFailure=FALSE), 'raw')
  }
  file <- tempfile(fileext='.png')
  on.exit(unlink(file))
  base <- list(forestPlotMode='posLorT',modelest='REML',addfit=TRUE,addpred=TRUE)
  for (style in c('line2','diamond','bar','shade','dist')) {
   options <- c(base,list(forestPredictionStyle=style))
   a <- engine(options)
   render(a,file);check(a)
   for (group in list('group','group',NULL,NULL)) {
    changed <- !identical(a$options$subgroup,group)
    a <- engine(c(options,list(subgroup=group)), previous=snapshot(a),
                changes=if(changed)'subgroup' else character())
    check(a)
    render(a,file)
   }
  }
 })
})

test_that('legacy partial size caches cannot corrupt an already rendered Image', {
 a <- make_analysis('mzDichotomous',extra=list(forestPlotMode='posLorT'))
 image <- a$results$mzForestPlot
 image$.setPath('rendered.png')
 size <- image$size
 apply <- get('mzApplyCachedSize',asNamespace('Metafora'))
 for (bad in list(list(predictionStyle='diamond'),list(w=NA_real_,h=720),
                  list(w=720,h=NULL),list(w=Inf,h=720),list(w=0,h=720))) {
  a$results$mzForestPlotSizeCache$setState(bad)
  apply(image,a$results$mzForestPlotSizeCache)
  expect_identical(image$size,size)
  expect_s4_class(snapshot(a),'Message')
 }
})
