# Confronta le bande realmente disegnate dal forest nativo e dal pannello ROB.
for (model in c('mzDichotomous','mzMeanDifferences','mzEffectSizes'))local({name <- model
 test_that(paste(name,'Classic zebra continues into ROB without statistical refits'), {
  ns <- asNamespace('Metafora');d <- model_data()
  d$D1 <- rep(c('Low','High','Some concerns'),length.out=nrow(d))
  for(grouped in c(FALSE,TRUE))for(reverse in c(FALSE,TRUE)) {
   extra <- list(forestPlotMode='posLorT',forestRobShow=TRUE,robDomains='D1',
                 forestOrder='prec',ordSubgr=reverse,shadeRows=FALSE,addfit=TRUE)
   if(grouped)extra$subgroup <- 'group'
   a <- make_analysis(name,data=d,extra=extra)
   extra$shadeRows <- TRUE
   calls <- 0L
   failFit <- function(...) {calls <<- calls+1L;stop('Unexpected statistical refit')}
   b <- with_mocked_bindings(make_analysis(name,data=d,extra=extra,
      previous=snapshot(a),changes='shadeRows'),rma=failFit,rma.uni=failFit,.package='metafor')
   expect_identical(calls,0L)
   expect_equal(b$modelResults$b,a$modelResults$b)
   path <- tempfile(fileext='.pdf');grDevices::pdf(path,width=15,height=16)
   rects <- list();nativeRect <- graphics::rect;result <- NULL
   nativePlot <- get('mzForestPlotAll',ns)
   captureRect <- function(xleft,ybottom,xright,ytop,...) {
      rects[[length(rects)+1L]] <<- list(y=as.numeric(ybottom)+.5,col=list(...)$col)
      nativeRect(xleft,ybottom,xright,ytop,...)
   }
   tryCatch({
    with_mocked_bindings(
     with_mocked_bindings(
      with_mocked_bindings(expect_true(b$.__enclos_env__$private$.mzForestPlot(b$results$mzForestPlot)),
       mzForestPlotAll=function(...) {result <<- nativePlot(...);result},.package='Metafora'),
      rma=failFit,rma.uni=failFit,rect=captureRect,.package='metafor'),
     rect=captureRect,.package='graphics')
   },finally={grDevices::dev.off();unlink(path)})
   expect_identical(calls,0L)
   expected <- result$tmp$rows[rep_len(c(TRUE,FALSE),length(result$tmp$rows))]
   expect_equal(result$rob$shading$rows,expected)
   # Ogni banda deve comparire nel disegno nativo e nell'estensione ROB.
   for(y in expected)expect_gte(sum(vapply(rects,function(x)
      any(abs(x$y-y)<1e-8) && identical(x$col,result$rob$shading$col),logical(1))),2L)
   if(grouped) {
    expect_true(length(result$rob$headerShading$rows)>0)
    expect_false(any(result$rob$headerShading$rows %in% expected))
    expect_false(identical(result$rob$headerShading$col,result$rob$shading$col))
   } else expect_null(result$rob$headerShading)
   hidden <- extra;hidden$shadeRows <- FALSE
   c <- with_mocked_bindings(make_analysis(name,data=d,extra=hidden,
      previous=snapshot(b),changes='shadeRows'),rma=failFit,rma.uni=failFit,.package='metafor')
   expect_identical(calls,0L)
   expect_equal(c$modelResults$b,a$modelResults$b)
  }
 })
})
