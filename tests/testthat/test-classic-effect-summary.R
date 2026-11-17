# Confronta i valori realmente passati e disegnati con i modelli della Tabella Forest.
test_that('Effect Sizes Classic summaries match pooled model estimates and variances', {
 ns <- asNamespace('Metafora')
 labelOf <- function(x)unname(as.character(if(!is.null(x$labels))x$labels else if(length(x)>=3L)x[[3L]] else NULL))
 for(method in c('FE','REML')) for(grouped in c(FALSE,TRUE)) for(reverse in c(FALSE,TRUE)) {
  opts <- list(modelest=method,forestPlotMode='posLorT',forestTableShow=TRUE,ordSubgr=reverse)
  if(grouped)opts$subgroup <- 'group'
  a <- make_analysis('mzEffectSizes',extra=opts)
  captured <- NULL;drawn <- list();original <- get('mzForestPlotAll',ns);nativeText <- graphics::text
  file <- tempfile(fileext='.pdf');grDevices::pdf(file,width=12,height=10)
  tryCatch(with_mocked_bindings(
    expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
    text=function(...) {args<-list(...);drawn[[length(drawn)+1L]]<<-args;do.call(nativeText,args)},
   mzForestPlotAll=function(...) {captured<<-list(...);do.call(original,captured)},.package='Metafora'),
   finally={grDevices::dev.off();unlink(file)})
  expect_identical(captured$ilab,cbind(sprintf('%.3f',a$dataProc$yi),sprintf('%.3f',a$dataProc$vi)))
  table <- get('mzBuildForestTable',ns)(a)$data
  expected <- list(a$modelResults)
  if(grouped)expected <- c(expected,unname(a$subgroupResults[levels(droplevels(a$dataProc$subgroup))]))
  for(fit in expected) {
   es <- sprintf('%.3f',as.numeric(fit$b[1]));variance <- sprintf('%.3f',as.numeric(fit$se[1])^2)
   expect_true(any(vapply(drawn,function(x)identical(labelOf(x),es),logical(1))))
   expect_true(any(vapply(drawn,function(x)identical(labelOf(x),variance),logical(1))))
   # La tabella conserva i numeri completi, il grafico applica solo l'arrotondamento.
   expect_true(any(abs(as.numeric(table[['Estimate<sup>1</sup>']])-as.numeric(fit$b[1]))<1e-10))
   expect_true(any(abs(as.numeric(table$Var)-as.numeric(fit$se[1])^2)<1e-10))
  }
  expect_identical(captured$ilabStat[[1]]$overall,sprintf('%.3f',as.numeric(a$modelResults$b[1])))
  if(grouped) {
   levels <- levels(droplevels(a$dataProc$subgroup))
   expect_identical(unname(captured$ilabStat[[2]]$groups),vapply(levels,function(g)
    sprintf('%.3f',as.numeric(a$subgroupResults[[g]]$se[1])^2),character(1),USE.NAMES=FALSE))
  }
 }
})

test_that('Effect Sizes Classic does not add pooled summaries with moderators', {
 ns<-asNamespace('Metafora')
 for(grouped in c(FALSE,TRUE)) {
  opts<-list(moderator='mod',forestPlotMode='posLorT');if(grouped)opts$subgroup<-'group'
  a<-make_analysis('mzEffectSizes',extra=opts);captured<-NULL;original<-get('mzForestPlotAll',ns)
  file<-tempfile(fileext='.pdf');grDevices::pdf(file,width=12,height=10)
  tryCatch(with_mocked_bindings(
   expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
   mzForestPlotAll=function(...) {captured<<-list(...);do.call(original,captured)},.package='Metafora'),
   finally={grDevices::dev.off();unlink(file)})
  expect_identical(captured$ilabStat,c(FALSE,FALSE))
 }
})
