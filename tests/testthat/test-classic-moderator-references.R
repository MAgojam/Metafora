# Verifica statistica indipendente e rettangoli fisici del footer, non stime di riga.
for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'Classic moderator references and footer do not overlap the native axis'),{
  ns<-asNamespace('Metafora');spec<-get('.mzClassicForestReferences',ns)
  for(width in c(8,12)) {
   file<-tempfile(fileext='.pdf');grDevices::pdf(file,width=width,height=10)
   tryCatch(for(grouped in c(FALSE,TRUE)) for(method in c('FE','REML')) {
    opts<-list(moderator='mod',forestPlotMode='posLorT',modelest=method)
    if(grouped)opts$subgroup<-'group'
    a<-make_analysis(name,extra=opts);fit<-a$modelResults
    expected<-stats::cov.wt(matrix(fit$yi,ncol=1),wt=1/fit$vi,method='ML')
    reference<-spec(fit)
    expect_equal(reference$values,c(expected$center-sqrt(expected$cov[1]),expected$center,expected$center+sqrt(expected$cov[1])),ignore_attr=TRUE)
    captured<-NULL;result<-NULL;lines<-NULL;prefixText<-NULL
    original<-get('mzForestPlotAll',ns);nativeSegments<-graphics::segments;nativeText<-graphics::text
    with_mocked_bindings(
     with_mocked_bindings(
      expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
      text=function(...) {args<-list(...);if(identical(args$labels,c('Blue:','Descriptive only:')))prefixText<<-args;do.call(nativeText,args)},
      segments=function(...) {args<-list(...);if(identical(args$col,c('#98A2B3','#98A2B3','#3E6DA9')))lines<<-args;do.call(nativeSegments,args)},.package='graphics'),
     mzForestPlotAll=function(...) {captured<<-list(...);result<<-do.call(original,captured);result},.package='Metafora')
    expect_true(is.na(captured$refline))
    expect_equal(lines$x0,reference$values[c(1,3,2)])
    expect_identical(lines$col,c('#98A2B3','#98A2B3','#3E6DA9'))
    footer<-result$footer
    expect_identical(footer$heterogeneity$label,'Residual heterogeneity')
    expect_equal(footer$heterogeneity$row,0)
    expect_lt(footer$heterogeneity$right,footer$axis$left)
    expect_lt(footer$notes$top,footer$axis$bottom)
    expect_lt(footer$notes$top,footer$heterogeneity$bottom)
    expect_gt(footer$notes$bottom,footer$figureBottom)
    expect_equal(footer$notes$cex,result$tmp$cex)
    expect_equal(footer$title$gapAbove,footer$title$gapBelow,tolerance=1e-10)
    expect_gt(footer$title$gapAbove,0)
    expect_identical(prefixText$font,2)
    expect_identical(footer$title$text,getFromNamespace('.setlab','metafor')(fit$measure,'FALSE','FALSE',gentype=1))
    expect_equal(result$tmp$ylim[1],-.25)
   },finally={grDevices::dev.off();unlink(file)})
  }
  plain<-make_analysis(name)$modelResults
  expect_null(spec(plain))
  single<-metafor::rma(yi=plain$yi,vi=plain$vi,mods=matrix(model_data()$mod,ncol=1),intercept=FALSE)
  expect_equal(single$p,1L);expect_false(single$int.only)
  expect_length(spec(single)$values,3L)
  # La cache compatta di un modello con moderatori non taglia il riepilogo globale.
  file<-tempfile(fileext='.pdf');grDevices::pdf(file,width=10,height=8)
  tryCatch({
   result<-get('mzForestPlotAll',ns)(plain,ylim=c(-.25,plain$k+4))
   expect_null(result$footer)
   expect_lte(result$tmp$ylim[1],-3)
  },finally={grDevices::dev.off();unlink(file)})
 })
})
