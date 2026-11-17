# I simboli ROB seguono l'ordine nativo anche con etichette duplicate e valori mancanti.
classic_rob_models <- c('mzDichotomous','mzMeanDifferences','mzEffectSizes')
classic_rob_data <- function() {
 d <- model_data();d$slab <- rep('Repeated label',nrow(d))
 d$D1 <- rep(c('Low','Some concerns','High',NA,'Unknown'),length.out=nrow(d))
 d[['Overall rating']] <- rev(d$D1)
 d
}
for (model in classic_rob_models) local({name <- model
 test_that(paste(name,'Classic ROB annotates retained studies without refitting'), {
  ns <- asNamespace('Metafora');data <- classic_rob_data()
  base <- list(forestPlotMode='posLorT',subgroup='group',forestPredictionStyle='dist')
  a <- make_analysis(name,data=data,extra=base)
  extra <- c(base,list(forestRobShow=TRUE,robDomains=c('Overall rating','D1')))
  b <- with_mocked_bindings(
   make_analysis(name,data=data,extra=extra,previous=snapshot(a),changes=c('forestRobShow','robDomains')),
   rma=function(...)stop('Unexpected refit from ROB'),
   rma.uni=function(...)stop('Unexpected refit from ROB'),.package='metafor')
  expect_equal(b$modelResults$b,a$modelResults$b)
  expect_equal(b$modelResults$vi,a$modelResults$vi)
  expect_equal(b$modelResults$k,nrow(data))
  expect_null(b$results$mzForestPlotSizeCache$state$xlim)
  expect_gt(b$results$mzForestPlot$size$width,a$results$mzForestPlot$size$width)
  expect_gt(b$results$mzForestPlot$size$height,a$results$mzForestPlot$size$height)
  collect <- get('.mzCollectRobDomains',ns)(b)
  expect_identical(collect$values$D1[4:5],c('na','na'))
  file <- tempfile(fileext='.pdf');grDevices::pdf(file,width=15,height=15)
  on.exit({grDevices::dev.off();unlink(file)},add=TRUE)
  native <- get('mzForestPlotAll',ns);result <- NULL
  with_mocked_bindings(
   expect_true(b$.__enclos_env__$private$.mzForestPlot(b$results$mzForestPlot)),
   mzForestPlotAll=function(...) {result <<- native(...);result},.package='Metafora')
  expect_identical(result$rob$names,extra$robDomains)
  expect_identical(result$rob$headings,c('R1','D1'))
  expect_equal(sort(result$rob$index),seq_len(nrow(data)))
  expect_equal(result$rob$rows,result$tmp$rows)
  expect_true(all(result$rob$centresIn>result$rob$plotRightIn))
  expect_lt(max(result$rob$centresIn),grDevices::dev.size('in')[1])
  expect_identical(result$rob$separatorY,0)
  expect_true(all(diff(result$rob$legendRowsIn)<0))
  expect_gt(result$rob$legendTopIn,max(result$rob$legendRowsIn))
  expect_s4_class(snapshot(b),'Message')
  hidden <- with_mocked_bindings(make_analysis(name,data=data,extra=base,
   previous=snapshot(b),changes=c('forestRobShow','robDomains')),
   rma=function(...)stop('Unexpected refit from ROB'),
   rma.uni=function(...)stop('Unexpected refit from ROB'),.package='metafor')
  expect_equal(hidden$results$mzForestPlot$size,a$results$mzForestPlot$size)
 })
 test_that(paste(name,'Classic ROB renders all study orders and moderator footer'), {
  ns <- asNamespace('Metafora');data <- classic_rob_data()
  file <- tempfile(fileext='.pdf');grDevices::pdf(file,width=15,height=16)
  on.exit({grDevices::dev.off();unlink(file)},add=TRUE)
  native <- get('mzForestPlotAll',ns)
  for(grouped in c(FALSE,TRUE))for(reverse in c(FALSE,TRUE))
   for(order in c('obs','fit','prec','resid','rstandard','abs.resid','abs.rstandard')) {
    options <- list(forestPlotMode='posLorT',forestRobShow=TRUE,robDomains=c('D1','Overall rating'),
                    forestOrder=order,ordSubgr=reverse,moderator='mod')
    if(grouped)options$subgroup <- 'group'
    a <- make_analysis(name,data=data,extra=options);result <- NULL
    with_mocked_bindings(
     expect_true(a$.__enclos_env__$private$.mzForestPlot(a$results$mzForestPlot)),
     mzForestPlotAll=function(...) {result <<- native(...);result},.package='Metafora')
    expect_equal(sort(result$rob$index),seq_len(nrow(data)))
    expect_equal(result$rob$rows,result$tmp$rows)
    expect_gt(result$footer$notes$bottom,result$rob$legendTopIn)
    expect_true(result$rob$legendHorizontal)
    expect_equal(length(unique(result$rob$legendRowsIn)),1L)
    expect_true(all(diff(result$rob$legendXIn)>0))
   }
 })
})

test_that('Classic ROB identity map agrees with native forest text, including tie breaks', {
 ns <- asNamespace('Metafora');map <- get('.mzClassicRobRows',ns)
 d <- model_data();vars <- unname(model_specs$mzDichotomous$vars)
 d[2,vars] <- d[1,vars]
 model <- make_analysis('mzDichotomous',data=d)$modelResults
 file <- tempfile(fileext='.pdf');grDevices::pdf(file,width=13,height=10)
 on.exit({grDevices::dev.off();unlink(file)},add=TRUE)
 nativeText <- get('text',asNamespace('metafor'))
 keys <- c(as.list(c('obs','fit','prec','resid','rstandard','abs.resid','abs.rstandard')),
           list(rep(c(1,2),length.out=model$k),seq_len(model$k)))
 for(key in keys) {
  captured <- NULL
  with_mocked_bindings(
   tmp <- metafor::forest(model,order=key,ilab=seq_len(model$k),addfit=FALSE),
   text=function(x,y,labels=NULL,...) {
    if(is.numeric(labels) && length(labels)==model$k && setequal(labels,seq_len(model$k)))
     captured <<- list(index=as.integer(labels),rows=y)
    nativeText(x,y,labels,...)
   },.package='metafor')
  expect_false(is.null(captured))
  result <- map(model,key)
  expect_equal(result$index,captured$index)
  expect_equal(result$rows,captured$rows)
 }
})

# La legenda non puo' produrre chiavi ambigue o eliminare casi per NA nei giudizi.
test_that('Classic ROB headings remain unique and map to full selected names', {
 layout <- get('.mzClassicRobLayout',asNamespace('Metafora'))(
  list(forestRobShow=TRUE,robDomains=c('Long domain heading','R1','D1')))
 expect_identical(layout$headings,c('RR1','R1','D1'))
 expect_true(any(grepl('Long domain heading',layout$mappingLines,fixed=TRUE)))
 expect_gte(layout$widthIn,sum(layout$widths)+.32)
})
for(model in classic_rob_models)local({name <- model
 test_that(paste(name,'Classic ROB stays aligned after excluding an invalid input row'), {
  ns <- asNamespace('Metafora');d <- classic_rob_data()
  d[[unname(model_specs[[name]]$vars[1])]][2] <- NA
  a <- make_analysis(name,data=d,extra=list(forestPlotMode='posLorT',forestRobShow=TRUE,
                    robDomains=c('D1','Overall rating')))
  rob <- get('.mzCollectRobDomains',ns)(a)
  expected <- get('MetaforServiceClass',ns)$new()$mzClassifyRobDomains(
       d[rownames(a$dataProc),c('D1','Overall rating'),drop=FALSE])$values
  expect_identical(rob$values,expected)
  expect_equal(length(rob$values[[1]]),nrow(a$dataProc))
 })
})

# OA e' soltanto un'intestazione grafica; nomi e ordine degli input non cambiano.
test_that('Classic ROB compacts domains and separates OA without renaming inputs', {
 layoutFn <- get('.mzClassicRobLayout',asNamespace('Metafora'))
 vars <- c(paste0('D',1:5),'Overall')
 x <- layoutFn(list(forestRobShow=TRUE,robDomains=vars))
 expect_identical(x$names,vars)
 expect_identical(x$headings,c(paste0('D',1:5),'OA'))
 expect_equal(x$widths,rep(.30,6))
 expect_equal(x$gapsBefore,c(rep(0,5),.08))
 centres <- cumsum(x$widths+x$gapsBefore)-x$widths/2
 expect_equal(diff(centres)[1:4],rep(.30,4))
 expect_gt(diff(centres)[5],diff(centres)[4])
 expect_lt(x$widthIn,2.5)
 expect_true(any(grepl('OA = Overall risk of bias',x$mappingLines,fixed=TRUE)))
 collision <- layoutFn(list(forestRobShow=TRUE,robDomains=c('OA','Overall')))
 expect_identical(collision$headings,c('OA','Overall'))
})
