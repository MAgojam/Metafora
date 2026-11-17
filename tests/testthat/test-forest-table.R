ft <- function(name)get(name,asNamespace('Metafora'))
ft_number <- function(data,id,row=nrow(data))as.numeric(data[[id]][row])
for(model in names(model_specs))local({
 name <- model
 test_that(paste(name,'Forest table uses canonical columns and complete native Summary'),{
  a <- make_analysis(name,extra=list(forestTableShow=TRUE))
  result <- ft('mzBuildForestTable')(a)
  expect_false(grepl('Error generating table:',a$results$outHtmlTable$content,fixed=TRUE))
  expect_true(nzchar(a$results$outHtmlTable$content))
  expect_equal(result$summaryRows,nrow(model_data())+1L)
  expect_equal(ft_number(result$data,'Estimate<sup>1</sup>'),as.numeric(a$modelResults$beta))
  expect_equal(ft_number(result$data,'Lower'),a$modelResults$ci.lb)
  expect_equal(ft_number(result$data,'Upper'),a$modelResults$ci.ub)
  expect_equal(ft_number(result$data,'SE'),a$modelResults$se)
  expect_equal(ft_number(result$data,'Var'),a$modelResults$se^2)
  stats <- ft('mzForestInputSummary')(a$dataProc,a$options$tabbedControlModelType)
  for(key in names(stats))expect_lt(abs(ft_number(result$data,ft('.formatColumnName')(key))-stats[[key]]),.00051)
  expect_match(result$html,'mz-ft-tooltip',fixed=TRUE)
 })
 test_that(paste(name,'Forest table has correct subgroup and overall Summary rows'),{
  a <- make_analysis(name,extra=list(forestTableShow=TRUE,subgroup='group'))
  result <- ft('mzBuildForestTable')(a);d <- result$data
  expect_length(result$summaryRows,3L)
  expect_identical(d$Study[tail(result$summaryRows,1)],'Overall Summary')
  expect_true(all(d$Study[head(result$summaryRows,-1)]=='Summary'))
  expect_equal(nrow(d),nrow(model_data())+3L)
  for(g in unique(as.character(a$dataProc$subgroup))) {
   row <- result$summaryRows[d$Subgroup[result$summaryRows]==g & nzchar(d$Subgroup[result$summaryRows])]
   expect_length(row,1)
   fit <- a$subgroupResults[[g]]
   expect_equal(ft_number(d,'Estimate<sup>1</sup>',row),as.numeric(fit$beta))
   expect_equal(ft_number(d,'Lower',row),fit$ci.lb)
   expect_equal(ft_number(d,'Upper',row),fit$ci.ub)
   stats <- ft('mzForestInputSummary')(a$dataProc[a$dataProc$subgroup==g,,drop=FALSE],a$options$tabbedControlModelType)
   for(key in names(stats))expect_lt(abs(ft_number(d,ft('.formatColumnName')(key),row)-stats[[key]]),.00051)
  }
 })
 test_that(paste(name,'Forest table never marks the last study as a pooled regression estimate'),{
  a <- make_analysis(name,extra=list(forestTableShow=TRUE,moderator='mod'))
  result <- ft('mzBuildForestTable')(a)
  expect_length(result$summaryRows,0)
  expect_equal(nrow(result$data),nrow(model_data()))
  expect_false(any(result$data$Study=='Summary'))
 })
})

test_that('column identities survive exact source names, collisions and HTML characters',{
 d <- model_data();d[['EXPERIMENTAL "N" <tag>&']] <- d$n1i
 d[['LOWER']] <- d$n2i;d[['Study ID & Case']] <- d$slab
 a <- make_analysis('mzMeanDifferences',data=d,extra=list(forestTableShow=TRUE,
     n1i='EXPERIMENTAL "N" <tag>&',n2i='LOWER',slab='Study ID & Case'))
 result <- ft('mzBuildForestTable')(a)
 expect_true(all(c('N1i','N2i','Study','Lower')%in%names(result$data)))
 headers <- paste(vapply(result$payload$columns,`[[`,character(1),'header'),collapse='')
 expect_match(headers,'EXPERIMENTAL &quot;N&quot; &lt;tag&gt;&amp;',fixed=TRUE)
 expect_match(headers,'Study ID &amp; Case',fixed=TRUE)
 expect_false(grepl('<tag>',result$html,fixed=TRUE))
 expect_equal(ft_number(result$data,'N1i'),sum(d$n1i))
 expect_equal(ft_number(result$data,'N2i'),sum(d$n2i))
})

test_that('descriptive Summary formulas use each group and do not confuse pooled SD with weighted SD',{
 d <- model_data();d$n1i <- seq_len(13)+5;d$n2i <- rev(seq_len(13))*30
 a <- make_analysis('mzMeanDifferences',data=d,extra=list(forestTableShow=TRUE))
 r <- ft('mzBuildForestTable')(a)$data
 sd1 <- sqrt(sum((d$n1i-1)*d$sd1i^2)/sum(d$n1i-1))
 sd2 <- sqrt(sum((d$n2i-1)*d$sd2i^2)/sum(d$n2i-1))
 expect_equal(ft_number(r,'Sd1i'),sd1,tolerance=.00051)
 expect_equal(ft_number(r,'Sd2i'),sd2,tolerance=.00051)
 expect_equal(ft_number(r,'M2i'),weighted.mean(d$m2i,d$n2i),tolerance=.00051)
 service <- ft('mzHtmlD3Service')$new();p <- service$.__enclos_env__$private
 info <- p$jsGetMeasureInfo('meandiff','SMD',FALSE)
 payload <- jsonlite::fromJSON(p$d3BuildForestJson(a$modelResults,info,qnorm(.975),seq_len(13),
     FALSE,NULL,ilab=as.list(d[c('n1i','m1i','sd1i','n2i','m2i','sd2i')]),
     ilab_names=c('N','M','SD','N','M','SD'),ilab_totals=c('sum','wmean','pooledsd','sum','wmean','pooledsd')))
 expect_equal(unlist(payload$pooled$ilab)[c(3,6)],round(c(sd1,sd2),2))
 expect_equal(unlist(payload$pooled$ilab)[5],round(weighted.mean(d$m2i,d$n2i),2))
})

test_that('CI source tooltips distinguish input bounds and derived variance from output intervals',{
 d <- model_data();d$low <- d$yi-qnorm(.975)*sqrt(d$vi);d$high <- d$yi+qnorm(.975)*sqrt(d$vi)
 a <- make_analysis('mzEffectSizes',data=d,extra=list(meaType='cimt',vi=NULL,ciLower='low',ciUpper='high',
     inputCiLevel=95,level=99,forestTableShow=TRUE))
 result <- ft('mzBuildForestTable')(a)
 expect_match(result$html,'low / high (95%)',fixed=TRUE)
 expect_match(result$html,'99% output CI',fixed=TRUE)
 expect_match(result$html,'Dataset: yi',fixed=TRUE)
})

test_that('input descriptive summaries have explicit known reference values',{
 data <- data.frame(ri=c(.2,.8),ai=c(.6,.9),mi=c(4,7),ni=c(10,30),xi=c(2,9))
 correlation <- ft('mzForestInputSummary')(data,'correlation')
 expect_equal(correlation$ri,.65);expect_equal(correlation$ni,40)
 alpha <- ft('mzForestInputSummary')(data,'alpha')
 expect_equal(alpha$ai,.825);expect_equal(alpha$mi,5.5);expect_equal(alpha$ni,40)
 proportion <- ft('mzForestInputSummary')(data,'proportion')
 expect_equal(proportion$xi,11);expect_equal(proportion$ni,40)
})

test_that('interactive table payload preserves exact values and safe column identities',{
 a <- make_analysis('mzMeanDifferences',extra=list(forestTableShow=TRUE,subgroup='group'))
 result <- ft('mzBuildForestTable')(a);p <- result$payload
 expect_length(p$rows,nrow(result$data))
 expect_equal(sum(vapply(p$rows,`[[`,logical(1),'summary')),3)
 expect_equal(sum(vapply(p$rows,`[[`,logical(1),'overall')),1)
 expect_true(p$grouped)
 for(i in seq_len(nrow(result$data))) {
  expect_equal(p$rows[[i]]$sort[['Estimate<sup>1</sup>']],result$data[['Estimate<sup>1</sup>']][i],tolerance=1e-14)
  expect_identical(p$rows[[i]]$values$Study,result$data$Study[i])
 }
 expect_match(result$html,'data-mz-stable-plot=',fixed=TRUE)
 expect_match(result$html,'renderForestTable',fixed=TRUE)
 expect_match(result$html,'mz-ft-static',fixed=TRUE)
 expect_match(result$html,'@media print',fixed=TRUE)
 expect_match(result$html,'data-mz-forest-table-version="12"',fixed=TRUE)
})

test_that('interactive table payload supports regressions without a fake global summary',{
 a <- make_analysis('mzEffectSizes',extra=list(forestTableShow=TRUE,moderator='mod'))
 p <- ft('mzBuildForestTable')(a)$payload
 expect_false(any(vapply(p$rows,`[[`,logical(1),'summary')))
 expect_length(p$rows,nrow(model_data()))
 expect_equal(p$rows[[1]]$sort$SE,as.numeric(ft('mzBuildForestTable')(a)$data$SE[1]))
})

test_that('copied analyses receive independent table identity and preserve the fit',{
 a <- make_analysis('mzEffectSizes',extra=list(forestTableShow=TRUE))
 options <- get('mzEffectSizesOptions',asNamespace('Metafora'))$new(
     slab='slab',yi='yi',vi='vi',modelmea='GEN',forestTableShow=TRUE)
 b <- get('mzEffectSizesClass',asNamespace('Metafora'))$new(
     options=options,data=model_data(),analysisId='copied-analysis')
 b$init();b$results$fromProtoBuf(snapshot(a),character(),character());b$postInit()
 b$run()
 expect_match(b$results$outHtmlTable$content,'copied-analysis',fixed=TRUE)
 expect_false(identical(b$results$outHtmlTable$content,a$results$outHtmlTable$content))
 expect_equal(b$modelResults$b,a$modelResults$b)
 expect_equal(ft('mzBuildForestTable')(b)$data,ft('mzBuildForestTable')(a)$data)
 html <- b$results$outHtmlTable$content
 ft('mzRenderForestTable')(b)
 expect_identical(b$results$outHtmlTable$content,html)
})
