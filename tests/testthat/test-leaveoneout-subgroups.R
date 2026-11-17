loo_fun <- function(name)get(name,asNamespace('Metafora'))
for(model in names(model_specs))local({
    name <- model
    test_that(paste(name,'within-subgroup omissions equal independent native refits'),{
        a <- make_analysis(name,extra=list(subgroup='group',leave1outTableShow=TRUE,level=99))
        data <- a$leave1outResults;fit <- a$modelResults
        expect_equal(nrow(data),nrow(a$dataProc))
        expect_equal(unique(data$subgroup),c('A','Z'))
        expect_setequal(data$study_id,loo_fun('.mzD3StudyIds')(fit))
        for(group in attr(data,'mz_loo_groups')) {
            idx <- which(as.character(a$dataProc$subgroup)==group$label)
            native <- metafor::rma(yi=fit$yi.f[idx],vi=fit$vi.f[idx],method=fit$method,test=fit$test,level=99)
            expect_equal(group$pooled$es,as.numeric(native$beta))
            for(i in seq_along(idx)) {
                keep <- idx[-i];pos <- group$start+i-1L
                q <- suppressWarnings(metafor::rma(yi=fit$yi.f[keep],vi=fit$vi.f[keep],
                    method=fit$method,test=fit$test,level=99))
                for(pair in list(c('estimate','beta'),c('se','se'),c('stat','zval'),c('pval','pval'),
                    c('ci.lb','ci.lb'),c('ci.ub','ci.ub'),c('tau2','tau2'),c('I2','I2'),
                    c('H2','H2'),c('QE','QE'),c('QEp','QEp')))
                    expect_equal(as.numeric(data[[pair[1]]][pos]),as.numeric(q[[pair[2]]]),tolerance=1e-9)
                expect_equal(data$delta_estimate[pos],as.numeric(q$beta-native$beta))
                expect_equal(data$influential[pos],!(as.numeric(native$beta)>=q$ci.lb && as.numeric(native$beta)<=q$ci.ub))
            }
        }
        expect_match(a$results$outHtmlLeave1Out$content,'Comparisons refer to the original subgroup pooled estimate',fixed=TRUE)
        expect_match(a$results$outHtmlLeave1Out$content,'rowspan="7"',fixed=TRUE)
        plain <- make_analysis(name,extra=list(leave1outTableShow=TRUE))
        expect_null(attr(plain$leave1outResults,'mz_loo_groups'))
        expect_equal(plain$leave1outResults,loo_fun('MetaforServiceClass')$new()$mzComputeLeave1Out(plain$modelResults))
    })
})

test_that('two-study subgroups are included and singleton omissions are explicitly missing',{
    d<-model_data();d$group<-c('pair','pair',rep('other',10),'single')
    a<-make_analysis('mzDichotomous',data=d,extra=list(subgroup='group',leave1outTableShow=TRUE))
    r<-a$leave1outResults;f<-a$modelResults
    take<-which(r$subgroup=='pair')
    expect_length(take,2L)
    expect_equal(r$estimate[take],rev(as.numeric(f$yi.f[1:2])))
    expect_equal(r$se[take],rev(sqrt(as.numeric(f$vi.f[1:2]))))
    expect_true(all(is.na(r$estimate[r$subgroup=='single'])))
    expect_length(attr(r,'mz_loo_groups'),3L)
    expect_match(a$results$outHtmlLeave1Out$content,'At least two studies are required',fixed=TRUE)
})

test_that('subgroup LOO preserves native inference variants, FE and SJ2',{
    for(method in c('FE','DL','REML','SJ2'))for(test in if(method=='FE')'z' else c('z','t','knha','adhoc')) {
        a<-make_analysis('mzEffectSizes',extra=list(subgroup='group',leave1outTableShow=TRUE,
            modelest=method,testType=test,level=90))
        r<-a$leave1outResults;f<-a$modelResults
        for(group in attr(r,'mz_loo_groups')) {
            idx<-which(as.character(a$dataProc$subgroup)==group$label)
            for(i in seq_along(idx)) {
                q<-suppressWarnings(loo_fun('mzFitRma')(yi=f$yi.f[idx[-i]],vi=f$vi.f[idx[-i]],
                    method=method,test=test,level=90))
                pos<-group$start+i-1L
                for(pair in list(c('estimate','beta'),c('se','se'),c('stat','zval'),c('pval','pval'),
                    c('ci.lb','ci.lb'),c('ci.ub','ci.ub'),c('I2','I2'),c('QE','QE')))
                    expect_equal(as.numeric(r[[pair[1]]][pos]),as.numeric(q[[pair[2]]]),tolerance=1e-8)
                if(method!='FE')expect_equal(r$tau2[pos],q$tau2,tolerance=1e-8)
            }
        }
    }
})

test_that('grouped LOO order and study identities survive duplicate labels and excluded rows',{
    d<-model_data();d$slab[]<-'Repeated';d$ai[3]<-NA_real_
    a<-make_analysis('mzDichotomous',data=d,extra=list(subgroup='group',leave1outPlotShow=TRUE))
    original<-a$leave1outResults
    expect_length(unique(original$study_id),nrow(a$dataProc))
    expect_false('study-row:3' %in% original$study_id)
    for(order in c('none','shift','estimate','pval','study')) {
        r<-loo_fun('.mzSortLeaveOneOut')(original,order)
        expect_setequal(r$study_id,original$study_id)
        expect_equal(unique(r$subgroup),c('A','Z'))
        for(group in c('A','Z'))expect_setequal(r$study_id[r$subgroup==group],original$study_id[original$subgroup==group])
        fit<-a$modelResults;ids<-loo_fun('.mzD3StudyIds')(fit)
        for(i in seq_len(nrow(r))) {
            omitted<-match(r$study_id[i],ids)
            keep<-setdiff(which(as.character(a$dataProc$subgroup)==r$subgroup[i]),omitted)
            q<-metafor::rma(yi=fit$yi.f[keep],vi=fit$vi.f[keep],method=fit$method,test=fit$test)
            expect_equal(r$estimate[i],as.numeric(q$beta))
        }

    }
    expect_match(a$results$outD3Leave1OutPlot$content,'data-mz-stable-plot=',fixed=TRUE)
})


test_that('two-study subgroups disclose the native single-study z fallback under t-based inference',{
    d<-model_data();d$group<-c('pair','pair',rep('other',11))
    for(test in c('z','t','knha','adhoc')) {
        a<-make_analysis('mzDichotomous',data=d,extra=list(subgroup='group',leave1outTableShow=TRUE,testType=test))
        r<-a$leave1outResults;fit<-a$modelResults;pos<-which(r$subgroup=='pair')
        for(i in 1:2) {
            q<-suppressWarnings(metafor::rma(yi=fit$yi.f[3L-i],vi=fit$vi.f[3L-i],method=fit$method,test=test))
            expect_identical(q$test,'z')
            expect_equal(r$stat[pos[i]],as.numeric(q$zval))
            expect_equal(r$pval[pos[i]],as.numeric(q$pval))
            expect_equal(r$ci.lb[pos[i]],as.numeric(q$ci.lb))
            expect_equal(r$ci.ub[pos[i]],as.numeric(q$ci.ub))
        }
        expect_match(a$results$outHtmlLeave1Out$content,'metafor uses z inference',fixed=TRUE)
        expect_match(a$results$outHtmlLeave1Out$content,'Statistic',fixed=TRUE)
    }
})

for(model in names(model_specs))local({
    name <- model
    test_that(paste(name,'LOO subgroup direction changes presentation without changing omissions'),{
        a <- make_analysis(name,extra=list(subgroup='group',leave1outTableShow=TRUE,
            leave1outGroupOrder=TRUE,leave1outOrder='estimate'))
        original <- a$leave1outResults
        forward <- loo_fun('.mzSortLeaveOneOut')(original,'estimate',FALSE)
        reverse <- loo_fun('.mzSortLeaveOneOut')(original,'estimate',TRUE)
        expect_identical(unique(forward$subgroup),c('A','Z'))
        expect_identical(unique(reverse$subgroup),c('Z','A'))
        expect_equal(reverse$estimate,original$estimate[match(reverse$study_id,original$study_id)])
        for(g in attr(reverse,'mz_loo_groups')) {
            take <- seq.int(g$start,g$end)
            expect_true(all(reverse$subgroup[take]==g$label))
            expect_equal(reverse$study_id[take],forward$study_id[forward$subgroup==g$label])
            expect_equal(g$pooled,attr(original,'mz_loo_groups')[[match(g$label,
                vapply(attr(original,'mz_loo_groups'),function(x)x$label,character(1)))]]$pooled)
        }
        html <- a$results$outHtmlLeave1Out$content
        expect_identical(unique(vapply(sens_payload(a$results$outHtmlLeave1Out)$rows,`[[`,character(1),'group')),c('Z','A'))
        b <- make_analysis(name,extra=list(subgroup='group',leave1outPlotShow=TRUE,leave1outGroupOrder=TRUE))
        doc <- b$results$outD3Leave1OutPlot$content
        expect_match(doc,'Original subgroup estimate',fixed=TRUE)
        expect_equal(a$leave1outResults,original)
    })
})

for(model in names(model_specs))local({
    name <- model
    test_that(paste(name,'LOO plot reports native p and heterogeneity for omissions and pooled fits'),{
        a <- make_analysis(name,extra=list(subgroup='group',leave1outPlotShow=TRUE,leave1outGroupOrder=TRUE))
        f <- a$modelResults;r <- a$leave1outResults
        service <- loo_fun('mzHtmlD3Service')$new();p <- service$.__enclos_env__$private
        mi <- p$jsGetMeasureInfo(a$options$tabbedControlModelType,a$options$modelmea,TRUE)
        payload <- jsonlite::fromJSON(p$d3BuildLeave1OutJson(f,r,mi,95,'estimate',a),simplifyVector=FALSE)
        expect_match(payload$model_label,'REML',fixed=TRUE)
        for(row in payload$studies) {
            idx <- match(row$id,r$study_id)
            expect_equal(row$pval,r$pval[idx],tolerance=1e-4)
            expect_equal(row$tau2,r$tau2[idx],tolerance=1e-4)
            expect_equal(row$tau,sqrt(r$tau2[idx]),tolerance=1e-4)
            expect_equal(row$I2,r$I2[idx],tolerance=1e-4)
        }
        for(g in payload$groups) {
            idx <- which(as.character(a$dataProc$subgroup)==g$label)
            native <- metafor::rma(yi=f$yi.f[idx],vi=f$vi.f[idx],method=f$method,test=f$test)
            expect_equal(g$pooled$pval,as.numeric(native$pval),tolerance=1e-4)
            expect_equal(g$pooled$tau2,native$tau2,tolerance=1e-4)
            expect_equal(g$pooled$tau,sqrt(native$tau2),tolerance=1e-4)
            expect_equal(g$pooled$I2,native$I2,tolerance=1e-4)
        }
    })
})
test_that('single remaining study does not display estimable heterogeneity',{
    d<-model_data();d$group<-c('pair','pair',rep('other',11))
    a<-make_analysis('mzDichotomous',data=d,extra=list(subgroup='group',leave1outPlotShow=TRUE))
    p<-loo_fun('mzHtmlD3Service')$new()$.__enclos_env__$private
    mi<-p$jsGetMeasureInfo(a$options$tabbedControlModelType,a$options$modelmea,TRUE)
    out<-jsonlite::fromJSON(p$d3BuildLeave1OutJson(a$modelResults,a$leave1outResults,mi,95,'none',a),simplifyVector=FALSE)
    group<-Filter(function(g)g$label=='pair',out$groups)[[1]]
    for(i in seq.int(group$start,group$end)) {
        row<-out$studies[[i]]
        expect_true(is.finite(row$pval));expect_null(row$tau2);expect_null(row$tau);expect_null(row$I2)
    }
})
