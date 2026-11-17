for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'matches native metafor across tests and confidence levels'),{
        for (test in c('z','t','knha','adhoc')) for (level in c(90,95,99)) {
            actual <- make_analysis(name,extra=list(testType=test,level=level))$modelResults
            expected <- do.call(metafor::rma,native_args(name,extra=list(test=test,level=level)))
            for (field in c('beta','se','ci.lb','ci.ub','pval','tau2','QE','QEp')) {
                expect_equal(unname(actual[[field]]),unname(expected[[field]]),tolerance=1e-10,
                             info=paste(name,test,level,field))
            }
        }
    })
    test_that(paste(name,'SJ2 matches an independent HE then SJ fit'),{
        he <- do.call(metafor::rma,native_args(name,extra=list(method='HE')))
        expected <- do.call(metafor::rma,native_args(name,extra=list(method='SJ',control=list(tau2.init=he$tau2))))
        actual <- make_analysis(name,extra=list(modelest='SJ2'))$modelResults
        for (field in c('beta','tau2','se','ci.lb','ci.ub','pval'))
            expect_equal(unname(actual[[field]]),unname(expected[[field]]),tolerance=1e-10)
    })
    test_that(paste(name,'moderator coefficients and residual Q are coherent'),{
        a <- make_analysis(name,extra=list(moderator=list('mod')))
        expected <- do.call(metafor::rma,native_args(name,extra=list(mods=~ mod,data=model_data())))
        expect_equal(unname(a$modelResults$beta),unname(expected$beta),tolerance=1e-10)
        expect_equal(nrow(a$results$textRICH$asDF),a$modelResults$p)
        expect_equal(a$results$tableTauSqaured$asDF$QallDF[1],expected$k-expected$p)
        expect_equal(a$results$tableTauSqaured$asDF$QallPval[1],expected$QEp,tolerance=1e-10)
    })
    test_that(paste(name,'uses the current report service and confidence level'),{
        a <- make_analysis(name,extra=list(showTextRepo=TRUE,level=90))
        html <- a$results$OutputHtml$content
        expect_true(nchar(html)>100L)
        expect_false(grepl('Text reporting function is not available',html,fixed=TRUE))
        expect_match(html,'90%')
    })
})

test_that('Generic accepts equivalent standard errors and variances',{
    d <- model_data();se <- d;se$vi <- sqrt(d$vi)
    variance <- make_analysis('mzEffectSizes',data=d)$modelResults
    standard_error <- make_analysis('mzEffectSizes',data=se,extra=list(meaType='seimt'))$modelResults
    expect_equal(standard_error$yi,variance$yi)
    expect_equal(standard_error$vi,variance$vi,tolerance=1e-12)
    expect_equal(standard_error$beta,variance$beta,tolerance=1e-12)
})
