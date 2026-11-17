for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'main statistics and Interactive Forest agree with independent metafor fits'),{
    f<-function(n)get(n,asNamespace('Metafora'))
    for(method in c('FE','REML'))for(test in if(method=='FE')'z'else c('z','t','knha','adhoc')) {
        a<-make_analysis(name,extra=list(modelest=method,testType=test,level=99))
        fit<-a$modelResults
        reference<-do.call(metafor::rma,native_args(name,extra=list(method=method,test=test,level=99)))
        for(field in c('yi','vi','beta','se','zval','pval','ci.lb','ci.ub','tau2','I2','H2','QE','QEp'))
            expect_equal(as.numeric(fit[[field]]),as.numeric(reference[[field]]),
                tolerance=1e-9,info=paste(name,method,test,field))
        expect_equal(as.numeric(weights(fit)),as.numeric(weights(reference)),tolerance=1e-9)
        s<-f('mzHtmlD3Service')$new();p<-s$.__enclos_env__$private
        mi<-p$jsGetMeasureInfo(a$options$tabbedControlModelType,a$options$modelmea,FALSE)
        forest<-jsonlite::fromJSON(p$d3BuildForestJson(fit,mi,qnorm(.995),seq_len(fit$k),FALSE,NULL))
        expect_equal(forest$studies$es,round(as.numeric(reference$yi),4))
        expect_equal(forest$studies$ci_lo,round(as.numeric(reference$yi-qnorm(.995)*sqrt(reference$vi)),4))
        expect_equal(forest$studies$ci_hi,round(as.numeric(reference$yi+qnorm(.995)*sqrt(reference$vi)),4))
        expect_equal(forest$pooled$es,round(as.numeric(reference$beta),4))
        expect_equal(forest$pooled$ci_lo,round(as.numeric(reference$ci.lb),4))
        expect_equal(forest$pooled$ci_hi,round(as.numeric(reference$ci.ub),4))
    }
 })
})
