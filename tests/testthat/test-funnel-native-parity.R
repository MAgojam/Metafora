# Il riferimento è la funzione nativa installata, non una replica della formula D3.
for(model in names(model_specs))local({
 name<-model
 test_that(paste(name,'Funnel Classic and D3 follow native coordinates, axis limits and reference'),{
    f<-function(n)get(n,asNamespace('Metafora'))
    file<-tempfile(fileext='.pdf');pdf(file,width=9,height=8)
    on.exit({dev.off();unlink(file)},add=TRUE)
    for(moderated in c(FALSE,TRUE)) {
        a<-make_analysis(name,extra=if(moderated)list(moderator='mod')else list())
        fit<-a$modelResults
        service<-f('mzHtmlD3Service')$new();private<-service$.__enclos_env__$private
        mi<-private$jsGetMeasureInfo(a$options$tabbedControlModelType,a$options$modelmea,TRUE)
        for(axis in c('sei','vi','ni','sqrtni','lni'))for(inverse in c(FALSE,TRUE)) {
            if(axis%in%c('ni','sqrtni','lni')&&is.null(fit$ni)) {
                expect_error(f('mzFunnelCoordinates')(fit,axis,inverse),'sample sizes')
                next
            }
            nativeAxis<-if(inverse&&axis!='lni')paste0(axis,'nv')else axis
            native<-metafor::funnel(fit,yaxis=nativeAxis,yaxs='i')
            nativeLimits<-graphics::par('usr')[3:4]
            json<-jsonlite::fromJSON(private$d3BuildFunnelJson(fit,mi,yaxis=axis,yaxisInv=inverse))
            expect_equal(json$studies$es,as.numeric(native$x),tolerance=1e-12)
            expect_equal(json$studies$y,as.numeric(native$y),tolerance=1e-12)
            expect_equal(json$studies$label,native$slab)
            expect_equal(as.numeric(json$y_limits),as.numeric(nativeLimits),tolerance=1e-12)
            expect_equal(json$funnel_center,if(moderated)0 else as.numeric(fit$beta[1]))
            expect_null(json$trimfill)
            if(moderated) {
                expect_true(json$residuals)
                expect_equal(json$transf_type,'identity')
                expect_equal(json$measure,'Residual Value')
            }
        }
        nativeFunnel<-metafor::funnel;captured<-NULL
        # Il servizio accetta anche un semplice elenco di opzioni di presentazione.
        opts<-list(yaxis='sei',yaxisInv=FALSE,enhanceFunnel=FALSE,
            modelmea=a$options$modelmea,tabbedControlModelType=a$options$tabbedControlModelType,
            showBackTransform=TRUE,xAxisTitle='')
        expect_true(testthat::with_mocked_bindings(f('MetaforServiceClass')$new()$mzFunnelPlot(fit,opts),
            funnel=function(...) {captured<<-list(...);do.call(nativeFunnel,captured)},.package='metafor'))
        expect_equal(captured$refline,if(moderated)0 else as.numeric(fit$beta[1]))
        if(moderated)expect_null(captured$atransf)
    }
 })
})

test_that('funnel residual identity survives omitted rows and repeated labels',{
 f<-function(n)get(n,asNamespace('Metafora'))
 d<-data.frame(yi=c(.2,.4,NA,.6,.1,.9),vi=rep(.1,6),mod=c(1,2,3,4,NA,6))
 fit<-NULL
 expect_warning(fit<-metafor::rma(yi,vi,mods=~mod,slab=rep('Repeated',6),data=d),'omitted')
 file<-tempfile(fileext='.pdf');pdf(file);on.exit({dev.off();unlink(file)},add=TRUE)
 native<-metafor::funnel(fit)
 previous<-getOption('na.action')
 data<-f('mzFunnelCoordinates')(fit)
 expect_equal(data$es,as.numeric(native$x))
 expect_equal(data$y,as.numeric(native$y))
 expect_length(unique(data$ids),nrow(native))
 expect_equal(data$labels,native$slab)
 expect_equal(getOption('na.action'),previous)
})
