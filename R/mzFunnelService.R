# Dati del funnel secondo funnel.rma: effetti senza moderatori, residui e
# relative SE di rstandard con moderatori. Le trasformazioni restano etichette.
mzFunnelCoordinates <- function(fit,yaxis='sei',inverse=FALSE) {
    pooled<-isTRUE(fit$int.only)
    labels<-as.character(fit$slab);ids<-.mzD3StudyIds(fit)
    if(pooled) {
        es<-as.numeric(fit$yi);se<-sqrt(as.numeric(fit$vi));ni<-fit$ni
        keep<-if(length(labels)!=length(es))which(fit$not.na)else seq_along(es)
    } else {
        previous<-options(na.action='na.pass');on.exit(options(previous),add=TRUE)
        residuals<-stats::rstandard(fit)
        keep<-which(!is.na(residuals$resid))
        es<-as.numeric(residuals$resid[keep]);se<-as.numeric(residuals$se[keep])
        ni<-if(is.null(fit$ni.f))NULL else fit$ni.f[keep]
    }
    labels<-labels[keep];ids<-ids[keep]
    inverted<-isTRUE(inverse)&&yaxis!='lni'
    if(yaxis%in%c('ni','sqrtni','lni')&&is.null(ni))
        stop('Sample-size axes require sample sizes stored in the fitted model.')
    y<-switch(yaxis,sei=se,vi=se^2,ni=as.numeric(ni),sqrtni=sqrt(ni),lni=log(ni),
        stop('Unsupported funnel axis.'))
    if(inverted)y<-1/y
    finite<-is.finite(y)
    if(!any(finite))stop('The selected funnel axis has no finite values.')
    limits<-if(yaxis%in%c('sei','vi')&&!inverted)c(max(y),0)else
        if(yaxis%in%c('ni','sqrtni')&&inverted)c(max(y,na.rm=TRUE),min(y,na.rm=TRUE))else
            c(min(y,na.rm=TRUE),max(y,na.rm=TRUE))
    if(any(!is.finite(limits)))stop('The selected funnel axis has non-finite limits.')
    list(es=es,se=se,ni=ni,y=y,labels=labels,ids=ids,indices=keep,
        center=if(pooled)as.numeric(fit$beta[1])else 0,
        y_limits=limits,residuals=!pooled,inverted=inverted)
}
