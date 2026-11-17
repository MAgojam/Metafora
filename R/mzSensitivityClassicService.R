# Il callback Image legge soltanto questo payload numerico preparato in run.
.mzPrepareLeaveOneOutClassic <- function(self,result,fit) {
    r <- .mzSortLeaveOneOut(result,self$options$leave1outOrder,self$options$leave1outGroupOrder)
    panels <- attr(r,'mz_loo_groups',exact=TRUE)
    sections <- if(is.null(panels))list(list(label='',start=1L,end=nrow(r),pooled=c(
        list(es=as.numeric(fit$beta),ci_lo=fit$ci.lb,ci_hi=fit$ci.ub),.mzLooPooledStats(fit),
        attr(r,'mz_loo_pooled_prediction',exact=TRUE))))else panels
    parts <- list();positions <- numeric();headers <- list();cursor <- 0
    field <- function(x,name,n=1L)if(is.null(x[[name]]))rep(NA_real_,n)else as.numeric(x[[name]])
    for(j in seq_along(sections)) {
        section <- sections[[j]];take <- seq.int(section$start,section$end);count <- length(take)
        if(!is.null(panels)) {cursor<-cursor+1;headers[[j]]<-list(label=section$label,position=cursor);cursor<-cursor+.5}
        remaining <- if(is.null(panels))fit$k-1L else count-1L
        rows <- data.frame(label=as.character(r$slab[take]),es=r$estimate[take],
            lo=r$ci.lb[take],hi=r$ci.ub[take],p=r$pval[take],tau2=r$tau2[take],
            tau=sqrt(r$tau2[take]),i2=r$I2[take],pi_lo=field(r,'pi.lb',nrow(r))[take],
            pi_hi=field(r,'pi.ub',nrow(r))[take],summary=FALSE,influential=r$influential[take])
        if(remaining<2L)rows[,c('tau2','tau','i2')]<-NA_real_
        q<-section$pooled
        summary<-data.frame(label=if(is.null(panels)).('Original Pooled Estimate')else .('Original subgroup estimate'),
            es=q$es,lo=q$ci_lo,hi=q$ci_hi,p=field(q,'pval'),tau2=field(q,'tau2'),tau=field(q,'tau'),
            i2=field(q,'I2'),pi_lo=field(q,'pi_lo'),pi_hi=field(q,'pi_hi'),summary=TRUE,influential=FALSE)
        parts[[j]]<-rbind(rows,summary)
        positions<-c(positions,cursor+seq_len(count));cursor<-cursor+count+1.3
        positions<-c(positions,cursor);cursor<-cursor+1.2
    }
    list(rows=do.call(rbind,parts),y=cursor+1-positions,
        headers=lapply(headers,function(h)list(label=h$label,y=cursor+1-h$position)),
        model_label=.mzSensitivityModelLabel(self,fit),prediction='pi.lb'%in%names(r),
        common_effect=mzEstimatorId(fit)%in%c('FE','EE','CE'),level=fit$level)
}

mzRenderLeaveOneOutClassic <- function(self,image) {
    data<-self$results$leave1outClassicCache$state
    if(!is.list(data)||is.null(data$rows))return(FALSE)
    tryCatch({
        r<-data$rows;m<-mzCumulativeMeasure(self)
        trans<-function(x)if(is.function(m$atransf))m$atransf(x)else x
        lo<-trans(r$lo);hi<-trans(r$hi)
        fmt<-function(x,id)vapply(x,.mzSensitivityFormat,character(1),id=id)
        columns<-cbind(paste0(fmt(trans(r$es),'Estimate'),' [',fmt(pmin(lo,hi),'Lower'),', ',fmt(pmax(lo,hi),'Upper'),']'),
            fmt(r$p,'p'),fmt(r$i2,'I2'),fmt(r$tau,'Tau'),fmt(r$tau2,'Tau2'))
        labels<-list(paste0(m$xlab_short,' [',self$options$level,'% CI]'),
            .('p-value'),quote(I^2~'(%)'),quote(tau),quote(tau^2))
        widths<-c(68,62,64,64)
        if(isTRUE(data$common_effect)) {
            columns<-columns[,1:3,drop=FALSE];labels<-labels[1:3];widths<-widths[1:2]
        }
        bounds<-c(r$lo,r$hi,if(data$prediction)c(r$pi_lo,r$pi_hi))
        bounds<-bounds[is.finite(bounds)];if(!length(bounds))return(FALSE)
        axis<-range(pretty(range(bounds),n=4));unit<-diff(axis)/310
        labelWidth<-min(320,max(210,nchar(r$label)*6+42))
        ends<-188+c(0,cumsum(widths))
        limits<-c(axis[1]-unit*(labelWidth+20),axis[2]+unit*(tail(ends,1)+20))
        args<-list(x=r$es,ci.lb=r$lo,ci.ub=r$hi,rows=data$y,
            slab=ifelse(r$summary,'',r$label),annotate=FALSE,header=.('Study Omitted'),
            alim=axis,xlim=limits,ylim=c(0,max(data$y)+3),xlab=m$xlab,
            atransf=if(is.function(m$atransf))m$atransf else FALSE,psize=1,
            pch=ifelse(r$summary,NA_integer_,15L),
            col=ifelse(!is.na(r$influential)&r$influential,'#e87352','#3e6da9'),
            ilab=columns,ilab.xpos=axis[2]+unit*ends,ilab.pos=2,
            ilab.lab=as.expression(labels),cex=.8,refline=NA_real_)
        if(isTRUE(self$options$leave1outShade))args$shade<-which(!r$summary&cumsum(!r$summary)%%2L==1L)
        # Numerici shade sono coordinate delle righe, non indici.
        if(!is.null(args$shade))args$shade<-data$y[args$shade]
        previous<-options(na.action='na.pass');on.exit(options(previous),add=TRUE)
        plotted<-do.call(metafor::forest,args)
        for(header in data$headers)graphics::text(limits[1],header$y,header$label,pos=4,col='#3e6da9',font=2,cex=.85)
        pooled<-which(r$summary)
        starts<-c(1L,head(pooled,-1L)+1L)
        for(j in seq_along(pooled)) {
            i<-pooled[j]
            if(is.finite(r$es[i])) {
                graphics::segments(r$es[i],data$y[i]-.55,r$es[i],data$y[starts[j]]+.5,col='#e87352',lty=2)
                metafor::addpoly(r$es[i],ci.lb=r$lo[i],ci.ub=r$hi[i],rows=data$y[i],
                    annotate=FALSE,mlab='',col='#e87352',border='#e87352',cex=.8)
            }
            graphics::text(limits[1],data$y[i],r$label[i],pos=4,font=2,cex=.8)
            graphics::text(limits[1],data$y[i]-.48,data$model_label,pos=4,col='#666666',cex=.65)
        }
        if(data$prediction) {
            valid<-is.finite(r$pi_lo)&is.finite(r$pi_hi)
            graphics::segments(r$pi_lo[valid],data$y[valid]-.24,r$pi_hi[valid],data$y[valid]-.24,col='#507980',lwd=2)
            a<-trans(r$pi_lo[valid]);b<-trans(r$pi_hi[valid])
            graphics::text(args$ilab.xpos[1],data$y[valid]-.3,
                paste0('PI [',fmt(pmin(a,b),'Lower'),', ',fmt(pmax(a,b),'Upper'),']'),pos=2,col='#507980',cex=.65)
        }
        if(isTRUE(data$common_effect))graphics::mtext(
            .('Common-effect model: tau-squared and tau are not estimated.'),
            side=1,line=4,adj=0,cex=.65,col='#666666')
        TRUE
    },error=function(e){mark('[LOO] classic failed',.mzLogErrorText(e), .loc = "mzSensitivityClassicService.R:92");FALSE})
}
