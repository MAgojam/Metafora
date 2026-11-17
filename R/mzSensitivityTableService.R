# Tabelle di sensibilita: numeri R, presentazione D3 condivisa con Forest Table.
.mzSensitivityFormat <- function(value,id) {
    if(length(value)!=1L || is.na(value))return('\u2014')
    if(!is.numeric(value))return(as.character(value))
    if(!is.finite(value))return('\u2014')
    if(id %in% c('p','Qp'))return(if(value<.001)'< .001' else sprintf('%.3f',value))
    if(id %in% c('Step','k'))return(sprintf('%.0f',value))
    sprintf(if(id %in% c('Tau2','Tau'))'%.4f' else if(id=='I2')'%.1f' else '%.3f',value)
}

.mzSensitivityTableHtml <- function(self,payload,static,kind) {
    frame <- mzHtmlD3Service$new()$forestTableHtml(self,payload,kind,renderer='renderSensitivityTable')
    styles <- paste(regmatches(static,gregexpr('(?s)<style>.*?</style>',static,perl=TRUE))[[1]],collapse='')
    static <- gsub('(?s)<style>.*?</style>','',static,perl=TRUE)
    css <- '<style>.mz-ft-output.mz-ft-ready>.mz-ft-static{display:none}.mz-ft-output:not(.mz-ft-ready)>.metafora-d3-output{position:absolute;visibility:hidden}@media print{.mz-ft-output>.metafora-d3-output{display:none!important}.mz-ft-output>.mz-ft-static{display:block!important}}</style>'
    paste0('<!DOCTYPE html><html><head><meta charset="UTF-8">',css,styles,frame$head,
        '</head><body><div class="mz-ft-output" data-mz-sensitivity-table="',kind,'" data-mz-table-analysis="',
        .mzForestHeaderEscape(frame$analysisScope),'">',frame$body,
        '<div class="mz-ft-static"><h4>',htmlSpecialChars(payload$title),'</h4>',static,
        '</div></div></body></html>')
}

.mzBuildSensitivityTable <- function(self,df,ids,groups=NULL,summaryRows=integer(),
                                     kind,title,note,linked=TRUE,finalRows=integer()) {
    labels <- c(Study=if(kind=='loo-table').('Study Omitted') else .('Study Added'),
        Step=.('Step'),k='k',Estimate=.('Estimate'),SE='SE',Statistic=.('Statistic'),p='p-value',
        Lower='LL',Upper='UL',PredictionLower='PI LL',PredictionUpper='PI UL',
        Tau2='\u03c4\u00b2',Tau='\u03c4',I2='I\u00b2 (%)',H2='H\u00b2',Q='Q',Qp='p(Q)')
    tips <- c(Study=paste0(labels['Study'],'\n',.('Dataset: '),self$options$slab),
        Step=.('Cumulative step within the subgroup. Sorting the table does not recompute the sequence.'),
        k=.('Number of studies in the fitted model.'),
        Estimate=.('Effect estimate on the displayed scale.'),
        SE=.('Standard error on the model scale.'),Statistic=.('Native test statistic on the model scale.'),
        p=.('Test of the effect against the null'),
        Lower=.('Lower confidence limit on the displayed effect scale.'),
        Upper=.('Upper confidence limit on the displayed effect scale.'),
        PredictionLower=.('Lower prediction limit for the true effect in a new study.'),
        PredictionUpper=.('Upper prediction limit for the true effect in a new study.'),
        Tau2=.('Between-study variance on the analysis scale'),
        Tau=.('Between-study standard deviation on the analysis scale'),
        I2=.('Heterogeneity as a percentage'),H2=.('Heterogeneity ratio.'),
        Q=.('Cochran heterogeneity statistic.'),Qp=.('P-value of the heterogeneity test.'))
    # Il nome derivato da labels non deve diventare Study.Study.
    names(tips)[1] <- 'Study'
    headers <- vapply(names(df),function(id)paste0('<span class="mz-ft-header" aria-label="',
        .mzForestHeaderEscape(tips[[id]]),'">',.mzForestHeaderEscape(labels[[id]]),'</span>'),character(1))
    columns <- lapply(seq_along(df),function(i)list(id=names(df)[i],header=headers[i],
        super=if(names(df)[i]%in%c('Lower','Upper'))paste0(self$options$level,'% CI') else
            if(names(df)[i]%in%c('PredictionLower','PredictionUpper'))paste0(self$options$level,'% PI') else ''))
    groupValues <- if(is.null(groups))rep('',nrow(df))else groups
    rows <- lapply(seq_len(nrow(df)),function(i) {
        values <- lapply(names(df),function(id).mzSensitivityFormat(df[[id]][i],id));names(values)<-names(df)
        sorting <- lapply(df,function(col) {value<-col[i];if(is.na(value))NULL else value})
        list(id=ids[i],index=i,label=as.character(df$Study[i]),group=groupValues[i],
            summary=i%in%summaryRows,overall=is.null(groups)&&i%in%summaryRows,final=i%in%finalRows,
            values=values,sort=sorting)
    })
    i18n <- c(.mzD3InteractionTexts(self),list('Table controls'=.('Table controls'),
        'Columns'=.('Columns'),'Reset view'=.('Reset view'),'Table zoom'=.('Table zoom'),
        'Table area'=.('Table area'),'Sort within subgroups'=.('Sort within subgroups'),
        'Ascending'=.('Ascending'),'Descending'=.('Descending'),'Original order'=.('Original order')))
    payload <- list(title=title,columns=columns,rows=rows,grouped=!is.null(groups),linked=linked,
        shade=if(kind=='loo-table')isTRUE(self$options$leave1outShade)else isTRUE(self$options$cumulativeShade),note=note,i18n=i18n)
    plain <- df
    # La copia statica usa gli stessi valori formattati, senza script nel body.
    for(id in names(plain))plain[[id]]<-vapply(seq_len(nrow(df)),function(i).mzSensitivityFormat(df[[id]][i],id),character(1))
    staticHeaders <- unname(labels[names(df)])
    if(!is.null(groups)){plain<-cbind(Subgroup=groupValues,plain);staticHeaders<-c(.('Subgroup'),staticHeaders)}
    static <- mzHtmlTableBuilder(headers=staticHeaders,data=plain,
        groupingColumns=if(is.null(groups))NULL else 'Subgroup',summaryRows=summaryRows,
        rowShade=if(isTRUE(payload$shade))'#F5F7FA'else NULL,
        footnote=htmlSpecialChars(note),colAlign=c('left',rep('right',ncol(plain)-1L)))
    list(data=df,payload=payload,html=.mzSensitivityTableHtml(self,payload,static,kind))
}

mzBuildLeaveOneOutTable <- function(self,l1o=self$leave1outResults,res=self$modelResults,
                                    sortOrder=self$options$leave1outOrder) {
    r <- .mzSortLeaveOneOut(l1o,sortOrder,isTRUE(self$options$leave1outGroupOrder))
    panels <- attr(r,'mz_loo_groups',exact=TRUE)
    remaining <- rep(res$k-1L,nrow(r))
    if(!is.null(panels))for(g in panels)remaining[seq.int(g$start,g$end)]<-g$end-g$start
    m <- mzCumulativeMeasure(self);trans <- function(x)if(is.function(m$atransf))m$atransf(x)else x
    prediction<-isTRUE(self$options$leave1outPrediction)&&'pi.lb'%in%names(r)
    rowData <- function(study,es,se,stat,p,lo,hi,tau2,I2,H2,Q,Qp,pi_lo=NA_real_,pi_hi=NA_real_) {
        low<-trans(lo);high<-trans(hi)
        out<-data.frame(Study=study,Estimate=trans(es),SE=se,Statistic=stat,p=p,Lower=pmin(low,high),
            Upper=pmax(low,high),I2=I2,Tau=sqrt(tau2),Tau2=tau2,H2=H2,Q=Q,Qp=Qp,stringsAsFactors=FALSE)
        if(prediction) {
            a<-trans(pi_lo);b<-trans(pi_hi)
            out$PredictionLower<-pmin(a,b);out$PredictionUpper<-pmax(a,b)
        }
        out
    }
    df <- rowData(r$slab,r$estimate,r$se,r$stat,r$pval,r$ci.lb,r$ci.ub,r$tau2,r$I2,r$H2,r$QE,r$QEp,
        if(prediction)r$pi.lb else NA_real_,if(prediction)r$pi.ub else NA_real_)
    df[remaining<2L,c('Tau2','Tau','I2','H2','Q','Qp')]<-NA_real_
    ids <- if('study_id'%in%names(r))r$study_id else .mzD3StudyIds(res)[attr(r,'mz_loo_indices')]
    sections <- if(is.null(panels))list(list(label='',start=1L,end=nrow(r),
        pooled=c(list(es=as.numeric(res$beta),ci_lo=res$ci.lb,ci_hi=res$ci.ub),.mzLooPooledStats(res),
            attr(r,'mz_loo_pooled_prediction',exact=TRUE)))) else panels
    parts<-list();allIds<-character();groupValues<-character();summaries<-integer();offset<-0L
    for(j in seq_along(sections)) {
        g<-sections[[j]];take<-seq.int(g$start,g$end);q<-g$pooled
        summary<-rowData(if(is.null(panels)).('Original Pooled Estimate')else .('Original subgroup estimate'),
            q$es,q$se,q$stat,q$pval,q$ci_lo,q$ci_hi,q$tau2,q$I2,q$H2,q$Q,q$Qp,
            if(is.null(q$pi_lo))NA_real_ else q$pi_lo,if(is.null(q$pi_hi))NA_real_ else q$pi_hi)
        parts[[j]]<-rbind(df[take,,drop=FALSE],summary)
        allIds<-c(allIds,ids[take],paste0('loo-summary-',j));groupValues<-c(groupValues,rep(g$label,length(take)+1L))
        offset<-offset+length(take)+1L;summaries<-c(summaries,offset)
    }
    note<-paste0(.('Effect scale: '),m$xlab,'. ',
        .('SE, test statistics, p-values and heterogeneity use the fitted model scale.'),' ',
        if(is.null(panels)).('Each study is omitted from the complete fitted dataset.') else
            .('Each study is omitted from its own subgroup, and the model is refitted using only the remaining studies in that subgroup. Comparisons refer to the original subgroup pooled estimate.'),' ',
        .('At least two studies are required in each subgroup. Results that cannot be estimated with the selected test are shown as missing.'),' ',
        .('When one study remains after omission, metafor uses z inference, even if t or Knapp-Hartung was selected; heterogeneity is not estimable from a single study.'))
    if(mzEstimatorId(res)%in%c('FE','EE','CE'))note<-paste(note,
        .('Common-effect model: tau-squared and tau are not estimated.'))
    if(prediction)note<-paste(note,.('Prediction intervals use the native metafor method for a new true effect, on the displayed scale. At least two studies must remain; unavailable intervals are shown as missing.'))
    displayed<-do.call(rbind,parts)
    if(mzEstimatorId(res)%in%c('FE','EE','CE'))displayed<-displayed[,!names(displayed)%in%c('Tau','Tau2'),drop=FALSE]
    .mzBuildSensitivityTable(self,displayed,allIds,if(is.null(panels))NULL else groupValues,
        summaries,'loo-table',.('Leave-One-Out Table'),note,linked=TRUE)
}

mzBuildCumulativeTable <- function(self,result) {
    n<-length(result$estimate);num<-function(x)if(is.null(x))rep(NA_real_,n)else as.numeric(x)
    m<-mzCumulativeMeasure(self);trans<-function(x)if(is.function(m$atransf))m$atransf(x)else x
    low<-trans(result$ci.lb);high<-trans(result$ci.ub)
    statistics<-.mzCumulativeStatistics(result)
    df<-data.frame(Study=as.character(result$slab),Step=if(is.null(result$.mzStep))seq_len(n)else result$.mzStep,
        k=num(result$k),Estimate=trans(result$estimate),SE=num(result$se),
        Statistic=num(if(is.null(result$tval))result$zval else result$tval),p=num(result$pval),
        Lower=pmin(low,high),Upper=pmax(low,high),I2=statistics$i2,Tau=statistics$tau,Tau2=statistics$tau2,
        H2=num(result$H2),Q=num(result$Q),Qp=num(result$Qp),stringsAsFactors=FALSE)
    df[df$k<2L,c('Tau2','Tau','I2','H2','Q','Qp')]<-NA_real_
    if(!is.null(result$pi.lb)) {
        a<-trans(result$pi.lb);b<-trans(result$pi.ub)
        df$PredictionLower<-pmin(a,b);df$PredictionUpper<-pmax(a,b)
    }
    groups<-result$.mzSubgroup
    final<-if(is.null(result$.mzGroups))n else vapply(result$.mzGroups,`[[`,integer(1),'end')
    note<-paste0(.('Effect scale: '),m$xlab,'. ',
        .('SE, test statistics, p-values and heterogeneity use the fitted model scale. Each row adds one study; this is not a sequential stopping test.'),' ',
        .('Sorting this table changes presentation only; Step and k retain the original cumulative sequence.'),' ',
        if(!is.null(groups)).('Cumulative sequences are fitted separately within each subgroup; step and k restart in every group.')else '',
        ' ',.('With one study remaining, metafor uses z inference; heterogeneity is not estimable.'))
    if(!is.null(result$pi.lb))note<-paste(note,
        .('Prediction intervals use the native metafor method for a new true effect, with the selected test and confidence level. At least two studies must be included; unavailable limits are shown as missing.'))
    if(mzEstimatorId(self$modelResults)%in%c('FE','EE','CE')) {
        df<-df[,!names(df)%in%c('Tau','Tau2'),drop=FALSE]
        note<-paste(note,.('Common-effect model: tau-squared and tau are not estimated.'))
    }
    .mzBuildSensitivityTable(self,df,paste0('cumulative-step-',seq_len(n)),groups,kind='cumulative-table',
        title=.('Cumulative Meta-Analysis Table'),note=note,linked=FALSE,finalRows=final)
}
