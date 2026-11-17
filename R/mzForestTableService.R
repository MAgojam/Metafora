# Identita delle colonne indipendente da etichette e nomi scelti nel dataset.
mzForestInputKeys <- function(model) switch(model,
    dichotomous=c('ai','bi','ci','di'),meandiff=c('n1i','m1i','sd1i','n2i','m2i','sd2i'),
    correlation=c('ri','ni'),alpha=c('ai','mi','ni'),proportion=c('xi','ni'),character())

# Descrittive degli input: non sono stime meta-analitiche.
mzForestAggregate <- function(values, n=NULL, kind='sum') {
    if (kind=='sum') return(sum(values))
    if (kind=='median') return(stats::median(values))
    if (is.null(n) || sum(n)<=0) return(NA_real_)
    if (kind=='wmean') return(sum(values*n)/sum(n))
    if (kind=='pooledsd') {
        denominator <- sum(n-1)
        if (denominator<=0) return(NA_real_)
        return(sqrt(sum((n-1)*values^2)/denominator))
    }
    if (kind=='prop') return(sum(values)/sum(n))
    NA_real_
}

mzForestInputSummary <- function(data, model) {
    a <- mzForestAggregate
    switch(model,
        dichotomous=lapply(data[c('ai','bi','ci','di')],a),
        proportion=lapply(data[c('xi','ni')],a),
        correlation=list(ri=a(data$ri,data$ni,'wmean'),ni=a(data$ni)),
        alpha=list(ai=a(data$ai,data$ni,'wmean'),mi=a(data$mi,kind='median'),ni=a(data$ni)),
        meandiff=list(n1i=a(data$n1i),m1i=a(data$m1i,data$n1i,'wmean'),
            sd1i=a(data$sd1i,data$n1i,'pooledsd'),n2i=a(data$n2i),
            m2i=a(data$m2i,data$n2i,'wmean'),sd2i=a(data$sd2i,data$n2i,'pooledsd')),
        list())
}

.mzForestHeaderEscape <- function(text) {
    text <- htmlSpecialChars(as.character(text))
    text <- gsub('"','&quot;',text,fixed=TRUE)
    gsub("'",'&#39;',text,fixed=TRUE)
}

mzForestTableHeaders <- function(self, ids, interactive = TRUE) {
    o <- self$options; model <- o$tabbedControlModelType
    keys <- mzForestInputKeys(model)
    titles <- switch(model,
        dichotomous=c('Events','Non-events','Events','Non-events'),
        meandiff=c('N','Mean','SD','N','Mean','SD'),correlation=c('r','N'),
        alpha=c('\u03b1','Items','N'),proportion=c('Events','N'),character())
    source <- function(key) {
        value <- o[[key]]
        if (length(value)) paste0('\n',.("Dataset: "),as.character(value)) else ''
    }
    descriptor <- function(key) {
        role <- switch(key,ai=if(model=='alpha') .("Cronbach's alpha") else .("Experimental events"),
            bi=.("Experimental non-events"),ci=.("Control events"),di=.("Control non-events"),
            n1i=.("Group 1 sample size"),m1i=.("Group 1 mean"),sd1i=.("Group 1 standard deviation"),
            n2i=.("Group 2 sample size"),m2i=.("Group 2 mean"),sd2i=.("Group 2 standard deviation"),
            ri=.("Correlation"),mi=.("Number of items"),ni=.("Sample size"),xi=.("Events"))
        rule <- if (key=='mi') .("Summary: median number of items.") else
            if (key %in% c('ri','m1i','m2i') || (model=='alpha'&&key=='ai'))
                .("Summary: sample-size-weighted descriptive mean; not a meta-analytic estimate.") else
            if (key %in% c('sd1i','sd2i')) .("Summary: pooled within-study SD, weighted by N - 1; excludes differences between study means.") else
                .("Summary: sum across included studies.")
        paste0(role,source(key),'\n',rule)
    }
    variance <- if(model=='effectsize') switch(o$meaType,
        cimt=paste0(.("Derived from input CI limits: "),o$ciLower,' / ',o$ciUpper,
            ' (',o$inputCiLevel,'%).'),
        seimt=paste0(.("Squared input standard error: "),o$vi),
        paste0(.("Input sampling variance: "),o$vi)) else .("Calculated sampling variance on the model scale.")
    vapply(seq_along(ids),function(i) {
        id <- ids[i]; key <- keys[match(id,vapply(keys,.formatColumnName,character(1)))]
        label <- switch(id,Study=.("Study"),Subgroup=.("Subgroup"),Lower='LL',Upper='UL',
            'Estimate<sup>1</sup>'=.("Estimate"),id)
        tip <- switch(id,Study=paste0(.("Study label"),source('slab')),
            Subgroup=paste0(.("Subgroup"),source('subgroup')),
            Var=paste0(variance,'\n',.("Summary: squared SE of the pooled model estimate, on the model scale.")),
            SE=paste0(.("Standard error on the model scale."),'\n',variance,'\n',
                .("Summary: SE of the pooled model estimate.")),
            'W%'=.("Study weight (%). Subgroup summaries show the subgroup share of the overall weights; overall summary is 100%."),
            Lower=paste0(o$level,'% ',.("output CI: lower limit, on the displayed effect scale. Calculated from the model; not a source input column.")),
            Upper=paste0(o$level,'% ',.("output CI: upper limit, on the displayed effect scale. Calculated from the model; not a source input column.")),
            'Estimate<sup>1</sup>'=paste0(.("Effect estimate on the displayed scale. Summary: meta-analytic model estimate."),
                if(model=='effectsize')source('yi')else ''),NULL)
        if(length(key)&&!is.na(key)) {
            label <- titles[match(key,keys)];tip <- descriptor(key)
        }
        if(is.null(tip))tip <- label
        if(!interactive)return(paste0(.mzForestHeaderEscape(label),
            if(id=='Estimate<sup>1</sup>')'<sup>1</sup>'else ''))
        cls <- if(i>length(ids)/2) ' mz-ft-tip-end' else ''
        paste0('<span class="mz-ft-header',cls,'" tabindex="0" aria-label="',
            .mzForestHeaderEscape(tip),'">',.mzForestHeaderEscape(label),
            if(id=='Estimate<sup>1</sup>')'<sup>1</sup>'else '',
            '<span class="mz-ft-tooltip" aria-hidden="true" hidden>',.mzForestHeaderEscape(tip),'</span></span>')
    },character(1))
}

# Un solo percorso di presentazione per i sei backend; nessun fit aggiuntivo.
mzBuildForestTable <- function(self) {
    o <- self$options; model <- o$tabbedControlModelType
    fit <- self$modelResults
    computed <- self$escalcResults
    full <- self$forestData
    if(is.null(full)||is.null(fit))return(NULL)
    keys <- mzForestInputKeys(model)
    map <- as.list(stats::setNames(keys,keys))
    df <- mzExtractForestData(full,fit,o,colNameMap=map,modelType=model)
    hasSummary <- isTRUE(fit$int.only)
    overall <- if(hasSummary)tail(df,1)else df[FALSE,,drop=FALSE]
    studies <- if(hasSummary)head(df,-1)else df
    studies$.mzStudyId <- .mzD3StudyIds(fit)
    overall$.mzStudyId <- rep(NA_character_,nrow(overall))
    orderVar <- switch(o$forestOrder,obs='yi',fit='fit',prec='vi',resid='resid',
        rstandard='rstandard',abs.resid='abs.resid',abs.rstandard='abs.rstandard',NULL)
    hasGroups <- 'Subgroup'%in%names(studies)
    studies <- if(hasGroups)mzSortForestTableWithSubgroup(studies,self$dataProc,computed,self$subgroupResults,orderVar) else
        mzSortForestTableNoSubgroup(studies,self$dataProc,computed,fit,orderVar)
    summaryRows <- integer();blocks <- list();used <- 0L
    if(hasGroups) {
        subfits <- if(hasSummary)self$subgroupResults else NULL
        w <- as.numeric(weights(fit)); allGroups <- as.character(self$dataProc$subgroup)
        for(g in unique(as.character(studies$Subgroup))) {
            block <- studies[as.character(studies$Subgroup)==g,,drop=FALSE]
            sg <- subfits[[g]]
            if(hasSummary&&!is.null(sg)&&isTRUE(sg$int.only)) {
                idx <- which(allGroups==g)
                row <- tail(mzExtractForestData(full[idx,,drop=FALSE],sg,o,colNameMap=map,modelType=model),1)
                row$Subgroup <- g
                row$.mzStudyId <- NA_character_
                row[['W%']] <- sprintf('%.2f%%',sum(w[idx])/sum(w)*100)
                block <- rbind(block,row);summaryRows <- c(summaryRows,used+nrow(block))
            }
            blocks[[length(blocks)+1L]] <- block;used <- used+nrow(block)
        }
        df <- do.call(rbind,blocks)
    } else df <- studies
    if(hasSummary) {
        if(hasGroups)overall$Study <- .("Overall Summary")
        df <- rbind(df,overall);summaryRows <- c(summaryRows,nrow(df))
    }
    studyIds <- df$.mzStudyId
    df$.mzStudyId <- NULL
    # SE e Var rimangono sul modello; label e gruppi sono solo metadati.
    df <- df[,c(setdiff(names(df),c('Var','SE')),'Var','SE'),drop=FALSE]
    supers <- list()
    addSuper <- function(label,ids) {
        cols <- match(ids,names(df));if(all(!is.na(cols)))
            supers[[length(supers)+1L]] <<- list(label=label,cols=cols)
    }
    if(model=='dichotomous') {
        addSuper(.("Experimental"),c('Ai','Bi'));addSuper(.("Control"),c('Ci','Di'))
    } else if(model=='meandiff') {
        addSuper(.("Group 1"),c('N1i','M1i','Sd1i'));addSuper(.("Group 2"),c('N2i','M2i','Sd2i'))
    }
    addSuper(paste0(o$level,'% CI'),c('Lower','Upper'))
    align <- rep('center',ncol(df));align[names(df)%in%c('Study','Subgroup')] <- 'left'
    widths <- rep('auto',ncol(df))
    shade <- rep('',ncol(df));shade[names(df)=='Estimate<sup>1</sup>'] <- '#E6F2FF'
    note <- paste0('<div class="mz-ft-note"><sup>1</sup> ',htmlSpecialChars(.mzGetBackTransform(model,o$modelmea,
        isTRUE(o$showBackTransform))$xlab),'.<br>',
        htmlSpecialChars(.("Input summaries are descriptive, not meta-analytic effects. Var and SE remain on the model scale. Subgroup summaries are shown when a subgroup model is available.")),'</div>')
    html <- mzHtmlTableBuilder(headers=mzForestTableHeaders(self,names(df),interactive=FALSE),data=df,
        supertitles=supers,colWidths=widths,colAlign=align,colShade=shade,
        rowShade=if(isTRUE(o$shadeRows))'#F5F5F5'else NULL,
        groupingColumns=if(hasGroups)'Subgroup'else NULL,summaryRows=summaryRows,footnote=note)
    headers <- mzForestTableHeaders(self,names(df))
    columns <- lapply(seq_along(df),function(i) {
        parent <- Filter(function(group)i%in%group$cols,supers)
        list(id=names(df)[i],header=headers[i],super=if(length(parent))parent[[1]]$label else '',
            menuLabel=if(length(parent)&&!names(df)[i]%in%c('Lower','Upper'))
                paste(parent[[1]]$label,switch(names(df)[i],N1i='N',N2i='N',M1i='Mean',M2i='Mean',Sd1i='SD',Sd2i='SD',Ai='Events',Bi='Non-events',Ci='Events',Di='Non-events',names(df)[i])) else NULL)
    })
    rows <- lapply(seq_len(nrow(df)),function(i) {
        values <- lapply(df,function(column) {
            val <- column[i]
            if(is.numeric(column))if(is.finite(val))sprintf('%.3f',val)else '\u2014'
            else if(is.na(val))'\u2014'else as.character(val)
        })
        sorting <- lapply(names(df),function(id) {
            val <- df[[id]][i]
            if(is.numeric(val)) {if(is.finite(val))val else NULL}
            else if(id%in%c('Study','Subgroup'))as.character(val) else {
                number <- suppressWarnings(as.numeric(sub('%','',as.character(val),fixed=TRUE)))
                if(is.finite(number))number else NULL
            }
        })
        names(sorting) <- names(df)
        summary <- i%in%summaryRows
        group <- if(hasGroups)as.character(df$Subgroup[i])else ''
        list(id=if(summary)paste0('summary-',i)else studyIds[i],index=i,label=as.character(df$Study[i]),group=group,
            summary=summary,overall=summary&&(!hasGroups||!nzchar(group)),
            values=values,sort=sorting)
    })
    i18n <- list('Table controls'=.('Table controls'),'Zoom out'=.('Zoom out'),
        'Zoom in'=.('Zoom in'),'Reset zoom'=.('Reset zoom'),'Columns'=.('Columns'),
        'Reset view'=.('Reset view'),'Forest Table'=.('Forest Table'),
        'Sort within subgroups'=.('Sort within subgroups'),'Ascending'=.('Ascending'),
        'Descending'=.('Descending'),'Original order'=.('Original order'),
        'Table zoom'=.('Table zoom'),'Table area'=.('Table area'),
        'Share of overall weight'=.('Share of overall weight'),'Pinned study'=.('Pinned study'),
        'Release pinned study'=.('Release pinned study'),
        'Click to pin this study; click again to release.'=.('Click to pin this study; click again to release.'))
    payload <- list(title=.("Forest Table"),columns=columns,rows=rows,grouped=hasGroups,shade=isTRUE(o$shadeRows),
        note=paste0('1 ',.mzGetBackTransform(model,o$modelmea,isTRUE(o$showBackTransform))$xlab,'.\n',
            .("Input summaries are descriptive, not meta-analytic effects. Var and SE remain on the model scale. Subgroup summaries are shown when a subgroup model is available.")),i18n=i18n)
    frame <- mzHtmlD3Service$new()$forestTableHtml(self,payload)
    # jamovi's structured export walks document.body, including script/style text.
    # Keep assets in head and a complete plain table in body for that parser.
    matches <- gregexpr('(?s)<style>.*?</style>',html,perl=TRUE)
    staticStyles <- paste(regmatches(html,matches)[[1]],collapse='')
    html <- gsub('(?s)<style>.*?</style>','',html,perl=TRUE)
    # Only mount/print visibility glue remains here; interactive styling lives in srcjs.
    css <- '<style>.mz-ft-output.mz-ft-ready>.mz-ft-static{display:none}.mz-ft-output:not(.mz-ft-ready)>.metafora-d3-output{position:absolute;visibility:hidden}.mz-ft-static .mz-ft-note{max-width:760px;white-space:normal}@media print{.mz-ft-output>.metafora-d3-output{display:none!important}.mz-ft-output>.mz-ft-static{display:block!important}}</style>'
    list(data=df,summaryRows=summaryRows,payload=payload,
        html=paste0('<!DOCTYPE html><html><head><meta charset="UTF-8">',css,staticStyles,frame$head,
            '</head><body><div class="mz-ft-output" data-mz-forest-table-version="12" data-mz-table-analysis="',
            .mzForestHeaderEscape(frame$analysisScope),'">',
            frame$body,'<div class="mz-ft-static"><h4>',htmlSpecialChars(payload$title),'</h4>',html,
            '</div></div></body></html>'))
}

mzRenderForestTable <- function(self) {
    output <- self$results$outHtmlTable
    if(!isTRUE(self$options$forestTableShow))return(invisible(NULL))
    scope <- mzHtmlD3Service$new()$forestTableAnalysisScope(self)
    scopeMarker <- paste0('data-mz-table-analysis="',.mzForestHeaderEscape(scope),'"')
    if(output$isFilled()&&grepl('data-mz-forest-table-version="12"',output$content,fixed=TRUE)&&
        grepl(scopeMarker,output$content,fixed=TRUE))return(invisible(NULL))
    tryCatch({
        result <- mzBuildForestTable(self)
        jinfo('[HTML/FT] publish', 'summaries =',length(result$summaryRows), .loc = "mzForestTableService.R:230")
        if(!is.null(result))self$results$outHtmlTable$setContent(
            result$html)
    },error=function(e) {
        mark('[HTML/FT] failed',.mzLogErrorText(e), .loc = "mzForestTableService.R:234")
        self$results$outHtmlTable$setContent(paste(.("Error generating table:"),htmlSpecialChars(conditionMessage(e))))
    })
    invisible(NULL)
}
