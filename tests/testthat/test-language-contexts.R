# I servizi condivisi devono funzionare con il traduttore dell'analisi e anche da R puro.
test_that('optional HTML renderers preserve values and use the supplied translation context', {
    ns <- asNamespace('Metafora')
    data <- model_data()
    fit <- metafor::rma(yi=data$yi,vi=data$vi)
    original <- fit
    seen <- character()
    self <- list(options=list(translate=function(text,n=1) {
        seen <<- c(seen,text)
        text
    }))
    tost <- get('mzHtmlTost',ns)(fit,self=self)
    expect_match(tost,'Interpretation',fixed=TRUE)
    expect_true(any(grepl('specified bounds',seen,fixed=TRUE)))
    expect_match(get('mzHtmlTost',ns)(fit),'Interpretation',fixed=TRUE)
    tes <- get('mzHtmlTes',ns)(fit,self=self)
    expect_match(tes,'Excess Significance',ignore.case=TRUE)
    expect_true(any(grepl('Excess Significance',seen,ignore.case=TRUE)))
    expect_identical(fit,original)
})

test_that('report templates receive self without translating model codes or numeric results', {
    ns <- asNamespace('Metafora')
    data <- model_data()
    fit <- metafor::rma(yi=data$yi,vi=data$vi,level=99)
    self <- list(options=list(translate=function(text,n=1) {
        if (identical(text,'The analysis used the %s model.'))
            return('TRANSLATED MODEL: %s.')
        text
    }))
    report <- get('mz_create_report_html',ns)(fit,self=self)
    expect_match(report,'TRANSLATED MODEL: random-effects.',fixed=TRUE)
    expect_match(report,'99% CI',fixed=TRUE)
    digits <- get('.mz_get_digits',ns)(fit)
    expect_match(report,formatC(fit$ci.lb,digits=digits$ci,format='f'),fixed=TRUE)
    expect_false(grepl('Error generating',report,fixed=TRUE))
    expect_match(get('mz_create_report_html',ns)(fit),'The analysis used',fixed=TRUE)
})

test_that('all six introductory templates explain measurement levels and have a translation context', {
    ns <- asNamespace('Metafora')
    for (suffix in c('Correlation','Dichotomous','Effectsize','Meandiff','Proportion','Alpha')) {
        seen <- character()
        self <- list(options=list(translate=function(text,n=1) { seen <<- c(seen,text); text }))
        render <- get(paste0('.htmlInfo',suffix),ns)
        output <- render(self=self)
        expect_match(output,'Numeric codes are treated as factors',fixed=TRUE)
        expect_true(any(grepl('Numeric codes are treated as factors',seen,fixed=TRUE)))
        expect_false(any(grepl('<style>',seen,fixed=TRUE)))
        expect_identical(render(),output)
    }
})

test_that('D3 labels use the R analysis translator without changing lookup keys', {
    seen <- character()
    self <- list(options=list(translate=function(text,n=1) {
        seen <<- c(seen,text)
        paste0('T:',text)
    }))
    texts <- get('.mzD3InteractionTexts',asNamespace('Metafora'))(self)
    for(label in c('Legend','Power','Study Omitted','Low Risk','Standard Error')) {
        expect_identical(texts[[label]],paste0('T:',label))
        expect_true(label%in%seen)
    }
})
