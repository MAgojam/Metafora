# Verifica del contenuto statico e del contesto dei sei percorsi HTML comuni.
for(model in names(model_specs)) local({
    name <- model
    test_that(paste(name,'publishes isolated HTML documents without assets in the export body'), {
        a <- make_analysis(name,extra=list(showTextRepo=TRUE,showTost=TRUE,
            showTes=TRUE,leave1outTableShow=TRUE))
        for(result in c('OutputHtml','outHtmlTost','outHtmlTes','outHtmlLeave1Out')) {
            html <- a$results[[result]]$content
            expect_match(html,if(result=='outHtmlLeave1Out')'data-mz-sensitivity-table=' else 'data-mz-html-key=',fixed=TRUE,info=result)
            body <- sub('(?s)^.*?<body>(.*)</body>.*$','\\1',html,perl=TRUE)
            # Gli asset del documento isolato sono leciti nell'attributo srcdoc;
            # il body esterno esportato e la tabella statica restano senza script.
            exportBody <- gsub('(?s)<iframe\\b.*?</iframe>','',body,perl=TRUE)
            expect_false(grepl('<style>|<script>',exportBody),info=result)
            if(result=='outHtmlLeave1Out')expect_match(exportBody,'<table',fixed=TRUE)
            expect_true(nzchar(body))
        }
        # Nessuna scrittura ripetuta a contenuto identico, ma un risultato
        # invalidato deve essere riempito anche se il valore e rimasto uguale.
        output <- new.env();output$content <- '';filled <- FALSE;writes <- 0L
        output$isFilled <- function()filled
        output$setContent <- function(value){output$content <- value;filled <<- TRUE;writes <<- writes+1L}
        publish <- get('.mzSetHtmlResult',asNamespace('Metafora'))
        publish(a,output,'test','<details open><summary>Title</summary>Value</details>')
        publish(a,output,'test','<details open><summary>Title</summary>Value</details>')
        expect_equal(writes,1L)
        filled <- FALSE
        publish(a,output,'test','<details open><summary>Title</summary>Value</details>')
        expect_equal(writes,2L)
    })
})

test_that('reports with identical study labels have independent and stable radio groups', {
    a <- make_analysis('mzEffectSizes',extra=list(showTextRepo=TRUE))
    b <- make_analysis('mzEffectSizes',extra=list(showTextRepo=TRUE))
    radioNames <- function(html)regmatches(html,gregexpr("name='mz-report-[^']+'",html))[[1]]
    expect_length(unique(radioNames(a$results$OutputHtml$content)),1)
    expect_false(identical(radioNames(a$results$OutputHtml$content),radioNames(b$results$OutputHtml$content)))
    report <- get('mz_create_report_html',asNamespace('Metafora'))
    expect_identical(radioNames(report(a$modelResults,self=a)),radioNames(a$results$OutputHtml$content))
})

test_that('HTML render errors remain readable text rather than executable markup', {
    ns <- asNamespace('Metafora')
    fit <- metafor::rma(yi=model_data()$yi,vi=model_data()$vi)
    local_mocked_bindings(TOSTmeta=function(...)stop('<script>TOST failure</script>'),.package='TOSTER')
    local_mocked_bindings(tes=function(...)stop('<img src=x onerror=bad()>'),.package='metafor')
    tost <- get('mzHtmlTost',ns)(fit)
    tes <- get('mzHtmlTes',ns)(fit)
    report <- get('mz_create_report_html',ns)(structure(list(),class='<script>bad class</script>'))
    for(html in c(tost,tes,report)) {
        expect_false(grepl('<script>|<img ',html))
        expect_match(html,'&lt;',fixed=TRUE)
    }
})

test_that('a missing HTML helper preserves readable results and explains the fallback', {
    a <- make_analysis('mzEffectSizes')
    local_mocked_bindings(.mzReadHtmlHelper=function()stop('Controlled missing resource'),.package='Metafora')
    html <- get('.mzHtmlDocument',asNamespace('Metafora'))(a,'test','<p>Original results</p>')
    expect_match(html,'Original results',fixed=TRUE)
    expect_match(html,'View state is unavailable; the results remain readable.',fixed=TRUE)
    expect_false(grepl('<script>',html,fixed=TRUE))
})
