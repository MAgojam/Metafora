# La richiesta di back-transform salvata non può modificare risultati PFT.
for (grouped in c(FALSE,TRUE)) local({
    subgroup <- grouped
    test_that(paste('PFT remains transformed with saved flags; subgroup =',subgroup), {
        d <- model_data()
        d$xi[c(1,2)] <- c(0,d$ni[2])
        extra <- list(modelmea='PFT',forestTableShow=TRUE,showTextRepo=TRUE)
        if(subgroup)extra$subgroup <- 'group'
        a <- make_analysis('mzProportions',data=d,extra=extra)
        extra$showBackTransform <- TRUE
        b <- make_analysis('mzProportions',data=d,extra=extra)
        expect_equal(b$modelResults$b,a$modelResults$b)
        expect_equal(b$results$textRICH$asDF,a$results$textRICH$asDF)
        # I report sono isolati per analisi: confronta testo e numeri,
        # non gli ID DOM che devono essere diversi fra a e b.
        reportText <- function(html) {
            body <- sub('(?s).*<body>(.*)</body>.*','\\1',html,perl=TRUE)
            gsub('<[^>]*>','',body)
        }
        expect_identical(reportText(b$results$OutputHtml$content),reportText(a$results$OutputHtml$content))
        # The interactive HTML has an analysis-specific iframe/channel identity.
        # PFT semantics must stay identical, independently of that session key.
        table <- get('mzBuildForestTable',asNamespace('Metafora'))
        expect_identical(table(b)$payload,table(a)$payload)
        ns <- asNamespace('Metafora')
        service <- get('mzHtmlD3Service',ns)$new()
        info <- service$.__enclos_env__$private$jsGetMeasureInfo('proportion','PFT',TRUE)
        expect_identical(info$transf_type,'identity')
        expect_identical(info$xlab,'Double Arcsine Proportion')
        expected <- metafor::escalc(measure='PFT',xi=d$xi,ni=d$ni)
        expect_equal(as.numeric(b$modelResults$yi),as.numeric(expected$yi))
        data <- get('mzExtractForestData',ns)(b$forestData,b$modelResults,b$options,
                                               modelType='proportion')
        expect_equal(data[['Estimate<sup>1</sup>']][seq_len(nrow(d))],as.numeric(expected$yi))
    })
})

test_that('PFT radial ignores saved alternative scales including the old inverse', {
    a <- make_analysis('mzProportions',extra=list(modelmea='PFT',showBackTransform=TRUE,
        radialPlotTransf='ipft',radialPlotShow=TRUE))
    seen <- FALSE
    local_mocked_bindings(radial=function(x,transf,...) {
        seen <<- TRUE
        expect_null(transf)
        graphics::plot.new()
    },.package='metafor')
    path <- tempfile(fileext='.pdf')
    grDevices::pdf(path)
    on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
    expect_true(a$.__enclos_env__$private$.mzRadialPlot(list(state=a$modelResults)))
    expect_true(seen)
})
