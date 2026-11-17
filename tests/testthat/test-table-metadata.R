for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'keeps CI headers and model captions coherent'),{
        a <- make_analysis(name,extra=list(level=90))
        expect_equal(a$results$textRICH$getColumn('CILow')$superTitle,'90% CI')
        expect_match(a$results$textRICH$title,'Random-Effects Model')
        b <- make_analysis(name,extra=list(modelest='FE',level=99))
        expect_equal(b$results$textRICH$getColumn('CIHigh')$superTitle,'99% CI')
        expect_match(b$results$textRICH$title,'Fixed-Effects Model')
        c <- make_analysis(name,extra=list(moderator=list('mod')))
        expect_match(c$results$textRICH$title,'Mixed-Effects Model')
        expect_true(!is.null(c$results$textRICH$notes$mixnote))
    })
})
