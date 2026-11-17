# L'etichetta dell'intercetta è per l'utente; il nome interno rma serve ai calcoli.
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'uses a readable intercept label with and without moderators'), {
        for (moderated in c(FALSE,TRUE)) {
            a <- make_analysis(name,extra=if(moderated)list(moderator='mod')else list())
            expect_identical(a$results$textRICH$asDF$Intercept[1L],'Intercept')
            expect_identical(rownames(a$modelResults$b)[1L],'intrcpt')
        }
    })
})
