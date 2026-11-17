for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'restores Publication Bias rows without rewriting unchanged cells'), {
        a <- make_analysis(name,extra=list(distabPBA=TRUE))
        b <- make_analysis(name,extra=list(distabPBA=TRUE,shadeRows=TRUE),
            previous=snapshot(a),changes='shadeRows',run=FALSE)
        # Il ripristino deve mantenere tutta la tabella prima di richiamare .run().
        expect_identical(b$results$tabPubBias$rowKeys,a$results$tabPubBias$rowKeys)
        expect_equal(b$results$tabPubBias$asDF,a$results$tabPubBias$asDF)
        table <- b$results$tabPubBias
        writes <- 0L
        original <- table$setRow
        unlockBinding('setRow',table)
        table$setRow <- function(...) { writes <<- writes+1L; original(...) }
        lockBinding('setRow',table)
        b$run()
        expect_identical(writes,0L)
        expect_equal(table$asDF,a$results$tabPubBias$asDF)
        # Con moderatori restano soltanto i due test supportati.
        mod <- make_analysis(name,extra=list(distabPBA=TRUE,moderator=list('mod')),
            previous=snapshot(b),changes='moderator')
        expect_equal(mod$results$tabPubBias$rowCount,2L)
        back <- make_analysis(name,extra=list(distabPBA=TRUE),
            previous=snapshot(mod),changes='moderator')
        expect_equal(back$results$tabPubBias$rowCount,4L)
        expect_equal(back$results$tabPubBias$asDF,a$results$tabPubBias$asDF)
    })
})
