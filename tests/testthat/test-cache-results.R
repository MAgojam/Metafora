for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'caches visible Publication Bias and preserves its notes'),{
        calls <- 0L;original <- metafor::fsn
        local_mocked_bindings(fsn=function(...){calls<<-calls+1L;original(...)},.package='metafor')
        a <- make_analysis(name,extra=list(distabPBA=TRUE))
        expect_equal(calls,1L)
        b <- make_analysis(name,extra=list(distabPBA=TRUE,shadeRows=TRUE),
                           previous=snapshot(a),changes='shadeRows')
        expect_equal(calls,1L)
        expect_equal(b$results$tabPubBias$asDF,a$results$tabPubBias$asDF)
        expect_identical(b$results$tabPubBias$state,a$results$tabPubBias$state)
        expect_equal(b$results$tabPubBias$notes$fsnNoteTable$note,a$results$tabPubBias$notes$fsnNoteTable$note)
        c <- make_analysis(name,extra=list(distabPBA=TRUE,fsntype='Orwin'),
                           previous=snapshot(b),changes='fsntype')
        expect_equal(calls,2L)
        expect_match(c$results$tabPubBias$notes$fsnNoteTable$note,'Orwin')
    })
    test_that(paste(name,'reuses subgroup fits and invalidates changed data'),{
        calls <- 0L;original <- metafor::rma
        local_mocked_bindings(rma=function(...){calls<<-calls+1L;original(...)},.package='metafor')
        a <- make_analysis(name,extra=list(subgroup='group'))
        initial <- calls;expect_gt(initial,1L)
        b <- make_analysis(name,extra=list(subgroup='group',shadeRows=TRUE),
                           previous=snapshot(a),changes='shadeRows')
        expect_equal(calls,initial)
        expect_identical(b$results$subgroupResults$state,a$results$subgroupResults$state)
        d <- model_data();d$group[1] <- NA
        c <- make_analysis(name,data=d,extra=list(subgroup='group'),previous=snapshot(b),variables='group')
        expect_gt(calls,initial)
        fresh <- make_analysis(name,data=d,extra=list(subgroup='group'))
        expect_equal(c$results$subgroupResults$asDF,fresh$results$subgroupResults$asDF)
    })
})
