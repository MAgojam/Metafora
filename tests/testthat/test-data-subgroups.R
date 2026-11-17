for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'uses one canonical preparation preserving row identities'),{
        d <- model_data();d$mod[2] <- NA;d$group[4] <- NA
        d[['Moderator with spaces']] <- cos(seq_len(nrow(d)))
        a <- make_analysis(name,data=d,extra=list(subgroup='group',moderator=list('mod','Moderator with spaces')))
        expect_identical(rownames(a$dataProc),as.character(setdiff(seq_len(nrow(d)),c(2L,4L))))
        expect_true('Moderator with spaces' %in% names(a$dataProc))
        svc <- get('MetaforServiceClass',asNamespace('Metafora'))$new()
        required <- c('slab',names(model_specs[[name]]$vars))
        prepared <- svc$mzValidateAndPrepareData(a,required)
        expect_true(prepared$success)
        expect_identical(prepared$data,a$dataProc)
        expect_equal(a$modelResults$k,11L)
    })
    test_that(paste(name,'keeps subgroup rows aligned when a singleton is skipped'),{
        d <- model_data();d$group <- factor(c('Singleton',rep(c('between_groups','Z'),6L)),
                                          levels=c('Singleton','Z','between_groups'))
        a <- make_analysis(name,data=d,extra=list(subgroup='group'))
        fits <- a$subgroupResults
        expect_false('Singleton' %in% names(fits))
        for (level in c('Z','between_groups')) {
            selected <- d[d$group==level,]
            expected <- do.call(metafor::rma,native_args(name,selected))
            expect_equal(a$results$subgroupResults$getCell('Estimate',rowKey=level)$value,
                         as.numeric(expected$beta),tolerance=1e-10)
        }
        key <- list('mz_between_groups')
        expect_equal(sum(vapply(a$results$subgroupResults$rowKeys,identical,logical(1),key)),1L)
        b <- make_analysis(name,data=d,extra=list(subgroup='group',shadeRows=TRUE),
                           previous=snapshot(a),changes='shadeRows')
        expect_equal(a$results$subgroupResults$asDF,b$results$subgroupResults$asDF)
    })
    test_that(paste(name,'preserves categorical moderators'),{
        d <- model_data();d$category <- factor(rep(c('High','Low'),length.out=nrow(d)))
        a <- make_analysis(name,data=d,extra=list(moderator=list('category')))
        expected <- do.call(metafor::rma,native_args(name,d,extra=list(mods=~ category,data=d)))
        expect_true(is.factor(a$dataProc$category))
        expect_equal(unname(a$modelResults$beta),unname(expected$beta),tolerance=1e-10)
    })
})

test_that('missing Risk of Bias values do not remove studies',{
    d <- model_data();d$risk <- factor(rep(c('Low','High'),length.out=nrow(d)));d$risk[3] <- NA
    a <- make_analysis('mzDichotomous',data=d,extra=list(robDomains='risk'))
    expect_equal(a$modelResults$k,nrow(d))
    expect_identical(rownames(a$dataProc),rownames(d))
})
