# La codifica numerica di una categoria non deve cambiare la meta-regressione.
test_that('moderator conversion preserves measure types and numeric factor codes', {
    prepare <- get('.mzPrepareModerator',asNamespace('Metafora'))
    nominal <- structure(c(1,2,1,3,NA_real_),'jmv-measure'='nominal')
    ordinal <- structure(nominal,'jmv-measure'='ordinal')
    expect_true(is.factor(prepare(nominal)))
    expect_true(is.factor(prepare(ordinal)))
    for (measure in c('nominal','ordinal')) for (values in list(c(1L,2L,3L),c(.5,1.5,2.5),c('A','B','C'))) {
        attr(values,'jmv-measure') <- measure
        expect_true(is.factor(prepare(values)))
    }
    expect_equal(as.character(prepare(nominal)),c('1','2','1','3',NA_character_))
    coded <- factor(c('random','alternate','random','systematic'))
    attr(coded,'values') <- c(2,1,3)
    expect_true(jmvcore::canBeNumeric(coded))
    expect_identical(prepare(coded),coded)
    attr(coded,'jmv-measure') <- 'continuous'
    expect_equal(prepare(coded),c(1,2,1,3))
    expect_identical(prepare(c(2,4,6)),c(2,4,6))
})

for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'fits coded nominal moderators as factors'), {
        data <- model_data()
        data$group <- factor(rep(c('Alpha','Beta','Gamma'),length.out=nrow(data)))
        data$group_n <- structure(as.numeric(data$group),'jmv-measure'='nominal')
        data$mod <- structure(data$mod,'jmv-measure'='continuous')
        text <- make_analysis(name,data=data,extra=list(moderator=list('group')))
        coded <- make_analysis(name,data=data,extra=list(moderator=list('group_n')))
        expect_true(is.factor(coded$dataProc$group_n))
        expect_equal(as.vector(coded$modelResults$X),as.vector(stats::model.matrix(~group_n,data=coded$dataProc)))
        expect_length(coded$modelResults$beta,3L)
        expect_equal(unname(coded$modelResults$beta),unname(text$modelResults$beta))
        expect_equal(unname(coded$modelResults$pval),unname(text$modelResults$pval))
        expect_equal(unname(coded$modelResults$ci.lb),unname(text$modelResults$ci.lb))
        expect_equal(unname(coded$modelResults$ci.ub),unname(text$modelResults$ci.ub))
        expect_equal(coded$modelResults$QE,text$modelResults$QE)
        data$group_values <- data$group
        attr(data$group_values,'values') <- c(1,2,3)
        native <- make_analysis(name,data=data,extra=list(moderator=list('group_values')))
        expect_true(is.factor(native$dataProc$group_values))
        expect_equal(unname(native$modelResults$beta),unname(text$modelResults$beta))
        mixed <- make_analysis(name,data=data,extra=list(moderator=list('mod','group_n')))
        expect_true(is.numeric(mixed$dataProc$mod))
        expect_true(is.factor(mixed$dataProc$group_n))
        expect_equal(as.vector(mixed$modelResults$X),as.vector(stats::model.matrix(~mod+group_n,data=mixed$dataProc)))
        expect_length(mixed$modelResults$beta,4L)
    })
})
