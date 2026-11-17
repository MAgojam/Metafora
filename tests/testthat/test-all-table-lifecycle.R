table_names <- c('textRICH','tableTauSqaured','subgroupResults',
                 'heterogeneityComparison','modelFitRICH','tabPubBias')
table_notes <- function(table) lapply(table$notes,function(note)note$note)
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'restores every visible table before an aesthetic run'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,
                      subgroup='group',moderator=list('mod'))
        a <- make_analysis(name,extra=extra)
        b <- make_analysis(name,extra=c(extra,list(shadeRows=TRUE)),
            previous=snapshot(a),changes='shadeRows',run=FALSE)
        writes <- 0L
        for (key in table_names) {
            expect_identical(b$results[[key]]$rowKeys,a$results[[key]]$rowKeys,info=key)
            expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
            expect_identical(b$results[[key]]$title,a$results[[key]]$title,info=key)
            expect_identical(table_notes(b$results[[key]]),table_notes(a$results[[key]]),info=key)
            local({
                table <- b$results[[key]]
                for (method in c('setRow','addRow','deleteRows')) local({
                    methodName <- method; original <- table[[methodName]]
                    unlockBinding(methodName,table)
                    table[[methodName]] <- function(...) { writes <<- writes+1L; original(...) }
                    lockBinding(methodName,table)
                })
            })
        }
        b$run()
        expect_identical(writes,0L)
        for (key in table_names)
            expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
    })
    test_that(paste(name,'keeps inference-independent tables stable across CI and test changes'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,subgroup='group')
        a <- make_analysis(name,extra=extra)
        for (change in list(list(level=90),list(testType='knha'),list(modelest='DL'))) {
            opts <- utils::modifyList(extra,change)
            b <- make_analysis(name,extra=opts,previous=snapshot(a),
                changes=names(change),run=FALSE)
            stable <- if ('modelest' %in% names(change)) 'heterogeneityComparison' else
                c('tableTauSqaured','modelFitRICH','heterogeneityComparison')
            if ('level' %in% names(change)) stable <- c(stable,'tabPubBias')
            for (key in stable)
                expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
            b$run()
            fresh <- make_analysis(name,extra=opts)
            for (key in table_names) {
                expect_equal(b$results[[key]]$asDF,fresh$results[[key]]$asDF,info=key)
                expect_identical(table_notes(b$results[[key]]),table_notes(fresh$results[[key]]),info=key)
            }
        }
    })
    test_that(paste(name,'invalidates every selected data field and matches a fresh run'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,
            subgroup='group',moderator=list('mod'))
        a <- make_analysis(name,extra=extra)
        for (variable in unique(c('slab',unname(model_specs[[name]]$vars),'group','mod'))) {
            data <- model_data()
            data[[variable]][1] <- NA
            b <- make_analysis(name,data=data,extra=extra,previous=snapshot(a),variables=variable)
            fresh <- make_analysis(name,data=data,extra=extra)
            expect_equal(b$modelResults$k,fresh$modelResults$k)
            for (key in table_names)
                expect_equal(b$results[[key]]$asDF,fresh$results[[key]]$asDF,info=paste(variable,key))
        }
    })
    test_that(paste(name,'restores categorical moderator coefficients and changed factor levels'), {
        data <- model_data()
        data[['Study setting']] <- factor(rep(c('Urban','Rural','Mixed'),length.out=nrow(data)))
        extra <- list(moderator=list('Study setting'))
        a <- make_analysis(name,data=data,extra=extra)
        b <- make_analysis(name,data=data,extra=c(extra,list(showweights=TRUE)),
            previous=snapshot(a),changes='showweights',run=FALSE)
        expect_identical(b$results$textRICH$rowKeys,a$results$textRICH$rowKeys)
        expect_equal(b$results$textRICH$asDF,a$results$textRICH$asDF)
        b$run()
        expect_equal(b$results$textRICH$asDF,a$results$textRICH$asDF)
        data[['Study setting']] <- factor(rep(c('Urban','Rural'),length.out=nrow(data)))
        c <- make_analysis(name,data=data,extra=extra,previous=snapshot(b),variables='Study setting')
        fresh <- make_analysis(name,data=data,extra=extra)
        expect_equal(c$results$textRICH$asDF,fresh$results$textRICH$asDF)
        expect_equal(c$results$textRICH$rowCount,c$modelResults$p)
    })
    test_that(paste(name,'keeps all table content stable for graph-only options'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,subgroup='group')
        a <- make_analysis(name,extra=extra)
        for (style in list(list(showBackTransform=TRUE),list(showweights=TRUE),
                           list(xAxisTitle='Teaching example'))) {
            b <- make_analysis(name,extra=utils::modifyList(extra,style),
                previous=snapshot(a),changes=names(style),run=FALSE)
            for (key in table_names) {
                expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
                expect_identical(b$results[[key]]$title,a$results[[key]]$title,info=key)
                expect_identical(table_notes(b$results[[key]]),table_notes(a$results[[key]]),info=key)
            }
            b$run()
            for (key in table_names)
                expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
        }
    })
    test_that(paste(name,'invalidates tables when the effect measure or Generic input mode changes'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,subgroup='group')
        a <- make_analysis(name,extra=extra)
        alternatives <- c(mzDichotomous='OR',mzCorrelations='COR',mzAlphas='AHW',
            mzMeanDifferences='MD',mzProportions='PLO')
        change <- if (name=='mzEffectSizes') list(meaType='seimt') else
            list(modelmea=unname(alternatives[name]))
        opts <- utils::modifyList(extra,change)
        b <- make_analysis(name,extra=opts,previous=snapshot(a),changes=names(change))
        fresh <- make_analysis(name,extra=opts)
        expect_equal(b$modelResults$beta,fresh$modelResults$beta)
        for (key in table_names) {
            expect_equal(b$results[[key]]$asDF,fresh$results[[key]]$asDF,info=key)
            expect_identical(table_notes(b$results[[key]]),table_notes(fresh$results[[key]]),info=key)
        }
    })
})
