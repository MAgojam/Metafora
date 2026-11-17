global_tables <- c('textRICH','tableTauSqaured','heterogeneityComparison','modelFitRICH','tabPubBias')
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'preserves global tables when subgroup selection keeps the same studies'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE)
        a <- make_analysis(name,extra=extra)
        b <- make_analysis(name,extra=c(extra,list(subgroup='group')),
            previous=snapshot(a),changes='subgroup',run=FALSE)
        for (key in global_tables) {
            expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
            expect_identical(b$results[[key]]$title,a$results[[key]]$title,info=key)
        }
        writes <- 0L
        for (key in global_tables) local({
            table <- b$results[[key]]
            for (method in c('setRow','addRow','deleteRows')) local({
                methodName <- method; original <- table[[methodName]]
                unlockBinding(methodName,table)
                table[[methodName]] <- function(...) { writes <<- writes+1L; original(...) }
                lockBinding(methodName,table)
            })
        })
        b$run()
        expect_identical(writes,0L)
        c <- make_analysis(name,extra=extra,previous=snapshot(b),changes='subgroup',run=FALSE)
        for (key in global_tables)
            expect_equal(c$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
        c$run()
    })
    test_that(paste(name,'recomputes global tables when subgroup missingness changes included studies'), {
        data <- model_data();data$group[1] <- NA
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE)
        a <- make_analysis(name,data=data,extra=extra)
        b <- make_analysis(name,data=data,extra=c(extra,list(subgroup='group')),
            previous=snapshot(a),changes='subgroup')
        fresh <- make_analysis(name,data=data,extra=c(extra,list(subgroup='group')))
        expect_equal(a$modelResults$k,13L)
        expect_equal(b$modelResults$k,12L)
        for (key in c(global_tables,'subgroupResults'))
            expect_equal(b$results[[key]]$asDF,fresh$results[[key]]$asDF,info=key)
        back <- make_analysis(name,data=data,extra=extra,previous=snapshot(b),changes='subgroup')
        expect_equal(back$modelResults$k,13L)
        for (key in global_tables)
            expect_equal(back$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
    })
    test_that(paste(name,'preserves global fits after subgroup relabeling but invalidates statistical changes'), {
        extra <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,subgroup='group')
        a <- make_analysis(name,extra=extra)
        data <- model_data(); levels(data$group) <- c('Group one','Group two')
        b <- make_analysis(name,data=data,extra=extra,previous=snapshot(a),variables='group',run=FALSE)
        for (key in global_tables)
            expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF,info=key)
        b$run()
        expect_true(all(c('Group one','Group two') %in% b$results$subgroupResults$asDF$level))
        # Un cambio simultaneo di stimatore non deve essere nascosto dall'impronta dei dati.
        changed <- utils::modifyList(extra,list(subgroup=NULL,modelest='DL'))
        c <- make_analysis(name,data=data,extra=changed,previous=snapshot(b),changes=c('subgroup','modelest'))
        fresh <- make_analysis(name,data=data,extra=changed)
        for (key in global_tables)
            expect_equal(c$results[[key]]$asDF,fresh$results[[key]]$asDF,info=key)
    })
})
