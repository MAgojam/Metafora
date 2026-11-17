# I giudizi ROB sono annotazioni: non selezionano casi e non modificano il fit.
rob_models <- c('mzDichotomous','mzMeanDifferences','mzEffectSizes')
expect_forest_document <- function(a,name) {
    output <- a$results$outD3ForestPlot
    if (inherits(output,'Svg')) {
        # Il pilota nativo incorpora le risorse e serializza direttamente il markup.
        expect_match(output$content,'MetaforaD3.mountNative(',fixed=TRUE)
        expect_length(output$scripts,0L)
        expect_identical(output$asProtoBuf(status=3L)$svg$content,output$content)
    } else {
        expect_true(inherits(output,'Html'))
        expect_match(output$content,'<iframe srcdoc=',fixed=TRUE)
    }
    expect_match(output$content,'data-mz-export-token=',fixed=TRUE)
    expect_true(inherits(a$results$outD3ForestPlotExport,'Image'))
}
for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'renders Forest with real options and no ROB selection'), {
        a <- make_analysis(name,extra=list(forestPlotMode='posRorB'))
        expect_null(get('.mzCollectRobDomains',asNamespace('Metafora'))(a))
        expect_forest_document(a,name)
        expect_false(grepl('Forest Plot error:',a$results$outD3ForestPlot$content,fixed=TRUE))
        b <- make_analysis(name,extra=list(forestPlotMode='posRorB',shadeRows=TRUE),
            previous=snapshot(a),changes='shadeRows')
        expect_forest_document(b,name)
        expect_equal(b$modelResults$beta,a$modelResults$beta)
        grouped <- make_analysis(name,extra=list(forestPlotMode='posRorB',subgroup='group'))
        expect_forest_document(grouped,name)
        expect_false(grepl('Forest Plot error:',grouped$results$outD3ForestPlot$content,fixed=TRUE))
    })
})
for (model_name in rob_models) local({
    name <- model_name
    test_that(paste(name,'aligns arbitrary ROB domains without changing model or tables'), {
        data <- model_data()
        data[['Selection bias']] <- factor(rep(c('Low','Some concerns','High'),length.out=nrow(data)))
        data[['Overall rating']] <- factor(rep(c('High','Low'),length.out=nrow(data)))
        data[['Selection bias']][3] <- NA
        base <- list(distabPBA=TRUE,showModelFit=TRUE,showHEC=TRUE,subgroup='group')
        a <- make_analysis(name,data=data,extra=base)
        extra <- utils::modifyList(base,list(robDomains=c('Overall rating','Selection bias'),
            forestRobShow=TRUE,forestPlotMode='posRorB',forestOrder='obs'))
        b <- make_analysis(name,data=data,extra=extra,previous=snapshot(a),
            changes=c('robDomains','forestRobShow','forestPlotMode','forestOrder'),run=FALSE)
        keys <- c('textRICH','tableTauSqaured','subgroupResults','heterogeneityComparison','modelFitRICH','tabPubBias')
        writes <- 0L
        for (key in keys) {
            expect_equal(b$results[[key]]$asDF,a$results[[key]]$asDF)
            local({
                table <- b$results[[key]]
                for (method in c('setRow','addRow','deleteRows')) local({
                    methodName <- method;original <- table[[methodName]]
                    unlockBinding(methodName,table)
                    table[[methodName]] <- function(...) {writes <<- writes+1L;original(...)}
                    lockBinding(methodName,table)
                })
            })
        }
        b$run()
        expect_identical(writes,0L)
        expect_equal(b$modelResults$k,nrow(data))
        expect_equal(b$modelResults$beta,a$modelResults$beta)
        collect <- get('.mzCollectRobDomains',asNamespace('Metafora'))
        ratings <- collect(b)
        expect_identical(ratings$names,extra$robDomains)
        expect_identical(ratings$values[['Selection bias']][3],'na')
        expect_identical(ratings$values[['Selection bias']][1:2],c('low','unclear'))
        expect_length(ratings$values[['Overall rating']],nrow(data))
        html <- b$results$outD3ForestPlot$content
        expect_match(html,'Selection bias',fixed=TRUE)
        expect_match(html,'Overall rating',fixed=TRUE)
        expect_false(grepl('Forest Plot error:',html,fixed=TRUE))
        service <- get('mzHtmlD3Service',asNamespace('Metafora'))$new()
        private <- service$.__enclos_env__$private
        mi <- private$jsGetMeasureInfo(b$options$tabbedControlModelType,b$options$modelmea,FALSE)
        payload <- jsonlite::fromJSON(private$d3BuildForestJson(b$modelResults,mi,qnorm(.975),
            order(b$modelResults$yi),FALSE,b$dataProc$subgroup,
            rob_domains=ratings$values,rob_domain_names=ratings$names,
            subgroupResults=b$subgroupResults),simplifyVector=FALSE)
        expect_identical(unlist(payload$rob_domain_names),extra$robDomains)
        for (study in payload$studies) {
            row <- match(study$label,b$dataProc$slab)
            expect_false(is.na(row))
            expect_identical(unlist(study$rob),unname(vapply(ratings$values,function(values)values[row],character(1))))
        }
        hidden <- make_analysis(name,data=data,extra=utils::modifyList(extra,list(forestRobShow=FALSE)))
        expect_null(collect(hidden))
    })
})

test_that('shared ROB extraction aligns retained studies and supports more than six domains', {
    collect <- get('.mzCollectRobDomains',asNamespace('Metafora'))
    ratings <- data.frame(row.names=c('rowA','rowB','rowC'))
    for (i in seq_len(8)) ratings[[paste('Domain',i)]] <- factor(c('Low',NA,'High'))
    ratings[['Domain 8']][1] <- 'Low'
    analysis <- list(options=list(forestRobShow=TRUE,robDomains=rev(names(ratings))),
        data=ratings,dataProc=data.frame(yi=c(1,2),row.names=c('rowC','rowA')))
    result <- collect(analysis)
    expect_identical(result$names,rev(names(ratings)))
    expect_length(result$values,8L)
    expect_identical(result$values[[1]],c('high','low'))
    analysis$options$robDomains <- character()
    expect_null(collect(analysis))
})
