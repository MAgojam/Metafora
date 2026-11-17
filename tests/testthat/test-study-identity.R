# Nomi duplicati e righe escluse: l'identita deve seguire lo studio reale,
# anche quando metafor normalizza slab e i renderer usano ordini diversi.
for (model in names(model_specs)) local({
    name <- model
    test_that(paste(name, 'links duplicate study labels by source row through sorting and subgroups'), {
        f <- function(n) get(n,asNamespace('Metafora'))
        d <- model_data();d$slab[1:3] <- 'Same name'
        input <- unname(model_specs[[name]]$vars)[1]
        d[[input]][5] <- NA_real_
        for (grouped in c(FALSE,TRUE)) {
            a <- make_analysis(name,data=d,extra=c(list(forestTableShow=TRUE,forestOrder='obs'),
                if(grouped)list(subgroup='group')else list()))
            fit <- a$modelResults
            ids <- f('.mzD3StudyIds')(fit)
            expect_identical(ids,paste0('study-row:',setdiff(seq_len(nrow(d)),5)))
            expect_equal(anyDuplicated(ids),0L)
            table <- f('mzBuildForestTable')(a)$payload
            rows <- Filter(function(row)!row$summary,table$rows)
            expect_setequal(vapply(rows,`[[`,character(1),'id'),ids)
            for (row in rows) {
                source <- as.integer(sub('study-row:','',row$id,fixed=TRUE))
                expect_identical(row$label,d$slab[source])
            }
            service <- f('mzHtmlD3Service')$new();p <- service$.__enclos_env__$private
            info <- p$jsGetMeasureInfo(a$options$tabbedControlModelType,a$options$modelmea,FALSE)
            forest <- jsonlite::fromJSON(p$d3BuildForestJson(fit,info,qnorm(.975),
                rev(seq_along(ids)),FALSE,if(grouped)a$dataProc$subgroup else NULL))$studies
            funnel <- jsonlite::fromJSON(p$d3BuildFunnelJson(fit,info),simplifyVector=FALSE)$studies
            expect_setequal(forest$id,ids)
            expect_identical(vapply(funnel,`[[`,character(1),'id'),ids)
            expect_equal(forest$es,round(as.numeric(fit$yi)[match(forest$id,ids)],4))
            for (i in seq_along(funnel)) expect_equal(funnel[[i]]$es,as.numeric(fit$yi)[i])
            loo <- a$leave1outResults
            lo <- jsonlite::fromJSON(p$d3BuildLeave1OutJson(fit,loo,info,95,'estimate'))$studies
            expect_setequal(lo$id,ids)
            looIds <- if('study_id' %in% names(loo))loo$study_id else ids
            expect_equal(lo$es,round(loo$estimate[match(lo$id,looIds)],4))
            expect_equal(lo$label,as.character(fit$slab[match(lo$id,ids)]))
        }
    })
})

test_that('source row IDs survive native subsetting without matching study labels', {
    f <- get('.mzD3StudyIds',asNamespace('Metafora'))
    d <- data.frame(yi=c(.2,.1,.4,.3),vi=rep(.1,4),row.names=c('rA','rB','rC','rD'))
    fit <- metafor::rma(yi=yi,vi=vi,data=d,subset=c(1,3,4),slab=rep('Repeated',4))
    expect_identical(f(fit),c('study-row:rA','study-row:rC','study-row:rD'))
})
