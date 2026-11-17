# Più errori nello stesso ciclo devono rimanere visibili anche dopo cache/ripristino.
test_that('publication bias preserves all notices and replays the combined notice from cache', {
    fail <- function(...) stop('<invalid> & controlled')
    local_mocked_bindings(fsn=fail,ranktest=fail,.package='metafor')
    local_mocked_bindings(mzRegtest=fail,mzTrimFill=fail,.package='Metafora')
    a <- make_analysis('mzDichotomous',extra=list(distabPBA=TRUE))
    notice <- a$results$get('infoMsg')
    expect_s3_class(notice,'Notice')
    for (text in c('fail-safe N','regression test','rank correlation','trim and fill'))
        expect_match(notice$content,text,fixed=TRUE)
    expect_match(notice$content,'&lt;invalid&gt; &amp; controlled',fixed=TRUE)
    expect_false(grepl('<invalid>',notice$content,fixed=TRUE))
    expect_match(notice$content,'white-space: pre-line',fixed=TRUE)
    b <- make_analysis('mzDichotomous',extra=list(distabPBA=TRUE,shadeRows=TRUE),
                       previous=snapshot(a),changes='shadeRows')
    expect_identical(b$results$get('infoMsg')$content,notice$content)
    expect_equal(length(b$results$tabPubBias$state$notices),4)
})

test_that('logging is opt-in, portable and records only the supplied run metadata', {
    ns <- asNamespace('Metafora')
    flags <- list2env(as.list(get('logFlags',ns)),parent=emptyenv())
    flags$log_active <- FALSE
    flags$j_INFO <- FALSE
    flags$j_DEBUG <- FALSE
    flags$fleWUD <- tempfile('metafora-log-test-',fileext='.log')
    local_mocked_bindings(logFlags=flags,.package='Metafora')
    logFile <- flags$fleWUD
    on.exit(unlink(logFile),add=TRUE)
    begin <- get('.mzBeginAnalysisLog',ns)
    self <- list(options=list(jlogMode='posOff',tabbedControlModelType='effectsize',
        modelest='REML',modelmea='GEN',level=95,moderator=list('PRIVATE_VARIABLE')))
    expect_null(begin(self))
    expect_false(file.exists(flags$fleWUD))
    self$options$jlogMode <- 'posLorT'
    end <- begin(self)
    expect_true(is.function(end))
    end()
    get('set_logflags',ns)('posOff')
    output <- paste(readLines(flags$fleWUD,warn=FALSE),collapse='\n')
    expect_match(output,'Environment: R',fixed=TRUE)
    expect_match(output,'Metafora=2.0.0',fixed=TRUE)
    expect_match(output,'ANALYSIS BEGIN',fixed=TRUE)
    expect_match(output,'ANALYSIS END',fixed=TRUE)
    expect_match(output,'moderators = 1',fixed=TRUE)
    expect_false(grepl('PRIVATE_VARIABLE',output,fixed=TRUE))
    expect_false(flags$log_active)
    expect_identical(normalizePath(dirname(get('logFlags',ns)$fleWUD),winslash='/'),
                     normalizePath(tempdir(),winslash='/'))
    # L'inizializzazione runtime deve usare il tempdir corrente senza consumare RNG.
    flags$fleWUD <- NULL
    seed <- if(exists('.Random.seed',.GlobalEnv))get('.Random.seed',.GlobalEnv) else NULL
    path <- get('.mzEnsureLogPath',ns)()
    expect_identical(normalizePath(dirname(path),winslash='/'),
                     normalizePath(tempdir(),winslash='/'))
    expect_identical(get('.mzEnsureLogPath',ns)(),path)
    after <- if(exists('.Random.seed',.GlobalEnv))get('.Random.seed',.GlobalEnv) else NULL
    expect_identical(after,seed)
})
