# Il logging non deve valutare dati o promesse quando è disattivato.
test_that('the logging manager does not instrument its own functions', {
    ns <- asNamespace('Metafora')
    managers <- c('jinfo','mark','open_log','close_log','set_logflags',
                  'render_ui_html','.mzEnsureLogPath','.mzLogFileStatus',
                  '.mzLogLocation','.mzLogErrorText')
    calls <- unlist(lapply(managers,function(name) all.names(body(get(name,ns)))))
    expect_false(any(c('jinfo','mark') %in% calls))
})

test_that('fit error diagnostics exclude the serialized R call', {
    error <- structure('PRIVATE RAW CALL', class='try-error',
        condition=simpleError('Numerical convergence failure',call=quote(fit(PRIVATE_DATA))))
    expect_identical(get('.mzLogErrorText',asNamespace('Metafora'))(error),
                     'Numerical convergence failure')
})

test_that('release logging stays lazy when disabled', {
    ns <- asNamespace('Metafora')
    flags <- list2env(as.list(get('logFlags',ns)),parent=emptyenv())
    flags$log_active <- flags$j_INFO <- flags$j_DEBUG <- FALSE
    local_mocked_bindings(logFlags=flags,.package='Metafora')
    expect_null(get('jinfo',ns)(stop('must not evaluate')))
    expect_null(get('mark',ns)(stop('must not evaluate')))
})

test_that('service logs share the run UI and carry escaped source references', {
    ns <- asNamespace('Metafora')
    flags <- list2env(as.list(get('logFlags',ns)),parent=emptyenv())
    flags$log_active <- flags$j_INFO <- flags$j_DEBUG <- FALSE
    flags$ui_log_obj <- NULL
    flags$fleWUD <- tempfile(fileext='.log')
    on.exit(unlink(flags$fleWUD),add=TRUE)
    local_mocked_bindings(logFlags=flags,.package='Metafora')
    html <- NULL
    target <- list(setContent=function(value) html <<- value)
    self <- list(options=list(jlogMode='posRorB',tabbedControlModelType='effectsize',
        modelest='REML',modelmea='GEN',level=95,moderator=NULL),results=list(debugLog=target))
    finish <- get('.mzBeginAnalysisLog',ns)(self)
    get('jinfo',ns)('[CACHE/FIT] hit',.loc='service.R:42')
    get('mark',ns)('[ERR] failed','<invalid> & controlled',.loc='service.R:43')
    expect_match(html,'[service.R][L42]',fixed=TRUE)
    expect_match(html,'[service.R][L43]',fixed=TRUE)
    expect_match(html,'&lt;invalid&gt; &amp; controlled',fixed=TRUE)
    expect_false(grepl('<invalid>',html,fixed=TRUE))
    finish()
    expect_null(flags$ui_log_obj)
    previous <- html
    get('jinfo',ns)('[OUTSIDE] no active run')
    expect_identical(html,previous)
    get('set_logflags',ns)('posOff')
    file <- paste(readLines(flags$fleWUD,warn=FALSE),collapse='\n')
    expect_match(file,'[service.R][L42]',fixed=TRUE)
    expect_match(file,'[service.R][L43]',fixed=TRUE)
})

test_that('all six models keep identical estimates with File plus UI logging', {
    ns <- asNamespace('Metafora')
    flags <- list2env(as.list(get('logFlags',ns)),parent=emptyenv())
    flags$log_active <- flags$j_INFO <- flags$j_DEBUG <- FALSE
    flags$ui_log_obj <- NULL
    flags$fleWUD <- tempfile(fileext='.log')
    on.exit(unlink(flags$fleWUD),add=TRUE)
    local_mocked_bindings(logFlags=flags,.package='Metafora')
    for (name in names(model_specs)) {
        off <- make_analysis(name)
        on <- make_analysis(name,extra=list(jlogMode='posRorB'))
        expect_identical(on$modelResults$beta,off$modelResults$beta,info=name)
        expect_identical(on$modelResults$ci.lb,off$modelResults$ci.lb,info=name)
        expect_match(on$results$debugLog$content,'[CACHE/FIT]',fixed=TRUE)
        expect_match(on$results$debugLog$content,'[L',fixed=TRUE)
        expect_null(flags$ui_log_obj)
    }
})

test_that('model cache hit remains lazy with diagnostics enabled', {
    ns <- asNamespace('Metafora')
    flags <- list2env(as.list(get('logFlags',ns)),parent=emptyenv())
    flags$log_active <- flags$j_INFO <- flags$j_DEBUG <- TRUE
    flags$log_to_file <- flags$log_to_ui <- FALSE
    flags$ui_log_obj <- NULL
    local_mocked_bindings(logFlags=flags,.package='Metafora')
    expect_identical(get('mzGetCachedModel',ns)(list(state=list(saved=TRUE)),
        stop('cache hit must not evaluate the fit')),list(saved=TRUE))
})
