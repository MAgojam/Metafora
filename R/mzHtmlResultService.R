# Identita e pubblicazione comuni per report e tabelle HTML senza iframe.
# Non cambia il contenuto statistico; gli asset rimangono separati dal body
# per il parser di esportazione strutturata di jamovi.
.mzHtmlScope <- function(self=NULL) {
    if (is.null(self$results$d3SessionScope)) return(NULL)
    mzHtmlD3Service$new()$forestTableAnalysisScope(self)
}

.mzReadHtmlHelper <- function() {
    path <- system.file('extdata','metaforahtml.min.js',package='Metafora')
    if (!nzchar(path)) stop('Missing HTML interaction helper')
    paste(readLines(path,encoding='UTF-8',warn=FALSE),collapse='\n')
}

.mzHtmlDocument <- function(self, kind, content) {
    scope <- .mzHtmlScope(self)
    if (is.null(scope)) return(content)
    key <- paste0('mz-html-',kind,'-',scope)
    keyJson <- jsonlite::toJSON(key,auto_unbox=TRUE)
    keyJson <- gsub('</','<\\/',keyJson,fixed=TRUE)
    styles <- regmatches(content,gregexpr('(?s)<style>.*?</style>',content,perl=TRUE))[[1]]
    body <- gsub('(?s)<style>.*?</style>','',content,perl=TRUE)
    helper <- tryCatch(.mzReadHtmlHelper(),error=function(e) {
        mark('[HTML] helper unavailable',conditionMessage(e), .loc = "mzHtmlResultService.R:24")
        ''
    })
    self <- .mzTranslationSelf(self)
    if (!nzchar(helper)) body <- paste0('<p>',
        .('View state is unavailable; the results remain readable.'),'</p>',body)
    scripts <- if(nzchar(helper))paste0('<script>',helper,'</script><script>MetaforaHtml.mount(',keyJson,');</script>')else ''
    paste0('<!DOCTYPE html><html><head><meta charset="UTF-8">',paste(styles,collapse=''),scripts,
        '</head><body><div data-mz-html-key="',.mzForestHeaderEscape(key),'">',body,'</div></body></html>')
}

.mzSetHtmlResult <- function(self, output, kind, content) {
    html <- .mzHtmlDocument(self,kind,content)
    if (!output$isFilled() || !identical(output$content,html)) output$setContent(html)
    invisible(html)
}
