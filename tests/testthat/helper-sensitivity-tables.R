# Legge il payload effettivamente pubblicato, non ricostruisce i calcoli.
sens_payload <- function(output) {
    html <- output$content
    src <- sub('(?s).*?srcdoc="([^"]*)".*','\\1',html,perl=TRUE)
    for(pair in list(c('&quot;','"'),c('&#39;',"'"),c('&lt;','<'),c('&gt;','>'),c('&amp;','&')))
        src <- gsub(pair[1],pair[2],src,fixed=TRUE)
    json <- sub('(?s).*?window.plotData = (.*?);\nwindow.plotData.i18n.*','\\1',src,perl=TRUE)
    jsonlite::fromJSON(json,simplifyVector=FALSE)
}
sens_count <- function(output)length(sens_payload(output)$rows)
sens_values <- function(output,column,mode=numeric(1)) {
    rows<-sens_payload(output)$rows
    if(column=='subgroup')return(vapply(rows,`[[`,character(1),'group'))
    keys<-names(rows[[1]]$sort);key<-keys[match(tolower(column),tolower(keys))]
    vapply(rows,function(row)if(is.null(row$sort[[key]]))NA else row$sort[[key]],mode)
}
