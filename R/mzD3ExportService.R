# Direct jamovi exports use native Image dimensions; D3 remains the live view.
.mzD3ExportKinds <- c(forest='outD3ForestPlot',funnel='outD3FunnelPlot',
                     leave1out='outD3Leave1OutPlot',cumulative='outD3CumulativePlot')
.mzD3ExportOption <- function(kind) paste0('d3Export',switch(kind,
    forest='Forest',funnel='Funnel',leave1out='Leave1Out',cumulative='Cumulative'))

.mzRegisterD3Export <- function(self,kind,doc) {
    cache <- self$results$d3ExportMeta
    if (is.null(cache)) return(NULL)
    stopifnot(kind %in% names(.mzD3ExportKinds))
    token <- digest::digest(doc,algo='sha256',serialize=FALSE)
    state <- cache$state
    if (!is.list(state)) state <- list()
    state[[kind]] <- token
    cache$setState(state)
    list(token=token,option=.mzD3ExportOption(kind),
         image=paste0(.mzD3ExportKinds[[kind]],'Export'))
}

.mzReadD3Snapshot <- function(value,expected) {
    if (!is.character(value) || length(value)!=1L || !nzchar(value) ||
        nchar(value,type='bytes')>12000000L || !is.character(expected)) return(NULL)
    tryCatch({
        data <- jsonlite::fromJSON(value,simplifyVector=FALSE)
        if (!identical(data$token,expected) || !is.character(data$png) ||
            length(data$png)!=1L || nchar(data$png)>11000000L) return(NULL)
        bytes <- jsonlite::base64_dec(data$png)
        if (length(bytes)<24L || length(bytes)>8000000L ||
            !identical(as.integer(bytes[1:8]),c(137L,80L,78L,71L,13L,10L,26L,10L)) ||
            !identical(rawToChar(bytes[13:16]),'IHDR')) return(NULL)
        number <- function(i) sum(as.integer(bytes[i+(0:3)])*256^(3:0))
        w<-number(17L);h<-number(21L)
        if (w<1 || h<1 || w>10000 || h>10000 ||
            !identical(as.numeric(data$width),w) || !identical(as.numeric(data$height),h)) return(NULL)
        list(bytes=bytes,width=w,height=h,
             hash=digest::digest(bytes,algo='sha256',serialize=FALSE))
    },error=function(e) NULL)
}

.mzApplyD3Exports <- function(self) {
    state <- self$results$d3ExportMeta$state
    for (kind in names(.mzD3ExportKinds)) {
        name <- .mzD3ExportKinds[[kind]]
        live <- self$results[[name]];image <- self$results[[paste0(name,'Export')]]
        if (is.null(image)) next
        # Svg conserva lo snapshot nativo: una vecchia cattura Html non va riattivata.
        if (inherits(live,'Svg')) {
            image$setVisible(FALSE)
            next
        }
        expected <- if (is.list(state)) state[[kind]] else NULL
        snap <- .mzReadD3Snapshot(self$options[[.mzD3ExportOption(kind)]],expected)
        ready <- !is.null(live) && isTRUE(live$visible) && !is.null(snap)
        image$setVisible(ready)
        if (!ready) next
        image$setSize(snap$width,snap$height)
        # Pixels stay in the transport option and native image resource,
        # not duplicated in serialized calculation/result state.
        image$setState(list(option=.mzD3ExportOption(kind),token=expected,hash=snap$hash))
    }
    invisible(NULL)
}

.mzRenderD3Export <- function(self,image) {
    state<-image$state
    if (!is.list(state)) return(FALSE)
    snap<-.mzReadD3Snapshot(self$options[[state$option]],state$token)
    if (is.null(snap) || !identical(snap$hash,state$hash)) return(FALSE)
    pixels<-png::readPNG(snap$bytes)
    grid::grid.raster(pixels,width=grid::unit(1,'npc'),height=grid::unit(1,'npc'),interpolate=FALSE)
    TRUE
}
