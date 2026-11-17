# Recupera i giudizi separatamente dai dati del modello: gli NA ROB non
# devono eliminare studi. Nomi e ordine dei domini vengono dal supplier.
.mzCollectRobDomains <- function(analysis) {
    # L'ambiente Options di jamovi genera errore leggendo opzioni assenti.
    # Il renderer comune deve funzionare anche nei modelli senza supplier ROB.
    if (!all(c('forestRobShow','robDomains') %in% names(analysis$options))) return(NULL)
    if (!isTRUE(analysis$options$forestRobShow)) return(NULL)
    variables <- analysis$options$robDomains
    if (is.null(variables) || length(variables) == 0L) return(NULL)
    jinfo("[ROB] collect", "columns =", length(variables), .loc = "mzRobService.R:10")
    data <- analysis$dataProc
    if (is.null(data) || nrow(data) == 0L) return(NULL)
    source <- analysis$data
    if (is.null(source))
        stop('Risk-of-bias ratings must be prepared while the analysis dataset is available.',call.=FALSE)
    ratings <- source[rownames(data),variables,drop=FALSE]
    classified <- MetaforServiceClass$new()$mzClassifyRobDomains(ratings)
    list(values=classified$values,names=variables)
}

# Stessa larghezza usata nel documento e nelle colonne SVG; i nomi lunghi
# vengono abbreviati solo a video, con nome completo nel tooltip e legenda.
.mzRobColumnWidth <- function(names) {
    if (length(names) == 0L) return(0L)
    as.integer(max(24L,min(64L,8L+6L*max(nchar(names,type='width')))))
}
# Spazio dedicato al ROB Classic: non riduce la regione del forest esistente.
.mzClassicRobLayout <- function(options) {
    if (!all(c('forestRobShow','robDomains') %in% names(options)) ||
        !isTRUE(options$forestRobShow) || !length(options$robDomains)) return(NULL)
    variables <- options$robDomains
    headings <- variables
    # Abbrevia solo Overall nel Classic, evitando collisioni con nomi OA esistenti.
    overall <- which(tolower(trimws(variables))=='overall')
    if (length(overall)==1L && !any(tolower(variables[-overall])=='oa'))
        headings[overall] <- 'OA'
    else overall <- integer()
    for (i in which(nchar(variables,type='width') > 9L)) {
        heading <- paste0('R',i)
        while (heading %in% headings[-i])heading <- paste0('R',heading)
        headings[i] <- heading
    }
    keyed <- headings != variables
    mappingNames <- variables
    mappingNames[overall] <- 'Overall risk of bias'
    mapping <- if (any(keyed)) paste(paste0(headings[keyed],' = ',mappingNames[keyed]),collapse='; ') else ''
    # Le parole lunghe vengono spezzate: i nomi completi restano leggibili nella legenda.
    mapping <- gsub('(.{60})','\\1 ',mapping,perl=TRUE)
    mappingLines <- if(nzchar(mapping))strwrap(mapping,width=70) else character()
    widths <- pmax(.30,(nchar(headings,type='width')*.095+.16)*.85)
    # Un piccolo distacco prima di OA la distingue dai cinque domini.
    gapsBefore <- rep(0,length(variables))
    if (length(overall) && overall>1L)gapsBefore[overall] <- .08
    list(names=variables,headings=headings,widths=widths,gapsBefore=gapsBefore,
         widthIn=max(1.1,sum(widths)+sum(gapsBefore)+.32),
         mappingLines=mappingLines,heightIn=.55+.22*length(mappingLines))
}

# Mappa l'identita' delle righe, non le etichette (che possono essere duplicate).
# Le chiavi e i tie-break sono quelli di forest.rma; tmp$rows e' gia' invertito.
.mzClassicRobRows <- function(model, orderBy=NULL, rows=NULL) {
    yi <- model$yi.f;vi <- model$vi.f
    n <- length(yi)
    index <- seq_len(n)
    old <- options(na.action='na.pass');on.exit(options(old),add=TRUE)
    if (!is.null(orderBy)) {
        if (length(orderBy)==1L && is.character(orderBy)) {
            pred <- as.numeric(stats::fitted(model))
            sortIndex <- switch(orderBy,
                rev=rev(index),est=order(yi),obs=order(yi),yi=order(yi),
                fit=order(pred),prec=order(vi,yi),vi=order(vi,yi),
                resid=order(yi-pred,yi),rstandard=order(stats::rstandard(model)$z,yi),
                abs.resid=order(abs(yi-pred),yi),
                abs.rstandard=order(abs(stats::rstandard(model)$z),yi),
                stop('Unknown Classic forest study order.',call.=FALSE))
        } else sortIndex <- order(orderBy)
        index <- index[sortIndex]
    }
    if (is.null(rows))rows <- n:1
    else if (length(rows)==1L)rows <- rows:(rows-n+1)
    if (length(rows)!=n)stop('Classic RoB row positions do not match the model.',call.=FALSE)
    index <- rev(index);rows <- rev(rows)
    keep <- !is.na(yi[index]) & !is.na(vi[index]) &
            !apply(is.na(model$X.f[index,,drop=FALSE]),1,any)
    # Metafora usa dati puliti; gestisce anche eventuali NA omessi dal renderer nativo.
    if (identical(old$na.action,'na.omit') && any(!keep)) {
        adjusted <- rows
        for (missingRow in rows[!keep])adjusted[rows>=missingRow] <- adjusted[rows>=missingRow]-1
        rows <- adjusted[keep];index <- index[keep]
    }
    sourceIndex <- if (is.null(model$subset))seq_len(n) else which(model$subset)
    list(index=sourceIndex[index],rows=rows)
}

# Risolve le stesse righe di shade di forest.rma dopo ordinamento e omissioni.
# Le coordinate restituite dal renderer nativo evitano alternanze basate sugli ID.
.mzClassicForestShading <- function(model,args,tmp) {
    shade <- args$shade
    if (is.null(shade))return(NULL)
    if (is.numeric(shade))return(list(rows=shade,col=args$colshade))
    if (is.logical(shade) && length(shade)==1L)shade <- 'zebra'
    if (is.character(shade)) {
        selected <- switch(shade[1],zebra=,zebra1=rep_len(c(TRUE,FALSE),length(tmp$rows)),
            zebra2=rep_len(c(FALSE,TRUE),length(tmp$rows)),all=rep(TRUE,length(tmp$rows)))
    } else {
        positions <- .mzClassicRobRows(model,args$order,args$rows)
        selected <- shade[positions$index]
    }
    list(rows=tmp$rows[which(selected)],col=args$colshade)
}

# Simboli e colori coerenti con forest_plot.js; nessun giudizio complessivo calcolato.
.mzDrawClassicRob <- function(rob,layout,model,forestArgs,tmp,cex,spaceTop,overallY=NULL,footer=NULL) {
    if (is.null(rob) || is.null(layout))return(NULL)
    positions <- .mzClassicRobRows(model,forestArgs$order,forestArgs$rows)
    if (length(positions$rows)!=length(tmp$rows) ||
        !isTRUE(all.equal(as.numeric(positions$rows),as.numeric(tmp$rows))))
        stop('Classic RoB rows do not match the native forest plot.',call.=FALSE)
    colors <- c(low='#1a9850',unclear='#eca038',high='#c0392b',na='#bbbbbb')
    marks <- c(low='+',unclear='?',high='-',na='')
    right <- graphics::grconvertX(graphics::par('usr')[2],'user','inches')
    centres <- right+(layout$widthIn-sum(layout$widths)-sum(layout$gapsBefore))/2+
               cumsum(layout$widths+layout$gapsBefore)-layout$widths/2
    xpos <- graphics::grconvertX(centres,'inches','user')
    headerY <- tmp$ylim[2]-(spaceTop-1)+1
    old <- graphics::par(xpd=NA);on.exit(graphics::par(old),add=TRUE)
    panelRight <- graphics::grconvertX(right+layout$widthIn-.08,'inches','user')
    # Prima gli sfondi, poi i simboli: zebratura e intestazioni proseguono senza vuoti.
    for (band in list(tmp$mzShading,tmp$mzHeaderShading))
        if (!is.null(band) && length(band$rows))
            graphics::rect(graphics::par('usr')[2],band$rows-.5,panelRight,band$rows+.5,
                           col=band$col,border=NA)
    graphics::text(mean(range(xpos)),headerY+1,'Risk of bias',font=2,cex=cex,col='#111111')
    graphics::text(xpos,headerY,layout$headings,font=2,cex=cex,col='#111111')
    graphics::segments(graphics::par('usr')[2],headerY-1,panelRight,headerY-1)
    # Completa il separatore nativo sotto gli studi, senza introdurlo con moderatori.
    if (!is.null(overallY))
        graphics::segments(graphics::par('usr')[2],0,panelRight,0)
    # Dimensioni fisiche: pch dipende anche dal cex del device, symbols no.
    drawMarks <- function(x,y,values) {
        radius <- graphics::strheight('M',units='inches',cex=cex)*.82
        graphics::symbols(x,y,circles=rep(1,length(values)),inches=radius,add=TRUE,
            bg=unname(colors[values]),fg='white',lwd=.7)
        graphics::text(x,y,unname(marks[values]),cex=cex*.95,font=2,col='white')
    }
    for (i in seq_along(rob$names)) {
        values <- rob$values[[i]][positions$index]
        values[is.na(values) | !values %in% names(colors)] <- 'na'
        drawMarks(rep(xpos[i],length(values)),positions$rows,values)
    }
    # Legenda verticale sotto Overall, nella zona sinistra libera dall'asse.
    # Con moderatori rimane sotto le note, che occupano l'intera larghezza.
    convertX <- function(x)graphics::grconvertX(x,'inches','user')
    convertY <- function(y)graphics::grconvertY(y,'inches','user')
    leading <- max(.22,graphics::strheight('Mg',units='inches',cex=cex)*1.5)
    left <- if (is.null(overallY)) .3 else
        graphics::grconvertX(min(tmp$textpos),'user','inches')+
        graphics::strwidth('M',units='inches',cex=cex)*.5
    labels <- c('Low risk','Some concerns / Unclear','High risk','Missing / Unrecognized')
    horizontal <- is.null(overallY) && !is.null(footer)
    if (horizontal) {
        # Una riga misurata subito dopo le note, mantenendo font e simboli leggibili.
        left <- footer$heterogeneity$left
        y <- footer$notes$bottom-leading*.85
        graphics::text(convertX(left),convertY(y),'Risk of bias:',adj=c(0,.5),font=2,cex=cex,col='#111111')
        cursor <- left+graphics::strwidth('Risk of bias:',units='inches',cex=cex,font=2)+.2
        legendX <- legendY <- numeric(4)
        for (i in seq_along(labels)) {
            width <- .17+graphics::strwidth(labels[i],units='inches',cex=cex*.95)+.24
            # Solo su figure ristrette manda a capo una voce intera, senza ridurre il font.
            if (cursor+width>grDevices::dev.size('in')[1]-.15) {
                cursor <- left+.08;y <- y-leading
            }
            legendX[i] <- cursor;legendY[i] <- y
            cursor <- cursor+width
        }
        y <- max(legendY)
    } else {
        y <- graphics::grconvertY(overallY,'user','inches')-leading*1.2
        graphics::text(convertX(left),convertY(y),'Risk of bias',adj=c(0,.5),font=2,cex=cex,col='#111111')
        legendX <- rep(left+.08,4)
        legendY <- y-leading*seq_len(4)
    }
    drawMarks(convertX(legendX),convertY(legendY),names(colors))
    graphics::text(convertX(legendX+.17),convertY(legendY),labels,adj=c(0,.5),cex=cex*.95,col='#111111')
    if(length(layout$mappingLines))graphics::text(convertX(left),
        convertY(min(legendY)-leading*seq_along(layout$mappingLines)),layout$mappingLines,
        adj=c(0,.5),cex=cex*.95,col='#111111')
    invisible(list(index=positions$index,rows=positions$rows,headings=layout$headings,
                   names=rob$names,centresIn=centres,plotRightIn=right,legendTopIn=y,legendRowsIn=legendY,legendXIn=legendX,legendHorizontal=horizontal,separatorY=if(is.null(overallY))NULL else 0,
                   panelRight=panelRight,shading=tmp$mzShading,headerShading=tmp$mzHeaderShading))
}
