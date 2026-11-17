# mzMetaforService.R

#' --- Funzione libera ---
#' Restituisce la trasformazione inversa (back-transform) canonica per una
#' data misura di effetto. Unica fonte di verita' per Tabella Forest (Classic
#' Html), Funnel Plot (Classic Image) e Forest Plot (Classic Image) nei sei
#' modelli. Sostituisce .getTransform (mzHtmlService.R) e
#' .getMeasureTransformInfo (ex-metodo privato di MetaforServiceClass), che
#' duplicavano la stessa mappa in modo divergente.
#'
#' NOTA D3: per i grafici interattivi (mzHtmlD3Service.R) atransf qui
#' restituito NON viene applicato lato R - il client D3 applica il
#' back-transform via metaforad3.min.js::applyTransf(v, transf_type), che
#' supporta i tag "exp","tanh","plogis","arcsin_sq","iprobit","iahw","iabt".
#' La misura PFT resta sulla scala trasformata in tutti gli output, anche
#' quando un'opzione salvata richiede il back-transform. La sua inversa richiede
#' un riferimento campionario che questa versione non sceglie implicitamente.
#'
#' @param modelType Tipo di modello ("correlation","alpha","dichotomous",
#'   "meandiff","proportion","effectsize" - da tabbedControlModelType)
#' @param measure Codice misura (modelmea)
#' @param showBackTransform TRUE/FALSE (showBackTransform)
#' @param ni Argomento mantenuto per il contratto dei chiamanti; non usato
#'   dalle trasformazioni supportate in questa versione.
#' @return list(atransf, targs, transf_type, xlab, xlab_short, null_value)
.mzGetBackTransform <- function(modelType, measure, showBackTransform = FALSE, ni = NULL) {
    
    bt <- isTRUE(showBackTransform)
    
    out <- list(
        atransf     = NULL,
        targs       = NULL,
        transf_type = "identity",
        xlab        = measure,
        xlab_short  = measure,
        null_value  = 0
    )
    
    if (modelType == "dichotomous") {
        out <- switch(measure,
                      "OR" = list(atransf = if (bt) exp else NULL, targs = NULL,
                                  transf_type = if (bt) "exp" else "identity",
                                  xlab = if (bt) "Odds Ratio" else "Log Odds Ratio",
                                  xlab_short = if (bt) "OR" else "Log OR",
                                  null_value = if (bt) 1 else 0),
                      "RR" = list(atransf = if (bt) exp else NULL, targs = NULL,
                                  transf_type = if (bt) "exp" else "identity",
                                  xlab = if (bt) "Risk Ratio" else "Log Risk Ratio",
                                  xlab_short = if (bt) "RR" else "Log RR",
                                  null_value = if (bt) 1 else 0),
                      "PETO" = list(atransf = if (bt) exp else NULL, targs = NULL,
                                    transf_type = if (bt) "exp" else "identity",
                                    xlab = if (bt) "Odds Ratio (Peto)" else "Log Odds Ratio (Peto)",
                                    xlab_short = if (bt) "OR (Peto)" else "Log OR (Peto)",
                                    null_value = if (bt) 1 else 0),
                      "RD" = list(atransf = NULL, targs = NULL,
                                  transf_type = "identity",
                                  xlab = "Risk Difference", xlab_short = "RD",
                                  null_value = 0),
                      # Una differenza di archi non ha un'inversa unica in Risk Difference.
                      # Ignora anche il flag salvato da versioni precedenti del modulo.
                      "AS" = list(atransf = NULL, targs = NULL,
                                  transf_type = "identity",
                                  xlab = "Arcsine Difference",
                                  xlab_short = "AS",
                                  null_value = 0),
                      out
        )
        
    } else if (modelType == "correlation") {
        out <- switch(measure,
                      "COR"  = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                    xlab = "Correlation (r)", xlab_short = "r", null_value = 0),
                      "UCOR" = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                    xlab = "Correlation (r, unbiased)", xlab_short = "r (unb.)", null_value = 0),
                      "ZCOR" = list(atransf = if (bt) metafor::transf.ztor else NULL, targs = NULL,
                                    transf_type = if (bt) "tanh" else "identity",
                                    xlab = if (bt) "Correlation (r)" else "Fisher's z",
                                    xlab_short = if (bt) "r" else "z",
                                    null_value = 0),
                      out
        )
        
    } else if (modelType == "meandiff") {
        out <- switch(measure,
                      "MD"    = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                     xlab = "Mean Difference", xlab_short = "MD", null_value = 0),
                      "SMD"   = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                     xlab = "Standardized Mean Difference", xlab_short = "SMD", null_value = 0),
                      "SMDH"  = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                     xlab = "SMD (Heteroscedastic)", xlab_short = "SMDH", null_value = 0),
                      "SMD1"  = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                     xlab = "Standardized Mean Difference (SD1)", xlab_short = "SMD1", null_value = 0),
                      "SMD1H" = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                     xlab = "SMD (SD1, Heteroscedastic)", xlab_short = "SMD1H", null_value = 0),
                      "ROM"   = list(atransf = if (bt) exp else NULL, targs = NULL,
                                     transf_type = if (bt) "exp" else "identity",
                                     xlab = if (bt) "Ratio of Means" else "Log Ratio of Means",
                                     xlab_short = if (bt) "ROM" else "Log ROM",
                                     null_value = if (bt) 1 else 0),
                      out
        )
        
    } else if (modelType == "proportion") {
        out <- switch(measure,
                      "PR"  = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                   xlab = "Proportion", xlab_short = "Prop.", null_value = 0),
                      "PLN" = list(atransf = if (bt) exp else NULL, targs = NULL,
                                   transf_type = if (bt) "exp" else "identity",
                                   xlab = if (bt) "Proportion" else "Log Proportion",
                                   xlab_short = if (bt) "Prop." else "Log P",
                                   null_value = 0),
                      "PLO" = list(atransf = if (bt) metafor::transf.ilogit else NULL, targs = NULL,
                                   transf_type = if (bt) "plogis" else "identity",
                                   xlab = if (bt) "Proportion" else "Log Odds",
                                   xlab_short = if (bt) "Prop." else "Log OR",
                                   null_value = 0),
                      "PRZ" = list(atransf = if (bt) metafor::transf.iprobit else NULL, targs = NULL,
                                   transf_type = if (bt) "iprobit" else "identity",
                                   xlab = if (bt) "Proportion" else "Probit Proportion",
                                   xlab_short = if (bt) "Prop." else "Probit P",
                                   null_value = 0),
                      "PAS" = list(atransf = if (bt) metafor::transf.iarcsin else NULL, targs = NULL,
                                   transf_type = if (bt) "arcsin_sq" else "identity",
                                   xlab = if (bt) "Proportion" else "Arcsine Proportion",
                                   xlab_short = if (bt) "Prop." else "AS(P)",
                                   null_value = 0),
                      "PFT" = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                   xlab = "Double Arcsine Proportion", xlab_short = "FT(P)",
                                   null_value = 0),
                      out
        )
        
    } else if (modelType == "alpha") {
        out <- switch(measure,
                      "ARAW" = list(atransf = NULL, targs = NULL, transf_type = "identity",
                                    xlab = "Cronbach's Alpha", xlab_short = "Alpha", null_value = 0),
                      "AHW"  = list(atransf = if (bt) metafor::transf.iahw else NULL, targs = NULL,
                                    transf_type = if (bt) "iahw" else "identity",
                                    xlab = if (bt) "Cronbach's Alpha" else "Log Alpha (HW)",
                                    xlab_short = if (bt) "Alpha" else "Log A(HW)",
                                    null_value = 0),
                      "ABT"  = list(atransf = if (bt) metafor::transf.iabt else NULL, targs = NULL,
                                    transf_type = if (bt) "iabt" else "identity",
                                    xlab = if (bt) "Cronbach's Alpha" else "Log Alpha (BT)",
                                    xlab_short = if (bt) "Alpha" else "Log A(BT)",
                                    null_value = 0),
                      out
        )
        
    } else if (modelType == "effectsize") {
        out <- list(atransf = NULL, targs = NULL, transf_type = "identity",
                    xlab = "Effect Size", xlab_short = "ES", null_value = 0)
    }
    
    out
}


# Adatta le varianti del modulo senza alterare i nomi dei metodi nativi di metafor.
mzFitRma <- function(...) {
    args <- list(...)
    if (!identical(args$method, "SJ2")) {
        result <- do.call(metafor::rma, args)
        # do.call inserisce la funzione completa nella call: basta il riferimento
        # qualificato equivalente, conservando tutti gli argomenti e l'uso di update().
        result$call[[1L]] <- quote(metafor::rma)
        return(result)
    }

    # SJ2 usa lo stesso avvio da HE già previsto nella tabella HEC.
    # Conserva misura, selezione studi, moderatori, test e livello del chiamante.
    initialArgs <- args
    initialArgs$method <- "HE"
    initialArgs$control <- NULL
    # I refit interni possono chiedere un output ridotto privo di tau2.
    initialArgs$outlist <- NULL
    initial <- do.call(metafor::rma, initialArgs)
    args$method <- "SJ"
    if (is.null(args$control)) args$control <- list()
    args$control$tau2.init <- initial$tau2
    result <- do.call(metafor::rma, args)
    result$call[[1L]] <- quote(metafor::rma)
    # method resta SJ per i metodi S3 nativi; la variante serve alle label del modulo.
    attr(result, "mz_estimator") <- "SJ2"
    result
}

mzEstimatorId <- function(result) {
    requested <- attr(result, "mz_estimator", exact = TRUE)
    if (is.null(requested)) result$method else requested
}

# I derivati nativi riutilizzano control$tau2.init del fit completo. Per SJ2
# ogni nuovo sottoinsieme richiede invece il proprio avvio HE. La copia locale
# del metodo conserva l'algoritmo metafor e ne adatta soltanto i refit rma.uni;
# nessuna funzione nel namespace della libreria viene sostituita.
mzSJ2RefitMethod <- function(method) {
    if (!grepl("rma.uni", paste(deparse(body(method)), collapse="\n"), fixed=TRUE)) {
        stop("SJ2 refit adapter is incompatible with this metafor version.")
    }
    localMethod <- method
    localEnv <- new.env(parent=environment(method))
    localEnv$rma.uni <- function(...) {
        args <- list(...)
        args$method <- "SJ2"
        do.call(mzFitRma, args)
    }
    environment(localMethod) <- localEnv
    localMethod
}

mzRefitSJ2 <- function(method, result, ...) {
    mzSJ2RefitMethod(method)(result, ...)
}

mzTrimFill <- function(result, ...) {
    if (!identical(mzEstimatorId(result), "SJ2")) return(metafor::trimfill(result, ...))
    method <- utils::getS3method("trimfill", "rma.uni", envir=asNamespace("metafor"))
    mzRefitSJ2(method, result, ...)
}

mzLeave1Out <- function(result, ...) {
    if (!identical(mzEstimatorId(result), "SJ2")) return(metafor::leave1out(result, ...))
    method <- utils::getS3method("leave1out", "rma.uni", envir=asNamespace("metafor"))
    mzRefitSJ2(method, result, ...)
}

mzInfluence <- function(result, ...) {
    if (!identical(mzEstimatorId(result), "SJ2")) return(stats::influence(result, ...))
    method <- utils::getS3method("influence", "rma.uni", envir=asNamespace("metafor"))
    mzRefitSJ2(method, result, ...)
}

mzBaujat <- function(result, ...) {
    if (!identical(mzEstimatorId(result), "SJ2")) return(metafor::baujat(result, ...))
    method <- utils::getS3method("baujat", "rma", envir=asNamespace("metafor"))
    mzRefitSJ2(method, result, ...)
}

mzRegtest <- function(result, ...) {
    if (!identical(mzEstimatorId(result), "SJ2")) return(metafor::regtest(result, ...))
    mzRefitSJ2(metafor::regtest, result, ...)
}

mzGosh <- function(result, ...) {
    if (!identical(mzEstimatorId(result), "SJ2")) return(metafor::gosh(result, ...))
    method <- utils::getS3method("gosh", "rma", envir=asNamespace("metafor"))
    # GOSH delega i refit a un worker separato, adattato nello stesso ambiente locale.
    if (!grepl(".profile.rma.uni", paste(deparse(body(method)), collapse="\n"), fixed=TRUE)) {
        stop("SJ2 GOSH adapter is incompatible with this metafor version.")
    }
    localEnv <- new.env(parent=environment(method))
    localEnv$.profile.rma.uni <- mzSJ2RefitMethod(get(".profile.rma.uni", asNamespace("metafor")))
    environment(method) <- localEnv
    method(result, ...)
}

# Evita scritture delle celle ripristinate senza saltare titoli, note o righe nuove.
mzSetTableRowIfChanged <- function(table, values, rowNo=NA, rowKey=NULL) {
    position <- rowNo
    if (is.na(position)) {
        jinfo("[TABLE] add row", "rows =", table$rowCount, .loc = "mzMetaforService.R:263")
        matches <- which(vapply(table$rowKeys, identical, logical(1), rowKey))
        if (length(matches) > 0L) position <- matches[1L]
    }
    if (is.na(position)) {
        return(table$addRow(rowKey=rowKey,values=values))
    }
    if (position > table$rowCount) {
        jinfo("[TABLE] extend", "row =", position, .loc = "mzMetaforService.R:271")
        return(table$setRow(values=values, rowNo=rowNo, rowKey=rowKey))
    }
    values <- values[intersect(names(values), names(table$columns))]
    same <- vapply(names(values), function(name) {
        old <- table$getCell(col=name, rowNo=position)$value
        value <- values[[name]]
        # Le celle calcolate possono contenere espressioni invece di valori atomici.
        if ((!is.null(old) && !is.atomic(old)) ||
            (!is.null(value) && !is.atomic(value))) return(FALSE)
        missingCell <- function(x) is.null(x) ||
            (length(x) == 1L && is.na(x) && !is.nan(x))
        if (missingCell(old) && missingCell(value)) return(TRUE)
        identical(unname(is.nan(old)), unname(is.nan(value))) &&
            isTRUE(all.equal(old, value, tolerance=0, check.attributes=FALSE))
    }, logical(1))
    if (!all(same)) {
        jinfo("[TABLE] update", "row =", position, .loc = "mzMetaforService.R:288")
        table$setRow(values=values, rowNo=position)
    } else {
        jinfo("[TABLE] reuse", "row =", position, .loc = "mzMetaforService.R:291")
    }
    invisible(NULL)
}

# Completa il ripristino nativo delle righe determinate dal risultato del modello.
# La decisione sulla validità resta a jmvcore: senza stato ripristinato non si
# recuperano righe precedenti. L'adattatore riguarda soltanto l'istanza corrente,
# non modifica classi/prototipi. Per le tabelle globali il cambio di subgroup
# è escluso soltanto se l'impronta dei dati statistici dimostra l'equivalenza.
mzEnableDynamicTableRestore <- function(table, validState=NULL, self=NULL, currentSignature=NULL) {
    if (isTRUE(attr(table$fromProtoBuf,'mzDynamicTableRestore'))) return(invisible(NULL))
    nativeRestore <- table$fromProtoBuf
    restore <- function(element,oChanges,vChanges) {
        if (!is.null(currentSignature)) {
            previous <- self$results$globalInputState$state
            groupVars <- unique(c(unlist(self$options$subgroup),
                if (is.list(previous)) previous$subgroup))
            groupChanged <- 'subgroup' %in% oChanges || any(vChanges %in% groupVars)
            if (groupChanged && is.list(previous) && identical(previous$version,1L) &&
                !is.null(previous$signature) && identical(previous$signature,currentSignature())) {
                oChanges <- setdiff(oChanges,'subgroup')
                vChanges <- setdiff(vChanges,groupVars)
            }
        }
        nativeRestore(element,oChanges,vChanges)
        if (is.null(validState) || !isTRUE(validState(table$state))) return(invisible(NULL))
        added <- FALSE
        for (name in element$table$rowNames) {
            key <- jsonlite::fromJSON(name,simplifyVector=FALSE)
            if (!any(vapply(table$rowKeys,identical,logical(1),key))) {
                table$addRow(rowKey=key)
                added <- TRUE
            }
        }
        # Ora anche le celle delle righe dinamiche possono essere ripristinate.
        if (added) nativeRestore(element,oChanges,vChanges)
        invisible(NULL)
    }
    attr(restore,'mzDynamicTableRestore') <- TRUE
    unlockBinding('fromProtoBuf',table)
    table$fromProtoBuf <- restore
    lockBinding('fromProtoBuf',table)
    invisible(NULL)
}

mzInitTableLifecycle <- function(self) {
    # Il digest viene calcolato una sola volta per i cinque ripristini globali.
    computed <- FALSE; signature <- NULL
    currentSignature <- function() {
        if (!computed) { signature <<- mzGlobalInputSignature(self,preload=TRUE); computed <<- TRUE }
        signature
    }
    mzEnableDynamicTableRestore(self$results$textRICH,function(state) inherits(state,'rma'),self,currentSignature)
    for (name in c('tableTauSqaured','heterogeneityComparison','modelFitRICH','tabPubBias'))
        mzEnableDynamicTableRestore(self$results[[name]],self=self,currentSignature=currentSignature)
    mzEnableDynamicTableRestore(self$results$subgroupResults,function(state)
        is.list(state) && identical(state$mzSubgroupVersion,1L))
}

# L'alias subgroup non entra nel modello globale, ma la selezione dei casi sì.
# Mantiene un eventuale moderatore realmente chiamato subgroup. Rownames e
# attributi dei fattori fanno parte dell'impronta; non si conservano i dati grezzi.
mzGlobalInputSignature <- function(self,preload=FALSE) {
    if (!self$ready) return(NULL)
    data <- if (preload) self$signatureDataProc else self$dataProc
    if (is.null(data)) return(NULL)
    columns <- names(data)
    if (!'subgroup' %in% unlist(self$options$moderator)) columns <- setdiff(columns,'subgroup')
    # Esplicita i rownames: la rappresentazione compatta interna di R può
    # cambiare dopo un subset pur rappresentando esattamente gli stessi casi.
    input <- list(rows=rownames(data),columns=as.list(data[,columns,drop=FALSE]))
    digest::digest(input,algo='sha256',serialize=TRUE,serializeVersion=2L)
}

# Titoli, note e intestazioni devono essere pronti prima della pubblicazione
# dei risultati ripristinati: le celle uguali da sole non garantiscono stabilità.
mzSaveGlobalInputSignature <- function(self) {
    self$results$globalInputState$setState(list(version=1L,
        signature=mzGlobalInputSignature(self),subgroup=unlist(self$options$subgroup)))
    invisible(NULL)
}

mzRestoreTableMetadata <- function(self) {
    fit <- self$results$textRICH$state
    if (inherits(fit,'rma')) mzUpdateModelTableMetadata(self,self$results$textRICH,fit)
    title <- jmvcore::format(.("{ciWidth}% CI"),ciWidth=self$options$level)
    for (name in c('CILow','CIHigh'))
        self$results$subgroupResults$getColumn(name)$setSuperTitle(title)
    state <- self$results$tabPubBias$state
    if (is.list(state) && identical(state$mzPBAVersion,1L))
        mzUpdatePublicationBiasMetadata(self$results$tabPubBias,self$options)
    invisible(NULL)
}

mzUpdatePublicationBiasMetadata <- function(table,options) {
    self <- table$analysis
    table$setTitle(.("Publication Bias Assessment"))
    table$setNote('fsnNoteTable',jmvcore::format(
        .("Fail-safe N Calculation Using the {method} Approach"),method=options$fsntype),init=FALSE)
    table$setNote('rctfNoteTable',if (length(options$moderator)>0L)
        .("Rank Correlation and Trim and Fill are not supported for models with moderators.")
        else NULL,init=FALSE)
    invisible(NULL)
}

# Conserva i fit dei sottogruppi usando le dipendenze già dichiarate dalla tabella.
mzGetCachedSubgroups <- function(table, value) {
    cached <- table$state
    if (is.list(cached) && identical(cached$mzSubgroupVersion, 1L)) {
        jinfo("[CACHE/SG] hit", .loc = "mzMetaforService.R:401")
        for (message in cached$warnings) warning(message, call.=FALSE)
        return(cached$results)
    }
    warnings <- character()
    jinfo("[CACHE/SG] miss", .loc = "mzMetaforService.R:406")
    result <- withCallingHandlers(value, warning=function(w) {
        warnings <<- c(warnings, conditionMessage(w))
    })
    table$setState(list(mzSubgroupVersion=1L, results=result, warnings=warnings))
    result
}

# Popola i sottogruppi per chiave: i gruppi saltati non spostano le righe successive.
mzPopulateSubgroupTable <- function(self) {
    if (is.null(self$options$subgroup) || length(self$options$subgroup) == 0L) return(invisible(NULL))
    results <- self$subgroupResults
    table <- self$results$subgroupResults
    if (is.null(table) || length(results) == 0L) return(invisible(NULL))
    table$setVisible(TRUE)
    ciTitle <- jmvcore::format(.("{ciWidth}% CI"), ciWidth=self$options$level)
    for (name in c('CILow','CIHigh')) table$getColumn(name)$setSuperTitle(ciTitle)
    for (level in setdiff(names(results),'metareg')) {
        fit <- results[[level]]
        q <- paste0(formatC(fit$QE,digits=2,format='f'),' (',
                    if (fit$QEp < .001) 'p < 0.001' else paste0('p = ',formatC(fit$QEp,digits=3,format='f')),')')
        mzSetTableRowIfChanged(table,rowKey=level,values=list(
            k=fit$k,Estimate=fit$b[1],CILow=fit$ci.lb[1],CIHigh=fit$ci.ub[1],
            p=fit$pval[1],I2=paste0(formatC(fit$I2,digits=1,format='f'),'%'),QTest=q))
    }
    if ('metareg' %in% names(results)) {
        fit <- results$metareg
        q <- paste0(formatC(fit$QM,digits=2,format='f'),' (',
                    if (fit$QMp < .001) 'p < 0.001' else paste0('p = ',formatC(fit$QMp,digits=3,format='f')),')')
        # La chiave composta non collide con un livello chiamato Between Groups.
        key <- list('mz_between_groups')
        if (!any(vapply(table$rowKeys,identical,logical(1),key))) table$addRow(rowKey=key)
        mzSetTableRowIfChanged(table,rowKey=key,values=list(level=.("Between Groups"),QTest=q))
    }
    invisible(NULL)
}

# Metadati comuni: sono aggiornati anche quando le celle statistiche sono riusate.
mzUpdateModelTableMetadata <- function(self, table, fit) {
    ci <- jmvcore::format(.("{ciWidth}% CI"),ciWidth=self$options$level)
    for (name in c('CILow','CIHigh')) table$getColumn(name)$setSuperTitle(ci)
    hasMods <- length(self$options$moderator) > 0L
    fixed <- self$options$modelest == 'FE'
    model <- if (hasMods) .("Mixed-Effects Model") else if (fixed) .("Fixed-Effects Model") else .("Random-Effects Model")
    table$setTitle(jmvcore::format(.("{model} (k={k})"),model=model,k=fit$k))
    table$setNote('mixnote',NULL)
    table$setNote('rannote',NULL)
    table$setNote('test',NULL)
    if (!fixed) {
        estimators <- c(DL='DerSimonian-Laird',HE='Hedges',HS='Hunter-Schmidt',
                        SJ='Sidik-Jonkman',SJ2='SJ (Alternative Init.)',ML='Maximum-Likelihood',
                        REML='Restricted Maximum-Likelihood',EB='Empirical Bayes',PM='Paule-Mandel')
        name <- unname(estimators[self$options$modelest])
        if (is.na(name)) name <- .("Not Applicable")
        note <- jmvcore::format(.("Tau\u00b2 Estimator: {estimator}"),estimator=name)
        table$setNote(if (hasMods) 'mixnote' else 'rannote',note,init=FALSE)
    }
    note <- if (fixed) .("Test statistics: z-test (Fixed-Effects)") else
        jmvcore::format(.("Test statistics: {test}-test"),test=fit$test)
    table$setNote('test',note,init=FALSE)
    if (identical(self$options$tabbedControlModelType,'effectsize'))
        mzEffectSizeInputNote(self, table)
    invisible(NULL)
}

#' @title MetaforServiceClass
#' @description Servizio R6 per le operazioni condivise con metafor
#' @importFrom R6 R6Class
#' @export
MetaforServiceClass <- R6::R6Class(
    "MetaforServiceClass",
    private = list(
        .noticeObj = NULL,
        .toastQueue = list(),
        
        #' Difesa contro breakout dal tag <script>: la sequenza "</script"
        #' (case-insensitive) chiude il tag lato parser HTML indipendentemente
        #' dal contesto stringa JS in cui compare - jsonlite::toJSON() non la
        #' tratta come carattere speciale JSON e quindi non la escapa. Divide
        #' e rigiunge inserendo un backslash prima dello slash: innocuo per il
        #' valore della stringa JS (invariato), ma la sequenza HTML letterale
        #' "</script" non compare piu' nel markup.
        .mzEscapeScriptClose = function(x) {
            x <- as.character(x)
            parts <- strsplit(x, "(?i)</script", perl = TRUE)[[1]]
            if (length(parts) <= 1) return(x)
            paste(parts, collapse = "<\\/script")
        },
        
        .createNotice = function(jmv_self, name, type, visible) {
            private$.noticeObj <- jmvcore::Notice$new(
                options = jmv_self$options,
                name = name,
                type = type,
                visible = visible)
            
            jmv_self$results$insert(1, private$.noticeObj)
            
            private$.noticeObj
        },
        
        .mzGetNoticeSvg = function(type = NULL) {
            
            if (is.null(type))
                return("")
            
            switch(as.character(type),
                   # -------------------------------------------------
                   # Spinner: pallina che gira sulla circonferenza
                   # -------------------------------------------------
                   "spinner" = paste0(
                       "<svg xmlns='http://www.w3.org/2000/svg' ",
                       "width='18' height='18' viewBox='0 0 18 18' ",
                       "style='vertical-align:-3px; margin-left:6px;'>",
                       
                       "<circle cx='9' cy='9' r='6.5' ",
                       "fill='none' stroke='currentColor' ",
                       "stroke-width='1.5' opacity='0.25'/>",
                       
                       "<circle cx='9' cy='2.5' r='2' ",
                       "fill='currentColor'>",
                       
                       "<animateTransform ",
                       "attributeName='transform' ",
                       "type='rotate' ",
                       "from='0 9 9' ",
                       "to='360 9 9' ",
                       "dur='1s' ",
                       "repeatCount='indefinite'/>",
                       
                       "</circle>",
                       "</svg>"
                   ),
                   # -------------------------------------------------
                   # Segno di conferma: effetto disegnato e leggermente elastico
                   # -------------------------------------------------
                   "check" = paste0(
                       "<svg xmlns='http://www.w3.org/2000/svg' ",
                       "width='18' height='18' viewBox='0 0 18 18' ",
                       "style='vertical-align:-3px; margin-left:6px;'>",
                       
                       # Quadrato
                       "<rect x='2.25' y='2.25' width='13.5' height='13.5' ",
                       "rx='3' fill='none' stroke='currentColor' ",
                       "stroke-width='1.4' opacity='0.35'>",
                       
                       # Leggera pulsazione del quadrato
                       "<animateTransform ",
                       "attributeName='transform' ",
                       "type='scale' ",
                       "values='1;1.06;1' ",
                       "dur='1.4s' ",
                       "repeatCount='indefinite' ",
                       "additive='sum' ",
                       "origin='9 9'/>",
                       
                       "</rect>",
                       
                       # Segno di conferma
                       "<path d='M5 9.2 L7.7 11.8 L13.2 6.3' ",
                       "fill='none' ",
                       "stroke='currentColor' ",
                       "stroke-width='1.8' ",
                       "stroke-linecap='round' ",
                       "stroke-linejoin='round' ",
                       "stroke-dasharray='12' ",
                       "stroke-dashoffset='12'>",
                       
                       # Disegno del check
                       "<animate ",
                       "attributeName='stroke-dashoffset' ",
                       "values='12;0;0;12' ",
                       "keyTimes='0;0.28;0.72;1' ",
                       "dur='1.4s' ",
                       "repeatCount='indefinite'/>",
                       
                       # Piccolo effetto elastico
                       "<animateTransform ",
                       "attributeName='transform' ",
                       "type='scale' ",
                       "values='0.8;1.12;0.96;1;0.8' ",
                       "keyTimes='0;0.28;0.4;0.72;1' ",
                       "dur='1.4s' ",
                       "repeatCount='indefinite' ",
                       "additive='sum' ",
                       "origin='9 9'/>",
                       
                       "</path>",
                       "</svg>"
                   ),
                   # -------------------------------------------------
                   # Nessun SVG
                   # -------------------------------------------------
                   "none" = "",
                   ""
            )
        }
    ),
    
    ### --- FUNZIONI PUBBLICHE RICHIAMATE DAL MODULO --- ###
    
    public = list(
        #' @description Inizializza una nuova istanza di MetaforServiceClass
        initialize = function() {
            # Nessuna inizializzazione richiesta
        },
        
        #' @description Formatta i nomi dei coefficienti per i moderatori categorici
        #' @param res Oggetto modello rma
        #' @param moderators Lista dei nomi dei moderatori
        #' @param data Dataset utilizzato per il modello
        #' @return Vettore con i nomi dei coefficienti formattati
        mzFormatCoefNames = function(res, moderators, data) {
            coef_names <- rownames(res$b)
            if (is.null(moderators) || length(moderators) == 0 || length(coef_names) <= 1 || is.null(data)) {
                return(coef_names)
            }
            
            new_names <- coef_names
            for (i in 2:length(coef_names)) {
                name <- coef_names[i]
                for (mod in moderators) {
                    # Se il moderatore è un fattore nei dati
                    if (mod %in% names(data) && is.factor(data[[mod]])) {
                        # Controlla se il nome del coefficiente inizia con il nome del moderatore
                        if (startsWith(name, mod)) {
                            level <- substring(name, nchar(mod) + 1)
                            # Se rimane qualcosa dopo il nome del moderatore, assumiamo sia il livello
                            if (nchar(level) > 0) {
                                new_names[i] <- paste0(mod, " [", level, "]")
                                break
                            }
                        }
                    }
                }
            }
            # Restituisce i nomi formattati
            return(new_names)
        },
        
        #' @description Normalizza i livelli factor arbitrari di uno o più domini
        #'   Risk of Bias in categorie canoniche {"low","unclear","high","na"}
        #'   tramite matching euristico case-insensitive. Unica fonte di verita':
        #'   il payload verso D3 contiene SOLO le categorie canoniche, mai il
        #'   testo raw del livello utente.
        #' @param robData data.frame con una colonna per dominio (allineato per
        #'   rowname a dataProc dal chiamante - il RoB non deve mai passare per
        #'   na.omit()/.cleanData(), va recuperato separatamente in .run()).
        #' @return list(values = lista di vettori categoria canonica, uno per
        #'   dominio, stesso ordine/lunghezza delle righe di robData;
        #'   na_count = numero totale di valori non classificati)
        mzClassifyRobDomains = function(robData) {

            if (is.null(robData) || ncol(robData) == 0 || nrow(robData) == 0) {
                return(list(values = list(), na_count = 0L))
            }
            
            classifyLevel <- function(x) {
                if (is.na(x)) return("na")
                # Normalizza case e spazi multipli ("Low Risk", "LOW  RISK",
                # " low ") prima del match - stesso risultato per ogni variante.
                s <- tolower(trimws(as.character(x)))
                s <- gsub("\\s+", " ", s)
                if (!nzchar(s)) return("na")
                
                low_pat     <- "^(low|l|yes|\\+|1|low risk( of bias)?)$"
                high_pat    <- "^(high|h|no|-|3|high risk( of bias)?)$"
                unclear_pat <- "^(unclear|some concerns?|moderate|u|\\?|2|unclear risk( of bias)?)$"
                
                if (grepl(low_pat, s))     return("low")
                if (grepl(high_pat, s))    return("high")
                if (grepl(unclear_pat, s)) return("unclear")
                "na"
            }
            
            naCount <- 0L
            values <- lapply(names(robData), function(colName) {
                cats <- vapply(robData[[colName]], classifyLevel, character(1))
                naCount <<- naCount + sum(cats == "na")
                unname(cats)
            })
            names(values) <- names(robData)
            
            list(values = values, na_count = naCount)
        },
        
        #' @description Mostra una notifica Toast orizzontale nell'interfaccia utente        #' @param htmlItem Oggetto HTML dei risultati di jamovi (es. self$results$toastTrigger)
        #' @param message Testo del messaggio (supporta un sottoinsieme di tag HTML,
        #'   sanificato via whitelist lato client - vedi mzToast.js::sanitizeToastHtml)
        #' @param type Tipo di notifica: "info", "success", "warning" o "error". Default: "info"
        #' @param duration Durata della notifica a schermo espressa in millisecondi. 
        #'   Se 0, attiva la modalità 'Sticky' (permanente fino all'azione dell'utente). Default: 4000
        #' @param position Posizione: "TL","TC","TR","CL","CC","CR","BL","BC","BR". Default: "CC"
        #' @param theme Tema visivo: "glass" (default), "dark", o "light".
        #' @param title Titolo opzionale mostrato sopra il messaggio (NULL = nessun titolo)
        #' @return Nessun valore restituito
        mzTriggerToast = function(htmlItem,
                                  message,
                                  type = "info",
                                  duration = 4000,
                                  position = "CC",
                                  theme = "glass",
                                  title = NULL) {
            
            if (is.null(htmlItem)) return(invisible(NULL))
            
            # --- Validazione difensiva di tutti i parametri prima della
            # serializzazione: NA/NULL/vettori/valori fuori whitelist non
            # devono mai propagarsi in un errore R fatale che interrompe .run() ---
            safe_message <- if (is.null(message) || length(message) == 0 || is.na(message[1])) {
                ""
            } else {
                gsub("\r\n|\n|\r", "<br>", as.character(message[1]))
            }
            
            safe_title <- if (is.null(title) || length(title) == 0 || is.na(title[1])) {
                ""
            } else {
                gsub("\r\n|\n|\r", "<br>", as.character(title[1]))
            }
            
            valid_types <- c("info", "success", "warning", "error")
            safe_type <- if (length(type) == 1 && !is.na(type) && type %in% valid_types) type else "info"
            
            safe_duration <- suppressWarnings(as.integer(duration[1]))
            # CORRETTO: 0 è ora ammesso per la modalità Sticky; viene corretto a 4000 solo se negativo o non valido
            if (length(safe_duration) == 0 || is.na(safe_duration) || safe_duration < 0) safe_duration <- 4000L
            
            valid_positions <- c("TL","TC","TR","CL","CC","CR","BL","BC","BR")
            safe_position <- if (length(position) == 1 && !is.na(position) && position %in% valid_positions) position else "CC"
            
            valid_themes <- c("auto", "glass", "dark", "light")
            safe_theme <- if (length(theme) == 1 && !is.na(theme) && theme %in% valid_themes) theme else "glass"
            
            # Accodiamo i messaggi generati durante questo .run()
            private$.toastQueue <- append(private$.toastQueue, list(list(
                message  = safe_message,
                title    = safe_title,
                type     = safe_type,
                duration = safe_duration,
                position = safe_position,
                theme    = safe_theme
            )))
            
            payload_json <- jsonlite::toJSON(
                list(
                    action   = "triggerToast",
                    toasts   = private$.toastQueue
                ),
                auto_unbox = TRUE
            )
            payload_json <- private$.mzEscapeScriptClose(payload_json)
            
            script <- paste0(
                "<script>",
                "(function() {",
                "  var payload = ", payload_json, ";",
                "  if (window.parent) {",
                "    window.parent.postMessage(payload, '*');",
                "    if (window.parent.frames) {",
                "      for (var i = 0; i < window.parent.frames.length; i++) {",
                "        try { window.parent.frames[i].postMessage(payload, '*'); } catch(e) {}",
                "      }",
                "    }",
                "  }",
                "})();",
                "</script>"
            )
            
            # Sequenza obbligatoria
            htmlItem$setVisible(TRUE)
            htmlItem$setContent(script)
            # Usa un hash del payload come stato: jamovi ri-renderizza
            # l'iframe solo se lo stato cambia rispetto al ciclo precedente.
            htmlItem$setState(digest::digest(payload_json, algo = "xxhash32"))
        },
        
        #' @description Svuota la coda interna dei toast accumulati durante il .run() precedente.
        #'   Va chiamato all'inizio di ogni .run() nei backend che usano mzTriggerToast(),
        #'   per evitare che i toast delle esecuzioni precedenti vengano ritrasmessi.
        mzResetToastQueue = function() {
            private$.toastQueue <- list()
        },
        
        #' @description Imposta un avviso globale di analisi
        #' @param jmv_self Serve sia per options che per results dal backend
        #' @param message Il messaggio da visualizzare
        #' @param name Il nome dell'avviso
        #' @param type Il tipo di avviso (predefinito: jmvcore::NoticeType$INFO)
        #' @param title Titolo opzionale per l'avviso
        #' @param visible Indica se visualizzare il badge con il messaggio
        mzSetNotice = function(jmv_self, 
                               message, 
                               name = 'infoMsg', 
                               type = jmvcore::NoticeType$INFO, 
                               title = NULL,
                               visible = TRUE,
                               svg = "none") {
            
            # Assegna palette colori in base al tipo di Notice
            if (type == 3) {        # INFO (3)
                bg_col <- "#f0f7fb"
                br_col <- "#d3e6f3"
                tx_col <- "#2a6b8e"
            } else if (type == 2) { # WARNING (2)
                bg_col <- "#fffbe6"
                br_col <- "#ffeda3"
                tx_col <- "#5c4a0e"
            } else if (type == 1) { # STRONG_WARNING (1)
                bg_col <- "#fff4e6"
                br_col <- "#ffd8a8"
                tx_col <- "#663a0a"
            } else {                # ERROR (0 o fallback)
                bg_col <- "#fdf3f2"
                br_col <- "#fad2cf"
                tx_col <- "#661b17"
            }
            
            # Per aggiungere una svg di servizio
            svg_content <- private$.mzGetNoticeSvg(svg)
            
            # Escaping difensivo: title e message possono contenere caratteri
            # speciali HTML se derivano da nomi variabile utente o messaggi R.
            safe_title   <- if (!is.null(title)) htmlSpecialChars(as.character(title)) else NULL
            safe_message <- htmlSpecialChars(as.character(message))
            
            # Costruiamo un HTML con sfondo colorato e angoli smussati (migliorabile)
            html_content <- paste0(
                "<div style='font-family: system-ui, -apple-system, sans-serif; ",
                "background-color: ", bg_col, "; ",
                "border: 1px solid ", br_col, "; ",
                "border-radius: 6px; ",
                "padding: 10px 14px; ",
                "color: ", tx_col, "; ",
                "margin-top: 2px; margin-bottom: 2px;'>",
                if (!is.null(safe_title))
                    paste0(
                        "<strong style='font-size: 1.1em; display: block; ",
                        "margin-bottom: 6px; opacity: 0.9;'>",
                        safe_title,
                        svg_content,
                        "</strong>"
                    )
                else "",
                "<span style='line-height: 1.5; font-size: 13px; white-space: pre-line;'>",
                safe_message,
                "</span>",
                "</div>"
            )
            
            # Crea il Notice soltanto alla prima chiamata
            if (is.null(private$.noticeObj)) {
                private$.createNotice(
                    jmv_self = jmv_self,
                    name = name,
                    type = type,
                    visible = visible
                )
            }
            
            # Aggiorna il Notice esistente
            private$.noticeObj$set(
                type = type,
                content = html_content
            )
            
            private$.noticeObj$setVisible(visible)
            
            invisible(private$.noticeObj)
        },
        
        #' @description Convalida e prepara i dati necessari per l'analisi
        #' @param self L'oggetto di analisi
        #' @param requiredVars Le variabili o i campi richiesti da convalidare
        #' @return I dati convalidati e preparati per l'analisi
        mzValidateAndPrepareData = function(self, requiredVars) {
            result <- list(success=FALSE, data=NULL, error=NULL)
            tryCatch({
                if (is.null(self) || length(requiredVars) == 0L) {
                    result$error <- "Invalid input parameters"
                    return(result)
                }
                missing <- requiredVars[vapply(requiredVars, function(name) is.null(self$options[[name]]), logical(1))]
                if (length(missing) > 0L) {
                    result$error <- sprintf(.("Missing required variables: %s"), paste(missing, collapse=", "))
                    return(result)
                }
                for (name in requiredVars) {
                    if (is.null(self$data[[self$options[[name]]]])) {
                        result$error <- sprintf(.("Data not found for variable: %s"), name)
                        return(result)
                    }
                }
                for (name in unique(unlist(self$options$moderator, use.names=FALSE))) {
                    if (is.null(self$data[[name]])) {
                        result$error <- sprintf(.("Moderator data not found for: %s"), name)
                        return(result)
                    }
                }
                # dataProc è l'unica preparazione: conserva fattori, rownames e selezione dei casi.
                result$data <- self$dataProc
                mark("[DATA] prepared", list(rows_in=nrow(self$data),
                     rows_used=nrow(result$data), moderators=length(self$options$moderator)), .loc = "mzMetaforService.R:907")
                result$success <- TRUE
            }, error=function(e) {
                mark("[DATA] failed", conditionMessage(e), .loc = "mzMetaforService.R:911")
                result$error <<- sprintf(.("Validation and data preparation failed: %s"), conditionMessage(e))
            })
            result
        },

        #' @description Riporta nel foglio di lavoro le nuove variabili calcolate (ES, varianza e SE) per ciascun studio
        #' @param self L'oggetto di analisi
        mzPopolateOutput = function(self) {
            if (self$options$effectsizeOV) {
                self$results$effectsizeOV$setRowNums(rownames(self$dataProc))
                self$results$effectsizeOV$setValues(self$effectsize)
            }
            
            if (self$options$varianceOV) {
                self$results$varianceOV$setRowNums(rownames(self$dataProc))
                self$results$varianceOV$setValues(self$variance)
            }
            
            if (self$options$stderrorOV) {
                self$results$stderrorOV$setRowNums(rownames(self$dataProc))
                self$results$stderrorOV$setValues(self$stderror)
            }
        },
        
        #' @description Confronta stimatori di eterogeneità
        #' @param data Dati da utilizzare
        #' @param table Tabella da popolare
        #' @param options Oggetto opzioni dal modulo
        #' @param dataType Tipo di analisi
        #' @param requiredVars Variabili richieste
        mzCompareHeterogeneityEstimators = function(data, table, options = NULL,
                                                    dataType, requiredVars) {
            # clearWith invalida questo stato quando cambiano dati o misura.
            # isFilled() non basta: alcune celle NA sono risultati legittimi.
            if (is.null(table) || identical(table$state,list(mzHECVersion=1L))) return()
            jinfo("[HEC] compute", .loc = "mzMetaforService.R:947")
            # Controlla se abbiamo dati validi
            if (is.null(data) || nrow(data) == 0) {
                return()
            }
            
            # Prepara i dati in base al tipo di analisi
            if (dataType == "effectsize") {
                # Per effectsize yi e vi sono gia' presenti nel dataframe
                dat <- data
            } else {
                # Per tutti gli altri tipi, calcola le effect size via escalc
                if (is.null(options) || is.null(options$modelmea)) {
                    return()
                }
                
                # Costruisce i parametri di escalc dinamicamente dalle variabili richieste
                escalc_params <- list(
                    measure = options$modelmea,
                    data = data
                )
                
                # Aggiunge slab se presente
                if ("slab" %in% requiredVars) {
                    escalc_params$slab <- data$slab
                }
                
                # Mappa le variabili richieste ai parametri escalc in base al tipo di dati
                switch(dataType,
                       "dichotomous" = {
                           escalc_params$ai <- data$ai
                           escalc_params$bi <- data$bi
                           escalc_params$ci <- data$ci
                           escalc_params$di <- data$di
                       },
                       "correlation" = {
                           escalc_params$ri <- data$ri
                           escalc_params$ni <- data$ni
                       },
                       "meandiff" = {
                           escalc_params$m1i <- data$m1i
                           escalc_params$sd1i <- data$sd1i
                           escalc_params$n1i <- data$n1i
                           escalc_params$m2i <- data$m2i
                           escalc_params$sd2i <- data$sd2i
                           escalc_params$n2i <- data$n2i
                       },
                       "proportion" = {
                           escalc_params$xi <- data$xi
                           escalc_params$ni <- data$ni
                       },
                       "alpha" = {
                           escalc_params$ai <- data$ai
                           escalc_params$mi <- data$mi
                           escalc_params$ni <- data$ni
                       }
                )
                
                dat <- try(do.call(metafor::escalc, escalc_params))
                
                if (inherits(dat, "try-error")) {
                    return()
                }
            }
            
            # Inizializza la lista per i risultati di tutti i modelli
            models <- list()
            methods <- list(
                "DL" = "[DL] DerSimonian-Laird",
                "HE" = "[HE] Hedges/Cochran",
                "HS" = "[HS] Hunter-Schmidt",
                "SJ" = "[SJ] Sidik-Jonkman",
                "SJ2" = "[SJ2] SJ (Alternative Init.)",
                "ML" = "[ML] Maximum-Likelihood",
                "REML" = "[REML] Restricted ML",
                "EB" = "[EB] Empirical Bayes",
                "PM" = "[PM] Paule-Mandel"
            )
            
            # Adatta tutti i modelli base
            basic_methods <- c("DL", "HE", "HS", "SJ", "ML", "REML", "EB", "PM")
            
            for (method in basic_methods) {
                tryCatch(
                    {
                        models[[method]] <- metafor::rma(yi = dat$yi, vi = dat$vi, method = method, data = dat)
                    },
                    error = function(e) {
                        models[[method]] <- NULL
                    }
                )
            }
            
            # SJ2 (caso speciale: inizializzazione da HE)
            if (!is.null(models[["HE"]])) {
                tryCatch(
                    {
                        models[["SJ2"]] <- metafor::rma(
                            yi = dat$yi, vi = dat$vi, method = "SJ", data = dat,
                            control = list(tau2.init = models[["HE"]]$tau2)
                        )
                        attr(models[["SJ2"]], "mz_estimator") <- "SJ2"
                    },
                    error = function(e) {
                        models[["SJ2"]] <- NULL
                    }
                )
            }
            
            # Azzera prima riga (pulizia tabella esistente)
            table$setRow(rowNo = 1, values = list(method = "", tau = NA, mu = NA, se = NA))
            
            # Popola la tabella con i risultati
            rowNo <- 1
            for (method_id in names(methods)) {
                if (!is.null(models[[method_id]])) {
                    # Estrae i valori
                    tau_val <- sqrt(models[[method_id]]$tau2)
                    mu_val  <- models[[method_id]]$b[1]
                    se_val  <- models[[method_id]]$se[1]
                    
                    # Arrotonda a 4 decimali
                    tau_val <- round(tau_val, 4)
                    mu_val  <- round(mu_val,  4)
                    se_val  <- round(se_val,  4)
                    
                    # Inserisce nella tabella
                    table$setRow(
                        rowNo = rowNo,
                        values = list(
                            method = methods[[method_id]],
                            tau = tau_val,
                            mu = mu_val,
                            se = se_val
                        )
                    )
                    
                    rowNo <- rowNo + 1
                }
            }
            
            # Azzera le righe rimanenti vuote
            if (rowNo <= 9) {
                for (i in rowNo:9) {
                    table$setRow(rowNo = i, values = list(method = "", tau = NA, mu = NA, se = NA))
                }
            }
            table$setState(list(mzHECVersion=1L))
        },
        
        #' @description Esegue i test di bias di pubblicazione
        #' @param jmv_self L'oggetto di analisi
        #' @param res Il modello da testare
        #' @param options Oggetto opzioni del modulo (tipo Fail-safe N)
        #' @return Una lista con i risultati dei test
        mzPublicationBiasTests = function(jmv_self, res, options = NULL) {
            results <- list()
            # La cache segue clearWith della tabella; anche gli esiti NA sono validi.
            cache <- if (!is.null(jmv_self)) jmv_self$results$tabPubBias else NULL
            cached <- if (!is.null(cache)) cache$state else NULL
            if (is.list(cached) && identical(cached$mzPBAVersion, 1L)) {
                jinfo("[CACHE/PBA] hit", .loc = "mzMetaforService.R:1108")
                if (length(cached$notices) > 0L) {
                    self$mzSetNotice(jmv_self, paste(unique(cached$notices), collapse="\n\n"),
                                     type=jmvcore::NoticeType$WARNING)
                }
                for (message in cached$warnings) warning(message, call.=FALSE)
                return(cached$results)
            }
            notices <- character()
            jinfo("[CACHE/PBA] miss", .loc = "mzMetaforService.R:1117")
            warnings <- character()
            # Raccoglie tutti gli avvisi: un solo Notice non deve perdere i precedenti.
            results <- withCallingHandlers({
            # Analisi Fail-Safe N
            tryCatch(
                {
                    fsnResult <- metafor::fsn(x = res$yi, vi = res$vi, type = options$fsntype)
                    
                    if (is.list(fsnResult)) {
                        fsnum <- if (!is.null(fsnResult$fsnum)) fsnResult$fsnum else NA
                        pval  <- if (!is.null(fsnResult$pval))  fsnResult$pval  else NA
                        
                        if (is.na(fsnum)) {
                            if (!is.null(fsnResult$k0))       fsnum <- fsnResult$k0
                            else if (!is.null(fsnResult$ks))  fsnum <- fsnResult$ks
                            else if (!is.null(fsnResult$failsafe)) fsnum <- fsnResult$failsafe
                        }
                        
                        results$failsafePB <- list(fsnum = fsnum, pval = pval)
                    } else if (is.numeric(fsnResult)) {
                        results$failsafePB <- list(fsnum = fsnResult, pval = NA)
                    } else {
                        results$failsafePB <- list(fsnum = NA, pval = NA)
                    }
                },
                error = function(e) {
                    results$failsafePB <<- list(fsnum = NA, pval = NA)
                    errmsg <- paste("Error in fail-safe N analysis:", conditionMessage(e))
                    notices <<- c(notices, errmsg)
                }
            )
            
            # Test di regressione (test di Egger)
            tryCatch(
                {
                    results$regtestPB <- mzRegtest(res)
                },
                error = function(e) {
                    results$regtestPB <<- list(zval = NA, pval = NA)
                    errmsg <- paste("Error in regression test:", conditionMessage(e))
                    notices <<- c(notices, errmsg)
                }
            )
            
            # Test di correlazione di rango (Begg e Mazumdar) e Trim and Fill
            # metafor::ranktest() e metafor::trimfill() non sono definiti
            # per modelli con moderatori (limite strutturale del metodo e non un errore numerico).
            # Stesso controllo gia' usato altrove nel progetto (vedi mzComputeLeave1Out()).
            # Va riconosciuto come caso atteso e saltato silenziosamente, non trattato
            # come anomalia da segnalare all'utente con una Notice di errore.
            # Li ritorno NULL per non averli in tabella e mando una Info con un toast message.
            if (!is.null(res$formula.mods)) {
                
                results$ranktestPB <- NULL
                results$trimfillPB <- NULL
                
                # Toast informativo all'utente
                if (isTRUE(options$distabPBA)) {
                    self$mzTriggerToast(
                        htmlItem = jmv_self$results$toastTrigger,
                        message  = "<b>Rank Correlation</b> and <b>Trim and Fill</b> are not supported for models with moderators.",
                        type     = "info",
                        duration = 4000,
                        position = "CC",
                        theme    = "glass"
                    )
                }
                
            } else {
                # Test di correlazione di rango (Begg e Mazumdar)
                tryCatch(
                    {
                        results$ranktestPB <- metafor::ranktest(x = res)
                    },
                    error = function(e) {
                        results$ranktestPB <<- list(tau = NA, pval = NA)
                        errmsg <- paste("Error in rank correlation test:", conditionMessage(e))
                        notices <<- c(notices, errmsg)
                    }
                )
                
                # Analisi Trim and Fill
                tryCatch(
                    {
                        tfResult <- mzTrimFill(res)
                        
                        results$trimfillPB <- list(
                            k0   = if (!is.null(tfResult$k0))   tfResult$k0   else NA, # Numero studi rimossi
                            side = if (!is.null(tfResult$side)) tfResult$side else NA, # Lato del funnel plot
                            pval = if (!is.null(tfResult$pval)) tfResult$pval else NA,
                            QE   = if (!is.null(tfResult$QE))   tfResult$QE   else NA  # Test per eterogeneita'
                        )
                    },
                    error = function(e) {
                        results$trimfillPB <<- list(k0 = NA, side = NA, pval = NA, QE = NA)
                        errmsg <- paste("Error in trim and fill analysis:", conditionMessage(e))
                        notices <<- c(notices, errmsg)
                    }
                )
            }
            
                results
            }, warning=function(w) {
                warnings <<- c(warnings, conditionMessage(w))
            })
            if (!is.null(jmv_self) && length(notices) > 0L) {
                self$mzSetNotice(jmv_self, paste(unique(notices), collapse="\n\n"),
                                 type=jmvcore::NoticeType$WARNING)
            }
            if (!is.null(cache)) {
                cache$setState(list(mzPBAVersion=1L, results=results,
                                    notices=notices, warnings=warnings))
            }
            return(results)
        },
        
        #' @description Popola la tabella di publication bias
        #' @param pubBiasResults Publication bias test results
        #' @param tabPubBias Publication bias tables
        #' @param options Oggetto delle opzioni del modulo
        mzPopulatePBT = function(pubBiasResults, tabPubBias, options = NULL) {
            if (!is.null(tabPubBias)) {
                # Popola la riga Fail-Safe N
                if (!is.null(pubBiasResults$failsafePB)) {
                    fsnum <- if (!is.null(pubBiasResults$failsafePB$fsnum)) pubBiasResults$failsafePB$fsnum else NA
                    pval <- if (!is.null(pubBiasResults$failsafePB$pval)) pubBiasResults$failsafePB$pval else NA
                    
                    mzSetTableRowIfChanged(tabPubBias,
                        rowNo = 1,
                        values = list(
                            label = "Fail-Safe N",
                            failSafeNumber = fsnum,
                            p = pval
                        )
                    )
                }
                
                # Popola la riga del test di regressione di Egger
                if (!is.null(pubBiasResults$regtestPB)) {
                    zval <- if (!is.null(pubBiasResults$regtestPB$zval)) pubBiasResults$regtestPB$zval else NA
                    pval <- if (!is.null(pubBiasResults$regtestPB$pval)) pubBiasResults$regtestPB$pval else NA
                    
                    mzSetTableRowIfChanged(tabPubBias,
                        rowKey = "Egger",
                        values = list(
                            label = "Egger's Regression",
                            failSafeNumber = zval,
                            p = pval
                        )
                    )
                }
                
                # Popola la riga della correlazione di rango di Begg e Mazumdar
                if (!is.null(pubBiasResults$ranktestPB)) {
                    tau <- if (!is.null(pubBiasResults$ranktestPB$tau)) pubBiasResults$ranktestPB$tau else NA
                    pval <- if (!is.null(pubBiasResults$ranktestPB$pval)) pubBiasResults$ranktestPB$pval else NA
                    
                    mzSetTableRowIfChanged(tabPubBias,
                        rowKey = "BeggMazumdar",
                        values = list(
                            label = "Begg and Mazumdar Rank Correlation",
                            failSafeNumber = tau,
                            p = pval
                        )
                    )
                }
                
                # Popola la riga Trim and Fill
                if (!is.null(pubBiasResults$trimfillPB)) {
                    k0 <- if (!is.null(pubBiasResults$trimfillPB$k0)) pubBiasResults$trimfillPB$k0 else NA
                    pval <- if (!is.null(pubBiasResults$trimfillPB$pval)) pubBiasResults$trimfillPB$pval else NA
                    
                    mzSetTableRowIfChanged(tabPubBias,
                        rowKey = "TrimFill",
                        values = list(
                            label = "Trim and Fill Number of Studies",
                            failSafeNumber = k0,
                            p = pval
                        )
                    )
                }
                
                mzUpdatePublicationBiasMetadata(tabPubBias,options)
            }
        },
        
        #' @description Calcola Leave-One-Out per il modello principale
        #' @param res Oggetto modello rma (senza moderatori)
        #' @return data.frame per-studio con estimate/se/stat/pval/ci.lb/ci.ub/
        #'         tau2/I2/H2/QE/QEp e colonne derivate (delta_estimate,
        #'         influential, delta_tau2, delta_I2). NULL se res non e' un
        #'         oggetto rma valido, se contiene moderatori, o se il calcolo
        #'         fallisce.
        mzComputeLeave1Out = function(res) {
            if (is.null(res) || !inherits(res, "rma")) {
                return(NULL)
            }
            
            # LOO non supportato con moderatori (stesso vincolo di Radial/GOSH)
            if (!is.null(res$formula.mods)) {
                return(NULL)
            }
            
            l1o <- try(mzLeave1Out(res), silent = TRUE)
            if (inherits(l1o, "try-error")) {
                return(NULL)
            }
            
            l1oDf <- try(as.data.frame(l1o), silent = TRUE)
            if (inherits(l1oDf, "try-error") || is.null(l1oDf) || nrow(l1oDf) == 0) {
                return(NULL)
            }
            
            # metafor::leave1out usa nomi colonna Q/Qp (non QE/QEp come l'oggetto
            # rma principale) e zval oppure tval a seconda di options$testType.
            # Normalizza su nomi interni stabili usati dal resto del modulo.
            statCol <- if ("zval" %in% names(l1oDf)) {
                l1oDf$zval
            } else if ("tval" %in% names(l1oDf)) {
                l1oDf$tval
            } else {
                NA_real_
            }
            
            tau2Col <- if ("tau2" %in% names(l1oDf)) l1oDf$tau2 else NA_real_
            I2Col   <- if ("I2"   %in% names(l1oDf)) l1oDf$I2   else NA_real_
            H2Col   <- if ("H2"   %in% names(l1oDf)) l1oDf$H2   else NA_real_
            QECol   <- if ("Q"    %in% names(l1oDf)) l1oDf$Q    else NA_real_
            QEpCol  <- if ("Qp"   %in% names(l1oDf)) l1oDf$Qp   else NA_real_
            
            out <- data.frame(
                slab       = as.character(res$slab),
                estimate   = l1oDf$estimate,
                se         = l1oDf$se,
                stat       = statCol,
                pval       = l1oDf$pval,
                ci.lb      = l1oDf$ci.lb,
                ci.ub      = l1oDf$ci.ub,
                tau2       = tau2Col,
                I2         = I2Col,
                H2         = H2Col,
                QE         = QECol,
                QEp        = QEpCol,
                stringsAsFactors = FALSE
            )
            
            # --- Colonne derivate ---
            pooledEstimate <- as.numeric(res$b[1])
            pooledTau2 <- if (!is.null(res$tau2)) res$tau2 else NA_real_
            pooledI2   <- if (!is.null(res$I2))   res$I2   else NA_real_
            
            out$delta_estimate <- out$estimate - pooledEstimate
            out$influential    <- !(pooledEstimate >= out$ci.lb & pooledEstimate <= out$ci.ub)
            out$delta_tau2     <- out$tau2 - pooledTau2
            out$delta_I2       <- out$I2   - pooledI2
            
            return(out)
        },
        
        #' @description Genera un funnel plot
        #' @param plotData Dati o modello da rappresentare
        #' @param options Opzioni del grafico
        #' @param subgroup_data Optional vector of subgroup data
        #' @return TRUE se il grafico è stato generato correttamente, FALSE altrimenti
        mzFunnelPlot = function(plotData, options, subgroup_data = NULL) {
            # Controlla la disponibilitC  dei dati richiesti
            if (is.null(plotData) || !inherits(plotData, "rma")) {
                return(FALSE)
            }
            
            # Recupera le opzioni del funnel plot
            yaxis <- options$yaxis
            yaxisInv <- options$yaxisInv
            enhanceFunnel <- options$enhanceFunnel
            #sunsetFunnel <- options$sunsetFunnel
            measure <- options$modelmea
            showBackTransform <- isTRUE(options$showBackTransform)
            
            # Usa helper unico per ottenere info trasformazione
            # Conserva il contratto comune dei chiamanti; PFT non applica inverse.
            niForTransf <- if (!is.null(plotData$ni)) plotData$ni else NULL
            measureInfo <- .mzGetBackTransform(options$tabbedControlModelType, measure, showBackTransform, ni = niForTransf)
            
            # Imposta valori da measureInfo
            atransf <- measureInfo$atransf
            # Default nativo metafor: pooled senza moderatori, zero sui residui.
            refline <- if(isTRUE(plotData$int.only))as.numeric(plotData$beta[1])else 0
            xlab <- if(isTRUE(plotData$int.only))measureInfo$xlab else
                if(is.function(options$translate))options$translate('Residual Value')else 'Residual Value'
            if(!isTRUE(plotData$int.only))atransf<-NULL
            
            # Aggiunge il titolo personalizzato, se specificato
            if (!is.null(options$xAxisTitle) && nchar(options$xAxisTitle) > 0) {
                xlab <- paste(xlab, " - ", options$xAxisTitle)
            }
            
            tryCatch(
                {
                    # Determine whether to use inverted y-axis
                    if (yaxisInv && yaxis != "lni") {
                        yaxisTrans <- paste0(yaxis, "nv")
                        
                        # Crea il grafico funnel con contorni di significatività
                        if (enhanceFunnel && (yaxis == "sei" || yaxis == "vi")) {
                            metafor::funnel(
                                plotData,
                                yaxis = yaxisTrans,
                                level = c(90, 95, 99),
                                shade = c("white", "gray", "darkgray"),
                                refline = refline,
                                atransf = atransf,
                                label = "out",
                                xlab = xlab,
                                legend = TRUE
                            )
                        } else {
                            # Grafico funnel senza contorni di significatività
                            metafor::funnel(
                                plotData,
                                yaxis = yaxisTrans,
                                refline = refline,
                                atransf = atransf,
                                label = "out",
                                xlab = xlab
                            )
                        }
                    } else {
                        # Crea il grafico funnel standard con contorni di significatività
                        if (enhanceFunnel && (yaxis == "sei" || yaxis == "vi")) {
                            metafor::funnel(
                                plotData,
                                yaxis = yaxis,
                                level = c(90, 95, 99),
                                shade = c("white", "gray", "darkgray"),
                                refline = refline,
                                atransf = atransf,
                                label = "out",
                                xlab = xlab,
                                legend = TRUE
                            )
                        } else {
                            # Grafico funnel senza contorni di significatività
                            metafor::funnel(
                                plotData,
                                yaxis = yaxis,
                                refline = refline,
                                atransf = atransf,
                                label = "out",
                                xlab = xlab
                            )
                        }
                    }
                    
                    return(TRUE)
                },
                error = function(e) {
                    # warning(paste("Error in metafor funnel plot:", e$message))
                    return(FALSE)
                }
            )
            
            return(FALSE)
        }
    )
)
