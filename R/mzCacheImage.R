# =============================================================================
# mzCacheImage.R
# Utilita di cache per i grafici nei moduli jamovi
#
# Fornisce un sistema a strati e intercambiabile per evitare ricalcoli
# ridondanti del modello statistico e re-render del grafico quando
# cambiano solo opzioni estetiche.
#
# Principi architetturali:
#   - Funzioni stateless: nessuno stato globale, nessun effetto collaterale
#     al di fuori dell'API jamovi
#   - Pattern Strategy: il calcolo delle dimensioni e una funzione
#     intercambiabile (dimsFn)
#   - Lazy evaluation: il calcolo del modello e differito fino a quando
#     effettivamente necessario
#   - Fail-safe: ogni funzione usa guard silenziosi anziche crashare
#
# Ambito di utilizzo:
#   - Grafici Image prodotti da metafor (grafica base di R)
#   - Grafici Html prodotti da d3.js / rendering client-side
#
# Requisito: jamovi >= 2.7.27.2 (jmvcore image.R commit f1bce8a)
#   Senza quel fix, mzApplyCachedSize provoca un re-render spurio e
#   l'intera cache risulta inefficace.
#
# Utilizzo nel modulo:
#   Mettere questo file nella cartella R/ del modulo. jmvtools include
#   automaticamente tutti i file .R di R/ nell'ambiente del pacchetto
#   alla compilazione. Non occorre source() o library().
# =============================================================================


# -----------------------------------------------------------------------------
# SEZIONE 1 - Verifica prerequisiti di sistema
# -----------------------------------------------------------------------------

#' Verifica che jmvcore supporti la cache dei grafici
#'
#' Il fix in fromProtoBuf (commit f1bce8a) che impedisce alle chiamate
#' programmatiche a setSize() di invalidare filePath e obbligatorio.
#' Chiamare una volta al momento dell'attach del pacchetto (es. .onAttach).
#'
#' @param silent  Se TRUE sopprime l'avviso (utile nei test automatici).
#' @return Invisibile TRUE se la versione e sufficiente, FALSE altrimenti.
#' @noRd
mzAssertCacheSupport <- function(silent = FALSE) {
  ver <- tryCatch(utils::packageVersion("jmvcore"), error = function(e) NULL)
  ok  <- !is.null(ver) && ver >= "2.7.27.2"
  if (!ok && !silent) {
    # Nota: warning() non e' visibile all'utente jamovi (nessuna console,
    # nessun contesto di analisi/self$results a questo punto). Resta un
    # avviso di livello R/console per sviluppo e installazione del pacchetto.
    warning(
      "mzCacheImage: jmvcore >= 2.7.27.2 is required for plot caching. ",
      "Current version: ", if (is.null(ver)) "not found" else as.character(ver),
      ". Image caching will be bypassed.", call. = FALSE
    )
  }
  invisible(ok)
}


# -----------------------------------------------------------------------------
# SEZIONE 2 - Cache del modello statistico
# -----------------------------------------------------------------------------

#' Guard: tutte le variabili richieste sono assegnate
#'
#' Controlla i binding delle opzioni senza toccare alcun active binding
#' che potrebbe innescare il calcolo del modello. Usare in .run() prima
#' di accedere a self$model per evitare di forzare il binding
#' inutilmente quando le variabili non sono ancora assegnate.
#'
#' @param options  Oggetto opzioni jamovi (self$options).
#' @param vars     Vettore di nomi di opzione (come dichiarati in .a.yaml)
#'                 che devono essere tutti non-NULL.
#' @return TRUE quando tutte le variabili sono assegnate, FALSE altrimenti.
#' @noRd
mzHasRequiredVars <- function(options, vars) {
  for (v in vars) {
    if (is.null(options[[v]])) return(FALSE)
  }
  TRUE
}


# Un tentativo per istanza/richiesta, anche se produce NULL o solleva un errore.
# Gli stati non entrano nei result$state: una nuova richiesta puo riprovare.
# Il primo errore viene propagato intatto; i successivi accessi restituiscono
# NULL senza coprirlo con errori secondari durante rendering o salvataggio.
mzRequestCalculation <- function(owner, key, value) {
  if (is.null(owner$.requestCalculations)) owner$.requestCalculations <- list()
  state <- owner$.requestCalculations[[key]]
  if (!is.null(state)) return(owner[[key]])
  owner$.requestCalculations[[key]] <- list(status='running')
  result <- tryCatch(force(value), error=function(e) {
    owner[[key]] <- NULL
    owner$.requestCalculations[[key]] <- list(status='failed', message=conditionMessage(e))
    stop(e)
  })
  owner[[key]] <- result
  owner$.requestCalculations[[key]] <- list(status=if(is.null(result))'empty' else
    if(inherits(result,'try-error'))'failed' else 'ready')
  result
}


#' Recupera o calcola un modello statistico dalla cache
#'
#' Implementa la cache del modello tra cicli successivi usando lo slot
#' serializzato $state di un elemento risultato come backing store.
#'
#' Percorso cache HIT (ciclo con sola modifica estetica):
#'   Il cacheElement non e stato invalidato da clearWith, il suo $state
#'   e ancora popolato, l'argomento model NON viene mai valutato
#'   per effetto della lazy evaluation di R, costo computazionale zero.
#'
#' Percorso cache MISS (ciclo con modifica statistica o prima esecuzione):
#'   $state e NULL, model viene valutato, risultato salvato con
#'   setState() per tutti i cicli futuri fino alla prossima modifica
#'   statistica.
#'
#' Il cacheElement deve essere un elemento risultato (Html, Table, Group)
#' la cui clearWith elenca SOLO le opzioni che influenzano il modello
#' statistico, mai opzioni estetiche.
#'
#' @param cacheElement  Elemento risultato jamovi usato come anchor della
#'   cache. La sua clearWith deve coprire esattamente solo le opzioni
#'   che richiedono ricalcolo del modello.
#' @param model         Qualsiasi espressione R che produce il modello.
#'   Valutata in modo lazy, non chiamata su cache hit.
#' @return L'oggetto modello, o NULL se il calcolo ha restituito NULL.
#' @noRd
mzGetCachedModel <- function(cacheElement, model) {

  # Controlla se il modello e già disponibile nello stato serializzato
  cached <- cacheElement$state
  if (!is.null(cached)) {
    jinfo("[CACHE/FIT] hit", .loc = "mzCacheImage.R:139")
    return(cached)
  }
  
  # Cache miss: calcola il modello e lo salva per i cicli successivi
  jinfo("[CACHE/FIT] miss", .loc = "mzCacheImage.R:144")
  if (!is.null(model)) {
    cacheElement$setState(model)
  } else {
    jinfo("[CACHE/FIT] empty result", .loc = "mzCacheImage.R:148")
  }
  model
}


# -----------------------------------------------------------------------------
# SEZIONE 3 - Cache delle dimensioni dell'immagine
# -----------------------------------------------------------------------------

#' Ripristina le dimensioni del grafico dalla cache permanente
#'
#' Chiamare da .postInit(). Il metodo .postInit() e un hook reale del
#' ciclo di vita jamovi (confermato in jmvcore/R/analysis.R): viene
#' chiamato dopo .init() e prima di .run(), quindi prima che jamovi
#' invochi la renderFun del grafico.
#'
#' Ciclo di vita jamovi completo in ordine di esecuzione:
#'   fromProtoBuf() -> .init() -> .postInit() -> .run() -> renderFun()
#'
#' In jmvcore >= 2.7.27.2, setSize() chiamato programmaticamente NON
#' azzera filePath, quindi il file del grafico gia renderizzato viene
#' preservato quando e cambiata solo un'opzione estetica. Questo e il
#' meccanismo centrale che elimina il re-render sui cicli estetici.
#'
#' Condizioni guard (tutte devono essere soddisfatte per il ripristino):
#'   - sizeCache$state non e NULL (dimensioni salvate in precedenza)
#'   - image$visible e TRUE (il grafico e visibile)
#'   - image$isFilled() e TRUE (filePath ancora valido, il grafico
#'     NON e stato invalidato da una modifica statistica in clearWith)
#'
#' @param image      Elemento risultato Image di jamovi.
#' @param sizeCache  Elemento risultato Group di jamovi con clearWith: [].
#' @noRd
mzApplyCachedSize <- function(image, sizeCache) {

  dims <- sizeCache$state


  # Le vecchie preview possono avere cache di soli assi, senza dimensioni.
  validDimension <- function(value)
    is.numeric(value) && length(value) == 1L && is.finite(value) && value > 0
  # Non passare NULL/NA a setSize: corromperebbe il protobuf al ciclo successivo.
  if (!is.null(dims) && validDimension(dims$w) && validDimension(dims$h) &&
      isTRUE(image$visible) && image$isFilled()) {
    image$setSize(dims$w, dims$h)
    jinfo("[IMG/SIZE] restored", dims$w, "x", dims$h, .loc = "mzCacheImage.R:194")
  }
}


#' Calcola, applica e cachea le dimensioni dell'immagine
#'
#' Orchestratore generico chiamato da .run() dopo che .postInit() ha
#' gia tentato il ripristino dalla cache. Delega il calcolo effettivo
#' delle dimensioni alla strategia dimsFn fornita, poi salva il
#' risultato in sizeCache per i cicli successivi.
#'
#' Guard: ritorna subito senza calcolo ne salvataggio quando:
#'   - image$isFilled() e TRUE: immagine ancora valida, dimensioni gia
#'     in cache e ripristinate da mzApplyCachedSize() in .postInit()
#'   - image$visible e FALSE: grafico non mostrato
#'   - model e NULL: calcolo statistico non ancora disponibile
#'
#' @param image      Elemento risultato Image di jamovi.
#' @param model      Oggetto modello statistico. NULL provoca ritorno immediato.
#' @param options    Oggetto opzioni jamovi (self$options).
#' @param sizeCache  Elemento risultato Group di jamovi con clearWith: [].
#' @param dimsFn     Strategia di dimensionamento: funzione con firma
#'   function(model, options, ...) -> list(w = <px>, h = <px>).
#'   Costruire con mzDimsMetafor(), mzDimsFixed() o mzDimsCustom().
#' @param ...        Argomenti extra passati verbatim a dimsFn.
#' @noRd
mzUpdateImageSize <- function(image, model, options, sizeCache, dimsFn, ...) {
    
    
    # Guard: skip se il grafico e ancora valido o i dati non sono disponibili
    if (!image$visible || image$isFilled() || is.null(model)) {
        
        return()
    }
    dims <- dimsFn(model, options, ...)
    
    
    # Preserva i campi esistenti nella sizeCache
    # (come xlim, ylim, alim e layoutSig) che sono
    # stati salvati nei cicli precedenti, sovrascrivendo
    # solo w e h.
    if (!is.null(sizeCache$state)) {
        
        oldState <- sizeCache$state
        
        for (k in names(oldState)) {
            if (!(k %in% names(dims))) {
                dims[[k]] <- oldState[[k]]
            }
        }
    }
    
    
    image$setSize(dims$w, dims$h)
    
    
    
    sizeCache$setState(dims)
    jinfo("[IMG/SIZE] saved", dims$w, "x", dims$h, .loc = "mzCacheImage.R:253")
    
    
}


# -----------------------------------------------------------------------------
# SEZIONE 4 - Factory delle strategie di dimensionamento
#
# Una factory restituisce una dimsFn da passare a mzUpdateImageSize:
#   dimsFn <- mzDimsXxx(...)
#   mzUpdateImageSize(image, model, options, sizeCache, dimsFn)
#
# Tutte le dimsFn restituite condividono la stessa firma:
#   function(model, options, ...) -> list(w = <integer px>, h = <integer px>)
#
# NOTA: mzDimsMetafor usa una stima formula (topMargin + k * rowHeight +
# botMargin). E la strategia attuale in attesa di una implementazione
# definitiva basata sul valore invisibile restituito da metafor::forest(),
# che contiene xlim, ylim, rows, cex calcolati internamente da metafor.
# La versione formula rimane utile come fallback.
# -----------------------------------------------------------------------------

#' Strategia dimensioni: stima formula per metafor (provvisoria)
#'
#' Stima le dimensioni del grafico dal numero di righe del modello e
#' da parametri di calibrazione configurabili.
#'
#' metafor usa la grafica base di R: le dimensioni del device devono
#' essere specificate PRIMA del render, quindi la misurazione post-render
#' non e applicabile. La strategia formula e la soluzione attuale in
#' attesa di implementare la cattura del valore invisibile di forest().
#'
#' Formula altezza (in pollici, convertita in pixel a 72 dpi):
#'   h = topMarginIn + (nRighe * rowHeightIn) + botMarginIn
#'
#' getRowsFn controlla quante righe vengono conteggiate. Sovrascrivere per:
#'   - Forest con sottogruppi: righe per gruppo + righe blank + summary
#'   - Leave-one-out: k righe, ognuna omette uno studio
#'   - Forest cumulativo: k righe
#'   - Qualsiasi layout dove il conteggio differisce da model$k
#'
#' @param widthIn     Larghezza grafico in pollici (default 10.0).
#' @param rowHeightIn Altezza per riga studio in pollici (default 0.30).
#'   Calibrare sul proprio output metafor::forest() con cex = 1.
#' @param topMarginIn Overhead fisso superiore in pollici (default 1.2).
#'   Include intestazioni colonne, titolo, righe blank iniziali.
#' @param botMarginIn Overhead fisso inferiore in pollici (default 1.8).
#'   Include poligono effetto complessivo, asse x, riga annotazioni.
#' @param getRowsFn   Funzione function(model, options) che restituisce
#'   il numero intero di righe da disegnare. Default: model$k.
#' @return Una strategia function(model, options, ...).
#' @noRd
mzDimsMetafor <- function(
    widthIn     = 10.0,
    rowHeightIn = 0.30,
    topMarginIn = 1.2,
    botMarginIn = 1.8,
    getRowsFn   = function(model, options) model$k
) {
  # Larghezza in pixel: costante per tutta la vita della strategia
  w <- as.integer(round(widthIn * 72))

  function(model, options, ...) {

    # Conteggio righe con fallback sicuro in caso di errore
    nRows <- tryCatch(
      as.integer(getRowsFn(model, options)),
      error = function(e) {
        20L
      }
    )
    nRows <- max(nRows, 1L)

    # Calcolo altezza in pixel a 72 dpi
    h <- as.integer(round((topMarginIn + nRows * rowHeightIn + botMarginIn) * 72))
    list(w = w, h = h)
  }
}


#' Strategia dimensioni: pixel fissi
#'
#' Restituisce dimensioni costanti indipendentemente dal contenuto del
#' modello. Appropriata per grafici la cui dimensione non dipende dal
#' volume dei dati: funnel plot, radial plot, Baujat plot, GOSH plot.
#'
#' Anche questa strategia partecipa al ciclo completo della cache
#' (guard isFilled, persist e restore di sizeCache), garantendo il
#' sizing corretto dopo reload di sessione e prevenendo re-render
#' ridondanti.
#'
#' @param widthPx   Larghezza in pixel.
#' @param heightPx  Altezza in pixel.
#' @return Una strategia function(model, options, ...).
#' @noRd
mzDimsFixed <- function(widthPx, heightPx) {
  # Dimensioni pre-calcolate e immutabili per tutta la vita della strategia
  dims <- list(w = as.integer(widthPx), h = as.integer(heightPx))
  function(model, options, ...) dims
}


#' Strategia dimensioni: funzione personalizzata
#'
#' Avvolge una funzione di sizing arbitraria in modo che sia conforme
#' all'interfaccia della strategia. Usare quando ne mzDimsMetafor ne
#' mzDimsFixed si adattano al caso specifico.
#'
#' @param fn  Funzione con firma
#'   function(model, options, ...) -> list(w = <px>, h = <px>).
#' @return La stessa funzione, validata al momento della factory.
#' @noRd
mzDimsCustom <- function(fn) {
  stopifnot(is.function(fn))
  fn
}


# -----------------------------------------------------------------------------
# SEZIONE 5 - Helper per grafici Html (d3.js / rendering client-side)
#
# Gli elementi Html renderizzati da d3.js non hanno un filePath e
# quindi non beneficiano della cache delle dimensioni dell'immagine.
# L'ottimizzazione rilevante e la cache del modello (Sezione 2) piu
# un guard isFilled() per saltare la rigenerazione HTML su cicli
# in cui nulla e cambiato.
# -----------------------------------------------------------------------------

#' Popola un elemento risultato Html con guard isFilled
#'
#' Scrive il contenuto in un elemento Html jamovi prodotto da d3.js o
#' qualsiasi altro renderer client-side. Il guard isFilled() esce
#' immediatamente quando l'elemento non e stato invalidato da clearWith,
#' quindi contentFn non viene valutata su cicli invariati.
#'
#' @param htmlResult  Elemento risultato Html di jamovi.
#' @param contentFn   Funzione senza argomenti che restituisce una stringa
#'   HTML. Valutata in modo lazy, non chiamata quando isFilled() e TRUE.
#' @noRd
mzPopulateHtmlPlot <- function(htmlResult, contentFn) {
  # Guard: skip se l'elemento non e visibile o e ancora valido
  if (!htmlResult$visible || htmlResult$isFilled()) return()
  htmlResult$setContent(contentFn())
}


#' Inizializza lo skeleton di un elemento risultato Html
#'
#' Scrive un placeholder minimale (solo titolo) prima che il modello sia
#' disponibile. Guard con isFilled() e mzHasRequiredVars.
#'
#' @param htmlResult   Elemento risultato Html di jamovi.
#' @param options      Oggetto opzioni jamovi.
#' @param requiredVars Vettore di nomi opzione che devono essere assegnati
#'   prima che il modello possa essere calcolato.
#' @param title        Titolo mostrato nel placeholder.
#' @noRd
mzInitHtmlElement <- function(htmlResult, options, requiredVars, title = "") {
  # Guard: skip se gia riempito o non visibile
  if (!htmlResult$visible || htmlResult$isFilled()) return()
  # Scrivi solo lo skeleton se le variabili non sono ancora assegnate
  if (!mzHasRequiredVars(options, requiredVars)) {
    htmlResult$setContent(
      if (nzchar(title)) paste0("<h2>", title, "</h2>") else ""
    )
  }
}


# -----------------------------------------------------------------------------
# SEZIONE 6 - Compositor di convenienza per i pattern metafor piu comuni
# -----------------------------------------------------------------------------

#' Aggiorna un forest plot metafor a dimensione variabile
#'
#' Compositor di convenienza per il pattern piu comune in Metafora:
#' un forest plot ad altezza variabile guidato da un oggetto rma o rma.mv.
#' Combina mzDimsMetafor e mzUpdateImageSize in una singola chiamata.
#'
#' Per forest plot con sottogruppi dove il conteggio righe differisce
#' da model$k, fornire getRowsFn. Esempio sottogruppi:
#'
#'   mzUpdateForestPlot(
#'     image, model, options, sizeCache,
#'     getRowsFn = function(m, o) m$k + length(unique(m$mods)) * 2L
#'   )
#'
#' @param image       Elemento risultato Image di jamovi.
#' @param model       Oggetto rma o rma.mv. NULL provoca ritorno immediato.
#' @param options     Oggetto opzioni jamovi.
#' @param sizeCache   Elemento risultato Group di jamovi con clearWith: [].
#' @param widthIn     Larghezza grafico in pollici (default 10.0).
#' @param rowHeightIn Altezza riga in pollici (default 0.30).
#' @param topMarginIn Overhead margine superiore in pollici (default 1.2).
#' @param botMarginIn Overhead margine inferiore in pollici (default 1.8).
#' @param getRowsFn   Funzione function(model, options) -> integer.
#'   Default: model$k.
#' @noRd
mzUpdateForestPlot <- function(
    image,
    model,
    options,
    sizeCache,
    widthIn     = 10.0,
    rowHeightIn = 0.30,
    topMarginIn = 1.2,
    botMarginIn = 1.8,
    getRowsFn   = function(m, o) m$k
) {
  robLayout <- .mzClassicRobLayout(options)
  if (!is.null(robLayout)) {
    widthIn <- widthIn + robLayout$widthIn
    botMarginIn <- botMarginIn + robLayout$heightIn
  }
  mzUpdateImageSize(
    image     = image,
    model     = model,
    options   = options,
    sizeCache = sizeCache,
    dimsFn    = mzDimsMetafor(
      widthIn     = widthIn,
      rowHeightIn = rowHeightIn,
      topMarginIn = topMarginIn,
      botMarginIn = botMarginIn,
      getRowsFn   = getRowsFn
    )
  )
}


#' Aggiorna un grafico a dimensione fissa
#'
#' Compositor di convenienza per grafici che si renderizzano sempre con
#' le stesse dimensioni: funnel plot, radial plot, Baujat plot, GOSH plot.
#' Partecipa comunque al ciclo completo della cache.
#'
#' @param image      Elemento risultato Image di jamovi.
#' @param model      Oggetto modello statistico. NULL provoca ritorno immediato.
#' @param options    Oggetto opzioni jamovi.
#' @param sizeCache  Elemento risultato Group di jamovi con clearWith: [].
#' @param widthPx    Larghezza in pixel (default 700).
#' @param heightPx   Altezza in pixel (default 500).
#' @noRd
mzUpdateFixedPlot <- function(
    image,
    model,
    options,
    sizeCache,
    widthPx  = 700L,
    heightPx = 500L
) {
  mzUpdateImageSize(
    image     = image,
    model     = model,
    options   = options,
    sizeCache = sizeCache,
    dimsFn    = mzDimsFixed(widthPx, heightPx)
  )
}
