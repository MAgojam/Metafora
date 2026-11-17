### -- Funzione helper per formattare nomi colonne (Title Case)
.formatColumnName <- function(name) {
  if (is.null(name) || !is.character(name) || nchar(name) == 0) {
    return(name)
  }
  paste0(toupper(substring(name, 1, 1)), tolower(substring(name, 2)))
}

# Escapa caratteri HTML minimi (contenuti loggati possono includere nomi di
# variabili/studio arbitrari: senza escaping, un '<' o '&' nel dato rompe il
# rendering dell'accordion)
htmlSpecialChars <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# I servizi puri possono essere chiamati anche da R senza un'analisi jamovi.
# Nel modulo ricevono self esplicito; fuori dal modulo mantengono il testo inglese.
.mzTranslationSelf <- function(self=NULL, options=NULL) {
  if (!is.null(self)) return(self)
  translate <- if (!is.null(options) && is.function(options$translate)) options$translate
               else function(text,n=1) text
  list(options=list(translate=translate))
}

### -- Hash polinomiale per ID DOM/canale stabili e a bassa collisione
### Sostituisco la precedente sum(utf8ToInt(x)) %% 1e8, perchè quella somma è
### invariante per anagrammi e collide facilmente.
### Questo hash tiene conto della posizione dei caratteri (Horner's method),
### riducendo le collisioni a ~0 su campioni di migliaia di stringhe
### (fatto una verifica empirica).
### mod = 1e8 mantenuto identico all'originale per compatibilità di formato
### (stessa lunghezza esadecimale attesa nei consumer downstream).
.mzPolyHash <- function(x, mod = 1e8, base = 131) {
  chars <- utf8ToInt(x)
  h <- 0
  for (c in chars) {
    h <- (h * base + c) %% mod
  }
  sprintf("%x", h)
}

### -- Funzione per estrarre i dati per una tabella forest
mzExtractForestData <- function(escalcResults,
                                modelResults,
                                options,
                                colNameMap = NULL,
                                modelType = NULL) {
  #jinfo("mzExtractForestData: Start")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  if (is.null(escalcResults) || is.null(modelResults)) {
    stop("escalcResults and modelResults cannot be NULL.")
  }
  
  # Estrazione parametri da options
  measure <- options$modelmea
  ciLevel <- if (!is.null(options$level)) options$level / 100 else 0.95
  showBackTransform <- tryCatch(isTRUE(options$showBackTransform), error = function(e) FALSE)
  
  # Determina modelType dinamicamente
  if (is.null(modelType)) {
    if (!is.null(options$tabbedControlModelType)) {
      modelType <- options$tabbedControlModelType
    } else {
      # Fallback sicuro o errore
      stop("modelType not specified and 'tabbedControlModelType' not found in options.")
    }
  }
  
  dataEscalc <- as.data.frame(escalcResults)
  nRows <- nrow(dataEscalc)
  
  # --- 1. PREPARAZIONE COLONNE BASE ---
  alpha <- 1 - ciLevel
  ciMult <- qnorm(1 - alpha / 2)
  
  # Calcolo CI su scala trasformata (es. Log Scale)
  dataEscalc$Effect <- dataEscalc$yi
  dataEscalc$CI_Lower <- dataEscalc$yi - ciMult * sqrt(dataEscalc$vi)
  dataEscalc$CI_Upper <- dataEscalc$yi + ciMult * sqrt(dataEscalc$vi)
  
  # Nuovi campi Var e SE
  dataEscalc$Var <- dataEscalc$vi
  dataEscalc$SE <- sqrt(dataEscalc$vi)
  
  weights <- weights(modelResults)
  # Formattazione Weight in % con 2 decimali
  dataEscalc$W_perc <- sprintf("%.2f%%", (weights / sum(weights)) * 100)
  
  
  # --- 2. GESTIONE DEI DATI SPECIFICI PER MODELLO ---
  
  dataSpec <- NULL
  overallSpecValues <- list()
  specColsNames <- character(0)
  
  # Helper per estrarre e formattare colonne
  extractCols <- function(cols, formatStr = "%.0f", isInteger = TRUE) {
    res <- list()
    for (col in cols) {
      # IMPORTANTE: usa sempre il nome INTERNO (col) per leggere dal dataframe
      # perché le colonne in dataEscalc si chiamano ai, bi, ci, di (nomi interni)
      if (col %in% names(dataEscalc)) {
        vals <- dataEscalc[[col]]
        if (isInteger) {
          formatted <- as.character(round(vals))
        } else {
          formatted <- sprintf(formatStr, vals)
        }
        
        # Usa colNameMap solo per il NOME della colonna nel risultato e applica Title Case
        outputName <- if (!is.null(colNameMap) && col %in% names(colNameMap)) {
          .formatColumnName(colNameMap[[col]])
        } else {
          .formatColumnName(col)
        }
        res[[outputName]] <- formatted
      } else {
        # Fallback vuoto
        outputName <- if (!is.null(colNameMap) && col %in% names(colNameMap)) {
          .formatColumnName(colNameMap[[col]])
        } else {
          .formatColumnName(col)
        }
        res[[outputName]] <- rep("", nRows)
      }
    }
    return(res)
  }
  
  # Helper per calcolare somme (per conteggi) o medie pesate (per medie - opzionale, qui mettiamo vuoto per semplicità sui continui)
  calcOverallSum <- function(cols) {
    res <- list()
    for (col in cols) {
      # Usa il nome INTERNO per leggere i dati
      if (col %in% names(dataEscalc) && is.numeric(dataEscalc[[col]])) {
        sumVal <- as.character(round(sum(dataEscalc[[col]], na.rm = TRUE)))
      } else {
        sumVal <- ""
      }
      
      # Usa colNameMap solo per il NOME della colonna nel risultato e applica Title Case
      outputName <- if (!is.null(colNameMap) && col %in% names(colNameMap)) {
        .formatColumnName(colNameMap[[col]])
      } else {
        .formatColumnName(col)
      }
      res[[outputName]] <- sumVal
    }
    return(res)
  }
  
  # --- SWITCH MODELLI ---
  if (modelType == "dichotomous") {
    # 4 Colonne: ai, bi, ci, di (Eventi/Totali o Eventi/Non-Eventi)
    colsNeeded <- c("ai", "bi", "ci", "di")
    rawDataList <- extractCols(colsNeeded, isInteger = TRUE)
    dataSpec <- as.data.frame(rawDataList, stringsAsFactors = FALSE)
    overallSpecValues <- calcOverallSum(colsNeeded)
  } else if (modelType == "proportion") {
    # 2 Colonne: xi (eventi), ni (totale)
    colsNeeded <- c("xi", "ni")
    rawDataList <- extractCols(colsNeeded, isInteger = TRUE)
    dataSpec <- as.data.frame(rawDataList, stringsAsFactors = FALSE)
    overallSpecValues <- calcOverallSum(colsNeeded)
  } else if (modelType == "correlation") {
    # 2 Colonne: ri (correlazione), ni (sample size)
    
    # Nomi interni (dati)
    rColData <- "ri"
    nColData <- "ni"
    
    # Nomi output (da mappa se presente)
    rColOutName <- if (!is.null(colNameMap$ri)) colNameMap$ri else rColData
    nColOutName <- if (!is.null(colNameMap$ni)) colNameMap$ni else nColData
    
    # Formato Title Case
    rColOut <- .formatColumnName(rColOutName)
    nColOut <- .formatColumnName(nColOutName)
    
    # Estrazione Dati (usa nomi interni)
    valsR <- if (rColData %in% names(dataEscalc)) sprintf("%.2f", dataEscalc[[rColData]]) else rep("", nRows)
    valsN <- if (nColData %in% names(dataEscalc)) as.character(round(dataEscalc[[nColData]])) else rep("", nRows)
    
    dataSpec <- data.frame(valsR, valsN, stringsAsFactors = FALSE)
    names(dataSpec) <- c(rColOut, nColOut)
    
    # Totali: Somma N (usa nome interno)
    sumN <- if (nColData %in% names(dataEscalc)) as.character(sum(dataEscalc[[nColData]], na.rm = TRUE)) else ""
    overallSpecValues <- setNames(list("", sumN), c(rColOut, nColOut))
  } else if (modelType == "meandiff") {
    # 6 Colonne in ordine: n1i, m1i, sd1i, n2i, m2i, sd2i
    # N interi, Medie e SD decimali
    grp1Cols <- c("n1i", "m1i", "sd1i")
    grp2Cols <- c("n2i", "m2i", "sd2i")
    
    # Estrazione custom per formati misti
    resList <- list()
    allCols <- c(grp1Cols, grp2Cols)
    
    for (col in allCols) {
      # col è già il nome interno (es. m1i, n1i)
      
      # Nome Output
      outNameRaw <- if (!is.null(colNameMap) && col %in% names(colNameMap)) colNameMap[[col]] else col
      outputName <- .formatColumnName(outNameRaw)
      
      if (col %in% names(dataEscalc)) {
        if (grepl("^n", col)) { # Se è un N (n1i, n2i) -> Intero
          resList[[outputName]] <- as.character(round(dataEscalc[[col]]))
        } else { # Media o SD -> Decimale
          resList[[outputName]] <- sprintf("%.2f", dataEscalc[[col]])
        }
      } else {
        resList[[outputName]] <- rep("", nRows)
      }
    }
    dataSpec <- as.data.frame(resList, stringsAsFactors = FALSE)
    
    # Totali: Solo N ha senso sommare
    ovList <- list()
    for (col in allCols) {
      # Nome Output
      outNameRaw <- if (!is.null(colNameMap) && col %in% names(colNameMap)) colNameMap[[col]] else col
      outputName <- .formatColumnName(outNameRaw)
      
      if (grepl("^n", col) && col %in% names(dataEscalc)) {
        ovList[[outputName]] <- as.character(sum(dataEscalc[[col]], na.rm = TRUE))
      } else {
        ovList[[outputName]] <- ""
      }
    }
    overallSpecValues <- ovList
  } else if (modelType == "alpha") {
    # 3 Colonne: ai (alpha), mi (items), ni (sample size)
    colsNeeded <- c("ai", "mi", "ni")
    # Alpha decimale, mi e ni interi
    resList <- list()
    for (col in colsNeeded) {
      # col è già il nome interno (ai, mi, ni)
      
      # Nome Output
      outNameRaw <- if (!is.null(colNameMap) && col %in% names(colNameMap)) colNameMap[[col]] else col
      outputName <- .formatColumnName(outNameRaw)
      
      if (col %in% names(dataEscalc)) {
        if (col == "ai") {
          resList[[outputName]] <- sprintf("%.3f", dataEscalc[[col]])
        } else {
          resList[[outputName]] <- as.character(round(dataEscalc[[col]]))
        }
      } else {
        resList[[outputName]] <- rep("", nRows)
      }
    }
    dataSpec <- as.data.frame(resList, stringsAsFactors = FALSE)
    overallSpecValues <- setNames(list("", "", ""), names(dataSpec)) # Totali non banali per alpha
  } else {
    # GENERIC / EFFECTSIZE (yi, vi) o Fallback
    # Nessuna colonna dati specifica di default, a meno che non sia passata in colNameMap custom
    dataSpec <- data.frame(matrix(ncol = 0, nrow = nRows))
    overallSpecValues <- list()
  }
  
  # Descrittive complete, con lo stesso contratto delle ilab del Forest.
  summaries <- mzForestInputSummary(dataEscalc, modelType)
  for (key in names(summaries)) {
    outputName <- .formatColumnName(if (!is.null(colNameMap[[key]])) colNameMap[[key]] else key)
    value <- summaries[[key]]
    overallSpecValues[[outputName]] <- if (!is.finite(value)) "" else
      if (key %in% c("ri","m1i","m2i","sd1i","sd2i") || (modelType=="alpha"&&key=="ai"))
        sprintf("%.3f",value) else as.character(value)
  }

  # --- 3. COMBINAZIONE DEI DATI DEGLI STUDI ---
  
  # Gestione Header Estimate con nota
  estColName <- "Estimate"
  if (!is.null(measure)) {
    # Se back-transform è attivo, la label cambia (gestito fuori o qui?)
    # Per ora manteniamo la nota generica, ma potremmo rimuoverla se trasformato
    estColName <- paste0("Estimate<sup>1</sup>")
  }
  
  # --- 4. RIGA TOTALI ---
  overallEffect <- modelResults$b[1, 1]
  overallSe <- modelResults$se[1]
  overallVar <- overallSe^2
  # Il CI del modello incorpora l'inferenza scelta, inclusi t e Knapp-Hartung.
  overallCiLower <- modelResults$ci.lb[1]
  overallCiUpper <- modelResults$ci.ub[1]
  
  # --- 5. BACK-TRANSFORM ---
  # Recupera funzione di trasformazione inversa (fonte unica, mzMetaforService.R)
  niForTransf <- if ("ni" %in% names(dataEscalc)) dataEscalc$ni else NULL
  transfRes <- .mzGetBackTransform(modelType, measure, showBackTransform, ni = niForTransf)
  atransf <- transfRes$atransf
  
  if (!is.null(atransf)) {
    # Conserva il contratto generale dei renderer per gli argomenti ausiliari.
    transformValues <- function(x) {
      if (is.null(transfRes$targs)) atransf(x)
      else atransf(x, targs = transfRes$targs)
    }
    # Applica trasformazione ai dati degli studi
    dataEscalc$Effect <- transformValues(dataEscalc$Effect)
    dataEscalc$CI_Lower <- transformValues(dataEscalc$CI_Lower)
    dataEscalc$CI_Upper <- transformValues(dataEscalc$CI_Upper)
    
    # Applica trasformazione ai totali
    overallEffect <- transformValues(overallEffect)
    overallCiLower <- transformValues(overallCiLower)
    overallCiUpper <- transformValues(overallCiUpper)
    
    # Nota: SE e Var non si trasformano linearmente, spesso si omettono o si ricalcolano (delta method)
    # Per semplicità, se trasformato, potremmo nascondere SE/Var o lasciarli sulla scala originale (ma confonde)
    # Jamovi Forest Plot standard mostra SE? No, mostra CI.
    # Lasciamo SE/Var sulla scala originale o li nascondiamo?
    # Per ora li lasciamo originali (Log scale) ma è un punto di attenzione.
  }
  
  # Costruzione DF principale
  # Se dataSpec è vuoto (es. effectsize), non aggiunge colonne
  
  # Gestione Subgroup
  hasSubgroup <- !is.null(options$subgroup) && "subgroup" %in% names(dataEscalc)
  
  dataStudies <- data.frame(
    Study = as.character(dataEscalc$slab %||% paste("Study", 1:nRows)),
    stringsAsFactors = FALSE
  )
  
  if (hasSubgroup) {
    # Aggiunge colonna Subgroup all'inizio
    dataStudies <- cbind(data.frame(Subgroup = as.character(dataEscalc$subgroup), stringsAsFactors = FALSE), dataStudies)
  }
  
  if (ncol(dataSpec) > 0) {
    dataStudies <- cbind(dataStudies, dataSpec)
  }
  
  dataStudies <- cbind(dataStudies, data.frame(
    Var = dataEscalc$Var,
    SE = dataEscalc$SE,
    `W%` = dataEscalc$W_perc,
    Estimate = dataEscalc$Effect,
    Lower = dataEscalc$CI_Lower,
    Upper = dataEscalc$CI_Upper,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
  
  # Rinomina colonna Estimate se necessario
  if (estColName != "Estimate") {
    names(dataStudies)[names(dataStudies) == "Estimate"] <- estColName
  }
  
  rowOverallList <- list()
  if (hasSubgroup) {
    rowOverallList$Subgroup <- "" # Cella vuota per la riga dei totali
  }
  rowOverallList$Study <- "Summary"
  
  rowOverallList <- c(
    rowOverallList,
    overallSpecValues,
    list(
      Var = overallVar,
      SE = overallSe,
      `W%` = "100.00%",
      Estimate = overallEffect,
      Lower = overallCiLower,
      Upper = overallCiUpper
    )
  )
  
  # Rinomina chiave Estimate nella lista overall se necessario
  if (estColName != "Estimate") {
    names(rowOverallList)[names(rowOverallList) == "Estimate"] <- estColName
  }
  
  rowOverall <- do.call(data.frame, c(rowOverallList, list(stringsAsFactors = FALSE, check.names = FALSE)))
  
  # Assicura che i nomi delle colonne corrispondano esattamente a dataStudies
  rowOverall <- rowOverall[, names(dataStudies)]
  
  # Nei modelli con moderatori l'intercetta non è un riepilogo globale degli studi.
  finalDf <- if (isTRUE(modelResults$int.only)) rbind(dataStudies, rowOverall) else dataStudies
  
  return(finalDf)
}


### -- Prepara il codice HTML per una tabella non nativa di jamovi
mzHtmlTableBuilder <- function(title = "",
                               headers = NULL,
                               data = NULL,
                               supertitles = NULL,
                               # --- CONTROLLI LAYOUT ---
                               colWidths = NULL,
                               colAlign = NULL,
                               colValign = NULL,
                               showVerticalBorders = FALSE,
                               # --- CONTROLLI FORMATTAZIONE DATI ---
                               naFormat = "\u2014",
                               numberFormat = "%.3f",
                               pValueCutoff = 0.001,
                               # --- PARAMETRI DI STILE E ALLINEAMENTO GLOBALE ---
                               titleAlignH = "left",
                               titleAlignV = "middle",
                               headerAlignH = "center",
                               headerAlignV = "top",
                               dataAlignH = "center",
                               dataAlignV = "top",
                               footerAlignH = "left",
                               footerAlignV = "top",
                               titleStyle = list(weight = "normal", style = "normal", color = "#000000"),
                               superStyle = list(weight = "normal", style = "normal", color = "#000000"),
                               headerStyle = list(weight = "normal", style = "normal", color = "#000000"),
                               dataStyle = list(weight = "normal", style = "normal", color = "#000000"),
                               # --- SHADING E CUSTOMIZZAZIONE CELLA ---
                               rowShade = NULL,
                               colShade = NULL,
                               regionShade = NULL,
                               cellStyles = NULL,
                               hiddenColumns = NULL,
                               groupingColumns = NULL,
                               summaryRows = NULL,
                               summaryRowStyle = "border-top: 1.5px solid #000; font-weight: bold; font-style: italic;",
                               footnote = NULL) {
  #jinfo("mzHtmlTableBuilder: Start")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # --- CONTROLLI PRE-RENDERING E FILTRAGGIO ---
  
  if (is.null(headers)) stop("Headers cannot be NULL.")
  
  if (!is.null(hiddenColumns) && is.data.frame(data)) {
    data <- data[, !(names(data) %in% hiddenColumns), drop = FALSE]
  }
  
  if (length(headers) != ncol(data)) {
    stop(paste0("Headers count (", length(headers), ") does not match the number of visible columns in data (", ncol(data), ") after filtering hiddenColumns."))
  }
  
  if (!is.null(colWidths) && length(colWidths) != ncol(data)) {
    stop("colWidths must have the same length as the number of visible columns.")
  }
  
  # Vettori di allineamento
  if (is.null(colAlign)) colAlign <- rep(dataAlignH, ncol(data))
  if (is.null(colValign)) colValign <- rep(dataAlignV, ncol(data))
  if (length(headerAlignH) == 1) {
    headerAlignHVec <- rep(headerAlignH, ncol(data))
  } else {
    headerAlignHVec <- headerAlignH
  }
  
  # --- LOGICA: CALCOLO ROWSPAN ---
  rowspan_matrix <- matrix(1,
                           nrow = nrow(data), ncol = ncol(data),
                           dimnames = list(NULL, names(data))
  )
  
  if (!is.null(groupingColumns)) {
    for (col_name in groupingColumns) {
      if (col_name %in% names(data)) {
        col_index <- which(names(data) == col_name)
        rle_result <- rle(data[[col_name]])
        current_row <- 1
        for (i in seq_along(rle_result$lengths)) {
          len <- rle_result$lengths[i]
          if (len > 1) {
            rowspan_matrix[current_row, col_index] <- len
            if (len > 1) {
              rowspan_matrix[(current_row + 1):(current_row + len - 1), col_index] <- 0
            }
          }
          current_row <- current_row + len
        }
      }
    }
  }
  
  # --- FUNZIONI DI UTILITÀ PER STRINGHE E STILE ---
  get_style_str <- function(h_align, v_align, custom_style = NULL) {
    style_str <- paste0("text-align:", h_align, ";", "vertical-align:", v_align, ";")
    if (!is.null(custom_style)) {
      style_str <- paste0(
        style_str,
        "font-weight:", custom_style$weight %||% "normal", ";",
        "font-style:", custom_style$style %||% "normal", ";",
        "color:", custom_style$color %||% "#000000", ";"
      )
    }
    return(style_str)
  }
  
  get_cell_custom_style <- function(r, c) {
    if (is.null(cellStyles)) {
      return(NULL)
    }
    style_def <- cellStyles[
      sapply(cellStyles, function(s) s$row == r && s$col == c)
    ]
    if (length(style_def) == 1) {
      return(style_def[[1]]$style)
    }
    return(NULL)
  }
  
  get_width_str <- function(col_index) {
    if (is.null(colWidths)) {
      return(list(css = "", overflow_style = ""))
    }
    
    width_val <- colWidths[col_index]
    
    if (tolower(width_val) == "auto") {
      # Aggiunto white-space: nowrap per evitare a capo e forzare scroll orizzontale
      return(list(css = "white-space: nowrap;", overflow_style = ""))
    } else {
      css_width <- paste0("width:", width_val, ";")
      overflow_style <- "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 1px;"
      return(list(css = css_width, overflow_style = overflow_style))
    }
  }
  
  # --- FUNZIONE HELPER PER SHADE AVANZATO ---
  get_region_shade <- function(r, c) {
    if (is.null(regionShade)) {
      return("")
    }
    
    for (rule in regionShade) {
      rs <- rule$row_start %||% 1
      re <- rule$row_end %||% nrow(data)
      cs <- rule$col_start %||% 1
      ce <- rule$col_end %||% ncol(data)
      
      if (r >= rs && r <= re && c >= cs && c <= ce) {
        return(rule$color %||% "")
      }
    }
    return("")
  }
  
  # --- COSTRUZIONE HTML PER BLOCCHI ---
  
  html_parts <- list()
  
  # 1. CSS Globale
  table_class <- "jmvCustom"
  css <- c(
    "<style>",
    "table.jmvCustom { border-collapse: collapse; width: 100%; font-family: sans-serif; font-size: 12px; color: #000000; }",
    "table.jmvCustom th, table.jmvCustom td { padding: 2px 4px; line-height: 1.1; }",
    ".jmv-line { border-bottom: 1.1px solid #000; }",
    ".jmv-thicktop { border-top: 1.5px solid #000; }",
    ".jmv-super-bot { border-bottom: 1.1px solid #000; }",
    ".p-red { color: #cc0000; font-weight: normal; }",
    ".p-blue { color: #0044cc; font-weight: normal; }",
    ".jmv-super-empty { height: 0px; padding: 0px; line-height: 0; }",
    "</style>"
  )
  
  if (showVerticalBorders) {
    css <- c(
      css,
      "<style>",
      "table.jmvCustom.vert-border th, table.jmvCustom.vert-border td { border-right: 1px solid #ddd; }",
      "table.jmvCustom.vert-border td:last-child { border-right: none; }",
      "</style>"
    )
    table_class <- "jmvCustom vert-border"
  }
  
  html_parts[["css"]] <- paste(css, collapse = "\n")
  
  # 2. RIGA 0: titolo
  titleStyleStr <- get_style_str(titleAlignH, titleAlignV, titleStyle)
  title_row <- paste0("<tr><th colspan=\"", ncol(data), "\" style=\"font-size: 14px;", titleStyleStr, "\">", title, "</th></tr>")
  line_title <- paste0("<tr><td colspan=\"", ncol(data), "\" class=\"jmv-line\"></td></tr>")
  html_parts[["title"]] <- c(title_row, line_title)
  
  # 3. RIGA 1: intestazioni di gruppo
  st_row <- NULL
  st_line_row <- NULL
  if (!is.null(supertitles) && length(supertitles) > 0) {
    superStyleStr <- get_style_str("center", "middle", superStyle)
    st_cells <- rep("<td></td>", ncol(data))
    st_line_cells <- rep("<td></td>", ncol(data))
    
    for (st in supertitles) {
      label <- st$label
      cols <- st$cols
      colspan_val <- length(cols)
      th_str <- paste0("<th colspan=\"", colspan_val, "\" style=\"", superStyleStr, "\">", label, "</th>")
      st_cells[cols[1]] <- th_str
      if (length(cols) > 1) {
        st_cells[cols[-1]] <- ""
      }
      for (j in cols) {
        st_line_cells[j] <- "<td class=\"jmv-super-bot\"></td>"
      }
    }
    st_row <- paste0("<tr>", paste(st_cells[st_cells != ""], collapse = ""), "</tr>")
    st_line_row <- paste0("<tr>", paste(st_line_cells, collapse = ""), "</tr>")
  } else {
    st_row <- paste0("<tr><td colspan=\"", ncol(data), "\" class=\"jmv-super-empty\"></td></tr>")
  }
  html_parts[["supertitles"]] <- c(st_row, st_line_row)
  
  # 4. RIGA 2: intestazioni senza ordinamento interattivo
  header_cells <- mapply(function(h, h_align, j) {
    headerStyleStr <- get_style_str(h_align, headerAlignV, headerStyle)
    width_info <- get_width_str(j)
    final_style <- paste0(headerStyleStr, width_info$css)
    # NB: gli header possono contenere markup HTML intenzionale (es. apice
    # nota <sup>1</sup> costruito da mzExtractForestData) - NON va escapato
    # qui. I nomi derivati da variabile utente (Study/Subgroup) sono gia'
    # sanificati a monte in formatCol() nei singoli backend .b.R.
    paste0("<th style=\"", final_style, "\">", h, "</th>")
  }, headers, headerAlignHVec, seq_along(headers), SIMPLIFY = TRUE)
  
  header_row <- paste0("<tr>", paste(header_cells, collapse = ""), "</tr>")
  header_line <- paste0("<tr><td colspan=\"", ncol(data), "\" class=\"jmv-line\"></td></tr>")
  html_parts[["header"]] <- c(header_row, header_line)
  
  # 5. DATA ROWS
  data_rows_list <- lapply(1:nrow(data), function(i) {
    row_bg_shade <- if (!is.null(rowShade) && i %% 2 != 0) rowShade else ""
    isSummary <- !is.null(summaryRows) && (i %in% summaryRows)
    rowClass <- if (isSummary) "jmv-summary-row" else "jmv-data-row"
    
    cells <- mapply(function(val, colname, align, valign, c_shade, j) {
      if (rowspan_matrix[i, j] == 0) {
        return(NULL)
      }
      
      rowspan_val <- rowspan_matrix[i, j]
      rowspan_attr <- if (rowspan_val > 1) paste0(" rowspan=\"", rowspan_val, "\"") else ""
      
      dataCellStyle <- get_style_str(align, valign, dataStyle)
      width_info <- get_width_str(j)
      
      # --- DETERMINAZIONE SFONDO FINALE (Priorità: Cella > Regione > Colonna > Riga Alternata) ---
      
      # 1. Regione (massima priorità)
      region_bg_color <- get_region_shade(i, j)
      
      # 2. Colonna (colShade)
      col_bg_color <- if (!is.null(colShade) && c_shade != "") c_shade else NULL
      
      # Precedenza Sfondo (Regionale > Colonna > Riga Alternata)
      # region_bg_color è vuota se manca una corrispondenza; nzchar verifica la presenza del colore
      # ECCEZIONE: Le colonne di raggruppamento (groupingColumns) non ricevono zebratura
      is_grouping_col <- !is.null(groupingColumns) && colname %in% groupingColumns
      
      bg_color <- ""
      if (nzchar(region_bg_color)) {
        bg_color <- region_bg_color
      } else if (!is.null(col_bg_color)) {
        bg_color <- col_bg_color
      } else if (nzchar(row_bg_shade) && !is_grouping_col) {
        bg_color <- row_bg_shade
      }
      
      final_bg_style <- if (nzchar(bg_color)) paste0("background-color:", bg_color, ";") else ""
      
      # Aggiunta stile summary row se applicabile
      sumStyle <- if (isSummary) summaryRowStyle else ""
      
      final_style_str <- paste0(dataCellStyle, final_bg_style, width_info$css, width_info$overflow_style, sumStyle)
      
      class_attr <- ""
      # Escaping HTML del testo (studi/subgroup provengono dal dataset
      # dell'utente): i rami numerici sotto riassegnano sempre val_content,
      # quindi l'escaping qui e' innocuo per quel caso (fix dopo controllo).
      val_content <- htmlSpecialChars(as.character(val))
      
      # --- GESTIONE VALORI E FORMATTAZIONE ---
      
      if (is.numeric(val)) {
        if (is.na(val) || is.nan(val) || is.infinite(val)) {
          val_content <- naFormat
          class_attr <- ""
        } else if (tolower(colname) %in% c("p", "pvalue", "p-value")) {
          p <- as.numeric(val)
          pFormatStr <- numberFormat
          if (p < pValueCutoff) {
            cutoffStr <- sprintf(pFormatStr, pValueCutoff)
            val_content <- paste0("<", sub("0\\.", "\\.", cutoffStr))
            class_attr <- " class=\"p-red\""
          } else if (p < 0.05) {
            val_content <- sprintf(pFormatStr, p)
            class_attr <- " class=\"p-red\""
          } else {
            val_content <- sprintf(pFormatStr, p)
            class_attr <- " class=\"p-blue\""
          }
        } else {
          val_content <- sprintf(numberFormat, val)
          class_attr <- ""
        }
      }
      
      # 4. Customizzazione Cella (Precedenza: Massima, prevale su tutto)
      custom_cell_style <- get_cell_custom_style(i, j)
      if (!is.null(custom_cell_style)) {
        final_style_str <- paste0(final_style_str, custom_cell_style$css %||% "")
        val_content <- custom_cell_style$html %||% val_content
      }
      
      paste0("<td", rowspan_attr, class_attr, " style=\"", final_style_str, "\">", val_content, "</td>")
    }, data[i, ], names(data), colAlign, colValign, colShade %||% rep("", ncol(data)), seq_along(data), SIMPLIFY = TRUE)
    
    cells <- cells[!sapply(cells, is.null)]
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })
  html_parts[["data_rows"]] <- unlist(data_rows_list)
  
  # 6. FOOTER
  footerStyleStr <- get_style_str(footerAlignH, footerAlignV, NULL)
  fn_row <- paste0(
    "<tr><td colspan=\"", ncol(data), "\" class=\"jmv-thicktop\" style=\"",
    footerStyleStr, "font-style: italic; font-size: 12px;\">",
    footnote %||% "", "</td></tr>"
  )
  html_parts[["footer"]] <- fn_row
  
  # --- ASSEMBLY FINALE (SENZA JAVASCRIPT) ---
  table_content <- paste(unlist(html_parts[c("title", "supertitles", "header", "data_rows", "footer")]), collapse = "\n")
  
  final_html <- paste0(
    html_parts[["css"]],
    # "\n<style>\n.jmv-results-html:has(.metafora-html-table) { width: max-content !important; }\n</style>\n",
    "\n<style>\n.jmv-results-html:has(details.mz-accordion[open] .metafora-html-table) { width: max-content !important; }\n</style>\n",
    "\n<div class=\"metafora-html-table\" style=\"overflow-x: auto; width: 100%;\">\n",
    "<table class=\"", table_class, "\">\n",
    table_content,
    "\n</table>\n",
    "</div>"
  )
  
  return(final_html)
}

# === Helpers comuni per Grafica HTML ===

.mzHtmlStyles <- function() {
  paste0(
    "<style>",
    
    # --- ACCORDION: stile "Info & Notes" portato su <details> nativo ---
    # Colore accento personalizzabile via --mz-accordion-accent (default blu
    # jamovi #3E6DA9), stessa tecnica di --mz-subtab-accent in css.js.
    "details.mz-accordion { font-family: sans-serif; margin-bottom: 16px; }",
    "details.mz-accordion summary { list-style: none; cursor: pointer; display: flex; align-items: center; padding: 4px 0; color: var(--mz-accordion-accent, #3E6DA9); font-size: 12px; font-weight: 500; transition: all 0.3s ease; user-select: none; }",
    "details.mz-accordion summary::-webkit-details-marker { display: none; }",
    "details.mz-accordion[open] > summary { font-weight: 700; }",
    
    ".mz-accordion-icon-container { width: 18px; height: 18px; display: flex; align-items: center; justify-content: center; margin-right: 6px; flex-shrink: 0; border-right: 1px solid color-mix(in srgb, var(--mz-accordion-accent, #3E6DA9) 50%, transparent); padding-right: 6px; }",
    ".mz-accordion-icon { width: 16px; height: 16px; color: var(--mz-accordion-accent, #3E6DA9); }",
    ".mz-accordion-icon .mz-icon-v { transition: transform 0.3s ease, opacity 0.3s ease; transform-origin: center; }",
    "details.mz-accordion[open] .mz-accordion-icon .mz-icon-v { transform: rotate(90deg); opacity: 0; }",
    
    ".mz-accordion-title { display: flex; align-items: center; flex-grow: 1; margin-left: 0; white-space: nowrap; }",
    ".mz-accordion-title::before, .mz-accordion-title::after { content: ''; height: 1px; background-color: color-mix(in srgb, var(--mz-accordion-accent, #3E6DA9) 50%, transparent); transition: all 0.3s ease; }",
    ".mz-accordion-title::before { flex-basis: 30px; margin: 0 8px; }",
    ".mz-accordion-title::after { flex-basis: 30px; margin-left: 8px; flex-grow: 0; }",
    "details.mz-accordion[open] .mz-accordion-title::before, details.mz-accordion[open] .mz-accordion-title::after { height: 2px; background-color: color-mix(in srgb, var(--mz-accordion-accent, #3E6DA9) 70%, transparent); }",
    "details.mz-accordion[open] .mz-accordion-title::after { flex-grow: 1; }",
    
    ".mz-accordion-badge { flex-shrink: 0; margin-left: 8px; }",
    ".mz-accordion-body { padding: 10px 2px 4px 2px; }",
    
    # --- Stili preesistenti (tabelle, badge, footer, ecc.) ---
    "table.mz-table { border-collapse: collapse; width: 100%; font-size: 0.95em; color: #333; border-top: 1px solid #222; border-bottom: 2px solid #222; margin-top: 8px; }",
    "table.mz-table thead tr:last-child th { border-bottom: 1px solid #222; }",
    "table.mz-table th { padding: 4px 8px; font-weight: bold; }",
    "table.mz-table td { padding: 4px 8px; vertical-align: middle; }",
    "table.mz-table tr:last-child td { border-bottom: none; }",
    ".mz-header { margin-bottom: 4px; text-align: left; color: #333; font-weight: normal; font-size: 1em; }",
    ".mz-divider { border-bottom: 1px solid #222; margin-bottom: 8px; }",
    ".mz-footer { margin-top: 15px; padding-top: 10px; border-top: 1px solid #222; font-size: 0.9em; color: #555; }",
    ".p-red { color: #cc0000; font-weight: bold; } .p-blue { color: #0044cc; }",
    ".mz-badge { margin-top: 10px; padding: 4px 12px; background: #e3f2fd; color: #1565c0; border-radius: 12px; display: inline-block; font-size: 0.85em; font-weight: bold; border: 1px solid #1565c0; }",
    
    "</style>"
  )
}

#' @param title Testo del titolo (semplice stringa, senza badge concatenati)
#' @param badge HTML opzionale (es. uno <span> badge) renderizzato come
#'              sibling a fine riga, dopo la linea che si estende
#' @param open  Se TRUE (default) l'accordion parte espanso
#' @param color Colore accento opzionale (es. "#E87352"). Se NULL usa il
#'              default blu jamovi definito in .mzHtmlStyles(). Impostato
#'              come custom property inline, cascata automaticamente a
#'              tutti i discendenti (icona, linee, testo header).
.mzHtmlAccordionStart <- function(title, badge = NULL, open = TRUE, color = NULL) {
  openAttr <- if (isTRUE(open)) "open" else ""
  colorAttr <- if (!is.null(color) && nzchar(color)) {
    paste0(" style='--mz-accordion-accent: ", color, ";'")
  } else {
    ""
  }
  badgeHtml <- if (!is.null(badge)) paste0("<span class='mz-accordion-badge'>", badge, "</span>") else ""
  paste0(
    "<details class='mz-accordion' ", openAttr, colorAttr, ">",
    "<summary>",
    "<span class='mz-accordion-icon-container'>",
    "<svg class='mz-accordion-icon' viewBox='0 0 16 16' fill='none' stroke='currentColor' stroke-width='2'>",
    "<line x1='4' y1='8' x2='12' y2='8'></line>",
    "<line class='mz-icon-v' x1='8' y1='4' x2='8' y2='12'></line>",
    "</svg>",
    "</span>",
    "<span class='mz-accordion-title'>", title, "</span>",
    badgeHtml,
    "</summary>",
    "<div class='mz-accordion-body'>"
  )
}

.mzHtmlAccordionEnd <- function() {
  "</div></details>"
}

#' Avvolge un blocco HTML gia' pronto (es. l'output di mzHtmlTableBuilder())
#' nell'accordion unificato, includendo automaticamente .mzHtmlStyles().
#' Riusabile identica in tutti i backend/modelli senza duplicare logica:
#' basta passare l'HTML gia' costruito e un titolo.
#'
#' @param html_content Stringa HTML gia' pronta (es. una tabella)
#' @param title Titolo dell'header dell'accordion
#' @param badge HTML opzionale (badge a fine riga titolo)
#' @param open Se TRUE (default) l'accordion parte espanso
#' @param color Colore accento opzionale, vedi .mzHtmlAccordionStart()
.mzHtmlAccordionWrap <- function(html_content, title, badge = NULL, open = TRUE, color = NULL) {
  paste0(
    .mzHtmlStyles(),
    .mzHtmlAccordionStart(title, badge = badge, open = open, color = color),
    html_content,
    .mzHtmlAccordionEnd()
  )
}

### ==========================================================================
### TABS CSS-ONLY (nuovi, generici) - nessun <script>, radio+label+sibling
### selector. Riusabili ovunque serva un tabbed control dentro un risultato
### Html senza rischiare il CSP.
### ==========================================================================

.mzHtmlTabsCss <- function() {
  paste0(
    "<style>",
    ".mz-tabs input[type='radio'] { display: none; }",
    ".mz-tabs-nav { display: flex; gap: 4px; border-bottom: 1px solid #d7dde3; margin-bottom: 14px; }",
    ".mz-tabs-nav label { padding: 6px 14px; cursor: pointer; font-size: 12px; font-weight: 500; color: #6b7a89; border-bottom: 2px solid transparent; user-select: none; transition: color 0.2s ease, border-color 0.2s ease; }",
    ".mz-tabs-nav label:hover { color: #3E6DA9; }",
    ".mz-tab-panel { display: none; line-height: 1.7; font-size: 13.5px; }",
    ".mz-tab-panel p { margin: 0 0 14px 0; text-align: justify; }",
    ".mz-report-stat { font-weight: 600; color: #1f3a5f; }",
    ".mz-report-callout { margin: 4px 0 16px 0; padding: 10px 14px; background: #eef3fa; border-left: 3px solid #3E6DA9; font-size: 13px; color: #2c3e50; border-radius: 0 3px 3px 0; }",
    "</style>"
  )
}

#' Costruisce un blocco di tab CSS-only (nessun JS, nessun rischio CSP).
#'
#' @param tabs Lista nominata label -> contenuto HTML, es.
#'             list(Methods = "<p>...</p>", Results = "<p>...</p>")
#' @param group_id Prefisso univoco per gli id/name dei radio (evita
#'                 collisioni se piu' blocchi tab compaiono nella stessa pagina)
#' @param active Indice (1-based) del tab attivo di default
.mzHtmlTabsBuild <- function(tabs, group_id, active = 1) {
  
  n <- length(tabs)
  labelsTxt <- names(tabs)
  
  radios <- character(n)
  labelsHtml <- character(n)
  panels <- character(n)
  rules <- character(n)
  
  for (i in seq_len(n)) {
    rid <- sprintf("%s-tab-%d", group_id, i)
    checkedAttr <- if (i == active) "checked" else ""
    
    radios[i] <- sprintf("<input type='radio' name='%s' id='%s' %s>", group_id, rid, checkedAttr)
    labelsHtml[i] <- sprintf("<label for='%s'>%s</label>", rid, labelsTxt[i])
    panels[i] <- sprintf("<div class='mz-tab-panel' id='%s-panel'>%s</div>", rid, tabs[[i]])
    
    rules[i] <- paste0(
      "#", rid, ":checked ~ .mz-tab-panels #", rid, "-panel { display: block; } ",
      "#", rid, ":checked ~ .mz-tabs-nav label[for='", rid, "'] { color: #3E6DA9; font-weight: 700; border-bottom-color: #3E6DA9; }"
    )
  }
  
  paste0(
    "<style>", paste(rules, collapse = "\n"), "</style>",
    "<div class='mz-tabs'>",
    paste(radios, collapse = ""),
    "<div class='mz-tabs-nav'>", paste(labelsHtml, collapse = ""), "</div>",
    "<div class='mz-tab-panels'>", paste(panels, collapse = ""), "</div>",
    "</div>"
  )
}

mzHtmlTost <- function(res,
                       lower = -0.5,
                       upper = 0.5,
                       alpha = 0.05,
                       digits = 4,
                       digits_bound = 3,
                       title = .("Equivalence Test (TOST)"),
                       jamovi_blue = "#3E6DA9", self=NULL) {
  self <- .mzTranslationSelf(self)
  # =========================
  # Controlli preliminari
  # =========================
  if (is.null(res$beta) || is.null(res$se)) {
    return(.("<div class='jmv-error'>Invalid meta-analysis model object (missing beta/se).</div>"))
  }
  
  ES <- as.numeric(res$beta[1])
  SE <- as.numeric(res$se[1])
  
  if (is.na(ES) || is.na(SE)) {
    return(.("<div class='jmv-error'>Invalid meta-analysis model object (ES/SE are NA).</div>"))
  }
  
  # =========================
  # Esegue TOSTmeta
  # =========================
  tost <- try(
    TOSTER::TOSTmeta(
      ES = ES,
      se = SE,
      low_eqbound_d = lower,
      high_eqbound_d = upper,
      alpha = alpha,
      plot = FALSE,
      verbose = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(tost, "try-error")) {
    mark('[HTML/TOST] failed',.mzLogErrorText(tost),ui_log_obj=self$results$debugLog, .loc = "mzHtmlService.R:954")
    return(paste0(
      "<div class='jmv-error'>",
      htmlSpecialChars(conditionMessage(attr(tost, "condition"))),
      "</div>"
    ))
  }
  
  # =========================
  # Funzioni di supporto
  # =========================
  # Formattatore delle statistiche con la precisione standard
  fmt_s <- function(x, d = digits) {
    if (is.null(x) || is.na(x)) {
      return("NA")
    }
    formatC(as.numeric(x), digits = d, format = "f")
  }
  
  # Formattatore di limiti e ipotesi con precisione digits_bound
  fmt_b <- function(x, d = digits_bound) {
    if (is.null(x) || is.na(x)) {
      return("NA")
    }
    formatC(as.numeric(x), digits = d, format = "f")
  }
  
  fmt_p <- function(p, d = digits) {
    if (is.null(p) || is.na(p)) {
      return("NA")
    }
    if (p < .001) {
      return("&lt; .001")
    }
    formatC(p, digits = d, format = "f")
  }
  
  # =========================
  # Calcola il valore Z NHST, assente nell'oggetto TOST
  # =========================
  NHST_Z <- ES / SE
  
  # =========================
  # Criteri decisionali
  # =========================
  # Equivalenza: entrambi i p-value TOST sono inferiori ad alpha
  equivalent <- (!is.na(tost$TOST_p1) && !is.na(tost$TOST_p2) &&
                   tost$TOST_p1 < alpha && tost$TOST_p2 < alpha)
  
  # Significatività NHST: il p-value è inferiore ad alpha
  nhst_sig <- (!is.na(tost$NHST_p) && tost$NHST_p < alpha)
  
  # Esito combinato
  if (nhst_sig && equivalent) {
    combined_msg <- .("The observed effect is <b>statistically different from zero</b> and <b>statistically equivalent within the specified bounds</b>.")
  } else if (nhst_sig && !equivalent) {
    combined_msg <- .("The observed effect is <b>statistically different from zero</b> but <b>not statistically equivalent within the specified bounds</b>.")
  } else if (!nhst_sig && equivalent) {
    combined_msg <- .("The observed effect is <b>not statistically different from zero</b> but <b>statistically equivalent within the specified bounds</b>.")
  } else {
    combined_msg <- .("The observed effect is <b>not statistically different from zero</b> and <b>not statistically equivalent within the specified bounds</b>.")
  }
  
  # Indicatori con lo stile standard di Metafora
  equiv_badge <- if (equivalent) {
    "<span style='float:right; padding:2px 10px; background:#d4edda; color:#155724; border-radius:12px; font-size:0.75em; font-weight:bold; border:1px solid #c3e6cb;'>EQUIVALENT</span>"
  } else {
    .("<span style='float:right; padding:2px 10px; background:#f8d7da; color:#721c24; border-radius:12px; font-size:0.75em; font-weight:bold; border:1px solid #f5c6cb;'>NOT EQUIVALENT</span>")
  }
  
  nhst_badge <- if (nhst_sig) {
    "<span style='padding:2px 8px; background:#d1ecf1; color:#0c5460; border-radius:10px; font-size:0.75em; font-weight:bold; border:1px solid #bee5eb;'>SIGNIFICANT</span>"
  } else {
    "<span style='padding:2px 8px; background:#e2e3e5; color:#383d41; border-radius:10px; font-size:0.75em; font-weight:bold; border:1px solid #d6d8db;'>NON-SIGNIFICANT</span>"
  }
  
  # =========================
  # Generazione HTML
  # =========================
  paste0(
    .mzHtmlStyles(),
    .mzHtmlAccordionStart(title, badge = equiv_badge),
    
    # Hypotheses & Alpha
    .("<div style='background:#f8f9fa; border-left:4px solid "), jamovi_blue, "; padding:10px 14px; margin-bottom:15px; border-radius:4px; font-size:0.9em;'>",
    "<div style='display:flex; justify-content:space-between; align-items:flex-start;'>",
    "<div>",
    "<div style='font-weight:bold; color:#555; margin-bottom:4px; text-transform:uppercase; font-size:0.85em;'>Hypotheses</div>",
    "<div><b>H<sub>0</sub></b>: ES &le; ", fmt_b(lower), " OR ES &ge; ", fmt_b(upper), "</div>",
    "<div><b>H<sub>1</sub></b>: ", fmt_b(lower), " &lt; ES &lt; ", fmt_b(upper), "</div>",
    "</div>",
    "<div style='padding:2px 8px; background:#f0f4f8; border:1px solid #d0d7de; border-radius:4px; font-size:0.85em; color:#555;'>&alpha; = ", fmt_s(alpha, 3), "</div>",
    "</div>",
    "</div>",
    
    # SEZIONE 1: effetto e limiti, disposti in orizzontale
    "<div style='margin-bottom:20px;'>",
    .("<div class='mz-header'>Effect Size &amp; Equivalence Bounds</div>"),
    "<table class='mz-table' style='width:100%; table-layout:fixed;'>",
    "<thead><tr>",
    .("<th style='text-align:center;'>Pooled ES</th>"),
    "<th style='text-align:center;'>Std. Error (SE)</th>",
    .("<th style='text-align:center;'>Lower Bound (d<sub>L</sub>)</th>"),
    .("<th style='text-align:center;'>Upper Bound (d<sub>U</sub>)</th>"),
    "</tr></thead>",
    "<tbody><tr>",
    "<td style='text-align:center;'>", fmt_s(tost$ES), "</td>",
    "<td style='text-align:center;'>", fmt_s(SE), "</td>",
    "<td style='text-align:center;'>", fmt_b(tost$low_eqbound_d), "</td>",
    "<td style='text-align:center;'>", fmt_b(tost$high_eqbound_d), "</td>",
    "</tr></tbody>",
    "</table>",
    "</div>",
    "<div style='display:flex; gap:30px; margin-bottom:15px; flex-wrap:wrap;'>",
    
    # SEZIONE UNIFICATA: risultati statistici TOST e NHST
    "<div style='flex:1; min-width:300px;'>",
    .("<div class='mz-header'>Statistical Test Results</div>"),
    "<table class='mz-table'>",
    .("<thead><tr><th style='text-align:left; width:50%;'>Test Type</th><th style='text-align:right; width:25%;'>Z-value</th><th style='text-align:right; width:25%;'>p-value</th></tr></thead>"),
    "<tbody>",
    .("<tr><td>Lower bound test (d<sub>L</sub>)</td><td style='text-align:right;'>"), fmt_s(tost$TOST_Z1), "</td><td style='text-align:right;'>", fmt_p(tost$TOST_p1), "</td></tr>",
    .("<tr><td>Upper bound test (d<sub>U</sub>)</td><td style='text-align:right;'>"), fmt_s(tost$TOST_Z2), "</td><td style='text-align:right;'>", fmt_p(tost$TOST_p2), "</td></tr>",
    .("<tr><td>Zero null test (NHST) "), nhst_badge, "</td><td style='text-align:right;'>", fmt_s(NHST_Z), "</td><td style='text-align:right;'>", fmt_p(tost$NHST_p), "</td></tr>",
    "</tbody>",
    "</table>",
    "</div>",
    
    # RIGA 2, COLONNA 2: intervalli di confidenza
    "<div style='flex:1; min-width:300px;'>",
    .("<div class='mz-header'>Confidence Intervals</div>"),
    "<table class='mz-table'>",
    .("<thead><tr><th style='text-align:left; width:50%;'>Interval Type</th><th style='text-align:right; width:25%;'>Lower Limit</th><th style='text-align:right; width:25%;'>Upper Limit</th></tr></thead>"),
    "<tbody>",
    .("<tr><td>TOST CI ("), 100 * (1 - alpha * 2), "%)</td><td style='text-align:right;'>", fmt_s(tost$LL_CI_TOST), "</td><td style='text-align:right;'>", fmt_s(tost$UL_CI_TOST), "</td></tr>",
    .("<tr><td>NHST CI ("), 100 * (1 - alpha), "%)</td><td style='text-align:right;'>", fmt_s(tost$LL_CI_ZTEST), "</td><td style='text-align:right;'>", fmt_s(tost$UL_CI_ZTEST), "</td></tr>",
    "</tbody>",
    "</table>",
    "</div>",
    "</div>",
    
    # Interpretazione
    .("<div style='margin-top:5px; padding:12px; background:#e7f3ff; border-left:4px solid "), jamovi_blue, "; border-radius:4px; font-size:0.95em;'>",
    .("<div style='font-weight:bold; color:"), jamovi_blue, "; margin-bottom:5px;'>Interpretation</div>",
    "<div>", combined_msg, "</div>",
    "</div>",
    .mzHtmlAccordionEnd()
  )
}

mzHtmlTes <- function(res, H0 = 0, alternative = "greater", alpha = 0.05, test = "chi2", self=NULL) {
  self <- .mzTranslationSelf(self)
  # Esecuzione metafor::tes con gestione errori
  restes <- try(
    metafor::tes(
      x = res,
      theta = H0,
      alternative = alternative,
      alpha = alpha,
      test = test
    ),
    silent = TRUE
  )
  
  if (inherits(restes, "try-error")) {
    mark('[HTML/TES] failed',.mzLogErrorText(restes),ui_log_obj=self$results$debugLog, .loc = "mzHtmlService.R:1119")
    return(paste0("<div class='jmv-error'>", htmlSpecialChars(conditionMessage(attr(restes, "condition"))), "</div>"))
  }
  
  fmt <- function(x, d = 3) if (is.null(x) || is.na(x)) "NA" else formatC(x, d, format = "f")
  fmt_aligned <- function(x, d = 2) formatC(x, digits = d, format = "f")
  
  # Etichette delle ipotesi alternative
  alt_label <- switch(alternative,
                      "greater" = "greater",
                      "less" = "less",
                      "two.sided" = "two-sided",
                      alternative
  )
  
  # Info sul test usato
  test_stat <- switch(test,
                      "chi2" = paste0("&chi;<sup>2</sup> = ", fmt(restes$X2, 4), ", df = 1"),
                      "binom" = .("Binomial test"),
                      "exact" = .("Exact test"),
                      "z" = if (!is.null(restes$z)) paste0("z = ", fmt(restes$z, 4)) else "z test",
                      paste0("Test: ", test)
  )
  
  pval_val <- as.numeric(restes$pval)
  pval <- if (!is.na(pval_val) && pval_val < .001) {
    "<span class='p-red'>&lt; .001</span>"
  } else {
    fmt(pval_val, 3)
  }
  
  pw <- summary(restes$power)
  power_row <- paste0(
    "<tr>",
    "<td style='text-align:center; padding:3px 8px;'>", fmt(pw["Min."], 3), "</td>",
    "<td style='text-align:center; padding:3px 8px;'>", fmt(pw["1st Qu."], 3), "</td>",
    "<td style='text-align:center; padding:3px 8px;'>", fmt(pw["Median"], 3), "</td>",
    "<td style='text-align:center; padding:3px 8px;'>", fmt(pw["3rd Qu."], 3), "</td>",
    "<td style='text-align:center; padding:3px 8px;'>", fmt(pw["Max."], 3), "</td>",
    "</tr>"
  )
  
  theta_limit_html <- paste0(.("<div style='margin-top:8px;'>Based on theta = "), fmt(restes$theta, 4))
  if (!is.null(restes$theta.lim)) {
    theta_limit_html <- paste0(
      theta_limit_html, .("<br>Limit estimate (theta<sub>lim</sub>): "),
      fmt(restes$theta.lim, 4), " (where p = ", restes$tes.alpha, ")"
    )
  }
  theta_limit_html <- paste0(theta_limit_html, "</div>")
  
  paste0(
    .mzHtmlStyles(),
    .mzHtmlAccordionStart(.("Test for Excess Significance")),
    "<div style='display:flex; gap:40px; margin-bottom:12px;'>",
    "<div style='flex:0 0 auto;'>",
    .("<div class='mz-header'>Significant Findings</div>"),
    "<table class='mz-table'>",
    .("<tr><td style='padding:3px 12px 3px 0;'>(<b>O</b>)bserved number</td><td style='padding:3px 0;'>"), fmt_aligned(restes$O, 0), "</td></tr>",
    .("<tr><td style='padding:3px 12px 3px 0;'>(<b>E</b>)xpected number</td><td style='padding:3px 0;'>"), fmt_aligned(restes$E, 2), "</td></tr>",
    "<tr><td style='padding:3px 12px 3px 0;'>Ratio O / E</td><td style='padding:3px 0;'>", fmt_aligned(restes$OEratio, 2), "</td></tr>",
    "</table>",
    "</div>",
    "<div style='flex:1;'>",
    .("<div class='mz-header'>Estimated Power of Tests</div>"),
    "<table class='mz-table' style='width:100%; text-align:center;'>",
    "<thead><tr><th>Min</th><th>Q<sub>1</sub></th><th>Median</th><th>Q<sub>3</sub></th><th>Max</th></tr></thead>",
    "<tbody>",
    power_row,
    "</tbody>",
    "</table>",
    theta_limit_html,
    "</div>",
    "</div>",
    "<div class='mz-footer' style='border-color:#ccc; color:#555;'>",
    "H<sub>0</sub>: &theta; = ", H0, " &nbsp;|&nbsp; Alternative: ", alt_label, " &nbsp;|&nbsp; &alpha; = ", alpha,
    "<div style='margin-top:8px; color:#444;'>Test: ", test_stat, ", p = ", pval, "</div>",
    "</div>",
    .mzHtmlAccordionEnd()
  )
}

mzHtmlPuniform <- function(res, options) {
  # Contesto esplicito per .(), anche nelle chiamate R prive dell'oggetto analisi.
  self <- list(options=if (is.function(options$translate)) options else
                 list(translate=function(text,n=1)text))
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # Estrai opzioni (sempre presenti in jamovi)
  side <- options$puniformSide
  method <- options$methodPunif
  alpha <- options$alphaPunif
  # La libreria restituisce CI al 95%; alpha regola la selezione degli studi.
  conf_level <- 95
  est <- options$modelest
  mea <- options$modelmea
  
  # === Gestione Errore ===
  if (!is.null(res$error) && res$error == TRUE) {
    error_msg <- res$message %||% .("p-uniform analysis failed")
    side_label <- if (side == "left") "Left-tailed" else "Right-tailed"
    
    # SVG animato con frecce left/right
    svg_icon <- paste0(
      "<svg width='80' height='80' viewBox='0 0 80 80' xmlns='http://www.w3.org/2000/svg'>",
      "  <defs>",
      "    <style>",
      "      @keyframes pulse-", side, " {",
      "        0%, 100% { opacity: 0.3; transform: scale(0.95); }",
      "        50% { opacity: 1; transform: scale(1.05); }",
      "      }",
      "      .arrow-", side, " { animation: pulse-", side, .(" 2s ease-in-out infinite; }"),
      "      .arrow-other { opacity: 0.15; }",
      "    </style>",
      "  </defs>",
      # Cerchio di sfondo
      "  <circle cx='40' cy='40' r='35' fill='#fff3e0' stroke='#ff9800' stroke-width='2'/>",
      # Freccia SINISTRA
      "  <g class='", if (side == "left") "arrow-left" else "arrow-other", "'>",
      "    <path d='M 25 40 L 35 30 L 35 36 L 45 36 L 45 44 L 35 44 L 35 50 Z' fill='#2196f3'/>",
      "  </g>",
      # Freccia DESTRA
      "  <g class='", if (side == "right") "arrow-right" else "arrow-other", "'>",
      "    <path d='M 55 40 L 45 30 L 45 36 L 35 36 L 35 44 L 45 44 L 45 50 Z' fill='#f44336'/>",
      "  </g>",
      # Linea centrale
      "  <line x1='40' y1='25' x2='40' y2='55' stroke='#999' stroke-width='1' stroke-dasharray='2,2'/>",
      "</svg>"
    )
    
    return(paste0(
      .mzHtmlStyles(),
      .mzHtmlAccordionStart("Method p-uniform"),
      "<div style='display:flex; align-items:center; justify-content:center; min-height:180px; padding:20px;'>",
      "  <div style='text-align:center; max-width:500px;'>",
      # SVG animato
      "    <div style='margin-bottom:20px;'>", svg_icon, "</div>",
      # Messaggio principale
      "    <div style='color:#666; font-size:1em; line-height:1.5; margin-bottom:15px;'>",
      "      ", htmlSpecialChars(error_msg),
      "    </div>",
      # Badge con side
      "    <div style='display:inline-flex; align-items:center; gap:8px; padding:8px 16px; background:#fff3e0; border:1px solid #ff9800; border-radius:20px;'>",
      "      <span style='font-size:1.2em;'>", if (side == "left") "\u2190" else "\u2192", "</span>",
      "      <span style='color:#e65100; font-weight:600; font-size:0.9em;'>", side_label, " Analysis</span>",
      "    </div>",
      # Suggerimento
      "    <div style='margin-top:15px; font-size:0.85em; color:#999;'>",
      .("      Try switching to "), if (side == "left") "right-tailed" else "left-tailed", .(" analysis or check your data for significant effects"),
      "    </div>",
      "  </div>",
      "</div>",
      .mzHtmlAccordionEnd()
    ))
  }
  
  # === Mapping Metodo ===
  methodLabel <- switch(method,
                        "P"       = .("Irwin-Hall distribution"),
                        "ML"      = .("Maximum Likelihood"),
                        "LNP"     = .("Fisher's method LNP"),
                        "LN1MINP" = "Fisher's method 1-p transf",
                        "KS"      = .("Kolmogorov-Smirnov test"),
                        "AD"      = .("Anderson-Darling test"),
                        method
  )
  
  # CI Label
  ci_label <- paste0(conf_level, "% CI")
  
  # Info Modello Base
  mod_info <- if (est == "FE") .("Fixed-Effects Model") else paste0(.("Random-Effects Model (Estimator: "), est, ")")
  
  # Label Misura
  mea_label <- switch(mea,
                      "GEN"   = .("Generic Effect Size"),
                      "COR"   = "Correlation",
                      "UCOR"  = .("Unbiased Correlation"),
                      "ZCOR"  = .("Fisher's z Transformed Correlation"),
                      "MD"    = .("Mean Difference"),
                      "SMD"   = .("Standardized Mean Difference"),
                      "SMDH"  = .("Standardized Mean Difference (heteroscedastic)"),
                      "SMD1"  = .("Standardized Mean Difference (using SD1)"),
                      "SMD1H" = .("Standardized Mean Difference (using SD1, heteroscedastic)"),
                      "ROM"   = .("Log Ratio of Means"),
                      mea
  )
  
  # === Helper Formattazione ===
  fmt <- function(x, d = 3) if (is.null(x) || is.na(x)) "NA" else formatC(x, d, format = "f")
  
  fmt_p <- function(p) {
    if (is.null(p) || is.na(p)) {
      return("NA")
    }
    p_val <- as.numeric(p)
    if (p_val < .001) {
      return("<span class='p-red'>&lt; .001</span>")
    }
    class <- if (p_val < 0.05) "p-red" else "p-blue"
    paste0(.("<span class='"), class, "'>", formatC(p_val, 3, format = "f"), "</span>")
  }
  
  # === OUTPUT HTML ===
  paste0(
    .mzHtmlStyles(),
    .mzHtmlAccordionStart("Method p-uniform"),
    "<div style='display:flex; gap:30px; margin-bottom:12px;'>",
    "<div style='flex:1; display:flex; flex-direction:column;'>",
    .("<div class='mz-header'>Publication Bias Test</div>"),
    "<table class='mz-table'>",
    .("<tr><td style='width:1px; white-space:nowrap;'>Test statistic (L<sub>pb</sub>)</td><td style='text-align:right;'>"), fmt(res$L.pb, 3), "</td></tr>",
    "<tr><td>p-value</td><td style='text-align:right;'>", fmt_p(res$pval.pb), "</td></tr>",
    "</table>",
    "<div style='text-align:center; flex-grow:1; display:flex; align-items:flex-end; justify-content:center;'>",
    .("<div class='mz-badge'>Significant studies: "), (res$ksig %||% "NA"), "</div>",
    "</div>",
    "</div>",
    "<div style='flex:1;'>",
    .("<div class='mz-header'>Effect Size Estimation</div>"),
    "<table class='mz-table'>",
    .("<tr><td style='width:1px; white-space:nowrap;'>Estimate</td><td style='width:1px;'></td><td style='text-align:right;'>"), fmt(res$est, 3), "</td></tr>",
    "<tr>",
    "<td rowspan='2' style='color:#555; white-space:nowrap; width:1px; padding-right:5px;'>", ci_label, "</td>",
    "<td style='width:1px; white-space:nowrap; padding-left:0; padding-right:10px;'>LL</td>",
    "<td style='text-align:right;'>", fmt(res$ci.lb, 3), "</td>",
    "</tr>",
    "<tr>",
    "<td style='width:1px; white-space:nowrap; padding-left:0; padding-right:10px;'>UL</td>",
    "<td style='text-align:right;'>", fmt(res$ci.ub, 3), "</td>",
    "</tr>",
    .("<tr><td colspan='2' style='white-space:nowrap;'>Test of Effect (L<sub>0</sub>)</td><td style='text-align:right;'>"), fmt(res$L.0, 3), "</td></tr>",
    "<tr><td colspan='2'>p-value</td><td style='text-align:right;'>", fmt_p(res$pval.0), "</td></tr>",
    "</table>",
    "</div>",
    "</div>",
    "<div class='mz-footer'>",
    "<div style='color:#3e6da9; margin-bottom:6px;'>",
    sub("95%", "<strong>95%</strong>", .("Confidence intervals are fixed at 95% by the p-uniform library; alpha controls study selection."), fixed=TRUE),
    "</div>",
    if (!is.null(res$ksig) && res$ksig < 3) {
      .("<div style='color:#a6611a; margin-bottom:8px;'><b>Warning:</b> p-uniform results may be unreliable with fewer than 3 significant studies.</div>")
    } else {
      ""
    },
    .("<div><b>p-uniform parameters:</b> Method ["), methodLabel, "] &nbsp;|&nbsp; Alpha [", alpha, "] &nbsp;|&nbsp; Direction [", side, "]</div>",
    .("<div style='margin-top:4px;'><b>Base Model:</b> "), mod_info, "</div>",
    if (mea_label != "") paste0("<div style='margin-top:4px;'><b>Measure:</b> ", mea_label, "</div>") else "",
    "</div>",
    .mzHtmlAccordionEnd()
  )
}

mzHtmlSelmodel <- function(sel_res, options, mod_info = "", mea_label = "") {
  self <- list(options=if (is.function(options$translate)) options else
                 list(translate=function(text,n=1)text))
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # === Helper Formattazione ===
  fmt <- function(x, d = 3) if (is.null(x) || is.na(x)) "" else formatC(x, d, format = "f")
  
  fmt_p <- function(p) {
    if (is.null(p) || is.na(p)) {
      return("")
    }
    p_val <- as.numeric(p)
    if (p_val < .001) {
      return("<span class='p-red'>&lt; .001</span>")
    }
    class <- if (p_val < 0.05) "p-red" else "p-blue"
    paste0(.("<span class='"), class, "'>", formatC(p_val, 3, format = "f"), "</span>")
  }
  
  # === Estrazione Parametri ===
  # 1. Stima aggiustata (Z con precisione 3)
  adj_row <- paste0(
    "<tr>",
    .("<td><b>Adjusted Estimate</b></td>"),
    "<td style='text-align:right;'>", fmt(sel_res$b[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$se[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$zval[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt_p(sel_res$pval[1]), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$ci.lb[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$ci.ub[1], 3), "</td>",
    "</tr>"
  )
  
  # 2. Parametri di selezione (delta)
  s_sel <- try(summary(sel_res), silent = TRUE)
  if (inherits(s_sel, "try-error") || !is.list(s_sel)) s_sel <- sel_res
  
  deltas <- s_sel$delta
  se_deltas <- s_sel$se.delta
  z_deltas <- s_sel$zval.delta
  p_deltas <- s_sel$pval.delta
  lb_deltas <- s_sel$ci.lb.delta
  ub_deltas <- s_sel$ci.ub.delta
  
  # Controlla stepfun e recupera le soglie
  is_stepfun <- as.character(options$selModelType %||% "") == "stepfun"
  steps <- if (is_stepfun && !is.null(sel_res$steps)) sel_res$steps else NULL
  
  # Calcola il numero di studi per intervallo e le etichette per stepfun
  k_counts <- NULL
  step_labels <- NULL
  show_k <- FALSE
  
  # Controlla prima ptable, come richiesto dall'utente
  # FIX: Gestisce correttamente ptable come data.frame da metafor::selmodel
  if (!is.null(sel_res$ptable)) {
    ptable <- sel_res$ptable
    
    # Caso 1: ptable è un data.frame (struttura standard di metafor)
    if (is.data.frame(ptable)) {
      if ("k" %in% names(ptable)) {
        k_counts <- ptable$k # Estrae il vettore dalla colonna
        show_k <- TRUE
      }
      # Estrai le etichette dai rownames
      step_labels <- rownames(ptable)
    }
    # Caso 2: ptable è una lista con elemento k
    else if (is.list(ptable) && !is.null(ptable$k)) {
      k_counts <- ptable$k
      show_k <- TRUE
      if (!is.null(names(ptable))) {
        step_labels <- names(ptable)
      }
    }
  }
  # Senza ptable non si inventano conteggi dai p-value dei coefficienti.
  # Le soglie possono già contenere 1: elimina duplicati prima di creare le etichette.
  else if (is_stepfun && !is.null(steps)) {
    cutpoints <- sort(unique(c(0, steps, 1)))
    if (length(cutpoints) - 1L == length(deltas)) {
      step_labels <- vapply(seq_along(deltas), function(i)
        paste0(formatC(cutpoints[i], format='g'), ' < p <= ',
               formatC(cutpoints[i+1L], format='g')), character(1))
    }
  }

  if (is.null(step_labels)) {
    if (!is.null(s_sel$labels)) {
      step_labels <- s_sel$labels
    } else if (!is.null(rownames(deltas))) {
      step_labels <- rownames(deltas)
    } else {
      step_labels <- names(deltas)
    }
  }
  
  # Aggiorna adj_row per colonna k se necessario
  adj_k_cell <- if (show_k) "<td></td>" else ""
  adj_row <- paste0(
    "<tr>",
    .("<td><b>Adjusted Estimate</b></td>"),
    adj_k_cell,
    "<td style='text-align:right;'>", fmt(sel_res$b[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$se[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$zval[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt_p(sel_res$pval[1]), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$ci.lb[1], 3), "</td>",
    "<td style='text-align:right;'>", fmt(sel_res$ci.ub[1], 3), "</td>",
    "</tr>"
  )
  
  delta_rows <- ""
  if (length(deltas) > 0) {
    for (i in seq_along(deltas)) {
      row_name <- if (!is.null(step_labels) && !is.na(step_labels[i]) && step_labels[i] != "") {
        step_labels[i]
      } else if (is_stepfun) {
        sprintf(.("Interval %s"), i)
      } else {
        paste0(.("Selection Parameter (&delta;<sub>"), i, "</sub>)")
      }
      
      k_val <- if (!is.null(k_counts) && i <= length(k_counts)) k_counts[i] else NA
      k_cell <- if (show_k) paste0("<td style='text-align:right;'>", fmt(k_val, 0), "</td>") else ""
      
      delta_rows <- paste0(
        delta_rows,
        "<tr>",
        "<td>", row_name, "</td>",
        k_cell,
        "<td style='text-align:right;'>", fmt(deltas[i], 3), "</td>",
        "<td style='text-align:right;'>", fmt(se_deltas[i], 3), "</td>",
        "<td style='text-align:right;'>", fmt(z_deltas[i], 3), "</td>",
        "<td style='text-align:right;'>", fmt_p(p_deltas[i]), "</td>",
        "<td style='text-align:right;'>", fmt(lb_deltas[i], 3), "</td>",
        "<td style='text-align:right;'>", fmt(ub_deltas[i], 3), "</td>",
        "</tr>"
      )
    }
  }
  
  # === OUTPUT HTML ===
  conf_level <- as.numeric(options$level %||% 95)
  ci_header <- paste0(conf_level, "% CI")
  sel_type <- as.character(options$selModelType %||% "unknown")
  
  # Mappa descrittiva per il footer
  sel_desc <- switch(sel_type,
                     "halfnorm" = .("Half-Normal selection model"),
                     "beta"     = .("Beta selection model"),
                     "negexp"   = .("Negative exponential selection model"),
                     "logistic" = .("Logistic selection model"),
                     "power"    = .("Power selection model"),
                     "stepfun"  = .("Step function selection model"),
                     sel_type
  )
  
  # Header K se necessario
  header_k <- if (show_k) "<th rowspan='2' style='text-align:right; vertical-align:bottom; border-bottom: 1px solid #222;'>k</th>" else ""
  
  paste0(
    .mzHtmlStyles(),
    .mzHtmlAccordionStart(sprintf(.("Selection Model: %s"), sel_type)),
    "<div style='overflow-x:auto;'>",
    "<table class='mz-table'>",
    "<thead>",
    "<tr>",
    "<th rowspan='2' style='text-align:left; vertical-align:bottom; border-bottom: 1px solid #222;'>Parameter</th>",
    header_k,
    .("<th rowspan='2' style='text-align:right; vertical-align:bottom; border-bottom: 1px solid #222;'>Estimate</th>"),
    "<th rowspan='2' style='text-align:right; vertical-align:bottom; border-bottom: 1px solid #222;'>SE</th>",
    "<th rowspan='2' style='text-align:right; vertical-align:bottom; border-bottom: 1px solid #222;'>Z</th>",
    "<th rowspan='2' style='text-align:right; vertical-align:bottom; border-bottom: 1px solid #222;'>p-value</th>",
    "<th colspan='2' style='text-align:center; border-bottom: 1px solid #222;'>", ci_header, "</th>",
    "</tr>",
    "<tr>",
    "<th style='text-align:right;'>LL</th>",
    "<th style='text-align:right;'>UL</th>",
    "</tr>",
    "</thead>",
    "<tbody>",
    adj_row,
    delta_rows,
    "</tbody>",
    "</table>",
    "</div>",
    "<div class='mz-footer' style='margin-top:15px;'>",
    if (length(sel_res$se) && all(is.na(sel_res$se))) paste0(
      "<div>", .("Uncertainty estimates are unavailable for this selection fit. Review the convergence warnings before interpreting the results."), "</div>"
    ) else "",
    .("<div><b>Selection Model:</b> "), sel_desc, "</div>",
    .("<div><b>Base Model:</b> "), mod_info, "</div>",
    if (mea_label != "") paste0("<div><b>Measure:</b> ", mea_label, "</div>") else "",
    "</div>",
    .mzHtmlAccordionEnd()
  )
}

### -- Genera tabella HTML per Leave-One-Out
mzHtmlLeave1OutTable <- function(l1oDf, res, options, sortOrder = "none",
                                 statLabel = "z") {
  self <- .mzTranslationSelf(options=options)
  if (is.null(l1oDf) || nrow(l1oDf) == 0) {
    return(.("<div class='jmv-error'>Leave-One-Out data not available (moderators present or calculation failed).</div>"))
  }
  
  ciLevel <- if (!is.null(options$level)) options$level else 95
  
  l1oDf <- .mzSortLeaveOneOut(l1oDf,sortOrder,isTRUE(options$leave1outGroupOrder))
  groups <- attr(l1oDf,'mz_loo_groups',exact=TRUE)
  if(!is.null(groups))statLabel <- .("Statistic")
  singleRemaining <- !is.null(groups) && any(vapply(groups,function(g)g$end-g$start+1L==2L,logical(1)))
  dispDf <- data.frame(
    Study    = l1oDf$slab,
    Estimate = l1oDf$estimate,
    SE       = l1oDf$se,
    Stat     = l1oDf$stat,
    Lower    = l1oDf$ci.lb,
    Upper    = l1oDf$ci.ub,
    p        = l1oDf$pval,
    Tau2     = l1oDf$tau2,
    Tau      = sqrt(l1oDf$tau2),
    ISqu     = l1oDf$I2,
    HSqu     = l1oDf$H2,
    Q        = l1oDf$QE,
    Qp       = l1oDf$QEp,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  remaining <- rep(res$k-1L,nrow(l1oDf))
  if(!is.null(groups))for(g in groups)remaining[seq.int(g$start,g$end)] <- g$end-g$start
  dispDf[remaining<2L,c('Tau2','Tau','ISqu','HSqu','Q','Qp')] <- NA_real_
  if(!is.null(groups))dispDf <- cbind(Subgroup=l1oDf$subgroup,dispDf)
  headers <- c("Study", .("Estimate"), "SE", statLabel, "LL", "UL", "p",
               "\u03C4\u00B2", "\u03C4", "I\u00B2 (%)", "H\u00B2", "Q", "p(Q)")
  
  if(!is.null(groups))headers <- c(.("Subgroup"),headers)
  offset <- if(is.null(groups))0L else 1L
  supertitles <- list(
    list(label = paste0("[", ciLevel, "%] CI"), cols = c(5, 6)+offset)
  )
  
  # Evidenzia le righe "influenti" (CI post-rimozione non contiene la stima
  # pooled originale) con uno sfondo tenue su tutta la riga
  influentialRows <- which(l1oDf$influential %in% TRUE)
  regionShade <- NULL
  if (length(influentialRows) > 0) {
    regionShade <- lapply(influentialRows, function(r) {
      list(row_start = r, row_end = r, col_start = 1, col_end = ncol(dispDf), color = "#FFF3E0")
    })
  }
  
  colAlignVec <- rep("center", ncol(dispDf))
  colAlignVec[seq_len(offset+1L)] <- "left"
  
  htmlTable <- mzHtmlTableBuilder(
    title = .("Leave-One-Out Analysis"),
    headers = headers,
    data = dispDf,
    groupingColumns = if(is.null(groups))NULL else "Subgroup",
    supertitles = supertitles,
    colAlign = colAlignVec,
    regionShade = regionShade,
    footnote = paste0(
      .("Rows highlighted indicate influential studies: removing them shifts the "),
      ciLevel, .("% CI outside the original pooled estimate."),
      if(!is.null(groups))paste0(' ',.("Each study is omitted from its own subgroup, and the model is refitted using only the remaining studies in that subgroup. Comparisons refer to the original subgroup pooled estimate."),
          ' ',.("At least two studies are required in each subgroup. Results that cannot be estimated with the selected test are shown as missing."),
          if(singleRemaining)paste0(' ',.("When one study remains after omission, metafor uses z inference, even if t or Knapp-Hartung was selected; heterogeneity is not estimable from a single study.")) else '') else '' 
    )
  )
  
  return(htmlTable)
}

### --- Info service ------------------------------------------------------ ###
mzCheckAnalysisReady <- function(self, options, anatype) {
  ready <- TRUE
  
  tryCatch(
    {
      # Controllo tutte le opzioni richieste
      for (opts in options) {
        if (is.null(self$options[[opts]])) {
          ready <- FALSE
          break
        }
      }
      
      if (!ready) {
        # Mostra istruzioni se mancano opzioni
        if (!is.null(self$results$instructions)) {
          html_content <- switch(anatype,
                                 "correlation" = .htmlInfoCorrelation(self=self),
                                 "dichotomous" = .htmlInfoDichotomous(self=self),
                                 "effectsize"  = .htmlInfoEffectsize(self=self),
                                 "meandiff"    = .htmlInfoMeandiff(self=self),
                                 "proportion"  = .htmlInfoProportion(self=self),
                                 "alpha"       = .htmlInfoAlpha(self=self),
                                 .htmlInfoCorrelation(self=self) # Predefinito
          )
          self$results$instructions$setVisible(TRUE)
          self$results$instructions$setContent(html_content)
        }
        return(FALSE)
      }
      
      # Nascondi istruzioni se analisi pronta
      if (!is.null(self$results$instructions)) {
        self$results$instructions$setVisible(FALSE)
      }
      
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

#' Template CSS comune per instructions
#' @return stringa CSS
.getInfoCss <- function() {
  return("
        <style>
            body {font-family: Arial, sans-serif; margin: 0; padding: 0;}
            .instructions {padding: 20px; border: 1px solid #ddd; border-radius: 8px; background-color: #fafafa; margin: 10px;}
            .instructions h3 {color: #333; margin-top: 0; margin-bottom: 15px; font-size: 1.2em;}
            .instructions p {margin: 10px 0; line-height: 1.5;}
            .instructions ul {margin: 10px 0; padding-left: 20px;}
            .instructions li {margin: 5px 0;}
            .required {color: #d32f2f; font-weight: bold;}
            .optional {color: #1976d2; font-weight: bold;}
            .divider {border-top: 1px solid #eee; margin: 15px 0;}
            .note {background-color: #e3f2fd; padding: 10px; border-left: 4px solid #2196f3; margin: 10px 0;}
            .warning {background-color: #fff3e0; padding: 10px; border-left: 4px solid #ff9800; margin: 10px 0;}
        </style>
    ")
}

#' Genera HTML instructions per analisi correlazioni
#' @return stringa HTML completa
.htmlInfoCorrelation <- function(self=NULL) {
  self <- .mzTranslationSelf(self)
  css <- .getInfoCss()
  
  html_content <- paste0(css, .("
        <body>
            <div class='instructions'>
                <h3>Getting Started with Correlation Meta-Analysis</h3>
                <p>Please select the following <span class='required'>required</span> variables:</p>
                <ul>
                    <li><span class='required'>Study Label</span>: A descriptive label for each study; distinct labels make studies easier to recognize</li>
                    <li><span class='required'>Correlations</span>: Correlation coefficients from each study</li>
                    <li><span class='required'>Sample Sizes</span>: Number of participants in each study</li>
                </ul>

                <p><span class='optional'>Subgroup Analysis</span> and <span class='optional'>Moderators</span> are optional components:</p>
                <ul>
                    <li>In the data editor, set the measure type to <b>Continuous</b> for covariates or <b>Nominal/Ordinal</b> for factors.</li>
                    <li>Numeric codes are treated as factors when their measure type is <b>Nominal</b> or <b>Ordinal</b>.</li>
                </ul>

                <div class='note'>
                    <strong>Note:</strong> Correlation coefficients should be between -1 and 1.
                </div>

                <div class='divider'></div>
                <p>Feature requests and bug reports can be made on our <a href='https://github.com/MAgojam/Metafora/issues' target='_blank'>GitHub</a>.</p>
            </div>
        </body>
    "))
  
  return(html_content)
}

#' Genera HTML instructions per analisi dicotomiche
#' @return stringa HTML completa
.htmlInfoDichotomous <- function(self=NULL) {
  self <- .mzTranslationSelf(self)
  css <- .getInfoCss()
  
  html_content <- paste0(css, .("
        <body>
            <div class='instructions'>
                <h3>Getting Started with Dichotomous Meta-Analysis</h3>
                <p>Please select the following <span class='required'>required</span> variables:</p>
                <ul>
                    <li><span class='required'>Study Label</span>: A descriptive label for each study; distinct labels make studies easier to recognize</li>
                    <li><span class='required'>Experimental Group</span>:
                        <ul>
                            <li>Number of positive outcomes (events)</li>
                            <li>Number of negative outcomes (non-events)</li>
                        </ul>
                    </li>
                    <li><span class='required'>Control Group</span>:
                        <ul>
                            <li>Number of positive outcomes (events)</li>
                            <li>Number of negative outcomes (non-events)</li>
                        </ul>
                    </li>
                </ul>

                <p><span class='optional'>Subgroup Analysis</span> and <span class='optional'>Moderators</span> are optional components:</p>
                <ul>
                    <li>In the data editor, set the measure type to <b>Continuous</b> for covariates or <b>Nominal/Ordinal</b> for factors.</li>
                    <li>Numeric codes are treated as factors when their measure type is <b>Nominal</b> or <b>Ordinal</b>.</li>
                </ul>

                <div class='divider'></div>
                <p>Feature requests and bug reports can be made on our <a href='https://github.com/MAgojam/Metafora/issues' target='_blank'>GitHub</a>.</p>
            </div>
        </body>
    "))
  
  return(html_content)
}

#' Genera HTML instructions per analisi effect size generico
#' @return stringa HTML completa
.htmlInfoEffectsize <- function(self=NULL) {
  self <- .mzTranslationSelf(self)
  css <- .getInfoCss()
  
  html_content <- paste0(css, .("
        <body>
            <div class='instructions'>
                <h3>Getting Started with Effect Size Meta-Analysis</h3>
                <p>Please select the following <span class='required'>required</span> variables:</p>
                <ul>
                    <li><span class='required'>Study Label</span>: A descriptive label for each study; distinct labels make studies easier to recognize</li>
                    <li><span class='required'>Effect Sizes</span>: Effect size estimates from each study</li>
                    <li><span class='required'>Variance, SE or CI</span>: Select one input mode; CI requires lower and upper limits and their original confidence level</li>
                </ul>

                <p><span class='optional'>Subgroup Analysis</span> and <span class='optional'>Moderators</span> are optional components:</p>
                <ul>
                    <li>In the data editor, set the measure type to <b>Continuous</b> for covariates or <b>Nominal/Ordinal</b> for factors.</li>
                    <li>Numeric codes are treated as factors when their measure type is <b>Nominal</b> or <b>Ordinal</b>.</li>
                </ul>

                <div class='note'>
                    <strong>Note:</strong> Select Variance, SE or CI above the input boxes. CI mode approximates sampling variances assuming normal-based Wald intervals on the same scale as the supplied effects. Input CI (%) describes the original study intervals; the Model tab sets the output confidence level separately.
                </div>

                <div class='divider'></div>
                <p>Feature requests and bug reports can be made on our <a href='https://github.com/MAgojam/Metafora/issues' target='_blank'>GitHub</a>.</p>
            </div>
        </body>
    "))
  
  return(html_content)
}

#' Genera HTML instructions per analisi differenze medie
#' @return stringa HTML completa
.htmlInfoMeandiff <- function(self=NULL) {
  self <- .mzTranslationSelf(self)
  css <- .getInfoCss()
  
  html_content <- paste0(css, .("
        <body>
            <div class='instructions'>
                <h3>Getting Started with Mean Differences Meta-Analysis</h3>
                <p>Please select the following <span class='required'>required</span> variables:</p>
                <ul>
                    <li><span class='required'>Study Label</span>: A descriptive label for each study; distinct labels make studies easier to recognize</li>
                    <li><span class='required'>Group 1</span>: Sample size, mean, and standard deviation</li>
                    <li><span class='required'>Group 2</span>: Sample size, mean, and standard deviation</li>
                </ul>

                <p><span class='optional'>Subgroup Analysis</span> and <span class='optional'>Moderators</span> are optional components:</p>
                <ul>
                    <li>In the data editor, set the measure type to <b>Continuous</b> for covariates or <b>Nominal/Ordinal</b> for factors.</li>
                    <li>Numeric codes are treated as factors when their measure type is <b>Nominal</b> or <b>Ordinal</b>.</li>
                </ul>

                <div class='warning'>
                    <strong>Important:</strong> All sample sizes, means, and standard deviations must be positive values.
                </div>

                <div class='divider'></div>
                <p>Feature requests and bug reports can be made on our <a href='https://github.com/MAgojam/Metafora/issues' target='_blank'>GitHub</a>.</p>
            </div>
        </body>
    "))
  
  return(html_content)
}

#' Genera HTML instructions per analisi proporzioni
#' @return stringa HTML completa
.htmlInfoProportion <- function(self=NULL) {
  self <- .mzTranslationSelf(self)
  css <- .getInfoCss()
  
  html_content <- paste0(css, .("
        <body>
            <div class='instructions'>
                <h3>Getting Started with Proportions Meta-Analysis</h3>
                <p>Please select the following <span class='required'>required</span> variables:</p>
                <ul>
                    <li><span class='required'>Study Label</span>: A descriptive label for each study; distinct labels make studies easier to recognize</li>
                    <li><span class='required'>Frequencies of the Event</span>: Number of participants experiencing the event</li>
                    <li><span class='required'>Total Sample Sizes</span>: Total number of participants in each study</li>
                </ul>

                <p><span class='optional'>Subgroup Analysis</span> and <span class='optional'>Moderators</span> are optional components:</p>
                <ul>
                    <li>In the data editor, set the measure type to <b>Continuous</b> for covariates or <b>Nominal/Ordinal</b> for factors.</li>
                    <li>Numeric codes are treated as factors when their measure type is <b>Nominal</b> or <b>Ordinal</b>.</li>
                </ul>

                <div class='note'>
                    <strong>Note:</strong> The number of events should not exceed the total sample size for any study.
                </div>

                <div class='divider'></div>
                <p>Feature requests and bug reports can be made on our <a href='https://github.com/MAgojam/Metafora/issues' target='_blank'>GitHub</a>.</p>
            </div>
        </body>
    "))
  
  return(html_content)
}

#' Genera HTML instructions per analisi alpha/reliability
#' @return stringa HTML completa
.htmlInfoAlpha <- function(self=NULL) {
  self <- .mzTranslationSelf(self)
  css <- .getInfoCss()
  
  html_content <- paste0(css, .("
        <body>
            <div class='instructions'>
                <h3>Getting Started with Reliability Generalization</h3>
                <p>Please select the following <span class='required'>required</span> variables:</p>
                <ul>
                    <li><span class='required'>Study Label</span>: A descriptive label for each study; distinct labels make studies easier to recognize</li>
                    <li><span class='required'>Alpha Values</span>: Cronbach's alpha coefficients</li>
                    <li><span class='required'>Number of Items</span>: Number of items in each scale</li>
                    <li><span class='required'>Sample Sizes</span>: Number of participants in each study</li>
                </ul>

                <p><span class='optional'>Subgroup Analysis</span> and <span class='optional'>Moderators</span> are optional components:</p>
                <ul>
                    <li>In the data editor, set the measure type to <b>Continuous</b> for covariates or <b>Nominal/Ordinal</b> for factors.</li>
                    <li>Numeric codes are treated as factors when their measure type is <b>Nominal</b> or <b>Ordinal</b>.</li>
                </ul>

                <div class='note'>
                    <strong>Note:</strong> Alpha values must be between 0 and 1. The number of items and total sample size must each be at least 2.
                </div>

                <div class='divider'></div>
                <p>Feature requests and bug reports can be made on our <a href='https://github.com/MAgojam/Metafora/issues' target='_blank'>GitHub</a>.</p>
            </div>
        </body>
    "))
  
  return(html_content)
}
