# ----------------------------------------------------------------------------
# Nota:
# Da ricordare che alcune funzioni di questo mzDebug.r sono state create da     
# Marcello Gallucci nel suo file aScafFunctions.R. aggiornato per jReshape e
# e riviste in questo file per una generalizzazione nei moduli per jamovi, a
# partire da Metafora
# ----------------------------------------------------------------------------



# Definisce l'ambiente globale per i flag e per l'accumulo dell'HTML
logFlags <- new.env(parent = emptyenv())
logFlags$j_DEBUG <- FALSE
logFlags$j_INFO <- FALSE
logFlags$log_active <- FALSE
logFlags$log_to_file <- FALSE
logFlags$log_to_ui <- FALSE
logFlags$ui_entries <- "" # Memorizza gli elementi HTML accumulati
logFlags$last_file_error <- NULL # Ultimo errore di scrittura file (diagnostica)
logFlags$ui_log_obj <- NULL

# Percorso portabile, distinto per processo e indipendente dalla cartella sorgente.
# tempfile non consuma il generatore casuale usato dalle analisi statistiche.
logFlags$fleWUD <- NULL

# Inizializza nel processo che esegue l'analisi, non durante la compilazione R.
.mzEnsureLogPath <- function() {
  if (is.null(logFlags$fleWUD)) {
    logFlags$fleWUD <- tempfile(pattern="Metafora_debug_", tmpdir=tempdir(), fileext=".log")
  }
  logFlags$fleWUD
}


# Funzione di supporto per ottenere il timestamp attuale
current_time <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# Aggiorna dinamicamente i flag di log leggendo l'opzione di tipo List (Tri-State)
set_logflags <- function(jlog_mode) {
  if (is.null(jlog_mode)) jlog_mode <- "posOff"

  is_active <- jlog_mode != "posOff"

  if (is_active && !logFlags$log_active) {
    open_log()
  } else if (!is_active && logFlags$log_active) {
    close_log()
  }

  logFlags$log_active <- is_active
  logFlags$j_DEBUG <- is_active
  logFlags$j_INFO <- is_active
  logFlags$log_to_file <- jlog_mode %in% c("posLorT", "posRorB")
  logFlags$log_to_ui <- jlog_mode == "posRorB"
}

# Apre il file di log e resetta il buffer HTML
# NB: log_active resta di competenza esclusiva di set_logflags() — qui non
# viene piu' toccato, per evitare che un fallimento di scrittura silenzioso
# venga comunque "sanato" dall'assegnazione successiva in set_logflags().
open_log <- function() {
  if (!logFlags$log_active) {
    .mzEnsureLogPath()
    logFlags$ui_entries <- "" # Resetta il log visuale per ogni nuova sessione
    logFlags$last_file_error <- NULL
    tryCatch(
      {
        # Difensivo: se la cartella tempdir() non risulta creata nel processo
        # che sta effettivamente scrivendo, la crea. Nessun effetto se esiste
        # gia'; se il fallimento ha un'altra causa (permessi, antivirus),
        # cat() sotto continuera' a fallire e l'errore reale resta visibile.
        dir.create(dirname(logFlags$fleWUD), recursive = TRUE, showWarnings = FALSE)
        cat(paste0("Logging started at  ", current_time(), "\n---------------------------------------\n"),
            file = logFlags$fleWUD, append = TRUE
        )
        versions <- vapply(c("Metafora", "jmvcore", "metafor", "puniform"), function(package) {
          tryCatch(as.character(utils::packageVersion(package)), error=function(e) "unavailable")
        }, character(1))
        cat(paste0("Environment: R ", getRversion(), " | ", R.version$platform,
                   " | ", paste(names(versions), versions, sep="=", collapse=" | "), "\n"),
            file=logFlags$fleWUD, append=TRUE)
      },
      error = function(e) {
        logFlags$last_file_error <- conditionMessage(e)
      }
    )
  }
}

# Chiude il file di log
close_log <- function() {
  if (logFlags$log_active) {
    tryCatch(
      {
        cat(paste0("---------------------------------------\nLogging disabled at ", current_time(), "\n\n"),
            file = logFlags$fleWUD, append = TRUE
        )
      },
      error = function(e) {
        logFlags$last_file_error <- conditionMessage(e)
      }
    )
  }
}

# Riga di intestazione del log: icona cartella + percorso assoluto completo
# (file incluso). L'eventuale ultimo errore di scrittura resta sotto, visibile
# solo quando presente — nessun rumore diagnostico quando tutto funziona.
.mzLogFileStatus <- function() {
  .mzEnsureLogPath()
  abs_path <- tryCatch(normalizePath(logFlags$fleWUD, mustWork = FALSE),
                       error = function(e) logFlags$fleWUD
  )
  
  folder_icon <- paste0(
    "<svg viewBox='0 0 24 24' width='13' height='13' ",
    "style='vertical-align:-2px; margin-right:5px; flex-shrink:0;' ",
    "fill='none' stroke='currentColor' stroke-width='1.8' ",
    "stroke-linecap='round' stroke-linejoin='round'>",
    "<path d='M3 7a2 2 0 0 1 2-2h4l2 2h6a2 2 0 0 1 2 2v1H3z'/>",
    "<path d='M3 8l1.5 9a2 2 0 0 0 2 1.7h9a2 2 0 0 0 2-1.7L19 10H3z'/>",
    "</svg>"
  )
  
  err_line <- if (!is.null(logFlags$last_file_error)) {
    paste0(
      "<div style='color:#c0392b; margin-top:2px;'>Last write error: ",
      escHtmlSpecChars(logFlags$last_file_error), "</div>"
    )
  } else {
    ""
  }
  
  paste0(
    "<div style='font-family:Consolas,Monaco,\"Andale Mono\",monospace; font-size:11px; ",
    "padding:2px 0 8px 0; color:#777; border-bottom:1px solid #e2e8f0; margin-bottom:6px;'>",
    "<div>", folder_icon, "<span style='color:#3e6da9;'>", escHtmlSpecChars(abs_path), "</span></div>",
    err_line,
    "</div>"
  )
}

# Escapa caratteri HTML minimi (contenuti loggati possono includere nomi di
# variabili/studio arbitrari: senza escaping, un '<' o '&' nel dato rompe il
# rendering dell'accordion)
escHtmlSpecChars <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# Funzione interna per renderizzare la struttura dell'Accordion HTML con il pulsante Clear
render_ui_html <- function(ui_log_obj) {
  if (is.null(ui_log_obj)) {
    return()
  }
  
  css_style <- "
    <style>
        .mz-debug-container { background: transparent; margin-top: 8px; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }

        .mz-debug-header {
            font-size: 12px; font-weight: 500; border: none; background-color: transparent;
            padding: 4px 0; color: #3e6da9; width: 100%; text-align: left; cursor: pointer;
            display: flex; align-items: center; justify-content: space-between; transition: all 0.3s ease; border-radius: 0;
            list-style: none !important;
            list-style-type: none !important;
        }
        .mz-debug-header::-webkit-details-marker { display: none !important; }

        .mz-debug-header:hover { color: #1565c0; background-color: transparent; }
        details[open] > .mz-debug-header { font-weight: 700; width: 100%; }

        .mz-header-left { display: flex; align-items: center; flex-grow: 1; }

        .mz-icon-container {
            width: 18px; height: 18px; display: flex; align-items: center; justify-content: center;
            margin-right: 6px; flex-shrink: 0; border-right: 1px solid rgba(62, 109, 169, 0.50); padding-right: 6px;
        }
        .mz-plus-minus { width: 12px; height: 12px; stroke: currentColor; stroke-width: 2; stroke-linecap: round; }
        .mz-plus-minus .mz-vertical { transition: transform 0.3s ease, opacity 0.3s ease; transform-origin: center; }

        details[open] > .mz-debug-header .mz-plus-minus .mz-vertical { transform: rotate(90deg); opacity: 0; }

        .mz-debug-title { display: flex; align-items: center; flex-grow: 1; margin-left: 0px; }
        .mz-debug-title::before, .mz-debug-title::after {
            content: ''; height: 1px; background-color: rgba(62, 109, 169, 0.50); transition: all 0.3s ease;
        }
        .mz-debug-title::before { flex-basis: 30px; margin: 0 8px; }
        .mz-debug-title::after { flex-basis: 30px; margin-left: 8px; }

        details[open] > .mz-debug-header .mz-debug-title::before,
        details[open] > .mz-debug-header .mz-debug-title::after { height: 2px; background-color: rgba(62, 109, 169, 0.70); }
        details[open] > .mz-debug-header .mz-debug-title::after { flex-grow: 1; }

        /* Stile del pulsante Clear */
        .mz-clear-btn {
            background-color: transparent;
            border: 1px solid #cbd5e1;
            color: #64748b;
            font-size: 11px;
            font-weight: 500;
            padding: 2px 8px;
            border-radius: 4px;
            cursor: pointer;
            transition: all 0.2s ease;
            flex-shrink: 0;
        }
        .mz-clear-btn:hover {
            background-color: #f1f5f9;
            color: #0f172a;
            border-color: #94a3b8;
        }

        .mz-clear-wrap {
            position: relative;
            display: inline-flex;
            flex-shrink: 0;
            margin-left: 10px;
        }
        .mz-clear-tooltip {
            display: none;
            position: absolute;
            top: 50%;
            right: calc(100% + 8px);
            transform: translateY(-50%);
            background-color: #3e6da9;
            color: #ffffff;
            font-size: 11px;
            font-weight: 400;
            padding: 4px 8px;
            border-radius: 4px;
            white-space: nowrap;
            z-index: 20;
            pointer-events: none;
        }
        .mz-clear-wrap:hover .mz-clear-tooltip {
            display: block;
        }

        .mz-debug-content {
            padding: 8px 15px; width: 100%; box-sizing: border-box; font-size: 12px; color: #555;
            background: transparent; max-height: 400px; overflow: auto;
            font-family: Consolas, Monaco, 'Andale Mono', monospace; line-height: 1.5; white-space: pre;
        }
    </style>
    "
  
  html_container <- paste0(
    css_style,
    "<details open class='mz-debug-container'>",
    "  <summary class='mz-debug-header'>",
    "    <div class='mz-header-left'>",
    "      <div class='mz-icon-container'>",
    "        <svg class='mz-plus-minus' viewBox='0 0 24 24'>",
    "          <line x1='4' y1='12' x2='20' y2='12' />",
    "          <line x1='12' y1='4' x2='12' y2='20' class='mz-vertical' />",
    "        </svg>",
    "      </div>",
    "      <div class='mz-debug-title'>Debug & Execution Log</div>",
    "    </div>",
    "    <span class='mz-clear-wrap'>",
    "      <span class='mz-clear-tooltip'>Clears the current view only; log repopulates on the next event</span>",
    "      <button class='mz-clear-btn' onclick=\"event.stopPropagation(); document.querySelector('.mz-debug-content').innerHTML='';\">Clear</button>",
    "    </span>",
    "  </summary>",
    "  <div class='mz-debug-content'>",
    .mzLogFileStatus(),
    logFlags$ui_entries,
    "  </div>",
    "</details>"
  )
  
  ui_log_obj$setContent(html_container)
}

# Scrive i messaggi di info nel file .log E/O nell'Accordion UI
.mzLogErrorText <- function(error) {
  condition <- if (inherits(error, "condition")) error else attr(error, "condition")
  # Evita la call serializzata di try-error, che può contenere argomenti dei dati.
  if (!inherits(condition, "condition")) return("Error condition unavailable")
  substr(conditionMessage(condition), 1L, 1000L)
}

.mzLogLocation <- function(location) {
  if (is.null(location)) return("")
  pieces <- strsplit(location, ":", fixed=TRUE)[[1L]]
  paste0("[", pieces[1L], "][L", pieces[2L], "] ")
}

jinfo <- function(..., ui_log_obj = NULL, .loc = NULL) {
  if (!logFlags$j_INFO || !logFlags$log_active) {
    return(invisible(NULL))
  }

  msg <- paste0(.mzLogLocation(.loc), paste(list(...), collapse = " "))
  if (is.null(ui_log_obj)) ui_log_obj <- logFlags$ui_log_obj

  if (logFlags$log_to_file) {
    tryCatch(
      {
        cat(paste0("[I] ", msg, "\n"), file = logFlags$fleWUD, append = TRUE)
      },
      error = function(e) {
        logFlags$last_file_error <- conditionMessage(e)
      }
    )
  }

  if (logFlags$log_to_ui && !is.null(ui_log_obj)) {
    entry_html <- paste0(
      "<div style='margin-bottom: 4px;'>",
      "<strong style='color: #3e6da9;'>[I]</strong> ", escHtmlSpecChars(msg),
      "</div>"
    )
    # Piu' recente in alto: evita di dover scorrere ad ogni re-render
    logFlags$ui_entries <- paste0(entry_html, logFlags$ui_entries)
    render_ui_html(ui_log_obj)
  }
}

# Scrive i marcatori di debug nel file .log E/O nell'Accordion UI
mark <- function(..., ui_log_obj = NULL, .loc = NULL) {
  if (!logFlags$j_DEBUG || !logFlags$log_active) {
    return(invisible(NULL))
  }

  items <- list(...)

  content_str <- .mzLogLocation(.loc)
  if (nzchar(content_str)) content_str <- paste0(content_str, "\n")
  if (is.null(ui_log_obj)) ui_log_obj <- logFlags$ui_log_obj
  if (length(items) == 0) {
    content_str <- "Mark here\n"
  } else {
    for (a in items) {
      if (is.character(a)) {
        content_str <- paste0(content_str, a, "\n")
      } else {
        content_str <- paste0(content_str, paste(capture.output(print(a)), collapse = "\n"), "\n")
      }
    }
  }

  if (logFlags$log_to_file) {
    out_txt <- paste0("______begin________\n", content_str, "_______end_________\n")
    tryCatch(
      {
        cat(out_txt, file = logFlags$fleWUD, append = TRUE)
      },
      error = function(e) {
        logFlags$last_file_error <- conditionMessage(e)
      }
    )
  }

  if (logFlags$log_to_ui && !is.null(ui_log_obj)) {
    entry_html <- paste0(
      "<div style='margin: 8px 0; padding: 6px 10px; background-color: rgba(0,0,0,0.03); border-left: 3px solid #d97706;'>",
      "<strong style='color: #d97706;'>[M]</strong>\n",
      escHtmlSpecChars(content_str),
      "</div>"
    )
    logFlags$ui_entries <- paste0(entry_html, logFlags$ui_entries)
    render_ui_html(ui_log_obj)
  }
}
