# Ciclo uniforme per i sei backend: solo codici delle opzioni e conteggi,
# senza valori dei dati, nomi degli studi o percorsi dei file dell'utente.
.mzBeginAnalysisLog <- function(self) {
  set_logflags(self$options$jlogMode)
  if (!isTRUE(logFlags$log_active)) return(NULL)
  id <- if (is.null(logFlags$runId)) 1L else logFlags$runId + 1L
  logFlags$runId <- id
  start <- proc.time()[["elapsed"]]
  previous_ui <- logFlags$ui_log_obj
  logFlags$ui_log_obj <- self$results$debugLog
  jinfo("ANALYSIS BEGIN", id, "model =", self$options$tabbedControlModelType,
        "estimator =", self$options$modelest, "measure =", self$options$modelmea,
        "CI =", self$options$level, "moderators =", length(self$options$moderator),
        ui_log_obj=self$results$debugLog, .loc = "mzAnalysisLogService.R:11")
  function() {
    on.exit(logFlags$ui_log_obj <- previous_ui, add=TRUE)
    jinfo("ANALYSIS END", id,
                   "elapsed_ms =", round(1000 * (proc.time()[["elapsed"]] - start)),
                   ui_log_obj=self$results$debugLog, .loc = "mzAnalysisLogService.R:17")
  }
}

