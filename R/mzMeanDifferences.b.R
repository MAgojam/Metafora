# mzMeanDifferences.b.r

mzMeanDifferencesClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "mzMeanDifferencesClass",
        inherit = mzMeanDifferencesBase,
        ## Proprietà attive (active bindings)
        active = list(
            # Prepara una copia temporanea dai dati completi, senza popolare dataProc.
            signatureDataProc = function() {
                sourceData <- tryCatch(self$readDataset(),error=function(e) NULL)
                if (is.null(sourceData)) sourceData <- self$data
                if (is.null(sourceData) || nrow(sourceData)==0L) return(NULL)
                private$.cleanData(sourceData)
            },
            ready = function() {
                if (is.null(self$options$slab) ||
                    is.null(self$options$m1i)  ||
                    is.null(self$options$sd1i) ||
                    is.null(self$options$n1i)  ||
                    is.null(self$options$m2i)  ||
                    is.null(self$options$sd2i) ||
                    is.null(self$options$n2i)) {
                    return(FALSE)
                } else {
                    return(TRUE)
                }
            },
            dataProc = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.dataProc)) {
                  private$.dataProc <- mzRequestCalculation(private, ".dataProc", private$.cleanData())
                }

                return(private$.dataProc)
            },
            modelResults = function() {
                if (!self$ready) return(NULL)

                if (is.null(private$.modelResults)) {
                  # textRICH funge da anchor-cache: clearWith solo statistiche.
                  # Su cicli con sole modifiche estetiche, textRICH$state è ancora
                  # popolato e private$.computeModel() NON viene valutato (lazy eval).
                  private$.modelResults <- mzRequestCalculation(private, ".modelResults", mzGetCachedModel(
                    self$results$textRICH,
                    private$.computeModel()
                  ))
                }

                return(private$.modelResults)
            },
            escalcResults = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.escalcResults)) {
                  private$.escalcResults <- mzRequestCalculation(private, ".escalcResults", private$.computeEscalc())
                }

                return(private$.escalcResults)
            },
            effectsize = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.effectsize)) {
                  private$.effectsize <- self$escalcResults$effectsize
                }

                return(private$.effectsize)
            },
            stderror = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.stderror)) {
                  private$.stderror <- sqrt(self$escalcResults$variance)
                }

                return(private$.stderror)
            },
            variance = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.variance)) {
                  private$.variance <- self$escalcResults$variance
                }

                return(private$.variance)
            },
            subgroupResults = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.subgroupResults)) {
                  private$.subgroupResults <- mzRequestCalculation(private, ".subgroupResults", mzGetCachedSubgroups(
                    self$results$subgroupResults, private$.computeSubgroupAnalysis()))
                }

                return(private$.subgroupResults)
            },
            forestData = function() {
                if (!self$ready) {
                  return(NULL)
                }

                if (is.null(private$.dataProc) || is.null(private$.escalcResults)) {
                  return(NULL)
                }

                # Unisce i dati processati e i risultati escalc
                return(cbind(private$.dataProc, private$.escalcResults))
            },
            leave1outResults = function() {
                if (is.null(private$.leave1outResults)) {
                  res <- self$modelResults
                  if (is.null(res)) return(NULL)
                  private$.leave1outResults <- mzRequestCalculation(private, ".leave1outResults", .mzCachedLeaveOneOut(self,private$.mfsc))
                }

                return(private$.leave1outResults)
            }
        ),
        private = list(
            .renderD3Export = function(image, ...) .mzRenderD3Export(self,image),

            .requestCalculations = NULL,
            ## Variabili dell'istanza
            .dataProc = NULL,
            .modelResults = NULL,
            .escalcResults = NULL,
            .subgroupResults = NULL,
            .leave1outResults = NULL,
            .forestData = NULL,
            .effectsize = NULL,
            .stderror = NULL,
            .variance = NULL,
            .mfsc = NULL,
            .requiredVars = list(
              correlation = c("slab", "ri", "ni"),
              alpha       = c("slab", "ai", "mi", "ni"),
              dichotomous = c("slab", "ai", "bi", "ci", "di"),
              meandiff    = c("slab", "m1i", "sd1i", "n1i", "m2i", "sd2i", "n2i"),
              proportion  = c("slab", "xi", "ni"),
              effectsize  = c("slab", "yi", "vi")
            ),


            ## Funzioni di inizializzazione
            .init = function() {
                set_logflags(self$options$jlogMode)  # Aggiorna i flag di log in base al tri-state

                # Inizializza MetaforServiceClass
                private$.mfsc <- MetaforServiceClass$new()
                mzInitTableLifecycle(self)
                mzInitCumulative(self)

                # Variabili richieste per la meta-analisi
                reqVars <- private$.requiredVars[[self$options$tabbedControlModelType]]

                # Controlla se sono presenti tutte le variabili required
                # cosi visualizza o nasconde anche le istruzioni di aiuto nel
                # File: mzHtmlService.R
                if (!mzCheckAnalysisReady(self, reqVars, self$options$tabbedControlModelType)) {
                    return()
                }

                # Inizializza i Plot (mzPlotService.R)
                mzInitPlots(self)
            },

            .postInit = function() {
                mzRestoreTableMetadata(self)
                mzApplyCachedSize(self$results$cumulativePlot, self$results$cumulativePlotSizeCache)

                # Hook del ciclo di vita jamovi: chiamato dopo .init() e prima di .run().
                # Ripristina le dimensioni di forest e funnel plot dalla cache
                # permanente. Se isFilled() è TRUE (ciclo estetico) il setSize()
                # ripristina le dimensioni senza invalidare filePath (fix f1bce8a
                # jmvcore >= 2.7.27.2). Se isFilled() è FALSE (ciclo statistico)
                # mzApplyCachedSize ritorna subito senza fare nulla.

                mzApplyCachedSize(
                  self$results$mzForestPlot,
                  self$results$mzForestPlotSizeCache
                )

                mzApplyCachedSize(
                  self$results$mzFunnelPlot,
                  self$results$mzFunnelPlotSizeCache
                )
              },

            ## Funzioni di esecuzione
            .run = function() {
                on.exit(.mzApplyD3Exports(self),add=TRUE)

                finishLog <- .mzBeginAnalysisLog(self)
                if (is.function(finishLog)) on.exit(finishLog(), add=TRUE)

                # Svuota la coda toast: ogni .run() parte con coda vuota per
                # non ritrasmettere i toast delle esecuzioni precedenti.
                private$.mfsc$mzResetToastQueue()

                # Variabili required per la meta-analisi
                reqVars <- private$.requiredVars[[self$options$tabbedControlModelType]]

                # 1. Controlla se sono presenti tutte le variabili richieste e mostra istruzioni
                if (!mzCheckAnalysisReady(self, reqVars, self$options$tabbedControlModelType)) {
                    return()
                }

                # 2. Validazione completa delle opzioni e preparazione dati
                data_prep <- private$.mfsc$mzValidateAndPrepareData(self, reqVars)
                if (!data_prep$success) {
                    jmvcore::reject(data_prep$error, code = "")
                    return()
                }

                # 3. Ottieni il dataset pronto
                data <- self$dataProc
                mzSaveGlobalInputSignature(self)

                # 4. Controlla se serve una validazione specifica per meandiff
                # Metterla qui in questo segnaposto
                # 4. Fine controllo ---

                # --- Ora possiamo procedere con i calcoli --- #

                # Salva tre nuove variabili nel dataset New_ES, New_SE, New_Var
                private$.mfsc$mzPopolateOutput(self)

                # Popola la tabella del Modello
                private$.popolateModelTable()

                # Popola la tabella dei sottogruppi se necessario
                if (!is.null(self$options$subgroup)) {
                    private$.popolateSubgroupTable()
                }

                # Calcola TOST
                if (self$options$showTost) {
                    private$.computeTOST()
                }

                # Calcola TES
                if (self$options$showTes) {
                    private$.computeTES()
                }

                # Calcola Selection Model
                if (self$options$showSelmodel) {
                    private$.computeSelmodel()
                }

                # Calcola p-uniform
                if (self$options$showPuniform) {
                    private$.computePuniform()
                }

                if (!is.null(self$modelResults) && !jmvcore::isError(self$modelResults)) {
                    res <- self$modelResults
                    # Calcolo opzionale dal fit in cache; render Image senza dati grezzi.
                    mzUpdatePredictiveProbability(self,res)
                    mzRunHeteroscedasticityTest(self,res,private$.mfsc)


                    # Calcola e cachea le dimensioni del forest plot.
                    # Guard automatico: skip se isFilled() == TRUE (ciclo estetico)
                    # perche le dimensioni sono gia in cache e ripristinate da .postInit().
                    mzUpdateForestPlot(
                      image     = self$results$mzForestPlot,
                      model     = res,
                      options   = self$options,
                      sizeCache = self$results$mzForestPlotSizeCache,
                      # Con sottogruppi le righe sono k studi + 2 per gruppo
                      # (una riga blank + un poligono summary per ciascuno).
                      getRowsFn = function(m, o) {
                        if (!is.null(o$subgroup) && !is.null(self$dataProc) &&
                            "subgroup" %in% names(self$dataProc)) {
                          nGroups <- length(unique(as.character(self$dataProc$subgroup)))
                          m$k + nGroups * 2L
                        } else {
                          m$k
                        }
                      }
                    )

                    # Triggera la render function del forest plot (sia ciclo statistico
                    # che estetico) via setState. Su ciclo estetico la render function
                    # usa xlim/ylim dalla sizeCache per mantenere le dimensioni stabili.
                    mzUpdateCachedForestPlot(
                      image           = self$results$mzForestPlot,
                      sizeCache       = self$results$mzForestPlotSizeCache,
                      model           = res,
                      subgroupResults = self$subgroupResults,
                      options         = self$options,
                        rob             = .mzCollectRobDomains(self),
                        data            = self$dataProc
                    )

                    # Calcola e cachea le dimensioni fisse del funnel plot.
                    # Guard automatico: skip se isFilled() == TRUE (ciclo estetico)
                    # perche le dimensioni sono gia in cache e ripristinate da .postInit().
                    mzUpdateFixedPlot(
                      image     = self$results$mzFunnelPlot,
                      model     = res,
                      options   = self$options,
                      sizeCache = self$results$mzFunnelPlotSizeCache,
                      widthPx   = self$options$funnelPlotWidth,
                      heightPx  = self$options$funnelPlotHeight
                    )

                    # Triggera la render function del funnel plot Classic via setState,
                    # con lo stesso guard su image$visible (funnelPlotMode == "posLorT")
                    # usato dal forest plot.
                    mzUpdateCachedFunnelPlot(
                      image   = self$results$mzFunnelPlot,
                      model   = res,
                      options = self$options
                    )

                    # Aggiorna tutti gli altri plot (diagnostic, ecc.)
                    # mzForestPlot e mzFunnelPlot sono esclusi da mzUpdatePlots: gestiti sopra.
                    mzUpdatePlots(self)

                    # --- Per generare la Tabella FOREST ---
                    mzRenderForestTable(self)

                }

                # Inizializza mzHtmlD3Service
                d3s <- mzHtmlD3Service$new()

                # 8. --- D3 Forest Plot (type: Html)
                d3s$renderForest(
                    jamovi_self = self,
                    res = res,
                    ilab = list(self$dataProc$n1i,
                                self$dataProc$m1i,
                                self$dataProc$sd1i,
                                self$dataProc$n2i,
                                self$dataProc$m2i,
                                self$dataProc$sd2i),
                    ilab_names  = c("N", "M", "SD", "N", "M", "SD"),
                    ilab_groups = list(list(label = .("Group 1"), span = 3L),
                                       list(label = .("Group 2"), span = 3L)),
                    ilab_totals = c("sum", "wmean", "pooledsd", "sum", "wmean", "pooledsd")
                )

                # 9. --- D3 Funnel Plot (type: Html)
                d3s$renderFunnel(jamovi_self = self, res = res)

                # 10. --- Leave-One-Out: grafico e tabella HTML D3 indipendenti
                private$.mzLeaveOneOut()
                mzRunCumulative(self)

                # 11. --- Generazione finale del report HTML (se attivato)
                if (isTRUE(self$options$showTextRepo) &&
                    !is.null(self$modelResults) &&
                    !jmvcore::isError(self$modelResults)) {

                  private$.mzCreateHtmlReport(self$modelResults)
                }
            },

            ## Prepara e pulisce il dataset
            .cleanData = function(sourceData = self$data) {

                # Riprende le varibili required per il modello di analisi
                reqvars <- private$.requiredVars[[self$options$tabbedControlModelType]] # <-- UNICA RIGA DA CAMBIARE

                # Ottieni slab, moderators e subgroup
                slab <- self$options$slab
                moderators <- self$options$moderator
                subgroup <- self$options$subgroup

                # Creazione data_list di base con variabili del modello
                data_list <- list()
                for (var in reqvars) {
                    if (var == "slab") {
                        data_list[[var]] <- as.character(sourceData[[slab]])
                    } else {
                        data_list[[var]] <- jmvcore::toNumeric(sourceData[[self$options[[var]]]])
                    }
                }

                # Aggiunge i moderatori (possono essere più di uno)
                if (!is.null(moderators)) {
                    for (mod_var in moderators) {
                        # Conversione condivisa rispettando il livello di misura jamovi.
                        data_list[[mod_var]] <- .mzPrepareModerator(sourceData[[mod_var]])
                    }
                }

                # Aggiunge il sottogruppo
                if (!is.null(subgroup)) {
                    data_list[["subgroup"]] <- sourceData[[subgroup]]
                }

                # Preserva i nomi dei moderatori, anche con spazi o punteggiatura.
                data <- data.frame(data_list, check.names=FALSE)

                data <- na.omit(data)
                attr(data, "class") <- "data.frame"
                return(data)
            },

            ## Calcola Risultati Modello
            .computeModel = function() {
                jinfo("[FIT] begin", .loc = "mzMeanDifferences.b.R:420")
              
                if (!is.null(private$.modelResults)) {
                    return(private$.modelResults)
                }

                data <- self$dataProc

                # Riprende le variabili required per il modello di analisi
                model_reqvars <- private$.requiredVars[[self$options$tabbedControlModelType]] # <-- UNICA RIGA DA CAMBIARE

                # Costruisci dinamicamente reqvars escludendo 'slab'
                reqvars <- list()
                for (var in model_reqvars) {
                    if (var != "slab") {
                        reqvars[[var]] <- var
                    }
                }

                # se in reqvars ci sono nomi di colonna (stringhe), convertili in vettori
                for (rv in names(reqvars)) {
                    if (is.character(reqvars[[rv]]) && reqvars[[rv]] %in% names(data)) {
                        reqvars[[rv]] <- data[[reqvars[[rv]]]]
                    }
                }

                # Gestione Moderatori
                moderators <- self$options$moderator
                if (length(moderators) == 0) {
                    res <- try(do.call(
                        mzFitRma,
                        c(
                            list(
                                method  = self$options$modelest,
                                measure = self$options$modelmea,
                                data    = data,
                                slab    = data[["slab"]],
                                test    = self$options$testType,
                                level   = self$options$level
                            ),
                            reqvars
                        )
                    ))
                } else {
                    # Costruzione automatica formula moderatori
                    mod_terms <- jmvcore::composeTerms(as.list(moderators))
                    # Le variabili sono nel dataframe; evita di catturare self nella formula.
                    mods_formula <- as.formula(paste("~", paste(mod_terms, collapse = " + ")), env = asNamespace("stats"))

                    res <- try(do.call(
                        mzFitRma,
                        c(
                            list(
                                method  = self$options$modelest,
                                measure = self$options$modelmea,
                                mods    = mods_formula,
                                data    = data,
                                slab    = data[["slab"]],
                                test    = self$options$testType,
                                level   = self$options$level
                            ),
                            reqvars
                        )
                    ))
                }

                if (jmvcore::isError(res)) {
                    errmsg <- jmvcore::extractErrorMessage(res)
                    jmvcore::reject(errmsg, code = "")
                }

                mark("[FIT] end", list(result_class=class(res),
                     studies=if (is.list(res)) res$k else NULL), .loc = "mzMeanDifferences.b.R:491")
                if (inherits(res, "try-error")) mark("[FIT] failed", .mzLogErrorText(res), .loc = "mzMeanDifferences.b.R:493")
                private$.modelResults <- res
                return(private$.modelResults)
            },

            ## Calcola con Escalc
            .computeEscalc = function() {
                jinfo("[ES] begin", .loc = "mzMeanDifferences.b.R:500")
              
                if (!is.null(private$.escalcResults)) {
                    return(private$.escalcResults)
                }

                data <- self$dataProc

                # Riprende le variabili required per il modello di analisi
                model_reqvars <- private$.requiredVars[[self$options$tabbedControlModelType]] # <-- UNICA RIGA DA CAMBIARE

                # Costruisce dinamicamente gli argomenti per escalc
                escalc_args <- list(measure = self$options$modelmea, data = data)

                # Aggiunge i parametri specifici del modello (escluso 'slab')
                required_vars <- setdiff(model_reqvars, "slab")
                for (var in required_vars) {
                    if (var %in% names(data)) {
                        escalc_args[[var]] <- data[[var]]
                    }
                }

                # Esegue escalc
                res <- try(do.call(metafor::escalc, escalc_args))

                if (jmvcore::isError(res)) {
                    errmsg <- jmvcore::extractErrorMessage(res)
                    jmvcore::reject(errmsg, code = "")
                    return(NULL)
                }

                # Crea il dataframe di risultato
                df <- data.frame(
                    yi = res$yi,
                    vi = res$vi,
                    effectsize = res$yi,
                    variance = res$vi
                )

                private$.escalcResults <- df
                return(private$.escalcResults)
            },

            ## Calcola l'analisi dei sottogruppi
            .computeSubgroupAnalysis = function() {
                jinfo("[SG] begin", .loc = "mzMeanDifferences.b.R:545")
              
                if (!is.null(private$.subgroupResults)) {
                    return(private$.subgroupResults)
                }
                if (is.null(self$options$subgroup)) {
                    return(NULL)
                }

                data <- self$dataProc

                # Riprende le variabili required per il modello di analisi
                model_reqvars <- private$.requiredVars[[self$options$tabbedControlModelType]] # <-- UNICA RIGA DA CAMBIARE

                # Controlla che la variabile sottogruppo sia nel dataset
                if (!"subgroup" %in% names(data)) {
                    subgroup_var <- self$options$subgroup
                    data$subgroup <- data[[subgroup_var]]
                }

                # Controlla se esiste la variabile sottogruppo
                if (!"subgroup" %in% names(data)) {
                    return(NULL)
                }

                # Crea una lista per salvare i risultati
                subgroupResults <- list()

                # Ottiene i livelli unici dei sottogruppi
                subgroupLevels <- unique(as.character(data[["subgroup"]]))
                subgroupLevels <- subgroupLevels[!is.na(subgroupLevels)]

                # Costruisce dinamicamente gli argomenti per metafor::rma (escluso 'slab')
                required_vars <- setdiff(model_reqvars, "slab")

                # Per ogni livello di sottogruppo, esegue meta-analisi separata
                for (sublev in subgroupLevels) {
                    # Seleziona i dati per il sottogruppo
                    subdata <- data[as.character(data[["subgroup"]]) == sublev, ]

                    # Salta se meno di 2 studi
                    if (nrow(subdata) < 2) {
                        next
                    }

                    # Costruisce dinamicamente gli argomenti per questa analisi di sottogruppo
                    rma_args <- list(
                        method  = self$options$modelest,
                        measure = self$options$modelmea,
                        data    = subdata,
                        slab    = subdata[["slab"]],
                        test    = self$options$testType,
                        level   = self$options$level
                    )

                    # Aggiunge le variabili specifiche del modello
                    for (var in required_vars) {
                        rma_args[[var]] <- subdata[[var]]
                    }

                    # Esegue la meta-analisi per questo sottogruppo
                    tryCatch(
                        {
                            res <- do.call(mzFitRma, rma_args)
                            # Salva i risultati
                            subgroupResults[[sublev]] <- res
                        },
                        error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:613")
                            # Ignora l'errore e prosegue con il livello successivo
                        }
                    )
                }

                # Calcola l'eterogeneità tra gruppi se ci sono almeno 2 sottogruppi validi
                if (length(subgroupResults) >= 2) {
                    # Costruisce dinamicamente gli argomenti per meta-regressione
                    metareg_args <- list(
                        # Mantiene i nomi dei coefficienti, catturando soltanto il dataframe.
                        mods    = as.formula('~ factor(data[["subgroup"]])', env = list2env(list(data = data), parent = asNamespace("stats"))),
                        method  = self$options$modelest,
                        measure = self$options$modelmea,
                        data    = data,
                        test    = self$options$testType,
                        level   = self$options$level
                    )

                    # Aggiunge le variabili specifiche del modello
                    for (var in required_vars) {
                        metareg_args[[var]] <- data[[var]]
                    }

                    # Esegue la meta-regressione con il sottogruppo come moderatore
                    tryCatch(
                        {
                            metareg <- do.call(mzFitRma, metareg_args)
                            subgroupResults$metareg <- metareg
                        },
                        error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:644")
                            # Ignora errore
                        }
                    )
                }

                private$.subgroupResults <- subgroupResults

                return(private$.subgroupResults)
            },

            ## Confronta e popola la tabella degli stimatori di eterogeneità
            .compareHeterogeneityEstimators = function() {
              
                if (!isTRUE(self$options$showHEC) || is.null(self$results$heterogeneityComparison)) {
                    return()
                }

                data <- self$dataProc

                private$.mfsc$mzCompareHeterogeneityEstimators(
                    data         = data,
                    table        = self$results$heterogeneityComparison,
                    options      = self$options,
                    dataType     = self$options$tabbedControlModelType,
                    requiredVars = private$.requiredVars[[self$options$tabbedControlModelType]]
                )
            },

            ## Funzione di supporto per i test di bias di pubblicazione
            .publicationBiasTests = function(res) {
                jinfo("[PBA] begin", .loc = "mzMeanDifferences.b.R:675")
              
                return(private$.mfsc$mzPublicationBiasTests(self, res, options = self$options))
            },

            ## Calcola TOST
            .computeTOST = function() {
                .mzComputeTOST(self)
            },

            ## Calcola TES (Test of Excess Significance)
            .computeTES = function() {
                .mzComputeTES(self)
            },

            ## Calcola Selection Models
            .computeSelmodel = function() {
                resSelMod <- .mzFitSelectionModel(self)
                if (is.null(resSelMod)) return()

                # Info sul modello base
                modEst <- if (self$options$modelest == "FE") .("FE Model") else sprintf(.("RE Model [%s]"), self$options$modelest)

                # Label Misura
                measureLabel <- switch(paste(self$options$modelmea),
                    "MD"    = .("Mean Difference"),
                    "SMD"   = .("Standardized Mean Difference"),
                    "SMDH"  = .("Standardized Mean Difference (heteroscedastic)"),
                    "SMD1"  = .("Standardized Mean Difference (using SD1)"),
                    "SMD1H" = .("Standardized Mean Difference (using SD1, heteroscedastic)"),
                    "ROM"   = .("Log Ratio of Means"),
                    self$options$modelmea
                ) # Ripiego predefinito

                htmlSelMod <- mzHtmlSelmodel(
                    resSelMod,
                    self$options,
                    modEst,
                    measureLabel
                )

                .mzSetHtmlResult(self, self$results$outHtmlSelmodel, 'selection', htmlSelMod)
            },

            ## Calcola p-uniform
            .computePuniform = function() {
                .mzComputePuniform(self)
            },

            ## Popola le tabelle delle funzioni e dell'output
            .popolateModelTable = function() {
                res <- self$modelResults
                table <- self$results$textRICH # Random-Effects Model
                mzUpdateModelTableMetadata(self, table, res)

                # Popola la tabella del modello
                mzSetTableRowIfChanged(table,rowNo = 1, values = list(
                    Intercept = .("Intercept"),
                    Estimate  = as.numeric(res$b[1]),
                    se        = res$se[1],
                    CILow     = res$ci.lb[1],
                    CIHigh    = res$ci.ub[1],
                    p         = res$pval[1],
                    stat      = if (!is.null(res$zval)) res$zval[1] else (res$b[1] / res$se[1]),
                    k         = res$k
                ))

                # Aggiunge righe per i moderatori (se presenti)
                if (length(res$b) > 1) {
                    coef_names <- private$.mfsc$mzFormatCoefNames(res, self$options$moderator, self$dataProc)
                    for (i in 2:length(res$b)) {
                        mzSetTableRowIfChanged(table,rowKey = i, values = list(
                            Intercept = coef_names[i],
                            Estimate  = as.numeric(res$b[i]),
                            se        = res$se[i],
                            CILow     = res$ci.lb[i],
                            CIHigh    = res$ci.ub[i],
                            p         = res$pval[i],
                            stat      = if (!is.null(res$zval)) res$zval[i] else (res$b[i] / res$se[i]),
                            k         = res$k
                        ))
                    }
                }

                ## Data Prep: Heterogeneity Stats ----
                tauSquared <- round(res$tau2, 4)
                tauSquaredSE <- ifelse(is.null(res$se.tau2), NA, round(res$se.tau2, 4))

                # Gestisce il caso in cui res$se.tau2 è NULL (es. modello FE)
                if (is.na(tauSquaredSE)) {
                    tauSqCombind <- as.character(tauSquared)
                } else {
                    tauSqCombind <- paste(tauSquared, "(SE=", tauSquaredSE, ")")
                }

                tauOnly <- ifelse(is.null(res$tau2) || is.na(res$tau2), NA, round(sqrt(res$tau2), 4))
                ISquStat <- ifelse(is.null(res$I2) || is.na(res$I2), NA, paste(round(res$I2, 2), "%", sep = ""))
                HSquStat <- ifelse(is.null(res$H2) || is.na(res$H2), NA, round(res$H2, 4))

                if (!is.null(self$options$moderator) && !is.null(res$R2)) {
                    RSquStat <- paste(round(res$R2, 2), "%", sep = "")
                } else {
                    RSquStat <- NULL
                }

                # Data Prep: Heterogeneity Test
                QTestStatDF <- if (is.null(res$k) || is.null(res$p)) NA_integer_ else as.integer(res$k - res$p)

                # Tabella delle statistiche e dei test di eterogeneità
                tableTauSqaured <- self$results$tableTauSqaured
                mzSetTableRowIfChanged(tableTauSqaured,
                    rowNo = 1,
                    values = list(
                        tauSqComb = tauSqCombind,
                        tauSQRT   = tauOnly,
                        ISqu      = ISquStat,
                        HSqu      = HSquStat,
                        RSqu      = RSquStat,
                        Qall      = res$QE,
                        QallDF    = QTestStatDF,
                        QallPval  = res$QEp
                    )
                )

                ## Statistiche di adattamento del modello
                if (self$options$showModelFit && !is.null(self$results$modelFitRICH)) {
                    modelFitTable <- self$results$modelFitRICH

                    # Popola la tabella
                    mzSetTableRowIfChanged(modelFitTable,
                        rowNo = 1,
                        values = list(
                            label = .("Maximum-Likelihood"),
                            loglikelihood = res$fit.stats[1, 1],
                            deviance = res$fit.stats[2, 1],
                            AIC = res$fit.stats[3, 1],
                            BIC = res$fit.stats[4, 1],
                            AICc = res$fit.stats[5, 1]
                        )
                    )

                    mzSetTableRowIfChanged(modelFitTable,
                        rowNo = 2,
                        values = list(
                            label = .("Restricted Maximum-Likelihood"),
                            loglikelihood = res$fit.stats[1, 2],
                            deviance = res$fit.stats[2, 2],
                            AIC = res$fit.stats[3, 2],
                            BIC = res$fit.stats[4, 2],
                            AICc = res$fit.stats[5, 2]
                        )
                    )
                }

                # Calcola i test soltanto quando la tabella viene richiesta.
                if (isTRUE(self$options$distabPBA)) {
                    pubBiasResults <- private$.publicationBiasTests(res)
                    private$.mfsc$mzPopulatePBT(pubBiasResults, self$results$tabPubBias, options = self$options)
                }

                private$.compareHeterogeneityEstimators()
            },

            ## Popola la tabella dei sottogruppi
            .popolateSubgroupTable = function() {
                mzPopulateSubgroupTable(self)
            },
            .getSubgrEnrichedData = function(dat) {
                # Delega alla funzione centralizzata in mzForestService.R
                # Il binding escalcResults gestisce anche i tentativi falliti.

                return(mzGetSubgrEnrichedData(
                    dat = dat,
                    escalcResults = self$escalcResults,
                    subgroupResults = self$subgroupResults,
                    modelResults = self$modelResults
                ))
            },

            ## Ordina Forest Table CON sottogruppo
            # Delega alla funzione centralizzata in mzForestService.R
            .sortForestTableWithSubgroup = function(studies, order_var) {
                return(mzSortForestTableWithSubgroup(
                    studies = studies,
                    dataProc = self$dataProc,
                    escalcResults = self$escalcResults,
                    subgroupResults = self$subgroupResults,
                    order_var = order_var
                ))
            },

            ## Ordina Forest Table SENZA sottogruppo
            # Delega alla funzione centralizzata in mzForestService.R
            .sortForestTableNoSubgroup = function(studies, order_var) {
                return(mzSortForestTableNoSubgroup(
                    studies = studies,
                    dataProc = self$dataProc,
                    escalcResults = self$escalcResults,
                    modelResults = self$modelResults,
                    order_var = order_var
                ))
            },

            ## Calcola posizioni righe per sottogruppi con ordinamento interno
            # Delega alla funzione centralizzata in mzForestService.R
            .forestCalcRowsSubgr = function(dat, group_var, order_var = NULL) {
                return(mzForestCalcRowsSubgr(dat, group_var, order_var))
            },

            # Usa mzForestPlotAll() che è nel file mzPlotService.R
            .mzPredProbPlot = function(image, ...) {
                mzRenderPredictiveProbability(image)
            },

            .mzForestPlot = function(image, ...) {
              
                state <- image$state
                plotData <- if (inherits(state,"rma")) state else
                    if (inherits(state$model,"rma")) state$model else self$modelResults
                # Il render usa lo snapshot aggiornato, anche se il fit viene dalla cache.
                dataProc <- if (is.data.frame(state$data)) state$data else
                    if (is.data.frame(plotData$data)) plotData$data else self$dataProc

                if (is.null(plotData)) {
                    jmvcore::reject(.("Model results not available."), code = "")
                    return(FALSE)
                }

                cachedDims <- self$results$mzForestPlotSizeCache$state
                
                # Firma del layout corrente: se cambia (subgroup ON/OFF o k diverso)
                # i valori xlim/ylim in cache sono stale e devono essere ignorati.
                # Senza questo guard, passare da posRorB+subgroup -> posOff -> posLorT
                # usa un ylim troppo stretto (calcolato senza subgroup) causando freeze.
                layoutSig <- list(
                    hasSubgroup = !is.null(self$options$subgroup),
                    k           = as.numeric(plotData$k)
                )
                if (!is.null(cachedDims) &&
                    (!isTRUE(cachedDims$layoutSig$hasSubgroup == layoutSig$hasSubgroup) ||
                     !isTRUE(cachedDims$layoutSig$k == layoutSig$k))) {
                    cachedDims$xlim <- NULL
                    cachedDims$ylim <- NULL
                }
                xlimFixed  <- if (!is.null(cachedDims)) cachedDims$xlim else NULL
                ylimFixed  <- if (!is.null(cachedDims)) cachedDims$ylim else NULL

                # Determina atransf e targs (fonte unica, mzMetaforService.R)
                btInfo <- .mzGetBackTransform(
                  self$options$tabbedControlModelType,
                  self$options$modelmea,
                  self$options$showBackTransform,
                  ni = if (!is.null(dataProc) && "ni" %in% names(dataProc)) dataProc$ni else NULL
                )
                getAtrTar <- list(atransf = btInfo$atransf, targs = btInfo$targs)

                # Verifica se c'è la variabile per i sottogruppi
                if (!is.null(self$options$subgroup)) {
                    subgroupVar <- dataProc$subgroup
                    colout <- "darkblue"
                } else {
                    subgroupVar <- NULL
                    colout <- "black"
                }

                ilab <- cbind(
                    dataProc$n1i, dataProc$m1i, dataProc$sd1i,
                    dataProc$n2i, dataProc$m2i, dataProc$sd2i
                )
                ilabLab <- c("N", "M", "SD", "N", "M", "SD")
                # Statistiche colonne: N=sum, Mean=mean, SD=mean (summary aritmetico)
                # Funzione helper per calcolare statistiche pooled (Overall + Subgroups)
                getPooledStats <- function(ni, mi, sdi, subVar = NULL) {
                    # 1. Risultato complessivo
                    N <- sum(ni, na.rm = TRUE)
                    pooledM <- sum(ni * mi, na.rm = TRUE) / N
                    # SD aggregata: approssimazione con varianze ponderate
                    # SDp = sqrt( sum((ni-1)*sdi^2) / sum(ni-1) )
                    pooledSD <- sqrt(sum((ni - 1) * sdi^2, na.rm = TRUE) / sum(ni - 1, na.rm = TRUE))

                    res <- list(overall = pooledM, overallSD = pooledSD) # overallSD non usato direttamente qui ma calcolato

                    # 2. Sottogruppi
                    if (!is.null(subVar)) {
                        groups <- if (is.factor(subVar)) levels(droplevels(subVar)) else sort(unique(subVar))
                        groupValsM <- numeric(length(groups))
                        groupValsSD <- numeric(length(groups))

                        for (i in seq_along(groups)) {
                            idx <- which(subVar == groups[i])
                            ns <- ni[idx]
                            ms <- mi[idx]
                            ss <- sdi[idx]
                            Ns <- sum(ns, na.rm = TRUE)
                            groupValsM[i] <- sum(ns * ms, na.rm = TRUE) / Ns
                            groupValsSD[i] <- sqrt(sum((ns - 1) * ss^2, na.rm = TRUE) / sum(ns - 1, na.rm = TRUE))
                        }
                        return(list(
                            meanStruct = list(overall = pooledM, groups = groupValsM),
                            sdStruct = list(overall = pooledSD, groups = groupValsSD)
                        ))
                    } else {
                        return(list(
                            meanStruct = list(overall = pooledM, groups = NULL),
                            sdStruct = list(overall = pooledSD, groups = NULL)
                        ))
                    }
                }

                # Calcolo statistiche per Gruppo 1
                stats1 <- getPooledStats(dataProc$n1i, dataProc$m1i, dataProc$sd1i, subgroupVar)
                # Calcolo statistiche per Gruppo 2
                stats2 <- getPooledStats(dataProc$n2i, dataProc$m2i, dataProc$sd2i, subgroupVar)

                # Statistiche colonne: N=sum, Mean=PooledMean, SD=PooledSD
                ilabStat <- list(
                    sum, # N1
                    stats1$meanStruct, # M1
                    stats1$sdStruct, # SD1
                    sum, # N2
                    stats2$meanStruct, # M2
                    stats2$sdStruct # SD2
                )

                # Riferimento: stima aggregata senza moderatori; altrimenti media e SD ponderate degli effetti osservati.

                refVal <- if (isTRUE(plotData$int.only)) as.numeric(plotData$b[1]) else NA_real_

                # Costruisci argomenti per la funzione mzForestPlotAll()
                forestArgs <- list(
                    plotData      = plotData,
                    self          = self,
                    rob           = image$state$rob,
                    refline       = refVal,
                    atransf       = getAtrTar$atransf,
                    targs         = getAtrTar$targs,
                    subgroupVar   = subgroupVar,
                    colout        = colout,
                    ilab          = ilab,
                    ilabLab       = ilabLab,
                    ilabStat      = ilabStat,
                    addpred       = isTRUE(self$options$addpred),
                    addfit        = isTRUE(self$options$addfit),
                    showweights   = isTRUE(self$options$showweights),
                    reverseOrder  = isTRUE(self$options$ordSubgr),
                    orderWithin   = self$options$forestOrder,
                    level         = self$options$level,
                    steps         = self$options$steps,
                    main          = self$options$mainTitle,
                    pch           = mzGetPchValue(self$options$pchForest),
                    xlimLine      = TRUE, # linea tratteggiata per il Summary
                    topHeader     = c("Group_1", "Group_2"),
                    header        = "Study"
                )

                # Aggiunge xlab solo se presente un titolo per xAxisTitle
                xlabOpt <- self$options$xAxisTitle
                if (length(xlabOpt) > 0 && isTRUE(nzchar(xlabOpt))) {
                    forestArgs$xlab <- xlabOpt
                }

                if (!is.null(xlimFixed)) {
                  forestArgs$xlim <- xlimFixed
                }
                if (!is.null(ylimFixed)) {
                  forestArgs$ylim <- ylimFixed
                }

                forestInvis <- do.call(mzForestPlotAll, forestArgs)

                if (is.null(xlimFixed) &&
                    is.list(forestInvis) &&
                    !is.null(forestInvis$tmp) &&
                    !is.null(forestInvis$tmp$xlim) &&
                    !is.null(forestInvis$tmp$ylim)) {

                  cachedState <- self$results$mzForestPlotSizeCache$state
                  newCachedState <- if (!is.null(cachedState)) cachedState else list()
                  newCachedState$xlim <- forestInvis$tmp$xlim
                  newCachedState$ylim <- forestInvis$tmp$ylim
                  newCachedState$alim <- forestInvis$tmp$alim
                  newCachedState$layoutSig <- layoutSig
                  self$results$mzForestPlotSizeCache$setState(newCachedState)
                }

                return(TRUE)
            },

            ## Funnel Plot
            .mzFunnelPlot = function(image, ...) {
              
                # image$state e' ora la renderState list (model + opts) impostata
                # da mzUpdateCachedFunnelPlot(), non piu' l'oggetto rma grezzo.
                # Stessa logica difensiva di .mzForestPlot: fallback su
                # self$modelResults quando lo state non e' un "rma" puro.
                if (is.null(image$state)) {
                  plotData <- self$modelResults
                } else if (!inherits(image$state, "rma")) {
                  plotData <- self$modelResults
                } else {
                  plotData <- image$state
                }

                # Prepara i dati dei sottogruppi, se necessari
                  subgroup_data <- NULL
                  if (!is.null(self$options$subgroup)) {
                      if ("subgroup" %in% names(self$dataProc)) {
                          subgroup_data <- self$dataProc$subgroup
                      } else if (self$options$subgroup %in% names(self$dataProc)) {
                          subgroup_data <- self$dataProc[[self$options$subgroup]]
                      }
                  }

                  # Usa il servizio mfsc per generare il grafico funnel
                  if (!is.null(private$.mfsc)) {
                      result <- private$.mfsc$mzFunnelPlot(
                          plotData,
                          self$options,
                          subgroup_data = subgroup_data
                      )
                      return(result)
                  } else {
                      jmvcore::reject(.("Funnel Plot: MetaforService not available."), code = "")
                      return(FALSE)
                  }
            },

            ## Pcurve Plot
            .mzPcurvePlot = function(image, ...) {
                if (is.null(image$state) || !inherits(image$state, "rma")) {
                    plotData <- self$modelResults
                } else {
                    plotData <- image$state
                }

                if (is.null(plotData)) {
                    jmvcore::reject(.("Model results not available."), code = "")
                    return(FALSE)
                }

                # Chiamata alla funzione pubblica nel file mzPlotService.R
                mzPcurvePlot(plotData, self)

                return(TRUE)
            },

            ## Influence Plot
            .mzInfDiagPlot = function(image, ...) {
                plotData <- image$state

                # Controlla la validitC  dell'oggetto rma
                if (is.null(plotData) || !inherits(plotData, "rma")) {
                    return(FALSE)
                }

                # Controlla il numero minimo di studi per la diagnostica
                if (plotData$k < 3) {
                    return(FALSE)
                }

                tryCatch(
                    {
                        infDiagPlot <- mzDiagnosticInfluence(self, plotData)
                        plot(infDiagPlot)

                        return(TRUE)
                    },
                    error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:1143")
                        return(FALSE)
                    }
                )
            },

            ## Q-Q Plot
            .mzInfQQPlot = function(image, ...) {
                plotData <- image$state

                # Controlla la validitC  dell'oggetto rma
                if (is.null(plotData) || !inherits(plotData, "rma")) {
                    return(FALSE)
                }

                # Controlla il numero minimo di studi
                if (plotData$k < 3) {
                    return(FALSE)
                }

                tryCatch(
                    {
                        # Genera il grafico Q-Q
                        qqnorm(plotData, grid = TRUE)

                        return(TRUE)
                    },
                    error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:1171")
                        return(FALSE)
                    }
                )
            },

            ## Grafico delle verosimiglianze dei singoli studi
            .mzLhPlot = function(image, ...) {
                plotData <- image$state
                if (is.null(plotData) || !inherits(plotData, "rma")) {
                    return(FALSE)
                }
                if (plotData$k < 2) {
                    return(FALSE)
                }
                tryCatch(
                    {
                        metafor::llplot(
                            measure = "GEN",
                            yi = plotData$yi,
                            vi = plotData$vi,
                            refline = NA,
                            lwd = 1
                        )
                        abline(
                            v = as.numeric(plotData$b),
                            col = "firebrick",
                            lwd = 1.5,
                            lty = "dashed"
                        )
                        mtext(.("Likelihood Distribution: GEN (Normal)"),
                            side = 3, adj = 1, cex = 0.80, col = "#3E6DA9", line = 0.3
                        )
                        return(TRUE)
                    },
                    error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:1207")
                        return(FALSE)
                    }
                )
            },

            ## Tost Plot
            .mzTostPlot = function(image, ...) {
                plotData <- image$state

                # Controlla la validitC  dell'oggetto rma
                if (is.null(plotData) || !inherits(plotData, "rma")) {
                    return(FALSE)
                }

                # Controlla il numero minimo di studi per la diagnostica
                if (plotData$k < 3) {
                    return(FALSE)
                }

                tostPlot <- TOSTER::TOSTmeta(
                    ES = plotData$beta[1],
                    se = plotData$se[1],
                    low_eqbound_d = self$options$tostLower,
                    high_eqbound_d = self$options$tostUpper,
                    alpha = self$options$tostAlpha,
                    plot = TRUE,
                    verbose = FALSE
                )

                return(TRUE)
            },

            ## Scatter Plot/Bubble Plot (meta-regressione)
            .mzScatterPlot = function(image, ...) {
                mzDiagnosticScatterRender(self, image)
            },

            # Grafico radiale
            .mzRadialPlot = function(image, ...) {
              plotData <- image$state
              if (is.null(plotData) || !inherits(plotData, "rma")) return(FALSE)
              if (plotData$k < 2) return(FALSE)

              # Opzioni
              center_arg <- isTRUE(self$options$radialPlotCenter)
              steps_arg  <- self$options$radialPlotSteps

              # Back-transform (utile per ROM - Log Ratio of Means)
              transf_arg <- switch(self$options$radialPlotTransf,
                                   "none" = NULL,
                                   "exp"  = exp,
                                   NULL
              )

              tryCatch(
                {
                  if (!is.null(transf_arg)) {
                    metafor::radial(plotData,
                                    center = center_arg, steps = steps_arg,
                                    transf = transf_arg, las = 1, col = "#3E6DA9",
                                    col.lab = "#3E6DA9", font.lab = 3
                    )
                  } else {
                    metafor::radial(plotData,
                                    center = center_arg, steps = steps_arg,
                                    las = 1, col = "#3E6DA9", col.lab = "#3E6DA9", font.lab = 3
                    )
                  }

                  # Linea dell'estimate (SEMPRE sulla scala originale, senza transf_arg!)
                  abline(a = 0, b = as.numeric(plotData$b), col = "firebrick", lwd = 1.5, lty = "dashed")

                  mtext(sprintf(.("Radial Plot [%s]"), self$options$modelmea),
                        side = 3, adj = 1, cex = 0.85, col = "#3E6DA9", line = 0.3
                  )
                  return(TRUE)
                },
                error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:1286")
                  return(FALSE)
                }
              )
            },

            ## Baujat Plot
            .mzBaujatPlot = function(image, ...) {
                plotData <- image$state
                if (is.null(plotData) || !inherits(plotData, "rma")) {
                    return(FALSE)
                }
                if (plotData$k < 2) {
                    return(FALSE)
                }

                symbol_arg <- switch(self$options$baujatPlotSymbol,
                    "ids"  = "ids",
                    "slab" = "slab",
                    "pch"  = 19
                )
                grid_arg <- isTRUE(self$options$baujatPlotGrid)

                tryCatch(
                    {
                        mzBaujat(plotData,
                            symbol = symbol_arg, grid = grid_arg,
                            las = 1, bty = "l", col = "#3E6DA9",
                            bg = "#3E6DA9",
                            col.lab = "#3E6DA9", font.lab = 3
                        )
                        mtext(sprintf(.("Baujat Plot [%s]"), self$options$modelmea),
                            side = 3, adj = 1, cex = 0.85, col = "#3E6DA9", line = 0.3
                        )
                        return(TRUE)
                    },
                    error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:1323")
                        return(FALSE)
                    }
                )
            },

            ## Gosh Plot
            .mzGoshPlot = function(image, ...) {
                plotData <- image$state
                if (is.null(plotData) || !inherits(plotData, "rma")) {
                    return(FALSE)
                }
                if (plotData$k < 3) {
                    return(FALSE)
                }

                het_arg <- self$options$goshPlotHet
                out_arg <- self$options$goshPlotOut

                tryCatch(
                    {
                        res_gosh <- mzDiagnosticGosh(self, plotData)

                        if (out_arg > 0 && out_arg <= plotData$k) {
                            plot(res_gosh,
                                het = het_arg, out = out_arg,
                                breaks = 50,
                                las = 1, bty = "l", col = c("#3E6DA9", "#E87352")
                            )
                        } else {
                            plot(res_gosh,
                                het = het_arg,
                                breaks = 50,
                                las = 1, bty = "l", col = "#3E6DA9"
                            )
                        }
                        mtext(sprintf(.("GOSH Plot [%s]"), self$options$modelmea),
                            side = 3, adj = 1, cex = 0.85, col = "#3E6DA9", line = 0.3
                        )
                        return(TRUE)
                    },
                    error = function(e) {
                        mark("[ERR] caught", conditionMessage(e), ui_log_obj=self$results$debugLog, .loc = "mzMeanDifferences.b.R:1365")
                        return(FALSE)
                    }
                )
            },

            ## Grafico e tabella LOO indipendenti, risultati statistici condivisi.
            .mzLeaveOneOutClassicPlot = function(image, ...) {
                mzRenderLeaveOneOutClassic(self,image)
            },

            .mzCumulativePlot = function(image, ...) {
                mzRenderCumulativeClassic(self,image)
            },

            .mzLeaveOneOut = function() {
                mzRunLeaveOneOut(self)
            },

            ## Gestire la creazione e l'iniezione del report HTML
            .mzCreateHtmlReport = function(rma_model) {
                jinfo("[REPORT] begin", .loc = "mzMeanDifferences.b.R:1386")
              
                output_html <- self$results$OutputHtml

              # Il toast e gestito nella UI; nessun avviso HTML/SVG nei Results.
              if (length(self$options$moderator) > 0) {
                output_html$setVisible(FALSE)
                if (isTRUE(nzchar(output_html$content))) output_html$setContent('')
                return(invisible(NULL))
              }
              output_html$setVisible(TRUE)

              # Se non c'è un moderatore, procede con la generazione del report.
              # Controllo di sicurezza sul modello passato alla funzione
              if (is.null(rma_model)) {
                error_msg <- "<div style='color: #e74c3c; padding: 15px;'><strong>Error:</strong> No valid model provided for reporting.</div>"
                .mzSetHtmlResult(self, output_html, 'report', error_msg)
                return()
              }

              # Richiama il servizio globale per generare il report HTML a schede (tabs)
              # Assicurati che mzHtmlRepoService.R sia caricato/disponibile nel modulo
              if (exists("mz_create_report_html") && is.function(mz_create_report_html)) {

                report_content <- mz_create_report_html(rma_model, self=self)
                .mzSetHtmlResult(self, output_html, 'report', report_content)

              } else {
                # Fallback in caso la funzione di servizio non sia stata caricata
                fallback_msg <- .("<div style='color: #e74c3c; padding: 15px;'><strong>Error:</strong> Text reporting service (mz_create_report_html) is not available.</div>")
                .mzSetHtmlResult(self, output_html, 'report', fallback_msg)
              }
            }
        )
    )
}
