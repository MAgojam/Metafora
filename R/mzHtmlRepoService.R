#' ==============================================================================
#' mzHtmlRepoService.R
#' Servizio globale per la generazione del report HTML (Methods/Results) a
#' partire da un oggetto rma.uni.
#'
#' NOTE ARCHITETTURALI:
#' - NON usa metafor::reporter(): dipende da rmarkdown::render() -> pandoc,
#'   assente nell'R engine embedded di jamovi. La logica testuale/statistica
#'   di reporter.rma.uni.r e' portata qui in R puro.
#' - Nessuna dipendenza da MathJax/LaTeX: la matematica e' resa con
#'   Unicode/HTML (τ, α, μ, <sup>).
#' - Nessun iframe, nessun <script>: il contenuto va DIRETTAMENTE in
#'   setContent(). L'accordion usa <details>/<summary> nativi (vedi
#'   .mzHtmlAccordionStart/End in mzHtmlService.R) e i tab Methods/Results
#'   sono CSS-only (radio+label+sibling selector, vedi .mzHtmlTabsBuild).
#'   Stesso principio gia' validato per TOST/TES/p-uniform/Selmodel: nessun
#'   JS -> nessun problema di CSP, nessun resize da gestire.
#'
#' Condiviso da tutti i sei modelli di Metafora (dichotomous, correlation,
#' meandiff, alpha, proportion, generic effectsize).
#'
#' Entry point pubblico: mz_create_report_html(rma_model)
#' ==============================================================================


# ------------------------------------------------------------------------------
# 1. Tabella citazioni statica (verificata) + formattatore
# ------------------------------------------------------------------------------

.mz_bib_static <- list(
  cochran1954      = "Cochran, 1954",
  higgins2002      = "Higgins & Thompson, 2002",
  riley2011        = "Riley et al., 2011",
  begg1994         = "Begg & Mazumdar, 1994",
  sterne2005       = "Sterne & Egger, 2005",
  viechtbauer2005  = "Viechtbauer, 2005",
  viechtbauer2010a = "Viechtbauer, 2010",
  viechtbauer2010b = "Viechtbauer & Cheung, 2010",
  rcore2020        = "R Core Team, 2020",
  dersimonian1986  = "DerSimonian & Laird, 1986",
  hunter1990       = "Hunter & Schmidt, 1990",
  hedges1985       = "Hedges & Olkin, 1985",
  sidik2005        = "Sidik & Jonkman, 2005",
  paule1982        = "Paule & Mandel, 1982",
  knapp2003        = "Knapp & Hartung, 2003",
  dersimonian2007  = "DerSimonian & Kacker, 2007"
  # Chiavi non verificate con sufficiente certezza (es. brannick2019,
  # viechtbauer2021, morris1983, hardy1996) sono volutamente omesse: la
  # citazione viene semplicemente saltata invece di rischiare un'attribuzione
  # non corretta.
)

#' Formatta una o piu' citekey (stile "key1; key2") in "(Cit. 1; Cit. 2)".
#' Le chiavi non presenti nella tabella statica vengono omesse in silenzio.
.mz_cite <- function(keys_str) {
  if (!nzchar(keys_str)) return("")
  keys <- trimws(strsplit(keys_str, ";\\s*")[[1]])
  cites <- vapply(keys, function(k) {
    v <- .mz_bib_static[[k]]
    if (is.null(v)) "" else v
  }, character(1))
  cites <- cites[nzchar(cites)]
  if (length(cites) == 0) return("")
  sprintf("(%s)", paste(cites, collapse = "; "))
}


# ------------------------------------------------------------------------------
# 2. Formattatori numerici (equivalenti leggeri a fmtx di metafor)
#    I valori sono avvolti in <span class="mz-report-stat"> per lo styling
#    (classe definita in .mzHtmlTabsCss(), mzHtmlService.R).
# ------------------------------------------------------------------------------

.mz_fmtx <- function(val, digits = 4) {
  if (is.null(val) || length(val) == 0 || is.na(val))
    return("<span class='mz-report-stat'>NA</span>")
  sprintf("<span class='mz-report-stat'>%s</span>",
          formatC(round(val, digits), format = "f", digits = digits))
}

.mz_fpval <- function(p, pdigits = 4) {
  if (is.na(p)) return("<span class='mz-report-stat'>p = NA</span>")
  if (p < 10^(-pdigits)) {
    sprintf("<span class='mz-report-stat'>p &lt; %s</span>",
            formatC(round(10^(-pdigits), pdigits), format = "f", digits = pdigits))
  } else {
    sprintf("<span class='mz-report-stat'>p = %s</span>",
            formatC(round(p, pdigits), format = "f", digits = pdigits))
  }
}

.mz_get_digits <- function(x) {
  defaults <- list(est = 4, se = 4, test = 4, pval = 4, ci = 4, var = 4, het = 4)
  d <- x$digits
  if (is.list(d)) {
    for (nm in names(defaults)) {
      if (is.null(d[[nm]]) || !is.numeric(d[[nm]])) d[[nm]] <- defaults[[nm]]
    }
    return(d)
  }
  if (is.numeric(d) && length(d) == 1)
    return(setNames(as.list(rep(d, length(defaults))), names(defaults)))
  defaults
}


# ------------------------------------------------------------------------------
# 3. Informazioni di modello (tipo, nome, metodo tau^2, riferimento tau^2)
# ------------------------------------------------------------------------------

.mz_model_info <- function(x) {

  model <- if (is.element(x$method, c("FE", "EE", "CE"))) x$method else "RE"

  model.name <- c(
    FE = "fixed-effects", EE = "equal-effects",
    CE = "common-effects", RE = "random-effects"
  )[model]

  tau2.method <- c(
    HS = "Hunter-Schmidt", HSk = "k-corrected Hunter-Schmidt",
    HE = "Hedges'", CO = "Hedges'", VC = "Hedges'",
    DL = "DerSimonian-Laird", GENQ = "generalized Q-statistic",
    GENQM = "(median-unbiased) generalized Q-statistic",
    SJ = "Sidik-Jonkman", ML = "maximum-likelihood",
    REML = "restricted maximum-likelihood", EB = "empirical Bayes",
    PM = "Paule-Mandel", MP = "Paule-Mandel",
    PMM = "(median-unbiased) Paule-Mandel"
  )[x$method]
  if (is.na(tau2.method)) tau2.method <- "restricted maximum-likelihood"
  if (identical(mzEstimatorId(x), "SJ2")) tau2.method <- "Sidik-Jonkman (HE initialization)"

  tau2_ref_map <- c(
    HS = "hunter1990; viechtbauer2005", HSk = "hunter1990; viechtbauer2005",
    HE = "hedges1985", CO = "hedges1985", VC = "hedges1985",
    DL = "dersimonian1986", GENQ = "dersimonian2007",
    SJ = "sidik2005", REML = "viechtbauer2005",
    PM = "paule1982", MP = "paule1982"
  )
  tau2.ref <- unname(tau2_ref_map[x$method])
  if (is.na(tau2.ref)) tau2.ref <- ""

  list(model = model, model.name = model.name,
       tau2.method = tau2.method, tau2.ref = tau2.ref)
}

# Etichette locali dei codici supportati, verificate contro metafor dello snapshot.
.mz_measure_labels <- c(OR = "log odds ratio", RR = "log risk ratio", PETO = "log odds ratio", 
RD = "risk difference", AS = "arcsine difference", 
COR = "correlation coefficient", UCOR = "correlation coefficient", 
ZCOR = "fisher's z transformed correlation coefficient", MD = "mean difference", 
SMD = "standardized mean difference", SMDH = "standardized mean difference", 
SMD1 = "standardized mean difference", SMD1H = "standardized mean difference", 
ROM = "log ratio of means", PR = "proportion", PLN = "log proportion", 
PLO = "log odds", PRZ = "probit transformed proportion", PAS = "arcsine transformed proportion", 
PFT = "double arcsine transformed proportion", ARAW = "cronbach's alpha", 
AHW = "transformed cronbach's alpha", ABT = "transformed cronbach's alpha", 
GEN = "observed outcome")

# Etichetta del report senza dipendenze dall'API privata della libreria.
.mz_measure_label <- function(x) {
  measure <- unname(.mz_measure_labels[x$measure])
  if (length(measure) != 1L || is.na(measure)) measure <- tolower(x$measure)

  measure <- sub("observed outcome", "outcome", measure)
  measure <- sub("fisher's z", "Fisher r-to-z", measure)
  measure <- sub("yule", "Yule", measure)
  measure <- sub("freeman", "Freeman", measure)
  measure <- sub("tukey", "Tukey", measure)
  measure <- sub("log ratio of means", "response ratio", measure)
  measure <- sub("cronbach", "Cronbach", measure)

  measure
}


# ------------------------------------------------------------------------------
# 4. Generazione testo Methods (HTML/Unicode, niente LaTeX)
# ------------------------------------------------------------------------------

.mz_generate_methods <- function(x, digits, measure, info, alpha, self=NULL) {
  self <- .mzTranslationSelf(self)
  
  parts <- c()

  if (x$measure != "GEN")
    parts <- c(parts, sprintf(.("The analysis was carried out using the %s as the outcome measure."), measure))

  parts <- c(parts, sprintf(.("The analysis used the %s model."),
                             self$options$translate(info$model.name)))

  if (info$model == "RE") {
    tau2_cite <- .mz_cite(info$tau2.ref)
    tau2_cite <- if (nzchar(tau2_cite)) paste0(" ", tau2_cite) else ""
    parts <- c(parts, sprintf(
      .("The amount of heterogeneity (i.e., \u03c4<sup>2</sup>) was estimated using the %s estimator%s."),
      self$options$translate(info$tau2.method), tau2_cite))
  }

  qtest_cite <- .mz_cite("cochran1954")
  i2_cite <- .mz_cite("higgins2002")

  if (is.element(info$model, c("FE", "EE", "CE"))) {
    parts <- c(parts, sprintf(
      .("The Q-test for heterogeneity %s and the I<sup>2</sup> statistic %s are reported."),
      qtest_cite, i2_cite))
  }

  if (info$model == "RE") {
    parts <- c(parts, sprintf(
      .("In addition to the estimate of \u03c4<sup>2</sup>, the Q-test for heterogeneity %s and the I<sup>2</sup> statistic %s are reported."),
      qtest_cite, i2_cite))
    parts <- c(parts, sprintf(
      .("In case any amount of heterogeneity is detected (i.e., \u03c4<sup>2</sup> &gt; 0, regardless of the results of the Q-test), a prediction interval for the true outcomes is also provided %s."),
      .mz_cite("riley2011")))
  }

  if (x$test == "knha")
    parts <- c(parts, sprintf(
      .("Tests and confidence intervals were computed using the Knapp and Hartung method %s."),
      .mz_cite("knapp2003")))

  parts <- c(parts, sprintf(
    .("Studentized residuals and Cook's distances are used to examine whether studies may be outliers and/or influential in the context of the model %s."),
    .mz_cite("viechtbauer2010b")))

  alphaStr <- formatC(alpha, format = "f", digits = 3)
  parts <- c(parts, sprintf(
    .("Studies with a studentized residual larger than the 100 \u00d7 (1 \u2212 %s/(2 \u00d7 k))th percentile of a standard normal distribution are considered potential outliers (i.e., using a Bonferroni correction with two-sided \u03b1 = %s for k studies included in the meta-analysis)."),
    alphaStr, alphaStr))

  parts <- c(parts,
    .("Studies with a Cook's distance larger than the median plus six times the interquartile range of the Cook's distances are considered to be influential."))

  parts <- c(parts, sprintf(
    .("The rank correlation test %s and the regression test %s, using the standard error of the observed outcomes as predictor, are used to check for funnel plot asymmetry."),
    .mz_cite("begg1994"), .mz_cite("sterne2005")))

  parts <- c(parts, sprintf(
    .("The analysis was carried out using R (version %s) %s and the <strong>metafor</strong> package (version %s) %s."),
    getRversion(), .mz_cite("rcore2020"), x$version, .mz_cite("viechtbauer2010a")))

  paste0("<p>", paste(parts, collapse = " "), "</p>")
}


# ------------------------------------------------------------------------------
# 5. Generazione testo Results (HTML/Unicode, niente LaTeX)
# ------------------------------------------------------------------------------

.mz_generate_results <- function(x, digits, measure, info, level, crit, self=NULL) {
  self <- .mzTranslationSelf(self)

  paragraphs <- c()

  # --- paragrafo 1: stima principale --------------------------------------
  p1 <- c()

  p1 <- c(p1, sprintf(.("A total of k=%s studies were included in the analysis."),
                       sprintf("<span class='mz-report-stat'>%d</span>", x$k)))

  p1 <- c(p1, sprintf(
    .("Observed %s values ranged from %s to %s,"),
    measure, .mz_fmtx(min(x$yi), digits$est), .mz_fmtx(max(x$yi), digits$est)))

  pct_pos <- mean(x$yi > 0)
  p1 <- c(p1, sprintf(
    .("with the majority of estimates being %s (%d%%)."),
    ifelse(pct_pos > 0.50, .("positive"), .("negative")),
    ifelse(pct_pos > 0.50, round(100 * pct_pos), round(100 * (1 - pct_pos)))))

  est_symbol <- ifelse(is.element(info$model, c("FE", "EE", "CE")), "\u03b8", "\u03bc")
  p1 <- c(p1, sprintf(
    .("The estimated average %s based on the %s model was %s = %s (%s%% CI: %s to %s)."),
    measure, info$model.name, est_symbol, .mz_fmtx(c(x$beta), digits$est),
    level, .mz_fmtx(x$ci.lb, digits$ci), .mz_fmtx(x$ci.ub, digits$ci)))

  p1 <- c(p1, sprintf(
    .("Therefore, the average outcome %s significantly from zero (%s = %s, %s)."),
    ifelse(x$pval > 0.05, .("did not differ"), .("differed")),
    ifelse(x$test == "z", "z", sprintf("t(%d)", x$k - 1)),
    .mz_fmtx(x$zval, digits$test),
    .mz_fpval(x$pval, digits$pval)))

  p1 <- c(p1, .("A forest plot showing the observed outcomes and the estimate based on the model is provided in the accompanying Forest Plot output."))

  paragraphs <- c(paragraphs, paste(p1, collapse = " "))

  # --- paragrafo 2: eterogeneita' + intervallo di previsione --------------
  p2 <- c()

  het_intro <- if (x$QEp > 0.10) {
    .("According to the Q-test, there was no significant amount of heterogeneity in the true outcomes")
  } else if (x$QEp > 0.05) {
    .("The Q-test for heterogeneity was not significant, but some heterogeneity may still be present in the true outcomes")
  } else {
    .("According to the Q-test, the true outcomes appear to be heterogeneous")
  }

  het_stats <- sprintf("(Q(%d) = %s, %s", x$k - 1, .mz_fmtx(x$QE, digits$test), .mz_fpval(x$QEp, digits$pval))
  if (info$model == "RE")
    het_stats <- paste0(het_stats, sprintf(", \u03c4<sup>2</sup> = %s", .mz_fmtx(x$tau2, digits$var)))
  het_stats <- paste0(het_stats, sprintf(", I<sup>2</sup> = %s%%).", .mz_fmtx(x$I2, digits$het)))

  p2 <- c(p2, paste(het_intro, het_stats))

  pred_callout <- NULL
  if (info$model == "RE" && x$tau2 > 0) {
    predres <- tryCatch(predict(x), error = function(e) NULL)
    if (!is.null(predres)) {
      p2 <- c(p2, sprintf(
        .("A %s%% prediction interval for the true outcomes is given by %s to %s."),
        level, .mz_fmtx(predres$pi.lb, digits$ci), .mz_fmtx(predres$pi.ub, digits$ci)))

      pred_callout <- if (c(x$beta) > 0 && predres$pi.lb < 0) {
        .("Hence, although the average outcome is estimated to be positive, in some studies the true outcome may in fact be negative.")
      } else if (c(x$beta) < 0 && predres$pi.ub > 0) {
        .("Hence, although the average outcome is estimated to be negative, in some studies the true outcome may in fact be positive.")
      } else {
        .("Hence, even though there may be some heterogeneity, the true outcomes of the studies are generally in the same direction as the estimated average outcome.")
      }
    }
  }

  para2_html <- paste0("<p>", paste(p2, collapse = " "), "</p>")
  if (!is.null(pred_callout))
    para2_html <- paste0(para2_html, sprintf("<div class='mz-report-callout'>%s</div>", pred_callout))

  paragraphs <- c(paragraphs, para2_html)

  # --- paragrafo 3: pesi elevati, outlier, influenza -----------------------
  p3 <- c()

  w <- tryCatch(weights(x), error = function(e) NULL)
  if (!is.null(w)) {
    largeweight <- w / 100 >= 3 / x$k
    if (any(largeweight)) {
      nms <- names(largeweight)[largeweight]
      if (is.null(nms) || !any(nzchar(nms))) nms <- paste("study", which(largeweight))
      n_lw <- sum(largeweight)
      lw_lbl <- if (n_lw == 1) .("One study") else if (n_lw == 2) .("Two studies") else .("Several studies")
      p3 <- c(p3, sprintf(
        .("%s (%s) had relatively large weight%s compared to the rest of the studies (i.e., weight \u2265 3/k, so a weight at least 3 times as large as having equal weights across studies)."),
        lw_lbl, paste(nms, collapse = "; "), ifelse(n_lw == 1, "", "s")))
    }
  }

  infres <- tryCatch(mzInfluence(x), error = function(e) NULL)
  if (!is.null(infres)) {
    zi <- infres$inf$rstudent
    abszi <- abs(zi)
    slabs <- infres$inf$slab

    if (all(abszi < crit, na.rm = TRUE)) {
      p3 <- c(p3, sprintf(
        .("An examination of the studentized residuals revealed that none of the studies had a value larger than \u00b1%s and hence there was no indication of outliers in the context of this model."),
        .mz_fmtx(crit, digits$test)))
    } else {
      out_idx <- which(abszi >= crit & !is.na(abszi))
      n_out <- length(out_idx)
      out_lbl <- if (n_out == 1) .("one study") else if (n_out == 2) .("two studies") else .("several studies")
      p3 <- c(p3, sprintf(
        .("An examination of the studentized residuals revealed that %s (%s) had %s larger than \u00b1%s and may be potential outlier%s in the context of this model."),
        out_lbl, paste(slabs[out_idx], collapse = "; "),
        ifelse(n_out == 1, "a value", .("values")), .mz_fmtx(crit, digits$test),
        ifelse(n_out == 1, "", "s")))
    }

    cook_d <- infres$inf$cook.d
    is_infl <- cook_d > (median(cook_d, na.rm = TRUE) + 6 * IQR(cook_d, na.rm = TRUE))
    if (all(!is_infl, na.rm = TRUE)) {
      p3 <- c(p3, .("According to the Cook's distances, none of the studies could be considered to be overly influential."))
    } else {
      infl_idx <- which(is_infl & !is.na(is_infl))
      n_infl <- length(infl_idx)
      infl_lbl <- if (n_infl == 1) .("one study") else if (n_infl == 2) .("two studies") else .("several studies")
      p3 <- c(p3, sprintf(
        .("According to the Cook's distances, %s (%s) could be considered to be overly influential."),
        infl_lbl, paste(slabs[infl_idx], collapse = "; ")))
    }
  }

  if (length(p3) > 0)
    paragraphs <- c(paragraphs, paste0("<p>", paste(p3, collapse = " "), "</p>"))

  # --- paragrafo 4: bias di pubblicazione -----------------------------------
  ranktest_res <- tryCatch(suppressWarnings(ranktest(x)), error = function(e) NULL)
  regtest_res  <- tryCatch(mzRegtest(x), error = function(e) NULL)

  if (!is.null(ranktest_res) && !is.null(regtest_res)) {
    rp <- ranktest_res$pval
    gp <- regtest_res$pval

    p4 <- .("A funnel plot of the estimates is provided in the accompanying Funnel Plot output.")

    if (rp > 0.05 && gp > 0.05) {
      p4 <- paste(p4, sprintf(
        .("Neither the rank correlation nor the regression test indicated any funnel plot asymmetry (%s and %s, respectively)."),
        .mz_fpval(rp, digits$pval), .mz_fpval(gp, digits$pval)))
    } else if (rp <= 0.05 && gp <= 0.05) {
      p4 <- paste(p4, sprintf(
        .("Both the rank correlation and the regression test indicated potential funnel plot asymmetry (%s and %s, respectively)."),
        .mz_fpval(rp, digits$pval), .mz_fpval(gp, digits$pval)))
    } else if (rp > 0.05 && gp <= 0.05) {
      p4 <- paste(p4, sprintf(
        .("The regression test indicated funnel plot asymmetry (%s) but not the rank correlation test (%s)."),
        .mz_fpval(gp, digits$pval), .mz_fpval(rp, digits$pval)))
    } else {
      p4 <- paste(p4, sprintf(
        .("The rank correlation test indicated funnel plot asymmetry (%s) but not the regression test (%s)."),
        .mz_fpval(rp, digits$pval), .mz_fpval(gp, digits$pval)))
    }

    paragraphs <- c(paragraphs, paste0("<p>", p4, "</p>"))
  }

  paste(paragraphs, collapse = "\n")
}


# ------------------------------------------------------------------------------
# 6. Messaggi di errore standard
# ------------------------------------------------------------------------------

.mz_report_error_html <- function(msg) {
  mark('[HTML/REPORT] unavailable',msg, .loc = "mzHtmlRepoService.R:426")
  sprintf(
    "<div style='color: #e74c3c; padding: 15px;'><strong>Error:</strong> %s</div>",
    htmlSpecialChars(msg)
  )
}


# ------------------------------------------------------------------------------
# 7. Entry point pubblico
# ------------------------------------------------------------------------------

#' Genera il report HTML (accordion + tab Methods/Results, entrambi CSS-only,
#' zero JS) a partire da un oggetto rma.uni, da iniettare direttamente in un
#' oggetto Html risultati di jamovi via setContent().
#'
#' Riusa .mzHtmlStyles() / .mzHtmlAccordionStart() / .mzHtmlAccordionEnd() /
#' .mzHtmlTabsCss() / .mzHtmlTabsBuild() da mzHtmlService.R (stesso file R/,
#' incluso automaticamente da jmvtools).
#'
#' @param rma_model Oggetto risultante da metafor::rma() (classe rma.uni)
#' @param color Colore accento opzionale per l'accordion (es. "#eca038").
#'              Se NULL usa il default blu jamovi. Vedi .mzHtmlAccordionStart().
#'
#' @return Stringa HTML pronta per setContent()
mz_create_report_html <- function(rma_model, color = NULL, self=NULL) {
  self <- .mzTranslationSelf(self)
  
  if (is.null(rma_model))
    return(.mz_report_error_html(.("No valid model provided for reporting.")))
  if (!inherits(rma_model, "rma.uni"))
    return(.mz_report_error_html(sprintf(
      .("Unsupported model class '%s' for reporting."), paste(class(rma_model), collapse = ", "))))

  x <- rma_model

  if (!is.element(x$test, c("z", "knha")))
    return(.mz_report_error_html(.("Reporting is only available when test='z' or test='knha'.")))
  if (!x$weighted)
    return(.mz_report_error_html(.("Reporting is not available for unweighted models.")))
  if (!is.null(x$weights))
    return(.mz_report_error_html(.("Reporting is not available for models with custom weights.")))
  if (!x$int.only)
    return(.mz_report_error_html(.("Reporting is not currently available for models with moderators.")))
  if (x$k == 1L)
    return(.mz_report_error_html(.("Reporting is not available when k = 1.")))

  result_html <- tryCatch({

    digits <- .mz_get_digits(x)
    measure <- .mz_measure_label(x)
    info <- .mz_model_info(x)
    
    # Il modello rma conserva alpha (es. 0.05), non la copertura del CI (95).
    alpha <- x$level
    level <- 100 * (1 - alpha)
    crit <- qnorm(alpha / (2 * x$k), lower.tail = FALSE)
    
    methods_html <- .mz_generate_methods(x, digits, measure, info, alpha, self=self)
    results_html <- .mz_generate_results(x, digits, measure, info, level, crit, self=self)

    # Chiave univoca per i name/id dei radio (evita collisioni se piu'
    # blocchi tab compaiono nella stessa pagina risultati)
    scope <- .mzHtmlScope(self)
    if (is.null(scope)) scope <- basename(tempfile('standalone-'))
    group_id <- paste0('mz-report-',paste(sprintf('%02x',as.integer(charToRaw(enc2utf8(scope)))),collapse=''))

    tabs_html <- .mzHtmlTabsBuild(
      stats::setNames(list(methods_html, results_html),c(.("Methods"),.("Results"))),
      group_id = group_id
    )

    paste0(
      .mzHtmlStyles(),
      .mzHtmlTabsCss(),
      .mzHtmlAccordionStart(.("Text-Based Report"), color = color),
      tabs_html,
      .mzHtmlAccordionEnd()
    )

  }, error = function(e) {
    .mz_report_error_html(sprintf(.("Error generating the report: %s"), conditionMessage(e)))
  })

  return(result_html)
}
