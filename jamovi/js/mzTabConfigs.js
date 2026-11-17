// jamovi/js/mzTabConfigs.js
'use strict';

// Marcatore di estrazione: il catalogo viene applicato all'istanza UI, non al caricamento.
const _ = text => text;

// GENERATORI DI TAB COMUNI (Modularizzazione)
// Questi generatori permettono di definire la struttura una sola volta per tutti i modelli.
// Vengono mantenuti gli ID originali per garantire la compatibilità con main.js.

// ================================================================
// INFO & NOTES — generazione HTML strutturato
// ================================================================
// Sostituisce i vecchi blocchi a <br>/bullet manuali con sezioni
// coerenti in tutti i tab e nei sei modelli. Stile in css.js
// (.info-section/.info-list/.info-item/.info-badge/.info-sublist).

const INFO_NOTES_SECTIONS = new Map();

function buildInfoNotes(sections, translate = (text) => text) {
  const html = sections.map((sec) => {
    const itemsHtml = sec.items.map((it) => {
      const badgeHtml = it.badge ? `<span class="info-badge">${translate(it.badge)}</span>` : "";
      const subHtml = it.sub
        ? `<ul class="info-sublist">${it.sub.map(
            (s) => `<li><strong>${translate(s.term)}</strong>${s.desc ? ": " + translate(s.desc) : ""}</li>`
          ).join("")}</ul>`
        : "";
      return `
        <div class="info-item">
          <div class="info-term">${translate(it.term)}</div>
          <div class="info-desc">${translate(it.desc)}</div>
          ${subHtml}
        </div>`;
    }).join("");

    // sec.variant aggiunge una classe modificatrice (es. "dev") per
    // distinguere visivamente sezioni secondarie/avanzate dalle normali
    // sezioni informative — vedi .info-section-dev in css.js.
    const variantClass = sec.variant ? ` info-section-${sec.variant}` : "";
    const iconHtml = sec.icon ? `<span class="info-section-icon">${sec.icon}</span>` : "";

    return `
      <div class="info-section${variantClass}">
        <div class="info-section-title">${iconHtml}${translate(sec.title)}</div>
        <div class="info-list">${itemsHtml}</div>
      </div>`;
  }).join("");
  // Conserva il testo sorgente per traduzione e verifica della copertura opzioni.
  INFO_NOTES_SECTIONS.set(html, sections);
  return html;
}

// Converte un elenco misure {code, title, desc} nel formato
// "Title (CODE)" usato uniformemente da tutti i modelli.
function buildMeasureItems(measures) {
  return measures.map((m) => ({
    term: `${m.title} (${m.code})`,
    options: ["modelmea"],
    desc: m.desc,
  }));
}

// Rappresentazione visiva di una combinazione di tasti (stile "kbd"),
// usata nella sezione Developer del tab Save per documentare gli
// shortcut del logging (CTRL+SHIFT+L / CTRL+ALT+L, vedi mzDebug.js).
function kbdCombo(keys) {
  return keys
    .map((k) => `<span class="info-kbd">${k}</span>`)
    .join('<span class="info-kbd-plus">+</span>');
}

// Icona "prompt" (>_) per il titolo delle sezioni Developer — stroke-based,
// coerente con lo stile delle altre icone SVG del modulo (mzDebug.R).
const DEV_SECTION_ICON =
  '<svg viewBox="0 0 24 24" width="12" height="12" style="vertical-align:-1px" ' +
  'fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round">' +
  '<polyline points="4 17 10 11 4 5"></polyline><line x1="12" y1="19" x2="20" y2="19"></line></svg>';

// Sezione "Inference" — identica in tutti i sei modelli
const INFERENCE_SECTION = {
  title: _("Inference"),
  items: [
    {
      term: _("Confidence Interval"),
      options: ["level"],
      desc: _("Confidence level for the model estimates (default 95%). Separate diagnostic settings may use their own significance level."),
    },
    {
      term: _("Select Tests"),
      options: ["testType"],
      desc: _("Inference for the model coefficients and their confidence intervals:"),
      sub: [
        { term: _("z test"), desc: _("Default large-sample approximation, standard for random- and fixed-effect models") },
        { term: _("t test"), desc: _("Uses a t-distribution with df = k − p (studies minus parameters)") },
        { term: _("Knapp–Hartung"), desc: _("Uses an adjusted coefficient variance and a t-distribution; intervals are not necessarily wider") },
        { term: _("ad hoc"), desc: _("Knapp–Hartung with the variance adjustment factor constrained to at least 1") },
      ],
    },
  ],
};

// Sezione "Model Configuration" — identica in tutti i sei modelli
const MODEL_CONFIG_SECTION = {
  title: _("Model Configuration"),
  items: [
    {
      term: _("Heteroscedasticity test"), options: ["showHetTest", "hetTestBoot"],
      desc: _("Native metafor hettest: tests equal true-effect heterogeneity variances across studies, not whether tau-squared is zero (the Q test). Uses the default Score test with configurable parametric bootstrap samples (default 1,000; whole numbers from 100 to 100,000) and fixed seed 20261115. Changing the sample count reruns the bootstrap, reusing the main fit; display changes retain the cached test. Requires ML or REML, no moderators and at least two included studies. Bootstrap reports successful/total iterations; the p-value has Monte Carlo uncertainty. This is one overall test, not a test for each subgroup. A cached numerical result is reused for display changes and after restoring the analysis. The checkbox is enabled only for ML/REML; its disabled tooltip explains this requirement. Activation with moderators is blocked with an informational toast. The native procedure performs its own bootstrap fits; the main meta-analysis fit is reused."),
    },
    {
      term: _("Predictive probability"),
      options: ["predProbTableShow", "predProbPlotShow", "predProbThreshold", "predProbDirection"],
      desc: _("Available for random-effects models without moderators and at least two included studies. Enter a natural-scale threshold (positive RR/OR/ratio of means; correlation between -1 and 1; proportion or alpha between 0 and 1, endpoints excluded). Mean differences and generic effects use their input scale; AS uses radians. PFT is unavailable because its inverse needs a reference sample size. Below/Above refers to the true effect of a new study from the same population, not the pooled mean. Native metafor predict(prob=...) includes mean-estimation uncertainty and the selected z/t/Knapp-Hartung predictive distribution. The plot has natural-scale tick labels where applicable, but its axis and density remain on the analysis scale. It displays the central 99%; a threshold beyond that range is marked at the boundary, while the probability always uses the full distribution. Subgroup selection affects included cases; this is one overall probability, not predictions for each subgroup. Independent of Forest Back-Transformed. Unsupported choices show an explanation without stopping other results. Table and plot are independent. Changing the threshold does not refit the main meta-analysis."),
    },
    {
      term: _("Estimator"),
      options: ["modelest"],
      desc: _("Selects the between-study variance estimator, or a common-effect model (FE). FE disables Select Tests, Heterogeneity Comparison and Model Fit in this interface."),
      sub: [{ term: _("SJ (Alternative Init.)"), desc: _("Sidik–Jonkman initialized with the Hedges estimate of between-study variance (τ²).") }],
    },
    {
      term: _("Heterogeneity Comparison"),
      options: ["showHEC"],
      desc: _("Compares τ² estimates across the available estimation methods (DerSimonian-Laird, Hedges, Hunter-Schmidt, and more)."),
    },
    {
      term: _("Model Fit"),
      options: ["showModelFit"],
      desc: _("Displays ML and REML fit statistics, including log-likelihood, deviance, AIC, AICc and BIC."),
    },
    {
      term: _("Text Reporting"),
      options: ["showTextRepo"],
      desc: _("Generates a Methods/Results narrative for weighted models without moderators, using z or Knapp–Hartung inference. Custom study weights are not supported. With moderators present, activation is blocked with an informational toast. Adding moderators to an active report hides it and explains the restriction with a toast; removing moderators restores the selected report."),
    },
  ],
};

const getSharedModelTab = (typeLabel, measureControls, infoContent, layoutOptions = {}) => ({
  id: "model-tab",
  label: _("Model"),
  showTabTitle: false,
  tabTitle: "",
  tabSubtitle: "",
  tabTitleAlign: "center",
  layout: "grid",
  grid: {
    rows: 6,
    cols: "2:1:1",
    gap: "2px",
    itemGap: "2px",
  },
  groups: [
    {
      title: _("Display Options"),
      position: { row: 3, col: 0 },
      rowSpan: 1,
      colSpan: 1,
      collapsible: true,
      defaultExpanded: false,
    },      
    {
      title: _("Predictive probability"),
      position: { row: 3, col: 1 },
      rowSpan: 3,
      contentRows: true,
      colSpan: 2, 
      inlineRows: [3], 
      collapsible: true,
      defaultExpanded: false,
    },
  ],
  controls: [
    {
      id: "modelest",
      type: "select",
      option: "modelest",
      label: _("Estimator"),
      position: { row: 0, col: 0 },
      labelPosition: "left",
      itemGap: "11px",
      options: [
        { value: "DL",   text: _("DerSimonian-Laird") },
        { value: "HE",   text: _("Hedges") },
        { value: "HS",   text: _("Hunter-Schmidt") },
        { value: "SJ",   text: _("Sidik-Jonkman") },
        { value: "SJ2",  text: _("SJ (Alternative Init.)") },
        { value: "ML",   text: _("Maximum-Likelihood") },
        { value: "REML", text: _("Restricted Maximum-Likelihood") },
        { value: "EB",   text: _("Empirical Bayes") },
        { value: "PM",   text: _("Paule-Mandel") },
        { value: "FE",   text: _("Fixed-Effect") },
      ],
      default: "REML",
    },
    {
      id: "testType",
      type: "select",
      option: "testType",
      label: _("Select Tests"),
      position: { row: 1, col: 0 },
      labelPosition: "left",
      itemGap: "1px",
      options: [
        { value: "z",     text: _("z test") },
        { value: "t",     text: _("t test") },
        { value: "knha",  text: _("Knapp and Hartung Adjustment") },
        { value: "adhoc", text: _("Knapp–Hartung (Ad Hoc Modification)") },
      ],
      default: "z",
      dependsOn: { control: "modelest", disableWhen: ["FE"], resetOnEnable: true },
    },
    {
      id: "level",
      type: "number",
      option: "level",
      label: _("Confidence Interval (%)"),
      position: layoutOptions.levelPosition || { row: 2, col: 0 },
      labelPosition: "left",
      width: "40px",
      inputMode: "spin",
      min: 50,
      max: 99,
      value: 95,
      step: 1,
    },
    ...measureControls,
    {
      id: "showHetTest", 
      option: "showHetTest",
      type: "checkbox",
      label: _("Heteroscedasticity test"), 
      checked: false, 
      checkboxSide: "left",
      position: { row: 3, col: 0 },
      dependsOn: { control: "modelest", enableWhen: ["ML", "REML"], resetOnEnable: true,
        disabledTooltip: _("Requires ML or REML estimation.") },
    },
    {
      id: "hetTestBoot", option: "hetTestBoot", type: "number",
      label: _("Bootstrap samples"),
      labelPosition: "left",
      inputMode: "spin",
      min: 100, max: 100000, step: 100, value: 1000,
      itemGap: "2px",
      width: "65px",
      margin: "8px 0 0 35px", 
      position: { row: 3, col: 0 },
      dependsOn: { controls: [
        { control: "showHetTest", isTrue: true },
        { control: "modelest", valueIn: ["ML", "REML"] },
        { control: "moderator", notEmpty: false }
      ], operator: "AND", resetOnEnable: false },
    },    
    {
      id: "showHEC",
      type: "checkbox",
      option: "showHEC",
      label: _("Heterogeneity comparison"),
      checked: false,
      checkboxSide: "left",
      margin: "10px 0 0 0",
      position: { row: 3, col: 0 },
      dependsOn: { control: "modelest", disableWhen: ["FE"], resetOnEnable: true },
    },
    {
      id: "showModelFit",
      type: "checkbox",
      option: "showModelFit",
      label: _("Display model fit"),
      checked: false,
      checkboxSide: "left",
      margin: "8px 0 0 0",
      position: { row: 3, col: 0 },
      dependsOn: { control: "modelest", disableWhen: ["FE"], resetOnEnable: true },
    },
    {
      id: "showTextRepo",
      type: "checkbox",
      option: "showTextRepo",
      label: _("Text reporting"),
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      margin: "8px 0 0 0",
      position: { row: 3, col: 0 },
    },
    // Gruppi affiancati: Show condiviso, checkbox indipendenti e direzione a bilancia.
    {
      id: "predProbShowLabel",
      type: "label",
      label: _("Show:"),
      align: "left", 
      position: { row: 3, col: 1, colSpan: 2 },
    },
    {
      id: "predProbPlotShow",
      option: "predProbPlotShow",
      type: "checkbox",
      label: _("Plot"),
      checked: false, 
      checkboxSide: "left",
      position: { row: 3, col: 1, colSpan: 2 },
    },
    {
      id: "predProbTableShow",
      option: "predProbTableShow",
      type: "checkbox",
      label: _("Table"),
      checked: false, 
      checkboxSide: "left", 
      position: { row: 3, col: 1, colSpan: 2 },
    },
    {
      id: "predProbThreshold",
      option: "predProbThreshold",
      type: "number",
      label: _("Effect threshold"), 
      labelPosition: "left",
      inputMode: "spin",
      width: "55px", 
      step: .01, min: -1e12, max: 1e12,
      value: typeLabel === "Dichotomous" ? 1 : typeLabel === "Alpha" ? .8 : typeLabel === "Proportion" ? .5 : 0,
      margin: "8px 0 0 0",
      position: { row: 4, col: 1, colSpan: 2 },
    },
    {
      id: "predProbDirection",
      option: "predProbDirection",
      type: "deviatore",
      label: _("Direction threshold"), labelPosition: "center", mainLabelPosition: "top",
      stateLabelPosition: "center", orientation: "horizontal", shape: "beam",
      states: { left: _("Above"), right: _("Below") },
      track: { color: "#3e6da9" }, marker: { shape: "circle", active: "#3e6da9" },
      labelStyle: { size: "12px", color: "#555555" },
      allowOff: false, stateValues: { posLorT: "above", posRorB: "below" },
      margin: "35px 0 0 0",
      default: "posRorB", position: { row: 5, col: 1, colSpan: 2 },
    },
  ],
  infoContent: infoContent,
});

const getSharedForestTab = (typeLabel, hasRob = false) => ({
  id: "forest-tab",
  label: _("Forest Plot"),
  showTabTitle: false,
  tabTitle: "",
  tabSubtitle: "",
  tabTitleAlign: "left",
  layout: "grid",
  grid: {
    rows: 3,
    cols: "1.35:1.4:1.25",
    gap: "2px",
    itemGap: "2px",
  },
  groups: [
    {
      title: _("Plot Settings"),
      position: { row: 0, col: 2 },
      rowSpan: 3,
      collapsible: true,
      defaultExpanded: false,
    },
  ],
  controls: [
    // Stile punto
    {
      id: "pchForest",
      type: "select",
      option: "pchForest",
      label: _("Point Style"),
      options: [
        { value: "bSquare",   text: _("Black Square") },
        { value: "eSquare",   text: _("Empty Square") },
        { value: "bCircle",   text: _("Black Circle") },
        { value: "eCircle",   text: _("Empty Circle") },
        { value: "bTriangle", text: _("Black Triangle") },
        { value: "eTriangle", text: _("Empty Triangle") },
        { value: "bDiamond",  text: _("Black Diamond") },
        { value: "eDiamond",  text: _("Empty Diamond") },
      ],
      default: "bSquare",
      margin: "10px 0 0 0",
      position: { row: 0, col: 0 },
    },
    // Titolo principale
    { 
      id: "mainTitle",
      type: "string",
      option: "mainTitle",
      label: _("Main Title"),
      value: "",
      labelPosition: "top",
      margin: "10px 0 0 0",
      position: { row: 0, col: 1 },
      align: "left"
    },
    // Ordine studi
    {
      id: "forestOrder",
      type: "select",
      option: "forestOrder",
      label: _("Study Order"),
      options: [
        { value: "obs", text: _("Effect Size (Observed)") },
        { value: "fit", text: _("Predicted Value (Fitted)") },
        { value: "prec", text: _("Precision (Inverse Variance)") },
        { value: "resid", text: _(" Residual (Unstandardized)") },
        { value: "rstandard", text: _("Residual (Standardized)") },
        { value: "abs.resid", text: _("Absolute Residual") },
        { value: "abs.rstandard", text: _("Absolute Standardized Residual") },
      ],
      default: "fit",
      margin: "5px 0 0 0",
      position: { row: 1, col: 0 },
    },
    // Titolo asse X
    { 
      id: "xAxisTitle",
      type: "string",
      option: "xAxisTitle",
      label: _("X-Axis Title"),
      value: "",
      labelPosition: "top",
      margin: "5px 0 0 0",
      position: { row: 1, col: 1 },
    },
    // Ordine sottogruppi
    { 
      id: "ordSubgr",
      type: "checkbox",
      option: "ordSubgr",
      label: _("Subgroup Order"),
      //color: "#fffe30",
      checked: false,
      checkboxSide: "left",
      displayType: "toggle",
      margin: "10px 0 0 0",
      position: { row: 2, col: 0 },
      dependsOn: {
      groups: [
        {
          controls: [
            {
              control: "subgroup",
              notEmpty: true,
            }
          ],
          operator: "AND"
        }
      ],
      operator: "AND",
      resetOnEnable: true
      },
      align: "left"
    },
    // Passi asse X
    {
      id: "steps",
      type: "number",
      option: "steps",
      inputMode: "spin",
      label: _("X-Axis Steps"),
      labelPosition: "left",
      min: 3,
      max: 17,
      value: 5,
      width: "45px",
      margin: "5px 0 0 0",
      position: { row: 2, col: 1 },
    },
    // Effetto globale
    { 
      id: "addfit",
      type: "checkbox",
      option: "addfit",
      label: _("Overall"),
      checked: true,
      margin: "10px 0 0 0",
      position: { row: 2, col: 0 },
      checkboxSide: "left",
    },
    // Intervallo di predizione
    {
      id: "addpred",
      type: "checkbox",
      option: "addpred",
      label: _("Prediction Interval"),
      checked: false,
      margin: "10px 0 0 0",
      position: { row: 2, col: 1 },
      checkboxSide: "left",
      dependsOn: {
        groups: [
            {
                controls: [
                    { control: "modelest", disableWhen: ["FE"] },
                    { control: "addfit", enableWhen: [true] }
                ],
                operator: "OR"
            },
            {
                controls: [
                    { control: "moderator", notEmpty: true, not: true }
                ]
            }
        ],
        operator: "AND",
        resetOnEnable: true
      }
    },
    // Pesi degli studi
    {
      id: "showweights",
      type: "checkbox",
      option: "showweights",
      label: _("Study Weights"),
      checked: false,
      margin: "5px 0 0 0",
      position: { row: 2, col: 0 },
      checkboxSide: "left",
      dependsOn: { control: "modelest", disableWhen: ["FE"], resetOnEnable: true }
    },
    // Ombreggiatura righe
    { 
      id: "shadeRows",
      type: "checkbox",
      option: "shadeRows",
      label: _("Shade Rows"),
      checked: false,
      margin: "5px 0 0 0",
      position: { row: 2, col: 1 },
      checkboxSide: "left",
    },
    // forestPlotMode
    {
      id: "forestPlotMode",
      type: "deviatore",
      option: "forestPlotMode",
      label: _("Show Plot"),
    
      // Posizione orizzontale della label principale:
      // "left"   → allineata a sinistra
      // "center" → centrata
      // "right"  → allineata a destra
      labelPosition: "center",
    
      // Posizione verticale della label principale:
      // "top"    → sopra il deviatore
      // "bottom" → sotto il deviatore
      mainLabelPosition: "top",
    
      // Posizione delle label degli stati:
      // "top"    → sopra il deviatore
      // "center" → esterne a sinistra/destra, centrate verticalmente
      // "bottom" → sotto il deviatore
      stateLabelPosition: "bottom",
    
      orientation: "horizontal",
      
      //shape: "line",         // "classic" (default) | "line" | "beam"
      //marker: { shape: "circle", fill: "#ffffff", active: "#2ecc71" },   // shape: "circle" "square" "diamond"
      //track:  { color: "#666666" },

      states: { left: _("Classic"), right: _("Interactive"), colorA: "#3e6da9", colorB: "#eca038" },
      thumb:  { color: "#ffffff", width: "7px", height: "11px" },   // shape: "rect" | "circle"
      default: "posOff",
      //margin: "5px 0 0 0",
      position: { row: 0, col: 2, rowSpan: 3 },
    },
        // Stili nativi del Classic: non modifica il modello o il Forest Interactive.
    {
      id: "forestPredictionStyle",
      type: "select",
      option: "forestPredictionStyle",
      label: _("PI style"),
      labelPosition: "left",
      width: "111px",
      itemGap: "2px",
      compactInline: true,
      align: "top-center",
      options: [
        { value: "line2", text: _("Line with limits") },
        { value: "diamond", text: _("Diamond") },
        { value: "bar", text: _("Bar") },
        { value: "shade", text: _("Shaded bar") },
        { value: "dist", text: _("Distribution") },
      ],
      default: "line2",
      valueAliases: { line: "line2" },
      position: { row: 1, col: 2 },
      margin: "5px 0 0 0",
      dependsOn: {
        controls: [
          { control: "forestPlotMode", valueIn: ["posLorT"] },
          { control: "addpred", enableWhen: [true] },
          { control: "addfit", enableWhen: [true] },
          { control: "modelest", disableWhen: ["FE"] },
          { control: "moderator", notEmpty: true, not: true },
        ],
        operator: "AND",
        resetOnEnable: false,
      },
    },
    // ROB nei modelli che espongono il supplier; sempre selezionabile.
    // Senza giudizi o con forest Off il backend non disegna annotazioni.
    ...(hasRob ? [{
      id: "forestRobShow",
      option: "forestRobShow",
      label: _("Show RoB"),
      color: "#2ECC71",            // verde smeraldo
      type: "checkbox",
      checked: false,
      checkboxSide: "left",
      displayType: "toggle",
      //margin: "5px 0 0 0",
      position: { row: 2, col: 2 },
    }] : []),
    // Tabella forest (Html)
    { 
      id: "forestTableShow",
      option: "forestTableShow",
      label: _("Show Table"),
      color: "#eca038",
      type: "checkbox",
      checked: false,
      checkboxSide: "left",
      displayType: "toggle",
      margin: "30px 0 0 0",
      position: { row: 2, col: 2 },
    },
  ],
  infoContent: buildInfoNotes([{
    title: _("Forest Plot Configuration"),
    items: [
      { term: _("Show Plot"), options: ["forestPlotMode"], desc: _("Choose Classic, Off or Interactive. Interactive provides zoom, tooltips and study selection.") },
      { term: _("Overall"), options: ["addfit"], desc: _("Shows the model summary as a diamond. No global summary is drawn with moderators: coefficients are not pooled effects.") },
      { term: _("Prediction Interval / Classic PI style"), options: ["addpred", "forestPredictionStyle"], desc: _("Predicts the true effect in a new study under a random-effects model, without moderators and with at least two studies. Classic PI style uses native metafor rendering: Line with limits is the default and prints the interval bounds; Diamond, Bar, Shaded bar and Distribution use a separate prediction row. The pooled diamond still represents the confidence interval of the mean effect. Prediction rows use the selected model test and interval level. Styles change presentation, not estimates or the Interactive forest. Subgroup predictions require at least two studies in that subgroup.") },
      { term: _("Study Weights"), options: ["showweights"], desc: _("Displays each study's percentage weight in the fitted model.") },
      { term: _("Shade Rows"), options: ["shadeRows"], desc: _("Adds alternating backgrounds to study rows.") },
      { term: _("Point Style"), options: ["pchForest"], desc: _("Selects the marker shape for individual studies.") },
      { term: _("Main Title / X-Axis Title"), options: ["mainTitle", "xAxisTitle"], desc: _("Sets custom text for the plot heading and effect-size axis.") },
      { term: _("Study Order"), options: ["forestOrder"], desc: _("Orders studies by observed or fitted effect, precision, or signed/absolute residuals. With subgroups, ordering is within each group.") },
      { term: _("Subgroup Order"), options: ["ordSubgr"], desc: _("Reverses the order of subgroup blocks while retaining the selected study order.") },
      { term: _("X-Axis Steps"), options: ["steps"], desc: _("Sets the approximate number of axis ticks.") },
      { term: _("Show Table"), options: ["forestTableShow"], desc: _("Displays an interactive Forest Table with study inputs, estimates, confidence intervals and model summaries. With subgroups, Summary describes each group and Overall Summary describes all studies; input summaries are descriptive, while estimates come from the fitted models.") },
      { term: _("Forest Table Tools"), desc: _("Use Columns to show or hide columns and click a heading to sort within subgroups. The Estimate column has a light blue background. Column tooltips explain the source variable and summary rule. Subgroup weight bars show their share of the overall weights. The Z slider enlarges or reduces the whole table; double-click it for 100%. Scroll the Results page to see a large table. Reset view restores its presentation without changing the model.") },
      { term: _("Linked Studies"), desc: _("Hover over a study in the table or an Interactive graph to highlight it in linked Forest, Funnel, Sunset and Leave-One-Out views of the same analysis. Click to pin it; click the same study again or use the cross beside Pinned study to release it. Enter or Space activates study buttons in the table. A pin survives redraws in the current Results session and does not scroll the page or refit the model. Distinct study labels make repeated names easier to recognize.") },
      { term: _("Interactive Tools"), desc: _("Inspect and pin the overall summary or prediction interval with click or Enter; press Escape to release. The local subgroup filter changes which studies are displayed, while the overall summary continues to represent all studies. These actions do not refit the model.") },
      ...(hasRob ? [{ term: _("Show RoB"), options: ["forestRobShow"], desc: _("Adds supplied Risk of Bias ratings in Classic or Interactive mode. Classic uses the same colors and symbols, with a static legend; an Overall column is headed OA (Overall risk of bias); long headings are keyed to their full variable names below the graph. Select up to six RoB 2 columns: five domains and, optionally, an overall rating; headings use the selected variable names. Low, Some concerns/Unclear and High are supported; missing or unrecognized ratings appear in gray. Ratings are displayed without changing the fitted model or calculating an overall judgment.") }] : []),
    ],
  }]),
});

const getSharedFunnelTab = (typeLabel) => ({
  id: "funnel-tab",
  label: _("Funnel Plot"),
  showTabTitle: false,
  tabTitle: "",
  tabSubtitle: "",
  tabTitleAlign: "left",
  layout: "grid",
  grid: {
    rows: 3,
    cols: 3,
    gap: "2px",
    itemGap: "2px",
  },
  groups: [
    {
      title: _("Plot Settings"),
      position: { row: 0, col: 0 },
      rowSpan: 2,
      colSpan: 3,
      collapsible: true,
      defaultExpanded: true,
    },
  ],
  controls: [
    {
      id: "yaxis",
      type: "select",
      option: "yaxis",
      label: _("Y-Axis Type"),
      labelPosition: "left",
      options: [
        { value: "sei",    text: _("Standard Error") },
        { value: "vi",     text: _("Sampling Variance") },
        { value: "ni",     text: _("Sample Size") },
        { value: "sqrtni", text: _("√ Sample Size") },
        { value: "lni",    text: _("Log Sample Size") },
      ],
      default: "sei",
      itemGap: "1px",
      width: "140px",
      position: { row: 0, col: 0 },
    },
    {
      id: "sunsetFunnel",
      option: "sunsetFunnel",
      type: "checkbox",
      label: _("Power"),
      color: "#2ECC71",        // verde smeraldo
      checked: false,
      checkboxSide: "right",
      displayType: "toggle",
      margin: "10px 0 0 20px",
      position: { row: 0, col: 1},
      align: "center",
      // Condizioni di disponibilità, tutte necessarie; il backend verifica anche il fit.
      dependsOn: {
        groups: [{
          controls: [
            { control: "funnelPlotMode", valueIn: ["posRorB"] },
            { control: "yaxis", valueIn: ["sei"] },
            { control: "moderator", notEmpty: false },
          ],
          operator: "AND",
        }],
        operator: "AND",
        resetOnEnable: false,
      },
    },
    {
      id: "yaxisInv",
      type: "checkbox",
      option: "yaxisInv",
      label: _("Inverse Y-Axis"),
      checked: false,
      checkboxSide: "left",
      margin: "5px 0 0 0",
      position: { row: 1, col: 0 },
      align: "left"
    },
    {
      id: "enhanceFunnel",
      type: "checkbox",
      option: "enhanceFunnel",
      label: _("Contour"),
      checked: false,
      checkboxSide: "right",
      margin: "5px 0 0 0",
      position: { row: 1, col: 1 },
      dependsOn: {
        controls: [
          { control: "funnelPlotMode", disableWhen: ["posOff"] },
          { control: "yaxis", disableWhen: ["ni", "sqrtni", "lni"] }
        ],
        operator: "OR",
        resetOnEnable: true
      }
    },
    // Selezione funnel plot (Classic o Interactive)
    {
      id: "funnelPlotMode",
      type: "deviatore",
      option: "funnelPlotMode",
      label: _("Show Plot"),
      labelPosition: "center",    // "left" | "center" | "right"
      mainLabelPosition: "top",
      stateLabelPosition: "bottom",
      orientation: "horizontal", // "horizontal" | "vertical"
      states: { left: _("Classic"), right: _("Interactive"), colorA: "#3e6da9", colorB: "#eca038" },
      thumb:  { color: "#ffffff", width: "7px", height: "11px" },   // shape: "rect" | "circle"
      default: "posOff",
      margin: "0 0 0 -50px",
      position: { row: 0, col: 2, rowSpan: 2 },
    },
  ],
  infoContent: buildInfoNotes([{
    title: _("Funnel Plot Configuration"),
    items: [
      { term: _("Funnel Plot"), options: ["funnelPlotMode"], desc: _("Choose Classic, Off or Interactive. Both plots follow metafor: the funnel is centered on the pooled estimate without moderators; with moderators it displays model residuals and their standard errors around zero. The standard funnel does not add Trim-and-Fill imputed studies.") },
      { term: _("Y-Axis Type"), options: ["yaxis"], desc: _("Choose standard error, sampling variance, sample size, its square root or its logarithm. Sample-size axes require sample sizes in the fitted model.") },
      { term: _("Inverse Y-Axis"), options: ["yaxisInv"], desc: _("Uses the reciprocal of the selected axis measure; it has no effect on the log sample-size axis.") },
      { term: _("Contour"), options: ["enhanceFunnel"], desc: _("Adds native metafor 90%, 95% and 99% pseudo confidence regions around the funnel reference with a standard-error or variance axis.") },
      { term: _("Power"), options: ["sunsetFunnel"], desc: _("Displays estimated power conditional on the global pooled effect, including when subgroups are shown. Requires Interactive mode, a standard-error axis and a model without moderators.") },
      { term: _("Display Scale"), desc: typeLabel === _("Effect Size") ? _("Effect labels use the supplied analysis scale.") : _("Supported back-transformations selected in the Model tab change effect labels, ticks and tooltips. Geometry, standard errors and power remain on the analysis scale.") },
      { term: _("Interpretation"), desc: _("Asymmetry can reflect small-study effects, heterogeneity or selection. Symmetry does not rule out publication bias.") },
      { term: _("Interactive Tools"), desc: _("Study hover and pin are linked to the Forest Table and other Interactive views of the same analysis. Hover over reference lines or legend entries for explanations; click or press Enter to pin them, and Escape to release. Drag the legend by its header. Selecting a subgroup in the legend highlights its studies without excluding observations or refitting the model.") },
      { term: _("Sunset Palette"), desc: _("Use the palette button beside the zoom slider to choose the power colors. The background and color bar use the same scale; changing the palette does not change power values or subgroup colors.") },
    ],
  }]),
});

// Configurazione dell'Abbé Plot (Solo per Dichotomous in TAB DIAGNOSTICS )
const labbeDiagnosticConfig = {
  groups: [
    {
      title: _("L'Abbé Plot"),
      position: { row: 4, col: 0 },
      colSpan: 2,
      collapsible: true,
      defaultExpanded: false,
    },
  ],
  controls: [
    {
      id: "labbePlotShow",
      type: "checkbox",
      option: "labbePlotShow",
      label: _("Show Plot"),
      // PETO e moderatori impediscono il grafico; non ripristinare selezioni obsolete.
      dependsOn: {
        controls: [
          { control: "modelmea", disableWhen: ["PETO"] },
          { control: "moderator", notEmpty: false },
        ],
        operator: "AND",
        resetOnEnable: false,
      },
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 4, col: 0 },
    },
    {
      id: "labbeTransf",
      type: "select",
      option: "labbeTransf",
      label: _("Y/X axis"),
      labelPosition: "top",
      width: "110px",
      margin: "25px 0 0 0",
      options: [
        { value: "log",    text: _("Log scale") },
        { value: "exp",    text: _("Odds scale") },
        { value: "plogis", text: _("Risk scale (0-1)") },
      ],
      default: "log",
      position: { row: 4, col: 0 },
    },
    {
      id: "labbePlotCI",
      type: "checkbox",
      option: "labbePlotCI",
      label: _("Confidence Interval"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      position: { row: 4, col: 1 },
    },
    {
      id: "labbePlotPI",
      type: "checkbox",
      option: "labbePlotPI",
      label: _("Prediction Interval"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "17px 0 0 0",
      position: { row: 4, col: 1 },
    },
    {
      id: "labbePlotLegend",
      type: "checkbox",
      option: "labbePlotLegend",
      label: _("Legend"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "34px 0 0 0",
      position: { row: 4, col: 1 },
    },
    {
      id: "labbePlotGrid",
      type: "checkbox",
      option: "labbePlotGrid",
      label: _("Grid"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "51px 0 0 0", // top, right, bottom, left
      position: { row: 4, col: 1 },
    },
    {
      id: "labbePetoBadge",
      type: "label",
      label: _("Not available for Peto [OR]"),
      variant: "badge",
      level: "span",
      style: {
        size: "10px",
        bold: 1,
        color: "#ffffff",
        background: "#e07b39",
        radius: "11px",
        padding: "1px 8px",
      },
      isGroupBadge: true,
      align: "center",
      position: { row: 4, col: 0 },
      dependsOn: { control: "modelmea", showWhen: ["PETO"] },
    },
  ]
};

// Attivo se il modello ammette LOO e almeno una vista e selezionata.
const leaveOneOutDependencies = (subgroup = false) => ({
  groups: [
    { operator: "AND", controls: [ { control: "moderator", notEmpty: false },
      ...(subgroup ? [{ control: "subgroup", notEmpty: true }] : []) ] },
    { operator: "OR", controls: [ { control: "leave1outPlotMode", valueIn: ["posLorT", "posRorB"] },
                                 { control: "leave1outTableShow", isTrue: true } ] },
  ], operator: "AND", resetOnEnable: true,
});

const getSharedDiagnosticTab = (typeLabel, extraGroups = [], extraControls = []) => ({
  id: "diagnostic-tab",
  label: _("Diagnostics"),
  showTabTitle: false,
  tabTitle: "",
  tabSubtitle: "",
  tabTitleAlign: "left",
  layout: "grid",
  grid: {
    rows: 5,
    cols: 3,
    gap: "2px",
    itemGap: "2px",
  },
  groups: [
    // Diagnostici senza opzioni aggiuntive, raccolti in un solo gruppo.
    {
      title: _("Diagnostic Plots"),
      position: { row: 0, col: 0 },
      collapsible: true,
      defaultExpanded: false,
    },
    // Plot Scatter/Bubble
    {
      title: _("Scatter/Bubble Plot"),
      position: { row: 0, col: 1 },
      colSpan: 2,
      collapsible: true,
      defaultExpanded: false,
    },
    // Radial Plot
    {
      title: _("Radial Plot"),
      position: { row: 1, col: 0 },
      collapsible: true,
      defaultExpanded: false,
    },
    // Gosh Plot
    {
      title: _("Gosh Plot"), 
      position: { row: 1, col: 1 },
      collapsible: true,
      defaultExpanded: false,
    },
    // Baujat Plot
    {
      title: _("Baujat Plot"),
      position: { row: 1, col: 2 },
      collapsible: true,
      defaultExpanded: false,
    },
    // Leave-One-Out
    {
      title: _("Leave-One-Out"),
      position: { row: 2, col: 0 },
      collapsible: true,
      defaultExpanded: false,
    },
    // Test di equivalenza (TOSTER)
    {
      title: _("Equivalence Test (TOSTER)"),
      position: { row: 3, col: 0 },
      colSpan: 2,
      collapsible: true,
      defaultExpanded: false,
    },
    // Analisi di sensibilita affiancate; TOSTER resta nella riga successiva.
    {
      title: _("Cumulative Meta-Analysis"),
      position: { row: 2, col: 1 },
      colSpan: 2, stackedColumns: [1],
      collapsible: true,
      defaultExpanded: false,
    },
    ...extraGroups,
  ],
  controls: [
    // Tre checkbox verticali nel gruppo Diagnostic Plots.
    // Influence Diagnostics
    {
      id: "infDiagPlotShow",
      type: "checkbox",
      option: "infDiagPlotShow",
      label: _("Influence Diagnostics"),
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 0, col: 0 },
    },
    // Q-Q Normal Plot
    {
      id: "infQQPlotShow",
      type: "checkbox",
      option: "infQQPlotShow",
      label: _("Q-Q Normal Plot"),
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      margin: "8px 0 0 0",
      position: { row: 0, col: 0 },
    },
    // Likelihood Plot
    {
      id: "lhPlotShow",
      type: "checkbox",
      option: "lhPlotShow",
      label: _("Likelihood Plot"),
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      margin: "8px 0 0 0",
      position: { row: 0, col: 0 },
    },

    // Grafico Scatter/Bubble (regplot)
    {
      id: "scatterPlotShow",
      type: "checkbox",
      option: "scatterPlotShow",
      label: _("Show Plot"),
      dependsOn: { control: "moderator", notEmpty: true, variableType: "numeric", resetOnEnable: false,
        disabledTooltip: _("Requires at least one continuous moderator."),
        mixedVariableToastTitle: _("Scatter/Bubble Plot"),
        mixedVariableToast: _("The scatter/bubble plot displays a separate panel for each numerical covariate. Categorical moderators remain included in the fitted model but are not plotted on the horizontal axis.") },
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 0, col: 1 },
    },
    {
      id: "scatterPlotGrid",
      type: "checkbox",
      option: "scatterPlotGrid",
      label: _("Grid"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "24px 0 0 0",
      position: { row: 0, col: 1 },
    },
    {
      id: "scatterPlotLegend",
      type: "checkbox",
      option: "scatterPlotLegend",
      label: _("Legend"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "48px 0 0 0",
      position: { row: 0, col: 1 },
    },
    {
      id: "scatterPlotCI",
      type: "checkbox",
      option: "scatterPlotCI",
      label: _("Confidence Interval"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      position: { row: 0, col: 2 },
    },
    {
      id: "scatterPlotPI",
      type: "checkbox",
      option: "scatterPlotPI",
      label: _("Prediction Interval"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "16px 0 0 0",
      position: { row: 0, col: 2 },
    },
    {
      id: "scatterPlotScale",
      type: "checkbox",
      option: "scatterPlotScale",
      label: _("Weight by Precision"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "32px 0 0 0",
      position: { row: 0, col: 2 },
    },
    {
      id: "scatterPlotLabel",
      type: "checkbox",
      option: "scatterPlotLabel",
      label: _("Study Label"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "48px 0 0 0",
      position: { row: 0, col: 2 },
    },

    // Grafico Radiale (Galbraith)
    {
      id: "radialPlotShow",
      type: "checkbox",
      option: "radialPlotShow",
      label: _("Show Plot"),
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 1, col: 0  },
    },
    {
      id: "radialPlotCenter",
      type: "checkbox",
      option: "radialPlotCenter",
      label: _("Center on Estimate"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "17px 0 0 0",
      position: { row: 1, col: 0 },
    },
    {
      id: "radialPlotSteps",
      type: "number",
      option: "radialPlotSteps",
      inputMode: "spin",
      label: _("Y-axis Steps"),
      labelPosition: "left",
      min: 3,
      max: 15,
      value: 7,
      step: 1,
      width: "40px",
      margin: "12px 0 0 5px",
      position: { row: 1, col: 0 },
    },
    // radial plot transf per tutti i modelli
    {
      id: "radialPlotTransf",
      type: "select",
      option: "radialPlotTransf",
      dependsOn: typeLabel === "Dichotomous" ? { control: "modelmea", hideWhen: ["RD", "AS"] }
        : typeLabel === "Proportion" ? { control: "modelmea", hideWhen: ["PFT"] } : undefined,
      label: _("Arc Scale"),
      labelPosition: "top",
      options: (() => {
        switch (typeLabel) {
          case "Correlation": // utilizzare valore esatto di typeLabel
            return [
              { value: "none", text: _("Analysis scale") },
              { value: "ztor", text: _("Correlation (r) scale") }
            ];
          case "Alpha":
            return [
              { value: "none", text: _("Original scale") },
              { value: "iahw", text: _("Inverse Hakstian-Whalen") },
              { value: "iabt", text: _("Inverse Bonett") }
            ];
          case "Dichotomous":
            return [
              { value: "none", text: _("Analysis scale") },
              { value: "exp",  text: _("Exponential (OR, RR)") }
            ];
          case "Mean Difference":
            return [
              { value: "none", text: _("Original scale (MD / SMD)") },
              { value: "exp",  text: _("Exponential (for ROM)") }
            ];
          case "Proportion":
            return [
              { value: "none",    text: _("Analysis scale") },
              { value: "exp",     text: _("Exponential") },
              { value: "ilogit",  text: _("Inverse Logit") },
              { value: "iarcsin", text: _("Inverse Arcsine") },
              { value: "iprobit", text: _("Inverse Probit") }
            ];
          case "Effect Size":
          default:
            return [
              { value: "none",   text: _("Original scale") },
              { value: "exp",    text: _("Exponential") },
              { value: "ilogit", text: _("Inverse Logit") },
              { value: "ztor",   text: _("Fisher's z to r") }
            ];
        }
      })(),
      default: "none",
      width: "135px",
      margin: "4px 0 0 5px",
      position: { row: 1, col: 0 },
    },

    // GOSH Plot
    {
      id: "goshPlotShow",
      type: "checkbox",
      option: "goshPlotShow",
      label: _("Show Plot"),
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 1, col: 1 },
    },
    {
      id: "goshPlotOut",
      type: "number",
      option: "goshPlotOut",
      inputMode: "spin",
      label: _("Highlight Study"),
      labelPosition: "left",
      min: 0,
      max: 999,
      value: 0,
      step: 1,
      itemGap: "1px",
      width: "50px",
      margin: "13px 0 0 0",
      position: { row: 1, col: 1 },
    },
    {
      id: "goshPlotSubsets",
      type: "number",
      option: "goshPlotSubsets",
      inputMode: "spin",
      label: _("Max Subsets"),
      labelPosition: "left",
      min: 100,
      max: 10000,
      value: 1000,
      step: 100,
      itemGap: "18px",
      width: "50px",
      margin: "10px 0 0 0",
      position: { row: 1, col: 1 },
    },
    {
      id: "goshPlotHet",
      type: "select",
      option: "goshPlotHet",
      label: _("Heterogeneity"),
      labelPosition: "left",
      options: [
        { value: "I2",   text: "I²" },
        { value: "H2",   text: "H²" },
        { value: "QE",   text: "Q" },
        { value: "tau2", text: _("tau²") },
        { value: "tau",  text: _("tau") },
      ],
      default: "I2",
      width: "59px",
      margin: "25px 0 0 0",
      position: { row: 1, col: 1 },
    },

    // Baujat Plot
    {
      id: "baujatPlotShow",
      type: "checkbox",
      option: "baujatPlotShow",
      label: _("Show Plot"),
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 1, col: 2 },
    },
    {
      id: "baujatPlotGrid",
      type: "checkbox",
      option: "baujatPlotGrid",
      label: _("Grid"),
      checked: true,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      margin: "17px 0 0 0",
      position: { row: 1, col: 2 },
    },
    {
      id: "baujatPlotSymbol",
      type: "select",
      option: "baujatPlotSymbol",
      label: _("Symbol"),
      labelPosition: "left",
      width: "95px",
      options: [
        { value: "ids",  text: _("Study IDs") },
        { value: "slab", text: _("Study Labels") },
        { value: "pch",  text: _("Points") },
      ],
      default: "ids",
      margin: "12px 0 0 5px",
      position: { row: 1, col: 2 },
    },
    // Leave-One-Out: grafico e tabella D3 indipendenti, zebra condivisa.
    {
      id: "leave1outPlotMode", type: "deviatore", option: "leave1outPlotMode",
      label: _("Show Plot"), labelPosition: "center",
      mainLabelPosition: "top", stateLabelPosition: "bottom", orientation: "horizontal",
      states: { left: _("Classic"), right: _("Interactive"), colorA: "#3e6da9", colorB: "#eca038" },
      thumb: { color: "#ffffff", width: "7px", height: "11px" },
      default: "posOff", margin: "0 0 24px -24px",
      position: { row: 2, col: 0 },
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
    },
    {
      id: "leave1outTableShow", type: "checkbox", option: "leave1outTableShow",
      label: _("Show Table"), checked: false, displayType: "toggle", checkboxSide: "left", color: "#eca038",
      margin: "8px 0 0 0", position: { row: 2, col: 0 },
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
    },
    {
      id: "leave1outShade", type: "checkbox", option: "leave1outShade",
      label: _("Shade Rows"), checked: true, checkboxSide: "left",
      margin: "8px 0 0 0", position: { row: 2, col: 0 },
      dependsOn: leaveOneOutDependencies(),
    },
    {
      id: "leave1outPrediction", type: "checkbox", option: "leave1outPrediction",
      label: _("Prediction Interval"), checked: false, checkboxSide: "left",
      margin: "8px 0 0 0", position: { row: 2, col: 0 },
      dependsOn: { ...leaveOneOutDependencies(), groups: [
        ...leaveOneOutDependencies().groups,
        { operator: "AND", controls: [{ control: "modelest", valueNotIn: ["FE", "EE", "CE"] }] }
      ] },
    },
    {
      id: "leave1outOrder",
      type: "select",
      option: "leave1outOrder",
      label: _("Order By"),
      labelPosition: "top",
      width: "160px",
      options: [
        { value: "none",     text: _("Original Study Order") },
        { value: "shift",    text: _("Influence |Delta Estimate|") },
        { value: "estimate", text: _("Estimate (ascending)") },
        { value: "pval",     text: _("p-value (ascending)") },
        { value: "study",    text: _("Study Name (A→Z)") },
      ],
      default: "none",
      position: { row: 2, col: 0 },
      margin: "18px 0 0 0",
      dependsOn: leaveOneOutDependencies(),
    },

    {
      id: "leave1outGroupOrder", type: "checkbox", option: "leave1outGroupOrder",
      label: _("Subgroup Order"), checked: false,
      displayType: "toggle", checkboxSide: "left",
      margin: "12px 0 0 0", position: { row: 2, col: 0 },
      dependsOn: leaveOneOutDependencies(true),
    },

    // TOST Plot
    {
      id: "tostPlotShow",
      type: "checkbox",
      option: "tostPlotShow",
      label: _("Show Plot"),
      checked: false,
      checkboxSide: "left",
      color: "#eca038",
      position: { row: 3, col: 0 },
    },
    // Tabella TOST
    {
      id: "showTost",
      type: "checkbox",
      option: "showTost",
      label: _("Show Table"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      color: "#2ECC71",            // verde smeraldo
      position: { row: 3, col: 1 },
      align: "right",
    },
    // Equivalence Test (TOSTER)
    {
      id: "equiBoundsDivider",
      type: "label",
      label: _("Equivalence Bounds"),
      variant: "divider",
      align: "center",
      line:  { width: 1, color: "rgba(62, 109, 169, 0.8)" },
      style: { size: "12px", color: "#3e6da9" },
      margin: "25px 0 0 25px",
      position: { row: 3, col: 0, colSpan: 2 }
    },    
    {
      id: "tostLower",
      type: "number",
      option: "tostLower",
      inputMode: "standard",
      label: _("Lower value"),
      labelPosition: "left",
      itemGap: "1px",
      width: "40px",
      margin: "50px 0 0 25px",
      position: { row: 3, col: 0 },
    },
    {
      id: "tostUpper",
      type: "number",
      option: "tostUpper",
      inputMode: "standard",
      label: _("Upper value"),
      labelPosition: "left",
      itemGap: "1px",
      width: "40px",
      margin: "50px 25px 0 0",
      position: { row: 3, col: 1 },
    },
    {
      id: "tostAlpha",
      type: "number",
      option: "tostAlpha",
      inputMode: "standard",
      label: _("\u03B1 value"),
      labelPosition: "left",
      itemGap: "25px",
      width: "40px",
      margin: "75px 0 0 25px",
      position: { row: 3, col: 0 },
    },
    {
      id: "cumulativePlotMode", type: "deviatore", option: "cumulativePlotMode",
      label: _("Show Plot"), labelPosition: "center",
      mainLabelPosition: "top", stateLabelPosition: "bottom", orientation: "horizontal",
      states: { left: _("Classic"), right: _("Interactive"), colorA: "#3e6da9", colorB: "#eca038" },
      thumb: { color: "#ffffff", width: "7px", height: "11px" },
      // Nei gruppi con span il margine è un offset dall'inizio della cella.
      default: "posOff", margin: "0 0 24px -24px", position: { row: 2, col: 1 },
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
    },
    {
      id: "cumulativeTableShow", type: "checkbox", option: "cumulativeTableShow",
      color: "#eca038",
      label: _("Show Table"), checked: false,
      displayType: "toggle", toggleOrientation: "horizontal", checkboxSide: "left",
      margin: "8px 0 0 0", position: { row: 2, col: 1 },
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
    },
    {
      id: "cumulativeShade", type: "checkbox", option: "cumulativeShade",
      label: _("Shade Rows"), checked: false, position: { row: 2, col: 1 }, margin: "8px 0 0 0",
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: true },
    },
    {
      id: "cumulativePrediction", type: "checkbox", option: "cumulativePrediction",
      label: _("Prediction Interval"), checked: false, checkboxSide: "left",
      position: { row: 2, col: 1 }, margin: "8px 0 0 0",
      dependsOn: {
        groups: [
          { operator: "AND", controls: [{ control: "moderator", notEmpty: false },
            { control: "modelest", valueNotIn: ["FE", "EE", "CE"] }] },
          { operator: "OR", controls: [{ control: "cumulativePlotMode", valueIn: ["posLorT", "posRorB"] },
            { control: "cumulativeTableShow", isTrue: true }] }
        ], operator: "AND", resetOnEnable: true,
      },
    },
    {
      id: "cumulativeOrder", type: "select", option: "cumulativeOrder",
      label: _("Order By"), labelPosition: "top", width: "160px",
      options: [
        { value: "original", text: _("Original Study Order") },
        { value: "se", text: _("Standard Error") },
        { value: "study", text: _("Study Label") },
        { value: "variable", text: _("Selected Variable") },
      ],
      margin: "18px 0 0 0", default: "original", position: { row: 2, col: 1 },
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: true },
    },
    {
      id: "cumulativeDirection", type: "deviatore", option: "cumulativeDirection",
      label: _("Direction"), labelPosition: "center", mainLabelPosition: "top",
      stateLabelPosition: "center", orientation: "horizontal", shape: "beam",
      states: { left: _("A–Z"), right: _("Z–A") },
      track: { color: "#3e6da9" }, marker: { shape: "circle", active: "#3e6da9" },
      labelStyle: { size: "12px", color: "#555555" },
      allowOff: false, stateValues: { posLorT: "asc", posRorB: "desc" },
      default: "posLorT", margin: "82px 0 0 0", position: { row: 2, col: 2 },
      dependsOn: { control: "moderator", notEmpty: false, resetOnEnable: false },
    },
    {
      id: "cumulativeGroupOrder", type: "checkbox", option: "cumulativeGroupOrder",
      label: _("Subgroup Order"), checked: false,
      displayType: "toggle", checkboxSide: "left",
      margin: "12px 0 0 0", position: { row: 2, col: 1 },
      dependsOn: {
        controls: [ { control: "moderator", notEmpty: false }, { control: "subgroup", notEmpty: true } ],
        operator: "AND", resetOnEnable: false,
      },
    },
    {
      id: "cumulativeSortVariable", type: "select", option: "cumulativeSortVariable",
      label: _("Ordering Variable"), labelPosition: "top", width: "170px",
      options: [{ value: "", text: _("Select a variable") }],
      margin: "0", position: { row: 2, col: 2 },
      dependsOn: {
        controls: [
          { control: "moderator", notEmpty: false },
          { control: "cumulativeOrder", valueIn: ["variable"] },
        ], operator: "AND", resetOnEnable: true,
      },
    },
    ...extraControls,
  ],
  infoContent: buildInfoNotes([
    {
      title: _("Diagnostic Plots"),
      items: [
        { term: _("Show Plot / Resize"), options: ["infDiagPlotShow", "infQQPlotShow", "lhPlotShow", "scatterPlotShow", "radialPlotShow", "baujatPlotShow", "goshPlotShow", "tostPlotShow", ...(typeLabel === "Dichotomous" ? ["labbePlotShow"] : [])], desc: _("Select a named checkbox in Diagnostic Plots, or Show Plot in the other groups, to display a graph. Click the image, then drag its lower-right corner to resize it; Ctrl + arrow keys also adjust its size. Resizing preserves the fitted model and cached Influence/GOSH results.") },
        { term: _("Influence Diagnostic"), desc: _("Shows residual, influence and case-deletion measures to identify studies needing closer examination.") },
        { term: _("Q-Q Normal Plot"), desc: _("Compares standardized model residuals with normal quantiles.") },
        { term: _("Likelihood Plot"), desc: _("Shows likelihood curves for individual studies: their location and width indicate effect estimates and uncertainty.") },
      ],
    },
    {
      title: _("Scatter / Radial / Baujat"),
      items: [
        { term: _("Scatter/Bubble Plot"), options: ["scatterPlotCI", "scatterPlotPI", "scatterPlotScale", "scatterPlotLabel", "scatterPlotLegend", "scatterPlotGrid"], desc: _("Requires a Continuous moderator included as a main effect; each gets its own panel. Nominal/Ordinal moderators remain factors, including numeric codes, and stay in the fitted model without their own panel. Confidence Interval and Prediction Interval add bands; Weight by Precision scales bubbles; Study Label, Legend and Grid control annotations.") },
        { term: _("Radial Plot"), options: ["radialPlotCenter", "radialPlotSteps", "radialPlotTransf"], desc: _("Plots standardized effects against precision without moderators. Center on Estimate changes the reference; Y-axis Steps controls tick density; Arc Scale selects the displayed effect scale. Use a transformation appropriate to the selected effect measure. For PFT, Arc Scale is unavailable and the plot remains on the double arcsine scale.") },
        { term: _("Baujat Plot"), options: ["baujatPlotGrid", "baujatPlotSymbol"], desc: _("Shows contributions to heterogeneity versus influence on the fitted result. Grid adds reference lines; Symbol chooses study IDs, labels or points.") },
      ],
    },
    {
      title: _("Sensitivity and Equivalence"),
      items: [
        { term: _("GOSH Plot"), options: ["goshPlotOut", "goshPlotSubsets", "goshPlotHet"], desc: _("Fits many study subsets without moderators. Max Subsets limits the number sampled; Heterogeneity selects I², H², Q, τ² or τ; Highlight Study uses a study index (0 means no highlight) to distinguish subsets containing that study.") },
        { term: _("Equivalence Test (TOSTER)"), options: ["tostLower", "tostUpper", "tostAlpha", "showTost"], desc: _("Set the Lower and Upper equivalence bounds on the analysis scale and the significance level α. Show Table displays the two one-sided tests; Show Plot displays the interval and bounds. A non-significant effect alone does not establish equivalence.") },
        { term: _("Cumulative Meta-Analysis"), options: ["cumulativeTableShow", "cumulativePlotMode", "cumulativeOrder", "cumulativeDirection", "cumulativeGroupOrder", "cumulativeSortVariable", "cumulativeShade", "cumulativePrediction"], desc: _("Available without moderators. Each step adds one study and refits the selected model. With Subgroup selected, a separate cumulative sequence is fitted within each group; step and k restart in each group. At a single-study step, metafor uses z inference and heterogeneity is not estimable. Subgroup Order switches group labels from A–Z (off) to Z–A (on), independently of the cumulative study order and without refitting. It is available only with a Subgroup variable. Show Table is an HTML D3 table, independent of Classic/Off/Interactive, with expanding zoom, column selection and collapsible subgroups. Sorting table rows changes presentation only; Step and k retain the cumulative sequence. Order by the original dataset, standard error, study label or a selected variable such as publication year. Direction uses A–Z for ascending order and Z–A for descending order, including numeric ordering variables. Ties retain original order; missing ordering values are placed last. Model confidence level and supported back-transformations apply; PFT remains on the double arcsine scale. Classic and Interactive plots show the cumulative effect and confidence interval, followed by p-value, I-squared, tau and tau-squared. Under the common-effect model, tau and tau-squared are not estimated and their columns are omitted from both plots and the table. A permanent note and an informational toast explain this convention; other software may display separate heterogeneity estimates alongside FE pooled results. The model and estimator are stated below the plot. Prediction Interval adds native metafor limits for a new true effect to each cumulative prefix, within its own subgroup when selected, and adds PI columns to the table. At least two studies are required. Limits retain the selected test and confidence level; another software package may use a different prediction-interval method. The additional statistics stay on the fitted model scale; heterogeneity is unavailable at a single-study step. Shade Rows changes only the drawing. Intermediate p-values are descriptive and do not define a sequential stopping rule.") },
        { term: _("Leave-One-Out / Order"), options: ["leave1outPlotMode", "leave1outTableShow", "leave1outShade", "leave1outPrediction", "leave1outOrder", "leave1outGroupOrder"], desc: _("Common-effect model: tau-squared and tau are not estimated. Their columns are omitted from plots and tables under FE. For RE the statistical columns follow p-value, I-squared, tau and tau-squared. A brief informational toast explains that other software may display separate heterogeneity estimates alongside FE results. Classic/Off/Interactive chooses the plot representation; Show Table is independent: display either or both to examine fits omitting one study at a time, without moderators. Prediction Interval adds native metafor prediction limits for random-effects fits to Classic, Interactive and table views. It predicts a new true effect, uses the selected CI level and native test method, and is unavailable when fewer than two studies remain after omission. Shade Rows controls alternating backgrounds in both views; it does not refit the model or change any statistics. The table shares expanding zoom, column selection and collapsible subgroups with the Forest Table. With Subgroup selected, each study is omitted from its own subgroup and the model is refitted using only the remaining studies in that subgroup. Comparisons and influence highlights refer to the original subgroup pooled estimate. At least two studies are required per group. If one study remains after omission, metafor uses z inference even when t or Knapp-Hartung was selected; heterogeneity is then not estimable. Other unavailable results are shown as missing. Without Subgroup, omissions use the full dataset. Order sorts within each group by original order, absolute estimate change, estimate, p-value or study name. Subgroup Order switches group labels from A–Z (off) to Z–A (on), without refitting the models. The plot reports the effect and CI, p-value, tau-squared, tau and I-squared for each omission and original pooled fit. Tau and tau-squared stay on the analysis scale; heterogeneity is shown as missing when only one study remains.") },
        ...(typeLabel === "Dichotomous" ? [{ term: _("L'Abbé Plot"), options: ["labbePlotCI", "labbePlotPI", "labbePlotLegend", "labbePlotGrid", "labbeTransf"], desc: _("Compares treatment and control outcomes without moderators; unavailable for PETO. Confidence Interval, Prediction Interval, Legend and Grid control the display. Y/X axis selects the transformation for OR/RR; RD/AS retain their analysis scale.") }] : []),
      ],
    },
  ]),
});

const getSharedPublicationTab = () => ({
  id: "publication-tab",
  label: _("Publication Bias"),
  showTabTitle: false,
  tabTitle: "",
  tabSubtitle: "",
  tabTitleAlign: "left",
  layout: "grid",
  grid: {
    rows: 3,
    cols: 3,
    //rowBackgrounds: { 0: "#e3f2fd" },
    //cellBackgrounds: { "0-1": "#e3f2fd" },
    gap: "2px",
    itemGap: "2px",
  },
  groups: [
    {
      title: _("Standard Metrics"),
      position: { row: 1, col: 0 },
      collapsible: true,
      defaultExpanded: false,
    },
    {
      title: _("Test of Excess Significance"),
      position: { row: 1, col: 1 },
      colSpan: 2,
      collapsible: true,
      defaultExpanded: false,
    },
    {
      title: _("Selection Models"),
      position: { row: 2, col: 0 },
      colSpan: 3,
      collapsible: true,
      defaultExpanded: false,
    },
  ],
  controls: [
    // Mostra PBA (Bias di Pubblicazione)
    {
      id: "distabPBA",
      type: "checkbox",
      option: "distabPBA",
      label: _("Show PBA"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",     // left, right, top, bottom
      position: { row: 0, col: 0 },
    },
    // Mostra TES
    {
      id: "showTes",
      type: "checkbox",
      option: "showTes",
      label: _("Show TES"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",     // left, right, top, bottom
      position: { row: 0, col: 1 },
    },
    // p-curve
    {
      id: "pcurvePlotShow",
      type: "checkbox",
      option: "pcurvePlotShow",
      label: _("Show P-curve"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "right",     // left, right, top, bottom
      position: { row: 0, col: 2 },
      align: "top-right",
    },
    // Fail-Safe N Method
    {
      id: "fsntype",
      type: "radio",
      option: "fsntype",
      label: _("Fail-Safe N Method"),
      layout: "vertical",
      labelPosition: "right",
      options: [
        { value: "Rosenthal", text: _("Rosenthal") },
        { value: "Orwin",     text: _("Orwin") },
        { value: "Rosenberg", text: _("Rosenberg") },
      ],
      default: "Rosenthal",
      position: { row: 1, col: 0 },
    },
    // Hypothesis
    {
      id: "tesAlternative",
      type: "radio",
      option: "tesAlternative",
      label: _("Hypothesis"),
      layout: "vertical",
      labelPosition: "right",
      options: [
        { value: "less",      text: _("Less"), color: "#e74c3c" },      // Rosso deciso
        { value: "two.sided", text: _("Two Sided") },                   // Nessun colore immesso. Eredita!
        { value: "greater",   text: _("Greater"), color: "#2ecc71" },   // Verde morbido
      ],
      default: "greater",
      position: { row: 1, col: 1 },
    },
        // "H₀ Value",
    {
      id: "tesH0",
      type: "number",
      option: "tesH0",
      inputMode: "standard",  // "spin"
      label: _("H\u2080 value"), // "H₀ Value",
      labelPosition: "left",
      value: 0,
      itemGap: "1px",
      width: "40px",
      margin: "23px 0 0 110px",
      position: { row: 1, col: 1 },
    },
    // Alpha value
    {
      id: "tesAlpha",
      type: "number",
      option: "tesAlpha",
      inputMode: "standard",
      label: _("\u03B1 value"),
      labelPosition: "left",
      min: 0.001,
      max: 1,
      value: 0.05,
      step: 0.001,
      itemGap: "7px",
      width: "40px",
      margin: "49px 0 0 110px",
      position: { row: 1, col: 1 },
    },
    // Test
    {
      id: "tesTest",
      type: "radio",
      option: "tesTest",
      label: _("Test"),
      layout: "vertical",
      labelPosition: "right",
      color: "#eca038",
      options: [
        { value: "chi2", text: "\u03C7\u00B2" },
        { value: "binom", text: _("Binom") },
        { value: "exact", text: _("Exact") },
      ],
      default: "chi2",
      margin: "0 0 0 250px",
      position: { row: 1, col: 1 },
    },
    // Show Model
    {
      id: "showSelmodel",
      type: "checkbox",
      option: "showSelmodel",
      label: _("Show Model"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      position: { row: 2, col: 0 },
    },
    // Model Type
    {
      id: "selModelType",
      type: "select",
      option: "selModelType",
      label: _("Model Type"),
      labelPosition: "top",
      options: [
        { value: "beta",     text: _("Beta") },
        { value: "halfnorm", text: _("Half normal") },
        { value: "logistic", text: _("Logistic") },
        { value: "negexp",   text: _("Negative exponential") },
        { value: "power",    text: _("Power") },
        { value: "stepfun",  text: _("Step function") },
      ],
      default: "halfnorm",
      margin: "25px 0 0 0",
      position: { row: 2, col: 0 },
    },
    // Show P-uniform
    {
      id: "showPuniform",
      type: "checkbox",
      option: "showPuniform",
      label: _("Show P-uniform"),
      checked: false,
      displayType: "toggle",
      toggleOrientation: "horizontal",
      checkboxSide: "left",
      position: { row: 2, col: 1 },
    },
    // Method
    {
      id: "methodPunif",
      type: "select",
      option: "methodPunif",
      label: _("Method"),
      labelPosition: "top",
      options: [
        { value: "P",       text: _("Irwin-Hall distribution") },
        { value: "ML",      text: _("Maximum Likelihood") },
        { value: "LNP",     text: _("Fisher's method LNP") },
        { value: "LN1MINP", text: _("Fisher's method 1-p transf") },
        //{ value: "KS",      text: "Kolmogorov-Smirnov test" },
        //{ value: "AD",      text: "Anderson-Darling test" },
      ],
      default: "P",
      margin: "25px 0 0 0",
      position: { row: 2, col: 1 },
    },
    // Alpha value
    {
      id: "alphaPunif",
      type: "number",
      option: "alphaPunif",
      inputMode: "standard",
      label: _("\u03B1 value"),
      labelPosition: "left",
      min: 0.001,
      max: 1,
      value: 0.05,
      step: 0.001,
      itemGap: "1px",
      width: "40px",
      margin: "0 0 0 10px", // <--- 0 sopra, 0 dx, 0 sottto, 20px sx
      position: { row: 2, col: 2 },
    },
    // Side
    {
      id: "puniformSide",
      type: "radio",
      option: "puniformSide",
      label: _("Side"),
      layout: "horizontal",
      labelPosition: "right",
      options: [
        { value: "left",  text: _("Left") },
        { value: "right", text: _("Right"), color: "#e74c3c" },
      ],
      margin: "25px 0 0 10px", // <--- 0 sopra, 0 dx, 0 sottto, 20px sx
      position: { row: 2, col: 2 },
    },
  ],
  infoContent: buildInfoNotes([{
    title: _("Publication Bias Advanced"),
    items: [
      { term: _("Show PBA / Fail-Safe N Method"), options: ["distabPBA", "fsntype"], desc: _("Displays Fail-Safe N, Egger's regression, rank correlation and trim-and-fill. Choose Rosenthal, Orwin or Rosenberg for Fail-Safe N. Rank correlation and trim-and-fill are skipped with moderators; these methods do not prove the presence or absence of bias.") },
      { term: _("Show TES / Hypothesis"), options: ["showTes", "tesAlternative"], desc: _("Compares observed with expected significant findings. Greater tests excess significance, Less tests a deficit, and Two Sided tests either direction.") },
      { term: _("TES: H₀ / α / Test"), options: ["tesH0", "tesAlpha", "tesTest"], desc: _("H₀ sets the effect assumed when calculating study power, on the analysis scale. α defines study significance. Choose the chi-square approximation, binomial test or exact test for the count of significant findings.") },
      { term: _("Show Model / Model Type"), options: ["showSelmodel", "selModelType"], desc: _("Fits a selection model with Beta, Half normal, Logistic, Negative exponential, Power or Step function selection weights. Results depend on the selection assumptions and model convergence.") },
      { term: _("Show P-curve"), options: ["pcurvePlotShow"], desc: _("Examines significant two-sided Wald z p-values calculated from study effects and sampling variances. At least three significant studies are required. These approximations may differ from the original study tests; interpretation depends on study selection and test assumptions.") },
      { term: _("Show P-uniform / Method"), options: ["showPuniform", "methodPunif"], desc: _("Estimates an effect using significant results. Method selects Irwin-Hall, Maximum Likelihood or a Fisher combination of p-values (LNP or 1-p transformation).") },
      { term: _("P-uniform: α / Side"), options: ["alphaPunif", "puniformSide"], desc: _("α sets the significance threshold used to select results; it does not set the confidence level. The library returns 95% confidence intervals independently of the Model CI option. Side specifies the effect direction. Where effects and variances are supplied, a z-test approximation may differ from the original study tests; the correlation model supplies raw correlations and sample sizes.") },
    ],
  }]),
});

const getSharedSaveTab = (typeLabel) => ({
  id: "save-tab",
  label: _("Save"),
  showTabTitle: false,
  tabTitle: "",
  tabSubtitle: "",
  tabTitleAlign: "left",
  layout: "grid",
  grid: {
    rows: 2,
    cols: 4,
    gap: "2px",
    itemGap: "2px",
  },
  groups: [
    {
      title: _("Save to Sheet"),
      position: { row: 0, col: 0 },
      rowSpan: 2,
      colSpan: 2,
      collapsible: true,
      defaultExpanded: false,
    },
  ],
  controls: [
    { 
      id: "effectsizeOV",
      option: "effectsizeOV",
      type: "output", 
      label: _("New_ES"),
      checked: false,
      displayType: "toggle",
      color: "#2ECC71",
      position: { row: 0, col: 0},
    },
    { 
      id: "varianceOV",
      option: "varianceOV",
      type: "output",
      label: _("New_VAR"),
      checked: false,
      displayType: "toggle",
      color: "orange",
      position: { row: 0, col: 1 },
    },
    { 
      id: "stderrorOV",
      option: "stderrorOV",
      type: "output",
      label: _("New_SE"),
      checked: false,
      displayType: "toggle",
      color: "orange",
      position: { row: 1, col: 1},
    },
    // Logging Mode: selettore a tre stati File Only/Off/File + UI HTML
    {
      id: "jlogMode",
      type: "deviatore",
      option: "jlogMode",
      label: _("Logging Mode"),
      labelPosition: "center",
      orientation: "horizontal",
      states: { left: _("File Only"), right: _("File + UI HTML"), colorA: "#eca038", colorB: "#3e6da9" },
      thumb:  { color: "#ffffff", width: "10px", height: "8px" },
      default: "posOff",
      position: { row: 0, col: 2, colSpan:2 },
    },
  ],
  infoContent: buildInfoNotes([
    {
      title: _("Save Variables to Dataset"),
      items: [
        { term: _("Effect Sizes (New_ES)"), options: ["effectsizeOV"], desc: _("Saves study effects on the analysis scale, including any transformation used for fitting.") },
        { term: _("Variances (New_VAR)"), options: ["varianceOV"], desc: _("Saves sampling variances on the analysis scale.") },
        { term: _("Standard Errors (New_SE)"), options: ["stderrorOV"], desc: _("Saves the square roots of the sampling variances.") },
        { term: _("Purpose"), desc: _("Exports calculated values as new columns for further analysis in the dataset.") },
      ],
    },
    {
      title: _("Developer Debug Logging"),
      variant: "dev",
      icon: DEV_SECTION_ICON,
      items: [
        {
          term: _("Show Logging Mode"),
          options: ["jlogMode"],
          desc: `Press ${kbdCombo([_("Ctrl"), _("Shift"), "L"])} to reveal the Logging Mode switch above — choose <strong>File Only</strong> or <strong>File + UI HTML</strong> to capture diagnostic output when reporting hard-to-reproduce errors to the developer.`,
        },
        {
          term: _("Hide Logging Mode"),
          desc: `Press ${kbdCombo([_("Ctrl"), _("Alt"), "L"])} to stop logging, reset the mode to Off, and hide the switch again.`,
        },
      ],
    },
  ]),
});

module.exports = {

  // Relationships
  correlation: [
    getSharedModelTab(
      "Correlation",
      [
        {
          id: "modelmea",
          type: "select",
          option: "modelmea",
          label: _("Effect Size"),
          position: { row: 0, col: 1, colSpan: 2 },
          labelPosition: "left",
          options: [
            { value: "COR",  text: _("Raw Correlation") },
            { value: "UCOR", text: _("Unbiased Correlation") },
            { value: "ZCOR", text: _("Fisher's z Transformed Correlation") },
          ],
          default: "COR",
        },
        {
          id: "showBackTransform",
          type: "checkbox",
          option: "showBackTransform",
          label: _("Back-Transformed"),
          selectionAnimation: "rotate-icon",
          respectReducedMotion: false,
          checked: false,
          checkboxSide: "left",
          displayType: "toggle",
          position: { row: 1, col: 1, colSpan: 2 },
          align: "center-center",
          dependsOn: {
            control: "modelmea",
            hideWhen: ["COR", "UCOR"],
            resetOnEnable: true,
          }
        },
      ],
      buildInfoNotes([
        MODEL_CONFIG_SECTION,
        {
          title: _("Effect Size Measure"),
          items: [
            ...buildMeasureItems([
              { code: "COR",  title: _("Raw Correlation"), desc: _("Not recommended for meta-analysis due to bias at small sample sizes.") },
              { code: "UCOR", title: _("Unbiased Correlation"), desc: _("Corrects the raw correlation for small-sample bias.") },
              { code: "ZCOR", title: _("Fisher's z Transformed Correlation"), desc: _("Standard variance-stabilizing transformation.") },
            ]),
            { term: _("Back-Transformed"), options: ["showBackTransform"], desc: _("Displays Fisher's z estimates and interval limits as correlations. Fitting remains on the Fisher's z scale.") },
          ],
        },
        INFERENCE_SECTION,
      ])
    ),
    getSharedForestTab("Correlation"),
    getSharedFunnelTab("Correlation"),
    getSharedDiagnosticTab("Correlation"),
    getSharedPublicationTab(),
    getSharedSaveTab("Correlation"),
  ],
  alpha: [
    getSharedModelTab(
      "Alpha",
      [
        {
          id: "modelmea",
          type: "select",
          option: "modelmea",
          label: _("Effect Size"),
          position: { row: 0, col: 1, colSpan: 2 },
          labelPosition: "left",
          options: [
            { value: "ARAW", text: _("Raw Alpha") },
            { value: "AHW",  text: _("Transformed Alpha (Hakstian & Whalen)") },
            { value: "ABT",  text: _("Transformed Alpha (Bonett)") },
          ],
          default: "ARAW",
        },
        {
          id: "showBackTransform",
          type: "checkbox",
          option: "showBackTransform",
          label: _("Back-Transformed"),
          selectionAnimation: "rotate-icon",
          respectReducedMotion: false,
          displayType: "toggle",
          checked: false,
          checkboxSide: "left",
          position: { row: 1, col: 1, colSpan: 2 },
          align: "center-center",
          dependsOn: { control: "modelmea", hideWhen: ["ARAW"], resetOnEnable: true },
        },
      ],
      buildInfoNotes([
        MODEL_CONFIG_SECTION,
        {
          title: _("Effect Size Measure"),
          items: [
            ...buildMeasureItems([
              { code: "ARAW", title: _("Raw Alpha"), desc: _("Uses Cronbach's alpha without transformation; negative estimates are possible.") },
              { code: "AHW",  title: _("Transformed Alpha (Hakstian & Whalen)"), desc: _("Variance-stabilizing transformation for alpha.") },
              { code: "ABT",  title: _("Transformed Alpha (Bonett)"), desc: _("Alternative transformation for reliability coefficients.") },
            ]),
            { term: _("Back-Transformed"), options: ["showBackTransform"], desc: _("Displays transformed estimates and interval limits on the original alpha scale.") },
          ],
        },
        INFERENCE_SECTION,
      ])
    ),
    getSharedForestTab("Alpha"),
    getSharedFunnelTab("Alpha"),
    getSharedDiagnosticTab("Alpha"),
    getSharedPublicationTab(),
    getSharedSaveTab("Alpha"),
  ],

  // Comparative
  dichotomous: [
    getSharedModelTab(
      "Dichotomous",
      [
        {
          id: "modelmea",
          type: "select",
          option: "modelmea",
          label: _("Effect Size"),
          labelPosition: "left",
          options: [
            { value: "RR",   text: _("Log Risk Ratio") },
            { value: "OR",   text: _("Log Odds Ratio") },
            { value: "PETO", text: _("Log Odds Ratio (Peto)") },
            { value: "RD",   text: _("Risk Difference") },
            { value: "AS",   text: _("Arcsine Difference") },
          ],
          default: "OR",
          position: { row: 0, col: 1, colSpan: 2 },
        },
        {
          id: "showBackTransform",
          type: "checkbox",
          option: "showBackTransform",
          label: _("Back-Transformed"),
          selectionAnimation: "rotate-icon",
          respectReducedMotion: false,
          checked: false,
          checkboxSide: "left",
          displayType: "toggle",
          position: { row: 1, col: 1, colSpan: 2 },
          align: "center-center",
          dependsOn: { control: "modelmea", hideWhen: ["RD", "AS"], resetOnEnable: true },
        },
      ],
      buildInfoNotes([
        MODEL_CONFIG_SECTION,
        {
          title: _("Effect Size Measure"),
          items: [
            ...buildMeasureItems([
              { code: "RR",   title: _("Log Risk Ratio"), desc: _("Ratio of event risk between groups, natural-log scale.") },
              { code: "OR",   title: _("Log Odds Ratio"), desc: _("Ratio of event odds between groups, on the natural-log scale.") },
              { code: "PETO", title: _("Log Odds Ratio (Peto)"), desc: _("Peto's approximation to the log odds ratio. Suitability depends on event rates, effect size and balance between groups.") },
              { code: "RD",   title: _("Risk Difference"), desc: _("Absolute difference in event risk between groups, original scale.") },
              { code: "AS",   title: _("Arcsine Difference"), desc: _("Difference between the arcsine square-root transformed group risks. It has no unique inverse to a risk difference.") },
            ]),
            { term: _("Back-Transformed"), options: ["showBackTransform"], desc: _("Displays OR, RR and PETO estimates as ratios; the forest retains log-spaced coordinates. RD is already on its original scale. AS cannot be inverted to a risk difference.") },
          ],
        },
        INFERENCE_SECTION,
      ])
    ),
    getSharedForestTab("Dichotomous", true),
    getSharedFunnelTab("Dichotomous"),
    getSharedDiagnosticTab("Dichotomous", labbeDiagnosticConfig.groups, labbeDiagnosticConfig.controls),
    getSharedPublicationTab(),
    getSharedSaveTab("Dichotomous"),
  ],
  meandiff: [
    getSharedModelTab(
      "Mean Difference",
      [
        {
          id: "modelmea",
          type: "select",
          option: "modelmea",
          label: _("Effect Size"),
          position: { row: 0, col: 1, colSpan: 2 },
          labelPosition: "left",
          options: [
            { value: "MD",    text: _("Mean Difference") },
            { value: "SMD",   text: _("Standardized Mean Difference") },
            { value: "SMDH",  text: _("SMD (Heteroscedastic)") },
            { value: "SMD1",  text: _("SMD (using SD1)") },
            { value: "SMD1H", text: _("SMD (SD1, Heteroscedastic)") },
            { value: "ROM",   text: _("Log Ratio of Means") },
          ],
          default: "MD",
        },
        {
          id: "showBackTransform",
          type: "checkbox",
          option: "showBackTransform",
          label: _("Back-Transformed"),
          selectionAnimation: "rotate-icon",
          respectReducedMotion: false,
          checked: false,
          checkboxSide: "left",
          displayType: "toggle",
          position: { row: 1, col: 1, colSpan: 2 },
          align: "center-center",
          dependsOn: {
            control: "modelmea",
            hideWhen: ["MD", "SMD", "SMDH", "SMD1", "SMD1H"],
            resetOnEnable: true,
          }
        },
      ],
      buildInfoNotes([
        MODEL_CONFIG_SECTION,
        {
          title: _("Effect Size Measure"),
          items: [
            ...buildMeasureItems([
              { code: "MD",    title: _("Mean Difference"), desc: _("Raw difference between group means, in the original measurement units.") },
              { code: "SMD",   title: _("Standardized Mean Difference"), desc: _("Bias-corrected standardized mean difference (Hedges' g), using the pooled SD.") },
              { code: "SMDH",  title: _("SMD (Heteroscedastic)"), desc: _("Standardized mean difference using separate group variances instead of a pooled SD.") },
              { code: "SMD1",  title: _("SMD (using SD1)"), desc: _("Standardized mean difference scaled by the SD of Group 1 only.") },
              { code: "SMD1H", title: _("SMD (SD1, Heteroscedastic)"), desc: _("Combines SD1 scaling with heteroscedastic variance estimation.") },
              { code: "ROM",   title: _("Log Ratio of Means"), desc: _("Ratio of group means, natural-log scale; suited to outcomes on a ratio scale.") },
            ]),
            { term: _("Back-Transformed"), options: ["showBackTransform"], desc: _("Displays ROM estimates and interval limits as ratios of means.") },
          ],
        },
        INFERENCE_SECTION,
      ])
    ),
    getSharedForestTab("Mean Difference", true),
    getSharedFunnelTab("Mean Difference"),
    getSharedDiagnosticTab("Mean Difference"),
    getSharedPublicationTab(),
    getSharedSaveTab("Mean Difference"),
  ],

  // Summary
  effectsize: [
    getSharedModelTab(
      "Effect Size",
      [
        {
          id: "modelmea",
          type: "select",
          option: "modelmea",
          label: _("Effect Size"),
          position: { row: 0, col: 1, colSpan: 2 },
          labelPosition: "left",
          options: [
            { value: "GEN", text: _("Generic ES") },
          ],
          default: "GEN",
        },
        // Il selettore sostituisce l'etichetta nativa del supplier, senza duplicare l'opzione.
        {
          id: "meaType",
          type: "segmentedControl",
          variant: "bracket",
          ariaLabel: _("Measure Type"),
          option: "meaType",
          label: "",
          mount: { targetOption: "vi", slot: "label" },
          orientation: "horizontal",
          labelPosition: "left",
          position: { row: 2, col: 1, colSpan: 2 },
          align: "top-left",
          default: "vimt",
          activeColor: "#3e6da9",
          backgroundColor: "transparent",
          options: [
            { value: "vimt", text: _("Variance") },
            { value: "seimt", text: _("SE") },
            { value: "cimt", text: _("CI") },
          ],
        },
      ],
      buildInfoNotes([
        MODEL_CONFIG_SECTION,
        {
          title: _("Effect Size Measure"),
          items: [
            { term: _("Generic Effect Size (GEN)"), options: ["modelmea"], desc: _("Uses precomputed effects on the supplied scale. All studies must use a comparable effect measure.") },
            { term: _("Variance / SE / CI"), options: ["meaType", "inputCiLevel"], desc: _("Use the horizontal selector above the input boxes to choose Variance, SE or CI. Standard errors are squared internally. CI mode requires lower and upper limits and their original confidence level in Input: next to the selector, independently of the output confidence level. Variances are approximated from the interval width assuming normal-based Wald intervals. Effects and limits must already use the same analysis scale (for example, all on the log scale for log ratios). Do not use untransformed ratio limits with log effects, or profile, exact, bootstrap or t-based intervals with this approximation.") },
          ],
        },
        INFERENCE_SECTION,
      ]),
      { levelPosition: { row: 1, col: 1, colSpan: 2 } }
    ),
    getSharedForestTab("Effect Size", true),
    getSharedFunnelTab("Effect Size"),
    getSharedDiagnosticTab("Effect Size"),
    getSharedPublicationTab(),
    getSharedSaveTab("Effect Size"),

  ],
  proportion: [
    getSharedModelTab(
      "Proportion",
      [
        {
          id: "modelmea",
          type: "select",
          option: "modelmea",
          label: _("Effect Size"),
          position: { row: 0, col: 1, colSpan: 2 },
          labelPosition: "left",
          options: [
            { value: "PR",  text: _("Raw proportion") },
            { value: "PLN", text: _("Log transformed") },
            { value: "PLO", text: _("Logit transformed") },
            { value: "PAS", text: _("Arcsine square root transformed") },
            { value: "PFT", text: _("Freeman-Tukey double arcsine transformed") },
            { value: "PRZ", text: _("Probit transformed") },
          ],
          default: "PR",
          selectionToasts: {
            PFT: {
              toastTitle: _("PFT display scale"),
              message: _("PFT estimates and confidence intervals remain on the double arcsine scale in all outputs. Back-transformation is unavailable because the pooled inverse requires a reference sample size. See Info & Notes for details."),
              duration: 8000,
            },
          },
        },
        {
          id: "showBackTransform",
          type: "checkbox",
          option: "showBackTransform",
          label: _("Back-Transformed"),
          selectionAnimation: "rotate-icon",
          respectReducedMotion: false,
          checked: false,
          checkboxSide: "left",
          displayType: "toggle",
          position: { row: 1, col: 1, colSpan: 2 },
          align: "center-center",
          dependsOn: { control: "modelmea", hideWhen: ["PR", "PFT"], resetOnEnable: true },
        },
      ],
      buildInfoNotes([
        MODEL_CONFIG_SECTION,
        {
          title: _("Effect Size Measure"),
          items: [
            ...buildMeasureItems([
              { code: "PR",  title: _("Raw Proportion"), desc: _("Original proportion scale [0, 1]; no transformation.") },
              { code: "PLN", title: _("Log Proportion"), desc: _("Natural logarithm of the proportion.") },
              { code: "PLO", title: _("Logit Proportion"), desc: _("Log odds of the proportion; zero corresponds to a proportion of 0.5.") },
              { code: "PAS", title: _("Arcsine Proportion"), desc: _("Variance-stabilizing arcsine square-root transformation.") },
              { code: "PFT", title: _("Freeman-Tukey (Double Arcsine)"), desc: _("Estimates and intervals remain on the double arcsine scale in all outputs. Back-transformation is unavailable because the pooled inverse requires a reference sample size and can be sensitive to its choice.") },
              { code: "PRZ", title: _("Probit Proportion"), desc: _("Normal-quantile (probit) transformation; symmetric alternative to the logit.") },
            ]),
            { term: _("Back-Transformed"), options: ["showBackTransform"], desc: _("Displays PLN, PLO, PAS and PRZ estimates and interval limits as proportions. PR already uses the proportion scale. PFT always remains on its transformed scale in Classic and Interactive output.") },
          ],
        },
        INFERENCE_SECTION,
      ])
    ),
    getSharedForestTab("Proportion"),
    getSharedFunnelTab("Proportion"),
    getSharedDiagnosticTab("Proportion"),
    getSharedPublicationTab(),
    getSharedSaveTab("Proportion"),
  ]

};

// Mantiene separati i testi sorgente e l'HTML, così la lingua non altera le opzioni.
for (const tabs of Object.values(module.exports)) {
  for (const tab of tabs) tab.infoSections = INFO_NOTES_SECTIONS.get(tab.infoContent);
}
Object.defineProperty(module.exports, 'renderInfoNotes', {value: buildInfoNotes});
