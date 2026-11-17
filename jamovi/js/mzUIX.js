// jamovi/js/mzUIX.js
// Ultima modifica: 2025-12-21 18:30 - Aggiunto supporto hideWhen/showWhen per nascondere controlli
// Sistema Modulare TabbedControl Generico
// Versione modificata con Vanilla JavaScript (de-jQueryed)

"use strict";

// Marcatore per l'estrattore; la traduzione effettiva usa il controllo dell'istanza.
const _ = text => text;

// =====================================================
// HELPER PER LOCALIZZAZIONE NUMERICA
// =====================================================

// Rileva la lingua risalendo il DOM per trovare l'attributo lang di jamovi
function getJamoviLanguage(element) {
  let el = element;
  while (el) {
    if (el.getAttribute && el.getAttribute('lang')) {
      return el.getAttribute('lang');
    }
    el = el.parentElement || (el.parentNode && el.parentNode.host);
  }
  // Se non troviamo nulla, proviamo a vedere se siamo in un ambiente che preferisce il punto
  // (es. jamovi in inglese su OS italiano spesso non setta lang="en" ovunque)
  return "en-US";
}

// =====================================================
// FUNZIONE PRINCIPALE DI SETUP DEL TABBED CONTROL
// =====================================================
// Accetta la configurazione come parametro esterno
// Questa funzione viene chiamata dai singoli moduli con la loro configurazione

const TABBED_BINDINGS = new WeakMap();
const TABBED_DISPOSAL_HOOKS = new WeakSet();

// Include i controlli montati nelle etichette native, limitando la ricerca all'istanza.
function queryUIXAll(element, selector) {
  const nodes = [...element.querySelectorAll(selector)];
  const bindings = TABBED_BINDINGS.get(element);
  for (const root of (bindings && bindings.mounts) || []) {
    if (root.matches(selector)) nodes.push(root);
    nodes.push(...root.querySelectorAll(selector));
  }
  return [...new Set(nodes)];
}

function queryUIX(element, selector) {
  return queryUIXAll(element, selector)[0] || null;
}

// Il supplier conserva il suo label e il relativo riferimento aria-labelledby.
function mountNativeLabels(element, ui, tabConfig) {
  const bindings = TABBED_BINDINGS.get(element);
  let pending = 0;
  for (const tab of tabConfig) for (const control of getAllControls(tab)) {
    if (!control.mount || control.mount.slot !== 'label') continue;
    const field = ui[control.mount.targetOption];
    const fieldElement = field && (field.el || field.$el);
    const target = fieldElement && (fieldElement[0] || fieldElement);
    const labelIds = target && target.getAttribute('aria-labelledby');
    const root = element.getRootNode();
    const scope = typeof root.getElementById === 'function' ? root : element.ownerDocument;
    const label = labelIds && labelIds.split(/\s+/).map(id =>
      scope.getElementById(id)).find(node =>
        node && node.classList.contains('silky-target-list-header'));
    const input = queryUIX(element, `#mz_${control.id}`);
    const wrapper = input && input.closest('.mz-control-wrapper');
    if (wrapper && bindings.mounts.includes(wrapper)) continue;
    if (!label || !wrapper) { pending++; continue; }
    const originalNodes = [...label.childNodes];
    const anchor = document.createComment('mzUIX native label mount');
    wrapper.before(anchor);
    label.replaceChildren(wrapper);
    wrapper.classList.add('mz-native-label-control');
    // L'intestazione nativa può restringersi al testo: usa la larghezza del target reale.
    const previousWidth = wrapper.style.getPropertyValue('--mz-native-target-width');
    const syncTargetWidth = () => {
      if (target.offsetWidth > 0)
        wrapper.style.setProperty('--mz-native-target-width', `${target.offsetWidth}px`);
    };
    syncTargetWidth();
    const sizeObserver = new ResizeObserver(syncTargetWidth);
    sizeObserver.observe(target);
    bindings.mounts.push(wrapper);
    bindings.cleanups.push(() => {
      sizeObserver.disconnect();
      if (previousWidth) wrapper.style.setProperty('--mz-native-target-width', previousWidth);
      else wrapper.style.removeProperty('--mz-native-target-width');
      if (wrapper.parentNode === label) label.replaceChildren(...originalNodes);
      wrapper.remove();
      anchor.remove();
    });
  }
  return pending;
}

// creating precede onRenderToGrid: l'etichetta e aria-labelledby possono arrivare dopo.
function setupNativeLabelMounts(element, ui, tabConfig) {
  if (!mountNativeLabels(element, ui, tabConfig)) return;
  const root = element.getRootNode();
  const scope = root.nodeType === 9 || root.host ? root : element.ownerDocument;
  const observer = new MutationObserver(() => {
    if (!mountNativeLabels(element, ui, tabConfig)) observer.disconnect();
  });
  observer.observe(scope, { subtree: true, childList: true,
    attributes: true, attributeFilter: ['aria-labelledby'] });
  TABBED_BINDINGS.get(element).cleanups.push(() => observer.disconnect());
}

// Tooltip condiviso: il contenitore resta accessibile anche con input disabilitato.
let dependencyTooltipId = 0;
function createDependencyTooltip(target, bindings) {
  const tip = document.createElement('div');
  tip.id = `mz-dependency-tooltip-${++dependencyTooltipId}`;
  tip.className = 'mz-dependency-tooltip';
  tip.setAttribute('role', 'tooltip');
  tip.hidden = true;
  document.body.appendChild(tip);
  const previousDescription = target.getAttribute('aria-describedby');
  const previousTabIndex = target.getAttribute('tabindex');
  const hide = () => { tip.hidden = true; };
  const show = () => {
    if (!tip.textContent) return;
    tip.hidden = false;
    const rect = target.getBoundingClientRect();
    const box = tip.getBoundingClientRect();
    tip.style.left = `${Math.max(8, Math.min(rect.left, window.innerWidth - box.width - 8))}px`;
    tip.style.top = `${Math.max(8, rect.bottom + box.height + 8 < window.innerHeight
      ? rect.bottom + 6 : rect.top - box.height - 6)}px`;
  };
  const keydown = event => { if (event.key === 'Escape') hide(); };
  const listeners = [[target,'mouseenter',show], [target,'mouseleave',hide],
    [target,'focusin',show], [target,'focusout',hide], [target,'keydown',keydown],
    [window,'scroll',hide,true], [window,'resize',hide]];
  listeners.forEach(([node,type,fn,capture]) => node.addEventListener(type,fn,!!capture));
  const restore = (name, value) => value === null
    ? target.removeAttribute(name) : target.setAttribute(name,value);
  bindings.cleanups.push(() => {
    listeners.forEach(([node,type,fn,capture]) => node.removeEventListener(type,fn,!!capture));
    restore('aria-describedby',previousDescription);
    restore('tabindex',previousTabIndex);
    tip.remove();
  });
  return text => {
    target.removeAttribute('title');
    tip.textContent = text;
    if (text) {
      target.setAttribute('aria-describedby', [previousDescription,tip.id].filter(Boolean).join(' '));
      target.setAttribute('tabindex','0');
    } else {
      hide();
      restore('aria-describedby',previousDescription);
      restore('tabindex',previousTabIndex);
    }
  };
}

// Rimuove anche gli eventi sul documento, che sopravvivono ai nodi sostituiti.
function disposeTabbedControl(uiOrElement) {
  const control = uiOrElement && uiOrElement.tabbedControl;
  let element = control ? control.$el : uiOrElement;
  if (element && element[0]) element = element[0];
  if (!element) return;
  const bindings = TABBED_BINDINGS.get(element);
  if (control && bindings && bindings.owner !== control) return;
  if (bindings) bindings.cleanups.forEach(cleanup => cleanup());
  TABBED_BINDINGS.delete(element);
  DEPENDENCY_REFRESHERS.delete(element);
}

// Traduce solo le proprietà di presentazione, lasciando invariati ID, valori e dipendenze.
function translateUIXConfig(value, translate) {
  const textKeys = new Set(['label', 'title', 'text', 'tabTitle', 'tabSubtitle',
    'placeholder', 'labelLorT', 'labelRorB', 'labelLow', 'labelHigh']);
  function visit(node) {
    if (Array.isArray(node)) return node.map(visit);
    if (!node || typeof node !== 'object') return node;
    return Object.fromEntries(Object.entries(node).map(([key, item]) => [key,
      textKeys.has(key) && typeof item === 'string' && item.trim() !== ''
        ? translate(item) : visit(item)]));
  }
  return visit(value);
}

// Sintassi raggruppata → chiavi piatte usate dal costruttore.
// Eseguita PRIMA di translateUIXConfig, così anche i default _("Left")/_("Right")
// vengono tradotti. Priorità: chiave raggruppata > chiave piatta > default.
const pick = (...values) => values.find(v => v !== undefined);

function normalizeDeviatore(control) {
  const { thumb = {}, marker = {}, track = {}, states = {}, ...rest } = control;
  return {
    ...rest,
    thumbShape:  pick(thumb.shape,  rest.thumbShape),
    thumbColor:  pick(thumb.color,  rest.thumbColor),
    thumbWidth:  pick(thumb.width,  rest.thumbWidth),
    thumbHeight: pick(thumb.height, rest.thumbHeight),
    markerShape: pick(marker.shape,  rest.markerShape),
    markerFill:  pick(marker.fill,   rest.markerFill),
    activeColor: pick(marker.active, rest.activeColor),
    shapeColor:  pick(track.color,   rest.shapeColor),
    colorA:      pick(states.colorA, rest.colorA),
    colorB:      pick(states.colorB, rest.colorB),
    // "" esplicito = nessuna etichetta; solo undefined ripiega sul default
    labelLorT:   pick(states.left,  rest.labelLorT, _("Left")),
    labelRorB:   pick(states.right, rest.labelRorB, _("Right")),
  };
}

// label: `style` oggetto → chiavi piatte. Una stringa resta CSS grezzo (compatibilità).
function normalizeLabel(control) {
  const { style, line = {}, ...rest } = control;
  const s = style && typeof style === 'object' ? style : {};
  return {
    ...rest,
    style:           typeof style === 'string' ? style : undefined,
    fontSize:        pick(s.size,       rest.fontSize),
    bold:            pick(s.bold,       rest.bold),
    italic:          pick(s.italic,     rest.italic),
    color:           pick(s.color,      rest.color),
    backgroundColor: pick(s.background, rest.backgroundColor),
    borderRadius:    pick(s.radius,     rest.borderRadius),
    padding:         pick(s.padding,    rest.padding),
    lineWidth:       pick(line.width,   rest.lineWidth),
    lineColor:       pick(line.color,   rest.lineColor),
  };
}

function normalizeUIXControl(control) {
  const out = control.type === 'deviatore' ? normalizeDeviatore(control)
    : control.type === 'label' ? normalizeLabel(control)
    : control;  
  return Array.isArray(out.children)
    ? { ...out, children: out.children.map(normalizeUIXControl) }
    : out;
}

function normalizeUIXConfig(tabConfig) {
  const mapControls = list => list && list.map(normalizeUIXControl);
  return tabConfig.map(tab => ({
    ...tab,
    controls: mapControls(tab.controls),
    groups: tab.groups && tab.groups.map(group => group.subTabs
      ? { ...group, subTabs: group.subTabs.map(sub => ({ ...sub, controls: mapControls(sub.controls) })) }
      : group),
  }));
}

function setupTabbedControl(ui, tabConfig) {
  // Se tabConfig non è passato, prova a prenderlo dall'UI
  if (!tabConfig && ui.TAB_CONFIG) {
    tabConfig = ui.TAB_CONFIG;
  }

  // Se ancora non c'è configurazione, esce
  if (!tabConfig) {
    console.error("TabbedControl: Nessuna configurazione TAB_CONFIG trovata!");
    return;
  }

  tabConfig = normalizeUIXConfig(tabConfig);
  tabConfig = translateUIXConfig(tabConfig, text =>
    typeof ui.tabbedControl.translate === 'function' ? ui.tabbedControl.translate(text) : text);

  // Ottiene il riferimento all'elemento DOM dal framework jamovi
  let element = ui.tabbedControl.$el;
  if (element && element[0]) {
    element = element[0]; // Converte da jQuery a DOM nativo
  }

  disposeTabbedControl(element);
  // CustomControl.onDisposed non emette disposing: aggancia soltanto questa istanza.
  const control = ui.tabbedControl;
  if (typeof control.onDisposed === 'function' && !TABBED_DISPOSAL_HOOKS.has(control)) {
    const originalDisposed = control.onDisposed;
    control.onDisposed = function () {
      disposeTabbedControl({ tabbedControl: this });
      return originalDisposed.apply(this, arguments);
    };
    TABBED_DISPOSAL_HOOKS.add(control);
  }

  // Rileva la lingua corretta di jamovi risalendo il DOM
  const lang = getJamoviLanguage(element);
  element.dataset.lang = lang;

  // Genera la struttura HTML completa delle schede
  const tabbedHtml = generateTabbedStructure(tabConfig, lang);
  element.innerHTML = tabbedHtml;
  const infoTitle = queryUIX(element, '.info-notes-title');
  if (infoTitle && typeof ui.tabbedControl.translate === 'function') {
    infoTitle.textContent = ui.tabbedControl.translate(_('Info & Notes'));
  }

  // Nasconde i bottoni Action nativi jamovi gestiti dal frontend custom

  // Configura gli eventi e le interazioni
  setupTabNavigation(element, tabConfig);
  setupSubTabNavigation(element);
  setupInfoNotesToggle(element);
  setupControlBindings(element, ui, tabConfig);
  hideNativeActionButtons(tabConfig, ui, TABBED_BINDINGS.get(element));
  watchNumericFormat(element, ui, tabConfig);

  initializeControlValues(element, ui, tabConfig);
  setupControlDependencies(element, ui, tabConfig);
  setupAccordionGroups(element, ui, tabConfig);
  setupNativeLabelMounts(element, ui, tabConfig);

  // jamovi riusa la vista e carica silenziosamente le opzioni di un'altra analisi.
  // ready arriva dopo il caricamento: riallinea i valori senza ricreare i tab.
  if (ui.view && typeof ui.view.on === 'function' && typeof ui.view.off === 'function') {
    const onReady = () => {
      initializeControlValues(element, ui, tabConfig);
      refreshControlDependencies(ui);
    };
    ui.view.on('ready', onReady);
    TABBED_BINDINGS.get(element).cleanups.push(() => ui.view.off('ready', onReady));
  }
  // CustomControl.update è emesso anche quando cambiano i tipi delle colonne.
  if (typeof control.on === 'function' && typeof control.off === 'function') {
    const onMetadataChange = () => refreshControlDependencies(ui);
    control.on('update', onMetadataChange);
    TABBED_BINDINGS.get(element).cleanups.push(() => control.off('update', onMetadataChange));
  }
}

// Preferenza esplicita di jamovi: indipendente da lingua del modulo e locale del SO.
function getJamoviNumberFormat() {
  return typeof FormatDef !== 'undefined' && FormatDef.number ? FormatDef.number : null;
}

// Aggiornamento live senza scritture alle opzioni; rimozione nel dispose dell'istanza.
function watchNumericFormat(element, ui, tabConfig) {
  const format = getJamoviNumberFormat();
  if (!format || typeof format.on !== 'function' || typeof format.off !== 'function') return;
  const refresh = () => initializeControlValues(element, ui, tabConfig);
  format.on('displayFormatChanged', refresh);
  TABBED_BINDINGS.get(element).cleanups.push(() => format.off('displayFormatChanged', refresh));
}

function getDecimalSeparator(lang) {
  const format = getJamoviNumberFormat();
  if (format && (format.decSymbol === '.' || format.decSymbol === ',')) return format.decSymbol;
  try {
    return new Intl.NumberFormat(lang || 'en-US').formatToParts(1.1)
      .find(part => part.type === 'decimal').value;
  } catch (e) { return '.'; }
}

// Lo spin modifica solo un numero finito e rispetta i limiti dichiarati.
// Larghezze comprensive del padding; spazio minimo per le cifre del passo decimale.
function numericInputWidth(control, inline) {
  const raw = control.width || (inline ? '70px' : '80px');
  if (!/^\d+(?:\.\d+)?px$/.test(raw)) return raw;
  const step = String(control.step || 1);
  const decimals = step.includes('e-') ? Number(step.split('e-')[1])
    : (step.split('.')[1] || '').length;
  const minimum = decimals ? 12 + 7 * (2 + Math.min(decimals,6)) : 32;
  return Math.max(minimum, Math.round(parseFloat(raw) * .85)) + 'px';
}

function stepNumericValue(value, control, direction) {
  const step = Number(control.step) > 0 ? Number(control.step) : 1;
  let next = Number.isFinite(value) ? value : Number(control.value);
  if (!Number.isFinite(next)) next = 0;
  next = Number((next + direction * step).toPrecision(15));
  if (control.min !== undefined && Number.isFinite(Number(control.min))) next = Math.max(Number(control.min), next);
  if (control.max !== undefined && Number.isFinite(Number(control.max))) next = Math.min(Number(control.max), next);
  return next;
}

function bindNumericControl(element, ui, control, listen) {
  const input = queryUIX(element, `#mz_${control.id}`);
  const option = ui[control.option];
  if (!input || !option) return;
  const lang = () => element.dataset.lang || 'en-US';
  const slider = queryUIX(element, `#mz_${control.id}_slider`);
  const sync = value => {
    input.value = formatNumber(value, lang());
    if (value === null || value === undefined) input.removeAttribute('aria-valuenow');
    else input.setAttribute('aria-valuenow', String(value));
    input.removeAttribute('aria-invalid');
    if (slider) slider.value = value;
  };
  const bounded = value => {
    if (control.min !== undefined && Number.isFinite(Number(control.min))) value = Math.max(Number(control.min), value);
    if (control.max !== undefined && Number.isFinite(Number(control.max))) value = Math.min(Number(control.max), value);
    return value;
  };
  const commit = () => {
    const value = parseNumber(input.value, lang());
    if (value === null && input.value.trim() !== '') { sync(option.value()); return; }
    const next = value === null ? null : bounded(value);
    if (!Object.is(next, option.value())) option.setValue(next);
    sync(next);
  };
  listen(input, 'input', () => {
    const value = parseNumber(input.value, lang());
    if ((value === null && input.value.trim() !== '') || (value !== null && value !== bounded(value))) {
      input.setAttribute('aria-invalid', 'true'); return;
    }
    input.removeAttribute('aria-invalid');
    if (!Object.is(value, option.value())) option.setValue(value);
    if (value !== null) input.setAttribute('aria-valuenow', String(value));
    if (slider) slider.value = value;
  });
  listen(input, 'change', commit);
  listen(input, 'blur', commit);
  const step = direction => {
    if (input.disabled || input.readOnly) return;
    let value = parseNumber(input.value, lang());
    if (value === null) value = option.value();
    const next = stepNumericValue(value, control, direction);
    if (!Object.is(next, option.value())) option.setValue(next);
    sync(next);
  };
  const mode = control.inputMode || (control.showSlider === true ? 'slider' : 'standard');
  if (mode === 'spin') {
    for (const [suffix, direction] of [['spinUp',1],['spinDown',-1]]) {
      const button = queryUIX(element, `#mz_${control.id}_${suffix}`);
      if (button) {
        const label = direction > 0 ? _('Increase') : _('Decrease');
        button.setAttribute('aria-label', ui.tabbedControl && typeof ui.tabbedControl.translate === 'function'
          ? ui.tabbedControl.translate(label) : label);
        listen(button, 'mousedown', event => event.preventDefault());
        listen(button, 'click', () => step(direction));
      }
    }
    listen(input, 'keydown', event => {
      if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
        event.preventDefault(); step(event.key === 'ArrowUp' ? 1 : -1);
      }
    });
  }
  if (slider) for (const event of ['input','change']) listen(slider, event, () => {
    if (input.disabled || input.readOnly) return;
    const value = Number(slider.value);
    if (!Number.isFinite(value)) return;
    const next = bounded(value);
    if (!Object.is(next, option.value())) option.setValue(next);
    sync(next);
  });
}

function formatNumber(value, lang) {
  if (value === null || value === undefined || value === "" || isNaN(value)) return "";
  const sep = getDecimalSeparator(lang);
  return value.toString().replace('.', sep);
}

function parseNumber(str, lang) {
  if (str === "" || str === undefined || str === null) return null;
  // Gli input accettano entrambi i separatori decimali, ma non le migliaia.
  // Valida l'intero testo: parseFloat troncherebbe "0,5" o "1abc" senza errore.
  const normalized = str.toString().trim().replace(',', '.');
  if (!/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$/.test(normalized)) return null;
  const val = Number(normalized);
  return Number.isFinite(val) ? val : null;
}

// =====================================================
// GENERATORE STRUTTURA HTML PRINCIPALE
// =====================================================
// Crea l'HTML completo per il sistema a schede

function generateTabbedStructure(tabConfig, lang) {
  // Genera gli header delle schede (tab navigation)
  const tabNavigation = tabConfig.map(
    (tab, index) =>
      `<div class="tab-header ${index === 0 ? "active" : ""}" data-tab="${tab.id}">
            ${tab.label}
        </div>`,
  ).join("");

  // Genera il contenuto di ogni scheda
  const tabContents = tabConfig.map(
    (tab, index) =>
      `<div id="${tab.id}" class="tab-content ${index === 0 ? "active" : ""}">
            ${generateTabTitleSection(tab)}
            <div class="tab-body">
                ${generateControlsHTML(tab)}
            </div>
        </div>`,
  ).join("");

  // Combina tutto nella struttura finale
  return `
        <div class="tabbed-container" lang="${lang}">
            <div class="tab-headers">
                ${tabNavigation}
            </div>
            <div class="tab-content-area">
                ${tabContents}
                ${generateInfoNotesSection(tabConfig)}
            </div>
        </div>
    `;
}

// ================================================================
// GENERATORE SEZIONE TITOLO SCHEDA
// ================================================================
// Crea titolo e sottotitolo opzionali per ogni scheda

function generateTabTitleSection(tab) {
  // Se non deve mostrare il titolo, ritorna stringa vuota
  if (!tab.showTabTitle || !tab.tabTitle) {
    return "";
  }

  // Determina la classe CSS per l'allineamento
  const alignClass = `align-${tab.tabTitleAlign || "left"}`;

  return `
        <div class="tab-title-section ${alignClass}">
            <h2 class="tab-main-title">${tab.tabTitle}</h2>
            ${tab.tabSubtitle ? `<p class="tab-main-subtitle">${tab.tabSubtitle}</p>` : ""}
        </div>
    `;
}

// ================================================================
// DISPATCHER PER GENERAZIONE HTML CONTROLLI
// ================================================================
// Sceglie il layout appropriato in base alla configurazione

function generateControlsHTML(tab) {
  if (tab.layout === "grid") {
    return generateGridLayout(tab);    // Layout a griglia complesso
  } else {
    return generateVerticalLayout(tab.controls); // Layout verticale semplice
  }
}

// ================================================================
// GENERATORE LAYOUT GRIGLIA - CON SFONDI CONFIGURABILI E ACCORDION
// ================================================================
// Crea una griglia CSS con posizionamento preciso dei controlli
// Supporta gruppi collassabili (accordion) e colSpan

function generateGridLayout(tab) {
  const { rows, cols, gap, itemGap, rowBackgrounds, colBackgrounds, cellBackgrounds } = tab.grid;
  const gridCols = parseColConfiguration(cols);
  const colFractions = typeof cols === "string"
    ? cols.split(':').map(ratio => ratio.trim() + 'fr')
    : (typeof cols === "number" ? Array(cols).fill('1fr') : ['1fr']);

  let gridHTML = `<div class="tab-body-grid tab-body-grid-forced" style="display: grid !important; grid-template-columns: ${gridCols} !important; grid-template-rows: repeat(${rows}, auto) !important; gap: ${gap} !important; align-items: stretch !important;">`;

  const numCols = typeof cols === "string" ? cols.split(":").length : cols;

  // Mappa per tracciare le celle già occupate da span
  const occupiedCells = new Set();

  for (let row = 0; row < rows; row++) {
    for (let col = 0; col < numCols; col++) {
      const cellKey = `${row}-${col}`;

      // Salta se la cella è già occupata
      if (occupiedCells.has(cellKey)) {
        continue;
      }

      const groupConfig = tab.groups
        ? tab.groups.find(
          (group) =>
            group.position &&
            group.position.row === row &&
            group.position.col === col,
        )
        : null;

      // NUOVA LOGICA: Cerca se c'è un plotBlock in questa posizione che definisce uno span
      const plotBlockConfig = tab.controls
        ? tab.controls.find(
          (c) =>
            c.type === "plotBlock" &&
            c.position &&
            c.position.row === row &&
            c.position.col === col
        )
        : null;

      // Salta celle vuote: nessun gruppo configurato E nessun controllo in questa posizione
      // Evita che le righe create dal rowSpan dei gruppi vicini generino spazio vuoto nel DOM
      if (!groupConfig) {
        const hasControls = tab.controls && tab.controls.some(
          c => c.position && c.position.row === row && c.position.col === col
        );
        if (!hasControls) {
          continue; // Cella vuota — non renderizzare il div
        }
      }

      const rawAlign = (groupConfig && groupConfig.align) || (plotBlockConfig && plotBlockConfig.align) || "top-left";
      const normalizedAlign = normalizeAlignment(rawAlign);
      const [vAlign, hAlign] = normalizedAlign.split('-');

      // --- LOGICA RISOLUZIONE COLORE SFONDO (Universal e Gerarchica) ---

      const resolveBackgroundColor = (r, c) => {
        const key = `${r}-${c}`;
        // Priorità: cellBackgrounds del gruppo > cellBackgrounds del tab > backgroundColor del gruppo > rowBackgrounds > colBackgrounds
        if (groupConfig && groupConfig.cellBackgrounds && groupConfig.cellBackgrounds[key]) {
          return groupConfig.cellBackgrounds[key];
        }
        if (cellBackgrounds && cellBackgrounds[key]) {
          return cellBackgrounds[key];
        }
        if (groupConfig && groupConfig.backgroundColor) {
          return groupConfig.backgroundColor;
        }
        if (rowBackgrounds && rowBackgrounds[r]) {
          return rowBackgrounds[r];
        }
        if (colBackgrounds && colBackgrounds[c]) {
          return colBackgrounds[c];
        }
        return "transparent";
      };

      const cellBackgroundColor = resolveBackgroundColor(row, col);

      // Se non c'è un gruppo ma c'è un plotBlock con colore, usalo come backup
      const finalBG = cellBackgroundColor !== "transparent"
        ? cellBackgroundColor
        : (plotBlockConfig && plotBlockConfig.backgroundColor ? plotBlockConfig.backgroundColor : "transparent");

      const alignClass = `v-align-${vAlign} h-align-${hAlign}`;
      const backgroundStyle =
        finalBG !== "transparent"
          ? `background: ${finalBG} !important;`
          : "";
      const gridRowStart = row + 1;
      const gridColStart = col + 1;

      // Trova i controlli presenti in questa cella
      const cellControls = tab.controls ? tab.controls.filter(
        (c) => c.position && c.position.row === row && c.position.col === col
      ) : [];

      // NUOVA GESTIONE: supporto per colSpan E rowSpan (da Gruppo, da PlotBlock o dai controlli della cella)
      let colSpan = 1;
      let rowSpan = 1;

      if (groupConfig && groupConfig.colSpan) {
        colSpan = groupConfig.colSpan;
      } else if (plotBlockConfig && plotBlockConfig.colSpan) {
        colSpan = plotBlockConfig.colSpan;
      } else if (cellControls.length > 0 && cellControls[0].position && cellControls[0].position.colSpan) {
        colSpan = cellControls[0].position.colSpan;
      }

      if (groupConfig && groupConfig.rowSpan) {
        rowSpan = groupConfig.rowSpan;
      } else if (plotBlockConfig && plotBlockConfig.rowSpan) {
        rowSpan = plotBlockConfig.rowSpan;
      } else if (cellControls.length > 0 && cellControls[0].position && cellControls[0].position.rowSpan) {
        rowSpan = cellControls[0].position.rowSpan;
      }

      // Calcola span CSS per colonne e righe
      const gridColSpanCSS = colSpan > 1 ? `${gridColStart} / span ${colSpan}` : gridColStart;
      const gridRowSpanCSS = rowSpan > 1 ? `${gridRowStart} / span ${rowSpan}` : gridRowStart;

      // MARCA TUTTE LE CELLE OCCUPATE dal colSpan E rowSpan
      for (let spanRow = row; spanRow < row + rowSpan && spanRow < rows; spanRow++) {
        for (let spanCol = col; spanCol < col + colSpan && spanCol < numCols; spanCol++) {
          occupiedCells.add(`${spanRow}-${spanCol}`);
        }
      }

      // Stile posizionamento griglia con ENTRAMBI gli span
      const gridPositionStyle = `grid-row: ${gridRowSpanCSS}; grid-column: ${gridColSpanCSS};`;

      let cellLayoutStyles = `${backgroundStyle} ${gridPositionStyle}`;
      const cellGap = `gap: ${itemGap || "6px"};`;

      const isSpanned = colSpan > 1 || rowSpan > 1;
      let subGridStyles = '';
      let controlsByCol = {};

      if (!isSpanned) {
        // Cella singola normale
        cellLayoutStyles = `display: flex; flex-direction: column; ${cellGap} ${cellLayoutStyles}`;
        const controlsInCol = tab.controls.filter(
          (control) =>
            control.position &&
            control.position.row === row &&
            control.position.col === col &&
            !control.isGroupBadge
        );
        controlsByCol[col] = controlsInCol;
      } else {
        // Cella con span (colonna e/o riga)
        cellLayoutStyles = `${backgroundStyle} ${gridPositionStyle}`;
        if (groupConfig && Array.isArray(groupConfig.stackedColumns))
          cellLayoutStyles += cellGap;

        // Gestione subgrid: specifica sia le colonne che le righe (Shorthand per massima stabilità)
        const spannedFractions = colFractions.slice(col, col + colSpan);
        // Opt-in del singolo gruppo: righe aderenti ai controlli, senza distribuire
        // lo spazio verticale imposto dal gruppo adiacente sulle righe interne.
        const rowSize = groupConfig && groupConfig.contentRows === true ? 'max-content' : '1fr';
        subGridStyles = `display: grid !important; grid-template: repeat(${rowSpan}, ${rowSize}) / ${spannedFractions.join(' ')} !important;`;
        if (groupConfig && groupConfig.contentRows === true) {
          cellLayoutStyles += cellGap;
          subGridStyles += 'row-gap: 0 !important;';
        }

        // Raccogli controlli da TUTTE le celle spanned (sia colonne che righe)
        for (let spanRow = row; spanRow < row + rowSpan && spanRow < rows; spanRow++) {
          for (let spanCol = col; spanCol < col + colSpan && spanCol < numCols; spanCol++) {
            const colControls = tab.controls.filter(
              (control) =>
                control.position &&
                control.position.row === spanRow &&
                control.position.col === spanCol &&
                !control.isGroupBadge
            );

            // Usa una chiave composita per gestire righe multiple
            const colKey = `${spanRow}-${spanCol}`;
            if (!controlsByCol[colKey]) {
              controlsByCol[colKey] = [];
            }
            controlsByCol[colKey] = colControls;
          }
        }
      }

      // Genera il div della cella
      const contentRowsClass = groupConfig && groupConfig.contentRows === true ? ' mz-content-rows' : '';
      gridHTML += `<div class="grid-cell ${alignClass}${contentRowsClass}" data-row="${row}" data-col="${col}" data-rowspan="${rowSpan}" data-colspan="${colSpan}" style="${cellLayoutStyles}">`;

      // Gestione identificazione badge applicati al titolo del gruppo (se presenti in questa cella originaria)
      const titleBadges = tab.controls ? tab.controls.filter(
        (c) => c.isGroupBadge && c.position && c.position.row === row && c.position.col === col
      ) : [];
      let badgesHTML = '';
      if (titleBadges.length > 0) {
        const badgeAlignClass = titleBadges[0].align ? `align-${titleBadges[0].align}` : 'align-right';
        badgesHTML = `<div class="group-title-badges ${badgeAlignClass}">${generateVerticalLayout(titleBadges)}</div>`;
      }

      // Gestione accordion 
      if (groupConfig && groupConfig.collapsible) {
        const accordionKey = `accordion-${tab.id}-${row}-${col}`;
        const expandedClass = groupConfig.defaultExpanded ? 'expanded' : '';
        const iconExpandedClass = groupConfig.defaultExpanded ? 'expanded' : '';

        gridHTML += `
          <div class="accordion-header-wrapper">
            <div class="group-title-collapsible ${expandedClass}" data-accordion="${accordionKey}">
              <svg class="accordion-icon ${iconExpandedClass}" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="2.5">
                <polyline points="6,4 10,8 6,12"></polyline>
              </svg>
              <span>${groupConfig.title}</span>
            </div>
            ${badgesHTML}
          </div>
        `;
        gridHTML += `<div class="accordion-content ${expandedClass}" data-accordion-content="${accordionKey}">`;
        gridHTML += `<div class="accordion-content-inner">`;
      } else if (groupConfig && groupConfig.title) {
        gridHTML += `
          <div class="accordion-header-wrapper">
            <div class="group-title">${groupConfig.title}</div>
            ${badgesHTML}
          </div>
        `;
      }

      const hasTitle = !!(groupConfig && groupConfig.title);
      const controlWrapperClass = isSpanned ? 'grid-span-controls' : 'grid-single-controls';
      const indentationClass = hasTitle ? '' : 'no-indent';

      const isTabbedGroup = !!(groupConfig && groupConfig.type === "tabbedGroup");
      const controlWrapperStyle = (isSpanned && !isTabbedGroup) ? subGridStyles : 'display: flex; flex-direction: column; width: 100%; height: 100%;';
      gridHTML += `<div class="${controlWrapperClass} ${indentationClass}" style="${controlWrapperStyle}">`;
      if (groupConfig && groupConfig.type === "tabbedGroup" && groupConfig.subTabs) {
        // --- LOGICA TABBED GROUP (Nested Tabs) ---
        const tabGroupId = `tabgroup-${tab.id}-${row}-${col}`;

        // Risolve il colore accent del gruppo (supporta preset nominali e hex custom)
        const SUBTAB_COLORS = {
          blue: '#3e6da9',  // Blu Jamovi primario
          lightblue: '#3498db',  // Blu Jamovi alternativo
          orange: '#eca038',  // Arancio Jamovi (icona Exploration)
        };
        const rawColor = groupConfig.color;
        const accentColor = rawColor
          ? (SUBTAB_COLORS[rawColor] || rawColor)   // preset → valore, altrimenti usa direttamente
          : null;                                    // null = usa il default CSS
        const accentStyle = accentColor ? ` style="--mz-subtab-accent: ${accentColor};"` : '';

        // 1. Genera la barra di navigazione per i sotto-tab (Stile Material/Underline)
        gridHTML += `<div class="mz-subtab-headers" data-tabgroup="${tabGroupId}"${accentStyle}>`;
        groupConfig.subTabs.forEach((subTab, idx) => {
          const activeClass = idx === 0 ? "active" : "";
          gridHTML += `<button class="mz-subtab-header ${activeClass}" data-subtab="${subTab.id}">${subTab.label}</button>`;
        });
        gridHTML += `</div>`; // Chiude .mz-subtab-headers

        // 2. Genera i pannelli di contenuto per ogni sub-tab
        groupConfig.subTabs.forEach((subTab, idx) => {
          const activeClass = idx === 0 ? "active" : "";
          const subTabLayout = subTab.layout === "horizontal" ? "row" : "column";
          const subTabGap = subTab.itemGap ? `gap: ${subTab.itemGap};` : "gap: 8px;";

          gridHTML += `<div id="${subTab.id}" class="mz-subtab-content ${activeClass}" data-parent-group="${tabGroupId}" style="flex-direction: ${subTabLayout}; ${subTabGap}">`;
          gridHTML += generateVerticalLayout(subTab.controls || []);
          gridHTML += `</div>`; // Chiude .mz-subtab-content
        });
      } else if (!isSpanned) {
        // Cella singola standard
        let allCellControls = controlsByCol[col] || [];
        gridHTML += generateVerticalLayout(allCellControls);
      } else {
        // Cella con span standard - genera wrapper per ogni controllo rispettando la sua posizione
        for (let spanRow = row; spanRow < row + rowSpan && spanRow < rows; spanRow++) {
          for (let spanCol = col; spanCol < col + colSpan && spanCol < numCols; spanCol++) {
            const colKey = `${spanRow}-${spanCol}`;
            const currentControls = controlsByCol[colKey] || [];

              const subGridCol = spanCol - col + 1;
              const subGridRow = spanRow - row + 1;

              // Raggruppiamo i controlli che condividono gli stessi attributi di estensione nella subgrid
              // per non generare un livello di griglia sovrapposto per ogni singolo oggetto.
              const groupedBySpan = {};
              currentControls.forEach(control => {
                const ctrlColSpan = control.colSpan || control.columnSpan || (control.position && (control.position.colSpan || control.position.columnSpan)) || 1;
                const ctrlRowSpan = control.rowSpan || (control.position && control.position.rowSpan) || 1;
                const spanKey = `${ctrlColSpan}-${ctrlRowSpan}`;
                if (!groupedBySpan[spanKey]) groupedBySpan[spanKey] = [];
                groupedBySpan[spanKey].push(control);
              });

              // Per ogni gruppo omogeneo genero l'html unificato
              for (const [spanKey, controlsGroup] of Object.entries(groupedBySpan)) {
                const [ctrlColSpan, ctrlRowSpan] = spanKey.split('-');
                const gridColSpanCSS = ctrlColSpan > 1 ? `${subGridCol} / span ${ctrlColSpan}` : subGridCol;
                const gridRowSpanCSS = ctrlRowSpan > 1 ? `${subGridRow} / span ${ctrlRowSpan}` : subGridRow;

                // Supporto Background per sotto-cella logica
                const subCellBG = resolveBackgroundColor(spanRow, spanCol);
                const subCellBGStyle = subCellBG !== "transparent" ? `background: ${subCellBG} !important;` : "";

                // Il wrapper riceve pointer-events: none per assicurare che non faccia mai da "scudo" contro i click
                // (i singoli controlli all'interno lo riattivano autonomamente tramite .mz-control-wrapper label ecc.)
                // Una colonna in flusso segue le altezze reali dei controlli, come una cella singola.
                const stacked = groupConfig && Array.isArray(groupConfig.stackedColumns) &&
                  groupConfig.stackedColumns.includes(spanCol);
                // Riga in flusso esplicita del singolo gruppo; nessun effetto sui gruppi preesistenti.
                const inline = groupConfig && Array.isArray(groupConfig.inlineRows) &&
                  groupConfig.inlineRows.includes(spanRow);
                const innerLayout = inline
                  ? 'display: flex !important; flex-direction: row; align-items: center; flex-wrap: wrap; gap: 12px;'
                  : stacked ? 'display: flex !important; flex-direction: column;'
                  : 'display: grid !important; grid-template: 1fr / 1fr !important;';
                gridHTML += `<div class="span-col-wrapper${inline ? ' mz-inline-row' : ''}" style="grid-column: ${gridColSpanCSS}; grid-row: ${gridRowSpanCSS}; ${innerLayout} pointer-events: none !important; ${subCellBGStyle}">`;
                gridHTML += generateVerticalLayout(controlsGroup);
                gridHTML += "</div>";
              }
          }
        }
      }

      gridHTML += `</div>`; // Chiude .grid-span-controls o .grid-single-controls

      if (groupConfig && groupConfig.collapsible) {
        gridHTML += `</div>`; // Chiude .accordion-content-inner
        gridHTML += `</div>`; // Chiude .accordion-content
      }

      gridHTML += `</div>`; // Chiude .grid-cell
    }
  }

  gridHTML += `</div>`; // Chiude .tab-body-grid
  return gridHTML;
}

// ================================================================
// PARSER CONFIGURAZIONE COLONNE
// ================================================================
// Converte diverse notazioni di colonne in CSS Grid syntax

function parseColConfiguration(cols) {
  if (typeof cols === "string") {
    // Converte "1:2:1" in "1fr 2fr 1fr"
    return cols
      .split(":")
      .map((ratio) => ratio.trim() + "fr")
      .join(" ");
  } else if (typeof cols === "number") {
    // Converte 3 in "repeat(3, 1fr)"
    return `repeat(${cols}, 1fr)`;
  } else {
    // Default: una colonna
    return "1fr";
  }
}

// ================================================================
// GENERATORE LAYOUT VERTICALE - CONTROLLI
// ================================================================
// Genera HTML per una lista verticale di controlli
// La funzione più complessa per gestisce tutti i tipi di controlli
// (1) Funzione per aggiornamenti e nuovi elementi
function generateVerticalLayout(controls) {

  return controls
    .map((control) => {

      let controlHTML = "";

      // Switch per gestire ogni tipo di controllo
      switch (control.type) {

        // ===== CONTROLLO CHECKBOX / TOGGLE =====
        case "checkbox":
        case "output": {
          const isOutput = control.type === "output";
          const checkboxSide = control.checkboxSide || "left";
          const displayType = control.displayType || "checkbox";
          const toggleOrientation = control.toggleOrientation || "horizontal";
          const checkboxClass = isOutput ? "control-checkbox output-checkbox" : "control-checkbox";

          if (displayType === "toggle") {
            const toggleClass = toggleOrientation === "vertical" ? "toggle-switch-vertical" : "toggle-switch";
            const sliderClass = toggleOrientation === "vertical" ? "toggle-slider-vertical" : "toggle-slider";
            const labelClass = toggleOrientation === "vertical" ? "toggle-label-vertical" : "toggle-label";

            if (checkboxSide === "right") {
              controlHTML = `
                <div class="control-item control-item-compact">
                  <label class="${labelClass} toggle-right">
                    <span class="toggle-text">${control.label}</span>
                    <div class="${toggleClass}"${control.color ? ` style="--mz-toggle-color: ${control.color};"` : ""}>
                      <input type="checkbox" id="mz_${control.id}" class="${checkboxClass}" ${control.checked ? "checked" : ""}>
                      <span class="${sliderClass}"></span>
                    </div>
                  </label>
                  ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
                </div>
              `;
            } else if (checkboxSide === "top") {
              controlHTML = `
                <div class="control-item control-item-compact">
                  <label class="${labelClass} toggle-top">
                    <span class="toggle-text">${control.label}</span>
                    <div class="${toggleClass}"${control.color ? ` style="--mz-toggle-color: ${control.color};"` : ""}>
                      <input type="checkbox" id="mz_${control.id}" class="${checkboxClass}" ${control.checked ? "checked" : ""}>
                      <span class="${sliderClass}"></span>
                    </div>
                  </label>
                  ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
                </div>
              `;
            } else if (checkboxSide === "bottom") {
              controlHTML = `
                <div class="control-item control-item-compact">
                  <label class="${labelClass} toggle-bottom">
                    <div class="${toggleClass}"${control.color ? ` style="--mz-toggle-color: ${control.color};"` : ""}>
                      <input type="checkbox" id="mz_${control.id}" class="${checkboxClass}" ${control.checked ? "checked" : ""}>
                      <span class="${sliderClass}"></span>
                    </div>
                    <span class="toggle-text">${control.label}</span>
                  </label>
                  ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
                </div>
              `;
            } else {
              controlHTML = `
                <div class="control-item control-item-compact">
                  <label class="${labelClass} toggle-left">
                    <div class="${toggleClass}"${control.color ? ` style="--mz-toggle-color: ${control.color};"` : ""}>
                      <input type="checkbox" id="mz_${control.id}" class="${checkboxClass}" ${control.checked ? "checked" : ""}>
                      <span class="${sliderClass}"></span>
                    </div>
                    <span class="toggle-text">${control.label}</span>
                  </label>
                  ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
                </div>
              `;
            }
          } else {
            const customColorStyleCheckbox = control.color ? ` style="--mz-checkbox-color: ${control.color};"` : "";

            if (checkboxSide === "right") {
              controlHTML = `
                <div class="control-item control-item-compact">
                  <label class="checkbox-label checkbox-right">
                    <span class="checkbox-text">${control.label}</span>
                    <input type="checkbox" id="mz_${control.id}" class="${checkboxClass}" ${control.checked ? "checked" : ""}${customColorStyleCheckbox}>
                  </label>
                  ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
                </div>
              `;
            } else {
              controlHTML = `
                <div class="control-item control-item-compact">
                  <label class="checkbox-label checkbox-left">
                    <input type="checkbox" id="mz_${control.id}" class="${checkboxClass}" ${control.checked ? "checked" : ""}${customColorStyleCheckbox}>
                    <span class="checkbox-text">${control.label}</span>
                  </label>
                  ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
                </div>
              `;
            }
          }
          break;
        }

        // ===== CONTROLLO NUMERO =====
        case "number": {
          const labelPos = control.labelPosition || "top";
          const customWidthNum = `width: ${numericInputWidth(control, labelPos === "left")} !important;`;
          const showHint = control.showHint === true;

          // Nuova logica inputMode: "standard" (default), "slider", "spin"
          // Mantiene compatibilità con showSlider: true
          const inputMode = control.inputMode || (control.showSlider === true ? "slider" : "standard");

          const showSlider = inputMode === "slider";
          const showSpin = inputMode === "spin";

          const hintHTML = showHint ? `<span class="control-hint">Range: ${control.min}-${control.max}</span>` : "";
          const sliderHTML = showSlider ? `
            <div class="slider-container">
              <input type="range" id="mz_${control.id}_slider" class="control-slider" min="${control.min}" max="${control.max}" value="${control.value}" step="${control.step || 1}">
            </div>` : "";

          // Classe per nascondere spin: standard o slider
          const noSpinClass = (!showSpin) ? "control-no-spin" : "";
          const sliderEnabledClass = showSlider ? "control-slider-enabled" : "";
          const numericForceClass = "control-input-numeric-force";
          const combinedClasses = `${noSpinClass} ${sliderEnabledClass} ${numericForceClass}`.trim();

          // Campo testuale in tutte le modalita: il browser non sceglie il separatore decimale.
          const inputType = "text";
          const inputModeAttr = 'inputmode="decimal" autocomplete="off"';

          const spinHTML = showSpin ? `<span class="mz-numeric-stepper">
            <button type="button" id="mz_${control.id}_spinUp" tabindex="-1" aria-label="Increase">▲</button>
            <button type="button" id="mz_${control.id}_spinDown" tabindex="-1" aria-label="Decrease">▼</button>
          </span>` : '';
          const numericInput = (classes) => `<span class="mz-numeric-field${showSpin ? ' mz-has-stepper' : ''}">
            <input type="${inputType}" ${inputModeAttr} id="mz_${control.id}" class="${classes} ${combinedClasses}" style="${customWidthNum}" ${showSpin ? 'role="spinbutton"' : ''} ${control.min !== undefined ? `aria-valuemin="${control.min}"` : ''} ${control.max !== undefined ? `aria-valuemax="${control.max}"` : ''} value="${control.value}" step="${control.step || 1}">
            ${spinHTML}</span>`;

          if (labelPos === "left") {
            const gapStyle = control.itemGap ? `gap: ${control.itemGap} !important;` : "";
            controlHTML = `
              <div class="control-item control-inline" style="${gapStyle}">
                <label class="control-label-inline">${control.label}:</label>
                <div class="control-input-group">
                  ${numericInput('control-input-inline')}
                  ${sliderHTML}
                  ${hintHTML}
                </div>
              </div>
            `;
          } else {
            controlHTML = `
              <div class="control-item">
                <label class="control-label">${control.label}:</label>
                ${numericInput('control-input')}
                ${hintHTML}
                ${sliderHTML}
              </div>
            `;
          }
          break;
        }

        // ===== CONTROLLO SELECT =====
        case "select": {
          const labelPosSel = control.labelPosition || "top";
          const customWidthSel = control.width ? `width: ${control.width} !important;` : "";
          const optionsHTML = control.options.map((opt) => `<option value="${opt.value}">${opt.text}</option>`).join("");

          if (labelPosSel === "left") {
            const gapStyle = control.itemGap ? `gap: ${control.itemGap} !important;` : "";
            controlHTML = `
              <div class="control-item control-inline" style="${gapStyle}">
                <label class="control-label-inline">${control.label}:</label>
                <div class="control-input-group">
                  <select id="mz_${control.id}" class="control-select control-select-inline" style="${customWidthSel}">${optionsHTML}</select>
                  ${control.hint ? `<span class="control-hint-inline">${control.hint}</span>` : ""}
                </div>
              </div>
            `;
          } else {
            controlHTML = `
              <div class="control-item">
                <label class="control-label">${control.label}:</label>
                <select id="mz_${control.id}" class="control-select" style="${customWidthSel}">${optionsHTML}</select>
                ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
              </div>
            `;
          }
          break;
        }

        // ===== CONTROLLO STRING =====
        case "string": {
          const labelPosStr = control.labelPosition || "top";
          const customWidthStr = control.width ? `width: ${control.width} !important;` : "";

          if (labelPosStr === "left") {
            const gapStyle = control.itemGap ? `gap: ${control.itemGap} !important;` : "";
            controlHTML = `
              <div class="control-item control-inline" style="${gapStyle}">
                <label class="control-label-inline">${control.label}:</label>
                <div class="control-input-group">
                  <input type="text" id="mz_${control.id}" class="control-input control-input-inline" style="${customWidthStr}" value="${control.value || ""}">
                  ${control.hint ? `<span class="control-hint-inline">${control.hint}</span>` : ""}
                </div>
              </div>
            `;
          } else {
            controlHTML = `
              <div class="control-item">
                <label class="control-label">${control.label}:</label>
                <input type="text" id="mz_${control.id}" class="control-input" style="${customWidthStr}" value="${control.value || ""}">
                ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
              </div>
            `;
          }
          break;
        }

        // ===== CONTROLLO RADIO =====
        case "radio": {
          const radioLayout = control.layout || "vertical";
          const radioSide = control.labelPosition || "right";
          const groupLabelPos = control.groupLabelPosition || "top"; // "top" (default) o "left"

          const radioOptionsHTML = control.options.map((opt) => {
            const checkedAttr = opt.value === control.value ? "checked" : "";
            const sideClass = radioSide === "left" ? "radio-left" : "radio-right";
            
            // Logica cromatica: colore opzione > colore gruppo (altrimenti default css)
            const resolvedColor = opt.color || control.color;
            const optColorStyle = resolvedColor ? ` style="--mz-radio-color: ${resolvedColor};"` : "";
            
            return `
                    <label class="radio-label ${sideClass}">
                        <input type="radio" class="control-radio" name="${control.option}" value="${opt.value}" ${checkedAttr}${optColorStyle}>
                        <span class="radio-text">${opt.text}</span>
                    </label>
                  `;
          }).join("");

          const containerClass = radioLayout === "horizontal" ? "radio-group-horizontal" : "radio-group-vertical";

          if (groupLabelPos === "left" && radioLayout === "horizontal") {
            // Label inline a sinistra, radio button sulla stessa riga
            controlHTML = `
                <div class="control-item">
                  <div class="radio-group-inline-left">
                    <span class="radio-group-label-inline">${control.label}:</span>
                    <div class="${containerClass}">${radioOptionsHTML}</div>
                  </div>
                </div>
              `;
          } else {
            // Comportamento default: label sopra
            controlHTML = `
                <div class="control-item">
                  <div class="radio-group-label">${control.label}:</div>
                  <div class="${containerClass}">${radioOptionsHTML}</div>
                </div>
              `;
          }
          break;
        }

        // ===== CONTROLLO PLOT BLOCK =====
        case "plotBlock": {
          const plotLayout = control.layout || "vertical";
          const plotLabelPos = control.labelPosition || "top";
          const plotShowHint = control.showHint === true;
          const plotShowSlider = control.showSlider === true;
          const allHorizontal = plotLayout === "allHorizontal" || control.allHorizontal === true;

          let backgroundStyle = control.backgroundColor ? `background: ${control.backgroundColor};` : "";
          const elementsBackgroundStyle = control.elementsBackgroundColor ? `background: ${control.elementsBackgroundColor};` : "";

          // Supporto per posizionamento chirurgico: margin/padding come oggetto
          if (control.margin) {
            const m = control.margin;
            backgroundStyle += `margin: ${m.top || 0}px ${m.right || 0}px ${m.bottom || 0}px ${m.left || 0}px !important;`;
          }
          if (control.padding) {
            const p = control.padding;
            backgroundStyle += `padding: ${p.top || 0}px ${p.right || 0}px ${p.bottom || 0}px ${p.left || 0}px !important;`;
          }

          const showOptionId = control.showOption;
          const widthOptionId = control.widthOption;
          const heightOptionId = control.heightOption;

          const widthHint = plotShowHint && !plotShowSlider ? `<span class="control-hint-range">Range: ${control.widthRange[0]}-${control.widthRange[1]}</span>` : "";
          const heightHint = plotShowHint && !plotShowSlider ? `<span class="control-hint-range">Range: ${control.heightRange[0]}-${control.heightRange[1]}</span>` : "";
          const widthSliderHTML = plotShowSlider ? `<div class="slider-container"><input type="range" id="mz_${widthOptionId}_slider" class="control-slider" min="${control.widthRange[0]}" max="${control.widthRange[1]}" value="${control.defaultWidth}" step="25"></div>` : "";
          const heightSliderHTML = plotShowSlider ? `<div class="slider-container"><input type="range" id="mz_${heightOptionId}_slider" class="control-slider" min="${control.heightRange[0]}" max="${control.heightRange[1]}" value="${control.defaultHeight}" step="25"></div>` : "";

          const generateDimControl = (optId, label, hint, slider) => {
            if (plotLabelPos === "left") {
              const gapStyle = control.itemGap ? `gap: ${control.itemGap} !important;` : "";
              return `
                <div class="plot-dim-item-left" style="${gapStyle}">
                  <label class="control-label-inline">${label}:</label>
                  <input type="number" id="mz_${optId}" class="control-input ${plotShowSlider ? "control-slider-enabled" : ""}" min="0" max="2000" value="0" step="25">
                  <div class="plot-dim-extra-area">
                    ${slider}
                    ${hint}
                  </div>
                </div>`;
            } else {
              const groupHTML = `
                <div class="input-slider-group">
                  <input type="number" id="mz_${optId}" class="control-input ${plotShowSlider ? "control-slider-enabled" : ""}" min="0" max="2000" value="0" step="25">
                  ${slider}
                </div>`;
              return `<div class="plot-dim-item-top"><label class="control-label">${label}:</label>${groupHTML}${hint}</div>`;
            }
          };

          const isToggle = control.displayType === "toggle";
          let plotCheckboxHTML = "";

          if (isToggle) {
            const toggleOrientation = control.toggleOrientation || "horizontal";
            const checkboxSide = control.checkboxSide || "left";
            const toggleClass = toggleOrientation === "vertical" ? "toggle-switch-vertical" : "toggle-switch";
            const sliderClass = toggleOrientation === "vertical" ? "toggle-slider-vertical" : "toggle-slider";
            const labelClass = toggleOrientation === "vertical" ? "toggle-label-vertical" : "toggle-label";

            // Supporto per rendering a destra o sinistra (usato comunemente nel plotBlock)
            if (checkboxSide === "right") {
              plotCheckboxHTML = `
                <div class="control-item control-item-compact">
                  <label class="${labelClass} toggle-right">
                    <span class="toggle-text">${control.label}</span>
                    <div class="${toggleClass}"${control.color ? ` style="--mz-toggle-color: ${control.color};"` : ""}>
                      <input type="checkbox" id="mz_${showOptionId}" class="control-checkbox" ${control.checked ? "checked" : ""}>
                      <span class="${sliderClass}"></span>
                    </div>
                  </label>
                </div>`;
            } else {
              plotCheckboxHTML = `
                <div class="control-item control-item-compact">
                  <label class="${labelClass} toggle-left">
                    <div class="${toggleClass}"${control.color ? ` style="--mz-toggle-color: ${control.color};"` : ""}>
                      <input type="checkbox" id="mz_${showOptionId}" class="control-checkbox" ${control.checked ? "checked" : ""}>
                      <span class="${sliderClass}"></span>
                    </div>
                    <span class="toggle-text">${control.label}</span>
                  </label>
                </div>`;
            }
          } else {
            plotCheckboxHTML = `
              <div class="control-item control-item-compact">
                <label class="checkbox-label checkbox-left">
                  <input type="checkbox" id="mz_${showOptionId}" class="control-checkbox" ${control.checked ? "checked" : ""}>
                  ${control.label}
                </label>
              </div>`;
          }

          const areaClass = allHorizontal ? "plot-elements-area all-horizontal" : "plot-elements-area";
          const effectiveLayout = allHorizontal ? "horizontal" : plotLayout;
          const dimsHTML = allHorizontal
            ? `${generateDimControl(widthOptionId, "W (px)", widthHint, widthSliderHTML)}${generateDimControl(heightOptionId, "H (px)", heightHint, heightSliderHTML)}`
            : `<div class="plot-dimensions-${effectiveLayout}">${generateDimControl(widthOptionId, "W (px)", widthHint, widthSliderHTML)}${generateDimControl(heightOptionId, "H (px)", heightHint, heightSliderHTML)}</div>`;

          let innerHTML = `
            <div class="plot-block ${effectiveLayout}" style="${backgroundStyle}">
              <div class="${areaClass}" style="${elementsBackgroundStyle}">
                ${plotCheckboxHTML}
                ${dimsHTML}
              </div>
            </div>`;

          // Integrazione Allineamento Unificato 3x3
          if (control.alignment) {
            const alignClass = `mz-align-${control.alignment.vertical || "center"}-${control.alignment.horizontal || "center"}`;
            controlHTML = `<div class="mz-control-wrapper ${alignClass}">${innerHTML}</div>`;
          } else {
            controlHTML = innerHTML;
          }
          break;
        }

        // ===== CONTROLLO ACTION BUTTON =====
        case "actionButton": {
          const btnShape = control.shape || "round";
          const btnColor = control.color || "blue";
          const btnIcon = control.icon || "";
          const btnSize = control.size || "medium";
          const btnLabel = control.label || "";
          const btnLabelPos = control.labelPosition || "bottom";
          const btnAlign = control.align || "left";

          const buttonHTML = `
            <div class="mz-btn-wrapper size-${btnSize}">
              <div class="mz-btn-base"></div>
              <button id="mz_${control.id}" class="mz-btn-new type-${btnShape}" style="background: ${btnColor}" data-action-option="${control.actionOption}" ${control.disabled ? "disabled" : ""} title="${btnLabel}">
                ${btnIcon ? `<span>${btnIcon}</span>` : ""}
              </button>
            </div>`;

          if (btnLabel) {
            controlHTML = `
              <div class="action-button-container">
                <div class="action-button-wrapper label-${btnLabelPos}">
                  <span class="action-button-label">${btnLabel}</span>
                  ${buttonHTML}
                </div>
              </div>`;
          } else {
            controlHTML = `<div class="action-button-container">${buttonHTML}</div>`;
          }
          break;
        }

        // ===== CONTROLLO DEVIATORE (interruttore fisico 3 stati) =====
        case "deviatore": {
          const orientation        = control.orientation || "horizontal";
          const isVertical         = orientation === "vertical";
          const devDefault         = control.default || "posOff";
          const colorA             = control.colorA || "#eca038";
          const colorB             = control.colorB || "#3e6da9";
          const thumbColor         = control.thumbColor || "#ffffff";
          const thumbW             = control.thumbWidth || "8px";
          const thumbH             = control.thumbHeight || "20px";
          const shapeColor         = control.shapeColor || "#3e6da9";
          const markerShape        = control.markerShape || "circle";
          const mainLabel          = control.label || "";
          const labelPos           = control.labelPosition || "left";
          const mainLabelPosition  = control.mainLabelPosition || "top";
          const stateLabelPosition = control.stateLabelPosition || "center";
          const labelLorT          = control.labelLorT || "";
          const labelRorB          = control.labelRorB || "";
          const shape              = control.shape || "classic";
          const thumbRadius        = control.thumbShape === "circle" ? "50%" : "2px";
          const markerFill         = control.markerFill || "transparent";
          const beamActiveColor    = control.activeColor || "#2ecc71";
          // beam: stile delle etichette di stato; le chiavi omesse usano il default
          const ls                 = control.labelStyle || {};
          const labelSize          = ls.size || "11px";
          const labelWeight        = ls.bold ? "700" : "400";
          const labelStyleV        = ls.italic ? "italic" : "normal";
          const labelColor         = ls.color || "#555555";

          const styleVars = [
            `--mz-dev-color-a: ${colorA}`,
            `--mz-dev-color-b: ${colorB}`,
            `--mz-dev-thumb-color: ${thumbColor}`,
            `--mz-dev-thumb-w: ${thumbW}`,
            `--mz-dev-thumb-h: ${thumbH}`,
            `--mz-dev-shape-color: ${shapeColor}`,
            `--mz-dev-thumb-radius: ${thumbRadius}`,
            `--mz-dev-marker-fill: ${markerFill}`,
            `--mz-dev-beam-active: ${beamActiveColor}`,
            `--mz-dev-label-size: ${labelSize}`,
            `--mz-dev-label-weight: ${labelWeight}`,
            `--mz-dev-label-style: ${labelStyleV}`,
            `--mz-dev-label-color: ${labelColor}`,
          ].join('; ');
        
          const verticalClass = isVertical ? " vertical" : "";
        
          /*
           * La shape "line" è esclusivamente orizzontale.
           * Se richiesta in verticale, ricade automaticamente
           * sulla shape standard.
           */
          const isBeam = shape === "beam";
          const useLineShape = (shape === "line" || isBeam) && !isVertical;
        
          /*
           * mainLabelPosition:
           *   top    → label principale sopra
           *   bottom → label principale sotto
           */
          const horizontalMainLabelPosition =
            !isVertical &&
            (mainLabelPosition === "bottom" || mainLabelPosition === "top")
              ? mainLabelPosition
              : "top";
        
          /*
           * stateLabelPosition:
           *   top    → state-label sopra
           *   center → esterne a sinistra/destra
           *   bottom → state-label sotto
           */
          const horizontalStateLabelPosition =
            !isVertical &&
            (stateLabelPosition === "top" ||
             stateLabelPosition === "center" ||
             stateLabelPosition === "bottom")
              ? stateLabelPosition
              : "center";
        
          const mainLabelHTML = mainLabel
            ? `<span class="deviatore-main-label">${mainLabel}</span>`
            : "";
        
          /*
           * =====================================================
           * SHAPE LINE
           *
           * Solo orizzontale.
           *
           * Tre marker di riferimento:
           *   sinistra → colorA
           *   centro   → bianco / OFF
           *   destra   → colorB
           *
           * Il selettore è un cerchio più grande, trasparente,
           * con bordo sempre shapeColor.
           * =====================================================
           */
          if (useLineShape) {
            const lineStateLabelsHTML = `
              <div class="deviatore-line-stage${isBeam ? ' deviatore-beam' : ''}"                   data-label-state="${horizontalStateLabelPosition}">
                <div class="deviatore-line-state-labels"
                     data-state="${devDefault}">
                  <span class="deviatore-label-l">${labelLorT}</span>
                  <span class="deviatore-label-r">${labelRorB}</span>
                </div>
        
                <div class="deviatore-row deviatore-line-row"
                     id="mz_${control.id}"
                     data-state="${devDefault}"
                     data-option="${control.option}"
                     data-orientation="horizontal">
        
                  <div class="deviatore-line-track">
                    <span class="deviatore-line-dot dot-a marker-${markerShape}"></span>
                    <span class="deviatore-line-dot dot-off marker-${markerShape}"></span>
                    <span class="deviatore-line-dot dot-b marker-${markerShape}"></span>

                    <div class="deviatore-line-marker marker-${markerShape}"></div>
                  </div>
        
                </div>
              </div>
            `;
        
            const mainLabelBeforeHTML =
              horizontalMainLabelPosition === "top"
                ? mainLabelHTML
                : "";
        
            const mainLabelAfterHTML =
              horizontalMainLabelPosition === "bottom"
                ? mainLabelHTML
                : "";
        
            controlHTML = `
              <div class="control-item control-item-compact">
                <div class="deviatore-wrapper main-label-${labelPos}"
                     id="mz_${control.id}_wrapper"
                     style="${styleVars}">
        
                  ${mainLabelBeforeHTML}
        
                  ${lineStateLabelsHTML}
        
                  ${mainLabelAfterHTML}
        
                </div>
        
                ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
              </div>
            `;
        
            break;
          }
        
          /*
           * =====================================================
           * SHAPE CLASSIC
           * =====================================================
           */
        
          const verticalLabelFirstHTML = isVertical
            ? `<span class="deviatore-label-t">${labelLorT}</span>`
            : "";
        
          const verticalLabelSecondHTML = isVertical
            ? `<span class="deviatore-label-b">${labelRorB}</span>`
            : "";
        
          const horizontalStateLabelsHTML = !isVertical
            ? `
              <div class="deviatore-horizontal-stage"
                   data-label-state="${horizontalStateLabelPosition}">
                <div class="deviatore-state-labels"
                     data-state="${devDefault}">
                  <span class="deviatore-label-l">${labelLorT}</span>
                  <span class="deviatore-label-r">${labelRorB}</span>
                </div>
        
                <div class="deviatore-row"
                     id="mz_${control.id}"
                     data-state="${devDefault}"
                     data-option="${control.option}"
                     data-orientation="${orientation}">
                  <div class="deviatore-track">
                    <div class="deviatore-thumb"></div>
                  </div>
                </div>
              </div>
            `
            : "";
        
          const verticalControlHTML = isVertical
            ? `
              <div class="deviatore-row${verticalClass}"
                   id="mz_${control.id}"
                   data-state="${devDefault}"
                   data-option="${control.option}"
                   data-orientation="${orientation}">
                ${verticalLabelFirstHTML}
        
                <div class="deviatore-track">
                  <div class="deviatore-thumb"></div>
                </div>
        
                ${verticalLabelSecondHTML}
              </div>
            `
            : "";
        
          const mainLabelBeforeHTML =
            !isVertical && horizontalMainLabelPosition === "top"
              ? mainLabelHTML
              : "";
        
          const mainLabelAfterHTML =
            !isVertical && horizontalMainLabelPosition === "bottom"
              ? mainLabelHTML
              : "";
        
          controlHTML = `
            <div class="control-item control-item-compact">
              <div class="deviatore-wrapper main-label-${labelPos}"
                   id="mz_${control.id}_wrapper"
                   style="${styleVars}">
        
                ${mainLabelBeforeHTML}
        
                ${horizontalStateLabelsHTML}
                ${verticalControlHTML}
        
                ${mainLabelAfterHTML}
        
              </div>
        
              ${control.hint ? `<span class="control-hint">${control.hint}</span>` : ""}
            </div>
          `;
        
          break;
        }

        // ===== CONTROLLO VERTICAL LIST SLIDER =====
        case "verticalListSlider": {
          const vlsDefault     = control.default || (control.options && control.options[0] && control.options[0].value) || "";
          const vlsLabelPos    = control.labelPosition || "left";
          const vlsLabel       = control.label        || "";
          const vlsThumb       = control.thumbType    || "pill";
          const vlsThumbColor  = control.thumbColor   || "#3e6da9";
          const vlsThumbW      = control.thumbWidth   || "10px";
          const vlsThumbH      = control.thumbHeight  || "22px";
          const vlsTrackColor  = control.trackColor   || "#dddddd";
          const vlsFillColor   = control.fillColor    || "#3e6da9";
          const vlsActiveColor = control.activeColor  || "#3e6da9";
          const vlsNotchW      = control.notchWidth   || "16px";
          const vlsItemGap     = parseInt(control.itemGap, 10) || 22;
          const vlsOptions     = control.options      || [];
          const n              = vlsOptions.length;
        
          // altezza totale colonna = una riga per ogni item
          const totalH = n * vlsItemGap;
        
          // centro di ogni riga in px (usato per thumb, notch, fill)
          // il track va dal centro prima riga al centro ultima riga
          // la track-col ha padding-top = itemGap/2 e padding-bottom = itemGap/2
          // così il track occupa da itemGap/2 a totalH - itemGap/2
          const trackTop    = vlsItemGap / 2;
          const trackBottom = vlsItemGap / 2;
          const trackH      = totalH - vlsItemGap; // = (n-1) * itemGap
        
          const defaultIdx  = vlsOptions.findIndex(o => o.value === vlsDefault);
          const clampedIdx  = defaultIdx >= 0 ? defaultIdx : 0;
        
          // posizione thumb = centro della riga i = i * itemGap + itemGap/2
          // relativa alla track-col che ha altezza totalH
          function rowCenterPx(i) { return i * vlsItemGap + vlsItemGap / 2; }
        
          const thumbTopPx = rowCenterPx(clampedIdx);
          const fillH      = n > 1 ? clampedIdx * vlsItemGap : 0;
        
          // thumb HTML
          function vlsThumbHTML(type, color, w, h) {
            switch (type) {
              case "pill":
                return `<div style="width:${w};height:${h};background:${color};border-radius:calc(${w} * 2);box-shadow:0 1px 3px rgba(0,0,0,0.2)"></div>`;
              case "line":
                return `<div style="width:${w};height:${h};background:${color};border-radius:2px;box-shadow:0 1px 2px rgba(0,0,0,0.18)"></div>`;
              case "arrow":
                return `<svg width="14" height="14" viewBox="0 0 14 14" style="display:block"><polygon points="12,7 2,1 2,13" fill="${color}" filter="drop-shadow(0 1px 2px rgba(0,0,0,0.2))"/></svg>`;
              case "disc":
                return `<div style="width:${w};height:${w};background:${color};border-radius:50%;box-shadow:0 1px 3px rgba(0,0,0,0.22)"></div>`;
              case "ring":
                return `<div style="width:${w};height:${w};border:2.5px solid ${color};border-radius:50%;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,0.12)"></div>`;
              case "bullseye":
                return `<div style="width:${w};height:${w};border:2px solid ${color};border-radius:50%;background:#fff;display:flex;align-items:center;justify-content:center;box-shadow:0 1px 2px rgba(0,0,0,0.12)"><div style="width:calc(${w} * 0.38);height:calc(${w} * 0.38);background:${color};border-radius:50%"></div></div>`;
              case "diamond":
                return `<div style="width:${w};height:${w};background:${color};transform:rotate(45deg);border-radius:2px;box-shadow:0 1px 3px rgba(0,0,0,0.2)"></div>`;
              case "teardrop":
                return `<svg width="16" height="18" viewBox="0 0 20 22" style="display:block"><path d="M10,1 C6,1 2,5 2,10 C2,15 6,20 10,21 C14,20 18,15 18,10 C18,5 14,1 10,1 Z" fill="${color}"/><polygon points="18,10 22,10 18,14" fill="${color}"/></svg>`;
              case "caret":
                return `<svg width="14" height="14" viewBox="0 0 18 16" style="display:block"><polyline points="2,3 8,8 2,13" fill="none" stroke="${color}" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/><polyline points="9,3 15,8 9,13" fill="none" stroke="${color}" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/></svg>`;
              case "hex":
                return `<svg width="14" height="14" viewBox="0 0 20 20" style="display:block"><polygon points="10,2 18,6 18,14 10,18 2,14 2,6" fill="${color}" filter="drop-shadow(0 1px 2px rgba(0,0,0,0.18))"/></svg>`;
              default:
                return `<div style="width:${w};height:${h};background:${color};border-radius:calc(${w} * 2)"></div>`;
            }
          }
        
          // notch ticks — uno per ogni riga, centrato sulla riga
          const notchesHTML = vlsOptions.map((opt, i) => {
            const topPx = rowCenterPx(i);
            return `<div class="vls-notch-tick"
                         style="width:${vlsNotchW};top:${topPx}px;transform:translate(-50%,-50%);left:50%;position:absolute;height:1px;background:rgba(0,0,0,0.18);z-index:1"
                         data-idx="${i}"></div>`;
          }).join("");
        
          // fill: da centro prima riga a centro riga attiva
          // usa un div assoluto che parte da trackTop e ha altezza = clampedIdx * itemGap
          const fillTopPx = trackTop;
        
          // label items
          const labelsHTML = vlsOptions.map((opt, i) => {
            const isActive = opt.value === vlsDefault;
            return `<div class="vls-label-item${isActive ? ' active' : ''}"
                         style="height:${vlsItemGap}px;display:flex;align-items:center;${isActive ? `color:${vlsActiveColor};` : ''}"
                         data-value="${opt.value}"
                         data-idx="${i}"
                         data-option="${control.option}">
                       ${opt.text}
                     </div>`;
          }).join("");
        
          const defaultOpt = vlsOptions[clampedIdx] || { text: "" };
        
          controlHTML = `
            <div class="control-item control-item-compact">
              <div class="vls-wrapper main-label-${vlsLabelPos}" id="mz_${control.id}_wrapper">
                <div class="vls-header" id="mz_${control.id}_header"
                     data-option="${control.option}"
                     data-expanded="false">
                  ${vlsLabel ? `<div class="vls-header-text">${vlsLabel}:</div>` : ""}
                  <div class="vls-header-value" id="mz_${control.id}_hval">${defaultOpt.text}</div>
                  <svg class="vls-header-arrow" id="mz_${control.id}_harrow"
                       viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="2.5">
                    <polyline points="4,6 8,10 12,6"></polyline>
                  </svg>
                </div>
                <div class="vls-body" id="mz_${control.id}_body">
                  <div class="vls-row" id="mz_${control.id}"
                       style="display:flex;flex-direction:row;align-items:flex-start"
                       data-option="${control.option}"
                       data-value="${vlsDefault}"
                       data-thumb-type="${vlsThumb}"
                       data-thumb-color="${vlsThumbColor}"
                       data-thumb-w="${vlsThumbW}"
                       data-thumb-h="${vlsThumbH}"
                       data-active-color="${vlsActiveColor}"
                       data-fill-color="${vlsFillColor}"
                       data-item-gap="${vlsItemGap}"
                       data-notch-w="${vlsNotchW}"
                       data-n="${n}">
                    <div style="position:relative;width:20px;flex-shrink:0;height:${totalH}px;overflow:visible">
                      <div style="position:absolute;left:50%;transform:translateX(-50%);width:3px;
                                  top:${trackTop}px;height:${trackH}px;
                                  background:${vlsTrackColor};border-radius:2px"></div>
                      <div id="mz_${control.id}_fill"
                           style="position:absolute;left:50%;transform:translateX(-50%);width:3px;
                                  top:${fillTopPx}px;height:${fillH}px;
                                  background:${vlsFillColor};border-radius:2px;transition:height 0.2s ease"></div>
                      ${notchesHTML}
                      <div id="mz_${control.id}_thumb"
                           style="position:absolute;left:50%;
                                  transform:translateX(-50%) translateY(-50%);
                                  top:${thumbTopPx}px;
                                  transition:top 0.2s cubic-bezier(0.4,0,0.2,1);
                                  pointer-events:none;z-index:2;overflow:visible">
                        ${vlsThumbHTML(vlsThumb, vlsThumbColor, vlsThumbW, vlsThumbH)}
                      </div>
                    </div>
                    <div style="display:flex;flex-direction:column;padding-left:10px;height:${totalH}px">
                      ${labelsHTML}
                    </div>
                  </div>
                </div>
              </div>
            </div>`;
          break;
        }

        // ===== CONTROLLO SEGMENTED CONTROL =====
        case "segmentedControl": {
          const scLabel       = control.label           || "";
          const scLabelPos    = control.labelPosition   || "left";
          const scOption      = control.option;
          const scOptions     = control.options         || [];
          const scDefault     = control.default         || (scOptions[0] && scOptions[0].value) || "";
          const scColor       = control.activeColor     || "#3e6da9";
          const scBg          = control.backgroundColor || "transparent";
          const scOrientation = control.orientation     || "horizontal";
          const scVariantClass = ['compact', 'pill', 'tab', 'bracket'].includes(control.variant)
            ? ` mz-seg-${control.variant}` : '';
        
          // converte hex #rrggbb in "r, g, b" per rgba() in CSS
          function hexToRgb(hex) {
            const h = hex.replace('#', '');
            if (h.length !== 6) return '62, 109, 169';
            const r = parseInt(h.substring(0, 2), 16);
            const g = parseInt(h.substring(2, 4), 16);
            const b = parseInt(h.substring(4, 6), 16);
            return `${r}, ${g}, ${b}`;
          }
        
          const scColorRgb = hexToRgb(scColor);
        
          // classe guida per verticale: dipende da labelPosition
          const guideClass = scOrientation === 'vertical'
            ? ` vertical guide-${scLabelPos}`
            : '';
        
          const btnsHTML = scOptions.map(opt => {
            const isActive = opt.value === scDefault;
            const optColor = opt.color || scColor;
            // Riserva il corpo maggiore senza spostare le parentesi al cambio di scelta.
            const optionText = control.variant === 'bracket'
              ? `<span class="mz-seg-text">${opt.text}</span><span class="mz-seg-text-size" aria-hidden="true">${opt.text}</span>`
              : opt.text;
            return `<button type="button" role="radio" aria-checked="${isActive}" tabindex="${isActive ? 0 : -1}" class="mz-seg-btn${isActive ? ' active' : ''}"
                         data-value="${opt.value}"
                         data-option="${scOption}"
                         data-opt-color="${opt.color || ''}"
                         style="${isActive
                           ? `color:${optColor}`
                           : (opt.color ? `color:${opt.color}` : '')}">
                       ${optionText}
                     </button>`;
          }).join("");
        
          controlHTML = `
            <div class="control-item control-item-compact">
              <div class="mz-seg-wrapper main-label-${scLabelPos}${scVariantClass}" id="mz_${control.id}_wrapper">
                ${scLabel ? `<div class="mz-seg-main-label">${scLabel}</div>` : ""}
                <div class="mz-seg-track${guideClass}"
                     role="radiogroup" aria-orientation="${scOrientation}" aria-label="${scLabel || scOption}"
                     id="mz_${control.id}"
                     data-option="${scOption}"
                     data-active-color="${scColor}"
                     data-label-pos="${scLabelPos}"
                     data-orientation="${scOrientation}"
                     style="--mz-seg-active-color:${scColor};--mz-seg-active-rgb:${scColorRgb};--mz-seg-bg:${scBg}">
                  ${btnsHTML}
                </div>
              </div>
            </div>`;
          break;
        }
        
        // ===== CONTROLLO PRIORITY PICKER =====
        case "priorityPicker": {
          const ppLabel      = control.label         || "";
          const ppLabelPos   = control.labelPosition  || "left";
          const ppOption     = control.option;
          const ppOptions    = control.options        || [];
          const ppDefault    = control.default        || (ppOptions[0] && ppOptions[0].value) || "";
          const ppColorFill  = control.fillColor      || "#3e6da9";
          const ppColorEmpty = control.emptyColor     || "#dddddd";
          const ppSegWidth   = control.segWidth       || "18px";
          const ppMinHeight  = control.minHeight      || 6;
          const ppMaxHeight  = control.maxHeight      || 24;
          const n            = ppOptions.length;
        
          // altezza progressiva per ogni segmento
          function ppSegH(i) {
            if (n <= 1) return ppMaxHeight;
            return Math.round(ppMinHeight + (ppMaxHeight - ppMinHeight) * (i / (n - 1)));
          }
        
          const defaultIdx = ppOptions.findIndex(o => o.value === ppDefault);
          const clampedIdx = defaultIdx >= 0 ? defaultIdx : 0;
          const defaultText = ppOptions[clampedIdx] ? ppOptions[clampedIdx].text : "";
        
          const segsHTML = ppOptions.map((opt, i) => {
            const active = i <= clampedIdx;
            return `<div class="mz-pp-seg"
                         style="width:${ppSegWidth};height:${ppSegH(i)}px;background:${active ? ppColorFill : ppColorEmpty}"
                         data-idx="${i}"
                         data-value="${opt.value}">
                     </div>`;
          }).join("");
        
          controlHTML = `
            <div class="control-item control-item-compact">
              <div class="mz-pp-wrapper main-label-${ppLabelPos}" id="mz_${control.id}_wrapper">
                ${ppLabel ? `<div class="mz-pp-main-label">${ppLabel}</div>` : ""}
                <div class="mz-pp-row" id="mz_${control.id}"
                     data-option="${ppOption}"
                     data-value="${ppDefault}"
                     data-fill-color="${ppColorFill}"
                     data-empty-color="${ppColorEmpty}"
                     data-n="${n}">
                  ${segsHTML}
                  <span class="mz-pp-value-label" id="mz_${control.id}_lbl">${defaultText}</span>
                </div>
              </div>
            </div>`;
          break;
        }
        
        // ===== CONTROLLO CONDITIONAL REVEAL =====
        case "conditionalReveal": {
          const crLabel    = control.label         || "";
          const crOption   = control.option;
          const crDefault  = control.checked       || false;
          const crColor    = control.color         || "#3e6da9";
          const crChildren = control.children      || [];
        
          // genera HTML dei figli usando generateVerticalLayout ricorsivamente
          const childrenHTML = generateVerticalLayout(crChildren);
        
          controlHTML = `
            <div class="control-item control-item-compact">
              <div class="mz-cr-wrapper" id="mz_${control.id}_wrapper"
                   style="--mz-cr-color: ${crColor}">
                <div class="mz-cr-trigger" id="mz_${control.id}_trigger">
                  <div class="mz-cr-toggle${crDefault ? ' on' : ''}" id="mz_${control.id}_toggle">
                    <div class="mz-cr-knob"></div>
                  </div>
                  <span class="mz-cr-trigger-label">${crLabel}</span>
                </div>
                <div class="mz-cr-reveal${crDefault ? ' open' : ''}" id="mz_${control.id}_reveal">
                  <div class="mz-cr-inner" id="mz_${control.id}">
                    ${childrenHTML}
                  </div>
                </div>
              </div>
            </div>`;
          break;
        }
        
        // ===== CONTROLLO LINKED SLIDER =====
        case "linkedSlider": {
          const lsLabel       = control.label         || "";
          const lsLabelPos    = control.labelPosition  || "left";
          const lsOptLo       = control.optionLow;
          const lsOptHi       = control.optionHigh;
          const lsLabelLo     = control.labelLow       || "Lower";
          const lsLabelHi     = control.labelHigh      || "Upper";
          const lsMin         = control.min            ?? -1;
          const lsMax         = control.max            ??  1;
          const lsDefLo       = control.defaultLow     ?? (lsMin + (lsMax - lsMin) * 0.2);
          const lsDefHi       = control.defaultHigh    ?? (lsMin + (lsMax - lsMin) * 0.8);
          const lsStep        = control.step           || 0.01;
          const lsDecimals    = control.decimals       ?? 2;
          const lsFillColor   = control.fillColor      || "#3e6da9";
          const lsUnit        = control.unit           || "";
          const lsOrientation = control.orientation    || "horizontal";
          const isVertical    = lsOrientation === "vertical";
        
          function lsValToStr(v, dec, unit) {
            return (v >= 0 ? '+' : '') + parseFloat(v).toFixed(dec).replace('.', getDecimalSeparator()) + unit;
          }
          function lsValToPct(v, mn, mx) {
            return Math.max(0, Math.min(100, (v - mn) / (mx - mn) * 100));
          }
        
          const loPct = lsValToPct(lsDefLo, lsMin, lsMax);
          const hiPct = lsValToPct(lsDefHi, lsMin, lsMax);
          const midX  = (loPct + hiPct) / 2;
        
          // HTML track orizzontale
          function hTrack(which, pct, val, label) {
            const fillW = which === 'lo' ? `width:${pct}%` : `width:${pct}%`;
            return `
              <div class="mz-ls-row">
                <span class="mz-ls-row-label">${label}</span>
                <div class="mz-ls-track" id="mz_${control.id}_${which}_track">
                  <div class="mz-ls-fill-active"
                       id="mz_${control.id}_${which}_fill"
                       style="left:0;${fillW};background:${lsFillColor}"></div>
                  <div class="mz-ls-thumb" id="mz_${control.id}_${which}_thumb"
                       style="left:${pct}%"></div>
                </div>
                <span class="mz-ls-value" id="mz_${control.id}_${which}_val">${val}</span>
              </div>`;
          }
        
          // HTML track verticale
          function vTrack(which, pct, val, label) {
            return `
              <div class="mz-ls-row">
                <span class="mz-ls-value" id="mz_${control.id}_${which}_val">${val}</span>
                <div class="mz-ls-track" id="mz_${control.id}_${which}_track">
                  <div class="mz-ls-fill-active"
                       id="mz_${control.id}_${which}_fill"
                       style="left:0;width:100%;height:${pct}%;background:${lsFillColor};bottom:0;top:auto"></div>
                  <div class="mz-ls-thumb" id="mz_${control.id}_${which}_thumb"
                       style="left:50%;bottom:${pct}%;top:auto;transform:translate(-50%,50%)"></div>
                </div>
                <span class="mz-ls-row-label">${label}</span>
              </div>`;
          }
        
          const loHTML  = isVertical
            ? vTrack('lo', loPct, lsValToStr(lsDefLo, lsDecimals, lsUnit), lsLabelLo)
            : hTrack('lo', loPct, lsValToStr(lsDefLo, lsDecimals, lsUnit), lsLabelLo);
          const hiHTML  = isVertical
            ? vTrack('hi', hiPct, lsValToStr(lsDefHi, lsDecimals, lsUnit), lsLabelHi)
            : hTrack('hi', hiPct, lsValToStr(lsDefHi, lsDecimals, lsUnit), lsLabelHi);
        
          const linkHTML = isVertical
            ? `<div class="mz-ls-link">
                 <div class="mz-ls-link-line" id="mz_${control.id}_link_line"></div>
                 <div class="mz-ls-link-icon" id="mz_${control.id}_link_icon"
                      style="--mz-ls-link-y:${midX}%">↔</div>
               </div>`
            : `<div class="mz-ls-link">
                 <div class="mz-ls-link-line" id="mz_${control.id}_link_line"></div>
                 <div class="mz-ls-link-icon" id="mz_${control.id}_link_icon"
                      style="margin-left:calc(${midX}% - 8px)">↕</div>
               </div>`;
        
          controlHTML = `
            <div class="control-item control-item-compact">
              <div class="mz-ls-wrapper main-label-${lsLabelPos}${isVertical ? ' vertical' : ''}"
                   id="mz_${control.id}_wrapper">
                ${lsLabel ? `<div class="mz-ls-main-label">${lsLabel}</div>` : ""}
                <div class="mz-ls-body" id="mz_${control.id}"
                     data-opt-lo="${lsOptLo}"
                     data-opt-hi="${lsOptHi}"
                     data-min="${lsMin}"
                     data-max="${lsMax}"
                     data-step="${lsStep}"
                     data-decimals="${lsDecimals}"
                     data-fill-color="${lsFillColor}"
                     data-unit="${lsUnit}"
                     data-lo="${lsDefLo}"
                     data-hi="${lsDefHi}"
                     data-orientation="${lsOrientation}">
                  ${loHTML}
                  ${linkHTML}
                  ${hiHTML}
                  <div class="mz-ls-error-msg" id="mz_${control.id}_err">
                    Lower bound must be less than upper bound.
                  </div>
                </div>
              </div>
            </div>`;
          break;
        }

        // ===== CONTROLLO LABEL =====
        case "label": {
          const lblAlign = control.align || "left";
          const lblVariant = control.variant || "plain";
          const lblLevel = control.level || "span";
          let lblStyle = control.style || "";

          if (control.color) lblStyle += `color: ${control.color};`;
          if (control.fontSize) lblStyle += `font-size: ${control.fontSize};`;
          if (control.bold) lblStyle += `font-weight: bold;`;
          if (control.italic) lblStyle += `font-style: italic;`;
          if (control.backgroundColor) lblStyle += `background-color: ${control.backgroundColor};`;
          if (control.borderRadius) lblStyle += `border-radius: ${control.borderRadius};`;
          if (control.padding) lblStyle += `padding: ${control.padding};`;

          // Per i badge: inietta centraggio verticale del testo inline (massima priorità)
          if (lblVariant === 'badge') {
            lblStyle += `display: inline-flex; align-items: center; justify-content: center; line-height: 1;`;
          }
          if (control.borderTop) lblStyle += `border-top: 1px solid rgba(0,0,0,0.15); padding-top: 4px; margin-top: 2px;`;
          if (control.borderBottom) lblStyle += `border-bottom: 1px solid rgba(0,0,0,0.15); padding-bottom: 4px; margin-bottom: 2px;`;

          const labelIdAttr = control.id ? `id="${control.id}" ` : "";

          if (lblVariant === "divider") {
            const lineWidth = control.lineWidth || 1;
            const lineColor = control.lineColor || "rgba(0,0,0,0.15)";
            const lineStyle = `height: ${lineWidth}px; background-color: ${lineColor};`;

            // Normalizza allineamento orizzontale
            const norm = normalizeAlignment(lblAlign);
            const hAlign = norm.split('-')[1];

            const showLineLeft = hAlign === "center" || hAlign === "right";
            const showLineRight = hAlign === "center" || hAlign === "left";
            const hasText = control.label && control.label.trim() !== "";

            controlHTML = `
              <div ${labelIdAttr}class="mz-divider-container align-${hAlign}">
                ${showLineLeft ? `<div class="mz-divider-line" style="${lineStyle}"></div>` : ""}
                ${hasText ? `<span class="mz-label-text variant-divider" style="${lblStyle}">${control.label}</span>` : ""}
                ${showLineRight ? `<div class="mz-divider-line" style="${lineStyle}"></div>` : ""}
              </div>`;
          } else {
            const normAlign = normalizeAlignment(control.align || "left");
            const hAlign = normAlign.split('-')[1];

            controlHTML = `
              <div ${labelIdAttr}class="mz-label-item align-${hAlign}">
                <${lblLevel} class="mz-label-text variant-${lblVariant}" style="${lblStyle}">${control.label}</${lblLevel}>
              </div>`;
          }
          break;
        }

        default:
          controlHTML = "";
      }

      // GESTIONE UNIFIED ALIGNMENT + SPACING (3x3 SELF-ALIGNMENT)
      // Ogni controllo viene avvolto in un wrapper che gestisce il proprio 3x3 tramite self-alignment
      let wrapperStyle = "";
      let wrapperClass = "mz-control-wrapper";
      // Opt-in: centra il blocco compatto etichetta/select senza margini ereditati.
      if (control.type === "select" && control.compactInline === true)
        wrapperClass += " mz-compact-inline-select";

      let alignValue = control.align || "top-left";

      if (control.type === "label" && control.variant === "divider") {
        const parts = normalizeAlignment(alignValue).split('-');
        alignValue = `top-${parts[1]}`;

        // Posizionamento Assoluto Indipendente: sopra tutto, partendo dal top:0 del contenitore
        // Il margine farà il lavoro chirurgico di distanziamento (es. 20px dal titolo)
        wrapperClass += " mz-divider-wrapper mz-overlap";
        wrapperStyle += "position: absolute !important; top: 0 !important; left: 0 !important; width: 100% !important; z-index: 100 !important; pointer-events: none !important;";
      }

      const normAlign = normalizeAlignment(alignValue);
      const [v, h] = normAlign.split('-');

      wrapperClass += ` self-v-${v} self-h-${h}`;

      // Se il controllo richiede specificamente di sovrapporsi agli altri nella cella
      if (control.overlap === true && !wrapperClass.includes("mz-overlap")) {
        wrapperClass += " mz-overlap";
      }

      if (control.margin) wrapperStyle += `margin: ${control.margin};`;
      if (control.padding) wrapperStyle += `padding: ${control.padding};`;

      if (wrapperStyle) {
        return `<div class="${wrapperClass}" style="${wrapperStyle}">${controlHTML}</div>`;
      } else {
        return `<div class="${wrapperClass}">${controlHTML}</div>`;
      }
    })
    .join("");
}

function generateInfoNotesSection(tabConfig) {
  // Prende il contenuto info dalla prima scheda come default
  const defaultContent = tabConfig[0] && tabConfig[0].infoContent ?
    tabConfig[0].infoContent :
    "<em>No information available for this tab.</em>";

  return `
                <div class="info-notes-container">
            <button class="info-notes-header">
                <span class="icon-container">
                    <svg class="plus-minus-icon" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="2">
                        <line class="horizontal" x1="4" y1="8" x2="12" y2="8"></line>
                        <line class="vertical" x1="8" y1="4" x2="8" y2="12"></line>
                    </svg>
                </span>
                <span class="info-notes-title">Info & Notes</span>
            </button>
            <div class="info-notes-content">
                <div class="info-notes-body">
                    ${defaultContent}
                </div>
            </div>
        </div>
                `;
}

// ================================================================
// SUPPORTO PER I GESTORI DEGLI EVENTI
// ================================================================

function addEventListener(element, event, handler) {
  if (element.addEventListener) {
    element.addEventListener(event, handler);
  } else if (element.attachEvent) {
    // Fallback per IE vecchi
    element.attachEvent('on' + event, handler);
  }
}

function removeEventListener(element, event, handler) {
  if (element.removeEventListener) {
    element.removeEventListener(event, handler);
  } else if (element.detachEvent) {
    // Fallback per IE vecchi
    element.detachEvent('on' + event, handler);
  }
}

// ================================================================
// FUNZIONALITÀ NAVIGAZIONE SCHEDE - Vanilla JS
// ================================================================

function setupTabNavigation(element, tabConfig) {

  // Trova tutti gli header delle schede usando vanilla JS
  const tabHeaders = queryUIXAll(element, ".tab-header");

  tabHeaders.forEach(function (header) {
    // Rimuove eventi esistenti per evitare duplicazioni
    header.onclick = null;

    // Aggiunge nuovo evento click con vanilla JS
    header.onclick = function (e) {

      const targetTab = this.dataset.tab;

      // Rimuove classe active da tutti gli header
      element
        .querySelectorAll(".tab-header")
        .forEach((h) => h.classList.remove("active"));
      // Aggiunge classe active all'header corrente
      this.classList.add("active");

      // Rimuove classe active da tutti i contenuti schede
      element
        .querySelectorAll(".tab-content")
        .forEach((c) => c.classList.remove("active"));
      // Aggiunge classe active al contenuto target
      const targetContent = queryUIX(element, `#${targetTab}`);
      if (targetContent) {
        targetContent.classList.add("active");
      }

      // Aggiorna contenuto info notes
      updateInfoNotesContent(element, targetTab, tabConfig);

      return false; // Previene propagazione evento
    };
  });
}

// ================================================================
// FUNZIONALITÀ NAVIGAZIONE SUB-TAB (NESTED) - Vanilla JS
// ================================================================
// Gestisce il cambio tra i tab interni di un tabbedGroup (Segmented Control)

function setupSubTabNavigation(element) {

  // Trova tutti i contenitori di sotto-tab
  const segmentedBars = queryUIXAll(element, ".mz-subtab-headers");

  segmentedBars.forEach(function (bar) {
    const tabGroupId = bar.dataset.tabgroup;
    const items = bar.querySelectorAll(".mz-subtab-header");

    items.forEach(function (item) {
      item.onclick = function () {
        const subTabId = this.dataset.subtab;

        // 1. Aggiorna lo stato dei pulsanti nella barra corrente
        items.forEach(i => i.classList.remove("active"));
        this.classList.add("active");

        // 2. Nasconde tutti i contenuti associati a questo gruppo e mostra quello attivo
        const parentElement = bar.closest(".grid-cell");
        if (parentElement) {
          const contents = parentElement.querySelectorAll(`.mz-subtab-content[data-parent-group="${tabGroupId}"]`);
          contents.forEach(c => c.classList.remove("active"));

          const targetContent = parentElement.querySelector(`#${subTabId}`);
          if (targetContent) {
            targetContent.classList.add("active");
          }
        }
      };
    });
  });
}

// ================================================================
// FUNZIONALITÀ TOGGLE INFO & NOTES - Vanilla JS
// ================================================================
// Gestisce apertura/chiusura della sezione informativa

function setupInfoNotesToggle(element) {
  const infoButton = queryUIX(element, ".info-notes-header");

  if (infoButton) {
    infoButton.onclick = function () {
      const content = this.nextElementSibling;

      const isExpanded = content.classList.contains("expanded");

      if (isExpanded) {
        // Chiude la sezione
        content.classList.remove("expanded");
        this.classList.remove("expanded");
      } else {
        // Apre la sezione
        content.classList.add("expanded");
        this.classList.add("expanded");
      }
    };
  }
}

// ================================================================
// AGGIORNAMENTO CONTENUTO INFO NOTES
// ================================================================
// Cambia il contenuto informativo in base alla scheda attiva

function updateInfoNotesContent(element, activeTabId, tabConfig) {
  const activeTabConfig = tabConfig.find((tab) => tab.id === activeTabId);
  const infoBody = queryUIX(element, ".info-notes-body");

  if (activeTabConfig && activeTabConfig.infoContent && infoBody) {
    infoBody.innerHTML = activeTabConfig.infoContent;
  }
}

// ================================================================
// UTILITY: RACCOLTA DI TUTTI I CONTROLLI DI UN TAB
// ================================================================
// Restituisce una lista piatta di tutti i controlli, inclusi quelli nei sub-tab

function getAllControls(tab) {
  const allControls = [];
  const collect = controls => {
    for (const control of controls || []) {
      allControls.push(control);
      if (control.type === 'conditionalReveal') collect(control.children);
    }
  };
  collect(tab.controls);

  if (tab.groups) {
    tab.groups.forEach(group => {
      // Se è un tabbedGroup, aggiungiamo i controlli da tutti i suoi sub-tab
      if (group.type === "tabbedGroup" && group.subTabs) {
        group.subTabs.forEach(subTab => {
          if (subTab.controls) {
            collect(subTab.controls);
          }
        });
      }
    });
  }

  return allControls;
}

function hideNativeActionButtons(tabConfig, ui, bindings) {
  tabConfig.forEach(tab => {
    getAllControls(tab).forEach(control => {
      if (control.type === 'actionButton' && control.actionOption) {
        const native = ui[control.actionOption];
        const field = native && (native.el || native.$el);
        const root = field && (field[0] || field);
        const button = root && (root.matches('button.jmv-action-button')
          ? root : root.querySelector('button.jmv-action-button'));
        if (button) {
          const previous = button.style.getPropertyValue('display');
          const priority = button.style.getPropertyPriority('display');
          button.style.setProperty('display','none');
          bindings.cleanups.push(() => previous
            ? button.style.setProperty('display',previous,priority)
            : button.style.removeProperty('display'));
        }
      }
    });
  });
}

// ================================================================
// BINDING CONTROLLI - SINCRONIZZAZIONE UI CON BACKEND
// ================================================================
// Collega ogni controllo UI con le opzioni backend di jamovi
// Questa è una delle funzioni più critiche per il funzionamento
// (2) Funzione per aggiornamenti e nuovi elementi
function setupControlBindings(element, ui, tabConfig) {
  const bindings = { owner: ui.tabbedControl, cleanups: [], mounts: [] };
  TABBED_BINDINGS.set(element, bindings);
  function listen(target, event, handler) {
    addEventListener(target, event, handler);
    bindings.cleanups.push(() => removeEventListener(target, event, handler));
  }
  tabConfig.forEach((tab) => {
    const allTabControls = getAllControls(tab); // Prende standard + sub-tab
    allTabControls.forEach((control) => {
      switch (control.type) {

        // Il solo radio selezionato scrive l'opzione; evita notifiche duplicate.
        case "radio": {
          const option = ui[control.option];
          if (!option) break;
          queryUIXAll(element, 'input.control-radio').forEach(radio => {
            if (radio.name !== control.option) return;
            radio.onchange = function () {
              if (this.checked && option.value() !== this.value) {
                option.setValue(this.value);
                refreshControlDependencies(ui);
              }
            };
          });
          break;
        }

        // === Binding per checkbox standard ===
        case "checkbox":
          const controlElement = queryUIX(element, `#mz_${control.id}`);
          if (controlElement) {
            const animatedText = control.selectionAnimation === 'rotate-icon'
              ? controlElement.closest('.toggle-label, .toggle-label-vertical')?.querySelector('.toggle-text')
              : null;
            if (animatedText) {
              // Eccezione locale esplicita, senza cambiare le preferenze del sistema.
              animatedText.classList.toggle('mz-icon-motion-allowed', control.respectReducedMotion === false);
              listen(animatedText, 'animationend', event => {
                if (event.animationName === 'mz-icon-selection-turn')
                  animatedText.classList.remove('mz-icon-rotate-once');
              });
              bindings.cleanups.push(() => animatedText.classList.remove('mz-icon-rotate-once', 'mz-icon-motion-allowed'));
            }
            listen(controlElement, 'change', function () {
              // Solo l'attivazione effettiva anima l'icona, mai restore o refresh.
              if (animatedText) {
                animatedText.classList.remove('mz-icon-rotate-once');
                if (this.checked && !ui[control.option].value() &&
                    (control.respectReducedMotion === false ||
                     !window.matchMedia('(prefers-reduced-motion: reduce)').matches)) {
                  void animatedText.offsetWidth;
                  animatedText.classList.add('mz-icon-rotate-once');
                }
              }
              // Aggiorna il valore nel backend quando checkbox cambia
              ui[control.option].setValue(this.checked);
              // Le opzioni hidden non garantiscono un update immediato della
              // vista jamovi: rivaluta anche i controlli custom dipendenti.
              refreshControlDependencies(ui);
            });
          }
          break;

        // === Binding per checkbox output (salva variabili) ===
        case "output":
          const outputControlElement = queryUIX(element, `#mz_${control.id}`);
          if (outputControlElement) {
            listen(outputControlElement, 'change', function () {
              // Gestisce struttura dati complessa per output variabili
              const currentData = ui[control.option].getValue() || {
                value: false,
                vars: [],
              };
              const outputData = {
                value: this.checked,
                vars: currentData.vars || [],
              };
              ui[control.option].setValue(outputData);
            });
          }
          break;

        // === Binding per input numerici ===
        case "number":
          bindNumericControl(element, ui, control, listen);
          break;

        // === Binding per select/dropdown ===
        case "select":
          const selectControlElement = queryUIX(element, `#mz_${control.id}`);
          if (selectControlElement) {
            listen(selectControlElement, 'change', function () {
              const previousValue = ui[control.option].value();
              const nextValue = this.value;
              ui[control.option].setValue(this.value);
              // Le dipendenze visive seguono subito il select, senza attendere un update dei risultati.
              refreshControlDependencies(ui);
              // Solo una transizione del select: nessun toast al restore o ai refresh.
              const notice = control.selectionToasts && control.selectionToasts[nextValue];
              if (previousValue !== nextValue && notice && notice.message) {
                const translate = text => ui.tabbedControl && typeof ui.tabbedControl.translate === 'function'
                  ? ui.tabbedControl.translate(text) : text;
                const { showToast } = require('./mzToast');
                showToast(translate(notice.message), 'info', notice.duration ?? 6000,
                  'CC', 'light', translate(notice.toastTitle || ''));
              }
            });
          }
          break;

        // === Binding per input testo ===
        case "string":
          const stringControlElement = queryUIX(element, `#mz_${control.id}`);
          if (stringControlElement) {
            listen(stringControlElement, 'input', function () {
              ui[control.option].setValue(this.value);
            });

            listen(stringControlElement, 'change', function () {
              ui[control.option].setValue(this.value);
            });
          }
          break;


        // === Binding per deviatore ===
        case "deviatore":
          const devEl = queryUIX(element, `#mz_${control.id}`);
        
          if (devEl) {
            const orientation = devEl.dataset.orientation || "horizontal";
            const wrapper = queryUIX(element, `#mz_${control.id}_wrapper`);
                    
            const setDevState = (newState) => {
                if (devEl.disabled) return;
                if (control.allowOff === false && newState === 'posOff') return;
                const currentState = devEl.dataset.state;
            
                if (currentState === newState) {
                    return;
                }
            
                devEl.dataset.state = newState;
            
                if (wrapper) {
                    const stateLabels = wrapper.querySelector(
                        '.deviatore-state-labels, .deviatore-line-state-labels'
                    );
            
                    if (stateLabels) {
                        stateLabels.dataset.state = newState;
                    }
                }
            
                const optionValue = control.stateValues && control.stateValues[newState] !== undefined
                    ? control.stateValues[newState] : newState;
                if (
                    ui[control.option] &&
                    ui[control.option].value() !== optionValue
                ) {
                    ui[control.option].setValue(optionValue);
                    refreshControlDependencies(ui);
                }
            };

            const track =
              devEl.querySelector('.deviatore-track') ||
              devEl.querySelector('.deviatore-line-track');
        
            if (track) {
              listen(track, 'click', function (e) {
                e.stopPropagation();
        
                const rect = track.getBoundingClientRect();
        
                const ratio = orientation === "vertical"
                  ? (e.clientY - rect.top) / rect.height
                  : (e.clientX - rect.left) / rect.width;
        
                // Linea: punti a 0 / 50% / 100%, soglie a metà strada tra i punti
                const isLine = track.classList.contains('deviatore-line-track');
                const lowCut = isLine ? 0.25 : 0.333;
                const highCut = isLine ? 0.75 : 0.666;

                let clicked;

                if (control.allowOff === false) {
                  clicked = ratio < 0.5 ? 'posLorT' : 'posRorB';
                } else if (ratio < lowCut) {
                  clicked = "posLorT";
                } else if (ratio < highCut) {
                  clicked = "posOff";
                } else {
                  clicked = "posRorB";
                }
        
                setDevState(
                  control.allowOff !== false && devEl.dataset.state === clicked
                    ? "posOff"
                    : clicked
                );
              });
            }
          }
          break;

        // === Binding per verticalListSlider ===
        case "verticalListSlider": {
          const vlsRow    = queryUIX(element, `#mz_${control.id}`);
          const vlsBody   = queryUIX(element, `#mz_${control.id}_body`);
          const vlsHeader = queryUIX(element, `#mz_${control.id}_header`);
          const vlsArrow  = queryUIX(element, `#mz_${control.id}_harrow`);
          const vlsHval   = queryUIX(element, `#mz_${control.id}_hval`);
        
          if (!vlsRow) break;
        
          // accordion toggle
          if (vlsHeader && vlsBody) {
            listen(vlsHeader, 'click', function () {
              const isExpanded = vlsBody.classList.contains('expanded');
              if (isExpanded) {
                vlsBody.classList.remove('expanded');
                if (vlsArrow) vlsArrow.classList.remove('expanded');
              } else {
                vlsBody.classList.add('expanded');
                if (vlsArrow) vlsArrow.classList.add('expanded');
              }
            });
          }
        
          function vlsUpdateUI(idx, opts, rowEl) {
            const n           = opts.length;
            const itemGap     = parseInt(rowEl.dataset.itemGap, 10) || 22;
            const activeColor = rowEl.dataset.activeColor || '#3e6da9';
            const fillColor   = rowEl.dataset.fillColor   || '#3e6da9';
            const thumbEl     = queryUIX(element, `#mz_${control.id}_thumb`);
            const fillEl      = queryUIX(element, `#mz_${control.id}_fill`);
            const labelEls    = rowEl.querySelectorAll('.vls-label-item');
            const hval        = queryUIX(element, `#mz_${control.id}_hval`);
        
            const centerPx = idx * itemGap + itemGap / 2;
            const fillH    = idx * itemGap;
            const trackTop = itemGap / 2;
        
            if (thumbEl) thumbEl.style.top = `${centerPx}px`;
            if (fillEl) {
              fillEl.style.top    = `${trackTop}px`;
              fillEl.style.height = `${fillH}px`;
            }
        
            labelEls.forEach((lbl, i) => {
              lbl.classList.toggle('active', i === idx);
              lbl.style.color = i === idx ? activeColor : '';
            });
        
            if (hval && opts[idx]) hval.textContent = opts[idx].text;
          }
        
          // click su label
          const labelItems = vlsRow.querySelectorAll('.vls-label-item');
          labelItems.forEach(function (lbl) {
            listen(lbl, 'click', function (e) {
              e.stopPropagation();
              const idx  = parseInt(this.dataset.idx, 10);
              const opts = control.options || [];
              if (!opts[idx]) return;
              vlsRow.dataset.value = opts[idx].value;
              if (ui[control.option]) ui[control.option].setValue(opts[idx].value);
              vlsUpdateUI(idx, opts, vlsRow);
            });
          });
        
          break;
        }

        // === Binding per segmentedControl ===
        case "segmentedControl": {
          const scTrack = queryUIX(element, `#mz_${control.id}`);
          if (!scTrack) break;
          const translate = text => typeof ui.tabbedControl.translate === 'function'
            ? ui.tabbedControl.translate(text) : text;
          // label è già tradotta al montaggio; ariaLabel e il ripiego vengono tradotti qui.
          scTrack.setAttribute('aria-label', control.ariaLabel ? translate(control.ariaLabel)
            : control.label || translate(_('Choose an option')));
        
          const activeColor  = scTrack.dataset.activeColor || "#3e6da9";
          const labelPos     = scTrack.dataset.labelPos    || "left";
          const orientation  = scTrack.dataset.orientation || "horizontal";
        
          scTrack.querySelectorAll('.mz-seg-btn').forEach(btn => {
            listen(btn, 'click', function () {
              scTrack.querySelectorAll('.mz-seg-btn').forEach(b => {
                b.classList.remove('active');
                b.setAttribute('aria-checked', 'false');
                b.tabIndex = -1;
                // ripristina colore opzione individuale o nessuno
                b.style.color = b.dataset.optColor || '';
              });
              this.classList.add('active');
              this.setAttribute('aria-checked', 'true');
              this.tabIndex = 0;
              this.style.color = this.dataset.optColor || activeColor;
              if (ui[control.option] && ui[control.option].value() !== this.dataset.value)
                ui[control.option].setValue(this.dataset.value);
            });
            listen(btn, 'keydown', function (event) {
              const buttons = [...scTrack.querySelectorAll('.mz-seg-btn')].filter(b => !b.disabled);
              const index = buttons.indexOf(this);
              const previousKey = orientation === 'vertical' ? 'ArrowUp' : 'ArrowLeft';
              const nextKey = orientation === 'vertical' ? 'ArrowDown' : 'ArrowRight';
              let next;
              if (event.key === previousKey) next = (index + buttons.length - 1) % buttons.length;
              else if (event.key === nextKey) next = (index + 1) % buttons.length;
              else if (event.key === 'Home') next = 0;
              else if (event.key === 'End') next = buttons.length - 1;
              else return;
              event.preventDefault();
              if (buttons[next]) { buttons[next].focus(); buttons[next].click(); }
            });
          });
          break;
        }
        
        // === Binding per priorityPicker ===
        case "priorityPicker": {
          const ppRow = queryUIX(element, `#mz_${control.id}`);
          if (!ppRow) break;
        
          function ppUpdateUI(idx, rowEl) {
            const fillColor  = rowEl.dataset.fillColor  || "#3e6da9";
            const emptyColor = rowEl.dataset.emptyColor || "#dddddd";
            const lbl        = queryUIX(element, `#mz_${control.id}_lbl`);
            const opts       = control.options || [];
        
            rowEl.querySelectorAll('.mz-pp-seg').forEach((seg, i) => {
              seg.style.background = i <= idx ? fillColor : emptyColor;
            });
        
            if (lbl && opts[idx]) lbl.textContent = opts[idx].text;
            rowEl.dataset.value = opts[idx] ? opts[idx].value : "";
          }
        
          ppRow.querySelectorAll('.mz-pp-seg').forEach(seg => {
            listen(seg, 'click', function () {
              const idx  = parseInt(this.dataset.idx, 10);
              const opts = control.options || [];
              if (ui[control.option]) ui[control.option].setValue(opts[idx] ? opts[idx].value : "");
              ppUpdateUI(idx, ppRow);
            });
          });
          break;
        }
        
        // === Binding per conditionalReveal ===
        case "conditionalReveal": {
          const crTrigger = queryUIX(element, `#mz_${control.id}_trigger`);
          const crToggle  = queryUIX(element, `#mz_${control.id}_toggle`);
          const crReveal  = queryUIX(element, `#mz_${control.id}_reveal`);
          if (!crTrigger) break;
        
          listen(crTrigger, 'click', function () {
            const isOn = crToggle.classList.toggle('on');
            crReveal.classList.toggle('open', isOn);
            if (ui[control.option]) ui[control.option].setValue(isOn);
          });
        
          // I figli sono già inclusi nella raccolta piatta, con un solo registro cleanup.
          break;
        }
        
        // === Binding per linkedSlider ===
        case "linkedSlider": {
          const lsBody = queryUIX(element, `#mz_${control.id}`);
          if (!lsBody) break;
        
          const isVertical = lsBody.dataset.orientation === "vertical";
          const loTrack    = queryUIX(element, `#mz_${control.id}_lo_track`);
          const hiTrack    = queryUIX(element, `#mz_${control.id}_hi_track`);
          const loThumb    = queryUIX(element, `#mz_${control.id}_lo_thumb`);
          const hiThumb    = queryUIX(element, `#mz_${control.id}_hi_thumb`);
          const loFill     = queryUIX(element, `#mz_${control.id}_lo_fill`);
          const hiFill     = queryUIX(element, `#mz_${control.id}_hi_fill`);
          const loVal      = queryUIX(element, `#mz_${control.id}_lo_val`);
          const hiVal      = queryUIX(element, `#mz_${control.id}_hi_val`);
          const errMsg     = queryUIX(element, `#mz_${control.id}_err`);
          if (errMsg && typeof ui.tabbedControl.translate === 'function')
            errMsg.textContent = ui.tabbedControl.translate(_('Lower bound must be less than upper bound.'));
          const linkIcon   = queryUIX(element, `#mz_${control.id}_link_icon`);
        
          const mn        = parseFloat(lsBody.dataset.min);
          const mx        = parseFloat(lsBody.dataset.max);
          const dec       = parseInt(lsBody.dataset.decimals, 10) ?? 2;
          const fillColor = lsBody.dataset.fillColor || "#3e6da9";
          const unit      = lsBody.dataset.unit || "";
        
          let lo = parseFloat(lsBody.dataset.lo);
          let hi = parseFloat(lsBody.dataset.hi);
          let dragging = null;
        
          function lsPct(v) { return Math.max(0, Math.min(100, (v - mn) / (mx - mn) * 100)); }
          function lsSnap(v) {
            const step = parseFloat(lsBody.dataset.step) || 0.01;
            return Math.round(v / step) * step;
          }
          function lsFmt(v) { return (v >= 0 ? '+' : '') + v.toFixed(dec).replace('.', getDecimalSeparator(element.dataset.lang)) + unit; }
        
          function lsUpdateUI() {
            lsBody.dataset.lo = lo;
            lsBody.dataset.hi = hi;
            const loPct  = lsPct(lo);
            const hiPct  = lsPct(hi);
            const isErr  = lo >= hi;
            const col    = isErr ? '#c0392b' : fillColor;
            const midPct = (loPct + hiPct) / 2;
        
            if (!isVertical) {
              // orizzontale
              loThumb.style.left    = loPct + '%';
              hiThumb.style.left    = hiPct + '%';
              loFill.style.width    = loPct + '%';
              hiFill.style.width    = hiPct + '%';
              loFill.style.background = col;
              hiFill.style.background = col;
              if (linkIcon) linkIcon.style.marginLeft = `calc(${midPct}% - 8px)`;
            } else {
              // verticale
              loThumb.style.bottom  = loPct + '%';
              hiThumb.style.bottom  = hiPct + '%';
              loFill.style.height   = loPct + '%';
              hiFill.style.height   = hiPct + '%';
              loFill.style.background = col;
              hiFill.style.background = col;
              if (linkIcon) linkIcon.style.marginTop = `calc(${100 - midPct}% - 8px)`;
            }
        
            loVal.textContent = lsFmt(lo);
            hiVal.textContent = lsFmt(hi);
            loThumb.classList.toggle('error', isErr);
            hiThumb.classList.toggle('error', isErr);
            loVal.classList.toggle('error', isErr);
            hiVal.classList.toggle('error', isErr);
            if (errMsg) errMsg.classList.toggle('show', isErr);
          }
        
          function lsGetPct(e, track) {
            const rect = track.getBoundingClientRect();
            if (!isVertical) {
              return Math.max(0, Math.min(100, (e.clientX - rect.left) / rect.width * 100));
            } else {
              // verticale: bottom = 0%, top = 100% → invertiamo
              return Math.max(0, Math.min(100, (1 - (e.clientY - rect.top) / rect.height) * 100));
            }
          }
        
          listen(loThumb, 'mousedown', e => { dragging = 'lo'; e.preventDefault(); });
          listen(hiThumb, 'mousedown', e => { dragging = 'hi'; e.preventDefault(); });
        
          listen(document, 'mousemove', function (e) {
            if (!dragging) return;
            lo = parseFloat(lsBody.dataset.lo);
            hi = parseFloat(lsBody.dataset.hi);
            const track   = dragging === 'lo' ? loTrack : hiTrack;
            const pct     = lsGetPct(e, track);
            const raw     = mn + (pct / 100) * (mx - mn);
            const snapped = lsSnap(raw);
        
            if (dragging === 'lo') {
              lo = snapped;
              if (ui[control.optionLow]) ui[control.optionLow].setValue(lo);
            } else {
              hi = snapped;
              if (ui[control.optionHigh]) ui[control.optionHigh].setValue(hi);
            }
            lsUpdateUI();
          });
        
          listen(document, 'mouseup', () => { dragging = null; });
        
          lsUpdateUI();
          break;
        }

        // === Binding per plotBlock (complesso) ===
        case "plotBlock":
          // Usa nomi opzione espliciti per flessibilità
          const showId = control.showOption;
          const showElement = queryUIX(element, `#mz_${showId}`);
          if (showElement) {
            listen(showElement, 'change', function () {
              ui[showId].setValue(this.checked);
            });
          }

          // Binding per larghezza plot
          const widthId = control.widthOption;
          const widthControlElement = queryUIX(element, `#mz_${widthId}`);
          if (widthControlElement) {
            listen(widthControlElement, 'input', function () {
              ui[widthId].setValue(parseInt(this.value));

              // Sincronizza slider larghezza se esiste
              const widthSliderElement = queryUIX(element, `#mz_${widthId}_slider`);
              if (widthSliderElement) {
                widthSliderElement.value = this.value;
              }
            });

            listen(widthControlElement, 'change', function () {
              ui[widthId].setValue(parseInt(this.value));

              // Sincronizza slider larghezza se esiste
              const widthSliderElement = queryUIX(element, `#mz_${widthId}_slider`);
              if (widthSliderElement) {
                widthSliderElement.value = this.value;
              }
            });
          }

          // Binding per altezza plot
          const heightId = control.heightOption;
          const heightControlElement = queryUIX(element, `#mz_${heightId}`);
          if (heightControlElement) {
            listen(heightControlElement, 'input', function () {
              ui[heightId].setValue(parseInt(this.value));

              // Sincronizza slider altezza se esiste
              const heightSliderElement = queryUIX(element, `#mz_${heightId}_slider`);
              if (heightSliderElement) {
                heightSliderElement.value = this.value;
              }
            });

            listen(heightControlElement, 'change', function () {
              ui[heightId].setValue(parseInt(this.value));

              // Sincronizza slider altezza se esiste
              const heightSliderElement = queryUIX(element, `#mz_${heightId}_slider`);
              if (heightSliderElement) {
                heightSliderElement.value = this.value;
              }
            });
          }

          // Gestisce slider larghezza se showSlider è abilitato
          if (control.showSlider) {
            const widthSliderControlElement = queryUIX(element, `#mz_${widthId}_slider`);
            if (widthSliderControlElement) {
              listen(widthSliderControlElement, 'input', function () {
                const value = parseInt(this.value) || control.defaultWidth;
                ui[widthId].setValue(value);

                // Sincronizza input larghezza
                if (widthControlElement) {
                  widthControlElement.value = value;
                }
              });

              listen(widthSliderControlElement, 'change', function () {
                const value = parseInt(this.value) || control.defaultWidth;
                ui[widthId].setValue(value);

                // Sincronizza input larghezza
                if (widthControlElement) {
                  widthControlElement.value = value;
                }
              });
            }

            // Gestisce slider altezza se showSlider è abilitato
            const heightSliderControlElement = queryUIX(element, `#mz_${heightId}_slider`);
            if (heightSliderControlElement) {
              listen(heightSliderControlElement, 'input', function () {
                const value = parseInt(this.value) || control.defaultHeight;
                ui[heightId].setValue(value);

                // Sincronizza input altezza
                if (heightControlElement) {
                  heightControlElement.value = value;
                }
              });

              listen(heightSliderControlElement, 'change', function () {
                const value = parseInt(this.value) || control.defaultHeight;
                ui[heightId].setValue(value);

                // Sincronizza input altezza
                if (heightControlElement) {
                  heightControlElement.value = value;
                }
              });
            }
          }
          break;

        // === Binding per actionButton ===
        case "actionButton":
          const actionButtonElement = queryUIX(element, `#mz_${control.id}`);
          if (actionButtonElement) {
            const actionOptionName = actionButtonElement.dataset.actionOption;
            listen(actionButtonElement, 'click', function () {
              const opt = ui[actionOptionName];
              if (!opt) {
                console.error(`[Metafora] Action option non trovata: ${actionOptionName}`);
                return;
              }
              // Prima resetta silenziosamente, poi triggera
              opt.setSourceValue(false);
              opt.setValue(true);
            });
          }
          break;
      }
    });
  });
}

// ================================================================
// GESTIONE DIPENDENZE TRA CONTROLLI - VERSIONE AVANZATA
// ================================================================
// Supporta dipendenze singole e multiple con operatori AND/OR

// ================================================================
// REGISTRO REFRESH DELLE DIPENDENZE
// ================================================================
// Permette di rivalutare le dipendenze quando cambia un'opzione
// Jamovi che non possiede un elemento DOM generato dal TabbedControl.

const DEPENDENCY_REFRESHERS = new WeakMap();

function registerDependencyRefresher(element, refreshFn) {
  let refreshers = DEPENDENCY_REFRESHERS.get(element);

  if (!refreshers) {
    refreshers = [];
    DEPENDENCY_REFRESHERS.set(element, refreshers);
  }

  refreshers.push(refreshFn);
}

function refreshControlDependencies(ui) {
    if (!ui || !ui.tabbedControl) return;

    let element = ui.tabbedControl.$el;

    if (element && element[0]) {
        element = element[0];
    }

    if (!element) return;

    const refreshers = DEPENDENCY_REFRESHERS.get(element);

    if (!refreshers || refreshers.length === 0) return;

    // Impedisce la rientranza quando un refresher modifica
    // un'opzione Jamovi tramite setValue(), provocando
    // immediatamente un nuovo evento update.
    if (element.__mzUIX_dependencyRefreshRunning === true) {
        return;
    }

    element.__mzUIX_dependencyRefreshRunning = true;

    try {
        refreshers.forEach(refreshFn => {
            try {
                refreshFn();
            } catch (e) {
                console.warn(
                    '[Metafora] Errore nel refresh delle dipendenze:',
                    e
                );
            }
        });
    } finally {
        element.__mzUIX_dependencyRefreshRunning = false;
    }
}

// Stesso riconoscimento delle condizioni per dipendenze singole, multiple e gruppi.
function hasDisableCondition(config) {
  return ['disableWhen', 'enableWhen', 'valueIn', 'valueNotIn'].some(key => Array.isArray(config[key])) ||
    typeof config.notEmpty === 'boolean' || config.isTrue === true || config.isFalse === true ||
    ['greaterThan', 'lessThan', 'greaterOrEqual', 'lessOrEqual', 'equalTo', 'notEqualTo',
     'contains', 'notContains', 'itemCountEqualTo', 'itemCountGreaterThan',
     'itemCountGreaterOrEqual', 'itemCountLessThan', 'itemCountLessOrEqual']
      .some(key => config[key] !== undefined);
}

function setupControlDependencies(element, ui, tabConfig) {
  // Mappa per salvare gli stati precedenti
  const previousStates = {};

  // Ricrea il registro per questo TabbedControl.
  // Evita duplicazioni se il setup viene richiamato.
  DEPENDENCY_REFRESHERS.set(element, []);

  tabConfig.forEach((tab) => {
    const allTabControls = getAllControls(tab); // Prende standard + sub-tab
    allTabControls.forEach((control) => {
      // Salta se il controllo non ha dipendenze
      if (!control.dependsOn) return;

      const dep = control.dependsOn;
      const dependentControlId = control.id;

      // Trova l'elemento DOM dipendente
      // fallback per tipo label: il div ha l'id direttamente
      let dependentElement = queryUIX(element, `#mz_${dependentControlId}`);
      if (!dependentElement && control.type === "label") {
        dependentElement = queryUIX(element, `[id="${dependentControlId}"]`);
      }
      if (!dependentElement) {
        console.warn(`Elemento dipendente non trovato: ${dependentControlId}`);
        return;
      }

      // ===== GESTIONE DIPENDENZA SINGOLA =====
      if (dep.control && typeof dep.control === 'string') {
        const masterControlId = dep.control;
        const masterElement = queryUIX(element, `#mz_${masterControlId}`);

        // Se l'elemento non è trovato localmente, verifichiamo se è un'opzione esterna nel setup
        if (!masterElement && !ui[masterControlId]) {
          console.warn(`Elemento master o opzione non trovati: ${masterControlId}`);
          return;
        }

        let metadataGeneration = 0;
        let typeNoticeActive = false;
        const translate = text => typeof ui.tabbedControl.translate === 'function'
          ? ui.tabbedControl.translate(text) : text;
        const setTooltip = dep.disabledTooltip ? createDependencyTooltip(
          dependentElement.closest('.checkbox-label, .toggle-label') || dependentElement,
          TABBED_BINDINGS.get(element)) : null;
        const applyDependencyDisabled = (disabled) => {
          applyDisabledState(dependentElement, disabled, control, dep,
            dependentControlId, previousStates, ui);
          if (setTooltip) setTooltip(disabled ? translate(dep.disabledTooltip) : '');
        };
        const updateDependentState = () => {
          const generation = ++metadataGeneration;
          const rawMasterValue = getMasterValue(masterControlId, masterElement);
          let masterValue = rawMasterValue;

          // Normalizzazione valore per variabili jamovi (array -> stringa/valore)
          // Gestione robusta anche per null/undefined
          if (masterValue === null || masterValue === undefined) {
            masterValue = "";
          } else if (Array.isArray(masterValue)) {
            if (masterValue.length === 0) masterValue = "";
            else if (masterValue.length === 1) masterValue = masterValue[0];
          }

          // Valuta e applica DISABLE (se configurato)
          if (hasDisableCondition(dep)) {
            // La cardinalità richiede l'array originale, anche con un solo elemento.
            const shouldDisable = evaluateSingleCondition(rawMasterValue, dep);
            // Condizione sui tipi delle variabili del master singolo.
            // Una covariata richiede livello continuo: la memoria numerica
            // da sola non distingue i valori dai codici di un fattore.
            if (dep.variableType === 'numeric' && !shouldDisable &&
                typeof ui.tabbedControl.requestData === 'function') {
              const names = [...new Set([rawMasterValue].flat(Infinity)
                .filter(value => typeof value === 'string' && value.length > 0))];
              Promise.all(names.map(columnName => Promise.resolve().then(() =>
                ui.tabbedControl.requestData('column', {
                  columnName, properties: ['dataType', 'measureType']
                })).catch(() => null))).then(columns => {
                  const binding = TABBED_BINDINGS.get(element);
                  if (generation !== metadataGeneration || !binding || binding.owner !== ui.tabbedControl) return;
                  const numeric = columns.some(column => column && column.measureType === 'continuous' &&
                    ['integer','decimal'].includes(column.dataType));
                  // Un errore di lettura non cancella una selezione valida;
                  // il backend conserva comunque il proprio controllo finale.
                  const unknown = columns.some(column => !column || column.dataType === undefined ||
                    column.measureType === undefined);
                  const incompatible = !numeric && !unknown;
                  applyDependencyDisabled(incompatible);
                  const mixed = numeric && !unknown && columns.some(column =>
                    ['nominal','ordinal'].includes(column.measureType));
                  // L'avviso spiega il grafico disponibile con covariate e fattori insieme.
                  // Con soli fattori bastano controllo disabilitato e tooltip.
                  if (mixed && !typeNoticeActive && dep.mixedVariableToast) {
                    const { showToast } = require('./mzToast');
                    showToast(translate(dep.mixedVariableToast), 'info', 6000, 'CC',
                      'light', translate(dep.mixedVariableToastTitle || _('Plot availability')));
                  }
                  typeNoticeActive = mixed;
                });
            } else {
              typeNoticeActive = false;
              applyDependencyDisabled(shouldDisable);
            }
          }

          // Valuta e applica HIDE (se configurato)
          if (dep.hideWhen || dep.showWhen) {
            const shouldHide = evaluateHideCondition(masterValue, dep);
            applyHiddenState(
              dependentElement,
              shouldHide,
              control,
              dep,
              dependentControlId,
              previousStates,
              ui
            );
          }
        };

        // Registra la funzione nel registro centrale.
        // Potrà essere richiamata anche quando il master è
        // un'opzione Jamovi senza DOM custom.
        registerDependencyRefresher(
          element,
          updateDependentState
        );

        // Valutazione iniziale.
        updateDependentState();

        // Il master custom viene aggiornato tramite l'evento
        // dell'opzione Jamovi e il refresh centrale.
        // Non registrare un secondo listener DOM:
        // il controllo custom ha già eseguito ui[control.option].setValue().
      }
      // ===== GESTIONE DIPENDENZE CON GRUPPI ANNIDATI =====
      else if (
        dep.groups &&
        Array.isArray(dep.groups)
      ) {

        const masterElements = [];

        // Raccoglie tutti i master presenti nei gruppi.
        // Un master può essere:
        // 1. un controllo custom nel DOM
        // 2. un'opzione Jamovi presente in ui ma non generata
        //    dal TabbedControl.
        dep.groups.forEach((group) => {

          if (
            !group ||
            !Array.isArray(group.controls)
          ) {
            return;
          }

          group.controls.forEach((conditionConfig) => {

            const masterControlId =
              conditionConfig.control;

            const masterElement =
              queryUIX(element, 
                `#mz_${masterControlId}`
              );

            const masterUI =
              ui && ui[masterControlId];

            if (masterElement || masterUI) {

              masterElements.push({
                element: masterElement || null,
                ui: masterUI || null,
                config: conditionConfig
              });

            } else {

              console.warn(
                `Elemento master o opzione Jamovi non trovati: ${masterControlId}`
              );
            }
          });
        });

        if (masterElements.length === 0) {

          console.warn(
            `Nessun master trovato per ${dependentControlId}`
          );

          return;
        }

        const updateDependentState = () => {

          // ---------------------------------------------------------
          // Recupero del valore di un master
          // ---------------------------------------------------------

          const getConditionValue = (conditionConfig) => {

            const master = masterElements.find(
              item =>
                item.config === conditionConfig
            );

            if (!master) {
              return "";
            }

            let value = getMasterValue(
              conditionConfig.control,
              master.element
            );

            // Le condizioni sulla cardinalità non devono perdere gli array vuoti o singoli.
            if (Array.isArray(value) &&
                ['itemCountEqualTo', 'itemCountGreaterThan', 'itemCountGreaterOrEqual',
                 'itemCountLessThan', 'itemCountLessOrEqual'].some(key => conditionConfig[key] !== undefined)) {
              return value;
            }

            if (
              value === null ||
              value === undefined
            ) {
              value = "";

            } else if (Array.isArray(value)) {

              if (value.length === 0) {
                value = "";

              } else if (value.length === 1) {
                value = value[0];
              }
            }

            return value;
          };

          // ---------------------------------------------------------
          // DISABLE
          // ---------------------------------------------------------
          // Riconoscitori group 
          const hasDisableConditions = dep.groups.some(group =>
            Array.isArray(group.controls) && group.controls.some(hasDisableCondition)
          );

          if (hasDisableConditions) {

            const conditionSatisfied =
              evaluateConditionGroups(
                dep.groups,
                dep.operator,
                getConditionValue,
                "disable"
              );

            applyDisabledState(
              dependentElement,
              !conditionSatisfied,
              control,
              dep,
              dependentControlId,
              previousStates,
              ui
            );
          }

          // ---------------------------------------------------------
          // HIDE
          // ---------------------------------------------------------

          const hasHideConditions =
            dep.groups.some(
              group =>
                Array.isArray(group.controls) &&
                group.controls.some(
                  c =>
                    c.hideWhen ||
                    c.showWhen
                )
            );

          if (hasHideConditions) {

            const shouldHide =
              evaluateConditionGroups(
                dep.groups,
                dep.operator,
                getConditionValue,
                "hide"
              );

            applyHiddenState(
              dependentElement,
              shouldHide,
              control,
              dep,
              dependentControlId,
              previousStates,
              ui
            );
          }
        };

        // ---------------------------------------------------------
        // Registro centrale del refresher
        // ---------------------------------------------------------

        registerDependencyRefresher(
          element,
          updateDependentState
        );

        // Valutazione iniziale
        updateDependentState();

        // I master custom e i master Jamovi vengono aggiornati
        // attraverso il normale ciclo di update dell'opzione Jamovi.
        // Non aggiungere listener DOM paralleli:
        // ui[master].setValue() provoca già il refresh centrale.
      }

      // ===== GESTIONE DIPENDENZE MULTIPLE =====
      else if (dep.controls && Array.isArray(dep.controls)) {

        const masterElements = [];

        // Raccoglie tutti i master.
        //
        // Un master può essere:
        // 1. un controllo custom presente nel DOM del TabbedControl
        // 2. un'opzione Jamovi presente in ui ma NON generata
        //    dal TabbedControl (es. robDomains)
        dep.controls.forEach((conditionConfig) => {

          const masterControlId = conditionConfig.control;

          const masterElement =
            queryUIX(element, `#mz_${masterControlId}`);

          const masterUI =
            ui && ui[masterControlId];

          if (masterElement || masterUI) {

            masterElements.push({
              element: masterElement || null,
              ui: masterUI || null,
              config: conditionConfig
            });

          } else {

            console.warn(
              `Elemento master o opzione Jamovi non trovati: ${masterControlId}`
            );
          }
        });

        if (masterElements.length === 0) {
          console.warn(
            `Nessun master trovato per ${dependentControlId}`
          );
          return;
        }

        const updateDependentState = () => {

          const operator = dep.operator || "AND";

        // ---------------------------------------------------------
        // LETTURA E NORMALIZZAZIONE DEI VALORI MASTER
        // ---------------------------------------------------------
        
         const values = masterElements.map((master) => {
        
              let value = getMasterValue(
                  master.config.control,
                  master.element
              );
        
              if (
                  value === null ||
                  value === undefined
              ) {
                value = "";
              }
        
              return value;
          });

          // ---------------------------------------------------------
          // DISABLE
          // ---------------------------------------------------------
          //Riconoscitori dep
          if (dep.controls.some(hasDisableCondition)) {

            const conditions = masterElements.map(
              (master, idx) => {
                return evaluateSingleCondition(
                  values[idx],
                  master.config
                );
              }
            );

            let shouldDisable = false;

            if (operator === "AND") {

              // AND:
              // tutte le condizioni devono essere soddisfatte.
              //
              // evaluateSingleCondition() restituisce TRUE
              // quando quella singola condizione richiede DISABLE.
              //
              // Quindi basta UNA condizione non soddisfatta
              // per disabilitare il controllo.
              shouldDisable = conditions.some(
                condition => condition === true
              );

            } else if (operator === "OR") {

              // OR:
              // il controllo rimane abilitato se almeno una
              // condizione è soddisfatta.
              //
              // Si disabilita soltanto quando tutte richiedono
              // DISABLE.
              shouldDisable = conditions.every(
                condition => condition === true
              );
            }

            applyDisabledState(
              dependentElement,
              shouldDisable,
              control,
              dep,
              dependentControlId,
              previousStates,
              ui
            );
          }

          // ---------------------------------------------------------
          // HIDE
          // ---------------------------------------------------------

          if (
            dep.controls.some(
              c =>
                c.hideWhen ||
                c.showWhen
            )
          ) {

            const hideConditions =
              masterElements.map(
                (master, idx) => {
                  return evaluateHideCondition(
                    values[idx],
                    master.config
                  );
                }
              );

            let shouldHide = false;

            if (operator === "AND") {

              shouldHide = hideConditions.every(
                condition => condition === true
              );

            } else if (operator === "OR") {

              shouldHide = hideConditions.some(
                condition => condition === true
              );
            }

            applyHiddenState(
              dependentElement,
              shouldHide,
              control,
              dep,
              dependentControlId,
              previousStates,
              ui
            );
          }
        };

        // Registra la funzione nel registro centrale.
        //
        // Questo permette a main.js di rivalutare la dipendenza
        // quando cambia un'opzione Jamovi che non ha un proprio
        // elemento DOM nel TabbedControl.
        registerDependencyRefresher(
          element,
          updateDependentState
        );

        // Valutazione iniziale.
        updateDependentState();

        // Il refresh delle dipendenze viene effettuato
        // dal ciclo di update centrale di main.js.
        // Non registrare listener DOM paralleli sui master custom.
      }
    });
  });


  // ===== FUNZIONI HELPER =====================================

  // ================================================================
  // VALUTAZIONE DI UN GRUPPO LOGICO
  // ================================================================
  //
  // Valuta un gruppo composto da condizioni singole.
  //
  // Restituisce TRUE quando il gruppo risulta soddisfatto.
  //
  // Esempio:
  //
  // {
  //   controls: [
  //     { control: "A", valueIn: ["x"] },
  //     { control: "B", notEmpty: true }
  //   ],
  //   operator: "AND"
  // }
  //
  // equivale a:
  //
  //   A soddisfatto AND B soddisfatto
  //
  function evaluateConditionGroup(
    group,
    getConditionValue,
    mode
  ) {

    if (
      !group ||
      !Array.isArray(group.controls) ||
      group.controls.length === 0
    ) {
      return false;
    }

    const operator =
      String(group.operator || "AND").toUpperCase();

    const results = group.controls.map((condition) => {

      const value = getConditionValue(condition);

      return mode === "hide"
        ? evaluateHideCondition(value, condition)
        : !evaluateSingleCondition(value, condition);
    });

    if (operator === "OR") {
      return results.some(result => result === true);
    }

    // Default = AND
    return results.every(result => result === true);
  }

  // ================================================================
  // VALUTAZIONE DI GRUPPI LOGICI ANNIDATI
  // ================================================================
  //
  // groups rappresenta più gruppi indipendenti.
  //
  // Esempio:
  //
  // groups: [
  //   {
  //     controls: [A, B],
  //     operator: "AND"
  //   },
  //   {
  //     controls: [C, D],
  //     operator: "AND"
  //   }
  // ],
  // operator: "OR"
  //
  // equivale a:
  //
  //   (A AND B) OR (C AND D)
  //
  function evaluateConditionGroups(
      groups,
      operator,
      getConditionValue,
      mode
    ) {
    
      if (!Array.isArray(groups) || groups.length === 0) {
        return false;
      }
    
      const results = groups.map((group) => {
    
        let result = evaluateConditionGroup(
          group,
          getConditionValue,
          mode
        );
    
        // NOT applicato al risultato dell'intero gruppo.
        //
        // Esempio:
        //
        // {
        //   controls: [A, B],
        //   operator: "AND",
        //   not: true
        // }
        //
        // equivale a:
        //
        //   NOT (A AND B)
        //
        if (group && group.not === true) {
          result = !result;
        }
    
        return result;
      });
    
      const normalizedOperator =
        String(operator || "AND").toUpperCase();
    
      if (normalizedOperator === "OR") {
        return results.some(result => result === true);
      }
    
      // Default = AND
      return results.every(result => result === true);
    }

  // Ottiene il valore dal controllo master (locale o Jamovi UI)
  function getMasterValue(id, element) {
    // 1. Prova PRIMA come opzione standard di Jamovi (Variable Picker, etc.)
    // Questo ha la priorità per evitare di pescare elementi DOM ambigui (es. div con stesso ID)
    if (ui && ui[id]) {
      if (typeof ui[id].value === 'function') {
        return ui[id].value(); // Variable Picker
      }
      if (ui[id].value !== undefined) return ui[id].value;
      return ui[id];
    }

    // 2. Prova come controllo custom locale (DOM) con prefisso mz_
    let el = null;
    if (element) {
      el = queryUIX(element, `#mz_${id}`);
    } else {
      el = document.getElementById(`mz_${id}`);
    }

    if (el) {
      if (el.type === 'checkbox') return el.checked;
      if (el.type === 'radio') {
        const radioGroup = document.querySelectorAll(`input[name="${el.name}"]:checked`);
        return radioGroup.length > 0 ? radioGroup[0].value : null;
      }
      // Controlli custom a stato tri-valore (deviatore): il valore corrente
      // e' in data-state, non in .value (l'elemento e' un div, non un input).
      if (el.dataset && el.dataset.state !== undefined) {
        return el.dataset.state;
      }
      return el.value;
    }

    return null;
  }
 

// ============================================================================
// VALUTAZIONE DELLE SINGOLE CONDIZIONI
//
// Condizioni supportate:
//   - notEmpty
//   - disableWhen
//   - enableWhen
//   - valueIn
//   - valueNotIn
//   - greaterThan
//   - lessThan
//   - greaterOrEqual
//   - lessOrEqual
//   - equalTo
//   - notEqualTo
//   - contains
//   - notContains
//   - isTrue
//   - isFalse
//
// Condizioni strutturali sulle liste:
//   - itemCountEqualTo
//   - itemCountGreaterThan
//   - itemCountGreaterOrEqual
//   - itemCountLessThan
//   - itemCountLessOrEqual
//
// `not: true` viene applicato alla condizione complessiva e consente di
// ottenere anche le relative negazioni senza introdurre operatori ridondanti.
// Esempio: itemCountEqualTo + not -> numero di elementi diverso da N.
//
// Le condizioni itemCount* richiedono che il valore rimanga un Array, così da
// poter contare gli elementi effettivamente selezionati da controlli multi-value
// quali VariableSupplier, indipendentemente dai nomi delle variabili del dataset.
//
// Le condizioni stringa `contains` / `notContains` sono case-sensitive.
// ============================================================================
function evaluateSingleCondition(value, config) {

  let shouldDisable = false;

  // =========================================================
  // notEmpty
  // =========================================================
  if (config.notEmpty === true || config.notEmpty === false) {

    const isEmpty =
      Array.isArray(value)
        ? (
            value.length === 0 ||
            (
              value.length === 1 &&
              (
                value[0] === "" ||
                value[0] === null ||
                value[0] === undefined
              )
            )
          )
        : (
            value === "" ||
            value === null ||
            value === undefined
          );

    if (config.notEmpty === true) {
      // notEmpty: true
      // DISABLE quando il valore è vuoto.
      shouldDisable = isEmpty;
    } else {
      // notEmpty: false
      // DISABLE quando il valore NON è vuoto.
      shouldDisable = !isEmpty;
    }

  // =========================================================
  // disableWhen
  // =========================================================
  } else if (
    config.disableWhen &&
    Array.isArray(config.disableWhen)
  ) {

    if (Array.isArray(value)) {

      shouldDisable = value.some(
        item => config.disableWhen.includes(item)
      );

    } else {

      shouldDisable =
        config.disableWhen.includes(value);
    }

  // =========================================================
  // enableWhen
  // =========================================================
  } else if (
    config.enableWhen &&
    Array.isArray(config.enableWhen)
  ) {

    if (Array.isArray(value)) {

      shouldDisable = !value.some(
        item => config.enableWhen.includes(item)
      );

    } else {

      shouldDisable =
        !config.enableWhen.includes(value);
    }

  // =========================================================
  // valueIn
  // =========================================================
  } else if (
    config.valueIn &&
    Array.isArray(config.valueIn)
  ) {

    if (Array.isArray(value)) {

      shouldDisable = !value.some(
        item => config.valueIn.includes(item)
      );

    } else {

      shouldDisable =
        !config.valueIn.includes(value);
    }

  // =========================================================
  // valueNotIn
  // =========================================================
  } else if (
    config.valueNotIn &&
    Array.isArray(config.valueNotIn)
  ) {

    if (Array.isArray(value)) {

      shouldDisable = value.some(
        item => config.valueNotIn.includes(item)
      );

    } else {

      shouldDisable =
        config.valueNotIn.includes(value);
    }

  // =========================================================
  // greaterThan
  // =========================================================
  } else if (config.greaterThan !== undefined) {

    const numericValue = Number(value);
    const numericLimit = Number(config.greaterThan);

    shouldDisable =
      !Number.isFinite(numericValue) ||
      !Number.isFinite(numericLimit) ||
      numericValue <= numericLimit;

  // =========================================================
  // lessThan
  // =========================================================
  } else if (config.lessThan !== undefined) {

    const numericValue = Number(value);
    const numericLimit = Number(config.lessThan);

    shouldDisable =
      !Number.isFinite(numericValue) ||
      !Number.isFinite(numericLimit) ||
      numericValue >= numericLimit;

  // =========================================================
  // greaterOrEqual
  // =========================================================
  } else if (config.greaterOrEqual !== undefined) {

    const numericValue = Number(value);
    const numericLimit = Number(config.greaterOrEqual);

    shouldDisable =
      !Number.isFinite(numericValue) ||
      !Number.isFinite(numericLimit) ||
      numericValue < numericLimit;

  // =========================================================
  // lessOrEqual
  // =========================================================
  } else if (config.lessOrEqual !== undefined) {

    const numericValue = Number(value);
    const numericLimit = Number(config.lessOrEqual);

    shouldDisable =
      !Number.isFinite(numericValue) ||
      !Number.isFinite(numericLimit) ||
      numericValue > numericLimit;

  // =========================================================
  // equalTo
  // =========================================================
  } else if (config.equalTo !== undefined) {

    const numericValue = Number(value);
    const numericLimit = Number(config.equalTo);

    shouldDisable =
      !Number.isFinite(numericValue) ||
      !Number.isFinite(numericLimit) ||
      numericValue !== numericLimit;

  // =========================================================
  // notEqualTo
  // =========================================================
  } else if (config.notEqualTo !== undefined) {

    const numericValue = Number(value);
    const numericLimit = Number(config.notEqualTo);

    shouldDisable =
      !Number.isFinite(numericValue) ||
      !Number.isFinite(numericLimit) ||
      numericValue === numericLimit;

  // =========================================================
  // contains
  // =========================================================
  } else if (config.contains !== undefined) {

    const searchValue = String(config.contains);

    if (Array.isArray(value)) {

      shouldDisable = !value.some(
        item =>
          String(item).includes(searchValue)
      );

    } else {

      shouldDisable =
        !String(value ?? "").includes(searchValue);
    }

  // =========================================================
  // notContains
  // =========================================================
  } else if (config.notContains !== undefined) {

    const searchValue = String(config.notContains);

    if (Array.isArray(value)) {

      shouldDisable = value.some(
        item =>
          String(item).includes(searchValue)
      );

    } else {

      shouldDisable =
        String(value ?? "").includes(searchValue);
    }

  // =========================================================
  // isTrue
  // =========================================================
  } else if (config.isTrue === true) {

    shouldDisable = value !== true;

  // =========================================================
  // isFalse
  // =========================================================
  } else if (config.isFalse === true) {

    shouldDisable = value !== false;

  // =========================================================
  // itemCountEqualTo
  // =========================================================
  } else if (config.itemCountEqualTo !== undefined) {

    if (Array.isArray(value)) {

      const count = value.filter(
        item =>
          item !== "" &&
          item !== null &&
          item !== undefined
      ).length;

      const expectedCount =
        Number(config.itemCountEqualTo);

      shouldDisable =
        !Number.isFinite(expectedCount) ||
        count !== expectedCount;

    } else {

      shouldDisable = true;
    }

  // =========================================================
  // itemCountGreaterThan
  // =========================================================
  } else if (config.itemCountGreaterThan !== undefined) {

    if (Array.isArray(value)) {

      const count = value.filter(
        item =>
          item !== "" &&
          item !== null &&
          item !== undefined
      ).length;

      const expectedCount =
        Number(config.itemCountGreaterThan);

      shouldDisable =
        !Number.isFinite(expectedCount) ||
        count <= expectedCount;

    } else {

      shouldDisable = true;
    }

  // =========================================================
  // itemCountGreaterOrEqual
  // =========================================================
  } else if (config.itemCountGreaterOrEqual !== undefined) {

    if (Array.isArray(value)) {

      const count = value.filter(
        item =>
          item !== "" &&
          item !== null &&
          item !== undefined
      ).length;

      const expectedCount =
        Number(config.itemCountGreaterOrEqual);

      shouldDisable =
        !Number.isFinite(expectedCount) ||
        count < expectedCount;

    } else {

      shouldDisable = true;
    }

  // =========================================================
  // itemCountLessThan
  // =========================================================
  } else if (config.itemCountLessThan !== undefined) {

    if (Array.isArray(value)) {

      const count = value.filter(
        item =>
          item !== "" &&
          item !== null &&
          item !== undefined
      ).length;

      const expectedCount =
        Number(config.itemCountLessThan);

      shouldDisable =
        !Number.isFinite(expectedCount) ||
        count >= expectedCount;

    } else {

      shouldDisable = true;
    }

  // =========================================================
  // itemCountLessOrEqual
  // =========================================================
  } else if (config.itemCountLessOrEqual !== undefined) {

    if (Array.isArray(value)) {

      const count = value.filter(
        item =>
          item !== "" &&
          item !== null &&
          item !== undefined
      ).length;

      const expectedCount =
        Number(config.itemCountLessOrEqual);

      shouldDisable =
        !Number.isFinite(expectedCount) ||
        count > expectedCount;

    } else {

      shouldDisable = true;
    }
  }

  // =========================================================
  // NOT
  // =========================================================
  if (config.not === true) {
    shouldDisable = !shouldDisable;
  }

  return shouldDisable;
}

  // Valuta una singola condizione per HIDE (NUOVA FUNZIONE)
  function evaluateHideCondition(value, config) {
    let shouldHide = false;

    if (config.hideWhen && Array.isArray(config.hideWhen)) {
      shouldHide = config.hideWhen.includes(value);
    } else if (config.showWhen && Array.isArray(config.showWhen)) {
      shouldHide = !config.showWhen.includes(value);
    }

    return shouldHide;
  }

    // Applica lo stato disabled al controllo
    function applyDisabledState(
    dependentElement,
    shouldDisable,
    control,
    dep,
    dependentControlId,
    previousStates,
    ui) {

    // Salva lo stato checked solo sulla vera transizione
    // enabled -> disabled.
    if (
        shouldDisable &&
        !dependentElement.disabled &&
        dep.resetOnEnable !== false
    ) {
        previousStates[dependentControlId] =
            dependentElement.checked;
    }

    // Applica lo stato disabled.
    dependentElement.disabled = shouldDisable;

    // Gestione visiva per segmentedControl.
    const segWrapper =
        dependentElement.closest('.mz-seg-wrapper');

    if (segWrapper) {
        segWrapper.classList.toggle(
            'disabled',
            shouldDisable
        );

        segWrapper.querySelectorAll('.mz-seg-btn')
            .forEach(btn => {
                btn.disabled = shouldDisable;
                btn.style.pointerEvents =
                    shouldDisable ? 'none' : '';
            });
    }

    // Gestione visiva per priorityPicker.
    const ppWrapper =
        dependentElement.closest('.mz-pp-wrapper');

    if (ppWrapper) {
        ppWrapper.classList.toggle(
            'disabled',
            shouldDisable
        );
    }

    // Gestione visiva per verticalListSlider.
    const vlsWrapper =
        dependentElement.closest('.vls-wrapper');

    if (vlsWrapper) {
        vlsWrapper.classList.toggle(
            'disabled',
            shouldDisable
        );
    }

    // Gestione visiva per toggle switch.
    const toggleContainer =
        dependentElement.closest('.toggle-switch');

    if (toggleContainer) {
        toggleContainer.style.opacity =
            shouldDisable ? '0.4' : '1';

        toggleContainer.style.cursor =
            shouldDisable ? 'not-allowed' : 'pointer';
    }

    // Gestione visiva per label.
    const checkboxLabel =
        dependentElement.closest(
            '.checkbox-label, .toggle-label'
        );

    if (checkboxLabel) {
        checkboxLabel.style.opacity =
            shouldDisable ? '0.6' : '1';

        checkboxLabel.style.cursor =
            shouldDisable ? 'not-allowed' : 'pointer';
    }

    // ---------------------------------------------------------
    // RESET SU DISABILITAZIONE
    // ---------------------------------------------------------
    //
    // Scrive nell'opzione SOLO se il valore dell'opzione
    // è realmente diverso.
    //
    if (
        shouldDisable &&
        control.type === "checkbox" &&
        dep.resetOnEnable === false
    ) {
        dependentElement.checked = false;

        if (
            ui &&
            ui[control.option] &&
            ui[control.option].value() !== false
        ) {
            ui[control.option].setValue(false);
        }
    }

    // ---------------------------------------------------------
    // RIPRISTINO SU RIABILITAZIONE
    // ---------------------------------------------------------
    if (
        !shouldDisable &&
        dep.resetOnEnable !== false &&
        previousStates[dependentControlId] !== undefined
    ) {
        const previousValue =
            previousStates[dependentControlId];

        delete previousStates[dependentControlId];

        dependentElement.checked = previousValue;

        if (
            ui &&
            ui[control.option] &&
            ui[control.option].value() !== previousValue
        ) {
            ui[control.option].setValue(previousValue);
        }
    }
}

  // Applica lo stato hidden al controllo (NUOVA FUNZIONE)
  function applyHiddenState(
    dependentElement,
    shouldHide,
    control,
    dep,
    dependentControlId,
    previousStates,
    ui
) {
    const controlContainer = control.type === "label"
        ? dependentElement
        : dependentElement.closest(
            '.control-item, .control-item-compact'
        );

    if (!controlContainer) {
        console.warn(
            `Contenitore controllo non trovato per ${dependentControlId}`
        );
        return;
    }

    if (shouldHide) {

        controlContainer.style.setProperty(
            'display',
            'none',
            'important'
        );

        if (
            dep.resetOnEnable !== false &&
            control.type === "checkbox"
        ) {
            previousStates[
                dependentControlId + '_hidden'
            ] = dependentElement.checked;
        }

        if (
            dep.resetOnHide === true &&
            control.type === "checkbox" &&
            dependentElement.checked
        ) {
            dependentElement.checked = false;

            if (
                ui &&
                ui[control.option] &&
                ui[control.option].value() !== false
            ) {
                ui[control.option].setValue(false);
            }
        }

    } else {

        controlContainer.style.setProperty(
            'display',
            '',
            'important'
        );

        if (
            window.getComputedStyle(controlContainer).display === 'none'
        ) {
            controlContainer.style.setProperty(
                'display',
                'block',
                'important'
            );
        }

        if (
            dep.resetOnEnable !== false &&
            control.type === "checkbox"
        ) {
            const savedState =
                previousStates[
                    dependentControlId + '_hidden'
                ];

            if (savedState !== undefined) {

                delete previousStates[
                    dependentControlId + '_hidden'
                ];

                dependentElement.checked = savedState;

                if (
                    ui &&
                    ui[control.option] &&
                    ui[control.option].value() !== savedState
                ) {
                    ui[control.option].setValue(savedState);
                }
            }
        }
    }
}
}

// ================================================================
// GESTIONE ACCORDION GROUPS - GRUPPI COLLASSABILI
// ================================================================
// Setup eventi click per espandere/collassare gruppi accordion

function setupAccordionGroups(element, ui, tabConfig) {
  tabConfig.forEach((tab) => {
    if (!tab.groups) return;

    tab.groups.forEach((group) => {
      if (!group.collapsible) return;

      const accordionKey = `accordion-${tab.id}-${group.position.row}-${group.position.col}`;

      const accordionHeader = queryUIX(element, `[data-accordion="${accordionKey}"]`);
      const accordionContent = queryUIX(element, `[data-accordion-content="${accordionKey}"]`);

      if (!accordionHeader || !accordionContent) {
        console.warn(`Elementi accordion non trovati per la chiave: ${accordionKey}`);
        return;
      }

      // Gestore del clic per l'espansione e la chiusura
      addEventListener(accordionHeader, 'click', function () {
        const icon = this.querySelector('.accordion-icon');
        const isExpanded = this.classList.contains('expanded');

        if (isExpanded) {
          // Collassa
          this.classList.remove('expanded');
          accordionContent.classList.remove('expanded');
          icon.classList.remove('expanded');
        } else {
          // Espandi
          this.classList.add('expanded');
          accordionContent.classList.add('expanded');
          icon.classList.add('expanded');
        }
      });
    });
  });
}

// ================================================================
// INIZIALIZZAZIONE VALORI CONTROLLI - SYNC BACKEND -> UI
// ================================================================
// Imposta i valori iniziali dei controlli dai dati backend
// (3) Funzione per aggiornamenti e nuovi elementi
function initializeControlValues(element, ui, tabConfig) {
  tabConfig.forEach((tab) => {
    const allTabControls = getAllControls(tab); // Prende standard + sub-tab
    allTabControls.forEach((control) => {
      try {
        switch (control.type) {

          // Inizializza checkbox
          case "checkbox":
            const controlElement = queryUIX(element, `#mz_${control.id}`);
            if (controlElement && ui[control.option]) {
              controlElement.checked = ui[control.option].value();
            }
            break;

          // Inizializza output checkbox
          case "output":
            const outputControlElement = queryUIX(element, `#mz_${control.id}`);
            if (outputControlElement && ui[control.option]) {
              // Il setup legge lo stato salvato; non resetta le colonne degli Output.
              const currentData = ui[control.option].value();
              outputControlElement.checked = !!(currentData && currentData.value);
            }
            break;

          // Inizializza input numerici
          case "number":
            const numberControlElement = queryUIX(element, `#mz_${control.id}`);
            if (numberControlElement) {
              const currentValue = ui[control.option].value();
              const lang = element.dataset.lang || 'en-US';
              numberControlElement.value = formatNumber(currentValue, lang);
              if (currentValue !== null && currentValue !== undefined)
                numberControlElement.setAttribute('aria-valuenow', String(currentValue));
              else numberControlElement.removeAttribute('aria-valuenow');

              // Inizializza slider se esiste
              const sliderControlElement = queryUIX(element, `#mz_${control.id}_slider`);
              if (sliderControlElement) {
                sliderControlElement.value = currentValue;
              }
            }
            break;

          // Inizializza select
          case "select":
            const selectControlElement = queryUIX(element, `#mz_${control.id}`);
            if (selectControlElement) {
              const storedValue = ui[control.option].value();
              // Alias dichiarati dal controllo: legge vecchi valori senza alterare le opzioni.
              const aliases = control.valueAliases;
              selectControlElement.value = aliases && Object.prototype.hasOwnProperty.call(aliases, storedValue)
                ? aliases[storedValue] : storedValue;
            }
            break;

          // Inizializza input testo
          case "string":
            const stringControlElement = queryUIX(element, `#mz_${control.id}`);
            if (stringControlElement) {
              stringControlElement.value = ui[control.option].value() || "";
            }
            break;

          // Inizializza radio button
          case "radio":
            const currentRadioValue = ui[control.option].value();
            const radioElement = queryUIX(element, 
              `input[name="${control.option}"][value="${currentRadioValue}"]`
            );
            if (radioElement) {
              radioElement.checked = true;
            }
            break;

          // Inizializza plotBlock
          case "plotBlock":
            // Usa nomi opzione espliciti
            const showControlId = control.showOption;
            const widthControlId = control.widthOption;
            const heightControlId = control.heightOption;
            const langPlot = element.dataset.lang || 'en-US';

            const showElement = queryUIX(element, `#mz_${showControlId}`);
            if (showElement && ui[showControlId]) {
              showElement.checked = ui[showControlId].value();
            }

            if (ui[widthControlId] && ui[heightControlId]) {
              const currentWidthValue = ui[widthControlId].value();
              const currentHeightValue = ui[heightControlId].value();

              const widthElement = queryUIX(element, `#mz_${widthControlId}`);
              if (widthElement) {
                widthElement.value = currentWidthValue;
              }

              const heightElement = queryUIX(element, `#mz_${heightControlId}`);
              if (heightElement) {
                heightElement.value = currentHeightValue;
              }

              // Inizializza slider se showSlider è abilitato
              if (control.showSlider) {
                const widthSliderControlElement = queryUIX(element, 
                  `#mz_${widthControlId}_slider`
                );
                if (widthSliderControlElement) {
                  widthSliderControlElement.value = currentWidthValue;
                }

                const heightSliderControlElement = queryUIX(element, 
                  `#mz_${heightControlId}_slider`
                );
                if (heightSliderControlElement) {
                  heightSliderControlElement.value = currentHeightValue;
                }
              }
            }
            break;

         // Inizializza deviatore
         case "deviatore":
           const devInitEl = queryUIX(element, `#mz_${control.id}`);
           if (devInitEl && ui[control.option]) {
             const saved = ui[control.option].value();
             const valid = ["posOff", "posLorT", "posRorB"];
             const mapped = control.stateValues && Object.keys(control.stateValues)
               .find(state => control.stateValues[state] === saved);
             const initialState = mapped || (valid.includes(saved) ? saved
               : control.allowOff === false ? 'posLorT' : 'posOff');

             devInitEl.dataset.state = initialState;

            const devWrapper = queryUIX(element, `#mz_${control.id}_wrapper`);
            if (devWrapper) {
                const stateLabels = devWrapper.querySelector(
                    '.deviatore-state-labels, .deviatore-line-state-labels'
                );            
                if (stateLabels) {
                    stateLabels.dataset.state = initialState;
                }
            }
           }
           break;

          // Inizializza verticalListSlider
          case "verticalListSlider": {
            const vlsRow = queryUIX(element, `#mz_${control.id}`);
            if (!vlsRow) break;

            const opts = control.options || [];
            let savedValue = "";
            try { savedValue = ui[control.option] ? ui[control.option].value() : ""; } catch(e) {}

            const idx        = opts.findIndex(o => o.value === savedValue);
            const clampedIdx = idx >= 0 ? idx : 0;
            const itemGap    = parseInt(vlsRow.dataset.itemGap, 10) || 22;
            const activeColor = vlsRow.dataset.activeColor || '#3e6da9';
            const fillColor   = vlsRow.dataset.fillColor   || '#3e6da9';

            const centerPx = clampedIdx * itemGap + itemGap / 2;
            const fillH    = clampedIdx * itemGap;
            const trackTop = itemGap / 2;

            const thumbEl  = queryUIX(element, `#mz_${control.id}_thumb`);
            const fillEl   = queryUIX(element, `#mz_${control.id}_fill`);
            const labelEls = vlsRow.querySelectorAll('.vls-label-item');
            const vlsHval  = queryUIX(element, `#mz_${control.id}_hval`);

            if (thumbEl) thumbEl.style.top = `${centerPx}px`;
            if (fillEl) {
                fillEl.style.top    = `${trackTop}px`;
                fillEl.style.height = `${fillH}px`;
            }

            labelEls.forEach((lbl, i) => {
                lbl.classList.toggle('active', i === clampedIdx);
                lbl.style.color = i === clampedIdx ? activeColor : '';
            });

            if (vlsHval && opts[clampedIdx]) vlsHval.textContent = opts[clampedIdx].text;

            break;
          }

          
          // Inizializza segmentedControl
          case "segmentedControl": {
              const scTrack = queryUIX(element, `#mz_${control.id}`);
              if (!scTrack) break;
              let saved = "";
              try { saved = ui[control.option] ? ui[control.option].value() : ""; } catch(e) {}
              const activeColor = scTrack.dataset.activeColor || "#3e6da9";
              scTrack.querySelectorAll('.mz-seg-btn').forEach(btn => {
                const isActive = btn.dataset.value === saved;
                btn.classList.toggle('active', isActive);
                btn.setAttribute('aria-checked', String(isActive));
                btn.tabIndex = isActive ? 0 : -1;
                btn.style.color = isActive ? activeColor : (btn.dataset.optColor || '');
              });
              break;
            }
            
          // Inizializza priorityPicker
          case "priorityPicker": {
              const ppRow = queryUIX(element, `#mz_${control.id}`);
              if (!ppRow) break;
              let saved = "";
              try { saved = ui[control.option] ? ui[control.option].value() : ""; } catch(e) {}
              const opts       = control.options  || [];
              const fillColor  = ppRow.dataset.fillColor  || "#3e6da9";
              const emptyColor = ppRow.dataset.emptyColor || "#dddddd";
              const idx        = opts.findIndex(o => o.value === saved);
              const clampedIdx = idx >= 0 ? idx : 0;
              const lbl        = queryUIX(element, `#mz_${control.id}_lbl`);
            
              ppRow.querySelectorAll('.mz-pp-seg').forEach((seg, i) => {
                seg.style.background = i <= clampedIdx ? fillColor : emptyColor;
              });
              if (lbl && opts[clampedIdx]) lbl.textContent = opts[clampedIdx].text;
              break;
            }
            
          // Inizializza conditionalReveal
          case "conditionalReveal": {
              const crToggle = queryUIX(element, `#mz_${control.id}_toggle`);
              const crReveal = queryUIX(element, `#mz_${control.id}_reveal`);
              if (!crToggle) break;
              let saved = false;
              try { saved = ui[control.option] ? ui[control.option].value() : false; } catch(e) {}
              crToggle.classList.toggle('on', !!saved);
              crReveal.classList.toggle('open', !!saved);
            
              // I figli vengono inizializzati una sola volta dalla raccolta piatta.
              break;
            }
            
          // Inizializza linkedSlider
          case "linkedSlider": {
              const lsBody = queryUIX(element, `#mz_${control.id}`);
              if (!lsBody) break;
              let savedLo = control.defaultLow ?? 0;
              let savedHi = control.defaultHigh ?? 0;
              try {
                if (ui[control.optionLow])  savedLo = ui[control.optionLow].value();
                if (ui[control.optionHigh]) savedHi = ui[control.optionHigh].value();
              } catch(e) {}
            
              const mn       = parseFloat(lsBody.dataset.min);
              const mx       = parseFloat(lsBody.dataset.max);
              const dec      = parseInt(lsBody.dataset.decimals, 10) ?? 2;
              const fillColor = lsBody.dataset.fillColor || "#3e6da9";
              const unit     = lsBody.dataset.unit || "";
            
              function lsPctInit(v) { return Math.max(0, Math.min(100, (v - mn) / (mx - mn) * 100)); }
              function lsFmtInit(v) { return (v >= 0 ? '+' : '') + parseFloat(v).toFixed(dec).replace('.', getDecimalSeparator()) + unit; }
            
              const loPct = lsPctInit(savedLo);
              const hiPct = lsPctInit(savedHi);
              const isErr = savedLo >= savedHi;
              const col   = isErr ? '#c0392b' : fillColor;
              const midX  = (loPct + hiPct) / 2;
            
              const loThumb = queryUIX(element, `#mz_${control.id}_lo_thumb`);
              const hiThumb = queryUIX(element, `#mz_${control.id}_hi_thumb`);
              const loFill  = queryUIX(element, `#mz_${control.id}_lo_fill`);
              const hiFill  = queryUIX(element, `#mz_${control.id}_hi_fill`);
              const loVal   = queryUIX(element, `#mz_${control.id}_lo_val`);
              const hiVal   = queryUIX(element, `#mz_${control.id}_hi_val`);
              const errMsg  = queryUIX(element, `#mz_${control.id}_err`);
              const linkIcon = queryUIX(element, `#mz_${control.id}_link_icon`);
            
              const vertical = lsBody.dataset.orientation === 'vertical';
              if (vertical) {
                if (loThumb) loThumb.style.bottom = loPct + '%';
                if (hiThumb) hiThumb.style.bottom = hiPct + '%';
                if (loFill) { loFill.style.height = loPct + '%'; loFill.style.background = col; }
                if (hiFill) { hiFill.style.height = hiPct + '%'; hiFill.style.background = col; }
                if (linkIcon) linkIcon.style.marginTop = `calc(${100 - midX}% - 8px)`;
              } else {
                if (loThumb) loThumb.style.left = loPct + '%';
                if (hiThumb) hiThumb.style.left = hiPct + '%';
                if (loFill) { loFill.style.width = loPct + '%'; loFill.style.background = col; }
                if (hiFill) { hiFill.style.width = hiPct + '%'; hiFill.style.background = col; }
                if (linkIcon) linkIcon.style.marginLeft = `calc(${midX}% - 8px)`;
              }
              if (loVal)   loVal.textContent = lsFmtInit(savedLo);
              if (hiVal)   hiVal.textContent = lsFmtInit(savedHi);
              if (errMsg)  errMsg.classList.toggle('show', isErr);
            
              lsBody.dataset.lo = savedLo;
              lsBody.dataset.hi = savedHi;
              break;
            }

          // Inizializza actionButton
          case "actionButton":
            const actionBtnElement = queryUIX(element, `#mz_${control.id}`);
            const actionOptName = control.actionOption;

            if (actionBtnElement && ui[actionOptName]) {
              // Action buttons non necessitano inizializzazione valore
              // Ma applichiamo lo stato disabled se specificato nella config
              if (control.disabled) {
                actionBtnElement.disabled = true;
              }
            }
            break;
        }
      } catch (e) {
        // Log warning se controllo non può essere inizializzato
        console.warn(`Could not initialize control ${control.id}: `, e);
      }
    });
  });
}

// ================================================================
// UTILITY: NORMALIZZAZIONE ALLINEAMENTO (3x3)
// ================================================================
// Converte formati misti (es. "left-top", "center") nello standard "vAlign-hAlign" (es. "top-left")

function normalizeAlignment(align) {
  if (!align || typeof align !== 'string') return "top-left";

  const parts = align.toLowerCase().split('-');
  let v = "top";
  let h = "left";

  const verticalOptions = ["top", "center", "bottom"];
  const horizontalOptions = ["left", "center", "right"];

  if (parts.length === 1) {
    const val = parts[0];
    if (verticalOptions.includes(val) && horizontalOptions.includes(val)) {
      // "center" è sia v che h
      v = "center";
      h = "center";
    } else if (verticalOptions.includes(val)) {
      v = val;
      h = "center"; // Default se solo verticale (es. "top" -> "top-center")
    } else if (horizontalOptions.includes(val)) {
      h = val;
      v = "top";    // Default se solo orizzontale (es. "left" -> "top-left")
    }
  } else if (parts.length >= 2) {
    const p1 = parts[0];
    const p2 = parts[1];

    // Se il primo è orizzontale e il secondo verticale, inverti (es. "left-top" -> "top-left")
    if (horizontalOptions.includes(p1) && verticalOptions.includes(p2) && p1 !== "center") {
      v = p2;
      h = p1;
    } else {
      v = verticalOptions.includes(p1) ? p1 : "top";
      h = horizontalOptions.includes(p2) ? p2 : (horizontalOptions.includes(p1) && parts.length === 2 ? "center" : "left");

      // Correzione per casi ambigui
      if (verticalOptions.includes(p1) && !horizontalOptions.includes(p2) && parts.length === 2) {
        if (horizontalOptions.includes(p1)) { /* already handled */ }
        else h = "center";
      }
    }
  }

  return `${v}-${h}`;
}

// ================================================================
// ESPORTAZIONI
// ================================================================

// Esporta per accesso globale (browser)
if (typeof window !== "undefined") {
  window.setupTabbedControl = setupTabbedControl;
  window.disposeTabbedControl = disposeTabbedControl;
  window.refreshControlDependencies = refreshControlDependencies;
}

// Esporta per sistemi di moduli (Node.js/CommonJS)
if (typeof module !== "undefined" && module.exports) {
  module.exports = {
    setupTabbedControl: setupTabbedControl,
    disposeTabbedControl: disposeTabbedControl,
    refreshControlDependencies: refreshControlDependencies
  };
}
