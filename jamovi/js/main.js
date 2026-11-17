'use strict';
const { migrateLeaveOneOutControls, notifyLeaveOneOutCommonEffect, bindLeaveOneOutCommonEffect } = require('./mzLeaveOneOutControls');

//console.log("[Metafora/main] Inizializzazione dello script main.js...");

// importa CSS/utility
try { require('./mzUIXcss'); } catch (e) { /* opzionale */ }

let showToast;
const { readToastPayload } = require('./mzMessageBridge');
try { 
    showToast = require('./mzToast').showToast; 
    //console.log("[Metafora/main] Modulo mzToast caricato con successo.");
} catch (e) { }



if (!window.__mzToastListenerAttached) {
    window.__mzToastListenerAttached = true;

    try {
        window.addEventListener('message', function(event) {
            const toasts = readToastPayload(event);
            if (toasts === null) return;
            
            if (typeof showToast === 'function') {
                toasts.forEach(t => {
                    showToast(
                        t.message,
                        t.type,
                        t.duration,
                        t.position,
                        t.theme,
                        t.title
                    );
                });
            }
        });
        //console.log("[Metafora/main] In ascolto globale per postMessage...");
    } catch(e) {
        console.warn("[Metafora/main] Errore nell'inizializzazione del listener dei messaggi:", e);
    }
} else {
    //console.log("[Metafora/main] Listener postMessage già presente su questa window, skip registrazione.");
}

// -----------------------------------------------------------------------------

const TAB_CONFIGS = require('./mzTabConfigs');
const { notifyPredictiveProbability, bindPredictiveProbabilityNotice } = require('./mzPredictiveProbabilityControls');
const {
    setupTabbedControl,
    refreshControlDependencies
} = require('./mzUIX');
const { enableLoggingIndicator } = require('./mzDebug');
const { refreshCumulativeVariables } = require('./mzCumulativeUI');
const { refreshEffectSizeInputs } = require('./mzEffectSizeUI');

// Variabili required per modello (tabbedControlModelType -> nomi opzione
// VariablesListBox dentro il VariableSupplier). DEVE restare sincronizzata
// con private$.requiredVars duplicata in ciascun *.b.R - nessuna fonte
// condivisa raggiungibile da JS lato client, duplicazione qui e' inevitabile.
const REQUIRED_VARS_BY_MODEL = {
    correlation: ['slab', 'ri', 'ni'],
    alpha:       ['slab', 'ai', 'mi', 'ni'],
    dichotomous: ['slab', 'ai', 'bi', 'ci', 'di'],
    meandiff:    ['slab', 'm1i', 'sd1i', 'n1i', 'm2i', 'sd2i', 'n2i'],
    proportion:  ['slab', 'xi', 'ni'],
    effectsize:  ['slab', 'yi', 'vi', 'ciLower', 'ciUpper'],
};

// Variabili optional comuni a tutti e sei i modelli (stesso nome opzione ovunque).
// robDomains esiste in dichotomous, meandiff ed effectsize - il
// guard "field && field.el" in styleVariableSupplier() lo rende sicuro ovunque.
const OPTIONAL_VARS = ['subgroup', 'moderator', 'robDomains'];

// ─────────────────────────────────────────────────────────────────────────────
// FUNZIONI DI SUPPORTO
// ─────────────────────────────────────────────────────────────────────────────
function hasValue(val) {
    if (Array.isArray(val)) return val.length > 0 && val[0] !== '';
    return !!val;
}

// Applica le classi mz-vs-* (css.js) a un target box VariablesListBox in
// base a categoria (required/optional) e stato compilato/vuoto.
function applyVarSupplierClasses(el, category, filled) {
    if (!el) return;
    const other = category === 'mz-vs-required' ? 'mz-vs-optional' : 'mz-vs-required';
    el.classList.add('mz-vs-target', category);
    el.classList.remove(other);
    el.classList.toggle('mz-vs-filled', filled);
    el.classList.toggle('mz-vs-empty', !filled);
}

// Styling del VariableSupplier nativo (lista sorgente, bottone di
// trasferimento, target box required/optional) - generalizzato sui sei
// modelli via tabbedControlModelType, nessun elemento hardcoded.
function styleVariableSupplier(ui) {
    const model = ui.tabbedControlModelType ? ui.tabbedControlModelType.value() : null;
    const requiredVars = (model && REQUIRED_VARS_BY_MODEL[model]) || [];

    requiredVars.forEach(name => {
        const field = ui[name];
        if (field && field.el) {
            applyVarSupplierClasses(field.el, 'mz-vs-required', hasValue(field.value()));
        }
    });

    OPTIONAL_VARS.forEach(name => {
        const field = ui[name];
        if (field && field.el) {
            applyVarSupplierClasses(field.el, 'mz-vs-optional', hasValue(field.value()));
            if (name === 'moderator') field.el.classList.add('mz-vs-moderator');
            if (name === 'robDomains') field.el.classList.add('mz-vs-rob');
        }
    });

    // Lista sorgente e bottone di trasferimento sono unici per l'intero
    // VariableSupplier (condivisi da tutti i target box) - styling idempotente.
    const sourceList = document.querySelector('jmv-selectgrid.silky-variable-supplier');
    if (sourceList) sourceList.classList.add('mz-vs-source');

    // Un bottone freccia distinto per ciascun target box
    document.querySelectorAll('button.jmv-variable-transfer').forEach(btn => {
        btn.classList.add('mz-vs-transfer-btn');
    });
}

// ─────────────────────────────────────────────────────────────────────────────
// ESPORTAZIONE EVENTI JAMOVI
// ─────────────────────────────────────────────────────────────────────────────
module.exports = {

    loaded: function (ui, event) {
        migrateLeaveOneOutControls(ui);
        notifyLeaveOneOutCommonEffect(ui, showToast, true);
        notifyPredictiveProbability(ui, showToast, true);
        bindLeaveOneOutCommonEffect(ui, showToast);
        //console.log("[Metafora/main] Evento 'loaded' innescato da jamovi.");

        enableLoggingIndicator(ui);
        refreshCumulativeVariables(ui);
        refreshEffectSizeInputs(ui);
        refreshControlDependencies(ui);
        styleVariableSupplier(ui);
    },

    update: function (ui, event) {
        migrateLeaveOneOutControls(ui);
        notifyLeaveOneOutCommonEffect(ui, showToast);
        notifyPredictiveProbability(ui, showToast);
        refreshCumulativeVariables(ui);
        refreshEffectSizeInputs(ui);
    
        refreshControlDependencies(ui);
        styleVariableSupplier(ui);
    
    },

    tabbedControl_creating: function (ui, event) {
        migrateLeaveOneOutControls(ui);
        notifyLeaveOneOutCommonEffect(ui, showToast, true);
        notifyPredictiveProbability(ui, showToast, true);
        //console.log("[Metafora/main] Evento 'tabbedControl_creating' innescato.");
        
        const modelCtrl = ui['tabbedControlModelType'];
        if (!modelCtrl) {
            console.warn('[Metafora/main] modelType non trovato durante tabbedControl_creating');
            return;
        }

        const model = modelCtrl.value().toLowerCase();
        //console.log(`[Metafora/main] Inizializzazione tab per il modello: "${model}"`);
        
        const sourceConfig = TAB_CONFIGS[model];
        // Traduce i testi delle note con il catalogo del modulo, senza mutare la configurazione condivisa.
        const translate = text => ui.tabbedControl.translate(text);
        const config = sourceConfig && sourceConfig.map(tab => ({
            ...tab,
            infoContent: TAB_CONFIGS.renderInfoNotes(tab.infoSections, translate)
        }));

        if (config) {
            setupTabbedControl(ui, config);
            bindPredictiveProbabilityNotice(ui, showToast);
            bindLeaveOneOutCommonEffect(ui, showToast);
            refreshCumulativeVariables(ui);
            refreshEffectSizeInputs(ui);
            refreshControlDependencies(ui);
        } else {
            console.error(`[Metafora/main] Errore: nessuna config trovata per modello "${model}"`);
        }
    },

    onChange_subgroup: function (ui, event) {
    
        refreshControlDependencies(ui);
        styleVariableSupplier(ui);
    
    },

    onChange_moderator: function (ui, event) {
        //console.log("[Metafora/main] Evento 'onChange_moderator' catturato. Valore:", ui.moderator?.value());

        // Il supplier nativo e' esterno al tab custom: notifica al cambio effettivo.
        notifyPredictiveProbability(ui, showToast);
        refreshControlDependencies(ui);
        styleVariableSupplier(ui);
    },
    
    onChange_robDomains: function (ui, event) {
        //console.log("[Metafora/main] Evento 'onChange_moderrobDomains' catturato. Valore:", ui.robDomains?.value());

        refreshControlDependencies(ui);
        styleVariableSupplier(ui);
    },
};
