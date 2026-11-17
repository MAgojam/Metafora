'use strict';

// Visibilita dei soli input alternativi di Generic ES; nessuna modifica ai valori.
// Il label di vi ospita il selettore UIX e rimane visibile anche in modalita CI.
const INSTANCES = new WeakMap();
const _ = text => text;

function element(control) {
    const el = control && (control.el || control.$el);
    return el && (el[0] || el);
}

function showCell(el, show) {
    const cell = el && el.closest('.layout-cell');
    if (cell && typeof cell.setVisibility === 'function') cell.setVisibility(show, true);
}

function showTarget(field, show, keepLabel=false) {
    if (!field) return;
    showCell(element(field), show);
    // TargetLayoutBox gestisce etichetta e pulsante in celle native distinte.
    for (let parent = field._parentControl; parent; parent = parent._parentControl) {
        if (parent.label && parent.buttons) {
            // I limiti sono descritti nei ghost text. Conserva il label nel DOM
            // per aria-labelledby, senza una riga di intestazione visibile.
            if (!keepLabel) showCell(element({ el: parent.label }), false);
            showCell(element({ el: parent.buttons }), show);
            break;
        }
    }
}

function mountInputLevel(ui, state, ci) {
    const target = element(ui.vi);
    const ids = target && target.getAttribute('aria-labelledby');
    const doc = target && target.ownerDocument;
    const label = ids && ids.split(/\s+/).map(id => doc.getElementById(id)).find(Boolean);
    const track = label && label.querySelector('#mz_meaType');
    const header = track && track.closest('.mz-native-label-control');
    const field = ui.inputCiLevel;
    if (!header || !field || !field.input) return;
    header.classList.add('mz-effectsize-input-header');
    if (!state.inline) {
        const inline = doc.createElement('span');
        inline.className = 'mz-effectsize-ci-level';
        const caption = doc.createElement('label');
        caption.textContent = ui.tabbedControl.translate(_('Input:'));
        caption.title = ui.tabbedControl.translate(_('Input CI (%)'));
        caption.htmlFor = field.input.id;
        const originalGrid = field.input.closest('.silky-layout-grid');
        const moved = [field.input, field.suffix].filter(Boolean).map(node => {
            const anchor = doc.createComment('mz input CI original position');
            node.before(anchor);
            return { node, anchor };
        });
        inline.append(caption, ...moved.map(item => item.node));
        header.append(inline);
        state.inline = { element: inline, header, originalGrid, moved };
    }
    state.inline.element.hidden = !ci;
    showCell(state.inline.originalGrid, false);
    showCell(element({ el: field.label }), false);
}

function refreshEffectSizeInputs(ui) {
    const owner = ui && ui.tabbedControl;
    if (!owner || !ui.meaType || !ui.ciLower || !ui.ciUpper) return;
    let state = INSTANCES.get(owner);
    if (!state) {
        state = { disposed: false };
        INSTANCES.set(owner, state);
        const refresh = () => refreshEffectSizeInputs(ui);
        // CustomControl.update riguarda inizializzazione/metadati, non ogni
        // modifica delle opzioni. Ascolta il trigger nativo della scelta.
        const option = ui.meaType;
        const trigger = typeof option.getTrigger === 'function'
            ? option.getTrigger('value') : 'value_changed';
        if (typeof option.on === 'function') option.on(trigger, refresh);
        if (typeof owner.on === 'function') owner.on('update', refresh);
        if (ui.view && typeof ui.view.on === 'function') ui.view.on('ready', refresh);
        const previous = owner.onDisposed;
        owner.onDisposed = function () {
            state.disposed = true;
            if (typeof option.off === 'function') option.off(trigger, refresh);
            if (typeof owner.off === 'function') owner.off('update', refresh);
            if (ui.view && typeof ui.view.off === 'function') ui.view.off('ready', refresh);
            if (state.inline) {
                for (const { node, anchor } of state.inline.moved) {
                    if (anchor.parentNode) anchor.replaceWith(node);
                }
                state.inline.element.remove();
                state.inline.header.classList.remove('mz-effectsize-input-header');
            }
            INSTANCES.delete(owner);
            if (typeof previous === 'function') return previous.apply(this, arguments);
        };
    }
    if (state.disposed) return;
    const ci = ui.meaType.value() === 'cimt';
    showTarget(ui.vi, !ci, true);
    showTarget(ui.ciLower, ci);
    showTarget(ui.ciUpper, ci);
    mountInputLevel(ui, state, ci);
}

module.exports = { refreshEffectSizeInputs };
