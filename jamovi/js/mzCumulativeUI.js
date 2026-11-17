'use strict';

// Il select usa l'opzione Variable nativa, senza ricostruire il TabbedControl.
// Nessun dato delle colonne viene richiesto: servono soltanto i nomi disponibili.
const INSTANCES = new WeakMap();

function refreshCumulativeVariables(ui) {
    const control = ui && ui.tabbedControl;
    if (!control || typeof control.requestData !== 'function' || !ui.cumulativeSortVariable) return;
    let state = INSTANCES.get(control);
    if (!state) {
        state = { request: 0, disposed: false };
        INSTANCES.set(control, state);
        const refresh = () => refreshCumulativeVariables(ui);
        if (typeof control.on === 'function') control.on('update', refresh);
        if (ui.view && typeof ui.view.on === 'function') ui.view.on('ready', refresh);
        const previous = control.onDisposed;
        control.onDisposed = function () {
            state.disposed = true;
            state.request++;
            if (typeof control.off === 'function') control.off('update', refresh);
            if (ui.view && typeof ui.view.off === 'function') ui.view.off('ready', refresh);
            INSTANCES.delete(control);
            if (typeof previous === 'function') return previous.apply(this, arguments);
        };
    }
    const root = control.$el && (control.$el[0] || control.$el);
    const select = root && root.querySelector('#mz_cumulativeSortVariable');
    if (!select) return;
    const request = ++state.request;
    Promise.resolve(control.requestData('columns', null)).then(info => {
        if (state.disposed || request !== state.request || !select.isConnected ||
            root.querySelector('#mz_cumulativeSortVariable') !== select) return;
        const columns = info && Array.isArray(info.columns) ? info.columns : [];
        const saved = ui.cumulativeSortVariable.value();
        const names = [...new Set(columns.filter(column => !column.hidden && typeof column.name === 'string')
            .map(column => column.name))];
        if (saved && !names.includes(saved)) names.push(saved);
        const translate = text => typeof control.translate === 'function' ? control.translate(text) : text;
        const signature = JSON.stringify([names, translate('Select a variable')]);
        if (select.dataset.mzCumulativeColumns !== signature) {
            const doc = select.ownerDocument;
            const placeholder = doc.createElement('option');
            placeholder.value = ''; placeholder.textContent = translate('Select a variable');
            placeholder.disabled = true;
            const options = names.map(name => {
                const option = doc.createElement('option');
                option.value = name; option.textContent = name;
                return option;
            });
            select.replaceChildren(placeholder, ...options);
            select.dataset.mzCumulativeColumns = signature;
        }
        select.value = saved || '';
    }).catch(error => console.warn('[Metafora/CUM] Column names unavailable:', error.message));
}

module.exports = { refreshCumulativeVariables };
