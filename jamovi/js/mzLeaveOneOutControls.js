// Compatibilita dei due vecchi schemi: deviatore tabella/grafico e Bool Show Plot.
// Il nuovo deviatore sceglie solo il grafico; Show Table resta indipendente.
const migrating = new WeakSet();
function migrateLeaveOneOutControls(ui) {
  if (!ui || migrating.has(ui)) return;
  const legacy = ui.leave1outMode;
  const plot = ui.leave1outPlotShow, table = ui.leave1outTableShow;
  if (!legacy || !plot || !table) return;
  const mode = legacy.value(), current = ui.leave1outPlotMode;
  if (mode !== 'posLorT' && mode !== 'posRorB' && (!current || !plot.value())) return;
  migrating.add(ui);
  try {
    if (current) {
      if ((mode === 'posRorB' || plot.value() === true) && current.value() === 'posOff')
        current.setValue('posRorB');
      if (mode === 'posLorT' && table.value() !== true) table.setValue(true);
      if (plot.value() === true) plot.setValue(false);
    } else if (mode === 'posRorB' || mode === 'posLorT') {
      const target = mode === 'posRorB' ? plot : table;
      if (target.value() !== true) target.setValue(true);
    }
    if (mode !== 'posOff') legacy.setValue('posOff');
  } finally { migrating.delete(ui); }
}
// Notifica soltanto l'ingresso in LOO FE; restore e refresh estetici sono silenziosi.
const commonEffectStates = new WeakMap();
function notifyLeaveOneOutCommonEffect(ui, showToast, initialize = false) {
  if (!ui) return;
  const value = key => ui[key] && typeof ui[key].value === 'function' ? ui[key].value() : null;
  const mode = value('leave1outPlotMode');
  const visible = mode === 'posLorT' || mode === 'posRorB' || value('leave1outTableShow') === true;
  const moderators = value('moderator');
  const common = ['FE', 'EE', 'CE'].includes(value('modelest')) && !(moderators && moderators.length);
  const cumulativeMode = value('cumulativePlotMode');
  const active = { loo: visible && common, cumulative: common &&
    (cumulativeMode === 'posLorT' || cumulativeMode === 'posRorB' || value('cumulativeTableShow') === true) };
  const previous = commonEffectStates.get(ui);
  commonEffectStates.set(ui, active);
  if (initialize || previous === undefined || typeof showToast !== 'function') return;
  const enteredLoo = active.loo && !previous.loo, enteredCumulative = active.cumulative && !previous.cumulative;
  if (!enteredLoo && !enteredCumulative) return;
  const translate = text => ui.tabbedControl && typeof ui.tabbedControl.translate === 'function'
    ? ui.tabbedControl.translate(text) : text;
  const title = enteredLoo && enteredCumulative ? 'Leave-One-Out and Cumulative: common-effect model'
    : enteredCumulative ? 'Cumulative Meta-Analysis: common-effect model' : 'Leave-One-Out: common-effect model';
  showToast(translate('Tau-squared and tau are not estimated under the common-effect model. Other software may display separate heterogeneity estimates alongside FE results.'),
    'info', 8000, 'CC', 'light', translate(title));
}
// update di jamovi segnala view.ready, non ogni scrittura delle opzioni hidden.
// La delega resta sul contenitore UI: segue anche i controlli ricreati nei tab.
const noticeRoots = new WeakMap();
function bindLeaveOneOutCommonEffect(ui, showToast) {
  let root = ui && ui.tabbedControl && ui.tabbedControl.$el;
  if (root && root[0]) root = root[0];
  if (!root || typeof root.addEventListener !== 'function') return;
  const existing = noticeRoots.get(root);
  if (existing) { existing.ui = ui; existing.showToast = showToast; return; }
  const state = { ui, showToast };
  noticeRoots.set(root, state);
  notifyLeaveOneOutCommonEffect(ui, showToast, true);
  const changed = () => queueMicrotask(() => {
    if (root.isConnected) notifyLeaveOneOutCommonEffect(state.ui, state.showToast);
  });
  for (const event of ['change', 'click', 'keydown']) root.addEventListener(event, changed, true);
}
module.exports = { migrateLeaveOneOutControls, notifyLeaveOneOutCommonEffect, bindLeaveOneOutCommonEffect };
