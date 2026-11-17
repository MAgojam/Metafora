// Notifica all'ingresso in una selezione non supportata; restore e refresh restano silenziosi.
'use strict';
const states = new WeakMap();
function notifyPredictiveProbability(ui, showToast, initialize = false) {
  if (!ui) return;
  const value = name => ui[name] && typeof ui[name].value === 'function' ? ui[name].value() : null;
  const mods = value('moderator');
  const state = { blocked: !!(mods && mods.length), plot: value('predProbPlotShow') === true,
    table: value('predProbTableShow') === true, report: value('showTextRepo') === true,
    het: value('showHetTest') === true, hetBlocked: !!(mods && mods.length) || !['ML','REML'].includes(value('modelest')) };
  const previous = states.get(ui); states.set(ui, state);
  if (initialize || !previous || typeof showToast !== 'function') return;
  const predictive = state.blocked && (state.plot || state.table) && (!previous.blocked ||
    (state.plot && !previous.plot) || (state.table && !previous.table));
  const report = state.blocked && state.report && (!previous.blocked || !previous.report);
  const het = state.het && state.blocked && (!previous.hetBlocked || !previous.het);
  if (!predictive && !report && !het) return;
  const translate = text => ui.tabbedControl && typeof ui.tabbedControl.translate === 'function'
    ? ui.tabbedControl.translate(text) : text;
  const title = het ? ((predictive || report) ? 'Model options' : 'Heteroscedasticity test') : predictive && report ? 'Predictive probability and Text reporting'
    : report ? 'Text reporting' : 'Predictive probability';
  const message = het ? ((predictive || report)
    ? 'The selected predictive results or text report are unavailable with moderators. Heteroscedasticity testing requires ML or REML without moderators.'
    : 'Heteroscedasticity testing requires ML or REML without moderators. Select ML or REML and remove moderators to display the selected test.') : predictive && report
    ? 'Predictive probabilities and text reporting are not currently available with moderators. Remove the moderators to display the selected results.'
    : report ? 'Text reporting is not currently available with moderators. Remove the moderators to display the report.'
    : 'Predictive probabilities currently require a random-effects model without moderators. Remove the moderators to display the selected table or plot.';
  showToast(translate(message), 'info', 8000, 'CC', 'light', translate(title));
}
// Blocco in cattura PRIMA del change che scriverebbe l'opzione hidden al backend.
function guardPredictiveProbabilitySelection(ui, showToast, event) {
  const mods = ui && ui.moderator && ui.moderator.value();
  if (!event.target) return false;
  let input = event.target;
  const optionFor = { mz_predProbPlotShow: 'predProbPlotShow', mz_predProbTableShow: 'predProbTableShow', mz_showTextRepo: 'showTextRepo', mz_showHetTest: 'showHetTest' };
  if (!optionFor[input.id]) {
    const label = typeof input.closest === 'function' && input.closest('label');
    input = label && label.querySelector('input');
  }
  const name = input && optionFor[input.id];
  if (!name || !ui[name] || ui[name].value() === true) return false; // Spegnere resta consentito.
  if (name === 'showHetTest' && input.disabled) return false; // Lo stimatore incompatibile e' spiegato dal tooltip.
  const invalidEstimator = name === 'showHetTest' && !(ui.modelest && ['ML','REML'].includes(ui.modelest.value()));
  if (!(mods && mods.length) && !invalidEstimator) return false;
  event.preventDefault();
  event.stopImmediatePropagation();
  // Il click cancellato annulla anche l'attivazione nativa; change copre eventi sintetici.
  input.checked = false;
  const translate = text => ui.tabbedControl && typeof ui.tabbedControl.translate === 'function'
    ? ui.tabbedControl.translate(text) : text;
  const isReport = name === 'showTextRepo', isHet = name === 'showHetTest';
  if (typeof showToast === 'function') showToast(
    translate(isHet ? 'Heteroscedasticity testing requires ML or REML without moderators. Select ML or REML and remove moderators before enabling this test.' : isReport ? 'Text reporting is not currently available with moderators. Remove the moderators before selecting Text reporting.'
      : 'Predictive probabilities currently require a random-effects model without moderators. Remove the moderators before selecting Plot or Table.'),
    'info', 8000, 'CC', 'light', translate(isHet ? 'Heteroscedasticity test' : isReport ? 'Text reporting' : 'Predictive probability'));
  return true;
}
const roots = new WeakMap();
function bindPredictiveProbabilityNotice(ui, showToast) {
  let root = ui && ui.tabbedControl && ui.tabbedControl.$el;
  if (root && root[0]) root = root[0];
  if (!root || typeof root.addEventListener !== 'function') return;
  const existing = roots.get(root);
  if (existing) { existing.ui = ui; existing.showToast = showToast; return; }
  const state = { ui, showToast }; roots.set(root, state);
  notifyPredictiveProbability(ui, showToast, true);
  const guard = event => guardPredictiveProbabilitySelection(state.ui, state.showToast, event);
  // Anche click sulle label e attivazioni da tastiera transitano dal click nativo.
  root.addEventListener('click', guard, true);
  root.addEventListener('change', guard, true);
  // Il select Estimator e' custom: in bubbling il suo binding ha gia scritto il valore.
  root.addEventListener('change', () => notifyPredictiveProbability(state.ui, state.showToast), false);
}
module.exports = { notifyPredictiveProbability, bindPredictiveProbabilityNotice, guardPredictiveProbabilitySelection };
