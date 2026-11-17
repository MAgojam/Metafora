// Toast: selezioni conservate, avviso solo all'ingresso nel contesto non supportato.
'use strict';
const assert=require('node:assert/strict');
const {notifyPredictiveProbability:notify}=require('../../jamovi/js/mzPredictiveProbabilityControls');
const data={moderator:[],predProbTableShow:false,predProbPlotShow:false};
const ui=Object.fromEntries(Object.keys(data).map(k=>[k,{value:()=>data[k]}]));
ui.tabbedControl={translate:s=>s};let notices=[];const toast=(...args)=>notices.push(args);
notify(ui,toast,true);
data.moderator=['age'];notify(ui,toast);assert.equal(notices.length,0);
data.predProbTableShow=true;notify(ui,toast);assert.equal(notices.length,1);
notify(ui,toast);assert.equal(notices.length,1);
data.predProbPlotShow=true;notify(ui,toast);assert.equal(notices.length,2);
notify(ui,toast);assert.equal(notices.length,2);
data.moderator=[];notify(ui,toast);assert.equal(notices.length,2);
data.moderator=['age'];notify(ui,toast);assert.equal(notices.length,3);
assert.equal(data.predProbTableShow,true);assert.equal(data.predProbPlotShow,true);
notify(ui,toast,true);assert.equal(notices.length,3);
assert.match(notices[0][0],/without moderators/);assert.equal(notices[0][1],'info');
console.log('Predictive moderator notice: activation, deduplication, silent restore and retained selections passed.');

// Regressione segnalata: l'attivazione deve fermarsi PRIMA di qualsiasi setValue.
const {guardPredictiveProbabilitySelection:guard,bindPredictiveProbabilityNotice:bind}=require('../../jamovi/js/mzPredictiveProbabilityControls');
let writes=0;for(const key of ['predProbPlotShow','predProbTableShow'])ui[key].setValue=v=>{writes++;data[key]=v;};
data.predProbTableShow=false;data.predProbPlotShow=false;data.moderator=['age'];
for(const name of ['predProbPlotShow','predProbTableShow']) {
 const input={id:'mz_'+name,checked:true};
 const event={target:input,preventDefault(){this.prevented=true;},stopImmediatePropagation(){this.stopped=true;}};
 const n=notices.length;
 assert.equal(guard(ui,toast,event),true);
 assert.equal(input.checked,false);assert.equal(event.prevented,true);assert.equal(event.stopped,true);
 assert.equal(writes,0);assert.equal(data[name],false);assert.equal(notices.length,n+1);
 // Anche la label, senza attendere il click sintetico sull'input.
 event.target={closest:()=>({querySelector:()=>input})};
 assert.equal(guard(ui,toast,event),true);assert.equal(writes,0);
 // Lo spegnimento di una selezione gia salvata resta consentito e silenzioso.
 data[name]=true;input.checked=false;event.target=input;const before=notices.length;
 assert.equal(guard(ui,toast,event),false);assert.equal(notices.length,before);data[name]=false;
 data.moderator=[];assert.equal(guard(ui,toast,event),false);data.moderator=['age'];
}
// Un solo listener per evento, nessuna microtask ritardata; segue la UI anche dopo rebind.
const events=[];const root={addEventListener:(type,cb,capture)=>events.push({type,cb,capture})};
ui.tabbedControl.$el=root;bind(ui,toast);bind(ui,toast);
assert.deepEqual(events.filter(e=>e.capture).map(e=>e.type),['click','change']);
assert.equal(events.filter(e=>!e.capture && e.type==='change').length,1);
const input={id:'mz_predProbPlotShow',checked:true};const before=notices.length;
events[0].cb({target:input,preventDefault(){},stopImmediatePropagation(){}});
assert.equal(notices.length,before+1);assert.equal(writes,0);assert.equal(input.checked,false);
console.log('Predictive activation guard: input, label, capture, immediate toast, zero backend writes and allowed deselection passed.');

// Text Reporting usa lo stesso blocco preventivo, senza creare l'avviso HTML/SVG.
data.showTextRepo=false;ui.showTextRepo={value:()=>data.showTextRepo,setValue:()=>writes++};
data.moderator=['age'];
const reportInput={id:'mz_showTextRepo',checked:true};
const reportEvent={target:reportInput,preventDefault(){this.prevented=true;},stopImmediatePropagation(){}};
let n=notices.length;
assert.equal(guard(ui,toast,reportEvent),true);assert.equal(reportInput.checked,false);
assert.equal(data.showTextRepo,false);assert.equal(writes,0);assert.equal(notices.length,n+1);
assert.equal(notices.at(-1)[5],'Text reporting');
data.showTextRepo=true;reportInput.checked=false;n=notices.length;
assert.equal(guard(ui,toast,reportEvent),false);assert.equal(notices.length,n);
console.log('Text Reporting: activation guard, report-specific toast and silent deselection passed.');
