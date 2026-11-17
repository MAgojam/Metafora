const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm'),path=require('node:path');
const helper=require('../../jamovi/js/mzPredictiveProbabilityControls');
const {notifyPredictiveProbability:notify,guardPredictiveProbabilitySelection:guard,bindPredictiveProbabilityNotice:bind}=helper;
const data={moderator:[],modelest:'DL',showHetTest:false};let writes=0,notices=[];
const ui=Object.fromEntries(Object.keys(data).map(k=>[k,{value:()=>data[k],setValue:()=>writes++}]));
ui.tabbedControl={translate:s=>s};const toast=(...args)=>notices.push(args);
notify(ui,toast,true);
const input={id:'mz_showHetTest',checked:true};const event={target:input,preventDefault(){this.prevented=true;},stopImmediatePropagation(){}};
assert.equal(guard(ui,toast,event),true);assert.equal(input.checked,false);assert.equal(writes,0);
assert.match(notices.at(-1)[0],/ML or REML/);
data.modelest='REML';assert.equal(guard(ui,toast,event),false);
data.moderator=['age'];assert.equal(guard(ui,toast,event),true);
data.moderator=[];data.showHetTest=true;notify(ui,toast);let n=notices.length;
data.modelest='DL';notify(ui,toast);assert.equal(notices.length,n);notify(ui,toast);assert.equal(notices.length,n);
data.modelest='ML';notify(ui,toast);data.moderator=['age'];n=notices.length;notify(ui,toast);assert.equal(notices.length,n+1);
// Il cambio del select Estimator viene osservato dopo la scrittura del binding, senza ritardo.
const events=[];ui.tabbedControl.$el={addEventListener:(type,cb,capture)=>events.push({type,cb,capture})};
data.moderator=[];data.modelest='REML';bind(ui,toast);n=notices.length;
data.modelest='FE';events.find(e=>e.type==='change'&&!e.capture).cb();assert.equal(notices.length,n);
const configs=require('../../jamovi/js/mzTabConfigs.js');
for(const [model,tabs] of Object.entries(configs)){
 const tab=tabs.find(t=>t.id==='model-tab');const c=tab.controls.filter(c=>c.id==='showHetTest');assert.equal(c.length,1);
 assert.equal(c[0].position.col,0);assert.equal(c[0].position.row,tab.groups.find(g=>g.title==='Display Options').position.row);
 assert.equal(c[0].type,'checkbox');assert.equal(c[0].checked,false);assert.deepEqual(c[0].dependsOn,{control:'modelest',enableWhen:['ML','REML'],resetOnEnable:true,disabledTooltip:'Requires ML or REML estimation.'});
 const name={dichotomous:'mzDichotomous',meandiff:'mzMeanDifferences',effectsize:'mzEffectSizes',alpha:'mzAlphas',correlation:'mzCorrelations',proportion:'mzProportions'}[model];
 assert.match(fs.readFileSync(path.join(__dirname,'../../jamovi',name+'.a.yaml'),'utf8'),/name: showHetTest[\s\S]*?default: false/);
 assert.match(fs.readFileSync(path.join(__dirname,'../../jamovi',name+'.r.yaml'),'utf8'),/name: hetTestTable/);
}
console.log('Hettest UI: six model configurations, ML/REML/moderator guards, estimator change notice and zero option writes passed.');

// Esercita il motore reale e il tooltip sul label del checkbox disabilitato.
class DomNode {
 constructor(){this.attrs={};this.events={};this.style={};this.hidden=true;}
 setAttribute(k,v){this.attrs[k]=v;}
 getAttribute(k){return this.attrs[k]??null;}
 removeAttribute(k){delete this.attrs[k];}
 addEventListener(type,cb){(this.events[type]??=new Set()).add(cb);}
 removeEventListener(type,cb){this.events[type]?.delete(cb);}
 fire(type){for(const cb of this.events[type]||[])cb({});}
 getBoundingClientRect(){return {left:10,top:10,bottom:30,width:140,height:20};}
 remove(){this.removed=true;}
}
const tips=[];const win=new DomNode();win.innerWidth=1000;win.innerHeight=600;
const sandbox={module:{exports:{}},console,window:win,document:{createElement:()=>new DomNode(),body:{appendChild:node=>tips.push(node)}}};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../../jamovi/js/mzUIX.js'),'utf8')+
 '\nmodule.exports={setupControlDependencies,refreshControlDependencies,disposeTabbedControl,makeBinding:el=>TABBED_BINDINGS.set(el,{cleanups:[]})};',sandbox);
for(const tabs of Object.values(configs)) {
 const control=tabs.find(t=>t.id==='model-tab').controls.find(c=>c.id==='showHetTest');
 let estimator='REML',selected=true,mutations=0;
 const label=new DomNode();const input={disabled:false,checked:true,closest:selector=>selector.includes('checkbox-label')?label:null};
 const root={querySelectorAll:selector=>selector==='#mz_showHetTest'?[input]:[]};
 const mockUI={modelest:{value:()=>estimator},showHetTest:{value:()=>selected,setValue:v=>{selected=v;mutations++;}},tabbedControl:{$el:root,translate:s=>s}};
 sandbox.module.exports.makeBinding(root);
 sandbox.module.exports.setupControlDependencies(root,mockUI,[{controls:[control]}]);
 const tip=tips.at(-1);assert.equal(input.disabled,false);assert.equal(tip.textContent,'');
 for(const method of ['DL','HE','HS','SJ','SJ2','EB','PM','FE','ML','REML','future-estimator']) {
  estimator=method;sandbox.module.exports.refreshControlDependencies(mockUI);
  const disabled=!['ML','REML'].includes(method);
  assert.equal(input.disabled,disabled,method);
  assert.equal(input.checked,true);assert.equal(selected,true);assert.equal(mutations,0);
  assert.equal(tip.textContent,disabled?'Requires ML or REML estimation.':'');
  label.fire('mouseenter');assert.equal(tip.hidden,!disabled);label.fire('mouseleave');assert.equal(tip.hidden,true);
  label.fire('focusin');assert.equal(tip.hidden,!disabled);label.fire('focusout');assert.equal(tip.hidden,true);
 }
 estimator='REML';sandbox.module.exports.refreshControlDependencies(mockUI);
 assert.equal(input.disabled,false);assert.equal(mutations,0);
 sandbox.module.exports.disposeTabbedControl(root);assert.equal(tip.removed,true);
 assert.equal(label.events.mouseenter.size,0);
}
// Il label disabilitato non produce toast al posto del tooltip.
data.modelest='DL';data.moderator=[];data.showHetTest=false;input.disabled=true;n=notices.length;
assert.equal(guard(ui,toast,event),false);assert.equal(notices.length,n);
console.log('Hettest dependency engine and disabled-label tooltip: six models, only ML/REML enabled, preserved selection, silent estimator changes and listener cleanup passed.');

// Il numero di repliche segue test, stimatore e moderatori senza perdere il valore.
for(const tabs of Object.values(configs)) {
 const control=tabs.find(t=>t.id==='model-tab').controls.find(c=>c.id==='hetTestBoot');
 assert.ok(control);assert.equal(control.value,1000);assert.equal(control.min,100);assert.equal(control.max,100000);
 assert.equal(control.type,'number');assert.equal(control.inputMode,'spin');
 let estimator='REML',selected=false,mods=[],samples=5000,writes=0;
 const input={disabled:false,value:'5000',closest:()=>null};
 const root={querySelectorAll:selector=>selector==='#mz_hetTestBoot'?[input]:[]};
 const mockUI={modelest:{value:()=>estimator},showHetTest:{value:()=>selected},moderator:{value:()=>mods},
  hetTestBoot:{value:()=>samples,setValue:v=>{samples=v;writes++;}},tabbedControl:{$el:root,translate:s=>s}};
 sandbox.module.exports.makeBinding(root);
 sandbox.module.exports.setupControlDependencies(root,mockUI,[{controls:[control]}]);assert.equal(input.disabled,true);
 selected=true;sandbox.module.exports.refreshControlDependencies(mockUI);assert.equal(input.disabled,false);
 mods=['age'];sandbox.module.exports.refreshControlDependencies(mockUI);assert.equal(input.disabled,true);
 mods=[];estimator='DL';sandbox.module.exports.refreshControlDependencies(mockUI);assert.equal(input.disabled,true);
 estimator='ML';sandbox.module.exports.refreshControlDependencies(mockUI);assert.equal(input.disabled,false);
 selected=false;sandbox.module.exports.refreshControlDependencies(mockUI);assert.equal(input.disabled,true);
 assert.equal(samples,5000);assert.equal(writes,0);
 sandbox.module.exports.disposeTabbedControl(root);
}
console.log('Bootstrap samples: six model controls, default/range and dependencies preserve the chosen count.');
