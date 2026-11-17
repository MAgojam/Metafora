const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm'),path=require('node:path');
const helper=require('../../jamovi/js/mzPredictiveProbabilityControls');
const noop=()=>{};let notices=[];
const stub=new Proxy({},{get:()=>noop});
const context={module:{exports:{}},console,window:{__mzToastListenerAttached:true},
 document:{querySelector:()=>null,querySelectorAll:()=>[]},
 require:name=> name==='./mzPredictiveProbabilityControls'?helper:
 name==='./mzToast'?{showToast:(...args)=>notices.push(args)}:name==='./mzTabConfigs'?{}:stub};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../../jamovi/js/main.js'),'utf8'),context);
const handlers=context.module.exports;
for(const model of ['mzDichotomous','mzMeanDifferences','mzEffectSizes','mzAlphas','mzCorrelations','mzProportions']) {
 const yaml=fs.readFileSync(path.join(__dirname,'../../jamovi',model+'.u.yaml'),'utf8');
 assert.match(yaml,/name: moderator[\s\S]*?change: \.\/main::onChange_moderator/);
 for(const views of Array.from({length:8},(_,i)=>[!!(i&1),!!(i&2),!!(i&4)])) {
  const data={moderator:[],predProbPlotShow:views[0],predProbTableShow:views[1],showTextRepo:views[2]};let writes=0;
  const ui=Object.fromEntries(Object.keys(data).map(key=>[key,{value:()=>data[key],setValue:()=>writes++}]));
  ui.tabbedControl={translate:s=>s};notices=[];
  // Esercita loaded e l'handler reale collegato nei sei .u.yaml, non il solo helper.
  handlers.loaded(ui,{});assert.equal(notices.length,0);
  data.moderator=['age'];handlers.onChange_moderator(ui,{});
  const expected=views.some(Boolean)?1:0;assert.equal(notices.length,expected,model+'/'+views);
  data.moderator=['age','year'];handlers.onChange_moderator(ui,{});assert.equal(notices.length,expected);
  data.moderator=[];handlers.onChange_moderator(ui,{});assert.equal(notices.length,expected);
  data.moderator=['age'];handlers.onChange_moderator(ui,{});assert.equal(notices.length,2*expected);
  assert.equal(writes,0);assert.equal(data.predProbPlotShow,views[0]);assert.equal(data.predProbTableShow,views[1]);assert.equal(data.showTextRepo,views[2]);
 }
}
console.log('Native moderator handler: six models × eight Plot/Table/Text Reporting combinations; immediate transition toast, deduplication, re-entry and zero option writes passed.');
