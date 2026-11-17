const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm');
const formats={number:{decSymbol:'.'}};
const sandbox={module:{exports:{}},console,FormatDef:formats};
vm.runInNewContext(fs.readFileSync(require('node:path').join(__dirname,'../../jamovi/js/mzUIX.js'),'utf8')+'\nmodule.exports={numericInputWidth,getDecimalSeparator,formatNumber,parseNumber,stepNumericValue,bindNumericControl,generateGridLayout,initializeControlValues,watchNumericFormat,makeBinding: el=>{const b={cleanups:[]};TABBED_BINDINGS.set(el,b);return b;}};',sandbox);
const api=sandbox.module.exports;
assert.equal(api.numericInputWidth({width:'40px',step:1},true),'34px');
assert.equal(api.numericInputWidth({width:'60px',step:.01},true),'51px');
assert.equal(api.numericInputWidth({width:'40px',step:.001},true),'47px');
assert.equal(api.numericInputWidth({step:1},false),'68px');

formats.number.decSymbol='.';
assert.equal(api.formatNumber(.75,'it-IT'),'0.75');
formats.number.decSymbol=',';
assert.equal(api.formatNumber(.75,'en-US'),'0,75');
for(const text of ['0.75','0,75'])assert.equal(api.parseNumber(text,'en'),.75);
for(const text of ['1,234.5','1.234,5','NaN','Infinity','1abc','--1','0,2,3'])assert.equal(api.parseNumber(text,'en'),null);
assert.equal(api.parseNumber('-1,25e-3','it'),-.00125);
assert.equal(api.formatNumber(1e-7,'it'),'1e-7');
assert.equal(api.stepNumericValue(.29,{step:.01},1),.3);
assert.equal(api.stepNumericValue(.3,{step:.01},-1),.29);
assert.equal(api.stepNumericValue(.99,{step:.01,max:1},1),1);
assert.equal(api.stepNumericValue(0,{step:.01,min:0},-1),0);
assert.equal(api.stepNumericValue(95,{step:1,min:50,max:99},1),96);
class Node {
 constructor(value=''){this.value=value;this.attrs={};this.events={};this.disabled=false;}
 addEventListener(e,cb){(this.events[e]??=[]).push(cb);}
 setAttribute(k,v){this.attrs[k]=v;}
 removeAttribute(k){delete this.attrs[k];}
 fire(e,extra={}){for(const cb of this.events[e]||[])cb({key:extra.key,preventDefault(){},...extra});}
}
let writes=[],value=.75;
const input=new Node(),up=new Node(),down=new Node(),slider=new Node();
const nodes={'#mz_x':input,'#mz_x_spinUp':up,'#mz_x_spinDown':down,'#mz_x_slider':slider};
const element={dataset:{lang:'en-US'},querySelectorAll:sel=>nodes[sel]?[nodes[sel]]:[]};
const option={value:()=>value,setValue:v=>{assert.ok(v===null||typeof v==='number');writes.push(v);value=v;}};
const ui={x:option};const control={id:'x',option:'x',type:'number',inputMode:'spin',step:.01,min:0,max:1};
api.bindNumericControl(element,ui,control,(node,e,cb)=>node.addEventListener(e,cb));
input.value='0,75';input.fire('change');assert.equal(input.value,'0,75');assert.equal(writes.length,0);
input.value='0,76';input.fire('input');assert.equal(value,.76);
input.value='-';input.fire('input');assert.equal(value,.76);assert.equal(input.attrs['aria-invalid'],'true');
input.fire('blur');assert.equal(input.value,'0,76');
up.fire('click');assert.equal(value,.77);assert.equal(input.value,'0,77');
down.fire('click');assert.equal(value,.76);
input.fire('keydown',{key:'ArrowDown'});assert.equal(value,.75);
input.disabled=true;up.fire('click');assert.equal(value,.75);input.disabled=false;
input.value='2';input.fire('input');assert.equal(value,.75);input.fire('change');assert.equal(value,1);
slider.value='.35';slider.fire('input');assert.equal(value,.35);assert.equal(input.value,'0,35');
// Cambio separatore: nessuna scrittura o modifica del numero.
formats.number.decSymbol='.';const before=writes.length;
api.initializeControlValues(element,ui,[{controls:[control]}]);assert.equal(input.value,'0.35');assert.equal(writes.length,before);
formats.number.decSymbol=',';api.initializeControlValues(element,ui,[{controls:[control]}]);assert.equal(input.value,'0,35');assert.equal(writes.length,before);
// L'evento nativo cambia i testi ma non le opzioni; il dispose rimuove il listener.
const listeners=new Set();
formats.number.on=(event,cb)=>{assert.equal(event,'displayFormatChanged');listeners.add(cb);};
formats.number.off=(event,cb)=>listeners.delete(cb);
const binding=api.makeBinding(element);api.watchNumericFormat(element,ui,[{controls:[control]}]);
assert.equal(listeners.size,1);
formats.number.decSymbol='.';for(const cb of listeners)cb();assert.equal(input.value,'0.35');assert.equal(writes.length,before);
for(const cleanup of binding.cleanups)cleanup();assert.equal(listeners.size,0);
// Fallback per ambienti che non espongono FormatDef.number.
const saved=formats.number;formats.number=null;
assert.equal(api.getDecimalSeparator('it-IT'),',');assert.equal(api.getDecimalSeparator('en-US'),'.');
formats.number=saved;
// Sono coperti tutti i controlli numerici effettivamente configurati nei sei modelli.
const configs=require('../../jamovi/js/mzTabConfigs.js');let count=0;
function all(tab){return [...tab.controls,...(tab.groups||[]).flatMap(g=>(g.subTabs||[]).flatMap(t=>t.controls||[]))];}
for(const tabs of Object.values(configs))for(const tab of tabs){
 const html=api.generateGridLayout(tab);
 for(const c of all(tab).filter(c=>c.type==='number')){
  assert.ok(html.includes(`type="text" inputmode="decimal" autocomplete="off" id="mz_${c.id}"`),c.id);
  if(c.inputMode==='spin')assert.ok(html.includes(`id="mz_${c.id}_spinUp"`),c.id);
  count++;
 }
}
assert.ok(count>50);
console.log(`Numeric controls passed: decimal preference, parsing, editing, spin, limits, slider, resync without writes; ${count} configured numeric controls across six models.`);
