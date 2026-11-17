// Contratto UI dei sei modelli: nuovo gruppo separato e controlli esistenti preservati.
'use strict';
const assert = require('node:assert/strict');
const configs = require('../../jamovi/js/mzTabConfigs.js');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const sandbox = { module: { exports: {} }, console };
vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../../jamovi/js/mzUIX.js'), 'utf8') + '\nmodule.exports.generateGridLayout = generateGridLayout;', sandbox);
const ids = ['predProbTableShow','predProbPlotShow','predProbThreshold','predProbDirection'];
let count = 0;
for (const [name,tabs] of Object.entries(configs)) {
 const tab = tabs.find(t => t.id === 'model-tab');
 const cols = tab.grid.cols.split(':').length;
 const group = tab.groups.find(g => g.title === 'Predictive probability');
 assert.ok(group, name);
 const display = tab.groups.find(g => g.title === 'Display Options');
 assert.equal(display.position.row,group.position.row);
 assert.equal(display.position.col,0); assert.equal(display.colSpan,1);
 assert.equal(group.position.col,1); assert.equal(group.colSpan,2);
 const control = id => tab.controls.find(c => c.id === id);
 assert.deepEqual(control('showTextRepo').position,{row:3,col:0});
 assert.equal(control('predProbPlotShow').label,'Plot');
 assert.equal(control('predProbTableShow').label,'Table');
 assert.equal(control('predProbShowLabel').label,'Show:');
 assert.equal(control('predProbThreshold').width,'55px');
 const direction=control('predProbDirection');
 assert.equal(direction.type,'deviatore'); assert.equal(direction.shape,'beam');
 assert.equal(direction.allowOff,false);
 assert.deepEqual(direction.stateValues,{posLorT:'above',posRorB:'below'});
 assert.equal(direction.default,'posRorB');
 assert.deepEqual(group.inlineRows,[3]);
 for(const id of ['predProbShowLabel','predProbPlotShow','predProbTableShow'])
  assert.deepEqual(control(id).position,{row:3,col:1,colSpan:2});
 assert.equal(control('showModelFit').margin,control('showTextRepo').margin);


 const html = sandbox.module.exports.generateGridLayout(tab);
 assert.equal(group.contentRows,true);
 assert.ok(html.includes('grid-template: repeat(3, max-content)'));
 assert.ok(html.includes('flex-direction: row; align-items: center; flex-wrap: wrap; gap: 12px;'));

 for(const id of ids) {
  const controls = tab.controls.filter(c => c.id === id);
  assert.equal(controls.length,1,name + '/' + id);
  const pos = controls[0].position;
  assert.ok(pos.row >= group.position.row && pos.row < group.position.row + group.rowSpan);
  assert.ok(pos.col >= 0 && pos.col + (pos.colSpan || 1) <= cols);
  assert.ok(html.includes('id="mz_' + id + '"'),name + '/' + id);
 }
 for(const control of tab.controls) {
  assert.ok(control.position.row < tab.grid.rows,name + '/' + control.id);
 }
 const info = tab.infoSections.flatMap(s => s.items).find(i => i.term === 'Predictive probability');
 assert.deepEqual(info.options,ids);
 assert.ok(info.desc.includes('PFT'));
 assert.ok(info.desc.includes('without moderators'));
 count++;
}
assert.equal(count,6);
console.log('Predictive probability UI: six models passed.');
