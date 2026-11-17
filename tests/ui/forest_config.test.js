// Verifica il contratto della griglia che mzUIX attraversa con row < grid.rows.
'use strict';
const assert = require('node:assert/strict');
const configs = require('../../jamovi/js/mzTabConfigs.js');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const sandbox = { module: { exports: {} }, console };
vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../../jamovi/js/mzUIX.js'), 'utf8') + '\nmodule.exports.generateGridLayout = generateGridLayout; module.exports.initializeControlValues = initializeControlValues;', sandbox);
let models = 0;
for (const [name, tabs] of Object.entries(configs)) {
    const tab = tabs.find(t => t.id === 'forest-tab');
    const columns = tab.grid.cols.split(':').length;
    for (const control of tab.controls) {
        const p = control.position;
        if (!p) continue;
        assert.ok(p.row >= 0 && p.row < tab.grid.rows, `${name}/${control.id}: row outside mzUIX grid`);
        assert.ok(p.col >= 0 && p.col < columns, `${name}/${control.id}: column outside mzUIX grid`);
    }
    const selectors = tab.controls.filter(c => c.option === 'forestPredictionStyle');
    assert.equal(selectors.length, 1);
    assert.equal(selectors[0].position.row, 1);
    assert.equal(selectors[0].position.col, 2);
    const group = tab.groups.find(g => g.title === 'Plot Settings');
    assert.equal(selectors[0].position.col, group.position.col);
    assert.ok(selectors[0].position.row >= group.position.row && selectors[0].position.row < group.position.row + group.rowSpan);
    const html = sandbox.module.exports.generateGridLayout(tab);
    assert.ok(html.includes('id="mz_forestPredictionStyle"'), `${name}: selector missing from generated HTML`);
    assert.equal(selectors[0].label, 'PI style');
    assert.equal(selectors[0].labelPosition, 'left');
    assert.ok(html.includes(`width: ${selectors[0].width} !important;`));
    assert.equal(selectors[0].compactInline, true);
    assert.equal(selectors[0].align, 'top-center');
    assert.equal(selectors[0].itemGap, '2px');
    assert.ok(html.includes('mz-compact-inline-select'));
    assert.equal(selectors[0].default, 'line2');
    assert.deepEqual(selectors[0].options.map(x => x.value), ['line2', 'diamond', 'bar', 'shade', 'dist']);
    assert.equal(selectors[0].valueAliases.line, 'line2');
    assert.ok(!html.includes('<option value="line">'));
    const rob = tab.controls.find(c => c.option === 'forestRobShow');
    if (rob) assert.equal(rob.dependsOn, undefined, `${name}: RoB must remain selectable`);
    const shade = tab.controls.find(c => c.option === 'shadeRows');
    assert.equal(shade.dependsOn, undefined, `${name}: Shade Rows must remain selectable`);
    models++;
}
assert.equal(models, 6);
console.log('Forest grid and prediction selector: six models passed.');

// Ripristino del valore legacy senza mutazioni delle opzioni o eventi spurii.
const select = { value: '' };
sandbox.mockSelect = select;
vm.runInNewContext('queryUIX = () => mockSelect;', sandbox);
sandbox.module.exports.initializeControlValues({}, { forestPredictionStyle: { value: () => 'line' } }, [{ controls: [{ id: 'forestPredictionStyle', option: 'forestPredictionStyle', type: 'select', valueAliases: { line: 'line2' } }] }]);
assert.equal(select.value, 'line2');
