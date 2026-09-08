// Isolated unit check of embedded controls; no browser or application is driven.
const fs = require('node:fs');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const html = fs.readFileSync(process.argv[2], 'utf8');
const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)];
const code = scripts.find(x => x[1].includes('function terrainDetails(open)'))?.[1];
assert.ok(code, 'Report control script retained');
const items = [{open:false}, {open:true}, {open:false}];
const events = {};
const context = {document:{querySelectorAll:() => items},
  window:{addEventListener:(name, callback) => {events[name] = callback;}}};
vm.runInNewContext(code, context);
context.terrainDetails(true);
assert.ok(items.every(x => x.open));
context.terrainDetails(false);
assert.ok(items.every(x => !x.open));
items[1].open = true;
events.beforeprint();
assert.ok(items.every(x => x.open));
events.beforeprint(); // Repeated events must not discard the original state.
events.afterprint();
assert.deepEqual(items.map(x => x.open), [false, true, false]);
events.afterprint();
assert.deepEqual(items.map(x => x.open), [false, true, false]);
console.log('Expand/collapse and print-state restoration passed isolated checks.');
