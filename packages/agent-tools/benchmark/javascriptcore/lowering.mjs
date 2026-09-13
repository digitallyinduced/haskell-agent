// Run with a Nix-provided Bun and the pinned Acorn UMD distribution:
// bun lowering.mjs /nix/store/.../dist/acorn.js
// This compares source lowering, not native JSC isolation or worker transport.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

const acornPath = process.argv[2];
assert(acornPath, "usage: bun lowering.mjs ACORN_DISTRIBUTION");
const parserContext = vm.createContext(Object.create(null));
vm.runInContext(readFileSync(acornPath, "utf8"), parserContext);
vm.runInContext(
  readFileSync(
    new URL("../../data/code-mode/javascriptcore/lower-module.js", import.meta.url),
    "utf8",
  ),
  parserContext,
);
const lowerModule = parserContext.lowerModule;

const success = (name, source, expected) => ({ name, source, expected, ok: true });
const failure = (name, source, expected = []) => ({ name, source, expected, ok: false });
const cases = [
  success("empty module", "", []),
  success("top-level await", "text(await Promise.resolve(42));", ["42"]),
  success("module lexical this", "text(this); text((() => this)());", ["undefined", "undefined"]),
  success("shadowed undefined", "const undefined = 7; text(this); text(undefined);", ["undefined", "7"]),
  success("unresolved arguments", "text(typeof arguments); text((() => typeof arguments)());", ["undefined", "undefined"]),
  success("function arguments", "function f(a) { text(arguments[0]); } f(8);", ["8"]),
  success("function this", "function f() { text(this); } f.call(8); f();", ["8", "undefined"]),
  success("function lexical arrow", "function f() { text((() => this)()); } f.call(9);", ["9"]),
  success("arrow default parameter", "text(((value = this) => value)());", ["undefined"]),
  success("nested class scopes", `
    const names = [];
    class C {
      [(names.push(this), "field")] = this;
      [(names.push((() => this)()), "method")]() { return this; }
      static self = this;
      static { text(this.name); }
    }
    const instance = new C();
    text(names.every(value => value === undefined));
    text(instance.field === instance);
    text(instance.method() === instance);
    text(C.self === C);
  `, ["C", "true", "true", "true", "true"]),
  success("class within ordinary function", `
    function f() {
      class C { [String(this)] = this; }
      const instance = new C();
      text(instance[12] === instance);
    }
    f.call(12);
  `, ["true"]),
  success("class extends lexical expression", `
    class C extends (text(this), Object) {}
    text(new C() instanceof Object);
  `, ["undefined", "true"]),
  success("object computed method", `
    const value = { [(text(this), "method")]() { return this; } };
    text(value.method() === value);
  `, ["undefined", "true"]),
  success("named export declarations", "export const a = 1; export let b = 2; export var c = 3; text(a+b+c);", ["6"]),
  success("export list and alias", "const value = 4; export {value, value as other}; text(value);", ["4"]),
  success("empty export", "export {}; text(5);", ["5"]),
  success("default function hoisting", "text(value()); export default function value() { return 6; }", ["6"]),
  success("named function export hoisting", "text(value()); export function value() { return 7; }", ["7"]),
  success("default class binding", "export default class Named {}; text(Named.name);", ["Named"]),
  success("anonymous default class name", "export default class { static { text(this.name); } }", ["default"]),
  success("parenthesized default class", "export default (class { static { text(this.name); } });", ["default"]),
  success("anonymous default function", "export default function() {}; text('complete');", ["complete"]),
  success("anonymous async default function", "export default async function() {}; text('complete');", ["complete"]),
  success("anonymous generator default function", "export default function*() {}; text('complete');", ["complete"]),
  success("anonymous async generator default", "export default async function*() {}; text('complete');", ["complete"]),
  success("parenthesized default expression", "export default (text(this));", ["undefined"]),
  success("default expression without semicolon", "export default text(this)", ["undefined"]),
  success("default expression await", "export default await text(10);", ["10"]),
  success("export comments", "export /* default */ default /* note */ (text(11));", ["11"]),
  success("default declaration boundary", "export default function() {}\n(text(12));", ["12"]),
  success("metadata object", `
    text(Object.getPrototypeOf(import.meta) === null);
    text(Object.keys(import.meta).length);
    import.meta.value = 13;
    text(import.meta.value);
    text((() => import.meta)() === import.meta);
    function f() { return import.meta; }
    text(f() === import.meta);
  `, ["true", "0", "13", "true", "true"]),
  success("metadata binding collision", `
    const __codeModeModuleMetadata0 = 14;
    const __codeModeModuleMetadata1 = 15;
    text(__codeModeModuleMetadata0 + __codeModeModuleMetadata1);
    text(Object.getPrototypeOf(import.meta) === null);
  `, ["29", "true"]),
  success("escaped metadata binding collision", String.raw`
    const __codeModeModuleMetadata\u0030 = 16;
    text(__codeModeModuleMetadata0);
    text(Object.getPrototypeOf(import.meta) === null);
  `, ["16", "true"]),
  success("metadata no global property", `
    text(import.meta);
    text(Object.getOwnPropertyNames(globalThis).some(name => name.startsWith("__codeModeModuleMetadata")));
  `, ["{}", "false"]),
  success("declarations do not become globals", "var local = 1; function f() {} text(globalThis.local); text(globalThis.f);", ["undefined", "undefined"]),
  success("hashbang", "#!/usr/bin/env unavailable\ntext(17);", ["17"]),
  success("source literals remain unchanged", 'text("this import.meta export default"); text(/this/.test("this"));', ["this import.meta export default", "true"]),
  success("dynamic import rejects", "text(await import('unavailable:module').then(() => false, () => true));", ["true"]),
  success("dynamic data import rejects", "text(await import('data:text/javascript,export default 42').then(() => false, () => true));", ["true"]),
  success("dynamic file import rejects", "text(await import('file:///definitely-not-present.js').then(() => false, () => true));", ["true"]),
  success("dynamic import argument exception", `
    let synchronous = false;
    let result;
    try { result = import((() => { throw 19; })()); } catch { synchronous = true; }
    text(synchronous);
    text(result === undefined ? undefined : await result.catch(value => value));
  `, ["true", "undefined"]),
  success("dynamic import generator argument", `
    function* f() { return import(yield 20); }
    const iterator = f();
    text(iterator.next().value);
    text(await iterator.next("unavailable:module").value.then(() => false, () => true));
  `, ["20", "true"]),
  success("dynamic import awaited argument", "text(await import(await Promise.resolve('unavailable:module')).then(() => false, () => true));", ["true"]),
  failure("top-level return", "text('must not execute'); return;"),
  failure("duplicate lexical binding", "text('must not execute'); let a; let a;"),
  failure("duplicate export", "text('must not execute'); const a = 1; export {a}; export {a};"),
  failure("missing export", "text('must not execute'); export {missing};"),
  failure("strict with statement", "text('must not execute'); with ({}) {}"),
  failure("top-level new target", "text('must not execute'); text(new.target);"),
  failure("strict assignment", "unbound = 21;"),
  failure("static import", "text('must not execute'); import 'unavailable:module';"),
  failure("named reexport", "text('must not execute'); export {value} from 'unavailable:module';"),
  failure("wildcard reexport", "text('must not execute'); export * from 'unavailable:module';"),
];

async function evaluate(source, lowered) {
  const output = [];
  const context = vm.createContext({
    text(value) {
      output.push(
        typeof value === "string" ? value : JSON.stringify(value) ?? String(value),
      );
    },
  }, { codeGeneration: { strings: false, wasm: false } });
  try {
    if (lowered) {
      await vm.runInContext(lowerModule(source), context);
    } else {
      const module = new vm.SourceTextModule(source, { context });
      await module.link(specifier => {
        throw new Error(`module imports are unavailable: ${specifier}`);
      });
      await module.evaluate();
    }
    return { ok: true, output };
  } catch (error) {
    return { ok: false, output, diagnostic: String(error) };
  }
}

for (const test of cases) {
  const baseline = await evaluate(test.source, false);
  const lowered = await evaluate(test.source, true);
  for (const [backend, result] of [["module", baseline], ["lowered", lowered]]) {
    assert.equal(result.ok, test.ok, `${test.name} (${backend}): ${result.diagnostic}`);
    assert.deepEqual(result.output, test.expected, `${test.name} (${backend})`);
  }
}
assert.throws(() => lowerModule(null), /module source must be a string/);
if (process.argv[3] === "--fixtures") {
  console.log(JSON.stringify(cases));
} else {
  console.log(`Module lowering: ${cases.length} differential cases and source validation passed.`);
}
