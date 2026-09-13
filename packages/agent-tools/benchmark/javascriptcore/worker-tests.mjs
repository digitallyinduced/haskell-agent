// Bounded protocol/conformance checks, not a security audit or performance test.
// Usage: bun worker-tests.mjs NATIVE_EXECUTABLE [ACORN LOWER_MODULE WORKER_JS]
// Use --baseline-only instead of NATIVE_EXECUTABLE to check fixture expectations.
import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { createInterface } from 'node:readline';
import { fileURLToPath } from 'node:url';
import { realpathSync } from 'node:fs';
import { dirname, resolve } from 'node:path';

const [nativeExecutable, ...nativeArguments] = process.argv.slice(2);
if (!nativeExecutable) throw new Error('usage: worker-tests.mjs NATIVE_EXECUTABLE [ACORN LOWER_MODULE WORKER_JS]');
const baselineOnly = nativeExecutable === '--baseline-only';
const bunCommand = [process.execPath, fileURLToPath(new URL('../../data/code-mode/worker.mjs', import.meta.url))];
const nativeCommand = [nativeExecutable, ...nativeArguments];
const tools = [{ name: 'echo', description: 'Echo JSON' }, { name: 'reject', description: 'Reject' }];
const text = value => ({ type: 'text', text: String(value) });
const expected = (content = [], overrides = {}) => ({
  failed: false, content: content.map(text), writes: {}, notifications: [], yields: [], calls: [],
  ...overrides,
});

class Worker {
  constructor(command) {
    this.process = spawn(command[0], command.slice(1), { stdio: ['pipe', 'pipe', 'pipe'] });
    this.watchdog = setTimeout(() => {
      this.fail(new Error('test worker exceeded 30-second lifetime'));
      this.process.kill('SIGKILL');
    }, 30000);
    this.queue = [];
    this.waiter = null;
    this.failure = null;
    this.stderr = '';
    this.nextId = 0;
    this.held = [];
    this.process.stderr.on('data', bytes => {
      this.stderr = (this.stderr + bytes.toString()).slice(-8192);
    });
    this.process.stdin.on('error', error => this.fail(error));
    this.process.on('error', error => this.fail(error));
    this.closed = new Promise(resolve => this.process.once('close', (code, signal) => {
      clearTimeout(this.watchdog);
      this.fail(new Error(`worker closed (${code}, ${signal}): ${this.stderr}`));
      resolve({ code, signal });
    }));
    this.lines = createInterface({ input: this.process.stdout });
    this.lines.on('line', line => {
      try {
        const message = JSON.parse(line);
        assert.equal(message.jsonrpc, '2.0');
        if (this.waiter) {
          const waiter = this.waiter;
          this.waiter = null;
          waiter.resolve(message);
        } else {
          if (this.queue.length >= 4096) throw new Error('test protocol queue limit exceeded');
          this.queue.push(message);
        }
      } catch (error) { this.fail(error); }
    });
  }
  fail(error) {
    this.failure ??= error;
    if (this.waiter) {
      const waiter = this.waiter;
      this.waiter = null;
      waiter.reject(error);
    }
  }
  async next(timeout = 5000) {
    if (this.queue.length) return this.queue.shift();
    if (this.failure) throw this.failure;
    assert.equal(this.waiter, null, 'only one protocol reader may wait');
    return await new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.waiter = null;
        reject(new Error('protocol deadline exceeded'));
      }, timeout);
      this.waiter = {
        resolve: value => { clearTimeout(timer); resolve(value); },
        reject: error => { clearTimeout(timer); reject(error); },
      };
    });
  }
  send(value) { this.process.stdin.write(JSON.stringify({ jsonrpc: '2.0', ...value }) + '\n'); }
  async ready() { assert.equal((await this.next()).method, 'ready'); }
  start(source, options = {}) {
    const id = ++this.nextId;
    this.send({ id, method: 'exec', params: {
      source, tools, stored_values: {}, image_detail_visible: false, ...options,
    } });
    return id;
  }
  async run(source, options = {}, holdTools = false) {
    const id = this.start(source, options);
    const observed = expected();
    for (;;) {
      const message = await this.next();
      if (message.method === 'content') observed.content.push(message.params.value);
      else if (message.method === 'notify') observed.notifications.push(message.params.text);
      else if (message.method === 'yield') observed.yields.push(message.params.value);
      else if (message.method === 'tool/call') {
        assert.ok(tools.some(tool => tool.name === message.params.name), 'unapproved tool escaped worker');
        observed.calls.push(message.params);
        const response = message.params.name === 'reject'
          ? { id: message.id, error: { code: -32000, message: 'fixture rejection' } }
          : { id: message.id, result: message.params.arguments };
        if (holdTools) this.held.push(response);
        else if (this.respond) this.respond(message);
        else this.send(response);
      } else {
        assert.equal(message.id, id, 'unexpected response ID or notification');
        observed.failed = Boolean(message.error);
        observed.writes = message.stored_value_writes ?? {};
        observed.content.push(...(message.result?.content ?? message.partial_result?.content ?? []));
        return observed;
      }
    }
  }
  async stop() {
    this.process.kill('SIGKILL');
    await this.closed;
    this.lines.close();
  }
}

const fixtures = [
  ['timer callback receiver', "await new Promise(resolve=>setTimeout(function(){text(this === undefined); resolve();},0));", expected(['true'])],
  ['text and Unicode', "text('hello'); text('😀\\u0000é'); text(undefined); text(123n);", expected(['hello', '😀\0é', 'undefined', '123'])],
  ['await', "text(await Promise.resolve('done'));", expected(['done'])],
  ['parallel callbacks', 'text(await Promise.all([tools.echo({x:1}), tools.echo({x:2})]));',
    expected(['[{"x":1},{"x":2}]'], { calls: [{ name: 'echo', arguments: { x: 1 } }, { name: 'echo', arguments: { x: 2 } }] })],
  ['tool rejection', "try { await tools.reject({}); } catch { text('caught'); }",
    expected(['caught'], { calls: [{ name: 'reject', arguments: {} }] })],
  ['forbidden tool', 'await tools.notAdvertised({});', expected([], { failed: true })],
  ['invalid tool argument', 'await tools.echo(null);', expected([], { failed: true })],
  ['multiple tool arguments', 'await tools.echo({}, {});', expected([], { failed: true })],
  ['metadata', 'text(ALL_TOOLS); text(Object.isFrozen(ALL_TOOLS) && Object.isFrozen(ALL_TOOLS[0]));',
    expected([JSON.stringify(tools), 'true'])],
  ['stored value clones', "const a=load('initial'); a.x=9; text(load('initial')); store('k',{a:2}); const b=load('k'); b.a=3; text(load('k'));",
    expected(['{"x":1}', '{"a":2}'], { writes: { k: { a: 2 } } }), { stored_values: { initial: { x: 1 } } }],
  ['store after partial failure', "store('saved',42); text('before'); throw Error('bad');",
    expected(['before'], { failed: true, writes: { saved: 42 } })],
  ['invalid store', "store('bad', undefined);", expected([], { failed: true })],
  ['notify and yield', "text('before'); notify('notice'); yield_control(); text('after');",
    expected(['before', 'after'], { notifications: ['notice'], yields: [{ content: [] }] })],
  ['empty notify', "notify('  ');", expected([], { failed: true })],
  ['exit', "text('before'); exit(); text('after');", expected(['before'])],
  ['timer await', "await new Promise(resolve => setTimeout(() => { text('timer'); resolve(); }, 1));", expected(['timer'])],
  ['timer cancellation', "const id=setTimeout(() => text('wrong'),1); clearTimeout(id); await new Promise(r=>setTimeout(r,5)); text('done');", expected(['done'])],
  ['timer rejection', "setTimeout(() => { throw Error('timer'); }, 1); await new Promise(()=>{});", expected([], { failed: true })],
  ['image hidden detail', "image('data:image/png;base64,YQ==', 'invalid');",
    expected([], { content: [{ type: 'image', image_url: 'data:image/png;base64,YQ==' }] })],
  ['image detail', "image({type:'image',data:'YQ==',mimeType:'image/png',_meta:{'codex/imageDetail':'low'}}, 'original');",
    expected([], { content: [{ type: 'image', image_url: 'data:image/png;base64,YQ==', detail: 'original' }] }), { image_detail_visible: true }],
  ['audio', "audio({type:'audio',data:'YQ==',mime_type:'audio/wav'});",
    expected([], { content: [{ type: 'audio', audio_url: 'data:audio/wav;base64,YQ==' }] })],
  ['generated image', "generatedImage({image_url:'data:image/png;base64,YQ==',output_hint:'saved'});",
    expected([], { content: [{ type: 'image', image_url: 'data:image/png;base64,YQ==' }, text('saved')] })],
  ['blocked globals', "text([process,require,fetch,WebSocket,WebAssembly,SharedArrayBuffer,Atomics,console,setInterval].every(x=>x===undefined));", expected(['true'])],
  ['module this', 'text(this===undefined); text((()=>this)()===undefined);', expected(['true', 'true'])],
  ['module arguments', 'text(typeof arguments);', expected(['undefined'])],
  ['lexical class this', 'class C { field=this; static field=this; [this===undefined ? "method" : "wrong"](){return this;} } const c=new C; text(c.field===c); text(C.field===C); text(c.method()===c);', expected(['true', 'true', 'true'])],
  ['shadowed undefined', 'const undefined=42; text(this===void 0);', expected(['true'])],
  ['module return', 'return 1;', expected([], { failed: true })],
  ['exports', 'export const x=42; export {x as answer}; text(x);', expected(['42'])],
  ['default export', "export default function named(){return 42;} text(named());", expected(['42'])],
  ['default class export', "export default class Named { static value=42; } text(Named.value);", expected(['42'])],
  ['import meta', 'text(Object.getPrototypeOf(import.meta)===null); text(import.meta===import.meta);', expected(['true', 'true'])],
  ['static import', "text('must not execute'); import x from 'unavailable';", expected([], { failed: true })],
  ['dynamic import', "try { await import('data:text/javascript,export default 42'); text('loaded'); } catch { text('blocked'); }", expected(['blocked'])],
  ['dynamic import argument failure', "let p; try { p=import((()=>{throw Error('argument')})()); text('promise'); } catch { text('sync'); } try { await p; } catch { text('rejected'); }", expected(['sync'])],
  ['strict assignment', 'unboundFixtureValue=1;', expected([], { failed: true })],
  ['eval', "eval('42');", expected([], { failed: true })],
  ['indirect eval', "(0,eval)('42');", expected([], { failed: true })],
  ['Function', "Function('return 42')();", expected([], { failed: true })],
  ['ordinary constructor', "(()=>{}).constructor('return 42')();", expected([], { failed: true })],
  ['async constructor', "await (async()=>{}).constructor('return 42')();", expected([], { failed: true })],
  ['generator constructor', "(function*(){}).constructor('yield 42')().next();", expected([], { failed: true })],
  ['async generator constructor', "await (async function*(){}).constructor('yield 42')().next();", expected([], { failed: true })],
  ['helper overwrite', "text=()=>{};", expected([], { failed: true })],
  ['JSON builtin mutation', "JSON.stringify=()=> 'forged'; text({real:42});",
    expected(['{"real":42}'])],
];

let checks = 0;
async function withWorker(command, action) {
  const worker = new Worker(command);
  try { await worker.ready(); return await action(worker); }
  finally { await worker.stop(); }
}
async function runFixtures(command, name) {
  await withWorker(command, async worker => {
    for (const [label, source, wanted, options] of fixtures) {
      let observed;
      try { observed = await worker.run(source, options); }
      catch (error) { throw new Error(`${name}: ${label}: ${error.message}`, { cause: error }); }
      assert.deepEqual(observed, wanted, `${name}: ${label}`);
      checks++;
    }
    assert.deepEqual(await worker.run('globalThis.fixtureSecret=42;'), expected(), `${name}: write global`);
    assert.deepEqual(await worker.run('text(typeof fixtureSecret);'), expected(['undefined']), `${name}: fresh context`);
    checks += 2;
    // A hostile Promise prototype can affect module evaluation itself in Bun.
    // Success/failure is deliberately not asserted here; protocol integrity is.
    const hostile = await worker.run("Promise.prototype.then=()=>{throw Error('forged')}; text('real');");
    assert.deepEqual(hostile.content, [text('real')], `${name}: builtin mutation cannot forge output`);
    assert.deepEqual(hostile.calls, [], `${name}: builtin mutation cannot forge calls`);
    assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']), `${name}: recovery after builtin mutation`);
    checks += 2;
    const abandoned = await worker.run("void tools.echo({late:true}); text('finished');", {}, true);
    assert.deepEqual(abandoned, expected(['finished'], { calls: [{ name: 'echo', arguments: { late: true } }] }));
    for (const response of worker.held.splice(0)) worker.send(response);
    assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']), `${name}: late callback recovery`);
    checks += 2;
  });
  // Hard termination is intentionally external. A yield is observation only,
  // and neither JS loop nor Promise churn may trap process-group cancellation.
  for (const [label, source] of [
    ['CPU loop', "text('started'); while(true){}"],
    ['Promise churn', "text('started'); await new Promise(()=>{ const again=()=>Promise.resolve().then(again); again(); });"],
  ]) {
    await withWorker(command, async worker => {
      worker.start(source);
      const started = await worker.next();
      assert.deepEqual(started.params?.value, text('started'), `${name}: ${label} reached execution`);
      const deadline = setTimeout(() => worker.process.kill('SIGKILL'), 250);
      try {
        const result = await worker.closed;
        assert.equal(result.signal, 'SIGKILL', `${name}: ${label} externally terminated`);
      } finally { clearTimeout(deadline); }
    });
    await withWorker(command, async worker =>
      assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']), `${name}: recovery after ${label}`));
    checks += 2;
  }
  console.log(`${name}: protocol, fresh contexts, late responses, and hard termination passed`);
}

await runFixtures(bunCommand, 'Bun');
if (!baselineOnly) {
  await runFixtures(nativeCommand, 'JavaScriptCore');
  await withWorker(nativeCommand, async worker => {
    const calls = [{ name: 'echo', arguments: {} }];
    for (const value of [null, false, 0, '😀\0"\\']) {
      worker.respond = request => {
        // Alternate JSON spelling of the same ID exercises native routing
        // without assuming the helper's generated ID format.
        const escapedId = JSON.stringify(request.id).replace(/[a-z0-9]/gi,
          character => `\\u${character.charCodeAt(0).toString(16).padStart(4, '0')}`);
        worker.process.stdin.write(`{"jsonrpc":"2.0","id":${escapedId},"result":${JSON.stringify(value)}}\n`);
      };
      assert.deepEqual(await worker.run('text(await tools.echo({}));'),
        expected([value === null ? 'null' : value], { calls }), 'native scalar/escaped response roundtrip');
      checks++;
    }
    const poison = "for(const key of ['result','error','id','message','method']) Object.defineProperty(Object.prototype,key,{get(){throw Error('inherited envelope getter');},configurable:true});";
    worker.respond = request => worker.send({ id: request.id, result: false });
    assert.deepEqual(await worker.run(`${poison} text(await tools.echo({}));`),
      expected(['false'], { calls }), 'native successful response does not read inherited error/method');
    worker.respond = request => worker.send({ id: request.id, error: {} });
    assert.deepEqual(await worker.run(`${poison} try{await tools.echo({});}catch{ text('caught'); }`),
      expected(['caught'], { calls }), 'native error response does not read inherited result/message');
    worker.respond = undefined;
    assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']),
      'native recovery after response prototype mutation');
    checks += 3;
  });
  for (const raw of ['{', 'null', '[]', '{"jsonrpc":"1.0","id":"bad","result":false}']) {
    await withWorker(nativeCommand, async worker => {
      worker.start('await tools.echo({});');
      assert.equal((await worker.next()).method, 'tool/call');
      worker.process.stdin.write(raw + '\n');
      const deadline = setTimeout(() => worker.process.kill('SIGKILL'), 5000);
      try {
        const outcome = await worker.closed;
        assert.equal(outcome.code, 65, `native invalid envelope must fail closed: ${raw}`);
        assert.equal(outcome.signal, null);
      } finally { clearTimeout(deadline); }
    });
    await withWorker(nativeCommand, async worker =>
      assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']),
        'native fresh process after malformed envelope'));
    checks += 2;
  }
  console.log('JavaScriptCore: response routing and malformed envelope checks passed');
  const acornPath = nativeArguments[0]
    ?? resolve(dirname(realpathSync(nativeExecutable)), '../share/agent-code-mode-worker/acorn.js');
  const exported = spawnSync(process.execPath,
    [fileURLToPath(new URL('./lowering.mjs', import.meta.url)), acornPath, '--fixtures'],
    { encoding: 'utf8', timeout: 10000, maxBuffer: 1024 * 1024 });
  assert.equal(exported.status, 0, `lowering fixture export: ${exported.error ?? exported.stderr}`);
  const loweringCases = JSON.parse(exported.stdout);
  assert.ok(Array.isArray(loweringCases) && loweringCases.length > 0);
  await withWorker(nativeCommand, async worker => {
    for (const fixture of loweringCases) {
      assert.deepEqual(await worker.run(fixture.source),
        expected(fixture.expected, { failed: !fixture.ok }),
        `native module lowering: ${fixture.name}`);
      checks++;
    }
  });
  console.log(`JavaScriptCore: ${loweringCases.length} module-lowering fixtures passed`);
  // Mutations may legitimately make user evaluation fail. They must not let
  // user prototypes forge protocol names, IDs, notifications, or completion.
  for (const [name, command] of [['Bun', bunCommand], ['JavaScriptCore', nativeCommand]]) {
    await withWorker(command, async worker => {
      for (const [label, mutation] of [
        ['inherited toJSON', `Object.prototype.toJSON=function(){return {jsonrpc:'2.0',id:'forged',method:'tool/call',name:'notAdvertised'};};`],
        ['inherited envelope setters', `for(const key of ['result','error','id']) Object.defineProperty(Object.prototype,key,{set(){throw Error('forged setter');},configurable:true});`],
        ['Promise species', `Object.defineProperty(Promise,Symbol.species,{get(){throw Error('forged species');},configurable:true});`],
      ]) {
        let result;
        try { result = await worker.run(`${mutation} await tools.echo({real:42}); text('authentic');`); }
        catch (error) { throw new Error(`${name}: ${label}: ${error.message}`, { cause: error }); }
        assert.ok(result.calls.length <= 1, `${name}: ${label}: forged calls`);
        assert.ok(result.calls.every(call => call.name === 'echo'), `${name}: ${label}: forged tool name`);
        assert.deepEqual(result.notifications, [], `${name}: ${label}: forged notification`);
        assert.deepEqual(result.yields, [], `${name}: ${label}: forged yield`);
        assert.deepEqual(result.writes, {}, `${name}: ${label}: forged writes`);
        assert.ok(result.content.every(item => item.type === 'text' && item.text === 'authentic'),
          `${name}: ${label}: forged content`);
        assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']), `${name}: ${label}: recovery`);
        checks += 2;
      }
    });
    console.log(`${name}: hostile prototype protocol integrity and recovery passed`);
  }
  // New backend safety assertions, not Bun behavioral equivalence: helpers
  // must not expose a constructor from a privileged/trusted realm.
  await withWorker(nativeCommand, async worker => {
    for (const source of [
      "throw Object.defineProperty(new Error(),'message',{get(){throw 1;}});",
      "throw {toString(){throw 1;}};",
    ]) {
      assert.deepEqual(await worker.run(source), expected([], { failed: true }),
        'native hostile exception formatting returns a cell error');
      assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']),
        'native recovery after hostile exception formatting');
      checks += 2;
    }
    for (const helper of ['text', 'image', 'audio', 'notify', 'store', 'load', 'setTimeout']) {
      assert.deepEqual(await worker.run(`${helper}.constructor("return 42")();`),
        expected([], { failed: true }), `native helper constructor: ${helper}`);
      checks++;
    }
    assert.deepEqual(await worker.run("text(typeof __native); text(typeof acorn); text(typeof lowerModule);"),
      expected(['undefined', 'undefined', 'undefined']), 'native driver/parser hooks are private');
    checks++;
  });
  // Finite allocation (at most 640 MiB plus engine overhead), never an
  // unbounded allocation loop. Touch every byte so RSS, not reservation,
  // exercises the native worker's default 512-MiB sampled watchdog.
  await withWorker(nativeCommand, async worker => {
    worker.start("text('allocating'); const blocks=globalThis.fixtureBlocks=[]; for(let i=0;i<160;i++){const block=new Uint8Array(4*1024*1024); block.fill(1); blocks.push(block);} await new Promise(()=>{});");
    assert.deepEqual((await worker.next()).params?.value, text('allocating'));
    const deadline = setTimeout(() => worker.process.kill('SIGKILL'), 5000);
    try {
      const outcome = await worker.closed;
      assert.equal(outcome.code, 75, `native memory watchdog must exit, not test timeout: ${worker.stderr}`);
      assert.equal(outcome.signal, null);
    } finally { clearTimeout(deadline); }
  });
  await withWorker(nativeCommand, async worker =>
    assert.deepEqual(await worker.run("text('recovered');"), expected(['recovered']), 'native recovery after RSS termination'));
  checks += 2;
}
console.log(`${checks} checks passed${baselineOnly ? ' (baseline only)' : ''}.`);
