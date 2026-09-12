// Differential rollout gate, not a claim that the prototype is production-safe.
// Only run these bounded, trusted fixtures against the in-process prototype.
import { spawn, spawnSync } from 'node:child_process';
import { createInterface } from 'node:readline';
import { isDeepStrictEqual } from 'node:util';

const [nativeProbe, workerPath] = process.argv.slice(2);
if (!nativeProbe || !workerPath) throw new Error('usage: compatibility.mjs NATIVE_PROBE WORKER_PATH');

const cases = [
  ['text', "text('ok');", ['ok']],
  ['top-level await', "text(await Promise.resolve('ok'));", ['ok']],
  ['module this', 'text(this === undefined);', ['true']],
  ['module arguments', 'text(typeof arguments);', ['undefined']],
  ['top-level return', 'return 1;', null],
  ['export declaration', 'export const value = 42; text(value);', ['42']],
  ['import.meta', 'text(typeof import.meta);', ['object']],
  ['strict assignment', 'undeclaredCompatibilityValue = 1;', null],
  ['direct eval', "text(eval('42'));", null],
  ['indirect eval', "text((0, eval)('42'));", null],
  ['Function constructor', "text(Function('return 42')());", null],
  ['function constructor alias', "text((() => {}).constructor('return 42')());", null],
  ['async constructor alias', "text(await (async () => {}).constructor('return 42')());", null],
  ['generator constructor alias', "text((function*() {}).constructor('yield 42')().next().value);", null],
  ['async generator constructor alias', "text((await (async function*() {}).constructor('yield 42')().next()).value);", null],
];

async function runBun(source) {
  const worker = spawn(process.execPath, [workerPath], { stdio: ['pipe', 'pipe', 'pipe'] });
  const lines = createInterface({ input: worker.stdout });
  let stderr = '';
  worker.stderr.on('data', chunk => { stderr += chunk; });
  const deadline = setTimeout(() => worker.kill('SIGKILL'), 5000);
  const exited = new Promise(resolve => worker.once('close', resolve));
  worker.on('error', error => worker.stdout.destroy(error));
  const content = [];
  try {
    for await (const line of lines) {
      const message = JSON.parse(line);
      if (message.method === 'ready') {
        worker.stdin.write(JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'exec', params: {
          source, tools: [], stored_values: {}, image_detail_visible: false,
        } }) + '\n');
      } else if (message.method === 'content') {
        content.push(message.params.value);
      } else if (message.id === 1) {
        if (message.error) return null;
        const items = [...content, ...(message.result?.content ?? [])];
        return items.map(item => {
          if (item.type !== 'text') throw new Error('unexpected content: ' + JSON.stringify(item));
          return item.text;
        });
      }
    }
    throw new Error('Bun worker exited without a response: ' + stderr);
  } finally {
    clearTimeout(deadline);
    lines.close();
    worker.kill('SIGKILL');
    await exited;
  }
}

let differences = 0;
let baselineFailures = 0;
for (const [label, source, expected] of cases) {
  const baseline = await runBun(source);
  const native = spawnSync(nativeProbe, [source], { encoding: 'utf8', timeout: 5000 });
  if (native.error || native.status !== 0) throw new Error('native probe failed: ' + (native.error ?? native.stderr));
  const [status, ...output] = native.stdout.trimEnd().split('\n');
  if (!['ok', 'error'].includes(status)) throw new Error('invalid native response: ' + native.stdout);
  const candidate = status === 'error' ? null : output;
  const matches = isDeepStrictEqual(baseline, candidate);
  if (!matches) differences++;
  if (!isDeepStrictEqual(baseline, expected)) baselineFailures++;
  console.log(JSON.stringify({ label, expected, bun: baseline, javascriptcore: candidate, matches }));
}
console.log(JSON.stringify({ summary: { cases: cases.length, differences, baselineFailures } }));
process.exitCode = differences || baselineFailures ? 1 : 0;
