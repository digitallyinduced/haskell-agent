# Haskell versus JavaScript: audited fulfillment

## Scope

This exploratory benchmark targets a specific Haskell strength: checking a whole
tool-composition cell before any of its effects execute. It is not a claim that
Haskell prevents all partial effects or makes arbitrary workflows transactional.
The [protocol](code-mode-fulfillment-protocol.md) separates model-generated work
from deliberately injected faults. All inventory, payment, and shipment writes
are local simulations.

Each of ten paired variants has three orders: two eligible, one ineligible.
The workflow reads eligibility and amounts, reserves inventory, authorizes
payment, and creates shipments using the identifiers actually returned by the
previous operations. Writes are non-idempotent. The fixture records every
accepted write and rejected attempt under a lock; both runtimes use exactly the
same handlers and business rules. A state-read tool supports recovery.

The independent oracle requires exactly six accepted writes on the two eligible
orders. **Final-state correctness** is distinct from **clean completion**, which
also requires no rejected invalid, duplicate, or out-of-order attempts. Reported
partial effects count accepted operations on orders left unshipped, not orders.

## Controlled faults: compiler protection, not model reliability

For each fault, a valid inventory reservation precedes the faulty operation in
the same cell. No model or compiler-repair model is involved. Each row below
represents ten fresh variants per runtime; counts are totals across those ten.

| Deliberate fault | Haskell accepted writes / partial effects | JS accepted writes / partial effects | Haskell / JS rejected invalid attempts |
|---|---:|---:|---:|
| Wrong scalar argument type | 0 / 0 | 10 / 10 | 0 / 10 |
| Missing required field | 0 / 0 | 10 / 10 | 0 / 10 |
| Unknown tool function | 0 / 0 | 10 / 10 | 0 / 0 |
| Wrong identifier, correct scalar type | 10 / 10 | 10 / 10 | 10 / 10 |

Haskell rejected all thirty statically invalid cells before any callback.
JavaScript had already performed the valid reservation in all thirty. This is
the intended **compile-before-effects advantage**. The wrong-identifier control
shows its boundary: identifiers are ordinary `Text`/strings, not nominal types,
so both runtimes reserved inventory before the handler rejected the bad ID.
Neither runtime rolled back that reservation.

Rejected tool calls may return error envelopes rather than throw. Accordingly,
JavaScript's wrong-type/missing-field cells and both wrong-ID cells reported
cell completion despite a rejected operation. The audit—not the outer cell's
success flag—detects this. The unknown JS function throws before dispatching
that nonexistent callback, but after the preceding reservation. Rejected
requests are **not committed harmful writes**.

All twenty correct reference workflows passed, each making seven callbacks and
six writes, decoding real return values and passing their IDs to shipment
creation. The existing eighty read-only reference solutions, two callback-limit
termination checks, and direct audit rejection/duplicate checks also passed.
The final admission run's sources were unchanged and matched the source at
model-run launch.

Fault observations are conditional on the supplied errors. Ten deterministic
variants do not estimate the frequency of those mistakes in generated code.
They must not be pooled with model-generated success rates.

The practical distinction is cell-scoped: if a cell says “reserve inventory,
then authorize a payment” but passes a string where the payment amount must be
an integer, GHCi rejects the entire action before reserving anything. The JS
handler also rejects that payment, but the preceding reservation has already
happened. Splitting the Haskell workflow into separate cells would not protect
writes in an earlier successful cell. Neither approach protects against a
type-correct runtime exception or business-rule error after a write.

## Model-generated track

Completed 2026-09-27 at 09:54 UTC: all 100 scheduled trials are retained and
passed the answer/state oracle. Source hashes remained unchanged throughout.

### Fulfillment: equal correctness, fewer Haskell round trips

| Metric, ten trials per backend | Haskell | JavaScript |
|---|---:|---:|
| Correct final state | 10/10 | 10/10 |
| Clean completion | 10/10 | 10/10 |
| Accepted writes, total | 60 | 60 |
| Rejected invalid / duplicate / dependency attempts | 0 / 0 / 0 | 0 / 0 / 0 |
| Unfinished partial effects | 0 | 0 |
| Median end-to-end seconds | 11.97 | 12.17 |
| Exec calls, total | 10 | 21 |
| Main model turns, total | 20 | 31 |
| Nested callbacks, total | 70 | 97 |
| Compiler repair attempts, total | 1 | 0 |
| Input tokens, total including repair | 78,347 | 126,139 |
| Output tokens, total including repair | 3,135 | 2,314 |
| Cached input tokens, total | 29,184 | 46,592 |

All ten correctness pairs tied. Haskell was faster in four pairs, JavaScript in
six; the median paired difference (Haskell minus JS) was **+0.41 seconds**.
The marginal medians therefore do not establish a Haskell speed advantage.
The observed advantage is fewer round trips and about **38% fewer input tokens**
in this workflow, not fewer output tokens or demonstrated lower dollar cost.
Token usage is complete for every trial; input totals include cached tokens.

Haskell used one `mapConcurrently` cell in every trial. JavaScript used two cells
in nine trials and three in one, generally inspecting the plan before composing
the writes. Nine JS trials used concurrency; trial 9 processed orders serially.
Both runtimes used real returned IDs and preserved each order's dependencies.
Some JS responses counted printed results in the main conversation rather than
inside exec, a minor deviation from the calculation instruction. The independent
state audit still proves the actual fulfillment; this is not a strict
instruction-following success score.

Haskell trial 8 initially used unavailable `Tools.reservationId` and
`Tools.authorizationId` selector functions. The side model replaced them with
record-dot projections before any callback. The single repair took **7.08 s**,
with **2,185 input / 275 output tokens**, already included above. No workflow
cell failed after repair, and no write was replayed.

### Unchanged read-only controls

| Task | Haskell correct | JS correct | Haskell median s | JS median s |
|---|---:|---:|---:|---:|
| CSV summary | 10/10 | 10/10 | 14.13 | 8.98 |
| Log-tree audit | 10/10 | 10/10 | 11.91 | 10.10 |
| Paginated analytics | 10/10 | 10/10 | 8.29 | 6.59 |
| Customer/order join | 10/10 | 10/10 | 23.70 | 9.70 |

JS retained lower median latency in every read-only family. Haskell used 15
repair attempts in the controls and exposed two failed cells (one CSV, one
join), then recovered; JS used no repair and had no failed cells. All costs and
failed cells remain in the results rather than being excluded as warmups.

Ten successes out of ten give a nominal Wilson 95% interval of roughly
72%–100% for each task/backend. These are deterministic variants of a small
fixture, not independent samples of production workloads. There is **no observed
model-generated reliability advantage** here; the separate fault track isolates
the compiler's protection conditional on static mistakes.

Settings: `gpt-6-sol`, low effort; production Haskell compiler repair with
`gpt-6-luna`, low effort; alternating runtime order, fresh conversation and state
for every trial, eight main turns, 128 nested callbacks, and a 180-second limit.
Both backends receive the same business instructions and tool contracts. The
model chooses cell boundaries and concurrency; no single-cell solution is
forced. Timings include startup, model calls, repair, and cleanup. This measures
the current code-mode stacks, not optimized language throughput.

## Reproduction and artifacts

```sh
nix develop -c python3 scripts/evaluate-code-mode.py --fulfillment \
  --results-dir "$TMPDIR/fulfillment-preflight" --preflight
nix develop -c python3 scripts/evaluate-code-mode.py --fulfillment \
  --results-dir "$TMPDIR/fulfillment-evaluation" --trials 10
python3 scripts/summarize-code-mode-evaluation.py \
  "$TMPDIR/fulfillment-evaluation" --fulfillment --expected-trials 10
```

- [Reference and fault cells, diagnostics, audits, and callback counts](data/code-mode-fulfillment-preflight-results.json)
- [Read-only control preflight](data/code-mode-fulfillment-control-preflight-results.json)
- [Admission source manifest](data/code-mode-fulfillment-preflight-manifest.json)
- [All 100 model-trial results, including token usage and audits](data/code-mode-fulfillment-results.json)
- [All model/tool/repair traces](data/code-mode-fulfillment-traces.json)
- [Completed model-run source manifest](data/code-mode-fulfillment-run-manifest.json)

The [earlier read-only evaluations](haskell-vs-javascript-code-mode.md) remain
separate. One small simulated workflow is not broad evidence of language
superiority; production systems still need validation, idempotency, recovery,
and transaction boundaries regardless of code-mode language.
