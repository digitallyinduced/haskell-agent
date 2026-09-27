# Code-mode fulfillment evaluation protocol

Specified before model-trial collection. Hypothesis: whole-cell Haskell
typechecking prevents partial effects caused by later type/name errors. This
does not imply transactions, rollback, nominal identifier safety, or protection
against type-correct business-logic mistakes.

## Model-generated track

Run ten deterministic paired variants of a simulated order-fulfillment workflow,
plus the existing four read-only task families unchanged as controls: 100 trials.
Each workflow has three orders, including an ineligible order. Read the plan,
reserve eligible inventory, authorize the corresponding payment, and create
shipments in dependency order. Never operate on ineligible orders or repeat
already successful writes. State and an append-only audit remain available to
the evaluator even when the model fails. All services are local simulations;
no real payments, inventory changes, or shipments are performed.

Use the existing main model (gpt-6-sol, low), production Haskell compiler repair
(gpt-6-luna, low), alternating backend order, fresh state/conversation per trial,
eight main turns, 128 callbacks, and a 180-second deadline. Count repair time and
tokens. No backend-specific business hints, forced single-cell solutions,
handwritten nominal identifier wrappers, or production changes to favor a
backend. Both receive identical tool contracts and business instructions.

Report final-state correctness separately from clean completion, rejected write
attempts, duplicate attempts, accepted writes, and unfinished partial effects.
Rejected invalid requests are not successful harmful writes. Retain all failed
trials and report latency and token accounting including repair. Read-only
controls remain separate from the new workflow; do not pool fault injection
with model-generated reliability.

## Controlled fault-injection track

Execute known-correct reference cells and paired single-fault variants through
both real hosts without a model or automatic repair. Place the fault after an
otherwise-valid write prefix. Cases: wrong scalar argument type, omitted
required argument, unknown tool function, and a same-typed wrong identifier as
a negative control. Record calls, accepted effects, rejection diagnostics, and
remaining state. Run each against all ten variants with fresh state.

This track measures behavior conditional on deliberately supplied faults, not
how frequently models make them. Typechecking is expected to prevent effects
for statically detectable faults; a same-typed wrong identifier should expose
the boundary. JavaScript runtime validation must be retained, not weakened.

## Admission and interpretation

Before paid calls, validate actual visible tool return values and generated
bindings, reference solutions, independent final-state oracles, audit rejection
and duplicate accounting, and callback-limit cancellation. Freeze source before
collection and record hashes. Fix harness defects before collecting results;
never selectively replace unsuccessful model samples.

Ten variants of one workflow are exploratory, not broad reliability evidence.
Report wins, ties, and regressions. Learned JSON shapes that remain Value do not
provide static return-field guarantees; Text identifiers are not nominal types.
Elapsed measurements are end-to-end behavioral measurements using the existing
GHCi evaluation harness, not optimized language throughput benchmarks.
