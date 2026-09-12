#!/usr/bin/env bash
# Run from the repository root on macOS. A nonzero result blocks rollout.
set -euo pipefail
: "${TMPDIR:?TMPDIR must be set}"
export CODE_MODE_COMPATIBILITY_OUT
CODE_MODE_COMPATIBILITY_OUT=$(mktemp -d "$TMPDIR/javascriptcore-compatibility.XXXXXX")
printf 'Artifacts: %s\n' "$CODE_MODE_COMPATIBILITY_OUT"
nix develop -c bash -euo pipefail -c '
bench=packages/agent-tools/benchmark/javascriptcore
out=$CODE_MODE_COMPATIBILITY_OUT
ghc -O2 -threaded -Wall -framework JavaScriptCore -i"$bench" \
  "$bench/CompatibilityProbe.hs" -outputdir "$out" -stubdir "$out" -o "$out/probe"
bun "$bench/compatibility.mjs" "$out/probe" "$PWD/packages/agent-tools/data/code-mode/worker.mjs" | tee "$out/results.jsonl"
'
