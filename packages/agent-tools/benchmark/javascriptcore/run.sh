#!/usr/bin/env bash
# Run from the repository root on macOS: bash packages/agent-tools/benchmark/javascriptcore/run.sh
set -euo pipefail
: "${TMPDIR:?TMPDIR must be set}"
bench=packages/agent-tools/benchmark/javascriptcore
out=$(mktemp -d "$TMPDIR/javascriptcore.XXXXXX")
export CODE_MODE_BENCH_OUT="$out"
nix develop -c bash -euo pipefail -c '
bench=packages/agent-tools/benchmark/javascriptcore
out=$CODE_MODE_BENCH_OUT
mkdir -p "$out/tests" "$out/comparison"
ghc -O2 -threaded -rtsopts -Wall -framework JavaScriptCore -i"$bench" \
  "$bench/JavaScriptCoreTests.hs" -outputdir "$out/tests" -stubdir "$out/tests" -o "$out/tests/run"
"$out/tests/run" +RTS -N4
cabal build agent-tools:lib:agent-tools --enable-optimization=2
cabal exec -- ghc -O2 -threaded -rtsopts -package agent-tools -framework JavaScriptCore \
  -i"$bench" "$bench/Comparison.hs" -outputdir "$out/comparison" -stubdir "$out/comparison" -o "$out/compare"
for run in 1 2; do
  "$out/compare" "$PWD/packages/agent-tools/data/code-mode/worker.mjs" 30 7 +RTS -N4 | tee "$out/results-$run.txt"
done
'
printf 'Artifacts: %s\n' "$out"
