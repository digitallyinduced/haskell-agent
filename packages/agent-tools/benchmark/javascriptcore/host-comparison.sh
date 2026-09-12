#!/usr/bin/env bash
# Run from the repository root on macOS:
#   bash packages/agent-tools/benchmark/javascriptcore/host-comparison.sh
# Builds the real optimized Haskell host and a GC-rooted Nix native helper.
# All build products/results are retained under TMPDIR. Remove the printed
# artifact directory when finished (this also removes the helper's GC root).
# Requires the repository's Nix development shell and cached Cabal dependencies;
# the minimal project avoids unrelated source-repository-package downloads.
set -euo pipefail
: "${TMPDIR:?TMPDIR must be set}"
if [[ $(uname -s) != Darwin ]]; then
  echo "This benchmark requires macOS JavaScriptCore." >&2
  exit 1
fi
bench=packages/agent-tools/benchmark/javascriptcore
test -f "$bench/HostComparison.hs"
out=$(mktemp -d "$TMPDIR/javascriptcore-host.XXXXXX")
trap 'printf "Artifacts: %s\n" "$out"' EXIT

# Keep optimization in the project so cabal exec selects the same -O2 library.
cat > "$out/cabal.project" <<EOF
packages:
    "$PWD/packages/agent-tools"
    "$PWD/packages/agent-core"
    "$PWD/packages/agent-json"
    "$PWD/packages/agent-process"
    "$PWD/packages/agent-responses-types"
optimization: 2
EOF

nix build .#agent-code-mode-worker --out-link "$out/native-worker"
"$out/native-worker/bin/agent-code-mode-worker" --check
export CODE_MODE_HOST_BENCH_OUT="$out"
nix develop -c bash -euo pipefail -c '
out=$CODE_MODE_HOST_BENCH_OUT
bench=packages/agent-tools/benchmark/javascriptcore
unset AGENT_CODE_MODE_WORKER AGENT_CODE_MODE_BACKEND
cabal build agent-tools:lib:agent-tools --offline \
  --project-file="$out/cabal.project" --builddir="$out/build" \
  --enable-optimization=2
mkdir -p "$out/comparison"
cabal exec --project-file="$out/cabal.project" --builddir="$out/build" -- \
  ghc -O2 -threaded -rtsopts -package agent-tools \
  "$bench/HostComparison.hs" -outputdir "$out/comparison" -o "$out/compare"
"$out/compare" "$PWD/packages/agent-tools/data/code-mode/worker.mjs" \
  "$out/native-worker/bin/agent-code-mode-worker" +RTS -N4 \
  | tee "$out/results.txt"
'
