#!/usr/bin/env bash
# From the repository root on macOS:
#   bash packages/agent-tools/benchmark/javascriptcore/host-tests.sh [PACKAGED_WORKER]
# Without an argument, build and GC-root the Nix helper. CI can reuse its already
# rooted helper. Uses only Nix-provided dependencies and TMPDIR build artifacts;
# never stages files or changes the repository's Cabal project.
set -euo pipefail
: "${TMPDIR:?TMPDIR must be set}"
[[ $(uname -s) == Darwin ]] || { echo "Requires macOS." >&2; exit 1; }
[[ $# -le 1 ]] || { echo "usage: $0 [PACKAGED_WORKER]" >&2; exit 1; }
test -f packages/agent-tools/test/Agent/Tools/CodeMode/HostSpec.hs
out=$(mktemp -d "$TMPDIR/javascriptcore-host-tests.XXXXXX")
trap 'printf "Artifacts: %s\n" "$out"' EXIT
cat > "$out/cabal.project" <<EOF
packages:
    "$PWD/packages/agent-tools"
    "$PWD/packages/agent-core"
    "$PWD/packages/agent-json"
    "$PWD/packages/agent-process"
    "$PWD/packages/agent-responses-types"
optimization: 2
tests: True
-- All external dependencies are supplied by the pinned Nix shell.
active-repositories: :none
EOF
if [[ $# == 1 ]]; then
  worker=$1
else
  nix build .#agent-code-mode-worker --no-write-lock-file --out-link "$out/native-worker"
  worker="$out/native-worker/bin/agent-code-mode-worker"
fi
[[ "$worker" == /* && -x "$worker" ]] || {
  echo "PACKAGED_WORKER must be an absolute executable path." >&2
  exit 1
}
export CODE_MODE_HOST_TEST_OUT="$out"
export AGENT_CODE_MODE_BACKEND=javascriptcore
export AGENT_CODE_MODE_WORKER="$worker"
# GNU timeout comes from the Nix development environment. Bound compilation and
# tests together, then separately bound the executable to catch lifecycle hangs.
nix develop .#code-mode --no-write-lock-file --command timeout --kill-after=30s 25m \
  bash -euo pipefail -c '
out=$CODE_MODE_HOST_TEST_OUT
timeout --kill-after=5s 15s "$AGENT_CODE_MODE_WORKER" --check
cabal build agent-tools:test:agent-tools-test --offline \
  --project-file="$out/cabal.project" --builddir="$out/build"
test_binary=$(cabal list-bin agent-tools:test:agent-tools-test \
  --project-file="$out/cabal.project" --builddir="$out/build")
# Match cabal test/repl working-directory semantics for the explicit Bun fixtures.
cd packages/agent-tools
timeout --kill-after=10s 3m "$test_binary" --no-color --match "code-mode Bun host" \
  | tee "$out/results.txt"
# Fail closed if the matcher stops selecting tests, or availability skips them.
grep -Eq "^[1-9][0-9]* examples?, 0 failures?$" "$out/results.txt"
'
