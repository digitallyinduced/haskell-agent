#!/usr/bin/env bash
# From repository root: bash .../worker-tests.sh NATIVE_EXECUTABLE [ACORN LOWER_MODULE WORKER_JS]
set -euo pipefail
if [ "$#" -lt 1 ]; then
    echo 'usage: worker-tests.sh NATIVE_EXECUTABLE [ACORN LOWER_MODULE WORKER_JS]' >&2
    exit 2
fi
nix develop -c bun packages/agent-tools/benchmark/javascriptcore/worker-tests.mjs "$@"
