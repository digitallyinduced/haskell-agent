#!/usr/bin/env bash
set -euo pipefail

# Run from the repository root inside `nix develop`.
directory="$(mktemp -d "${TMPDIR:?}/clipboard-types-benchmark.XXXXXX")"
trap 'rm -rf "$directory"' EXIT
cc -O2 -Wall -Wextra -Werror -Wno-deprecated-declarations \
    packages/agent-cli/cbits/ClipboardTypes.c \
    packages/agent-cli/benchmark/ClipboardTypesLatency.c \
    -framework ApplicationServices -framework CoreServices \
    -o "$directory/clipboard-types-benchmark"
"$directory/clipboard-types-benchmark" "${1:-7}"
