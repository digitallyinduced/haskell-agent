#!/usr/bin/env bash
set -euo pipefail

# Run from the repository root inside `nix develop`.
if [[ "$(uname -s)" != Darwin ]]; then
    echo "Clipboard metadata tests require macOS." >&2
    exit 1
fi
directory="$(mktemp -d "${TMPDIR:?}/clipboard-types.XXXXXX")"
trap 'rm -rf "$directory"' EXIT
cc -O2 -Wall -Wextra -Werror -Wno-deprecated-declarations \
    packages/agent-cli/cbits/ClipboardTypes.c \
    packages/agent-cli/test/ClipboardTypes.c \
    -framework ApplicationServices -framework CoreServices \
    -o "$directory/clipboard-types-test"
"$directory/clipboard-types-test"
