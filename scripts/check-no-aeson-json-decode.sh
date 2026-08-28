#!/usr/bin/env bash
set -euo pipefail

# Aeson remains the repository's JSON encoder. JSON input must use Hermes.
# Agent.Skills is the single documented exception because Data.Yaml's typed API
# is built on Aeson's FromJSON class and the input is YAML, not JSON.
pattern='Aeson\.(eitherDecode|eitherDecodeStrict|decode|decodeStrict|fromJSON)|\beitherDecode(Strict)?'"'"'?\b|\bdecodeStrict'"'"'?\b|\bfromJSON\b|\bparseJSON\b|\bFromJSON\b|\bparseEither\b'

matches="$(
  rg --line-number \
    --glob '*.hs' \
    --glob '!packages/agent-core/src/Agent/Skills.hs' \
    "$pattern" \
    packages || true
)"

if [[ -n "$matches" ]]; then
  echo "Aeson JSON decoding is forbidden; use Agent.Json.Decode/Hermes:" >&2
  echo "$matches" >&2
  exit 1
fi
