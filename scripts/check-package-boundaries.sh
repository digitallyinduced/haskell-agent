#!/usr/bin/env bash
set -euo pipefail

root="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
cli="$root/packages/agent-cli"
external_session="$root/packages/agent-external-session"
repository="$root/packages/agent-repository"
bridge="$root/packages/agent-native-bridge"

fail() {
  echo "package boundary check failed: $*" >&2
  exit 1
}

for package in "$cli" "$external_session" "$repository" "$bridge"; do
  [[ -d "$package" ]] || fail "missing package directory: $package"
done

moved_modules=(
  Agent.CLI.BrowserTools
  Agent.CLI.McpAdmin
  Agent.CLI.ProcessSecurity
  Agent.CLI.RepositoryDelivery
  Agent.CLI.RepositoryReview
  Agent.CLI.ResourceAdmin
  Agent.CLI.MacOS.Bridge
  Agent.CLI.MacOS.EngineMailbox
  Agent.CLI.MacOS.NativeLoopEvent
  Agent.CLI.MacOS.ResourceAdmin
)

for module in "${moved_modules[@]}"; do
  if rg --line-number --fixed-strings "$module" "$cli"; then
    fail "$module leaked back into agent-cli"
  fi
done

if rg --line-number \
  '\bagent-(native-bridge|repository|runtime-daemon)\b' \
  "$cli/agent-cli.cabal"; then
  fail "agent-cli gained a dependency on an extracted package"
fi

if ! rg --quiet --fixed-strings \
  'agent-external-session:Agent.CLI.ExternalSession as Agent.CLI.ExternalSession' \
  "$cli/agent-cli.cabal"; then
  fail "agent-cli must re-export the external-session facade"
fi

if [[ -e "$cli/src/Agent/CLI/ExternalSession.hs" \
  || -d "$cli/src/Agent/CLI/ExternalSession" \
  || -e "$cli/test/Agent/CLI/ExternalSessionSpec.hs" ]]; then
  fail "external-session implementation leaked back into agent-cli"
fi

if rg --line-number 'Agent\.CLI\.ExternalSession\.' \
  "$cli/src" "$cli/test"; then
  fail "agent-cli must use only the external-session facade"
fi

if rg --line-number '\bagent-cli\b' \
  "$external_session/agent-external-session.cabal"; then
  fail "agent-external-session must remain independent of agent-cli"
fi

if rg --line-number '\bagent-cli\b' \
  "$repository/agent-repository.cabal"; then
  fail "agent-repository must remain independent of agent-cli"
fi

for registration in \
  packages/agent-external-session \
  packages/agent-repository \
  packages/agent-native-bridge; do
  if ! rg --quiet --fixed-strings "$registration" "$root/cabal.project"; then
    fail "$registration is missing from cabal.project"
  fi
done

required_files=(
  packages/agent-external-session/src/Agent/CLI/ExternalSession.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Content.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/JSONL.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Paths.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Provider/Claude.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Provider/Codex.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Provider/Cursor.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Provider/Grok.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/SQLite.hs
  packages/agent-external-session/src/Agent/CLI/ExternalSession/Types.hs
  packages/agent-external-session/test/Agent/CLI/ExternalSessionSpec.hs
  packages/agent-repository/src/Agent/CLI/ProcessSecurity.hs
  packages/agent-repository/src/Agent/CLI/RepositoryDelivery.hs
  packages/agent-repository/src/Agent/CLI/RepositoryReview.hs
  packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs
  packages/agent-native-bridge/src/Agent/CLI/McpAdmin.hs
  packages/agent-native-bridge/src/Agent/CLI/ResourceAdmin.hs
  packages/agent-native-bridge/ffi/Agent/CLI/MacOS/Bridge.hs
  packages/agent-native-bridge/ffi/Agent/CLI/MacOS/EngineMailbox.hs
  packages/agent-native-bridge/ffi/Agent/CLI/MacOS/NativeLoopEvent.hs
  packages/agent-native-bridge/ffi/Agent/CLI/MacOS/ResourceAdmin.hs
)

for file in "${required_files[@]}"; do
  [[ -f "$root/$file" ]] || fail "missing moved module: $file"
done
