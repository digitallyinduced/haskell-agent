#!/usr/bin/env bash
set -euo pipefail

root="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
cli="$root/packages/agent-cli"
repository="$root/packages/agent-repository"
bridge="$root/packages/agent-native-bridge"

fail() {
  echo "package boundary check failed: $*" >&2
  exit 1
}

for package in "$cli" "$repository" "$bridge"; do
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

if rg --line-number '\bagent-cli\b' \
  "$repository/agent-repository.cabal"; then
  fail "agent-repository must remain independent of agent-cli"
fi

for registration in \
  packages/agent-repository \
  packages/agent-native-bridge; do
  if ! rg --quiet --fixed-strings "$registration" "$root/cabal.project"; then
    fail "$registration is missing from cabal.project"
  fi
done

required_files=(
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
