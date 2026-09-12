#!/usr/bin/env bash
set -euo pipefail

root="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
cli="$root/packages/agent-cli"
external_session="$root/packages/agent-external-session"
repository="$root/packages/agent-repository"
bridge="$root/packages/agent-native-bridge"
runtime="$root/packages/agent-runtime"
integration_api="$root/packages/agent-integration-api"
core="$root/packages/agent-core"

fail() {
  echo "package boundary check failed: $*" >&2
  exit 1
}

for package in "$cli" "$external_session" "$repository" "$bridge" "$runtime" \
  "$core" "$root/packages/agent-accounts" "$root/packages/agent-computer-use" \
  "$root/packages/agent-tools"; do
  [[ -d "$package" ]] || fail "missing package directory: $package"
done

if [[ -e "$cli/src/Agent/CLI/Dialects.hs" \
  || -e "$cli/src/Agent/CLI/Runtime/Orchestration/Tools/Resources.hs" ]]; then
  fail "shared tool assembly and ownership must remain in agent-runtime"
fi

python3 "$root/scripts/check-package-graph.py" "$root"
[[ ! -e "$root/packages/agent-cli-runtime/agent-cli-runtime.cabal" ]] \
  || fail "agent-cli-runtime must be replaced by agent-runtime"

# Public builds must remain usable without any proprietary integration source.
[[ ! -e "$root/packages/agent-integrations" ]] \
  || fail "concrete integrations returned to the public tree"
[[ ! -e "$root/packages/agent-mail/src/Agent/Mail/Contract.hs" ]] \
  || fail "product integration catalog returned to the low-level mail library"
if rg --line-number '^import[[:space:]]+(qualified[[:space:]]+)?Agent\.Integrations(\.|[[:space:]])' \
    "$root/packages" --glob '*.hs'; then
  fail "public source imports a private integration implementation"
fi
if rg --line-number '\bagent-integrations\b' "$root/cabal.project" \
    "$root/flake.nix" "$root/packages" --glob '*.cabal' --glob 'package.nix'; then
  fail "public build graph depends on private integrations"
fi
if rg --line-number 'Agent\.(CLI|Mail|Integrations)(\.|[[:space:]])' \
    "$integration_api/src"; then
  fail "integration API gained product or frontend dependencies"
fi

# Lower packages must neither implement nor import frontend modules.
for name in agent-core agent-accounts agent-computer-use agent-tools agent-runtime; do
  source_dirs=("$root/packages/$name/src")
  [[ ! -d "$root/packages/$name/test" ]] || source_dirs+=("$root/packages/$name/test")
  if rg --line-number '^[[:space:]]*(module|import)[[:space:]]+(qualified[[:space:]]+)?Agent\.(CLI|TUI)(\.|[[:space:]])' \
    "${source_dirs[@]}" --glob '*.hs'; then
    fail "frontend types leaked into $name"
  fi
done

# Core owns contracts, scheduling and output memory, not concrete tool handlers.
while IFS= read -r file; do
  case "${file#"$core/src/"}" in
    Agent/Tools/Types.hs|Agent/Tools/Scheduling.hs|Agent/Tools/ResourceArbiter.hs|Agent/Tools/OutputArtifact/Memory.hs) ;;
    *) fail "concrete tool implementation returned to core: $file" ;;
  esac
done < <(find "$core/src/Agent/Tools" -type f -name '*.hs')

# Shared resources and session policy have frontend-neutral module names.
for module_path in Agent/Runtime/NativeProcess.hs Agent/Runtime/Session/Threads.hs Agent/Runtime/Session/PullRequest.hs; do
  [[ -f "$runtime/src/$module_path" ]] || fail "missing shared resource: $module_path"
  [[ ! -e "$cli/src/$module_path" ]] || fail "resource implementation returned to CLI: $module_path"
  [[ ! -e "$cli/src/${module_path/Runtime/CLI}" ]] || fail "legacy resource implementation returned to CLI: $module_path"
done

# Provider construction and model-history compaction have one runtime owner,
# not legacy CLI implementations or reexport facades.
if rg --line-number \
  'Agent\.CLI\.(Compaction|ProviderRuntime|Provider\.OpenAI|Subagents\.Runtime\.OpenAI|Runtime\.Orchestration\.Providers)(\.|[^[:alnum:]_]|$)' \
  "$root/packages" --glob '*.hs' --glob '*.cabal'; then
  fail "provider or compaction ownership returned to a retired CLI namespace"
fi

# This is deliberately a presentation-only module. Live transcript operations
# must be imported from Agent.Runtime.Session.History instead of reexported here.
if ! rg --quiet --multiline \
  'module[[:space:]]+Agent\.CLI\.Session\.History[[:space:]]*\([[:space:]]*hydrateUiHistory[[:space:]]*\)[[:space:]]+where' \
  "$cli/src/Agent/CLI/Session/History.hs"; then
  fail "CLI session history must expose only UI hydration"
fi

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

if rg --line-number 'CliOptions|Agent\.CLI\.Options|nativePrepareOptions' \
  "$root/packages/agent-server/src/Agent/Server/Runtime.hs" \
  "$root/packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Types.hs" \
  "$root/packages/agent-runtime/src/Agent/Runtime/StartupPolicy.hs"; then
  fail "native startup contracts and server runtime must not depend on CLI options"
fi

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

if rg --line-number '\bagent-cli([[:space:],><=]|$)' \
  "$repository/agent-repository.cabal"; then
  fail "agent-repository must remain independent of agent-cli"
fi

for registration in \
  packages/agent-accounts \
  packages/agent-computer-use \
  packages/agent-tools \
  packages/agent-runtime \
  packages/agent-external-session \
  packages/agent-repository \
  packages/agent-native-bridge; do
  if ! rg --quiet --fixed-strings "$registration" "$root/cabal.project"; then
    fail "$registration is missing from cabal.project"
  fi
done

required_files=(
  packages/agent-runtime/src/Agent/Runtime/Tools/Dialects.hs
  packages/agent-runtime/src/Agent/Runtime/Tools/Resources.hs
  packages/agent-runtime/src/Agent/Runtime/Tools/Startup.hs
  packages/agent-runtime/src/Agent/Runtime/Startup/Model.hs
  packages/agent-runtime/src/Agent/Runtime/Startup/Policy.hs
  packages/agent-runtime/src/Agent/Runtime/Startup/Gateway.hs
  packages/agent-runtime/test/Agent/Runtime/Tools/ResourcesSpec.hs
  packages/agent-runtime/test/Agent/Runtime/Tools/StartupSpec.hs
  packages/agent-runtime/src/Agent/Runtime/Providers.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/Types.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/Common.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/OpenAI.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/XAI.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/Gemini.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/OpenRouter.hs
  packages/agent-runtime/src/Agent/Runtime/Providers/Claude.hs
  packages/agent-runtime/src/Agent/Runtime/Provider/OpenAI.hs
  packages/agent-runtime/src/Agent/Runtime/Provider/OpenAI/Fresh.hs
  packages/agent-runtime/src/Agent/Runtime/Compaction.hs
  packages/agent-runtime/src/Agent/Runtime/Compaction/Provider.hs
  packages/agent-runtime/src/Agent/Runtime/Compaction/Types.hs
  packages/agent-runtime/src/Agent/Runtime/Compaction/Projection.hs
  packages/agent-runtime/src/Agent/Runtime/Compaction/Continuation.hs
  packages/agent-runtime/src/Agent/Runtime/Session/Backend.hs
  packages/agent-runtime/src/Agent/Runtime/Session/History.hs
  packages/agent-runtime/test/Agent/Runtime/ProviderRuntimeSpec.hs
  packages/agent-runtime/test/Agent/Runtime/CompactionSpec.hs
  packages/agent-runtime/src/Agent/Runtime/StartupPolicy.hs
  packages/agent-runtime/src/Agent/Runtime/ConversationStore.hs
  packages/agent-runtime/src/Agent/Runtime/SessionState.hs
  packages/agent-runtime/test/Agent/Runtime/ConversationStoreSpec.hs
  packages/agent-runtime/test/Agent/Runtime/ConversationSessionSpec.hs
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
