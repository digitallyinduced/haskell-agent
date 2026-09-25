# Terminal-Bench integration

This directory pins Harbor 0.23.0 and its Python dependencies through Nix,
`uv2nix`, and checked-in lock files. The adapter runs the native `agent-cli`
inside Harbor's task container; Harbor then runs the task verifier.

The initial procedure evaluates **one selected task**, not the full benchmark.
It validates installation, authentication, execution, and verification. A
single-task result is not a Terminal-Bench score or a comparative performance
claim. Token usage and cost accounting are not implemented in this adapter.

`run_sample_comparison.sh` runs two ten-task jobs from prepared artifacts.
Generated results and transcripts are private artifacts, not repository files.

## Requirements

- An x86_64 Linux evaluation host, such as `ssh office-builder`.
- Nix with flakes, Docker, and Docker Compose, usable by the evaluation account.
- A checkout of this repository at the revision to evaluate.
- Explicit authorization to use a Codex login for the run.

Run the commands below in Bash, from the repository root on the Linux host.
Do not run benchmark tasks directly on the host. The Docker socket and host
home directory must not be mounted into task containers.

## Build the evaluator and package the agent

```sh
set -euo pipefail
umask 077

# NixOS SSH sessions may not define TMPDIR.
export TMPDIR="${TMPDIR:-${XDG_RUNTIME_DIR:?Set TMPDIR to a temporary directory}}"
evaluation_directory=$(mktemp -d "$TMPDIR/terminal-bench-evaluation.XXXXXXXX")
repository_directory=$PWD
benchmark_directory="$repository_directory/benchmarks/terminal-bench"
benchmark_flake="path:$benchmark_directory"

# Preserve the credential location before isolating Harbor's HOME.
codex_auth_path="${CODEX_HOME:-$HOME/.codex}/auth.json"
test -r "$codex_auth_path"

nix build "$benchmark_flake" --out-link "$evaluation_directory/evaluator"
nix build "$benchmark_flake#checks.x86_64-linux.harbor-cli" --no-link
nix build .#agent-cli-static --out-link "$evaluation_directory/agent"
nix build "$benchmark_flake#privilege-support" --no-link

evaluator_path=$(readlink -f "$evaluation_directory/evaluator")
agent_path=$(readlink -f "$evaluation_directory/agent")
# sudo has multiple outputs: select outPath, not all --print-out-paths lines.
support_path=$(nix eval --raw "$benchmark_flake#privilege-support.outPath")

nixpkgs_reference=github:NixOS/nixpkgs/afe3d8ac4395617bdcdac9f188ac8717a062e014
nix build "$nixpkgs_reference#cacert" --out-link "$evaluation_directory/certificates"
ca_bundle_path="$evaluation_directory/certificates/etc/ssl/certs/ca-bundle.crt"

package_closure() {
    local store_path=$1
    local archive_path=$2
    local manifest_path="${archive_path}.paths"
    nix-store -qR "$store_path" > "$manifest_path"
    sed 's|^/||' "$manifest_path" > "${manifest_path}.relative"
    tar -C / -czf "$archive_path" -T "${manifest_path}.relative"
}

package_closure "$agent_path" "$evaluation_directory/agent-closure.tar.gz"
package_closure "$support_path" "$evaluation_directory/support-closure.tar.gz"

git rev-parse HEAD > "$evaluation_directory/repository-revision.txt"
git diff --binary > "$evaluation_directory/repository-changes.patch"
"$agent_path/bin/agent-cli" --version > "$evaluation_directory/agent-version.txt"
"$evaluator_path/bin/harbor" --version > "$evaluation_directory/harbor-version.txt"
sha256sum "$evaluation_directory/"*-closure.tar.gz \
    > "$evaluation_directory/closure-checksums.txt"
```

The full Linux Nix build is intentional here: this packages the executable and
runtime dependencies for another operating-system environment. For ordinary
Haskell development, continue using GHCi.

For an installation-only investigation, an existing Nix-installed Linux
`agent-cli` can be packaged instead. Record its actual `--version` and store
path; do not describe that run as evaluating the current checkout.

## Run one task

Run adapter tests before making a model request:

```sh
PYTHONPATH="$benchmark_directory" \
    "$evaluator_path/bin/python" -m unittest discover \
    -s "$benchmark_directory" -p 'test_*.py'

mkdir -p "$evaluation_directory/harbor-home" "$evaluation_directory/jobs"
```

Use the Harbor `v0.23.0` release's
[registry.json](https://raw.githubusercontent.com/harbor-framework/harbor/v0.23.0/registry.json),
not a moving registry branch. Its `terminal-bench-sample@2.0` entry pins the
sample repository to `7e917f35c281188532772312d4ad91ca9274febc`. The selected
task is `log-summary-date-ranges`, with `gpt-5.6-luna` and `medium` reasoning effort.
This is the public sample dataset, not the full Terminal-Bench dataset.

```sh
HOME="$evaluation_directory/harbor-home" \
PYTHONPATH="$benchmark_directory" \
    "$evaluator_path/bin/harbor" run \
    --registry-url https://raw.githubusercontent.com/harbor-framework/harbor/v0.23.0/registry.json \
    --dataset terminal-bench-sample@2.0 \
    --include-task-name log-summary-date-ranges \
    --agent haskell_agent:HaskellAgent \
    --model gpt-5.6-luna \
    --agent-kwarg effort=medium \
    --agent-kwarg "closure_archive=$evaluation_directory/agent-closure.tar.gz" \
    --agent-kwarg "executable_path=$agent_path/bin/agent-cli" \
    --agent-kwarg "support_archive=$evaluation_directory/support-closure.tar.gz" \
    --agent-kwarg "sudo_executable=$support_path/bin/sudo" \
    --agent-kwarg "ca_bundle_path=$ca_bundle_path" \
    --agent-kwarg "codex_auth_path=$codex_auth_path" \
    --n-attempts 1 \
    --n-concurrent 1 \
    --max-retries 0 \
    --jobs-dir "$evaluation_directory/jobs" \
    --delete
```

Harbor owns task timeouts and verification. Inspect the job and trial
`result.json` files, verifier reward, and logs. An infrastructure error is not
evidence that the agent attempted and failed the task. An agent exit code of
zero is not evidence that the verifier passed.

`summarize_results.py FIRST_JOB SECOND_JOB` validates that all ten tasks are
paired and summarizes Harbor's raw rewards, exception types, and timings.
It does not establish verifier validity: inspect verifier logs even when Harbor
reports reward zero with no exception. A test bootstrap failure can be encoded
as zero by the task script without running any assertions. Report such cases
separately and never silently relabel them as agent failures or passes.

The selected model must be available to the authorized account. Do not silently
substitute another model, change limits, or repeat failed attempts when reporting
results. Record any installation corrections separately from scored attempts.

The comparison runner accepts `BENCHMARK_MODEL` (default `gpt-5.6-luna`).
Run a trivial preflight with both pinned executables before starting a new model
comparison. Account access can differ between model names; stop on quota or
authentication failures rather than reporting them as task failures.

## Stock Codex comparison

`stock_codex:StockCodex` invokes the unmodified Nix-packaged `codex exec`.
Package the installed Codex closure with the same `package_closure` function,
then use the same Harbor command with `--agent stock_codex:StockCodex`, its
closure archive, and its `/nix/store/.../bin/codex` executable path. Record
`codex --version` and the closure checksum; do not use an unpinned npm install.

This adapter shares provisioning, the nonroot/sudo environment note, isolated
home, credentials, and certificates with the Haskell adapter. It passes
`--dangerously-bypass-approvals-and-sandbox --skip-git-repo-check --json` and
`-c 'model_reasoning_effort="medium"'`. Codex retains its native prompts and
tools; no personal configuration is copied. Harbor's task timeout applies,
but Codex has no matching 100-turn cap. Compare verifier results first;
single-run wall times are observations, not a reliable speed comparison.

## Run the paired sample

After the preparation above, package a pinned, installed Codex executable and
copy the adapters into the evaluation directory:

```sh
codex_executable=$(readlink -f "$(command -v codex)")
codex_package=$(dirname "$(dirname "$codex_executable")")
package_closure "$codex_package" "$evaluation_directory/codex-closure.tar.gz"
"$codex_executable" --version > "$evaluation_directory/codex-version.txt"
mkdir -p "$evaluation_directory/adapter"
cp "$benchmark_directory/"*.py "$evaluation_directory/adapter/"
postgres_psql_executable=$(while read -r path; do
    if test -x "$path/bin/psql"; then printf '%s\n' "$path/bin/psql"; fi
done < "$evaluation_directory/agent-closure.tar.gz.paths")
test -x "$postgres_psql_executable"

EVALUATION_DIRECTORY="$evaluation_directory" \
EVALUATOR_PATH="$evaluator_path" \
HASKELL_EXECUTABLE="$agent_path/bin/agent-cli" \
POSTGRES_PSQL_EXECUTABLE="$postgres_psql_executable" \
CODEX_EXECUTABLE="$codex_executable" \
SUPPORT_ARCHIVE="$evaluation_directory/support-closure.tar.gz" \
SUDO_EXECUTABLE="$support_path/bin/sudo" \
CODEX_AUTH_PATH="$codex_auth_path" CA_BUNDLE_PATH="$ca_bundle_path" \
BENCHMARK_MODEL=gpt-5.6-sol JOB_PREFIX=sample-sol \
    bash "$benchmark_directory/run_sample_comparison.sh"

"$evaluator_path/bin/python" "$benchmark_directory/summarize_results.py" \
    "$evaluation_directory/jobs/sample-sol-haskell" \
    "$evaluation_directory/jobs/sample-sol-codex"
```

The runner uses two concurrent trials per harness and starts both harness jobs
together (up to four active tasks). One attempt per task, no retries, medium
reasoning effort. Shared host/provider contention and stochastic model choices
affect timing; this is not an isolated causal performance experiment.

Each trial's `agent/execution-timing.json` records elapsed container execution
and transcript export separately. Execution includes CLI startup and model/tool
work, but excludes prompt upload and transcript export. Harbor's broader agent
execution phase includes those operations. Summed trial times are not wall-clock
job duration because trials overlap. Review zero-reward verifier logs explicitly;
the sample QEMU verifiers have encountered stale package-index bootstrap failures.

## Isolation and credentials

### Transcript retention

The adapters export to each trial's `agent/transcripts/` before container
cleanup, including on cancellation. Native runs enable `--save-session`, copy
allowlisted parent/child transcript files, and paginate the CLI session view.
Pass `--ak psql_executable=/nix/store/.../bin/psql` from the agent's PostgreSQL
closure to also retain a read-only, transcript-table-only database snapshot
including pre-compaction history. The comparison runner requires this as
`POSTGRES_PSQL_EXECUTABLE`. Codex retains its native session JSONL.

Check `transcripts/manifest.json` for export errors and coverage. Exports are
bounded and cannot recover uncommitted/in-flight output. The first comparison
predates this change: missing transcripts from deleted containers cannot be
recovered retroactively. Raw transcripts are private evaluation artifacts;
review them before sharing and never archive credentials or the entire home.

The adapter copies trusted Nix closures into the task container. It creates an
isolated agent home and uploads only the authorized credential file, separately
from the archives. It does not copy personal configuration, memory, skills,
sessions, or MCP connections. The agent runs without interactive approvals and
can use passwordless sudo **inside the task container only**. This is necessary
because the managed PostgreSQL runtime cannot run as root, while benchmark
tasks may require root operations.

Setup also provisions a writable `.haskell-agent` metadata directory in the
task working directory; existing task files and their permissions are retained.

Never put credentials in Nix expressions, derivations, closure archives, or
checked-in files. Do not pass credential contents as command-line arguments.
Credential files remain readable by the agent inside its container; use an
appropriately scoped evaluation account where possible.

Keep Harbor's environment deletion enabled. After an interrupted run, inspect
and remove only the containers belonging to that job. Do not retain or publish
container images containing credentials. Do not publish logs or artifacts
without reviewing them for secrets and sensitive model/tool output.

`$TMPDIR` and NixOS `$XDG_RUNTIME_DIR` are temporary storage. Copy reviewed results,
resolved job configuration, revision records, and checksums to durable storage
before logout or cleanup. Archive the recorded image digests and task revisions
as well: pinned evaluator dependencies alone do not pin mutable upstream images.

## Updating dependencies

Edit `pyproject.toml`, then regenerate the dependency lock through the pinned
development shell:

```sh
nix develop path:./benchmarks/terminal-bench \
    -c uv lock --project benchmarks/terminal-bench
```

Use Nix to build/install the resulting environment. Do not use `pip install`,
`uv sync`, or `uv tool install` as a parallel dependency-management mechanism.
