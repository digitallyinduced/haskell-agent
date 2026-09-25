#!/usr/bin/env bash
# Run the pinned ten-task sample once per harness, without automatic retries.
set -euo pipefail
umask 077

: "${EVALUATION_DIRECTORY:?Set the prepared evaluation directory}"
: "${EVALUATOR_PATH:?Set the Nix Harbor environment path}"
: "${HASKELL_EXECUTABLE:?Set the packaged agent-cli executable path}"
: "${POSTGRES_PSQL_EXECUTABLE:?Set psql from the packaged agent PostgreSQL closure}"
: "${CODEX_EXECUTABLE:?Set the packaged Codex executable path}"
: "${SUPPORT_ARCHIVE:?Set the sudo closure archive path}"
: "${SUDO_EXECUTABLE:?Set the Nix sudo executable path}"
: "${CODEX_AUTH_PATH:?Set the authorized credential file path}"
: "${CA_BUNDLE_PATH:?Set the CA bundle file path}"
: "${TMPDIR:?Set a temporary directory}"

export HOME="$EVALUATION_DIRECTORY/harbor-home"
export PYTHONPATH="$EVALUATION_DIRECTORY/adapter"
job_prefix=${JOB_PREFIX:-sample-comparison}
benchmark_model=${BENCHMARK_MODEL:-gpt-5.6-luna}
common_arguments=(
    --registry-url https://raw.githubusercontent.com/harbor-framework/harbor/v0.23.0/registry.json
    --dataset terminal-bench-sample@2.0
    --model "$benchmark_model" --n-concurrent 2 --n-attempts 1 --max-retries 0
    --ak "codex_auth_path=$CODEX_AUTH_PATH"
    --ak "ca_bundle_path=$CA_BUNDLE_PATH"
    --ak "support_archive=$SUPPORT_ARCHIVE"
    --ak "sudo_executable=$SUDO_EXECUTABLE"
    --ak effort=medium --jobs-dir "$EVALUATION_DIRECTORY/jobs" --delete
)

"$EVALUATOR_PATH/bin/python" -m unittest discover -s "$PYTHONPATH"
"$EVALUATOR_PATH/bin/harbor" run "${common_arguments[@]}" \
    --agent haskell_agent:HaskellAgent \
    --ak "psql_executable=$POSTGRES_PSQL_EXECUTABLE" \
    --ak "closure_archive=$EVALUATION_DIRECTORY/agent-closure.tar.gz" \
    --ak "executable_path=$HASKELL_EXECUTABLE" \
    --job-name "$job_prefix-haskell" > "$EVALUATION_DIRECTORY/$job_prefix-haskell.log" 2>&1 &
haskell_process=$!
"$EVALUATOR_PATH/bin/harbor" run "${common_arguments[@]}" \
    --agent stock_codex:StockCodex \
    --ak "closure_archive=$EVALUATION_DIRECTORY/codex-closure.tar.gz" \
    --ak "executable_path=$CODEX_EXECUTABLE" \
    --job-name "$job_prefix-codex" > "$EVALUATION_DIRECTORY/$job_prefix-codex.log" 2>&1 &
codex_process=$!

terminate_children() {
    kill "$haskell_process" "$codex_process" 2>/dev/null || true
    wait "$haskell_process" "$codex_process" 2>/dev/null || true
}
trap terminate_children INT TERM EXIT
status=0
wait "$haskell_process" || status=1
wait "$codex_process" || status=1
trap - INT TERM EXIT
cat "$EVALUATION_DIRECTORY/$job_prefix-haskell.log"
cat "$EVALUATION_DIRECTORY/$job_prefix-codex.log"
exit "$status"
