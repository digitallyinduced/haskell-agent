#!/bin/bash
set -euo pipefail

expected_repository="digitallyinduced/haskell-agent"
expected_event="push"
expected_ref="refs/heads/master"
expected_workflow_ref="${expected_repository}/.github/workflows/publish-binary-cache.yml@${expected_ref}"

deny() {
    printf 'Refusing self-hosted macOS job: %s\n' "$*" >&2
    exit 1
}

[[ "${GITHUB_REPOSITORY:-}" == "$expected_repository" ]] ||
    deny "unexpected repository '${GITHUB_REPOSITORY:-unset}'"
[[ "${GITHUB_EVENT_NAME:-}" == "$expected_event" ]] ||
    deny "unexpected event '${GITHUB_EVENT_NAME:-unset}'"
[[ "${GITHUB_REF:-}" == "$expected_ref" ]] ||
    deny "unexpected ref '${GITHUB_REF:-unset}'"
[[ "${GITHUB_WORKFLOW_REF:-}" == "$expected_workflow_ref" ]] ||
    deny "unexpected workflow '${GITHUB_WORKFLOW_REF:-unset}'"

printf 'Authorized %s from %s on %s\n' \
    "$GITHUB_WORKFLOW_REF" "$GITHUB_EVENT_NAME" "$GITHUB_REF"
