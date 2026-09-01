#!/usr/bin/env bash
#
# Measure whether an agent-cli test-only change invalidates the production
# Nix derivation. Evaluation is enough: a changed production input produces a
# different output path, without spending minutes rebuilding an output that is
# expected to be discarded. Run from the repository's nix develop shell.
set -euo pipefail

samples="${1:-3}"
stage="${2:-baseline}"
if [[ ! "$samples" =~ ^[1-9][0-9]*$ ]]; then
  echo "samples must be a positive integer" >&2
  exit 2
fi
case "$stage" in
  baseline) expected_changed=true ;;
  refactored) expected_changed=false ;;
  *)
    echo "stage must be baseline or refactored" >&2
    exit 2
    ;;
esac

root="$(git rev-parse --show-toplevel)"
marker="$root/packages/agent-cli/test/Agent/CLI/OptionsSpec.hs"
case "$stage" in
  baseline)
    if grep -q 'agent-cli-runtime' \
        "$root/packages/agent-telegram/agent-telegram.cabal"; then
      echo "baseline stage requested for a refactored package graph" >&2
      exit 2
    fi
    ;;
  refactored)
    if ! grep -q 'agent-cli-runtime' \
        "$root/packages/agent-telegram/agent-telegram.cabal"; then
      echo "refactored stage requested for a baseline package graph" >&2
      exit 2
    fi
    ;;
esac
: "${TMPDIR:?TMPDIR must be set; run this benchmark from nix develop}"
scratch="$(mktemp -d "$TMPDIR/agent-cli-nix-bench.XXXXXX")"
backup="$scratch/OptionsSpec.hs"
cp -p "$marker" "$backup"

cleanup() {
  [[ "${cleaned:-false}" == true ]] && return
  cleaned=true
  cp -p "$backup" "$marker"
  find "$scratch" -depth -delete
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

flake="${AGENT_BENCH_FLAKE:-$root#agent-cli}"

timed_evaluation() {
  local sample="$1"
  local timing="$scratch/time.${sample}"
  local output_file="$scratch/output.${sample}"
  local error_file="$scratch/error.${sample}"
  if ! /usr/bin/time -p -o "$timing" \
      nix eval --offline --no-write-lock-file --raw "${flake}.outPath" \
        >"$output_file" 2>"$error_file"; then
    cat "$error_file" >&2
    return 1
  fi
  awk -v output="$(cat "$output_file")" '
    $1 == "real" { real = $2 }
    $1 == "user" { user = $2 }
    $1 == "sys" { sys = $2 }
    END { printf "%.3f\t%.3f\t%.3f\t%s\n", real, user, sys, output }
  ' "$timing"
}

printf 'sample\treal-seconds\tuser-seconds\tsys-seconds\toutput-path\tchanged\n'
printf '# revision\t%s\n' "$(git -C "$root" rev-parse HEAD)"
printf '# stage\t%s\n' "$stage"
printf '# nix\t%s\n' "$(nix --version)"
# Compare dirty-to-dirty revisions so AGENT_BUILD_COMMIT is held constant.
# The baseline graph still hashes this excluded-test candidate into its source;
# the refactored production source does not.
printf '\n-- nix source benchmark sentinel --\n' >>"$marker"
baseline_output="$(
  nix eval --offline --no-write-lock-file --raw "${flake}.outPath"
)"
real_values=()
user_values=()
sys_values=()
for ((i = 1; i <= samples; i++)); do
  printf '\n-- nix source marker %s --\n' "$i" >>"$marker"
  IFS=$'\t' read -r real user sys output <<<"$(timed_evaluation "$i")"
  changed=false
  [[ "$output" != "$baseline_output" ]] && changed=true
  if [[ "$changed" != "$expected_changed" ]]; then
    echo "unexpected Nix invalidation result for $stage sample $i" >&2
    echo "baseline output: $baseline_output" >&2
    echo "sample output:   $output" >&2
    exit 1
  fi
  printf '%d\t%s\t%s\t%s\t%s\t%s\n' \
    "$i" "$real" "$user" "$sys" "$output" "$changed"
  real_values+=("$real")
  user_values+=("$user")
  sys_values+=("$sys")
done

median() {
  printf '%s\n' "$@" | sort -n | awk '
    { value[NR] = $1 }
    END {
      if (NR % 2) print value[(NR + 1) / 2]
      else print (value[NR / 2] + value[NR / 2 + 1]) / 2
    }'
}

printf '# median\t%s\t%s\t%s\n' \
  "$(median "${real_values[@]}")" \
  "$(median "${user_values[@]}")" \
  "$(median "${sys_values[@]}")"
