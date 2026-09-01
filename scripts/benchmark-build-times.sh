#!/usr/bin/env bash
#
# Build-time benchmark for the Cabal package boundaries.  Run from a nix
# develop shell (or prefix the invocation with `nix develop -c`).
#
# The default workload is deliberately small enough for local iteration:
#   ./scripts/benchmark-build-times.sh [samples]
#
# It uses a fresh private Cabal build directory for each full local-package
# build and a shared primed directory for incremental builds. Marker edits are
# always reverted by the EXIT trap.
set -euo pipefail

samples="${1:-3}"
stage="${2:-baseline}"
mode="${3:-all}"
jobs="${AGENT_BENCH_JOBS:-8}"

if [[ ! "$samples" =~ ^[1-9][0-9]*$ ]]; then
  echo "samples must be a positive integer" >&2
  exit 2
fi
case "$stage" in
  baseline|refactored) ;;
  *)
    echo "stage must be baseline or refactored" >&2
    exit 2
    ;;
esac
case "$mode" in
  all|cold|incremental) ;;
  *)
    echo "mode must be all, cold, or incremental" >&2
    exit 2
    ;;
esac

root="$(git rev-parse --show-toplevel)"
: "${TMPDIR:?TMPDIR must be set; run this benchmark from nix develop}"
benchmark_parent="$root/dist-newstyle/build-time-benchmark"
mkdir -p "$benchmark_parent"
build_root="$(mktemp -d "$benchmark_parent/run.XXXXXX")"
shared_source_cache="$benchmark_parent/source-cache"
marker_file="$root/packages/agent-cli/src/Agent/CLI/TUI/LambdaArt.hs"
runtime_marker_file="$root/packages/agent-cli/src/Agent/CLI/ManagedTurn.hs"
if [[ "$stage" == "refactored" ]]; then
  runtime_marker_file="$root/packages/agent-cli-runtime/src/Agent/CLI/ManagedTurn.hs"
fi
marker_backup="$build_root/LambdaArt.hs"
runtime_marker_backup="$build_root/ManagedTurn.hs"

if [[ ! -f "$marker_file" ]]; then
  echo "marker file not found: $marker_file" >&2
  exit 1
fi
if [[ ! -f "$runtime_marker_file" ]]; then
  echo "runtime marker file not found: $runtime_marker_file" >&2
  exit 1
fi
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
cp -p "$marker_file" "$marker_backup"
cp -p "$runtime_marker_file" "$runtime_marker_backup"

cleaned=false
cleanup() {
  [[ "$cleaned" == true ]] && return
  cleaned=true
  cp -p "$marker_backup" "$marker_file"
  cp -p "$runtime_marker_backup" "$runtime_marker_file"
  find "$build_root" -depth -delete
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

median() {
  printf '%s\n' "$@" | sort -n | awk '
    { a[NR] = $1 }
    END {
      if (NR == 0) exit 1
      if (NR % 2) print a[(NR + 1) / 2]
      else print (a[NR / 2] + a[NR / 2 + 1]) / 2
    }'
}

component_built() {
  local component="$1" log="$2"
  if grep -Eq "^Building library for ${component}-[0-9]" "$log"; then
    printf true
  else
    printf false
  fi
}

run_build() {
  local builddir="$1" target="$2" log="$3"
  local timing="$log.time"
  if [[ -d "$shared_source_cache" && ! -e "$builddir/src" ]]; then
    cp -R "$shared_source_cache" "$builddir/src"
  fi
  if ! /usr/bin/time -p -o "$timing" \
      cabal v2-build --builddir="$builddir" \
        --enable-optimization=1 --jobs="$jobs" "$target" >"$log" 2>&1; then
    cat "$log" >&2
    return 1
  fi
  if [[ ! -d "$shared_source_cache" && -d "$builddir/src" ]]; then
    cp -R "$builddir/src" "$shared_source_cache"
  fi
  awk '
    $1 == "real" { real = $2 }
    $1 == "user" { user = $2 }
    $1 == "sys" { sys = $2 }
    END { printf "%.3f\t%.3f\t%.3f\n", real, user, sys }
  ' "$timing"
}

printf 'scenario\tsample\treal-seconds\tuser-seconds\tsys-seconds\tcompiled-lines\tagent-cli-built\truntime-built\ttelegram-built\n'
printf '# revision\t%s\n' "$(git -C "$root" rev-parse HEAD)"
printf '# index-tree\t%s\n' "$(git -C "$root" write-tree)"
printf '# diff\t%s\n' \
  "$(git -C "$root" diff --binary HEAD | git hash-object --stdin)"
printf '# stage\t%s\n' "$stage"
printf '# jobs\t%s\n' "$jobs"
printf '# ghc\t%s\n' "$(ghc --numeric-version)"
printf '# cabal\t%s\n' "$(cabal --numeric-version)"

if [[ "$mode" != "incremental" ]]; then
  for target in agent-cli:lib:agent-cli agent-telegram:lib:agent-telegram; do
    name="${target%%:*}"
    real_values=()
    user_values=()
    sys_values=()
    for ((i = 1; i <= samples; i++)); do
      dir="$build_root/cold-$name-$i"
      log="$build_root/cold-$name-$i.log"
      mkdir -p "$dir"
      IFS=$'\t' read -r real user sys \
        <<<"$(run_build "$dir" "$target" "$log")"
      compiled="$(grep -Ec 'Compiling|Linking|Building' "$log" || true)"
      cli_built="$(component_built agent-cli "$log")"
      runtime_built="$(component_built agent-cli-runtime "$log")"
      telegram_built="$(component_built agent-telegram "$log")"
      printf 'cold-%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$name" "$i" "$real" "$user" "$sys" "$compiled" \
        "$cli_built" "$runtime_built" "$telegram_built"
      real_values+=("$real")
      user_values+=("$user")
      sys_values+=("$sys")
      find "$dir" -depth -delete
    done
    printf '# median cold-%s\t%s\t%s\t%s\n' "$name" \
      "$(median "${real_values[@]}")" \
      "$(median "${user_values[@]}")" \
      "$(median "${sys_values[@]}")"
  done
fi

if [[ "$mode" != "cold" ]]; then
  warm="$build_root/warm"
  mkdir -p "$warm"
  for scenario in telegram cli; do
    cp -p "$marker_backup" "$marker_file"
    target="agent-telegram:lib:agent-telegram"
    [[ "$scenario" == cli ]] && target="agent-cli:lib:agent-cli"
    run_build "$warm" "$target" "$build_root/prime-cli-$scenario.log" \
      >/dev/null
    real_values=()
    user_values=()
    sys_values=()
    for ((i = 1; i <= samples; i++)); do
      printf '\n-- build marker %s --\n' "$i" >>"$marker_file"
      log="$build_root/warm-cli-$scenario-$i.log"
      IFS=$'\t' read -r real user sys \
        <<<"$(run_build "$warm" "$target" "$log")"
      compiled="$(grep -Ec 'Compiling|Linking|Building' "$log" || true)"
      cli_built="$(component_built agent-cli "$log")"
      runtime_built="$(component_built agent-cli-runtime "$log")"
      telegram_built="$(component_built agent-telegram "$log")"
      if [[ "$stage" == refactored && "$scenario" == telegram \
          && "$cli_built" != false ]]; then
        cat "$log" >&2
        echo "CLI-only edit unexpectedly rebuilt the Telegram graph" >&2
        exit 1
      fi
      printf 'warm-cli-edit-to-%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$scenario" "$i" "$real" "$user" "$sys" "$compiled" \
        "$cli_built" "$runtime_built" "$telegram_built"
      real_values+=("$real")
      user_values+=("$user")
      sys_values+=("$sys")
    done
    printf '# median warm-cli-edit-to-%s\t%s\t%s\t%s\n' "$scenario" \
      "$(median "${real_values[@]}")" \
      "$(median "${user_values[@]}")" \
      "$(median "${sys_values[@]}")"
  done

  for scenario in telegram cli; do
    cp -p "$runtime_marker_backup" "$runtime_marker_file"
    target="agent-telegram:lib:agent-telegram"
    [[ "$scenario" == cli ]] && target="agent-cli:lib:agent-cli"
    run_build "$warm" "$target" "$build_root/prime-runtime-$scenario.log" \
      >/dev/null
    real_values=()
    user_values=()
    sys_values=()
    for ((i = 1; i <= samples; i++)); do
      printf '\n-- runtime build marker %s --\n' "$i" \
        >>"$runtime_marker_file"
      log="$build_root/warm-runtime-$scenario-$i.log"
      IFS=$'\t' read -r real user sys \
        <<<"$(run_build "$warm" "$target" "$log")"
      compiled="$(grep -Ec 'Compiling|Linking|Building' "$log" || true)"
      cli_built="$(component_built agent-cli "$log")"
      runtime_built="$(component_built agent-cli-runtime "$log")"
      telegram_built="$(component_built agent-telegram "$log")"
      printf 'warm-runtime-edit-to-%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$scenario" "$i" "$real" "$user" "$sys" "$compiled" \
        "$cli_built" "$runtime_built" "$telegram_built"
      real_values+=("$real")
      user_values+=("$user")
      sys_values+=("$sys")
    done
    printf '# median warm-runtime-edit-to-%s\t%s\t%s\t%s\n' "$scenario" \
      "$(median "${real_values[@]}")" \
      "$(median "${user_values[@]}")" \
      "$(median "${sys_values[@]}")"
  done
fi

cat >&2 <<EOF
Benchmark complete. Build logs and temporary build artifacts were removed.
Run inside nix develop for reproducibility, e.g.:
  nix develop -c $root/scripts/benchmark-build-times.sh $samples
EOF
