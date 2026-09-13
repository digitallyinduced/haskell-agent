# nix build --impure --no-link -L --file nix/benchmarks/repository-fd-limit.nix \
#   --argstr root "git+file://$PWD"
# Rebuild this derivation to repeat; the normal check runs no benchmark loop.
{ root, system ? builtins.currentSystem }:
let
  flake = builtins.getFlake root;
in
flake.checks.${system}.agent-repository.overrideAttrs (old: {
  preCheck = ''
    echo "Builder descriptor limit: $(ulimit -Sn) (hard: $(ulimit -Hn))"
    # Identical compiled code and fixtures, with only the soft limit varied.
    # Subshells keep each trial's limit from affecting subsequent trials.
    for sample in 1 2 3; do
      for limit in 524288 4096 65536; do
        for workload in \
          "snapshots unusual paths and parses diff hunk coordinates" \
          "stages, unstages, restores, and commits under fingerprint guards"; do
          (
            ulimit -Sn "$limit"
            echo "FD_BENCH sample=$sample limit=$limit workload=$workload"
            dist/build/agent-repository-test/agent-repository-test \
              --match "$workload" | tee fd-bench-sample.log
            grep -q '^1 example, 0 failures$' fd-bench-sample.log
          )
        done
      done
    done
    # Also validate the entire suite at the candidate limit.
    ulimit -Sn 4096
  '' + (old.preCheck or "");
})
