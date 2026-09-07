{ self, pkgs }:
let
  mkTestRunner =
    marker:
    pkgs.writeShellScriptBin "agent-sandbox-runner" ''
      set -eu
      test "$#" -eq 1
      test "$1" = marker
      printf '%s\n' ${pkgs.lib.escapeShellArg marker}
    '';

  testRunnerV1 = mkTestRunner "runner-v1";
  testRunnerV2 = mkTestRunner "runner-v2";
  trustedRunnerBase = "/run/haskell-agent-server-runners";
  trustedRunnerRoot = "${trustedRunnerBase}/test-agent-server";
  trustedRunnerGenerationV1 = "${trustedRunnerRoot}/${builtins.baseNameOf (toString testRunnerV1)}";
  trustedRunnerGenerationV2 = "${trustedRunnerRoot}/${builtins.baseNameOf (toString testRunnerV2)}";
  trustedRunnerV1 = "${trustedRunnerGenerationV1}/agent-sandbox-runner";
  trustedRunnerV2 = "${trustedRunnerGenerationV2}/agent-sandbox-runner";
  trustedRunnerStagingV1 = "${trustedRunnerGenerationV1}/.agent-sandbox-runner.new";
  trustedRunnerStagingV2 = "${trustedRunnerGenerationV2}/.agent-sandbox-runner.new";

  testServer = pkgs.writeShellApplication {
    name = "agent-server";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      gawk
      gnugrep
    ];
    text = ''
      set -Eeuo pipefail

      state_root=
      runner=
      while [ "$#" -gt 0 ]; do
        case "$1" in
          --tenant-state-root)
            test "$#" -ge 2
            state_root=$2
            shift 2
            ;;
          --sandbox-runner)
            test "$#" -ge 2
            runner=$2
            shift 2
            ;;
          *)
            shift
            ;;
        esac
      done

      test "$state_root" = /var/lib/test-agent-server/tenants
      test -x "$runner"
      evidence_root="''${state_root%/tenants}/runtime-evidence"
      rm -rf -- "$evidence_root"
      mkdir -m 0700 -- "$evidence_root"

      cgroup="$(awk -F: '$1 == "0" { print $3 }' /proc/self/cgroup)"
      case "$cgroup" in
        /*.service/supervisor) ;;
        *) exit 20 ;;
      esac
      unit_dir="/sys/fs/cgroup''${cgroup%/supervisor}"
      test ! -s "$unit_dir/cgroup.procs"
      for controller in cpu memory pids; do
        grep -qw -- "$controller" "$unit_dir/cgroup.controllers"
      done
      printf '+cpu +memory +pids\n' >"$unit_dir/cgroup.subtree_control"
      mkdir -p -- "$unit_dir/sandboxes/runtime-test"

      trap 'exit 0' INT TERM
      bash -c 'trap "" INT TERM; exec sleep infinity' &
      child_pid=$!
      printf '%s\n' "$child_pid" >"$unit_dir/sandboxes/runtime-test/cgroup.procs"

      "$runner" marker >"$evidence_root/generation"
      printf '%s\n' "$runner" >"$evidence_root/runner-path"
      printf '%s\n' "$$" >"$evidence_root/main-pid"
      printf '%s\n' "$child_pid" >"$evidence_root/child-pid"
      printf '%s\n' "$cgroup" >"$evidence_root/cgroup"
      id -u >"$evidence_root/uid"
      id -g >"$evidence_root/gid"
      id -G >"$evidence_root/groups"
      cp -- /proc/self/status "$evidence_root/status"
      printf 'ready\n' >"$evidence_root/ready"

      wait "$child_pid"
    '';
  };
in
pkgs.testers.runNixOSTest {
  name = "haskell-agent-server-module-runtime";

  nodes.machine =
    { lib, pkgs, ... }:
    {
      imports = [ (import ../modules/agent-server.nix { inherit self; }) ];

      services.haskell-agent.server = {
        enable = true;
        package = testServer;
        sandboxRunnerPackage = testRunnerV1;
        user = "test-agent";
        group = "test-agent";
        stateDirectory = "test-agent-server";
        tenantRegistryFile = "/run/keys/tenants.json";
        workspaceRoots = [
          "/srv/tenant-a"
          "/srv/tenant-b"
        ];
      };

      specialisation = {
        runner-v2.configuration = {
          services.haskell-agent.server.sandboxRunnerPackage = lib.mkForce testRunnerV2;
        };
        runner-v2-failing.configuration = {
          services.haskell-agent.server.sandboxRunnerPackage = lib.mkForce testRunnerV2;
          system.activationScripts.rejectRunnerGeneration = {
            deps = [ "haskellAgentServerSandboxRunner" ];
            text = ''
              echo 'rejecting test runner generation after installation' >&2
              exit 1
            '';
          };
        };
      };

      systemd.services.haskell-agent-server.serviceConfig.TimeoutStopSec = lib.mkForce "2s";

      systemd.tmpfiles.rules = [
        "d /run/keys 0755 root root -"
        "f /run/keys/tenants.json 0600 test-agent test-agent - {}"
        "d /srv/tenant-a 0700 test-agent test-agent -"
        "d /srv/tenant-b 0700 test-agent test-agent -"
      ];

      environment.systemPackages = [ pkgs.util-linux ];

      virtualisation = {
        cores = 2;
        memorySize = 1024;
      };

      system.stateVersion = "26.05";
    };

  testScript = ''
    evidence = "/var/lib/test-agent-server/runtime-evidence"
    unit_cgroup = "/sys/fs/cgroup/system.slice/haskell-agent-server.service"

    def service_pid():
        return int(machine.succeed(
            "systemctl show --property MainPID --value haskell-agent-server.service"
        ).strip())

    def evidence_pid(name):
        return int(machine.succeed(f"cat {evidence}/{name}").strip())

    def assert_runtime(marker, source, generation_root, runner, staging):
        machine.wait_until_succeeds(
            f'test "$(cat {evidence}/generation)" = {marker}'
        )
        main_pid = service_pid()
        child_pid = evidence_pid("child-pid")
        assert main_pid == evidence_pid("main-pid")
        machine.succeed(
            f"grep -Fx '0::/system.slice/haskell-agent-server.service/supervisor' "
            f"/proc/{main_pid}/cgroup"
        )
        machine.succeed(
            f"grep -Fx '0::/system.slice/haskell-agent-server.service/sandboxes/runtime-test' "
            f"/proc/{child_pid}/cgroup"
        )
        machine.succeed(f"test ! -s {unit_cgroup}/cgroup.procs")
        for controller in ("cpu", "memory", "pids"):
          machine.succeed(
              f"grep -qw -- {controller} {unit_cgroup}/cgroup.controllers"
          )
          machine.succeed(
              f"grep -qw -- {controller} {unit_cgroup}/cgroup.subtree_control"
          )
        machine.succeed(
            f'test "$(stat -c %U:%G {unit_cgroup})" = test-agent:test-agent'
        )
        machine.succeed(f'test "$(cat {evidence}/uid)" != 0')
        machine.succeed(f"cmp -s {evidence}/gid {evidence}/groups")
        machine.succeed(
            f"grep -Eq '^NoNewPrivs:[[:space:]]+1$' {evidence}/status"
        )
        for field in ("CapInh", "CapPrm", "CapEff", "CapBnd", "CapAmb"):
          machine.succeed(
              f"grep -Eq '^{field}:[[:space:]]+0000000000000000$' "
              f"{evidence}/status"
          )
        machine.succeed(
            f'test "$(cat {evidence}/runner-path)" = {runner}'
        )
        for path in (
            "${trustedRunnerBase}",
            "${trustedRunnerRoot}",
            generation_root,
        ):
            machine.succeed(
                f'test "$(stat -c %U:%G:%a {path})" = root:root:755'
            )
        machine.succeed(
            f'test "$(stat -c %U:%G:%a {runner})" = root:root:555'
        )
        machine.succeed(
            'test "$(stat -c %U:%G:%a /var/lib/test-agent-server)" '
            '= test-agent:test-agent:700'
        )
        machine.succeed(f"cmp -s {source} {runner}")
        machine.succeed(f'test "$({runner} marker)" = {marker}')
        machine.succeed(f"test ! -e {staging}")
        machine.fail(
            f"runuser -u test-agent -- touch {generation_root}/forbidden"
        )
        return main_pid, child_pid

    machine.start()
    machine.wait_for_unit("haskell-agent-server.service")
    machine.wait_until_succeeds(f"test -f {evidence}/ready")
    booted_system = machine.succeed("readlink -f /run/current-system").strip()
    upgrade = f"{booted_system}/specialisation/runner-v2"
    failing_upgrade = f"{booted_system}/specialisation/runner-v2-failing"

    with subtest("service runs inside the delegated hardened boundary"):
        v1_main, v1_child = assert_runtime(
            "runner-v1",
            "${testRunnerV1}/bin/agent-sandbox-runner",
            "${trustedRunnerGenerationV1}",
            "${trustedRunnerV1}",
            "${trustedRunnerStagingV1}",
        )

    with subtest("switch couples the new unit to the new runner and reaps descendants"):
        machine.succeed(f"{upgrade}/bin/switch-to-configuration test")
        machine.wait_for_unit("haskell-agent-server.service")
        v2_main, v2_child = assert_runtime(
            "runner-v2",
            "${testRunnerV2}/bin/agent-sandbox-runner",
            "${trustedRunnerGenerationV2}",
            "${trustedRunnerV2}",
            "${trustedRunnerStagingV2}",
        )
        assert v2_main != v1_main
        machine.wait_until_succeeds(f"! kill -0 {v1_main} 2>/dev/null")
        machine.wait_until_succeeds(f"! kill -0 {v1_child} 2>/dev/null")

    with subtest("rollback preserves and restores the old runner generation"):
        machine.succeed(f"{booted_system}/bin/switch-to-configuration test")
        machine.wait_for_unit("haskell-agent-server.service")
        rollback_main, rollback_child = assert_runtime(
            "runner-v1",
            "${testRunnerV1}/bin/agent-sandbox-runner",
            "${trustedRunnerGenerationV1}",
            "${trustedRunnerV1}",
            "${trustedRunnerStagingV1}",
        )
        assert rollback_main != v2_main
        machine.wait_until_succeeds(f"! kill -0 {v2_main} 2>/dev/null")
        machine.wait_until_succeeds(f"! kill -0 {v2_child} 2>/dev/null")
        machine.succeed(
            'test "$(${trustedRunnerV2} marker)" = runner-v2'
        )
        machine.succeed(
            "cmp -s ${testRunnerV2}/bin/agent-sandbox-runner "
            "${trustedRunnerV2}"
        )

    with subtest("failed activation cannot replace the running generation"):
        machine.fail(f"{failing_upgrade}/activate {failing_upgrade}")
        assert service_pid() == rollback_main
        machine.succeed(f"kill -0 {rollback_child}")
        machine.succeed(
            f'test "$(readlink -f /run/current-system)" = "{booted_system}"'
        )
        machine.succeed(
            'test "$(${trustedRunnerV1} marker)" = runner-v1'
        )
        machine.succeed(
            'test "$(${trustedRunnerV2} marker)" = runner-v2'
        )
        machine.succeed("test ! -e ${trustedRunnerStagingV2}")
        machine.succeed(
            "cmp -s ${testRunnerV1}/bin/agent-sandbox-runner "
            "${trustedRunnerV1}"
        )
  '';
}
