{
  self,
  nixpkgs,
  pkgs,
  system,
}:
let
  testPackage = pkgs.writeShellScriptBin "agent-server" ''
    exit 0
  '';
  testRunner = pkgs.writeShellScriptBin "agent-sandbox-runner" ''
    exit 0
  '';
  trustedTestRunnerGeneration = builtins.baseNameOf (toString testRunner);
  trustedTestRunnerRoot = "/run/haskell-agent-server-runners/test-agent-server";
  trustedTestRunnerGenerationRoot = "${trustedTestRunnerRoot}/${trustedTestRunnerGeneration}";
  trustedTestRunner = "${trustedTestRunnerGenerationRoot}/agent-sandbox-runner";
  trustedTestRunnerStaging = "${trustedTestRunnerGenerationRoot}/.agent-sandbox-runner.new";

  baseServerConfig = {
    enable = true;
    package = testPackage;
    sandboxRunnerPackage = testRunner;
    tenantRegistryFile = "/run/keys/tenants.json";
    workspaceRoots = [
      "/srv/tenant-a"
      "/srv/tenant-b"
    ];
  };

  evaluate =
    serverConfig:
    nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        (import ../modules/agent-server.nix { inherit self; })
        {
          boot.isContainer = true;
          system.stateVersion = "26.05";
          services.haskell-agent.server = baseServerConfig // serverConfig;
        }
      ];
    };

  evaluated = evaluate {
    user = "test-agent";
    group = "test-agent";
    stateDirectory = "test-agent-server";
    host = "0.0.0.0";
    port = 4444;
    allowRemote = true;
    corsOrigins = [ "https://console.example.test" ];
    environment.MODULE_TEST = "present";
    environmentFiles = [
      "/run/keys/provider"
      "-/run/keys/optional-provider"
    ];
    maxConcurrentTurns = 7;
    maxConcurrentTurnsPerTenant = 3;
    maxQueuedTurns = 23;
    maxQueuedTurnsPerTenant = 11;
    maxActiveTenants = 5;
    maxEventSubscribers = 19;
    maxEventSubscribersPerTenant = 4;
    eventReplayLimit = 31;
    maximumRequestBytes = 65536;
  };

  service = evaluated.config.systemd.services.haskell-agent-server;
  unit = evaluated.config.systemd.units."haskell-agent-server.service".unit;
  activation = evaluated.config.system.activationScripts.haskellAgentServerSandboxRunner.text;

  serviceFacts = builtins.toJSON {
    inherit (service)
      after
      environment
      restartIfChanged
      restartTriggers
      wantedBy
      wants
      ;
    requiresMountsFor = service.unitConfig.RequiresMountsFor;
    inherit (service.serviceConfig)
      AmbientCapabilities
      CapabilityBoundingSet
      CPUAccounting
      CPUQuota
      Delegate
      DelegateSubgroup
      EnvironmentFile
      ExecCondition
      ExecStart
      Group
      KillMode
      KillSignal
      LimitNOFILE
      MemoryAccounting
      MemoryHigh
      MemoryMax
      MemorySwapMax
      NoNewPrivileges
      OOMPolicy
      PrivateTmp
      ProtectControlGroups
      ProtectHome
      ProtectSystem
      ReadWritePaths
      Restart
      RestartSec
      RestrictAddressFamilies
      StateDirectory
      StateDirectoryMode
      TasksAccounting
      TasksMax
      TimeoutStopSec
      Type
      UMask
      User
      WorkingDirectory
      ;
  };

  moduleFacts = builtins.toJSON {
    inherit activation;
    groupExists = builtins.hasAttr "test-agent" evaluated.config.users.groups;
    user = {
      inherit (evaluated.config.users.users.test-agent)
        createHome
        description
        extraGroups
        group
        home
        isSystemUser
        ;
    };
  };

  failedAssertions =
    serverConfig:
    map (assertion: assertion.message) (
      builtins.filter (assertion: !assertion.assertion) (evaluate serverConfig).config.assertions
    );

  validFailures = builtins.toJSON (failedAssertions { });
  hiddenWorkspaceFailures = builtins.toJSON (failedAssertions {
    workspaceRoots = [ "/.hidden" ];
  });
  validationFailures = builtins.toJSON {
    duplicateWorkspaceRoots = failedAssertions {
      workspaceRoots = [
        "/srv/tenant"
        "/srv/tenant"
      ];
    };
    emptyWorkspaceRoots = failedAssertions {
      workspaceRoots = [ ];
    };
    limitInversions = failedAssertions {
      maxConcurrentTurns = 1;
      maxConcurrentTurnsPerTenant = 2;
      maxQueuedTurns = 1;
      maxQueuedTurnsPerTenant = 2;
      maxEventSubscribers = 1;
      maxEventSubscribersPerTenant = 2;
    };
    registryRelative = failedAssertions {
      tenantRegistryFile = "tenants.json";
    };
    remoteWithoutOptIn = failedAssertions {
      host = "0.0.0.0";
      allowRemote = false;
    };
    reservedAccounts = failedAssertions {
      user = "root";
      group = "nogroup";
    };
    secretEnvironment = failedAssertions {
      environment.AGENT_SERVER_TOKEN = "must-not-enter-the-store";
    };
    workspaceNotAbsolute = failedAssertions {
      workspaceRoots = [
        "relative"
        "/"
      ];
    };
    workspaceDotRoot = failedAssertions {
      workspaceRoots = [ "/." ];
    };
    workspaceDotSegment = failedAssertions {
      workspaceRoots = [ "/srv/./tenant" ];
    };
    workspaceParentSegment = failedAssertions {
      workspaceRoots = [ "/srv/tenant/.." ];
    };
    workspaceRepeatedSeparator = failedAssertions {
      workspaceRoots = [ "/srv//tenant" ];
    };
    workspaceRootAlias = failedAssertions {
      workspaceRoots = [ "/srv/tenant/../.." ];
    };
    workspaceSpecifier = failedAssertions {
      workspaceRoots = [ "/srv/%n" ];
    };
    workspaceTrailingSeparator = failedAssertions {
      workspaceRoots = [ "/srv/tenant/" ];
    };
    environmentFileRelative = failedAssertions {
      environmentFiles = [ "provider.env" ];
    };
    environmentFileNotCanonical = failedAssertions {
      environmentFiles = [ "-/run/keys//provider.env" ];
    };
    registryNotCanonical = failedAssertions {
      tenantRegistryFile = "/run/keys/../tenants.json";
    };
  };
in
pkgs.runCommand "haskell-agent-server-module-test"
  {
    inherit
      hiddenWorkspaceFailures
      moduleFacts
      serviceFacts
      unit
      validationFailures
      validFailures
      ;
    nativeBuildInputs = [ pkgs.jq ];
  }
  ''
    printf '%s\n' "$serviceFacts" | jq -e \
      --arg runner "${testRunner}" \
      --arg trustedRunner "${trustedTestRunner}" '
      .after == ["network-online.target"]
      and .wants == ["network-online.target"]
      and .wantedBy == ["multi-user.target"]
      and .restartTriggers == [$runner]
      and .restartIfChanged
      and .ExecCondition == "'"${pkgs.coreutils}"'/bin/test -x " + $trustedRunner
      and .requiresMountsFor == [
        "/var/lib/test-agent-server",
        "/run/keys/tenants.json",
        "/srv/tenant-a",
        "/srv/tenant-b"
      ]
      and .environment.HOME == "/var/lib/test-agent-server"
      and .environment.MODULE_TEST == "present"
      and (.environment | has("AGENT_SERVER_TOKEN") | not)
      and .Type == "simple"
      and .User == "test-agent"
      and .Group == "test-agent"
      and .StateDirectory == "test-agent-server"
      and .StateDirectoryMode == "0700"
      and .WorkingDirectory == "/var/lib/test-agent-server"
      and .EnvironmentFile == [
        "/run/keys/provider",
        "-/run/keys/optional-provider"
      ]
      and .Restart == "on-failure"
      and .RestartSec == "5s"
      and .TimeoutStopSec == "30s"
      and .KillSignal == "SIGINT"
      and .UMask == "0077"
      and .Delegate == "cpu memory pids"
      and .DelegateSubgroup == "supervisor"
      and (.ProtectControlGroups | not)
      and .CPUAccounting
      and .MemoryAccounting
      and .TasksAccounting
      and .CPUQuota == "400%"
      and .MemoryHigh == "12G"
      and .MemoryMax == "16G"
      and .MemorySwapMax == "2G"
      and .TasksMax == 2048
      and .OOMPolicy == "continue"
      and .KillMode == "control-group"
      and .NoNewPrivileges
      and .CapabilityBoundingSet == ""
      and .AmbientCapabilities == ""
      and .PrivateTmp
      and .ProtectHome
      and .ProtectSystem == "strict"
      and .ReadWritePaths == [
        "/var/lib/test-agent-server",
        "/srv/tenant-a",
        "/srv/tenant-b"
      ]
      and .RestrictAddressFamilies == [
        "AF_UNIX",
        "AF_INET",
        "AF_INET6",
        "AF_NETLINK",
        "AF_PACKET"
      ]
      and .LimitNOFILE == 65536
      and (.ExecStart | startswith("'"${testPackage}"'/bin/agent-server "))
      and (.ExecStart | contains("--host 0.0.0.0 --port 4444"))
      and (.ExecStart | contains("--tenant-registry /run/keys/tenants.json"))
      and (.ExecStart | contains("--tenant-state-root /var/lib/test-agent-server/tenants"))
      and (.ExecStart | contains("--sandbox-runner " + $trustedRunner))
      and (.ExecStart | contains("--max-concurrent-turns 7"))
      and (.ExecStart | contains("--max-concurrent-turns-per-tenant 3"))
      and (.ExecStart | contains("--max-queued-turns 23"))
      and (.ExecStart | contains("--max-queued-turns-per-tenant 11"))
      and (.ExecStart | contains("--max-active-tenants 5"))
      and (.ExecStart | contains("--max-event-subscribers 19"))
      and (.ExecStart | contains("--max-event-subscribers-per-tenant 4"))
      and (.ExecStart | contains("--event-replay-limit 31"))
      and (.ExecStart | contains("--maximum-request-bytes 65536"))
      and (.ExecStart | contains("--allow-remote"))
      and (.ExecStart | contains("--cors-origin https://console.example.test"))
      and (.ExecStart | contains($runner) | not)
    ' >/dev/null

    printf '%s\n' "$moduleFacts" | jq -e \
      --arg runner "${testRunner}" \
      --arg trustedRunner "${trustedTestRunner}" '
      .groupExists
      and .user == {
        createHome: false,
        description: "Multi-tenant Haskell Agent server",
        extraGroups: [],
        group: "test-agent",
        home: "/var/lib/test-agent-server",
        isSystemUser: true
      }
      and (.activation | contains($runner + "/bin/agent-sandbox-runner"))
      and (.activation | contains("/run/haskell-agent-server-runners/test-agent-server"))
      and (.activation | contains($trustedRunner + ".new") | not)
      and (.activation | contains("'"${trustedTestRunnerStaging}"'"))
      and (.activation | contains("-m 0555 -o root -g root"))
      and (.activation | contains("-fT --"))
    ' >/dev/null

    renderedUnit="$unit/haskell-agent-server.service"
    grep -Fx 'Delegate=cpu memory pids' "$renderedUnit" >/dev/null
    grep -Fx 'DelegateSubgroup=supervisor' "$renderedUnit" >/dev/null
    grep -Fx 'ProtectControlGroups=false' "$renderedUnit" >/dev/null
    grep -Fx 'NoNewPrivileges=true' "$renderedUnit" >/dev/null
    grep -Fx 'CapabilityBoundingSet=' "$renderedUnit" >/dev/null
    grep -Fx 'AmbientCapabilities=' "$renderedUnit" >/dev/null
    grep -Fx 'KillMode=control-group' "$renderedUnit" >/dev/null
    grep -Fx 'ExecCondition='"${pkgs.coreutils}"'/bin/test -x '"${trustedTestRunner}" "$renderedUnit" >/dev/null

    printf '%s\n' "$validFailures" | jq -e 'length == 0' >/dev/null
    printf '%s\n' "$hiddenWorkspaceFailures" | jq -e 'length == 0' >/dev/null
    printf '%s\n' "$validationFailures" | jq -e '
      (.duplicateWorkspaceRoots | any(contains("workspaceRoots must not contain duplicates")))
      and (.emptyWorkspaceRoots | any(contains("workspaceRoots must not be empty")))
      and (.workspaceNotAbsolute | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceDotRoot | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceDotSegment | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceParentSegment | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceRepeatedSeparator | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceRootAlias | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceSpecifier | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.workspaceTrailingSeparator | any(contains("workspaceRoots must contain only canonical absolute non-root paths")))
      and (.registryRelative | any(contains("tenantRegistryFile must be a canonical absolute non-root path")))
      and (.registryNotCanonical | any(contains("tenantRegistryFile must be a canonical absolute non-root path")))
      and (.environmentFileRelative | any(contains("environmentFiles must contain only canonical absolute non-root paths")))
      and (.environmentFileNotCanonical | any(contains("environmentFiles must contain only canonical absolute non-root paths")))
      and (.remoteWithoutOptIn | any(contains("allowRemote must be true")))
      and (.reservedAccounts | any(contains("user must be a dedicated unprivileged account")))
      and (.reservedAccounts | any(contains("group must be a dedicated unprivileged group")))
      and (.secretEnvironment | any(contains("must not define AGENT_SERVER_TOKEN")))
      and (.limitInversions | any(contains("maxConcurrentTurnsPerTenant must not exceed")))
      and (.limitInversions | any(contains("maxQueuedTurnsPerTenant must not exceed")))
      and (.limitInversions | any(contains("maxEventSubscribersPerTenant must not exceed")))
    ' >/dev/null

    touch "$out"
  ''
