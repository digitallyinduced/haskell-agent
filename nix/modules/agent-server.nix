{ self }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    concatMap
    escapeShellArg
    escapeShellArgs
    hasPrefix
    hasInfix
    mkEnableOption
    mkIf
    mkOption
    optional
    types
    unique
    ;

  cfg = config.services.haskell-agent.server;
  stateRoot = "/var/lib/${cfg.stateDirectory}";
  tenantStateRoot = "${stateRoot}/tenants";
  trustedRunnerBase = "/run/haskell-agent-server-runners";
  trustedRunnerRoot = "${trustedRunnerBase}/${cfg.stateDirectory}";
  trustedRunnerGeneration = builtins.baseNameOf (toString cfg.sandboxRunnerPackage);
  trustedRunnerGenerationRoot = "${trustedRunnerRoot}/${trustedRunnerGeneration}";
  trustedRunner = "${trustedRunnerGenerationRoot}/agent-sandbox-runner";
  trustedRunnerStaging = "${trustedRunnerGenerationRoot}/.agent-sandbox-runner.new";
  isCanonicalAbsoluteNonRootPath =
    path:
    let
      components = lib.splitString "/" path;
    in
    hasPrefix "/" path
    && path != "/"
    && builtins.head components == ""
    && builtins.all (component: component != "" && component != "." && component != "..") (
      builtins.tail components
    )
    && !hasInfix "%" path;
  environmentFilePath =
    path:
    if hasPrefix "-" path then builtins.substring 1 (builtins.stringLength path - 1) path else path;
  isCanonicalEnvironmentFile = path: isCanonicalAbsoluteNonRootPath (environmentFilePath path);
  isLoopbackHost = builtins.elem (lib.toLower cfg.host) [
    "127.0.0.1"
    "localhost"
    "::1"
  ];
  isDedicatedUser = cfg.user != "root" && cfg.user != "nobody";
  isDedicatedGroup = cfg.group != "root" && cfg.group != "nobody" && cfg.group != "nogroup";

  serverArguments = [
    "${cfg.package}/bin/agent-server"
    "--host"
    cfg.host
    "--port"
    (toString cfg.port)
    "--tenant-registry"
    cfg.tenantRegistryFile
    "--tenant-state-root"
    tenantStateRoot
    "--sandbox-runner"
    trustedRunner
    "--max-concurrent-turns"
    (toString cfg.maxConcurrentTurns)
    "--max-concurrent-turns-per-tenant"
    (toString cfg.maxConcurrentTurnsPerTenant)
    "--max-queued-turns"
    (toString cfg.maxQueuedTurns)
    "--max-queued-turns-per-tenant"
    (toString cfg.maxQueuedTurnsPerTenant)
    "--max-active-tenants"
    (toString cfg.maxActiveTenants)
    "--max-event-subscribers"
    (toString cfg.maxEventSubscribers)
    "--max-event-subscribers-per-tenant"
    (toString cfg.maxEventSubscribersPerTenant)
    "--event-replay-limit"
    (toString cfg.eventReplayLimit)
    "--maximum-request-bytes"
    (toString cfg.maximumRequestBytes)
  ]
  ++ optional cfg.allowRemote "--allow-remote"
  ++ concatMap (origin: [
    "--cors-origin"
    origin
  ]) cfg.corsOrigins;
in
{
  options.services.haskell-agent.server = {
    enable = mkEnableOption "the multi-tenant haskell-agent HTTP server";

    package = mkOption {
      type = types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.agent-server;
      defaultText = lib.literalExpression "haskell-agent.packages.\${pkgs.system}.agent-server";
      description = "The package providing the agent-server executable.";
    };

    sandboxRunnerPackage = mkOption {
      type = types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.agent-sandbox-runner;
      defaultText = lib.literalExpression "haskell-agent.packages.\${pkgs.system}.agent-sandbox-runner";
      description = ''
        The immutable gVisor runner package. The module installs its executable
        under a root-owned, generation-addressed runtime path because a
        multi-user Nix store normally has group-writable ancestry, which
        agent-server deliberately rejects.
      '';
    };

    user = mkOption {
      type = types.strMatching "[a-z_][a-z0-9_-]{0,30}";
      default = "haskell-agent-server";
      description = "Dedicated system user under which agent-server runs.";
    };

    group = mkOption {
      type = types.strMatching "[a-z_][a-z0-9_-]{0,30}";
      default = "haskell-agent-server";
      description = "Dedicated primary group under which agent-server runs.";
    };

    stateDirectory = mkOption {
      type = types.strMatching "[A-Za-z0-9][A-Za-z0-9_.-]*";
      default = "haskell-agent-server";
      description = ''
        Name of the private systemd StateDirectory below /var/lib. Tenant
        state lives there; trusted runner generations live under a separate
        root-owned hierarchy in /run.
      '';
    };

    tenantRegistryFile = mkOption {
      type = types.str;
      example = "/run/agent-server/tenants.json";
      description = ''
        Canonical absolute non-root runtime path to the strict tenant registry,
        without dot segments, repeated or trailing separators, or systemd
        specifiers. It and every credential file named by it must be regular,
        non-symlink, mode-0600 files owned by the configured service user.
      '';
    };

    workspaceRoots = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [
        "/srv/agent-tenants/acme/workspace"
        "/srv/agent-tenants/example/workspace"
      ];
      description = ''
        Exact tenant workspace roots made writable through the systemd
        filesystem sandbox. Every workspace root in the runtime registry must
        be listed here as a canonical absolute non-root path, without dot
        segments, repeated separators, trailing separators, or systemd
        specifiers. These paths are not passed as local-mode --workspace-root
        arguments.
      '';
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Address on which agent-server listens.";
    };

    port = mkOption {
      type = types.port;
      default = 4096;
      description = "TCP port on which agent-server listens.";
    };

    allowRemote = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Permit a non-loopback listener. Remote deployments still require
        trusted TLS termination in front of agent-server.
      '';
    };

    corsOrigins = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Browser origins explicitly allowed by agent-server.";
    };

    environment = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Additional service environment. Values are written to the Nix store;
        use environmentFiles for secrets.
      '';
    };

    environmentFiles = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Environment files read by systemd at service start. Every entry must
        be a canonical absolute non-root path without dot segments, repeated
        or trailing separators, or systemd specifiers. Prefix it with - to
        ignore a missing file.
      '';
    };

    maxConcurrentTurns = mkOption {
      type = types.ints.positive;
      default = 3;
      description = "Maximum number of concurrently running turns.";
    };

    maxConcurrentTurnsPerTenant = mkOption {
      type = types.ints.positive;
      default = 2;
      description = "Maximum number of concurrently running turns for one tenant.";
    };

    maxQueuedTurns = mkOption {
      type = types.ints.positive;
      default = 100;
      description = "Maximum number of queued turns.";
    };

    maxQueuedTurnsPerTenant = mkOption {
      type = types.ints.positive;
      default = 25;
      description = "Maximum number of queued turns for one tenant.";
    };

    maxActiveTenants = mkOption {
      type = types.ints.positive;
      default = 16;
      description = "Maximum number of active tenant sandboxes.";
    };

    maxEventSubscribers = mkOption {
      type = types.ints.positive;
      default = 256;
      description = "Maximum number of concurrent SSE subscribers.";
    };

    maxEventSubscribersPerTenant = mkOption {
      type = types.ints.positive;
      default = 8;
      description = "Maximum number of concurrent SSE subscribers for one tenant.";
    };

    eventReplayLimit = mkOption {
      type = types.ints.positive;
      default = 1000;
      description = "SSE events retained per authenticated access boundary.";
    };

    maximumRequestBytes = mkOption {
      type = types.ints.positive;
      default = 32 * 1024 * 1024;
      description = "Maximum JSON request body size.";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.workspaceRoots != [ ];
        message = "services.haskell-agent.server.workspaceRoots must not be empty";
      }
      {
        assertion = builtins.all isCanonicalAbsoluteNonRootPath cfg.workspaceRoots;
        message = "services.haskell-agent.server.workspaceRoots must contain only canonical absolute non-root paths without systemd specifiers";
      }
      {
        assertion = builtins.length cfg.workspaceRoots == builtins.length (unique cfg.workspaceRoots);
        message = "services.haskell-agent.server.workspaceRoots must not contain duplicates";
      }
      {
        assertion = isCanonicalAbsoluteNonRootPath cfg.tenantRegistryFile;
        message = "services.haskell-agent.server.tenantRegistryFile must be a canonical absolute non-root path without systemd specifiers";
      }
      {
        assertion = builtins.all isCanonicalEnvironmentFile cfg.environmentFiles;
        message = "services.haskell-agent.server.environmentFiles must contain only canonical absolute non-root paths without systemd specifiers, optionally prefixed with -";
      }
      {
        assertion = isLoopbackHost || cfg.allowRemote;
        message = "services.haskell-agent.server.allowRemote must be true for a non-loopback host";
      }
      {
        assertion = isDedicatedUser;
        message = "services.haskell-agent.server.user must be a dedicated unprivileged account";
      }
      {
        assertion = isDedicatedGroup;
        message = "services.haskell-agent.server.group must be a dedicated unprivileged group";
      }
      {
        assertion = !(cfg.environment ? AGENT_SERVER_TOKEN);
        message = "services.haskell-agent.server.environment must not define AGENT_SERVER_TOKEN in multi-tenant mode";
      }
      {
        assertion = cfg.maxConcurrentTurnsPerTenant <= cfg.maxConcurrentTurns;
        message = "services.haskell-agent.server.maxConcurrentTurnsPerTenant must not exceed maxConcurrentTurns";
      }
      {
        assertion = cfg.maxQueuedTurnsPerTenant <= cfg.maxQueuedTurns;
        message = "services.haskell-agent.server.maxQueuedTurnsPerTenant must not exceed maxQueuedTurns";
      }
      {
        assertion = cfg.maxEventSubscribersPerTenant <= cfg.maxEventSubscribers;
        message = "services.haskell-agent.server.maxEventSubscribersPerTenant must not exceed maxEventSubscribers";
      }
    ];

    # Do not partially redefine a privileged built-in account while NixOS is
    # reporting the assertion above.
    users.groups = mkIf isDedicatedGroup {
      ${cfg.group} = { };
    };
    users.users = mkIf isDedicatedUser {
      ${cfg.user} = {
        isSystemUser = true;
        group = cfg.group;
        extraGroups = lib.mkForce [ ];
        home = stateRoot;
        createHome = false;
        description = "Multi-tenant Haskell Agent server";
      };
    };

    # agent-server validates runner ancestry through the filesystem root. A
    # standard multi-user /nix/store is root:nixbld/1775, so the store path
    # itself correctly fails that policy. Install an immutable copy below
    # root-owned, non-group-writable runtime ancestry instead. Its store-derived
    # generation directory makes the unit and runner generation-atomic: an
    # activation can install the next runner without replacing the path used
    # by the still-running server or by a rollback generation.
    system.activationScripts.haskellAgentServerSandboxRunner = {
      deps = [ "users" ];
      text = ''
        ${pkgs.coreutils}/bin/install \
          -d -m 0700 \
          -o ${escapeShellArg cfg.user} \
          -g ${escapeShellArg cfg.group} \
          ${escapeShellArg stateRoot}
        ${pkgs.coreutils}/bin/install \
          -d -m 0755 -o root -g root \
          ${escapeShellArg trustedRunnerBase} \
          ${escapeShellArg trustedRunnerRoot} \
          ${escapeShellArg trustedRunnerGenerationRoot}
        ${pkgs.coreutils}/bin/install \
          -T \
          -m 0555 -o root -g root \
          ${escapeShellArg "${cfg.sandboxRunnerPackage}/bin/agent-sandbox-runner"} \
          ${escapeShellArg trustedRunnerStaging}
        ${pkgs.coreutils}/bin/mv \
          -fT -- \
          ${escapeShellArg trustedRunnerStaging} \
          ${escapeShellArg trustedRunner}
      '';
    };

    systemd.services.haskell-agent-server = {
      description = "Multi-tenant Haskell Agent HTTP server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      restartTriggers = [ cfg.sandboxRunnerPackage ];
      restartIfChanged = true;
      unitConfig.RequiresMountsFor = [
        stateRoot
        cfg.tenantRegistryFile
      ]
      ++ cfg.workspaceRoots;

      environment = cfg.environment // {
        HOME = stateRoot;
      };

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        StateDirectory = cfg.stateDirectory;
        StateDirectoryMode = "0700";
        WorkingDirectory = stateRoot;
        ExecCondition = "${pkgs.coreutils}/bin/test -x ${trustedRunner}";
        ExecStart = escapeShellArgs serverArguments;
        EnvironmentFile = cfg.environmentFiles;
        Restart = "on-failure";
        RestartSec = "5s";
        TimeoutStopSec = "30s";
        KillSignal = "SIGINT";
        UMask = "0077";

        # Keep the server in a process-only subgroup while delegating exactly
        # the controllers used for bounded per-sandbox child cgroups.
        Delegate = "cpu memory pids";
        DelegateSubgroup = "supervisor";
        ProtectControlGroups = false;
        CPUAccounting = true;
        MemoryAccounting = true;
        TasksAccounting = true;
        CPUQuota = "400%";
        MemoryHigh = "12G";
        MemoryMax = "16G";
        MemorySwapMax = "2G";
        TasksMax = 2048;
        OOMPolicy = "continue";
        KillMode = "control-group";

        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
        AmbientCapabilities = "";
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ stateRoot ] ++ cfg.workspaceRoots;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
          "AF_PACKET"
        ];
        LimitNOFILE = 65536;
      };
    };
  };
}
