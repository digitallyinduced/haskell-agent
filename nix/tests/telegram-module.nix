{
  self,
  nixpkgs,
  pkgs,
  system,
}:
let
  testPackage = pkgs.writeShellScriptBin "agent-telegram" ''
    exit 0
  '';

  baseModule = {
    system.stateVersion = "26.05";
  };

  evaluate =
    module:
    nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        (import ../modules/telegram.nix { inherit self; })
        baseModule
        module
      ];
    };

  evaluated = evaluate {
    services.haskell-agent.telegram.instances = {
      primary = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/primary";
        tokenFile = "/run/keys/primary-token";
        allowedUsers = [ 123456789 ];
        contextBotUsers = [ 222333444 ];
        model = null;
        effort = null;
        respondToAllGroupMessages = true;
        environment = {
          HOME = "/tmp/ignored";
          AGENT_POSTGRES_PORT = "1";
          MODULE_TEST = "present";
        };
        environmentFiles = [
          "/run/keys/provider"
          "-/run/keys/optional-provider"
        ];
        mcpInitStrategy = "progressive";
        mcpServers.example = {
          command = "/nix/store/example/bin/example-mcp";
          args = [ "--stdio" ];
          environment.CREDENTIAL_FILE = "/run/keys/example";
          startupTimeoutSeconds = 12;
          requestTimeoutSeconds = 34;
        };
        mcpServers.remote = {
          url = "https://example.test/mcp";
          startupTimeoutSeconds = 20;
        };
      };

      secondary = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/secondary";
        tokenFile = "/run/keys/secondary-token";
        allowedUsers = [ 987654321 ];
        provider = "openrouter";
        model = "openai/gpt-5.1";
        postgresPort = 55432;
      };

      external = {
        enable = true;
        package = testPackage;
        createUser = false;
        user = "external-agent";
        group = "external-agent";
        homeDirectory = "/srv/external-home";
        workingDirectory = "/srv/external";
        tokenFile = "/run/keys/external-token";
        allowedUsers = [ 111222333 ];
      };

      disabled.enable = false;
    };
  };

  primary = evaluated.config.systemd.services.haskell-agent-telegram-primary;
  secondary = evaluated.config.systemd.services.haskell-agent-telegram-secondary;

  primaryService = builtins.toJSON {
    inherit (primary) environment preStart;
    inherit (primary.serviceConfig)
      EnvironmentFile
      ExecStart
      Group
      LoadCredential
      User
      WorkingDirectory
      ;
  };

  secondaryService = builtins.toJSON {
    inherit (secondary) environment preStart;
    inherit (secondary.serviceConfig)
      ExecStart
      Group
      LoadCredential
      User
      WorkingDirectory
      ;
  };

  moduleFacts = builtins.toJSON {
    disabledServiceExists = builtins.hasAttr "haskell-agent-telegram-disabled" evaluated.config.systemd.services;
    disabledUserExists = builtins.hasAttr "haskell-agent-disabled" evaluated.config.users.users;
    externalUserExists = builtins.hasAttr "external-agent" evaluated.config.users.users;
    externalGroupExists = builtins.hasAttr "external-agent" evaluated.config.users.groups;
    primaryRequiresMountsFor = primary.unitConfig.RequiresMountsFor;
    primaryTmpfiles = evaluated.config.systemd.tmpfiles.settings."10-haskell-agent-telegram-primary";
    primaryUser = {
      inherit (evaluated.config.users.users.haskell-agent-primary)
        createHome
        description
        group
        home
        isSystemUser
        ;
    };
  };

  failedAssertions =
    module:
    map (assertion: assertion.message) (
      builtins.filter (assertion: !assertion.assertion) (evaluate module).config.assertions
    );

  validationFailures = builtins.toJSON {
    relativeCodexHome = failedAssertions {
      services.haskell-agent.telegram.instances.invalid = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/invalid";
        tokenFile = "/run/keys/invalid-token";
        allowedUsers = [ 1 ];
        codexHome = "relative";
      };
    };
    duplicateContextBotUsers = failedAssertions {
      services.haskell-agent.telegram.instances.invalid = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/invalid";
        tokenFile = "/run/keys/invalid-token";
        allowedUsers = [ 1 ];
        contextBotUsers = [
          2
          2
        ];
      };
    };
    overlappingContextBotUsers = failedAssertions {
      services.haskell-agent.telegram.instances.invalid = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/invalid";
        tokenFile = "/run/keys/invalid-token";
        allowedUsers = [ 2 ];
        contextBotUsers = [ 2 ];
      };
    };
    nonPositiveContextBotUsers = failedAssertions {
      services.haskell-agent.telegram.instances.invalid = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/invalid";
        tokenFile = "/run/keys/invalid-token";
        allowedUsers = [ 1 ];
        contextBotUsers = [ 0 ];
      };
    };
    duplicateAllowedUsers = failedAssertions {
      services.haskell-agent.telegram.instances.invalid = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/invalid";
        tokenFile = "/run/keys/invalid-token";
        allowedUsers = [
          1
          1
        ];
      };
    };
    relativeEnvironmentFile = failedAssertions {
      services.haskell-agent.telegram.instances.invalid = {
        enable = true;
        package = testPackage;
        workingDirectory = "/srv/invalid";
        tokenFile = "/run/keys/invalid-token";
        allowedUsers = [ 1 ];
        environmentFiles = [ "relative.env" ];
      };
    };
  };
in
pkgs.runCommand "haskell-agent-telegram-module-test"
  {
    inherit
      moduleFacts
      primaryService
      secondaryService
      validationFailures
      ;
    nativeBuildInputs = [ pkgs.jq ];
  }
  ''
    primary_config="$(
      printf '%s\n' "$primaryService" |
        jq -r .preStart |
        grep -o "/nix/store/[^ ']*-haskell-agent-telegram-primary.json" |
        head -n1
    )"
    secondary_config="$(
      printf '%s\n' "$secondaryService" |
        jq -r .preStart |
        grep -o "/nix/store/[^ ']*-haskell-agent-telegram-secondary.json" |
        head -n1
    )"
    primary_mcp_config="$(
      printf '%s\n' "$primaryService" |
        jq -r .preStart |
        grep -o "/nix/store/[^ ']*-haskell-agent-telegram-primary-mcp.json" |
        head -n1
    )"

    test -f "$primary_config"
    test -f "$secondary_config"
    test -f "$primary_mcp_config"

    jq -e '
      . == {
        allowedUsers: [123456789],
        contextBotUsers: [222333444],
        cwd: "/srv/primary",
        effort: null,
        model: null,
        provider: "openai",
        respondToAllGroupMessages: true,
        yolo: false
      }
    ' "$primary_config" >/dev/null

    jq -e '
      . == {
        mcpInitStrategy: "progressive",
        mcpServers: {
          example: {
            args: ["--stdio"],
            command: "/nix/store/example/bin/example-mcp",
            cwd: null,
            enabled: true,
            env: { CREDENTIAL_FILE: "/run/keys/example" },
            requestTimeoutSeconds: 34,
            startupTimeoutSeconds: 12
          },
          remote: {
            args: [],
            cwd: null,
            enabled: true,
            env: {},
            requestTimeoutSeconds: 60,
            startupTimeoutSeconds: 20,
            url: "https://example.test/mcp"
          }
        }
      }
    ' "$primary_mcp_config" >/dev/null

    jq -e '
      . == {
        allowedUsers: [987654321],
        contextBotUsers: [],
        cwd: "/srv/secondary",
        effort: null,
        model: "openai/gpt-5.1",
        provider: "openrouter",
        respondToAllGroupMessages: false,
        yolo: false
      }
    ' "$secondary_config" >/dev/null

    printf '%s\n' "$primaryService" | jq -e '
      .User == "haskell-agent-primary"
      and .Group == "haskell-agent-primary"
      and .WorkingDirectory == "/srv/primary"
      and .LoadCredential == ["telegram-token:/run/keys/primary-token"]
      and .EnvironmentFile == [
        "/run/keys/provider",
        "-/run/keys/optional-provider"
      ]
      and .environment.HOME == "/var/lib/haskell-agent-telegram-primary"
      and .environment.CODEX_HOME == "/var/lib/haskell-agent-telegram-primary/.codex"
      and .environment.AGENT_POSTGRES_PORT == "55432"
      and (.environment.AGENT_POSTGRES_BIN | endswith("/bin"))
      and .environment.MODULE_TEST == "present"
      and (.ExecStart | endswith("/bin/agent-telegram run"))
      and (.preStart | contains("$CREDENTIALS_DIRECTORY/telegram-token"))
      and (.preStart | contains("jq -n --slurpfile managed"))
      and (.preStart | contains("jq -e ."))
      and (.preStart | contains("[ -s \"$harness_config\" ] && grep -q '[^[:space:]]' \"$harness_config\""))
      and (.preStart | contains(".mcpInitStrategy = $managed[0].mcpInitStrategy"))
      and (.preStart | contains(".mcpServers = $managed[0].mcpServers"))
    ' >/dev/null

    printf '%s\n' "$secondaryService" | jq -e '
      .User == "haskell-agent-secondary"
      and .WorkingDirectory == "/srv/secondary"
      and .environment.HOME == "/var/lib/haskell-agent-telegram-secondary"
      and .environment.AGENT_POSTGRES_PORT == "55432"
    ' >/dev/null

    printf '%s\n' "$moduleFacts" | jq -e '
      (.disabledServiceExists | not)
      and (.disabledUserExists | not)
      and (.externalUserExists | not)
      and (.externalGroupExists | not)
      and .primaryRequiresMountsFor == [
        "/var/lib/haskell-agent-telegram-primary",
        "/srv/primary"
      ]
      and .primaryUser == {
        createHome: true,
        description: "Haskell Agent Telegram service",
        group: "haskell-agent-primary",
        home: "/var/lib/haskell-agent-telegram-primary",
        isSystemUser: true
      }
      and .primaryTmpfiles["/var/lib/haskell-agent-telegram-primary"].d.mode == "0700"
      and .primaryTmpfiles["/var/lib/haskell-agent-telegram-primary"].d.user == "haskell-agent-primary"
      and .primaryTmpfiles["/var/lib/haskell-agent-telegram-primary/.haskell-agent/gateways/telegram"].d.mode == "0700"
    ' >/dev/null

    printf '%s\n' "$validationFailures" | jq -e '
      (.relativeCodexHome | any(contains(".codexHome must be absolute")))
      and (.duplicateAllowedUsers | any(contains(".allowedUsers must not contain duplicates")))
      and (.duplicateContextBotUsers | any(contains(".contextBotUsers must not contain duplicates")))
      and (.overlappingContextBotUsers | any(contains(".contextBotUsers must not overlap allowedUsers")))
      and (.nonPositiveContextBotUsers | any(contains(".contextBotUsers must contain only positive IDs")))
      and (.relativeEnvironmentFile | any(contains(".environmentFiles must contain only absolute paths")))
    ' >/dev/null

    touch "$out"
  ''
