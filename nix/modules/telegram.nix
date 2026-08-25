{ self }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    filterAttrs
    hasPrefix
    mapAttrs'
    mapAttrsToList
    mkEnableOption
    mkIf
    mkOption
    nameValuePair
    types
    ;

  cfg = config.services.haskell-agent.telegram;
  enabledInstances = filterAttrs (_: instance: instance.enable) cfg.instances;
  managedInstances = filterAttrs (_: instance: instance.createUser) enabledInstances;
  instanceHomes = mapAttrsToList (_: instance: instance.homeDirectory) enabledInstances;
  instanceUsers = mapAttrsToList (_: instance: instance.user) enabledInstances;
  isAbsoluteEnvironmentFile =
    path: hasPrefix "/" path || (hasPrefix "-/" path && builtins.stringLength path > 2);

  instanceType = types.submodule (
    { name, config, ... }:
    {
      options = {
        enable = mkEnableOption "this Telegram agent instance";

        package = mkOption {
          type = types.package;
          default = self.packages.${pkgs.stdenv.hostPlatform.system}.agent-telegram;
          defaultText = lib.literalExpression "haskell-agent.packages.\${pkgs.system}.agent-telegram";
          description = "The haskell-agent package providing the agent-telegram executable.";
        };

        user = mkOption {
          type = types.str;
          default = "haskell-agent-${name}";
          description = "User account under which the instance runs.";
        };

        group = mkOption {
          type = types.str;
          default = config.user;
          defaultText = lib.literalExpression "config.user";
          description = "Group under which the instance runs.";
        };

        createUser = mkOption {
          type = types.bool;
          default = true;
          description = "Whether to create the configured system user and group.";
        };

        homeDirectory = mkOption {
          type = types.str;
          default = "/var/lib/haskell-agent-telegram-${name}";
          description = ''
            Persistent home directory for the instance. Haskell-agent keeps
            gateway and managed PostgreSQL state below
            <filename>.haskell-agent</filename> in this directory.
          '';
        };

        workingDirectory = mkOption {
          type = types.str;
          description = ''
            Project directory exposed to the agent. It must exist before the
            service starts and be writable when mutating tools are enabled.
          '';
        };

        tokenFile = mkOption {
          type = types.str;
          description = ''
            Path to the BotFather token. The file is loaded with systemd
            credentials and is never copied to the Nix store.
          '';
        };

        provider = mkOption {
          type = types.enum [
            "openai"
            "xai"
            "openrouter"
            "claude-code"
          ];
          default = "openai";
          example = "openrouter";
          description = "Provider name written to the Telegram gateway configuration.";
        };

        model = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "gpt-5.6-sol";
          description = "Optional model override. Null uses the provider default.";
        };

        effort = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "high";
          description = "Optional reasoning-effort override.";
        };

        yolo = mkOption {
          type = types.bool;
          default = false;
          description = "Whether mutating tools may run without interactive approval.";
        };

        allowedUsers = mkOption {
          type = types.listOf types.ints.unsigned;
          default = [ ];
          example = [ 123456789 ];
          description = "Telegram user IDs allowed to interact with the bot.";
        };

        respondToAllGroupMessages = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether messages from allowed users in groups are considered
            without requiring a mention or reply to the bot. The agent stays
            silent when it does not have a useful response.
          '';
        };

        codexHome = mkOption {
          type = types.str;
          default = "${config.homeDirectory}/.codex";
          defaultText = lib.literalExpression ''"${config.homeDirectory}/.codex"'';
          description = "CODEX_HOME used by the OpenAI provider.";
        };

        postgresPackage = mkOption {
          type = types.package;
          default = pkgs.postgresql_18;
          defaultText = lib.literalExpression "pkgs.postgresql_18";
          description = "PostgreSQL package used for the private managed state database.";
        };

        postgresPort = mkOption {
          type = types.port;
          default = 55432;
          description = ''
            Port recorded by the private PostgreSQL cluster. The server listens
            only on its per-instance Unix socket, so instances may share this value.
          '';
        };

        extraPackages = mkOption {
          type = types.listOf types.package;
          default = [ ];
          example = lib.literalExpression "with pkgs; [ gh nix ripgrep ]";
          description = "Additional packages added to PATH for the agent's Bash tool.";
        };

        environment = mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            GH_PROMPT_DISABLED = "true";
            PAGER = "cat";
          };
          description = "Additional environment variables for the service.";
        };

        environmentFiles = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Environment files read by systemd at service start. Use these for
            provider API keys instead of putting secret values in Nix options.
            Prefix an absolute path with <literal>-</literal> to ignore a
            missing file.
          '';
        };
      };
    }
  );

  gatewayDirectory = instance: "${instance.homeDirectory}/.haskell-agent/gateways/telegram";

  gatewayConfig =
    name: instance:
    pkgs.writeText "haskell-agent-telegram-${name}.json" (
      builtins.toJSON {
        provider = instance.provider;
        model = instance.model;
        cwd = instance.workingDirectory;
        effort = instance.effort;
        yolo = instance.yolo;
        allowedUsers = instance.allowedUsers;
        inherit (instance) respondToAllGroupMessages;
      }
    );

  serviceName = name: "haskell-agent-telegram-${name}";
in
{
  options.services.haskell-agent.telegram.instances = mkOption {
    type = types.attrsOf instanceType;
    default = { };
    description = "Named agent-telegram service instances.";
  };

  config = mkIf (enabledInstances != { }) {
    assertions = [
      {
        assertion = builtins.length instanceHomes == builtins.length (lib.unique instanceHomes);
        message = "Enabled Telegram agent instances must use distinct homeDirectory values";
      }
      {
        assertion = builtins.length instanceUsers == builtins.length (lib.unique instanceUsers);
        message = "Enabled Telegram agent instances must use distinct user values";
      }
    ]
    ++ lib.concatLists (
      mapAttrsToList (name: instance: [
        {
          assertion = builtins.match "[a-z][a-z0-9-]{0,15}" name != null;
          message = "Telegram agent instance name ${name} must match [a-z][a-z0-9-]{0,15}";
        }
        {
          assertion = hasPrefix "/" instance.homeDirectory;
          message = "services.haskell-agent.telegram.instances.${name}.homeDirectory must be absolute";
        }
        {
          assertion = hasPrefix "/" instance.codexHome;
          message = "services.haskell-agent.telegram.instances.${name}.codexHome must be absolute";
        }
        {
          assertion = builtins.stringLength "${instance.homeDirectory}/.haskell-agent/postgres/run" <= 90;
          message = "services.haskell-agent.telegram.instances.${name}.homeDirectory is too long for the private PostgreSQL socket";
        }
        {
          assertion = hasPrefix "/" instance.workingDirectory;
          message = "services.haskell-agent.telegram.instances.${name}.workingDirectory must be absolute";
        }
        {
          assertion = instance.allowedUsers != [ ];
          message = "services.haskell-agent.telegram.instances.${name}.allowedUsers must not be empty";
        }
        {
          assertion = builtins.all (userId: userId > 0) instance.allowedUsers;
          message = "services.haskell-agent.telegram.instances.${name}.allowedUsers must contain only positive IDs";
        }
        {
          assertion =
            builtins.length instance.allowedUsers == builtins.length (lib.unique instance.allowedUsers);
          message = "services.haskell-agent.telegram.instances.${name}.allowedUsers must not contain duplicates";
        }
        {
          assertion = hasPrefix "/" instance.tokenFile;
          message = "services.haskell-agent.telegram.instances.${name}.tokenFile must be an absolute runtime path";
        }
        {
          assertion = builtins.all isAbsoluteEnvironmentFile instance.environmentFiles;
          message = "services.haskell-agent.telegram.instances.${name}.environmentFiles must contain only absolute paths, optionally prefixed with -";
        }
      ]) enabledInstances
    );

    users.groups = mapAttrs' (_: instance: nameValuePair instance.group { }) managedInstances;

    users.users = mapAttrs' (
      _: instance:
      nameValuePair instance.user {
        isSystemUser = true;
        group = instance.group;
        home = instance.homeDirectory;
        createHome = true;
        description = "Haskell Agent Telegram service";
      }
    ) managedInstances;

    systemd.tmpfiles.settings = mapAttrs' (
      name: instance:
      nameValuePair "10-haskell-agent-telegram-${name}" (
        lib.genAttrs
          [
            instance.homeDirectory
            "${instance.homeDirectory}/.haskell-agent"
            "${instance.homeDirectory}/.haskell-agent/gateways"
            (gatewayDirectory instance)
          ]
          (_: {
            d = {
              mode = "0700";
              user = instance.user;
              group = instance.group;
            };
          })
      )
    ) enabledInstances;

    systemd.services = mapAttrs' (
      name: instance:
      let
        directory = gatewayDirectory instance;
        generatedConfig = gatewayConfig name instance;
      in
      nameValuePair (serviceName name) {
        description = "Haskell Agent Telegram gateway (${name})";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        unitConfig.RequiresMountsFor = [
          instance.homeDirectory
          instance.workingDirectory
        ];

        environment = instance.environment // {
          HOME = instance.homeDirectory;
          CODEX_HOME = instance.codexHome;
          AGENT_POSTGRES_BIN = "${instance.postgresPackage}/bin";
          AGENT_POSTGRES_PORT = toString instance.postgresPort;
        };

        path = [
          instance.package
          instance.postgresPackage
          pkgs.bash
          pkgs.coreutils
          pkgs.git
        ]
        ++ instance.extraPackages;

        preStart = ''
          install -d -m 0700 ${lib.escapeShellArg directory}
          generated_config="$(mktemp ${lib.escapeShellArg "${directory}/config.json.XXXXXX"})"
          install -m 0600 \
              ${lib.escapeShellArg generatedConfig} \
              "$generated_config"
          mv -f "$generated_config" ${lib.escapeShellArg "${directory}/config.json"}
          ln -sfn \
              "$CREDENTIALS_DIRECTORY/telegram-token" \
              ${lib.escapeShellArg "${directory}/token"}
        '';

        serviceConfig = {
          Type = "simple";
          User = instance.user;
          Group = instance.group;
          WorkingDirectory = instance.workingDirectory;
          ExecStart = "${instance.package}/bin/agent-telegram run";
          LoadCredential = [
            "telegram-token:${instance.tokenFile}"
          ];
          EnvironmentFile = instance.environmentFiles;
          Restart = "on-failure";
          RestartSec = "5s";
          UMask = "0077";
          NoNewPrivileges = true;
          PrivateTmp = true;
          LimitNOFILE = 65536;
        };
      }
    ) enabledInstances;
  };
}
