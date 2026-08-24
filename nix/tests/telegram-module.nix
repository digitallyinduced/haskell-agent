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

  evaluated = nixpkgs.lib.nixosSystem {
    inherit system;
    modules = [
      (import ../modules/telegram.nix { inherit self; })
      {
        system.stateVersion = "26.05";

        services.haskell-agent.telegram.instances = {
          primary = {
            enable = true;
            package = testPackage;
            workingDirectory = "/srv/primary";
            tokenFile = "/run/keys/primary-token";
            allowedUsers = [ 123456789 ];
            model = null;
            effort = null;
            respondToAllGroupMessages = true;
            environment = {
              HOME = "/tmp/ignored";
              AGENT_POSTGRES_PORT = "1";
              MODULE_TEST = "present";
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
        };
      }
    ];
  };

  primary = evaluated.config.systemd.services.haskell-agent-telegram-primary;
  secondary = evaluated.config.systemd.services.haskell-agent-telegram-secondary;

  primaryService = builtins.toJSON {
    inherit (primary) environment preStart;
    inherit (primary.serviceConfig)
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
in
pkgs.runCommand "haskell-agent-telegram-module-test"
  {
    inherit primaryService secondaryService;
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

    test -f "$primary_config"
    test -f "$secondary_config"

    jq -e '
      . == {
        allowedUsers: [123456789],
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
        allowedUsers: [987654321],
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
      and .environment.HOME == "/var/lib/haskell-agent-telegram-primary"
      and .environment.CODEX_HOME == "/var/lib/haskell-agent-telegram-primary/.codex"
      and .environment.AGENT_POSTGRES_PORT == "55432"
      and (.environment.AGENT_POSTGRES_BIN | endswith("/bin"))
      and .environment.MODULE_TEST == "present"
      and (.ExecStart | endswith("/bin/agent-telegram run"))
      and (.preStart | contains("$CREDENTIALS_DIRECTORY/telegram-token"))
    ' >/dev/null

    printf '%s\n' "$secondaryService" | jq -e '
      .User == "haskell-agent-secondary"
      and .WorkingDirectory == "/srv/secondary"
      and .environment.HOME == "/var/lib/haskell-agent-telegram-secondary"
      and .environment.AGENT_POSTGRES_PORT == "55432"
    ' >/dev/null

    touch "$out"
  ''
