# NixOS

The flake exports `nixosModules.default` and `nixosModules.telegram` for
running one or more Telegram gateways as systemd services.

```nix
{
  inputs.haskell-agent.url = "github:digitallyinduced/haskell-agent";

  outputs = { self, nixpkgs, haskell-agent, ... }: {
    nixosConfigurations.example = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        haskell-agent.nixosModules.default
        ({ pkgs, ... }: {
          services.haskell-agent.telegram.instances.assistant = {
            enable = true;
            workingDirectory = "/srv/project";
            tokenFile = "/run/secrets/telegram-bot-token";
            allowedUsers = [ 123456789 ];

            provider = "openai";
            yolo = false;
            extraPackages = with pkgs; [
              gh
              nix
              ripgrep
            ];
          };
        })
      ];
    };
  };
}
```

The module:

- creates a dedicated system user and private home for each instance;
- writes the non-secret gateway configuration declaratively (`allowedUsers` is
  the minimum set after each restart; in-chat `/allow` grants persist in
  `state.json`);
- loads the BotFather token through systemd credentials, without copying it
  to the Nix store;
- supplies Bash, Git, and PostgreSQL 18 by default;
- configures the private managed PostgreSQL cluster used for durable agent
  state; and
- restarts the foreground gateway after failures.

The project directory must exist before the service starts. If `yolo` is
enabled, it must also be writable by the service user. The generated systemd
unit orders itself after mounts needed by the project and instance home
directories.

Instance names must begin with a lowercase letter, contain only lowercase
letters, digits, and hyphens, and be at most 16 characters long. Each enabled
instance must use a distinct service user and home directory. `homeDirectory`,
`codexHome`, `workingDirectory`, and `tokenFile` must be absolute paths.

Disabled instances do not require the enabled-only options:

```nix
services.haskell-agent.telegram.instances.assistant.enable = false;
```

Set `createUser = false` to use an account managed elsewhere. In that case the
configured user and group must already exist, and the home directory must be
writable by them.

## Provider credentials

Provider credentials are separate from the Telegram token. Provision them in
the instance home using the provider's normal login flow, or use
`environmentFiles` for API-key environment variables. For OpenAI subscription
authentication, the default `CODEX_HOME` is:

```text
/var/lib/haskell-agent-telegram-INSTANCE/.codex
```

It can be changed with `codexHome`.

Every `environmentFiles` entry must be an absolute path. Prefix it with `-` to
let systemd ignore a missing file, for example
`-/run/secrets/haskell-agent-provider`.

## State and PostgreSQL

Each instance gets an isolated state tree:

```text
/var/lib/haskell-agent-telegram-INSTANCE/.haskell-agent/
├── gateways/telegram/
│   ├── config.json
│   ├── state.json
│   └── token -> /run/credentials/haskell-agent-telegram-INSTANCE.service/telegram-token
└── postgres/
    ├── data/
    ├── run/
    └── postgres.log
```

Haskell-agent initializes, starts, and migrates its private PostgreSQL server.
It listens only on a mode-0700 Unix socket below the instance state directory;
it does not use the system-wide `services.postgresql` server. Instances may
therefore keep the default port even when they run on the same host.

Changing `postgresPackage` across PostgreSQL major versions requires migrating
the existing cluster first. Haskell-agent rejects a cluster created by an
incompatible major version instead of attempting an unsafe in-place upgrade.

Do not back up a running PostgreSQL data directory as ordinary files. Use
`pg_dump` through the private socket for database contents, and separately
back up `gateways/telegram/state.json`. Provider credentials and the Telegram
token should be covered by the host's secret-management recovery procedure.
