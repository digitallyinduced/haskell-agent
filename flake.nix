{
    description = "Universal agent harness";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        flake-utils.url = "github:numtide/flake-utils";
        nix-filter.url = "github:numtide/nix-filter";
    };

    outputs =
        inputs@{
            self,
            nixpkgs,
            flake-utils,
            nix-filter,
            ...
        }:
        flake-utils.lib.eachDefaultSystem (
            system:
            let
                pkgs = import nixpkgs { inherit system; };

                agentOpenaiSource = nix-filter.lib {
                    root = ./packages/agent-openai;
                    include = [
                        "app"
                        "src"
                        "test"
                        "agent-openai.cabal"
                        "CHANGELOG.md"
                        "LICENSE"
                        "README.md"
                        "UPSTREAM.md"
                    ];
                };

                agentTuiSource = nix-filter.lib {
                    root = ./packages/agent-tui;
                    include = [
                        "src"
                        "test"
                        "agent-tui.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentCoreSource = nix-filter.lib {
                    root = ./packages/agent-core;
                    include = [
                        "src"
                        "test"
                        "agent-core.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentResponsesSource = nix-filter.lib {
                    root = ./packages/agent-responses;
                    include = [
                        "src"
                        "agent-responses.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentCliSource = nix-filter.lib {
                    root = ./packages/agent-cli;
                    include = [
                        "app"
                        "src"
                        "test"
                        "agent-cli.cabal"
                        "LICENSE"
                    ];
                };

                agentXaiSource = nix-filter.lib {
                    root = ./packages/agent-xai;
                    include = [
                        "src"
                        "test"
                        "agent-xai.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentOpenrouterSource = nix-filter.lib {
                    root = ./packages/agent-openrouter;
                    include = [
                        "src"
                        "test"
                        "agent-openrouter.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                haskellPackages = pkgs.haskellPackages.extend (
                    final: previous: {
                        vty-unix = pkgs.haskell.lib.appendPatch
                            previous.vty-unix
                            ./patches/vty-unix-all-motion.patch;
                        agent-core = pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-core/package.nix { }) {
                                src = agentCoreSource;
                            })
                            [
                                pkgs.git
                                pkgs.ripgrep
                            ];
                        agent-responses = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-responses/package.nix { }) {
                            src = agentResponsesSource;
                        };
                        agent-openai = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-openai/package.nix { }) {
                            src = agentOpenaiSource;
                        };
                        agent-xai = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-xai/package.nix { }) {
                            src = agentXaiSource;
                        };
                        agent-openrouter = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-openrouter/package.nix { }) {
                            src = agentOpenrouterSource;
                        };
                        agent-tui = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-tui/package.nix { }) {
                            src = agentTuiSource;
                        };
                        agent-cli = pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-cli/package.nix { }) {
                                src = agentCliSource;
                            })
                            [ pkgs.git ];
                    }
                );

                agentCorePackage = haskellPackages.agent-core;
                agentResponsesPackage = haskellPackages.agent-responses;
                agentOpenaiPackage = haskellPackages.agent-openai;
                agentXaiPackage = haskellPackages.agent-xai;
                agentOpenrouterPackage = haskellPackages.agent-openrouter;
                agentTuiPackage = haskellPackages.agent-tui;
                agentCliPackage = haskellPackages.agent-cli;
                agentCliExecutable = pkgs.haskell.lib.justStaticExecutables agentCliPackage;
                agentOpenaiExecutables = pkgs.haskell.lib.justStaticExecutables agentOpenaiPackage;

                # Opens cabal repl on the agent-cli library and enters the
                # GHCi :cmd loop that reloads + resumes after agent :reload.
                # Keep this single-component: GHC 9.10 multi-home-unit mode
                # does not support the :module or :cmd commands used below.
                # Use the documented manual multi-package REPL when editing
                # agent-tui or another dependency alongside agent-cli.
                # expect waits for modules to load, then starts the agent
                # (ghci scripts run before cabal loads the package).
                agentRepl = pkgs.writeShellScriptBin "repl" ''
                    set -euo pipefail
                    root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
                    script="$root/scripts/agent-repl.ghci"
                    if [ ! -f "$script" ]; then
                      echo "repl: missing $script" >&2
                      exit 1
                    fi
                    # Cap the long-running GHCi/agent process without forcing
                    # every command in nix develop to inherit the same limit.
                    if [ -z "''${GHCRTS:-}" ]; then
                      export GHCRTS="-M8G -A64m"
                    fi
                    cabal="${haskellPackages.cabal-install}/bin/cabal"
                    expect_bin="${pkgs.expect}/bin/expect"
                    export AGENT_REPL_SCRIPT="$script"
                    export AGENT_REPL_CABAL="$cabal"
                    exec "$expect_bin" -c '
                      set timeout -1
                      set cabal $env(AGENT_REPL_CABAL)
                      set script $env(AGENT_REPL_SCRIPT)
                      spawn -noecho $cabal repl lib:agent-cli --repl-options=-ghci-script=$script
                      expect {
                        -re {Ok, [0-9]+ modules? loaded\.} {}
                        eof {
                          puts stderr "repl: cabal repl exited before modules loaded"
                          exit 1
                        }
                      }
                      send -- ":module +Agent.CLI\r"
                      expect -re {ghci>}
                      send -- ":cmd afterDev =<< devMain\r"
                      interact
                    '
                '';
            in
            {
                packages.default = agentCliExecutable;
                packages.agent-cli = agentCliExecutable;
                packages.agent-core = agentCorePackage;
                packages.agent-tui = agentTuiPackage;
                packages.agent-responses = agentResponsesPackage;
                packages.agent-openai = agentOpenaiPackage;
                packages.agent-xai = agentXaiPackage;
                packages.agent-openrouter = agentOpenrouterPackage;
                packages.agent-openai-login = agentOpenaiExecutables;

                apps.default = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-cli;
                    exePath = "/bin/agent-cli";
                };
                apps.agent-openai-login = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-openai-login;
                    exePath = "/bin/agent-openai-login";
                };

                devShells.default = haskellPackages.shellFor {
                    packages = packages: [
                        packages.agent-cli
                        packages.agent-core
                        packages.agent-tui
                        packages.agent-responses
                        packages.agent-openai
                        packages.agent-xai
                        packages.agent-openrouter
                    ];
                    withHoogle = false;
                    nativeBuildInputs =
                        (with haskellPackages; [
                            cabal-install
                            ghcid
                        ])
                        ++ (with pkgs; [
                            cabal2nix
                            ripgrep
                        ])
                        ++ [ agentRepl ];
                };

                checks = {
                    agent-cli = agentCliPackage;
                    agent-core = agentCorePackage;
                    agent-tui = agentTuiPackage;
                    agent-responses = agentResponsesPackage;
                    agent-openai = agentOpenaiPackage;
                    agent-xai = agentXaiPackage;
                    agent-openrouter = agentOpenrouterPackage;
                };

                formatter = pkgs.nixfmt-rfc-style;
            }
        );
}
