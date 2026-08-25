{
    description = "Universal agent harness";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        flake-utils.url = "github:numtide/flake-utils";
        nix-filter.url = "github:numtide/nix-filter";
        skylighting = {
            url = "github:jgm/skylighting/e432d65743ecef9475816b2cc074d34833837ced";
            flake = false;
        };
    };

    outputs =
        inputs@{
            self,
            nixpkgs,
            flake-utils,
            nix-filter,
            skylighting,
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

                agentProcessSource = nix-filter.lib {
                    root = ./packages/agent-process;
                    include = [
                        "src"
                        "agent-process.cabal"
                        "LICENSE"
                    ];
                };

                agentResponsesTypesSource = nix-filter.lib {
                    root = ./packages/agent-responses-types;
                    include = [
                        "src"
                        "agent-responses-types.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };
                agentCodexDialectSource = nix-filter.lib {
                    root = ./packages/agent-codex-dialect;
                    include = [
                        "src"
                        "test"
                        "agent-codex-dialect.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentGrokBuildDialectSource = nix-filter.lib {
                    root = ./packages/agent-grok-build-dialect;
                    include = [
                        "src"
                        "test"
                        "agent-grok-build-dialect.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentStoreSource = nix-filter.lib {
                    root = ./packages/agent-store;
                    include = [
                        "src"
                        "test"
                        "agent-store.cabal"
                        "LICENSE"
                    ];
                };

                claudeAgentSdkHaskellSource = nix-filter.lib {
                    root = ./packages/claude-agent-sdk-haskell;
                    include = [
                        "src"
                        "test"
                        "claude-agent-sdk-haskell.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentClaudeSource = nix-filter.lib {
                    root = ./packages/agent-claude;
                    include = [
                        "src"
                        "test"
                        "agent-claude.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentSyntaxSource = nix-filter.lib {
                    root = ./packages/agent-syntax;
                    include = [
                        "src"
                        "test"
                        "agent-syntax.cabal"
                        "LICENSE"
                        "README.md"
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
                        "test"
                        "agent-responses.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentCliSource = nix-filter.lib {
                    root = ./packages/agent-cli;
                    include = [
                        "app"
                        "config"
                        "eval"
                        "skills"
                        "src"
                        "test"
                        "agent-cli.cabal"
                        "LICENSE"
                    ];
                };

                agentTelegramSource = nix-filter.lib {
                    root = ./packages/agent-telegram;
                    include = [
                        "app"
                        "src"
                        "test"
                        "agent-telegram.cabal"
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

                skylightingSyntaxes = pkgs.runCommand "skylighting-syntaxes" { } ''
                    mkdir -p "$out/share/skylighting/xml"
                    cp ${skylighting}/skylighting-core/xml/*.xml \
                        "$out/share/skylighting/xml/"

                    mkdir -p "$out/share/doc/skylighting-syntaxes"
                    cp ${skylighting}/skylighting-core/README.md \
                        "$out/share/doc/skylighting-syntaxes/UPSTREAM_README.md"
                    cat > "$out/share/doc/skylighting-syntaxes/NOTICE" <<'EOF'
                    These unmodified KDE XML syntax definitions come from the
                    pinned jgm/skylighting source. They are distributed under
                    various licenses recorded in the individual XML files.
                    See UPSTREAM_README.md for upstream provenance.
                    EOF
                '';
                skylightingSyntaxDirectory =
                    "${skylightingSyntaxes}/share/skylighting/xml";

                haskellPackages = pkgs.haskellPackages.extend (
                    final: previous: {
                        pqi = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "pqi";
                                ver = "1.1.0.1";
                                sha256 = "sha256-y92T7Cry8sGOt/iCHwC4mC2DGCHVQDf+PuHEu4LxA+g=";
                            } { });
                        pqi-ffi = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "pqi-ffi";
                                ver = "1.0.1.0";
                                sha256 = "sha256-wZfnWVJwMNG40YS6MAarGIDZ9Nudqup+jncERD1fhmU=";
                            } { });
                        pqi-native = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "pqi-native";
                                ver = "1.0.1.11";
                                sha256 = "sha256-9lvHBXlIzADmgxHDyNsU7l+oKNzUlmQgAHjZTh27LLo=";
                            } { });
                        pqi-conformance = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "pqi-conformance";
                                ver = "1.0.11.0";
                                sha256 = "sha256-C14NRT6arn6U4T7KzS/vkjkzeA21gABfZo3MrN35Y5g=";
                            } { });
                        postgresql-binary = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "postgresql-binary";
                                ver = "0.15.0.1";
                                sha256 = "sha256-q5t2OgiDxyt8WU+zHVxpyVhFF9PtDu2BlQRfuPpBkgk=";
                            } { });
                        hasql = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "hasql";
                                ver = "2.0.1.0";
                                sha256 = "sha256-iA9yNnh+lfRjs4oWrnf1YN7oOrMwj+iANHALxBMq55U=";
                            } { });
                        hasql-pool = pkgs.haskell.lib.dontCheck
                            (final.callHackageDirect {
                                pkg = "hasql-pool";
                                ver = "1.5.0.1";
                                sha256 = "sha256-79Jj0htGBHdPXufyhuOKsL2H0qq2QwpC4fcVfvVLHCQ=";
                            } { });
                        hasql-transaction =
                            pkgs.haskell.lib.dontCheck
                                (final.callHackageDirect {
                                    pkg = "hasql-transaction";
                                    ver = "1.2.3.1";
                                    sha256 = "sha256-EteEnSgJB4MXixv/58D2Qo70L/AfZxNGin/pYiIjVhY=";
                                } { });
                        vty-unix = pkgs.haskell.lib.appendPatch
                            previous.vty-unix
                            ./patches/vty-unix-all-motion.patch;
                        skylighting-core = final.callCabal2nix
                            "skylighting-core"
                            "${skylighting}/skylighting-core"
                            { };
                        agent-syntax =
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage ./packages/agent-syntax/package.nix { })
                                {
                                    src = agentSyntaxSource;
                                }).overrideAttrs
                                (old: {
                                    preCheck =
                                        (old.preCheck or "")
                                        + ''
                                            export AGENT_SYNTAX_DIR=${skylightingSyntaxDirectory}
                                        '';
                                });
                        agent-core =
                            pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-core/package.nix { }) {
                                src = agentCoreSource;
                            })
                            [
                                pkgs.git
                                pkgs.ripgrep
                            ];
                        agent-process = pkgs.haskell.lib.overrideSrc
                            (final.callPackage ./packages/agent-process/package.nix { })
                            {
                                src = agentProcessSource;
                            };
                        agent-responses-types = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-responses-types/package.nix { }) {
                            src = agentResponsesTypesSource;
                        };
                        agent-codex-dialect = pkgs.haskell.lib.overrideSrc
                            (final.callPackage ./packages/agent-codex-dialect/package.nix { })
                            {
                                src = agentCodexDialectSource;
                            };
                        agent-grok-build-dialect =
                            pkgs.haskell.lib.overrideSrc
                                (final.callPackage ./packages/agent-grok-build-dialect/package.nix { })
                                {
                                    src = agentGrokBuildDialectSource;
                                };
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
                        claude-agent-sdk-haskell = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/claude-agent-sdk-haskell/package.nix { }) {
                            src = claudeAgentSdkHaskellSource;
                        };
                        agent-claude = pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-claude/package.nix { }) {
                            src = agentClaudeSource;
                        };
                        agent-tui =
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage ./packages/agent-tui/package.nix { })
                                {
                                    src = agentTuiSource;
                                }).overrideAttrs
                                (old: {
                                    preCheck =
                                        (old.preCheck or "")
                                        + ''
                                            export AGENT_SYNTAX_DIR=${skylightingSyntaxDirectory}
                                        '';
                                });
                        agent-store = pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage ./packages/agent-store/package.nix { })
                                {
                                    src = agentStoreSource;
                                })
                            [ pkgs.postgresql_18 ];
                        agent-cli = pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-cli/package.nix { }) {
                                src = agentCliSource;
                            })
                            [
                                pkgs.git
                                pkgs.postgresql_18
                            ];
                        agent-telegram = pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-telegram/package.nix { }) {
                                src = agentTelegramSource;
                            })
                            [ pkgs.postgresql_18 ];
                    }
                );

                agentCorePackage = haskellPackages.agent-core;
                agentProcessPackage = haskellPackages.agent-process;
                agentCodexDialectPackage = haskellPackages.agent-codex-dialect;
                agentGrokBuildDialectPackage = haskellPackages.agent-grok-build-dialect;
                agentSyntaxPackage = haskellPackages.agent-syntax;
                agentResponsesTypesPackage = haskellPackages.agent-responses-types;
                agentResponsesPackage = haskellPackages.agent-responses;
                agentOpenaiPackage = haskellPackages.agent-openai;
                agentXaiPackage = haskellPackages.agent-xai;
                agentOpenrouterPackage = haskellPackages.agent-openrouter;
                claudeAgentSdkHaskellPackage = haskellPackages.claude-agent-sdk-haskell;
                agentClaudePackage = haskellPackages.agent-claude;
                agentTuiPackage = haskellPackages.agent-tui;
                agentStorePackage = haskellPackages.agent-store;
                agentCliPackage = haskellPackages.agent-cli;
                agentTelegramPackage = haskellPackages.agent-telegram;
                agentCliExecutable =
                    (pkgs.haskell.lib.justStaticExecutables agentCliPackage).overrideAttrs
                        (old: {
                            nativeBuildInputs =
                                (old.nativeBuildInputs or [ ])
                                ++ [ pkgs.makeWrapper ];
                            disallowedRequisites = pkgs.lib.remove
                                haskellPackages.ghc
                                (old.disallowedRequisites or [ ]);
                            postInstall =
                                (old.postInstall or "")
                                + ''
                                    wrapProgram "$out/bin/agent-cli" \
                                        --set-default AGENT_SYNTAX_DIR \
                                            "${skylightingSyntaxDirectory}" \
                                        --prefix PATH : \
                                            "${pkgs.lib.makeBinPath [
                                                pkgs.ffmpeg
                                                pkgs.postgresql_18
                                                haskellPackages.ghc
                                            ]}"
                                '';
                        });
                agentTelegramExecutable =
                    (pkgs.haskell.lib.justStaticExecutables agentTelegramPackage).overrideAttrs
                        (old: {
                            nativeBuildInputs =
                                (old.nativeBuildInputs or [ ])
                                ++ [ pkgs.makeWrapper ];
                            disallowedRequisites = pkgs.lib.remove
                                haskellPackages.ghc
                                (old.disallowedRequisites or [ ]);
                            postInstall =
                                (old.postInstall or "")
                                + ''
                                    wrapProgram "$out/bin/agent-telegram" \
                                        --set-default AGENT_SYNTAX_DIR \
                                            "${skylightingSyntaxDirectory}" \
                                        --set-default HASKELL_AGENT_EXECUTABLE \
                                            "${agentCliExecutable}/bin/agent-cli" \
                                        --prefix PATH : \
                                            "${pkgs.lib.makeBinPath [
                                                pkgs.ffmpeg
                                                pkgs.postgresql_18
                                                haskellPackages.ghc
                                            ]}"
                                '';
                        });
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
                      export GHCRTS="-M8G"
                    fi
                    cabal="${haskellPackages.cabal-install}/bin/cabal"
                    expect_bin="${pkgs.expect}/bin/expect"
                    stty_bin="${pkgs.coreutils}/bin/stty"
                    export AGENT_REPL_SCRIPT="$script"
                    export AGENT_REPL_CABAL="$cabal"
                    export AGENT_REPL_STTY="$stty_bin"
                    exec "$expect_bin" -c '
                      set timeout -1
                      set cabal $env(AGENT_REPL_CABAL)
                      set script $env(AGENT_REPL_SCRIPT)
                      set external_stty $env(AGENT_REPL_STTY)
                      spawn -noecho $cabal repl lib:agent-cli --repl-options=-ghci-script=$script
                      # Expect gives Cabal/GHCi its own PTY. Keep that PTY in
                      # sync so Vty receives resize events with current bounds.
                      proc sync_spawn_size {} {
                        global external_stty spawn_out
                        if {![info exists spawn_out(slave,name)]} {
                          return
                        }
                        if {[catch {
                          exec $external_stty --file=/dev/tty size
                        } size]} {
                          return
                        }
                        if {[scan $size "%d %d" rows columns] != 2
                            || $rows <= 0
                            || $columns <= 0} {
                          return
                        }
                        catch {
                          exec $external_stty --file=$spawn_out(slave,name) rows $rows columns $columns
                        }
                      }
                      trap sync_spawn_size SIGWINCH
                      sync_spawn_size
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
                packages.agent-telegram = agentTelegramExecutable;
                packages.agent-core = agentCorePackage;
                packages.agent-process = agentProcessPackage;
                packages.agent-codex-dialect = agentCodexDialectPackage;
                packages.agent-grok-build-dialect = agentGrokBuildDialectPackage;
                packages.agent-syntax = agentSyntaxPackage;
                packages.agent-tui = agentTuiPackage;
                packages.agent-store = agentStorePackage;
                packages.skylighting-syntaxes = skylightingSyntaxes;
                packages.agent-responses-types = agentResponsesTypesPackage;
                packages.agent-responses = agentResponsesPackage;
                packages.agent-openai = agentOpenaiPackage;
                packages.agent-xai = agentXaiPackage;
                packages.agent-openrouter = agentOpenrouterPackage;
                packages.claude-agent-sdk-haskell = claudeAgentSdkHaskellPackage;
                packages.agent-claude = agentClaudePackage;
                packages.agent-openai-login = agentOpenaiExecutables;

                apps.default = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-cli;
                    exePath = "/bin/agent-cli";
                };
                apps.agent-telegram = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-telegram;
                    exePath = "/bin/agent-telegram";
                };
                apps.agent-openai-login = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-openai-login;
                    exePath = "/bin/agent-openai-login";
                };

                devShells.default = haskellPackages.shellFor {
                    packages = packages: [
                        packages.agent-cli
                        packages.agent-telegram
                        packages.agent-core
                        packages.agent-process
                        packages.agent-codex-dialect
                        packages.agent-grok-build-dialect
                        packages.agent-syntax
                        packages.agent-tui
                        packages.agent-responses-types
                        packages.agent-store
                        packages.agent-responses
                        packages.agent-openai
                        packages.agent-xai
                        packages.agent-openrouter
                        packages.claude-agent-sdk-haskell
                        packages.agent-claude
                    ];
                    withHoogle = false;
                    doBenchmark = true;
                    extraDependencies = packages: {
                        benchmarkHaskellDepends = [ packages.text-builder ];
                    };
                    shellHook = ''
                        export AGENT_SYNTAX_DIR=${skylightingSyntaxDirectory}
                        export AGENT_POSTGRES_BIN=${pkgs.postgresql_18}/bin
                    '';
                    nativeBuildInputs =
                        (with haskellPackages; [
                            cabal-install
                            ghcid
                        ])
                        ++ (with pkgs; [
                            cabal2nix
                            ffmpeg
                            postgresql_18
                            ripgrep
                        ])
                        ++ [ agentRepl ];
                };

                checks = {
                    agent-cli = agentCliPackage;
                    agent-telegram = agentTelegramPackage;
                    agent-core = agentCorePackage;
                    agent-process = agentProcessPackage;
                    agent-codex-dialect = agentCodexDialectPackage;
                    agent-grok-build-dialect = agentGrokBuildDialectPackage;
                    agent-syntax = agentSyntaxPackage;
                    agent-tui = agentTuiPackage;
                    agent-responses-types = agentResponsesTypesPackage;
                    agent-store = agentStorePackage;
                    agent-responses = agentResponsesPackage;
                    agent-openai = agentOpenaiPackage;
                    agent-xai = agentXaiPackage;
                    agent-openrouter = agentOpenrouterPackage;
                    claude-agent-sdk-haskell = claudeAgentSdkHaskellPackage;
                    agent-claude = agentClaudePackage;
                } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
                    nixos-module = import ./nix/tests/telegram-module.nix {
                        inherit self nixpkgs pkgs system;
                    };
                };

                formatter = pkgs.nixfmt-rfc-style;
            }
        )
        // {
            nixosModules.telegram = import ./nix/modules/telegram.nix {
                inherit self;
            };
            nixosModules.default = self.nixosModules.telegram;
        };
}
