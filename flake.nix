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
                            pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-core/package.nix { }) {
                                src = agentCoreSource;
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
                        agent-cli = pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-cli/package.nix { }) {
                                src = agentCliSource;
                            })
                            [ pkgs.git ];
                    }
                );

                agentCorePackage = haskellPackages.agent-core;
                agentCodexDialectPackage = haskellPackages.agent-codex-dialect;
                agentGrokBuildDialectPackage = haskellPackages.agent-grok-build-dialect;
                agentSyntaxPackage = haskellPackages.agent-syntax;
                agentResponsesPackage = haskellPackages.agent-responses;
                agentOpenaiPackage = haskellPackages.agent-openai;
                agentXaiPackage = haskellPackages.agent-xai;
                agentOpenrouterPackage = haskellPackages.agent-openrouter;
                agentTuiPackage = haskellPackages.agent-tui;
                agentCliPackage = haskellPackages.agent-cli;
                agentCliExecutable =
                    (pkgs.haskell.lib.justStaticExecutables agentCliPackage).overrideAttrs
                        (old: {
                            nativeBuildInputs =
                                (old.nativeBuildInputs or [ ])
                                ++ [ pkgs.makeWrapper ];
                            postInstall =
                                (old.postInstall or "")
                                + ''
                                    wrapProgram "$out/bin/agent-cli" \
                                        --set-default AGENT_SYNTAX_DIR \
                                            "${skylightingSyntaxDirectory}"
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
                packages.agent-core = agentCorePackage;
                packages.agent-codex-dialect = agentCodexDialectPackage;
                packages.agent-grok-build-dialect = agentGrokBuildDialectPackage;
                packages.agent-syntax = agentSyntaxPackage;
                packages.agent-tui = agentTuiPackage;
                packages.skylighting-syntaxes = skylightingSyntaxes;
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
                        packages.agent-codex-dialect
                        packages.agent-grok-build-dialect
                        packages.agent-syntax
                        packages.agent-tui
                        packages.agent-responses
                        packages.agent-openai
                        packages.agent-xai
                        packages.agent-openrouter
                    ];
                    withHoogle = false;
                    doBenchmark = true;
                    extraDependencies = packages: {
                        benchmarkHaskellDepends = [ packages.text-builder ];
                    };
                    shellHook = ''
                        export AGENT_SYNTAX_DIR=${skylightingSyntaxDirectory}
                    '';
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
                    agent-codex-dialect = agentCodexDialectPackage;
                    agent-grok-build-dialect = agentGrokBuildDialectPackage;
                    agent-syntax = agentSyntaxPackage;
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
