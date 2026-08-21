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
                    final: _previous: {
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
                agentCliPackage = haskellPackages.agent-cli;
                agentCliExecutable = pkgs.haskell.lib.justStaticExecutables agentCliPackage;
                agentOpenaiExecutables = pkgs.haskell.lib.justStaticExecutables agentOpenaiPackage;

                # Opens cabal repl on the agent-cli library and enters the
                # GHCi :cmd loop that reloads + resumes after agent :reload.
                # expect waits for modules to load, then starts the agent
                # (ghci scripts run before cabal loads the package).
                agentReplExpect = pkgs.writeText "agent-repl.expect" ''
                    set timeout -1
                    set cabal $env(AGENT_REPL_CABAL)
                    set script $env(AGENT_REPL_SCRIPT)
                    set pgid_file $env(AGENT_REPL_PGID_FILE)

                    set child_pid [spawn -noecho $cabal repl lib:agent-cli --repl-options=-ghci-script=$script]
                    set pgid_handle [open $pgid_file w]
                    puts $pgid_handle $child_pid
                    close $pgid_handle
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
                '';

                agentRepl = pkgs.writeShellScriptBin "repl" ''
                    set -euo pipefail
                    root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
                    script="$root/scripts/agent-repl.ghci"
                    if [ ! -f "$script" ]; then
                      echo "repl: missing $script" >&2
                      exit 1
                    fi
                    # Cap the agent/GHCi heap so a runaway session OOMs itself
                    # instead of the whole machine. Override with GHCRTS=...
                    if [ -z "''${GHCRTS:-}" ]; then
                      export GHCRTS="-M8G -A64m"
                    fi
                    cabal="${haskellPackages.cabal-install}/bin/cabal"
                    expect_bin="${pkgs.expect}/bin/expect"
                    kill_bin="${pkgs.coreutils}/bin/kill"
                    sleep_bin="${pkgs.coreutils}/bin/sleep"
                    stty_bin="${pkgs.coreutils}/bin/stty"
                    rm_bin="${pkgs.coreutils}/bin/rm"
                    rmdir_bin="${pkgs.coreutils}/bin/rmdir"
                    state_dir="$(${pkgs.coreutils}/bin/mktemp -d "''${TMPDIR:-/tmp}/agent-repl.XXXXXX")"
                    pgid_file="$state_dir/cabal-pgid"
                    expect_pid=""
                    tty_state=""

                    if [ -t 0 ]; then
                      tty_state="$("$stty_bin" -g < /dev/tty 2>/dev/null || true)"
                    fi

                    terminate_group() {
                      local pgid="''${1:-}"
                      case "$pgid" in
                        ""|*[!0-9]*) return 0 ;;
                      esac
                      [ "$pgid" -gt 1 ] || return 0

                      "$kill_bin" -TERM -- "-$pgid" 2>/dev/null || true
                      for _ in {1..20}; do
                        if ! "$kill_bin" -0 -- "-$pgid" 2>/dev/null; then
                          return 0
                        fi
                        "$sleep_bin" 0.05
                      done
                      "$kill_bin" -KILL -- "-$pgid" 2>/dev/null || true
                    }

                    cleanup() {
                      local status=$?
                      local pgid=""
                      trap - EXIT HUP INT TERM
                      case "$expect_pid" in
                        ""|*[!0-9]*) ;;
                        *) "$kill_bin" -TERM "$expect_pid" 2>/dev/null || true ;;
                      esac
                      if [ -r "$pgid_file" ]; then
                        IFS= read -r pgid < "$pgid_file" || true
                        terminate_group "$pgid"
                      fi
                      case "$expect_pid" in
                        ""|*[!0-9]*) ;;
                        *)
                          "$kill_bin" -KILL "$expect_pid" 2>/dev/null || true
                          wait "$expect_pid" 2>/dev/null || true
                          ;;
                      esac
                      if [ -n "$tty_state" ]; then
                        "$stty_bin" "$tty_state" < /dev/tty 2>/dev/null || true
                      fi
                      "$rm_bin" -f "$pgid_file"
                      "$rmdir_bin" "$state_dir" 2>/dev/null || true
                      return "$status"
                    }

                    trap cleanup EXIT
                    trap 'exit 129' HUP
                    trap 'exit 130' INT
                    trap 'exit 143' TERM

                    export AGENT_REPL_SCRIPT="$script"
                    export AGENT_REPL_CABAL="$cabal"
                    export AGENT_REPL_PGID_FILE="$pgid_file"
                    "$expect_bin" "${agentReplExpect}" <&0 &
                    expect_pid=$!
                    if wait "$expect_pid"; then
                      status=0
                    else
                      status=$?
                    fi
                    expect_pid=""
                    exit "$status"
                '';
            in
            {
                packages.default = agentCliExecutable;
                packages.agent-cli = agentCliExecutable;
                packages.agent-core = agentCorePackage;
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
                    # Default RTS heap ceiling for GHCi / agent runs started from
                    # this shell (including `cabal repl` and `cabal run`). The
                    # `repl` wrapper sets the same default if GHCRTS is unset.
                    shellHook = ''
                      if [ -z "''${GHCRTS:-}" ]; then
                        export GHCRTS="-M8G -A64m"
                      fi
                    '';
                };

                checks = {
                    agent-cli = agentCliPackage;
                    agent-core = agentCorePackage;
                    agent-responses = agentResponsesPackage;
                    agent-openai = agentOpenaiPackage;
                    agent-xai = agentXaiPackage;
                    agent-openrouter = agentOpenrouterPackage;
                };

                formatter = pkgs.nixfmt-rfc-style;
            }
        );
}
