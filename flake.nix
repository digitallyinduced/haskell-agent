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
                    mktemp_bin="${pkgs.coreutils}/bin/mktemp"
                    rm_bin="${pkgs.coreutils}/bin/rm"
                    sleep_bin="${pkgs.coreutils}/bin/sleep"
                    stty_bin="${pkgs.coreutils}/bin/stty"
                    # Keep this shell alive as a supervisor. Expect gives cabal
                    # its own PTY/process group; recording that PGID lets either
                    # layer tear down cabal, setup, and GHCi together.
                    state_dir="$("$mktemp_bin" -d "''${TMPDIR:-/tmp}/agent-repl.XXXXXX")"
                    child_pgid_file="$state_dir/cabal-pgid"
                    expect_script="$state_dir/launcher.expect"
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
                      if [ "$pgid" -le 1 ]; then
                        return 0
                      fi

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
                      local child_pgid=""
                      trap - EXIT HUP INT TERM
                      case "$expect_pid" in
                        ""|*[!0-9]*) ;;
                        *) "$kill_bin" -TERM "$expect_pid" 2>/dev/null || true ;;
                      esac
                      if [ -r "$child_pgid_file" ]; then
                        IFS= read -r child_pgid < "$child_pgid_file" || true
                        terminate_group "$child_pgid"
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
                      "$rm_bin" -rf "$state_dir"
                      return "$status"
                    }

                    trap cleanup EXIT
                    trap 'exit 129' HUP
                    trap 'exit 130' INT
                    trap 'exit 143' TERM

                    export AGENT_REPL_SCRIPT="$script"
                    export AGENT_REPL_CABAL="$cabal"
                    export AGENT_REPL_CHILD_PGID_FILE="$child_pgid_file"
                    export AGENT_REPL_KILL="$kill_bin"
                    cat > "$expect_script" <<'EXPECT'
                    set timeout -1
                    set cabal $env(AGENT_REPL_CABAL)
                    set script $env(AGENT_REPL_SCRIPT)
                    set child_pgid_file $env(AGENT_REPL_CHILD_PGID_FILE)
                    set kill_bin $env(AGENT_REPL_KILL)
                    set child_pid 0
                    set child_spawn_id ""
                    set child_status 1
                    set cleaned 0

                    proc group_alive {} {
                      global child_pid kill_bin
                      if {$child_pid <= 1} {
                        return 0
                      }
                      return [expr {![catch {
                        exec $kill_bin -0 -- -$child_pid
                      }]}]
                    }

                    proc signal_group {signal} {
                      global child_pid kill_bin
                      if {$child_pid > 1} {
                        catch {exec $kill_bin -$signal -- -$child_pid}
                      }
                    }

                    proc decode_wait_status {waited} {
                      if {[llength $waited] < 4 || [lindex $waited 2] != 0} {
                        return 1
                      }
                      if {
                        [llength $waited] >= 6
                        && [lindex $waited 4] eq "CHILDKILLED"
                      } {
                        switch -- [lindex $waited 5] {
                          SIGHUP  { return 129 }
                          SIGINT  { return 130 }
                          SIGKILL { return 137 }
                          SIGTERM { return 143 }
                          default { return 1 }
                        }
                      }
                      set status [lindex $waited 3]
                      if {$status < 0 || $status > 255} {
                        return 1
                      }
                      return $status
                    }

                    proc cleanup_child {} {
                      global cleaned child_pid child_spawn_id child_status
                      global child_pgid_file
                      if {$cleaned} {
                        return
                      }
                      set cleaned 1

                      signal_group TERM
                      for {set attempt 0} {
                        $attempt < 20 && [group_alive]
                      } {incr attempt} {
                        after 50
                      }
                      if {[group_alive]} {
                        signal_group KILL
                      }

                      if {$child_spawn_id ne ""} {
                        if {![catch {
                          wait -i $child_spawn_id
                        } waited]} {
                          set child_status [decode_wait_status $waited]
                        }
                      }
                      catch {file delete -force $child_pgid_file}
                    }

                    exit -onexit {cleanup_child}
                    trap {exit 129} SIGHUP
                    trap {exit 130} SIGINT
                    trap {exit 143} SIGTERM

                    set child_pid [spawn -noecho $cabal repl lib:agent-cli --repl-options=-ghci-script=$script]
                    set child_spawn_id $spawn_id
                    set pgid_handle [open $child_pgid_file w]
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
                    cleanup_child
                    exit $child_status
                    EXPECT
                    "$expect_bin" "$expect_script" <&0 &
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
