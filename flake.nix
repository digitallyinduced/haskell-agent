{
    description = "Universal agent harness";

    nixConfig = {
        extra-substituters = [
            "https://cache.digitallyinduced.com/public"
        ];
        extra-trusted-public-keys = [
            "public:kR6JCoqAIMaO4s+EdDGh+jsHEHnoLq4ZLJPMCo0hcIQ="
        ];
    };

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
                agentBuildCommit =
                    if self ? shortRev then self.shortRev
                    else if self ? dirtyShortRev then self.dirtyShortRev
                    else "development";
                agentBuildTimestamp = self.lastModifiedDate or "";
                # Use the source revision date rather than wall-clock build
                # time so identical revisions produce identical binaries.
                agentBuildDate =
                    if builtins.stringLength agentBuildTimestamp >= 8 then
                        builtins.substring 0 4 agentBuildTimestamp
                        + "-"
                        + builtins.substring 4 2 agentBuildTimestamp
                        + "-"
                        + builtins.substring 6 2 agentBuildTimestamp
                    else "unknown";
                bun_1_4 = pkgs.bun.overrideAttrs (_old: {
                    version = "1.4.0";
                    src = pkgs.fetchurl {
                        url = {
                            aarch64-darwin = "https://github.com/oven-sh/bun/releases/download/bun-v1.4.0/bun-darwin-aarch64.zip";
                            x86_64-darwin = "https://github.com/oven-sh/bun/releases/download/bun-v1.4.0/bun-darwin-x64.zip";
                            aarch64-linux = "https://github.com/oven-sh/bun/releases/download/bun-v1.4.0/bun-linux-aarch64.zip";
                            x86_64-linux = "https://github.com/oven-sh/bun/releases/download/bun-v1.4.0/bun-linux-x64-baseline.zip";
                        }.${system};
                        hash = {
                            aarch64-darwin = "sha256-xmnpf2Fk4cluBwF0jbmN+ndJKQjL2DlMdVcTSnNd44E=";
                            x86_64-darwin = "sha256-HQIRuPHcmRGCNEaHrRXnLuhvFUhFpff6R3mUzTQd2bA=";
                            aarch64-linux = "sha256-SxozLuhhmD65O8/m93D/+U4+MbLDiL2uo8jtNeWO7Q4=";
                            x86_64-linux = "sha256-GE+0WV8NQBohfPfHjBvEMLqDMU2reouUgFurv3+nCX8=";
                        }.${system};
                    };
                    sourceRoot = {
                        aarch64-darwin = "bun-darwin-aarch64";
                        x86_64-darwin = "bun-darwin-x64";
                    }.${system} or null;
                });

                # Codex upstream model catalog and fallback instructions.
                # Fetched at build time (pinned by content hash) instead of
                # vendored, mirroring how other third-party dependencies enter
                # the closure. The runtime additionally refreshes the catalog
                # from the ChatGPT /models endpoint when credentials permit.
                codexUpstreamRev = "4f39251a010a8bd7d692d25fb33832ff06f1635a";
                codexModelsJson = pkgs.fetchurl {
                    url = "https://raw.githubusercontent.com/openai/codex/${codexUpstreamRev}/codex-rs/models-manager/models.json";
                    hash = "sha256-6w17ml3K8QOJXF+KFMFrJp30bgObN1pVupf2I4VC0u0=";
                };
                codexPromptMd = pkgs.fetchurl {
                    url = "https://raw.githubusercontent.com/openai/codex/${codexUpstreamRev}/codex-rs/models-manager/prompt.md";
                    hash = "sha256-rIrhB6DXL+NHa0MK+xYepOZ9ouRG13iu/ESCgWBVmAc=";
                };

                agentOpenaiSource = nix-filter.lib {
                    root = ./packages/agent-openai;
                    include = [
                        "app"
                        "benchmark"
                        "src"
                        "test"
                        "agent-openai.cabal"
                        "CHANGELOG.md"
                        "LICENSE"
                        "README.md"
                        "UPSTREAM.md"
                    ];
                };

                agentServerClientSource = nix-filter.lib {
                    root = ./packages/agent-server-client;
                    include = [
                        "src"
                        "test"
                        "agent-server-client.cabal"
                        "LICENSE"
                    ];
                };

                agentGeminiSource = nix-filter.lib {
                    root = ./packages/agent-gemini;
                    include = [
                        "src"
                        "test"
                        "agent-gemini.cabal"
                        "README.md"
                        "LICENSE"
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

                agentConnectivitySource = nix-filter.lib {
                    root = ./packages/agent-connectivity;
                    include = [
                        "cbits"
                        "src"
                        "test"
                        "agent-connectivity.cabal"
                        "LICENSE"
                    ];
                };

                agentRuntimeDaemonSource = nix-filter.lib {
                    root = ./packages/agent-runtime-daemon;
                    include = [
                        "app"
                        "src"
                        "test"
                        "agent-runtime-daemon.cabal"
                        "LICENSE"
                    ];
                };

                agentJsonSource = nix-filter.lib {
                    root = ./packages/agent-json;
                    include = [
                        "src"
                        "test"
                        "agent-json.cabal"
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
                        "benchmark"
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
                        "data"
                        "src"
                        "test"
                        "agent-core.cabal"
                        "LICENSE"
                        "README.md"
                    ];
                };

                agentMcpSource = nix-filter.lib {
                    root = ./packages/agent-mcp;
                    include = [
                        "src"
                        "test"
                        "benchmark"
                        "agent-mcp.cabal"
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

                # Production derivations intentionally exclude tests. The
                # checking package set selects the complete sources below.
                agentCliProductionSource = nix-filter.lib {
                    root = ./packages/agent-cli;
                    include = [
                        "app"
                        "data"
                        "eval"
                        "skills"
                        "src"
                        "agent-cli.cabal"
                        "LICENSE"
                    ];
                };

                agentCliCheckSource = nix-filter.lib {
                    root = ./packages/agent-cli;
                    include = [
                        "app"
                        "data"
                        "eval"
                        "skills"
                        "src"
                        "test"
                        "agent-cli.cabal"
                        "LICENSE"
                    ];
                };

                agentExternalSessionProductionSource = nix-filter.lib {
                    root = ./packages/agent-external-session;
                    include = [
                        "src"
                        "agent-external-session.cabal"
                        "LICENSE"
                    ];
                };

                agentExternalSessionCheckSource = nix-filter.lib {
                    root = ./packages/agent-external-session;
                    include = [
                        "src"
                        "test"
                        "agent-external-session.cabal"
                        "LICENSE"
                    ];
                };

                agentRepositoryProductionSource = nix-filter.lib {
                    root = ./packages/agent-repository;
                    include = [
                        "src"
                        "agent-repository.cabal"
                        "LICENSE"
                    ];
                };

                agentRepositoryCheckSource = nix-filter.lib {
                    root = ./packages/agent-repository;
                    include = [
                        "src"
                        "test"
                        "agent-repository.cabal"
                        "LICENSE"
                    ];
                };

                agentNativeBridgeProductionSource = nix-filter.lib {
                    root = ./packages/agent-native-bridge;
                    include = [
                        "cbits"
                        "ffi"
                        "include"
                        "src"
                        "agent-native-bridge.cabal"
                        "LICENSE"
                    ];
                };

                agentNativeBridgeCheckSource = nix-filter.lib {
                    root = ./packages/agent-native-bridge;
                    include = [
                        "cbits"
                        "ffi"
                        "include"
                        "src"
                        "test"
                        "agent-native-bridge.cabal"
                        "LICENSE"
                    ];
                };

                agentCliRuntimeProductionSource = nix-filter.lib {
                    root = ./packages/agent-cli-runtime;
                    include = [
                        "config"
                        "src"
                        "agent-cli-runtime.cabal"
                        "LICENSE"
                    ];
                };

                agentCliRuntimeCheckSource = nix-filter.lib {
                    root = ./packages/agent-cli-runtime;
                    include = [
                        "config"
                        "src"
                        "test"
                        "agent-cli-runtime.cabal"
                        "LICENSE"
                    ];
                };

                agentTelegramProductionSource = nix-filter.lib {
                    root = ./packages/agent-telegram;
                    include = [
                        "app"
                        "src"
                        "agent-telegram.cabal"
                        "LICENSE"
                    ];
                };

                agentTelegramCheckSource = nix-filter.lib {
                    root = ./packages/agent-telegram;
                    include = [
                        "app"
                        "src"
                        "test"
                        "agent-telegram.cabal"
                        "LICENSE"
                    ];
                };

                agentServerProductionSource = nix-filter.lib {
                    root = ./packages/agent-server;
                    include = [
                        "app"
                        "openapi.json"
                        "src"
                        "agent-server.cabal"
                        "LICENSE"
                    ];
                };

                agentServerCheckSource = nix-filter.lib {
                    root = ./packages/agent-server;
                    include = [
                        "app"
                        "openapi.json"
                        "src"
                        "test"
                        "agent-server.cabal"
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

                mkHaskellPackages = baseHaskellPackages: packageMode:
                    baseHaskellPackages.extend (
                    final: previous:
                    let
                        # Checks exercise unoptimised static libraries.
                        # Optimisation, profiling/shared copies, and Haddock
                        # output add compile work without adding test coverage.
                        # User-facing builds remain optimised and only need the
                        # statically linked executables. Development packages
                        # retain the upstream defaults.
                        localPackage = package:
                            if packageMode == "check"
                            then
                                pkgs.haskell.lib.dontHaddock
                                    (pkgs.haskell.lib.disableSharedLibraries
                                        (pkgs.haskell.lib.disableLibraryProfiling
                                            (pkgs.haskell.lib.disableOptimization
                                                package)))
                            else if packageMode == "production"
                            then
                                pkgs.haskell.lib.dontHaddock
                                    (pkgs.haskell.lib.dontCheck
                                        (pkgs.haskell.lib.disableSharedLibraries
                                            (pkgs.haskell.lib.disableLibraryProfiling package)))
                            else
                                package;
                    in {
                        hermes-json =
                            pkgs.haskell.lib.overrideSrc previous.hermes-json {
                                src = pkgs.fetchFromGitHub {
                                    owner = "velveteer";
                                    repo = "hermes";
                                    rev = "c04619a2b490fb49c67cacc0d2eb15368b78505f";
                                    hash = "sha256-BZEIcQrQTYE7Vf3FVq1EOaeH/dLnFvU+INZZNNQSMcw=";
                                    fetchSubmodules = true;
                                };
                            };
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
                        agent-syntax = localPackage (
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
                                }));
                        agent-core = localPackage (
                            pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-core/package.nix { }) {
                                src = agentCoreSource;
                            })
                            [
                                pkgs.git
                                bun_1_4
                                pkgs.ripgrep
                            ]);
                        agent-mcp = localPackage (pkgs.haskell.lib.overrideSrc
                            (final.callPackage ./packages/agent-mcp/package.nix { })
                            {
                                src = agentMcpSource;
                            });
                        agent-process = localPackage (pkgs.haskell.lib.overrideSrc
                            (final.callPackage ./packages/agent-process/package.nix { })
                            {
                                src = agentProcessSource;
                            });
                        agent-connectivity = localPackage
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage
                                    ./packages/agent-connectivity/package.nix
                                    { })
                                {
                                    src = agentConnectivitySource;
                                });
                        agent-runtime-daemon = localPackage
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage
                                    ./packages/agent-runtime-daemon/package.nix
                                    { })
                                {
                                    src = agentRuntimeDaemonSource;
                                });
                        agent-json = localPackage (pkgs.haskell.lib.overrideSrc
                            (final.callPackage ./packages/agent-json/package.nix { })
                            {
                                src = agentJsonSource;
                            });
                        agent-responses-types = localPackage (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-responses-types/package.nix { }) {
                            src = agentResponsesTypesSource;
                        });
                        agent-codex-dialect = localPackage (pkgs.haskell.lib.overrideSrc
                            (final.callPackage ./packages/agent-codex-dialect/package.nix { })
                            {
                                src = agentCodexDialectSource;
                            });
                        agent-grok-build-dialect = localPackage (
                            pkgs.haskell.lib.overrideSrc
                                (final.callPackage ./packages/agent-grok-build-dialect/package.nix { })
                                {
                                    src = agentGrokBuildDialectSource;
                                });
                        agent-responses = localPackage (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-responses/package.nix { }) {
                            src = agentResponsesSource;
                        });
                        agent-openai = localPackage (pkgs.haskell.lib.compose.overrideCabal
                            (old: {
                                prePatch = (old.prePatch or "") + ''
                                    mkdir -p data
                                    cp ${codexModelsJson} data/models.json
                                    cp ${codexPromptMd} data/prompt.md
                                '';
                            })
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-openai/package.nix { }) {
                                src = agentOpenaiSource;
                            }));
                        agent-xai = localPackage (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-xai/package.nix { }) {
                            src = agentXaiSource;
                        });
                        agent-openrouter = localPackage (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-openrouter/package.nix { }) {
                            src = agentOpenrouterSource;
                        });
                        agent-gemini = localPackage (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-gemini/package.nix { }) {
                            src = agentGeminiSource;
                        });
                        claude-agent-sdk-haskell = localPackage
                            (pkgs.haskell.lib.addTestToolDepends
                                (pkgs.haskell.lib.overrideSrc
                                    (final.callPackage
                                        ./packages/claude-agent-sdk-haskell/package.nix
                                        { })
                                    {
                                        src = claudeAgentSdkHaskellSource;
                                    })
                                [ pkgs.util-linux ]);
                        agent-claude = localPackage (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-claude/package.nix { }) {
                            src = agentClaudeSource;
                        });
                        agent-tui = localPackage (
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
                                }));
                        agent-store = localPackage (pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage ./packages/agent-store/package.nix { })
                                {
                                    src = agentStoreSource;
                                })
                            [ pkgs.postgresql_18 ]);
                        agent-server-client = localPackage
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage
                                    ./packages/agent-server-client/package.nix
                                    { })
                                {
                                    src = agentServerClientSource;
                                });
                        agent-cli-runtime = localPackage
                            (pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage
                                    ./packages/agent-cli-runtime/package.nix
                                    { })
                                {
                                    src =
                                        if packageMode != "production"
                                            then agentCliRuntimeCheckSource
                                            else agentCliRuntimeProductionSource;
                                })
                            [ pkgs.postgresql_18 ]);
                        agent-external-session = localPackage
                            (pkgs.haskell.lib.addTestToolDepends
                                (pkgs.haskell.lib.overrideSrc
                                    (final.callPackage
                                        ./packages/agent-external-session/package.nix
                                        { })
                                    {
                                        src =
                                            if packageMode != "production"
                                                then agentExternalSessionCheckSource
                                                else agentExternalSessionProductionSource;
                                    })
                                [ pkgs.zstd ]);
                        agent-repository = localPackage
                            (pkgs.haskell.lib.addTestToolDepends
                                (pkgs.haskell.lib.overrideSrc
                                    (final.callPackage
                                        ./packages/agent-repository/package.nix
                                        { })
                                    {
                                        src =
                                            if packageMode != "production"
                                                then agentRepositoryCheckSource
                                                else agentRepositoryProductionSource;
                                    })
                                [
                                    pkgs.bash
                                    pkgs.coreutils
                                    pkgs.git
                                    pkgs.python3
                                ]);
                        agent-cli = localPackage (pkgs.haskell.lib.addTestToolDepends
                            ((pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-cli/package.nix { }) {
                                src =
                                    if packageMode != "production"
                                        then agentCliCheckSource
                                        else agentCliProductionSource;
                            }).overrideAttrs (old: {
                                # Check packages use the source defaults for
                                # build identity. Embedding every merge SHA in
                                # them invalidates the test cache even when the
                                # filtered package sources are unchanged.
                                configureFlags =
                                    (old.configureFlags or [ ])
                                    ++ pkgs.lib.optionals
                                        (packageMode != "check")
                                        [
                                            "--ghc-option=-DAGENT_BUILD_COMMIT=\"${agentBuildCommit}\""
                                            "--ghc-option=-DAGENT_BUILD_DATE=\"${agentBuildDate}\""
                                        ];
                            } // pkgs.lib.optionalAttrs
                                (packageMode == "check"
                                    && pkgs.stdenv.hostPlatform.isLinux)
                                {
                                    # Each shard is a separate process, so
                                    # tests that temporarily modify
                                    # process-global state remain isolated
                                    # while the subprocess-heavy suite runs
                                    # concurrently.
                                    AGENT_CLI_TEST_SHARDS = "6";
                                }))
                            [
                                pkgs.bash
                                pkgs.coreutils
                                pkgs.git
                                bun_1_4
                                pkgs.postgresql_18
                                pkgs.python3
                                pkgs.zstd
                            ]);
                        agent-native-bridge = localPackage
                            ((pkgs.haskell.lib.overrideSrc
                                (final.callPackage
                                    ./packages/agent-native-bridge/package.nix
                                    { })
                                {
                                    src =
                                        if packageMode != "production"
                                            then agentNativeBridgeCheckSource
                                            else agentNativeBridgeProductionSource;
                                }).overrideAttrs (old: {
                                    # GHC's Darwin native-shared output is
                                    # already linked for runtime loading.
                                    # Stripping it in the package can turn it
                                    # into an object file, so exclude only the
                                    # bridge dylib.
                                    stripExclude =
                                        (old.stripExclude or [ ])
                                        ++ pkgs.lib.optionals
                                            pkgs.stdenv.hostPlatform.isDarwin
                                            [ "lib/libhaskell-agent-bridge.dylib" ];
                                }));
                        agent-telegram = localPackage (pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-telegram/package.nix { }) {
                                src =
                                    if packageMode != "production"
                                        then agentTelegramCheckSource
                                        else agentTelegramProductionSource;
                            })
                            [ pkgs.postgresql_18 ]);
                        agent-server = localPackage
                            (pkgs.haskell.lib.overrideSrc
                                (final.callPackage
                                    ./packages/agent-server/package.nix
                                    { })
                                {
                                    src =
                                        if packageMode != "production"
                                            then agentServerCheckSource
                                            else agentServerProductionSource;
                                });
                    }
                );

                haskellPackages =
                    mkHaskellPackages pkgs.haskellPackages "check";
                developmentHaskellPackages =
                    mkHaskellPackages pkgs.haskellPackages "development";
                productionHaskellPackages =
                    mkHaskellPackages pkgs.haskellPackages "production";
                staticHaskellPackages =
                    if pkgs.stdenv.hostPlatform.isLinux then
                        mkHaskellPackages
                            pkgs.pkgsStatic.haskellPackages
                            "production"
                    else
                        null;
                agentCorePackage = productionHaskellPackages.agent-core;
                agentMcpPackage = productionHaskellPackages.agent-mcp;
                agentJsonPackage = productionHaskellPackages.agent-json;
                agentProcessPackage = productionHaskellPackages.agent-process;
                agentConnectivityPackage =
                    productionHaskellPackages.agent-connectivity;
                agentRuntimeDaemonPackage =
                    productionHaskellPackages.agent-runtime-daemon;
                agentCodexDialectPackage = productionHaskellPackages.agent-codex-dialect;
                agentGrokBuildDialectPackage = productionHaskellPackages.agent-grok-build-dialect;
                agentSyntaxPackage = productionHaskellPackages.agent-syntax;
                agentResponsesTypesPackage = productionHaskellPackages.agent-responses-types;
                agentResponsesPackage = productionHaskellPackages.agent-responses;
                agentOpenaiPackage = productionHaskellPackages.agent-openai;
                agentXaiPackage = productionHaskellPackages.agent-xai;
                agentOpenrouterPackage = productionHaskellPackages.agent-openrouter;
                agentGeminiPackage = productionHaskellPackages.agent-gemini;
                claudeAgentSdkHaskellPackage = productionHaskellPackages.claude-agent-sdk-haskell;
                agentClaudePackage = productionHaskellPackages.agent-claude;
                agentTuiPackage = productionHaskellPackages.agent-tui;
                agentStorePackage = productionHaskellPackages.agent-store;
                agentCliRuntimePackage =
                    productionHaskellPackages.agent-cli-runtime;
                agentExternalSessionPackage =
                    productionHaskellPackages.agent-external-session;
                agentRepositoryPackage =
                    productionHaskellPackages.agent-repository;
                agentCliPackage = productionHaskellPackages.agent-cli;
                agentNativeBridgeHaskellPackage =
                    productionHaskellPackages.agent-native-bridge;
                agentTelegramPackage = productionHaskellPackages.agent-telegram;
                agentServerPackage = productionHaskellPackages.agent-server;
                agentServerClientPackage =
                    productionHaskellPackages.agent-server-client;
                # Exercise these packages' own test suites against the
                # production dependency graph. Referencing the all-check
                # package set here would also rerun every transitive local
                # package test suite, making focused checks fail for unrelated
                # dependencies already covered by the agent-cli root.
                agentTelegramCheckPackage = pkgs.haskell.lib.doCheck
                    (pkgs.haskell.lib.overrideSrc agentTelegramPackage {
                        src = agentTelegramCheckSource;
                    });
                agentNativeBridgeCheckPackage = pkgs.haskell.lib.doCheck
                    (pkgs.haskell.lib.overrideSrc
                        agentNativeBridgeHaskellPackage {
                            src = agentNativeBridgeCheckSource;
                        });
                agentServerCheckPackage = pkgs.haskell.lib.doCheck
                    (pkgs.haskell.lib.overrideSrc agentServerPackage {
                        src = agentServerCheckSource;
                    });
                # Both installable CLI variants expose the same advertised
                # runtime capabilities; only the harness linkage differs.
                agentCliGstreamerCorePlugins =
                    pkgs.lib.getLib pkgs.gst_all_1.gstreamer;
                agentCliGstreamerPlugins =
                    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
                        # Core elements supplies filesink, which terminates
                        # the Wayland portal screenshot pipeline.
                        agentCliGstreamerCorePlugins
                        pkgs.gst_all_1.gst-plugins-base
                        pkgs.gst_all_1.gst-plugins-good
                        pkgs.gst_all_1.gst-plugins-bad
                    ];
                agentCliLinuxComputerUseTools =
                    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
                        pkgs.gst_all_1.gstreamer
                        pkgs.maim
                        pkgs.xdotool
                        pkgs.xrandr
                    ];
                agentCliRuntimeTools = [
                    pkgs.ffmpeg
                    bun_1_4
                    pkgs.postgresql_18
                    pkgs.ripgrep
                    pkgs.zstd
                ] ++ agentCliLinuxComputerUseTools;
                prepareAgentCli = package:
                    package.overrideAttrs
                        (old: {
                            nativeBuildInputs =
                                (old.nativeBuildInputs or [ ])
                                ++ pkgs.lib.optionals
                                    (system == "aarch64-darwin")
                                    [ pkgs.removeReferencesTo ];
                            postInstall =
                                (old.postInstall or "")
                                + pkgs.lib.optionalString
                                    (system == "aarch64-darwin")
                                    ''
                                        # Darwin's linker leaves generated
                                        # Paths_* store prefixes in otherwise
                                        # dead constants. The packages' live
                                        # data files use separate -data
                                        # outputs, so remove only these package
                                        # output references before fixup strips
                                        # and signs the executable.
                                        remove-references-to \
                                            -t ${claudeAgentSdkHaskellPackage} \
                                            -t ${agentOpenaiPackage} \
                                            -t ${agentCorePackage} \
                                            "$out/bin/agent-cli"
                                    '';
                        } // pkgs.lib.optionalAttrs
                            pkgs.stdenv.hostPlatform.isDarwin {
                                # Darwin retains GHC as a requisite of the
                                # justStaticExecutables output. GHC is
                                # deliberately not included in either runtime
                                # package, so code mode still uses an
                                # independently installed compiler when one is
                                # available.
                                disallowedRequisites = pkgs.lib.remove
                                    haskellPackages.ghc
                                    (old.disallowedRequisites or [ ]);
                            });
                wrapAgentCli = package:
                    package.overrideAttrs
                        (old: {
                            nativeBuildInputs =
                                (old.nativeBuildInputs or [ ])
                                ++ [ pkgs.makeWrapper ];
                            postInstall =
                                (old.postInstall or "")
                                + ''
                                    computerUseWrapperArgs=()
                                ''
                                + pkgs.lib.optionalString
                                    pkgs.stdenv.hostPlatform.isLinux
                                    ''
                                        computerUseWrapperArgs+=(
                                            --prefix GST_PLUGIN_SYSTEM_PATH_1_0 :
                                            "${pkgs.lib.makeSearchPath
                                                "lib/gstreamer-1.0"
                                                agentCliGstreamerPlugins}"
                                        )
                                    ''
                                + ''
                                    wrapProgram "$out/bin/agent-cli" \
                                        --set-default AGENT_SYNTAX_DIR \
                                            "${skylightingSyntaxDirectory}" \
                                        --set-default AGENT_POSTGRES_BIN \
                                            "${pkgs.postgresql_18}/bin" \
                                        --prefix PATH : \
                                            "${pkgs.lib.makeBinPath agentCliRuntimeTools}" \
                                        "''${computerUseWrapperArgs[@]}"
                                '';
                        });
                agentCliBareExecutable =
                    prepareAgentCli
                        (pkgs.haskell.lib.justStaticExecutables agentCliPackage);
                agentCliStaticExecutable =
                    if pkgs.stdenv.hostPlatform.isLinux then
                        wrapAgentCli
                            (pkgs.haskell.lib.justStaticExecutables
                                staticHaskellPackages.agent-cli)
                    else
                        agentCliExecutable;
                agentCliExecutable =
                    wrapAgentCli agentCliBareExecutable;
                agentCliMacosRelease =
                    if pkgs.stdenv.hostPlatform.isDarwin then
                        import ./nix/macos-bundle.nix {
                            inherit pkgs skylightingSyntaxes;
                            agentCli = agentCliBareExecutable;
                            agentCliSource = agentCliProductionSource;
                            agentCliRuntimeSource =
                                agentCliRuntimeProductionSource;
                            inherit agentCoreSource;
                            bun = bun_1_4;
                            sourceDateEpoch = self.lastModified or 1;
                        }
                    else
                        null;
                agentRuntimeDaemonExecutable =
                    pkgs.haskell.lib.justStaticExecutables
                        agentRuntimeDaemonPackage;
                agentCliStaticRuntimeCheck =
                    pkgs.runCommand "agent-cli-static-runtime"
                        { }
                        ''
                            home="$TMPDIR/home"
                            mkdir -p "$home"

                            run_agent() {
                                env -i \
                                    HOME="$home" \
                                    PATH="${pkgs.coreutils}/bin" \
                                    LC_ALL=C \
                                    "${agentCliStaticExecutable}/bin/agent-cli" "$@"
                            }

                            cleanup() {
                                run_agent storage stop || true
                            }
                            trap cleanup EXIT

                            wrapper="${agentCliStaticExecutable}/bin/agent-cli"
                            for dependency in \
                                ${pkgs.gst_all_1.gstreamer} \
                                ${pkgs.maim} \
                                ${pkgs.xdotool} \
                                ${pkgs.xrandr}
                            do
                                ${pkgs.gnugrep}/bin/grep -F \
                                    "$dependency/bin" "$wrapper"
                            done
                            ${pkgs.gnugrep}/bin/grep -F \
                                "GST_PLUGIN_SYSTEM_PATH_1_0" "$wrapper"
                            pluginPath="${pkgs.lib.makeSearchPath
                                "lib/gstreamer-1.0"
                                agentCliGstreamerPlugins}"
                            ${pkgs.gnugrep}/bin/grep -F \
                                "${
                                    agentCliGstreamerCorePlugins
                                }/lib/gstreamer-1.0" \
                                "$wrapper"
                            env -i \
                                HOME="$home" \
                                GST_PLUGIN_SYSTEM_PATH_1_0="$pluginPath" \
                                GST_REGISTRY_1_0="$TMPDIR/gstreamer-registry.bin" \
                                ${pkgs.gst_all_1.gstreamer}/bin/gst-inspect-1.0 \
                                filesink >/dev/null

                            run_agent storage start
                            test "$(
                                cat "$home/.haskell-agent/postgres/data/PG_VERSION"
                            )" = "18"
                            run_agent storage doctor
                            run_agent storage stop
                            trap - EXIT
                            touch "$out"
                        '';
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
                agentServerExecutable =
                    (pkgs.haskell.lib.justStaticExecutables
                        agentServerPackage).overrideAttrs
                        (old: {
                            nativeBuildInputs =
                                (old.nativeBuildInputs or [ ])
                                ++ [ pkgs.makeWrapper ];
                            postInstall =
                                (old.postInstall or "")
                                + ''
                                    wrapProgram "$out/bin/agent-server" \
                                        --set-default AGENT_SYNTAX_DIR \
                                            "${skylightingSyntaxDirectory}" \
                                        --set-default AGENT_POSTGRES_BIN \
                                            "${pkgs.postgresql_18}/bin" \
                                        --prefix PATH : \
                                            "${pkgs.lib.makeBinPath agentCliRuntimeTools}"
                                '';
                        } // pkgs.lib.optionalAttrs
                            pkgs.stdenv.hostPlatform.isDarwin {
                                disallowedRequisites = pkgs.lib.remove
                                    haskellPackages.ghc
                                    (old.disallowedRequisites or [ ]);
                            });
                agentSandboxVm =
                    if pkgs.stdenv.hostPlatform.isLinux then
                        (nixpkgs.lib.nixosSystem {
                            inherit system;
                            specialArgs = {
                                agentServer = agentServerExecutable;
                            };
                            modules = [ ./nix/sandbox-vm.nix ];
                        }).config.system.build.vm
                    else
                        null;
                agentSandboxRunner =
                    if pkgs.stdenv.hostPlatform.isLinux then
                        import ./nix/sandbox-runner.nix {
                            inherit pkgs;
                            vm = agentSandboxVm;
                        }
                    else
                        null;
                agentNativeBridgePackage = pkgs.runCommand
                    "haskell-agent-native-bridge-0.1.0"
                    {
                        # The bridge was already excluded from its Cabal
                        # package's strip pass; do not strip it after copying.
                        dontStrip = true;
                    }
                    ''
                        bridge="$(${pkgs.findutils}/bin/find \
                            ${agentNativeBridgeHaskellPackage}/lib \
                            -name libhaskell-agent-bridge.dylib \
                            -print -quit)"
                        header="$(${pkgs.findutils}/bin/find \
                            ${agentNativeBridgeHaskellPackage}/lib \
                            -name HaskellAgentBridge.h \
                            -print -quit)"
                        test -n "$bridge"
                        test -n "$header"
                        mkdir -p "$out/lib" "$out/include"
                        cp "$bridge" "$out/lib/libhaskell-agent-bridge.dylib"
                        cp "$header" "$out/include/HaskellAgentBridge.h"
                        bridgeType="$(${pkgs.file}/bin/file -b \
                            "$out/lib/libhaskell-agent-bridge.dylib")"
                        case "$bridgeType" in
                            Mach-O\ 64-bit*dynamically\ linked\ shared\ library*)
                                ;;
                            *)
                                echo "Expected a Mach-O shared library, got: $bridgeType" >&2
                                exit 1
                                ;;
                        esac
                    '';
                agentOpenaiExecutables = pkgs.haskell.lib.justStaticExecutables agentOpenaiPackage;
                functionalTestCredentialHome =
                    builtins.getEnv "AGENT_FUNCTIONAL_TEST_CREDENTIAL_HOME";
                # A live provider call cannot run in Nix's normal
                # network-isolated sandbox. CI opts into this Linux-only
                # check with impure evaluation, staged credentials, and
                # `sandbox = relaxed`.
                functionalTestEnabled =
                    functionalTestCredentialHome != ""
                    && pkgs.stdenv.hostPlatform.isLinux;
                functionalTestModel = provider: default:
                    let
                        configured = builtins.getEnv
                            "AGENT_FUNCTIONAL_TEST_${provider}_MODEL";
                    in
                    if configured == "" then default else configured;
                agentCliHelloWorldFunctional = provider: model:
                    pkgs.runCommand "agent-cli-functional-${provider}-hello-world"
                        {
                            __noChroot = true;
                            AGENT_FUNCTIONAL_TEST_CREDENTIAL_HOME =
                                functionalTestCredentialHome;
                            AGENT_FUNCTIONAL_TEST_PROVIDER = provider;
                            AGENT_FUNCTIONAL_TEST_MODEL = model;
                            LANG = "C.UTF-8";
                            LC_ALL = "C.UTF-8";
                            SSL_CERT_FILE =
                                "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
                            nativeBuildInputs = [
                                agentCliExecutable
                                haskellPackages.ghc
                                pkgs.coreutils
                                pkgs.tmux
                            ];
                        }
                        ''
                            ${pkgs.bash}/bin/bash \
                                ${./tests/functional/agent-cli-hello-world.sh} \
                                ${agentCliExecutable}/bin/agent-cli
                            touch "$out"
                        '';

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
                # Linux uses a statically linked musl harness, wrapped with the
                # same runtime tools as the native build. The native build
                # remains available as `agent-cli`.
                packages.default = agentCliStaticExecutable;
                packages.agent-cli-static = agentCliStaticExecutable;
                packages.agent-cli = agentCliExecutable;
                packages.agent-telegram = agentTelegramExecutable;
                packages.agent-server = agentServerExecutable;
                packages.agent-server-client = agentServerClientPackage;
                packages.${if pkgs.stdenv.hostPlatform.isLinux
                    then "agent-sandbox-runner" else null} = agentSandboxRunner;
                packages.${if pkgs.stdenv.hostPlatform.isDarwin
                    then "agent-native-bridge" else null} = agentNativeBridgePackage;
                packages.${if pkgs.stdenv.hostPlatform.isDarwin
                    then "agent-cli-macos-bundle" else null} =
                    agentCliMacosRelease.bundle;
                packages.${if pkgs.stdenv.hostPlatform.isDarwin
                    then "agent-cli-macos-archive" else null} =
                    agentCliMacosRelease.archive;
                packages.agent-cli-runtime = agentCliRuntimePackage;
                packages.agent-external-session =
                    agentExternalSessionPackage;
                packages.agent-repository = agentRepositoryPackage;
                packages.agent-native-bridge-library =
                    agentNativeBridgeHaskellPackage;
                packages.agent-core = agentCorePackage;
                packages.agent-mcp = agentMcpPackage;
                packages.agent-json = agentJsonPackage;
                packages.agent-process = agentProcessPackage;
                packages.agent-connectivity = agentConnectivityPackage;
                packages.agent-runtime-daemon = agentRuntimeDaemonExecutable;
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
                packages.agent-gemini = agentGeminiPackage;
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
                apps.agent-server = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-server;
                    exePath = "/bin/agent-server";
                };
                apps.${if pkgs.stdenv.hostPlatform.isLinux
                    then "agent-sandbox-runner" else null} =
                    flake-utils.lib.mkApp {
                        drv = self.packages.${system}.agent-sandbox-runner;
                        exePath = "/bin/agent-sandbox-runner";
                    };
                apps.agent-runtime-daemon = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-runtime-daemon;
                    exePath = "/bin/agent-runtime-daemon";
                };
                apps.agent-openai-login = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-openai-login;
                    exePath = "/bin/agent-openai-login";
                };

                devShells.default = developmentHaskellPackages.shellFor {
                    packages = packages: [
                        packages.agent-cli
                        packages.agent-cli-runtime
                        packages.agent-external-session
                        packages.agent-repository
                        packages.agent-native-bridge
                        packages.agent-telegram
                        packages.agent-server
                        packages.agent-core
                        packages.agent-mcp
                        packages.agent-json
                        packages.agent-process
                        packages.agent-connectivity
                        packages.agent-runtime-daemon
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
                        packages.agent-gemini
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
                        # Development builds embed the nix-fetched Codex catalog
                        # at compile time; provision it into the checkout.
                        if [ -d packages/agent-openai ]; then
                            mkdir -p packages/agent-openai/data
                            install -m 644 ${codexModelsJson} packages/agent-openai/data/models.json
                            install -m 644 ${codexPromptMd} packages/agent-openai/data/prompt.md
                        fi
                    '';
                    nativeBuildInputs =
                        (with haskellPackages; [
                            cabal-install
                            ghcid
                        ])
                        ++ (with pkgs; [
                            cabal2nix
                            ffmpeg
                            bun_1_4
                            postgresql_18
                            python3
                            ripgrep
                            zstd
                        ])
                        ++ agentCliLinuxComputerUseTools
                        ++ agentCliGstreamerPlugins
                        ++ [ agentRepl ];
                };

                checks = {
                    # The package check does not exercise the wrapped
                    # justStaticExecutables output or its requisite assertions.
                    agent-cli-executable = agentCliExecutable;
                    agent-cli-runtime = haskellPackages.agent-cli-runtime;
                    agent-external-session =
                        haskellPackages.agent-external-session;
                    agent-repository = haskellPackages.agent-repository;
                    agent-native-bridge = agentNativeBridgeCheckPackage;
                    agent-cli = haskellPackages.agent-cli;
                    package-boundaries = pkgs.runCommand
                        "agent-package-boundaries"
                        {
                            nativeBuildInputs = [
                                pkgs.bash
                                pkgs.ripgrep
                            ];
                        }
                        ''
                            bash ${./scripts/check-package-boundaries.sh} \
                                ${./.}
                            touch "$out"
                        '';
                    agent-telegram = agentTelegramCheckPackage;
                    agent-server = agentServerCheckPackage;
                    agent-server-client =
                        haskellPackages.agent-server-client;
                    agent-core = haskellPackages.agent-core;
                    agent-mcp = haskellPackages.agent-mcp;
                    agent-json = haskellPackages.agent-json;
                    agent-process = haskellPackages.agent-process;
                    agent-connectivity = haskellPackages.agent-connectivity;
                    agent-runtime-daemon =
                        haskellPackages.agent-runtime-daemon;
                    agent-codex-dialect = haskellPackages.agent-codex-dialect;
                    agent-grok-build-dialect = haskellPackages.agent-grok-build-dialect;
                    agent-syntax = haskellPackages.agent-syntax;
                    agent-tui = haskellPackages.agent-tui;
                    agent-responses-types = haskellPackages.agent-responses-types;
                    agent-store = haskellPackages.agent-store;
                    agent-responses = haskellPackages.agent-responses;
                    agent-openai = haskellPackages.agent-openai;
                    agent-xai = haskellPackages.agent-xai;
                    agent-openrouter = haskellPackages.agent-openrouter;
                    agent-gemini = haskellPackages.agent-gemini;
                    claude-agent-sdk-haskell =
                        haskellPackages.claude-agent-sdk-haskell;
                    agent-claude = haskellPackages.agent-claude;
                } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
                    agent-cli-static-runtime = agentCliStaticRuntimeCheck;
                    agent-sandbox-runner = agentSandboxRunner;
                    nixos-module = import ./nix/tests/telegram-module.nix {
                        inherit self nixpkgs pkgs system;
                    };
                } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
                    agent-cli-macos-bundle = agentCliMacosRelease.bundle;
                } // pkgs.lib.optionalAttrs functionalTestEnabled {
                    agent-cli-functional-openai-hello-world =
                        agentCliHelloWorldFunctional "openai"
                            (functionalTestModel "OPENAI" "gpt-5.6-terra");
                    # Temporarily disabled while the CI Grok account has no
                    # verified available usage. Keep package/unit checks enabled.
                    # agent-cli-functional-xai-hello-world =
                    #     agentCliHelloWorldFunctional "xai"
                    #         (functionalTestModel "XAI" "grok-4.6");
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
