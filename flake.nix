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

                mkHaskellPackages = baseHaskellPackages: checkLocalPackages:
                    baseHaskellPackages.extend (
                    final: previous:
                    let
                        # User-facing builds only need the statically linked
                        # executables. Keep the complete builds for the dev
                        # shell and `nix flake check`.
                        localPackage = package:
                            if checkLocalPackages then package else
                                pkgs.haskell.lib.dontHaddock
                                    (pkgs.haskell.lib.dontCheck
                                        (pkgs.haskell.lib.disableSharedLibraries
                                            (pkgs.haskell.lib.disableLibraryProfiling package)));
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
                        agent-cli = localPackage (pkgs.haskell.lib.addTestToolDepends
                            ((pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-cli/package.nix { }) {
                                src = agentCliSource;
                            }).overrideAttrs (old: {
                                configureFlags = (old.configureFlags or [ ]) ++ [
                                    "--ghc-option=-DAGENT_BUILD_COMMIT=\"${agentBuildCommit}\""
                                    "--ghc-option=-DAGENT_BUILD_DATE=\"${agentBuildDate}\""
                                ];
                            }))
                            [
                                pkgs.git
                                bun_1_4
                                pkgs.postgresql_18
                            ]);
                        agent-telegram = localPackage (pkgs.haskell.lib.addTestToolDepends
                            (pkgs.haskell.lib.overrideSrc (final.callPackage ./packages/agent-telegram/package.nix { }) {
                                src = agentTelegramSource;
                            })
                            [ pkgs.postgresql_18 ]);
                    }
                );

                haskellPackages = mkHaskellPackages pkgs.haskellPackages true;
                productionHaskellPackages =
                    mkHaskellPackages pkgs.haskellPackages false;
                staticHaskellPackages =
                    if pkgs.stdenv.hostPlatform.isLinux then
                        mkHaskellPackages pkgs.pkgsStatic.haskellPackages false
                    else
                        null;
                agentCorePackage = productionHaskellPackages.agent-core;
                agentMcpPackage = productionHaskellPackages.agent-mcp;
                agentJsonPackage = productionHaskellPackages.agent-json;
                agentProcessPackage = productionHaskellPackages.agent-process;
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
                agentCliPackage = productionHaskellPackages.agent-cli;
                agentTelegramPackage = productionHaskellPackages.agent-telegram;
                agentCliStaticExecutable =
                    if pkgs.stdenv.hostPlatform.isLinux then
                        pkgs.haskell.lib.justStaticExecutables
                            staticHaskellPackages.agent-cli
                    else
                        agentCliExecutable;
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
                                                bun_1_4
                                                pkgs.postgresql_18
                                                pkgs.ripgrep
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
                # Linux users get fully static musl executables rather than
                # the tool-bundled package's multi-gigabyte runtime closure.
                # The native wrapped build remains available as `agent-cli`.
                packages.default = agentCliStaticExecutable;
                packages.agent-cli-static = agentCliStaticExecutable;
                packages.agent-cli = agentCliExecutable;
                packages.agent-telegram = agentTelegramExecutable;
                packages.agent-core = agentCorePackage;
                packages.agent-mcp = agentMcpPackage;
                packages.agent-json = agentJsonPackage;
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
                apps.agent-openai-login = flake-utils.lib.mkApp {
                    drv = self.packages.${system}.agent-openai-login;
                    exePath = "/bin/agent-openai-login";
                };

                devShells.default = haskellPackages.shellFor {
                    packages = packages: [
                        packages.agent-cli
                        packages.agent-telegram
                        packages.agent-core
                        packages.agent-mcp
                        packages.agent-json
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
                            ripgrep
                        ])
                        ++ [ agentRepl ];
                };

                checks = {
                    agent-cli = haskellPackages.agent-cli;
                    agent-telegram = haskellPackages.agent-telegram;
                    agent-core = haskellPackages.agent-core;
                    agent-mcp = haskellPackages.agent-mcp;
                    agent-json = haskellPackages.agent-json;
                    agent-process = haskellPackages.agent-process;
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
                    nixos-module = import ./nix/tests/telegram-module.nix {
                        inherit self nixpkgs pkgs system;
                    };
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
