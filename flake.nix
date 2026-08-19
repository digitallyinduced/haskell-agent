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

                agentCliSource = nix-filter.lib {
                    root = ./packages/agent-cli;
                    include = [
                        "app"
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

                haskellPackages = pkgs.haskellPackages.extend (
                    final: _previous: {
                        agent-core = final.callCabal2nix "agent-core" agentCoreSource { };
                        agent-openai = final.callCabal2nix "agent-openai" agentOpenaiSource { };
                        agent-xai = final.callCabal2nix "agent-xai" agentXaiSource { };
                        agent-cli = final.callCabal2nix "agent-cli" agentCliSource { };
                    }
                );

                agentCorePackage = haskellPackages.agent-core;
                agentOpenaiPackage = haskellPackages.agent-openai;
                agentXaiPackage = haskellPackages.agent-xai;
                agentCliPackage = haskellPackages.agent-cli;
                agentCliExecutable = pkgs.haskell.lib.justStaticExecutables agentCliPackage;
                agentOpenaiExecutables = pkgs.haskell.lib.justStaticExecutables agentOpenaiPackage;
            in
            {
                packages.default = agentCliExecutable;
                packages.agent-cli = agentCliExecutable;
                packages.agent-core = agentCorePackage;
                packages.agent-openai = agentOpenaiPackage;
                packages.agent-xai = agentXaiPackage;
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
                        packages.agent-openai
                        packages.agent-xai
                    ];
                    withHoogle = false;
                    nativeBuildInputs = with haskellPackages; [
                        cabal-install
                        ghcid
                        haskell-language-server
                    ];
                };

                checks = {
                    agent-cli = agentCliPackage;
                    agent-core = agentCorePackage;
                    agent-openai = agentOpenaiPackage;
                    agent-xai = agentXaiPackage;
                };

                formatter = pkgs.nixfmt-rfc-style;
            }
        );
}
