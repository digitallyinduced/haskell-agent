{
  description = "Reproducible Harbor environment for Terminal-Bench evaluations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/afe3d8ac4395617bdcdac9f188ac8717a062e014";
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, pyproject-nix, uv2nix, pyproject-build-systems, ... }:
    let
      inherit (nixpkgs) lib;
      forAllSystems = lib.genAttrs [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      workspace = uv2nix.lib.workspace.loadWorkspace { workspaceRoot = ./.; };
      overlay = workspace.mkPyprojectOverlay { sourcePreference = "wheel"; };
      environments = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pythonSet = (pkgs.callPackage pyproject-nix.build.packages {
            python = pkgs.python313;
          }).overrideScope (lib.composeManyExtensions [
            pyproject-build-systems.overlays.wheel
            overlay
          ]);
        in pythonSet.mkVirtualEnv "terminal-bench-environment" workspace.deps.default);
    in {
      packages = forAllSystems (system: {
        default = environments.${system};
        # Copy this closure into the task container when its base image has no
        # sudo. Setuid permissions and policy belong only to that container.
        privilege-support = nixpkgs.legacyPackages.${system}.sudo;
      });
      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${environments.${system}}/bin/harbor";
        };
      });
      checks = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          harbor-cli = pkgs.runCommand "harbor-cli-check" { } ''
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME" "$out"
            ${environments.${system}}/bin/harbor --help > "$out/help.txt"
          '';
          adapter-tests = pkgs.runCommand "terminal-bench-adapter-tests" { } ''
            export HOME="$TMPDIR/home"
            export PYTHONDONTWRITEBYTECODE=1
            mkdir -p "$HOME" "$out"
            ${environments.${system}}/bin/python -m unittest discover \
              -s ${./.} -p 'test_*.py' > "$out/tests.txt" 2>&1
            ${pkgs.bash}/bin/bash -n ${./run_sample_comparison.sh}
          '';
        });
      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            packages = [ environments.${system} pkgs.uv pkgs.git ];
            UV_NO_SYNC = "1";
            UV_PYTHON_DOWNLOADS = "never";
            UV_PYTHON = "${pkgs.python313}/bin/python3";
          };
        });
    };
}
