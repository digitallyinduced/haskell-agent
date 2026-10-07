{ pkgs, self, system }:
let
    extend = packageMode:
        pkgs.haskellPackages.extend (self.lib.haskellPackageOverrides {
            inherit pkgs packageMode;
        });
    development = pkgs.haskellPackages.extend
        (self.lib.haskellPackageOverrides { inherit pkgs; });
    check = extend "check";
    production = extend "production";
    composed = pkgs.haskellPackages.extend (pkgs.lib.composeExtensions
        (self.lib.haskellPackageOverrides { inherit pkgs; })
        (_final: previous: {
            agent-core = pkgs.haskell.lib.dontHaddock previous.agent-core;
        }));
in
assert development.ghc.drvPath == pkgs.haskellPackages.ghc.drvPath;
assert development.agent-core.doCheck;
assert builtins.elem "doc" development.agent-core.outputs;
assert !(builtins.elem "doc" composed.agent-core.outputs);
assert !(builtins.elem "doc" check.agent-core.outputs);
assert !production.agent-core.doCheck;
assert check.agent-core.drvPath == self.checks.${system}.agent-core.drvPath;
assert production.agent-core.drvPath == self.packages.${system}.agent-core.drvPath;
assert production.agent-openai.drvPath == self.packages.${system}.agent-openai.drvPath;
assert development.agent-openai.prePatch != "";
assert builtins.elem pkgs.git development.agent-tools.nativeBuildInputs;
assert builtins.elem (pkgs.lib.getDev pkgs.postgresql_18)
    development.agent-telegram-connector.nativeBuildInputs;
pkgs.runCommand "haskell-package-overrides" { } ''
    touch "$out"
''
