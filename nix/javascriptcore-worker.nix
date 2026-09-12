{ lib, stdenv, fetchurl }:
assert stdenv.hostPlatform.isDarwin;
let
    acorn = fetchurl {
        url = "https://registry.npmjs.org/acorn/-/acorn-8.15.0.tgz";
        hash = "sha256-5G6uib2XYfFtHSVWO4N7VbS3jz7OBu4Ll4nLJkT1I7M=";
    };
in stdenv.mkDerivation {
    pname = "agent-code-mode-worker";
    version = "0.1.0";
    src = lib.cleanSourceWith {
        src = ../packages/agent-tools;
        filter = path: type:
            type == "directory"
            || lib.hasSuffix "/cbits/javascriptcore-worker.mm" path
            || lib.hasInfix "/data/code-mode/javascriptcore/" path;
    };
    strictDeps = true;
    buildPhase = ''
        runHook preBuild
        $CXX -std=c++17 -O2 -Wall -Wextra -fobjc-arc \
            -framework Foundation -framework JavaScriptCore \
            cbits/javascriptcore-worker.mm -o agent-code-mode-worker
        runHook postBuild
    '';
    installPhase = ''
        runHook preInstall
        install -Dm755 agent-code-mode-worker "$out/bin/agent-code-mode-worker"
        mkdir -p "$out/share/agent-code-mode-worker" "$out/share/licenses/agent-code-mode-worker"
        tar -xzf ${acorn} package/dist/acorn.js package/LICENSE
        cp package/dist/acorn.js data/code-mode/javascriptcore/{lower-module,worker}.js \
            "$out/share/agent-code-mode-worker/"
        cp package/LICENSE "$out/share/licenses/agent-code-mode-worker/acorn-LICENSE"
        runHook postInstall
    '';
    doInstallCheck = true;
    installCheckPhase = ''
        "$out/bin/agent-code-mode-worker" --check
    '';
    meta = {
        description = "Isolated code-mode worker using the macOS JavaScriptCore framework";
        license = lib.licenses.mit;
        platforms = lib.platforms.darwin;
        mainProgram = "agent-code-mode-worker";
    };
}
