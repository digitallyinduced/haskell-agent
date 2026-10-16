# Apply the production launcher to one existing optimized executable. Both
# variants use identical resources; no CLI rebuild or model request is needed.
{ pkgs, executable, syntaxDirectory, postgresBin, runtimeTools, gstreamerPlugins
, mediaHelper ? null, appleSessionTitle ? null
}:

let
  configuration = {
    inherit pkgs syntaxDirectory postgresBin runtimeTools gstreamerPlugins
      mediaHelper appleSessionTitle;
  };
  package = target: pkgs.stdenvNoCC.mkDerivation {
    name = "agent-cli-launcher-fixture";
    dontUnpack = true;
    dontConfigure = true;
    dontBuild = true;
    dontFixup = true;
    installPhase = ''
      runHook preInstall
      mkdir -p "$out/bin"
      ln -s ${pkgs.lib.escapeShellArg (toString target)} "$out/bin/agent-cli"
      runHook postInstall
    '';
  };
  wrap = wrapperTool: target:
    import ../cli-wrapper.nix (configuration // { inherit wrapperTool; })
      (package target);
  shell = wrap pkgs.makeWrapper executable;
  native = wrap null executable;
  # Coreutils may be a multicall binary whose dispatch uses argv[0]. Give env
  # its ordinary name after the launcher's intentional argv[0] inheritance.
  environmentProgram = pkgs.writeShellScript "launcher-environment" ''
    exec ${pkgs.coreutils}/bin/env "$@"
  '';
  shellEnvironment = wrap pkgs.makeWrapper environmentProgram;
  nativeEnvironment = wrap null environmentProgram;
  compiler = pkgs.haskellPackages.ghcWithPackages (packages: [ packages.aeson packages.async ]);
  checkConfiguration = pkgs.writeText "agent-cli-launcher-check.json" (builtins.toJSON {
    commandDirectories = pkgs.lib.splitString ":" (pkgs.lib.makeBinPath runtimeTools);
    pluginDirectories = pkgs.lib.splitString ":"
      (pkgs.lib.makeSearchPath "lib/gstreamer-1.0" gstreamerPlugins);
    inherit syntaxDirectory postgresBin;
    shellEnvironmentProgram = "${shellEnvironment}/bin/agent-cli";
    nativeEnvironmentProgram = "${nativeEnvironment}/bin/agent-cli";
    shellCliProgram = "${shell}/bin/agent-cli";
    nativeCliProgram = "${native}/bin/agent-cli";
  });
  check = pkgs.runCommand "agent-cli-launcher-environment-check"
    { nativeBuildInputs = [ compiler ]; }
    ''
      runghc ${../../scripts/BenchmarkCliLauncher.hs} --check ${checkConfiguration}
      touch "$out"
    '';
in
pkgs.linkFarm "agent-cli-launcher-comparison" [
  { name = "shell"; path = shell; }
  { name = "native"; path = native; }
  { name = "environment-check"; path = check; }
]
