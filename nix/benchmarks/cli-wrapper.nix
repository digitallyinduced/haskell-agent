# Apply the production launcher to one existing optimized executable. Both
# variants use identical resources; no Haskell rebuild or model request is needed.
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
  check = pkgs.runCommand "agent-cli-launcher-environment-check"
    { nativeBuildInputs = [ pkgs.python3 ]; }
    ''
      python3 - <<'PY'
      import os
      import subprocess

      variables = [
          "PATH", "GST_PLUGIN_SYSTEM_PATH_1_0", "AGENT_SYNTAX_DIR",
          "AGENT_POSTGRES_BIN", "AGENT_MEDIA_HELPER",
          "HASKELL_AGENT_APPLE_SESSION_TITLE",
      ]
      paths = ${builtins.toJSON
        (pkgs.lib.splitString ":" (pkgs.lib.makeBinPath runtimeTools))}
      plugins = ${builtins.toJSON
        (pkgs.lib.splitString ":"
          (pkgs.lib.makeSearchPath "lib/gstreamer-1.0" gstreamerPlugins))}

      def environment(binary, overrides):
          inherited = {key: value for key, value in os.environ.items()
                       if key not in variables}
          inherited.update(overrides)
          result = subprocess.run([binary, "-0"], env=inherited,
                                  capture_output=True, check=True)
          values = dict(entry.split(b"=", 1) for entry in result.stdout.split(b"\0")
                        if b"=" in entry)
          return {key: values.get(key.encode()) for key in variables}

      cases = [
          {},
          {key: "" for key in variables},
          {key: "/custom/location with spaces" for key in variables},
          {"PATH": ":".join(["/custom/bin", *paths, "/usr/bin"]),
           "GST_PLUGIN_SYSTEM_PATH_1_0": ":".join(["/custom/plugins", *plugins])},
          {"PATH": ":".join(reversed(paths)),
           "GST_PLUGIN_SYSTEM_PATH_1_0": ":".join(reversed(plugins))},
      ]
      for index, case in enumerate(cases):
          shell = environment("${shellEnvironment}/bin/agent-cli", case)
          native = environment("${nativeEnvironment}/bin/agent-cli", case)
          assert native["PATH"].decode().split(":")[:len(paths)] == paths
          assert native["GST_PLUGIN_SYSTEM_PATH_1_0"].decode().split(":")[:len(plugins)] == plugins
          assert native["AGENT_SYNTAX_DIR"] == case.get(
              "AGENT_SYNTAX_DIR", ${builtins.toJSON (toString syntaxDirectory)}).encode()
          assert native["AGENT_POSTGRES_BIN"] == case.get(
              "AGENT_POSTGRES_BIN", ${builtins.toJSON (toString postgresBin)}).encode()
          # Nix's Bash supplies a non-existent default when PATH is unset.
          # The compiled launcher adds only the configured command directories.
          if "PATH" not in case:
              shell["PATH"] = shell["PATH"].removesuffix(b":/no-such-path")
          for key in variables:
              assert shell[key] == native[key], (index, key)
      print("Launcher environments agree for all five cases")

      for arguments, expected_status in [
          (["--version"], 0),
          (["--help"], 0),
          (["--invalid-launcher-option"], 1),
          (["+RTS", "-N2", "-RTS", "--version"], 0),
      ]:
          shell = subprocess.run(["${shell}/bin/agent-cli", *arguments],
                                 capture_output=True, timeout=30)
          native = subprocess.run(["${native}/bin/agent-cli", *arguments],
                                  capture_output=True, timeout=30)
          assert native.returncode == shell.returncode == expected_status
          assert (native.stdout, native.stderr) == (shell.stdout, shell.stderr)
      print("Launcher arguments, output and exit statuses agree")
      PY
      touch "$out"
    '';
in
pkgs.linkFarm "agent-cli-launcher-comparison" [
  { name = "shell"; path = shell; }
  { name = "native"; path = native; }
  { name = "environment-check"; path = check; }
]
