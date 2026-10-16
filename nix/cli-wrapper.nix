{ pkgs
, syntaxDirectory
, postgresBin
, runtimeTools
, gstreamerPlugins
, mediaHelper ? null
, appleSessionTitle ? null
, wrapperTool ? null
}:

let
  # The pinned helper appends a separator to an explicitly empty value. For
  # PATH that would introduce the current directory; match the shell launcher
  # by treating empty search paths as absent before adding a prefix.
  nativeWrapperTool = pkgs.makeBinaryWrapper.overrideAttrs (old: {
    buildCommand = old.buildCommand + ''
      substituteInPlace "$out/nix-support/setup-hook" \
        --replace-fail 'if (existing_env) {' 'if (existing_env && *existing_env) {'
    '';
  });
  # makeWrapper splits search paths into individual prefixes. Keep that
  # ordering and duplicate removal when using the compiled wrapper too.
  prefixDirectories = variable: directories:
    pkgs.lib.concatMap
      (directory: [ "--prefix" variable ":" directory ])
      (pkgs.lib.reverseList directories);
  arguments = [
    "--set-default" "AGENT_SYNTAX_DIR" syntaxDirectory
    "--set-default" "AGENT_POSTGRES_BIN" postgresBin
  ]
    ++ prefixDirectories "PATH"
      (pkgs.lib.splitString ":" (pkgs.lib.makeBinPath runtimeTools))
    ++ pkgs.lib.optionals (mediaHelper != null) [
      "--set" "AGENT_MEDIA_HELPER" "${mediaHelper}/bin/agent-media-helper"
    ]
    ++ pkgs.lib.optionals (appleSessionTitle != null) [
      "--set" "HASKELL_AGENT_APPLE_SESSION_TITLE" "${appleSessionTitle}/bin/apple-session-title"
    ]
    ++ prefixDirectories "GST_PLUGIN_SYSTEM_PATH_1_0"
      (pkgs.lib.splitString ":"
        (pkgs.lib.makeSearchPath "lib/gstreamer-1.0" gstreamerPlugins));
in
package: package.overrideAttrs (old: {
  nativeBuildInputs = (old.nativeBuildInputs or [ ])
    ++ [ (if wrapperTool == null then nativeWrapperTool else wrapperTool) ];
  postInstall = (old.postInstall or "") + ''
    wrapProgram "$out/bin/agent-cli" ${pkgs.lib.escapeShellArgs arguments}
  '';
})
