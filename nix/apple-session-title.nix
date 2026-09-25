{
    pkgs,
    source,
}:
assert pkgs.stdenv.hostPlatform.isDarwin;
let
    # Symlinks the host Xcode. FoundationModels and its macro plugin are not in
    # the Nix SDK, and nixpkgs Swift is too old to compile them.
    xcodeWrapper = pkgs.xcodeenv.composeXcodeWrapper {
        xcodeBaseDir = "/Applications/Xcode.app";
    };
in
pkgs.stdenv.mkDerivation {
    pname = "apple-session-title";
    version = "0.1.0";
    src = source;
    dontConfigure = true;

    buildInputs = [ (pkgs.darwinMinVersionHook "26.0") ];

    # The wrapper fails in a sandbox. Host Xcode stays outside the store.
    __noChroot = true;
    preferLocalBuild = true;
    allowSubstitutes = false;

    buildPhase = ''
        runHook preBuild

        if [ ! -x /Applications/Xcode.app/Contents/Developer/usr/bin/xcodebuild ]; then
            echo "apple-session-title requires Xcode at /Applications/Xcode.app" >&2
            exit 1
        fi

        # The wrapper has to precede stdenv's xcrun. Nix points DEVELOPER_DIR
        # at apple-sdk; clearing it selects the host Xcode, as MacVim does.
        export PATH=${xcodeWrapper}/bin:$PATH
        # The Nix build user cannot sandbox_apply, so the macro plugin must
        # run unsandboxed. A closed stdin makes swift-plugin-server reject
        # @Generable, and Nix does not leave stdin open.
        env -u DEVELOPER_DIR -u SDKROOT \
            xcrun --sdk macosx swiftc \
                -parse-as-library \
                -O \
                -disable-sandbox \
                -o apple-session-title \
                helpers/apple-session-title/main.swift \
                < /dev/null

        runHook postBuild
    '';

    installPhase = ''
        runHook preInstall
        install -Dm755 apple-session-title "$out/bin/apple-session-title"
        runHook postInstall
    '';
}
