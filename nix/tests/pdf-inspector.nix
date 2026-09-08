{ pkgs }:

pkgs.runCommand "pdf-inspector-contract"
  {
    nativeBuildInputs = [
      (pkgs.callPackage ../pdf-inspector.nix { })
      pkgs.python3
    ];
  }
  ''
    python3 ${../../scripts/verify-pdf-inspector.py}
    touch "$out"
  ''
