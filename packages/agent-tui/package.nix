{ mkDerivation, aeson, agent-core, base, brick, containers, hspec
, lib, text, vty
}:
mkDerivation {
  pname = "agent-tui";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core base brick containers text vty
  ];
  testHaskellDepends = [
    agent-core base hspec text
  ];
  description = "Retained terminal UI for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
