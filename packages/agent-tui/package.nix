{ mkDerivation, aeson, aeson-pretty, agent-core, agent-json
, agent-syntax, base, brick, bytestring, containers, hspec, lib
, QuickCheck, text, vty
}:
mkDerivation {
  pname = "agent-tui";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty agent-core agent-json agent-syntax base brick
    bytestring containers text vty
  ];
  testHaskellDepends = [
    agent-core agent-syntax base brick containers hspec QuickCheck text
    vty
  ];
  description = "Retained terminal UI for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
