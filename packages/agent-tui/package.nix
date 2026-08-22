{ mkDerivation, aeson, agent-core, base, brick, containers
, directory, filepath, hspec, lib, skylighting-core, text, vty
}:
mkDerivation {
  pname = "agent-tui";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-core base brick containers filepath skylighting-core
    text vty
  ];
  testHaskellDepends = [
    agent-core base brick directory hspec text vty
  ];
  description = "Retained terminal UI for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
