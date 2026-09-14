{ mkDerivation, aeson, aeson-pretty, agent-core, agent-json
, agent-syntax, agent-tools, base, brick, bytestring, containers
, deepseq, hspec, lib, QuickCheck, safe-exceptions, text, vty
}:
mkDerivation {
  pname = "agent-tui";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty agent-core agent-json agent-syntax agent-tools
    base brick bytestring containers text vty
  ];
  testHaskellDepends = [
    agent-core agent-syntax agent-tools base brick containers hspec
    QuickCheck text vty
  ];
  benchmarkHaskellDepends = [
    aeson aeson-pretty agent-core agent-json agent-syntax agent-tools
    base brick bytestring containers deepseq safe-exceptions text vty
  ];
  description = "Retained terminal UI for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
