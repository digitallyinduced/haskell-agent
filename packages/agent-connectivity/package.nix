{ mkDerivation, agent-core, async, base, hspec, lib
, safe-exceptions, stm, text
}:
mkDerivation {
  pname = "agent-connectivity";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-core async base safe-exceptions stm text
  ];
  testHaskellDepends = [
    agent-core async base hspec safe-exceptions stm
  ];
  description = "Connection recovery for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
