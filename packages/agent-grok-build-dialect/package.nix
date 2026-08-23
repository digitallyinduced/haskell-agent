{ mkDerivation, aeson, agent-core, async, base, containers
, directory, filepath, hspec, lib, process, safe-exceptions, text
, time, transformers, unix
}:
mkDerivation {
  pname = "agent-grok-build-dialect";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core async base containers directory filepath process
    safe-exceptions text time transformers unix
  ];
  testHaskellDepends = [
    agent-core base containers directory filepath hspec safe-exceptions text
    time unix
  ];
  description = "Grok Build model-facing dialect for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
