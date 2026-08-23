{ mkDerivation, aeson, agent-core, async, base, containers
, directory, filepath, hspec, lib, safe-exceptions, text, time
, transformers, unix
}:
mkDerivation {
  pname = "agent-codex-dialect";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core async base containers directory filepath
    safe-exceptions text time transformers unix
  ];
  testHaskellDepends = [
    agent-core base directory filepath hspec safe-exceptions text time
    unix
  ];
  description = "Codex model-facing dialect for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
