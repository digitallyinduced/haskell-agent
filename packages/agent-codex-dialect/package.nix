{ mkDerivation, agent-core, agent-json, agent-tools, async, base
, containers, directory, filepath, hspec, lib, process
, safe-exceptions, text, time, transformers, unix
}:
mkDerivation {
  pname = "agent-codex-dialect";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-core agent-json agent-tools async base containers directory
    filepath safe-exceptions text time transformers unix
  ];
  testHaskellDepends = [
    agent-core agent-tools base directory filepath hspec process
    safe-exceptions text time unix
  ];
  description = "Codex model-facing dialect for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
