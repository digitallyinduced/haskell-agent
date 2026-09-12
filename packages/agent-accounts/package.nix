{ mkDerivation, aeson, agent-claude, agent-core, agent-gemini
, agent-json, agent-openai, agent-openrouter, agent-server-client
, agent-xai, async, base, bytestring, containers, directory
, filepath, hspec, lib, network-uri, safe-exceptions, stm, text
, time, transformers, unix
}:
mkDerivation {
  pname = "agent-accounts";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-claude agent-core agent-gemini agent-json agent-openai
    agent-openrouter agent-server-client agent-xai base bytestring
    containers directory filepath network-uri safe-exceptions stm text
    time transformers unix
  ];
  testHaskellDepends = [
    aeson agent-core agent-json async base bytestring directory
    filepath hspec safe-exceptions text unix
  ];
  description = "Shared harness accounts and credential ownership";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
