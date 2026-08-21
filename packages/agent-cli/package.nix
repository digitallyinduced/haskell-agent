{ mkDerivation, aeson, agent-core, agent-openai, agent-openrouter
, agent-responses, agent-xai, ansi-terminal, async, base
, base64-bytestring, brick, bytestring, colour, containers
, directory, filepath, haskeline, hspec, lib, mtl, process
, safe-exceptions, stm, text, time, transformers, unix, vty
, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-core agent-openai agent-openrouter agent-responses
    agent-xai ansi-terminal async base base64-bytestring brick
    bytestring colour containers directory filepath haskeline mtl
    process safe-exceptions stm text time transformers unix vty
    vty-crossplatform
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-core agent-openai agent-responses agent-xai
    ansi-terminal base bytestring colour containers directory filepath
    haskeline hspec process safe-exceptions text time unix
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "agent-cli";
}
