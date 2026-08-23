{ mkDerivation, aeson, agent-claude, agent-core, agent-openai
, agent-openrouter, agent-responses, agent-syntax, agent-tui
, agent-xai, ansi-terminal, async, base, base64-bytestring, brick
, bytestring, colour, containers, directory, filepath, haskeline
, hspec, JuicyPixels, lib, mtl, process, safe-exceptions, stm, text
, time, transformers, unix, vector, vty, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-claude agent-core agent-openai agent-openrouter
    agent-responses agent-syntax agent-tui agent-xai ansi-terminal
    async base base64-bytestring brick bytestring colour containers
    directory filepath haskeline JuicyPixels mtl process
    safe-exceptions stm text time transformers unix vector vty
    vty-crossplatform
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-claude agent-core agent-openai agent-openrouter
    agent-responses agent-tui agent-xai ansi-terminal base brick
    bytestring colour containers directory filepath haskeline hspec
    JuicyPixels process safe-exceptions stm text time transformers unix
    vty
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "agent-cli";
}
