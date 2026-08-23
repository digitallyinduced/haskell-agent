{ mkDerivation, aeson, agent-claude, agent-codex-dialect
, agent-core, agent-grok-build-dialect, agent-openai
, agent-openrouter, agent-responses, agent-syntax, agent-tui
, agent-xai, ansi-terminal, async, base, base64-bytestring, brick
, bytestring, colour, containers, directory, filelock, filepath
, haskeline, hspec, JuicyPixels, lib, mtl, process, safe-exceptions
, stm, text, time, transformers, unix, vector, vty
, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-claude agent-codex-dialect agent-core
    agent-grok-build-dialect agent-openai agent-openrouter
    agent-responses agent-syntax agent-tui agent-xai ansi-terminal
    async base base64-bytestring brick bytestring colour containers
    directory filelock filepath haskeline JuicyPixels mtl process
    safe-exceptions stm text time transformers unix vector vty
    vty-crossplatform
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-claude agent-codex-dialect agent-core
    agent-grok-build-dialect agent-openai agent-openrouter
    agent-responses agent-tui agent-xai ansi-terminal async base brick
    bytestring colour containers directory filepath haskeline hspec
    JuicyPixels process safe-exceptions stm text time transformers unix
    vty
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-responses base bytestring containers
    JuicyPixels text
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "agent-cli";
}
