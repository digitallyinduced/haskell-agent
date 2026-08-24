{ mkDerivation, aeson, agent-claude, agent-codex-dialect
, agent-core, agent-grok-build-dialect, agent-openai
, agent-openrouter, agent-responses, agent-responses-types
, agent-store, agent-syntax, agent-tui, agent-xai, ansi-terminal
, async, base, base64-bytestring, brick, bytestring, colour
, containers, directory, filelock, filepath, haskeline, hasql-pool
, hspec, http-client, http-client-tls, http-types, JuicyPixels, lib
, mtl, process, retry, safe-exceptions, stm, text, time
, transformers, unix, vector, vty, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = packages/agent-cli;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-claude agent-codex-dialect agent-core
    agent-grok-build-dialect agent-openai agent-openrouter
    agent-responses agent-responses-types agent-store agent-syntax
    agent-tui agent-xai ansi-terminal async base base64-bytestring
    brick bytestring colour containers directory filelock filepath
    haskeline hasql-pool http-client http-client-tls http-types
    JuicyPixels mtl process retry safe-exceptions stm text time
    transformers unix vector vty vty-crossplatform
  ];
  executableHaskellDepends = [
    aeson agent-responses agent-responses-types agent-store base
    bytestring containers directory filepath process safe-exceptions
    text time unix
  ];
  testHaskellDepends = [
    aeson agent-claude agent-codex-dialect agent-core
    agent-grok-build-dialect agent-openai agent-openrouter
    agent-responses agent-responses-types agent-store agent-tui
    agent-xai ansi-terminal async base brick bytestring colour
    containers directory filepath haskeline hspec JuicyPixels process
    safe-exceptions stm text time transformers unix vty
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-responses agent-store base bytestring
    containers directory filepath JuicyPixels safe-exceptions text
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
