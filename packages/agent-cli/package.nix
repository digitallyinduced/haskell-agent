{ mkDerivation, aeson, agent-codex-dialect, agent-core
, agent-grok-build-dialect, agent-openai, agent-openrouter
, agent-responses, agent-responses-types, agent-store, agent-syntax
, agent-tui, agent-xai, hasql-pool
, ansi-terminal, async, base, base64-bytestring, brick, bytestring
, colour, containers, directory, filelock, filepath, haskeline, hspec
, http-client, http-client-tls, JuicyPixels, lib, mtl, process
, safe-exceptions, stm, text, time
, transformers, unix, vector, vty, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-codex-dialect agent-core agent-grok-build-dialect
    agent-openai agent-openrouter agent-responses agent-responses-types
    agent-store agent-syntax agent-tui agent-xai ansi-terminal async base
    base64-bytestring brick bytestring colour containers directory filelock
    filepath haskeline hasql-pool http-client http-client-tls JuicyPixels mtl
    process safe-exceptions stm text
    time transformers unix vector vty vty-crossplatform
  ];
  executableHaskellDepends = [
    aeson agent-responses agent-store base bytestring containers directory
    filepath process safe-exceptions text time unix
  ];
  testHaskellDepends = [
    aeson agent-codex-dialect agent-core agent-grok-build-dialect
    agent-openai agent-responses agent-responses-types agent-store agent-tui
    agent-xai ansi-terminal base brick bytestring colour containers directory
    filepath haskeline hspec JuicyPixels process safe-exceptions stm
    text time transformers unix vty
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "agent-cli";
}
