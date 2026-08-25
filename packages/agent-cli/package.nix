{ mkDerivation, aeson, agent-claude, agent-codex-dialect
, agent-core, agent-grok-build-dialect, agent-openai
, agent-openrouter, agent-responses, agent-responses-types
, agent-store, agent-syntax, agent-tui, agent-xai, ansi-terminal
, async, base, base64-bytestring, brick, bytestring, colour
, containers, deepseq, directory, filelock, filepath, haskeline
, hasql-pool, hspec, http-client, http-client-tls, http-types
, JuicyPixels, lib, mtl, network, network-uri, process, QuickCheck
, retry, safe-exceptions, scientific, stm, tagsoup, text, time
, transformers, unix, vector, vty, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
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
    JuicyPixels mtl network network-uri process retry safe-exceptions
    scientific stm tagsoup text time transformers unix vector vty
    vty-crossplatform
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
    QuickCheck safe-exceptions stm text time transformers unix vty
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-responses agent-store base brick bytestring
    containers deepseq directory filepath JuicyPixels safe-exceptions
    text time vty
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
