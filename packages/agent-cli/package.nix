{ mkDerivation, aeson, agent-claude, agent-cli-runtime
, agent-codex-dialect, agent-core, agent-gemini
, agent-grok-build-dialect, agent-json, agent-mcp, agent-openai
, agent-openrouter, agent-process, agent-responses
, agent-responses-types, agent-store, agent-syntax, agent-tui
, agent-xai, ansi-terminal, async, base, base64-bytestring, brick
, bytestring, colour, containers, crypton, deepseq, directory
, entropy, filelock, filepath, haskeline, hasql-pool, hspec
, http-client, http-client-tls, http-types, JuicyPixels, lib
, memory, mtl, network, network-uri, optparse-applicative, process
, QuickCheck, retry, safe-exceptions, scientific, stm, tagsoup
, text, time, transformers, unix, vector, vty, vty-crossplatform
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-claude agent-cli-runtime agent-codex-dialect agent-core
    agent-gemini agent-grok-build-dialect agent-json agent-mcp
    agent-openai agent-openrouter agent-process agent-responses
    agent-responses-types agent-store agent-syntax agent-tui agent-xai
    ansi-terminal async base base64-bytestring brick bytestring colour
    containers crypton directory entropy filelock filepath haskeline
    hasql-pool http-client http-client-tls http-types JuicyPixels
    memory mtl network network-uri optparse-applicative process retry
    safe-exceptions scientific stm tagsoup text time transformers unix
    vector vty vty-crossplatform
  ];
  executableHaskellDepends = [
    aeson agent-cli-runtime agent-responses agent-responses-types
    agent-store base bytestring containers directory filepath process
    safe-exceptions text time unix
  ];
  testHaskellDepends = [
    aeson agent-claude agent-cli-runtime agent-codex-dialect agent-core
    agent-gemini agent-grok-build-dialect agent-json agent-mcp
    agent-openai agent-openrouter agent-responses agent-responses-types
    agent-store agent-tui agent-xai ansi-terminal async base brick
    bytestring colour containers directory filepath haskeline hspec
    JuicyPixels process QuickCheck safe-exceptions stm text time
    transformers unix vty
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    agent-store async base brick bytestring containers deepseq
    directory filepath JuicyPixels safe-exceptions text time vty
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
