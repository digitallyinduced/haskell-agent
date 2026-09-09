{ mkDerivation, aeson, agent-claude, agent-cli-runtime
, agent-codex-dialect, agent-connectivity, agent-core
, agent-external-session, agent-gemini, agent-grok-build-dialect
, agent-integration-api, agent-json, agent-mcp, agent-openai
, agent-openrouter, agent-process, agent-responses
, agent-responses-types, agent-store, agent-syntax, agent-tui
, agent-xai, ansi-terminal, async, base, base64-bytestring, brick
, bytestring, colour, containers, crypton, crypton-connection, dbus
, deepseq, directory, entropy, filelock, filepath, haskeline
, hasql-pool, hspec, http-client, http-client-tls, http-types
, JuicyPixels, lib, memory, mtl, network, network-uri
, optparse-applicative, process, QuickCheck, retry, safe-exceptions
, scientific, stm, tagsoup, temporary, terminfo, text, time, tls
, transformers, unix, vector, vty, vty-crossplatform, vty-unix, wai
, warp
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-claude agent-cli-runtime agent-codex-dialect
    agent-connectivity agent-core agent-external-session agent-gemini
    agent-grok-build-dialect agent-integration-api agent-json agent-mcp
    agent-openai agent-openrouter agent-process agent-responses
    agent-responses-types agent-store agent-syntax agent-tui agent-xai
    ansi-terminal async base base64-bytestring brick bytestring colour
    containers crypton crypton-connection dbus directory entropy
    filelock filepath haskeline hasql-pool http-client http-client-tls
    http-types JuicyPixels memory mtl network network-uri
    optparse-applicative process retry safe-exceptions scientific stm
    tagsoup terminfo text time tls transformers unix vector vty
    vty-crossplatform vty-unix wai warp
  ];
  executableHaskellDepends = [
    aeson agent-cli-runtime agent-responses agent-responses-types
    agent-store base bytestring containers directory filepath process
    safe-exceptions text time unix
  ];
  testHaskellDepends = [
    aeson agent-claude agent-cli-runtime agent-codex-dialect
    agent-connectivity agent-core agent-gemini agent-grok-build-dialect
    agent-integration-api agent-json agent-mcp agent-openai
    agent-openrouter agent-responses agent-responses-types agent-store
    agent-tui agent-xai ansi-terminal async base base64-bytestring
    brick bytestring colour containers dbus directory filelock filepath
    haskeline hspec http-client http-types JuicyPixels network
    network-uri process QuickCheck safe-exceptions stm temporary text
    time transformers unix vty vty-unix wai warp
  ];
  benchmarkHaskellDepends = [
    aeson agent-cli-runtime agent-core agent-json agent-mcp
    agent-openai agent-responses agent-responses-types agent-store
    agent-tui ansi-terminal async base brick bytestring colour
    containers deepseq directory filepath JuicyPixels process
    safe-exceptions stm text time unix vty
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
