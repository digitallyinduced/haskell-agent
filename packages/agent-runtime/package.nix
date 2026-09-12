{ mkDerivation, aeson, agent-accounts, agent-claude
, agent-connectivity, agent-core, agent-gemini
, agent-integration-api, agent-json, agent-mcp, agent-openai
, agent-openrouter, agent-process, agent-responses
, agent-responses-types, agent-server-client, agent-store
, agent-tools, agent-xai, async, base, base64-bytestring
, bytestring, containers, crypton, directory, entropy, filelock
, filepath, hasql-pool, hspec, http-client, http-client-tls
, http-types, lib, memory, network, network-uri, process
, QuickCheck, safe-exceptions, scientific, stm, text, time
, transformers, unix, vector, wai, warp
}:
mkDerivation {
  pname = "agent-runtime";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-accounts agent-claude agent-connectivity agent-core
    agent-gemini agent-integration-api agent-json agent-mcp
    agent-openai agent-openrouter agent-process agent-responses
    agent-responses-types agent-server-client agent-store agent-tools
    agent-xai async base base64-bytestring bytestring containers
    crypton directory entropy filelock filepath hasql-pool http-client
    http-client-tls http-types memory network network-uri process
    safe-exceptions scientific stm text time transformers unix vector
    wai warp
  ];
  testHaskellDepends = [
    aeson agent-accounts agent-connectivity agent-core agent-json
    agent-openai agent-openrouter agent-responses agent-responses-types
    agent-store agent-tools agent-xai async base bytestring containers
    directory filepath hspec http-client http-types network QuickCheck
    safe-exceptions stm text time transformers unix wai warp
  ];
  description = "Frontend-independent conversation and turn lifecycle";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
