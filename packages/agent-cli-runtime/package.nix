{ mkDerivation, aeson, agent-claude, agent-core, agent-gemini
, agent-json, agent-openai, agent-openrouter, agent-process
, agent-responses-types, agent-server-client, agent-store
, agent-xai, async, base, base64-bytestring, bytestring, containers
, crypton, directory, entropy, filelock, filepath, hspec
, http-client, http-client-tls, http-types, lib, memory, network
, network-uri, process, QuickCheck, safe-exceptions, scientific
, stm, text, time, transformers, unix, vector, wai, warp
}:
mkDerivation {
  pname = "agent-cli-runtime";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-claude agent-core agent-gemini agent-json agent-openai
    agent-openrouter agent-process agent-responses-types
    agent-server-client agent-store agent-xai async base
    base64-bytestring bytestring containers crypton directory entropy
    filelock filepath http-client http-client-tls http-types memory
    network network-uri process safe-exceptions scientific stm text
    time transformers unix vector wai warp
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses-types agent-store async
    base bytestring containers directory filepath hspec http-client
    http-types QuickCheck safe-exceptions text time unix wai warp
  ];
  description = "Headless shared runtime for agent frontends";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
