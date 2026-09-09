{ mkDerivation, aeson, agent-cli, agent-cli-runtime, agent-core
, agent-integration-api, agent-json, agent-mcp, agent-openai
, agent-openrouter, agent-repository, agent-responses-types
, agent-runtime-daemon, agent-store, agent-xai, async, base
, base64-bytestring, bytestring, containers, directory, filelock
, filepath, hspec, http-client, http-client-tls, JuicyPixels, lib
, network-uri, safe-exceptions, stm, text, time, transformers, unix
, uuid
}:
mkDerivation {
  pname = "agent-native-bridge";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-cli agent-core agent-json agent-mcp agent-store base
    bytestring containers filepath http-client http-client-tls
    network-uri safe-exceptions text time transformers uuid
  ];
  testHaskellDepends = [
    aeson agent-cli agent-cli-runtime agent-core agent-integration-api
    agent-json agent-mcp agent-openai agent-openrouter agent-repository
    agent-responses-types agent-runtime-daemon agent-store agent-xai
    async base base64-bytestring bytestring containers directory
    filelock filepath hspec JuicyPixels safe-exceptions stm text time
    unix
  ];
  description = "Native host integration for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
