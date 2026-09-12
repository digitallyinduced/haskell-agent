{ mkDerivation, aeson, agent-cli, agent-computer-use, agent-core
, agent-integration-api, agent-json, agent-mcp
, agent-responses-types, agent-runtime, agent-runtime-daemon
, agent-store, agent-tools, async, base, base64-bytestring
, bytestring, containers, directory, filepath, hspec, http-client
, http-client-tls, http-types, lib, network-uri, safe-exceptions
, stm, text, time, transformers, unix, uuid, websockets, wuss
}:
mkDerivation {
  pname = "agent-native-bridge";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-core agent-json agent-mcp agent-runtime agent-store
    agent-tools base bytestring containers filepath http-client
    http-client-tls http-types network-uri safe-exceptions text time
    transformers uuid websockets wuss
  ];
  testHaskellDepends = [
    aeson agent-cli agent-computer-use agent-core agent-integration-api
    agent-json agent-mcp agent-responses-types agent-runtime
    agent-runtime-daemon agent-store agent-tools async base
    base64-bytestring bytestring containers directory filepath hspec
    http-client http-client-tls http-types safe-exceptions stm text
    unix websockets wuss
  ];
  description = "Native host integration for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
