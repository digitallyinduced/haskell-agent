{ mkDerivation, aeson, agent-core, agent-json, agent-responses
, agent-responses-types, async, base, base64-bytestring, bytestring
, case-insensitive, containers, directory, exceptions, filepath
, HsOpenSSL, hspec, http-client, http-conduit, http-streams
, http-types, io-streams, lib, network, network-uri, retry
, safe-exceptions, scientific, template-haskell, temporary, text
, time, transformers, unix, vector, wai, warp, websockets, wuss
}:
mkDerivation {
  pname = "agent-openai";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    async base base64-bytestring bytestring containers directory
    exceptions filepath HsOpenSSL http-client http-conduit http-streams
    io-streams network-uri retry safe-exceptions scientific
    template-haskell text time transformers unix vector websockets wuss
  ];
  executableHaskellDepends = [
    agent-core base directory filepath text
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    async base base64-bytestring bytestring case-insensitive directory
    filepath hspec http-types network retry safe-exceptions temporary
    text time unix vector wai warp websockets
  ];
  benchmarkHaskellDepends = [
    agent-core agent-responses agent-responses-types base bytestring
    text
  ];
  description = "Haskell client for the OpenAI Responses API";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "agent-openai-login";
}
