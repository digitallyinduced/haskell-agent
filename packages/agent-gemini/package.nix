{ mkDerivation, aeson, agent-core, agent-json, agent-responses
, agent-responses-types, async, base, base64-bytestring, bytestring
, containers, crypton, entropy, hspec, http-client, http-client-tls
, http-conduit, http-types, lib, memory, network, retry
, safe-exceptions, scientific, text, time, wai, warp
}:
mkDerivation {
  pname = "agent-gemini";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    base base64-bytestring bytestring containers crypton entropy
    http-client http-client-tls http-conduit http-types memory network
    retry safe-exceptions scientific text time
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    async base bytestring containers hspec http-conduit http-types
    network retry safe-exceptions text wai warp
  ];
  description = "Native Google Gemini transports for haskell-agent";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
