{ mkDerivation, aeson, agent-core, agent-json, agent-responses
, agent-responses-types, async, base, base64-bytestring, bytestring
, case-insensitive, containers, hspec, http-client, http-client-tls
, http-conduit, http-types, lib, process, retry, safe-exceptions
, text, time, wai, warp, websockets, wuss
}:
mkDerivation {
  pname = "agent-xai";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    async base bytestring containers http-client http-client-tls
    http-conduit process retry safe-exceptions text time websockets
    wuss
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    async base base64-bytestring bytestring case-insensitive containers
    hspec http-types retry safe-exceptions text wai warp
  ];
  description = "Haskell client for the xAI Grok subscription transport";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
