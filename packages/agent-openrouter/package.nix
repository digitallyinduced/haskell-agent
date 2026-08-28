{ mkDerivation, aeson, agent-core, agent-json, agent-responses
, agent-responses-types, base, bytestring, case-insensitive
, containers, hspec, http-client, http-client-tls, http-conduit
, http-types, lib, retry, safe-exceptions, scientific, text, time
, wai, warp
}:
mkDerivation {
  pname = "agent-openrouter";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-core agent-json agent-responses agent-responses-types base
    bytestring containers http-client http-client-tls http-conduit
    http-types retry safe-exceptions scientific text time
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    base bytestring case-insensitive containers hspec http-types retry
    text time wai warp
  ];
  description = "Haskell client for the OpenRouter Responses transport";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
