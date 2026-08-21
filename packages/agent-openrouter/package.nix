{ mkDerivation, aeson, agent-core, agent-openai, base, bytestring
, case-insensitive, hspec, http-client, http-client-tls, http-conduit, http-types
, lib, retry, safe-exceptions, scientific, text, time, wai, warp
}:
mkDerivation {
  pname = "agent-openrouter";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-openai base bytestring http-client http-client-tls
    http-conduit http-types retry safe-exceptions scientific text time
  ];
  testHaskellDepends = [
    aeson agent-core agent-openai base bytestring case-insensitive
    hspec http-types retry text time wai warp
  ];
  description = "Haskell client for the OpenRouter Responses transport";
  license = lib.licenses.bsd3;
}
