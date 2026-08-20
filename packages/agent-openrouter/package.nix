{ mkDerivation, aeson, agent-core, agent-openai, base, bytestring
, case-insensitive, hspec, http-client, http-conduit, http-types
, lib, safe-exceptions, text, time, wai, warp
}:
mkDerivation {
  pname = "agent-openrouter";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-openai base bytestring http-client
    http-conduit http-types safe-exceptions text time
  ];
  testHaskellDepends = [
    aeson agent-core agent-openai base bytestring case-insensitive
    hspec http-types text time wai warp
  ];
  description = "Haskell client for the OpenRouter Responses transport";
  license = lib.licenses.bsd3;
}
