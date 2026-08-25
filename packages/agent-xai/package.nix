{ mkDerivation, aeson, agent-core, agent-responses, agent-responses-types, async, base
, base64-bytestring, bytestring, case-insensitive, hspec
, http-client, http-client-tls, http-conduit, http-types, lib, retry, safe-exceptions
, scientific, text, time, wai, warp, process, websockets, wuss
}:
mkDerivation {
  pname = "agent-xai";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-responses agent-responses-types async base base64-bytestring bytestring
    http-client http-client-tls http-conduit retry safe-exceptions scientific
    text time process websockets wuss
  ];
  testHaskellDepends = [
    aeson agent-core agent-responses agent-responses-types async base base64-bytestring bytestring
    case-insensitive hspec http-types retry safe-exceptions text wai warp
  ];
  description = "Haskell client for the xAI Grok subscription transport";
  license = lib.licenses.bsd3;
}
