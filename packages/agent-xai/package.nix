{ mkDerivation, aeson, agent-core, agent-openai, base
, base64-bytestring, bytestring, case-insensitive, hspec
, http-client, http-conduit, http-types, lib, safe-exceptions, text
, wai, warp
}:
mkDerivation {
  pname = "agent-xai";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-openai base base64-bytestring bytestring
    http-client http-conduit safe-exceptions text
  ];
  testHaskellDepends = [
    aeson agent-core agent-openai base base64-bytestring bytestring
    case-insensitive hspec http-types text wai warp
  ];
  description = "Haskell client for the xAI Grok subscription transport";
  license = lib.licenses.bsd3;
}
