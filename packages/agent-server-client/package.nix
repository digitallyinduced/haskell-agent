{ mkDerivation, aeson, async, base, base64-bytestring, bytestring, crypton
, directory, hspec, http-client, http-client-tls, http-types, lib
, network-uri, safe-exceptions, text, time, unix, uuid, wai, warp
}:
mkDerivation {
  pname = "agent-server-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring crypton http-client
    http-client-tls http-types network-uri safe-exceptions text time unix uuid
  ];
  testHaskellDepends = [
    aeson base bytestring directory hspec http-types text unix wai warp
  ];
  description = "Typed REST and SSE client for agent-server";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
