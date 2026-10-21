{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, crypton, directory, filelock, filepath, hspec, http-client
, http-client-tls, http-types, lib, network-uri, process
, safe-exceptions, text, time, unix, uuid, wai, warp
}:
mkDerivation {
  pname = "agent-server-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring crypton directory
    filelock filepath http-client http-client-tls http-types
    network-uri process safe-exceptions text time unix uuid
  ];
  testHaskellDepends = [
    aeson base bytestring directory filepath hspec http-types
    safe-exceptions text unix wai warp
  ];
  description = "Typed REST and SSE client for agent-server";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
