{ mkDerivation, aeson, async, base, bytestring, hspec, http-client
, http-types, lib, process, safe-exceptions, text, unix, wai, warp
}:
mkDerivation {
  pname = "agent-audio";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-client http-types process
    safe-exceptions text unix
  ];
  testHaskellDepends = [
    async base bytestring hspec http-client http-types text wai warp
  ];
  description = "Bounded audio conversion and transcription transport";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
