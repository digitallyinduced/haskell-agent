{ mkDerivation, aeson, base, bytestring, containers, hspec
, jsonifier, lib, QuickCheck, scientific, text, vector
}:
mkDerivation {
  pname = "agent-json-codec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers jsonifier scientific text vector
  ];
  testHaskellDepends = [
    aeson base bytestring hspec QuickCheck scientific text
  ];
  description = "Direct JSON codecs without an intermediate DOM";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
