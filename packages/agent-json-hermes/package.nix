{ mkDerivation, aeson, agent-json-codec, async, base, bytestring
, hermes-json, hspec, jsonifier, lib, safe-exceptions, text
}:
mkDerivation {
  pname = "agent-json-hermes";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-json-codec base bytestring hermes-json safe-exceptions text
  ];
  testHaskellDepends = [
    agent-json-codec async base bytestring hspec text
  ];
  benchmarkHaskellDepends = [
    aeson agent-json-codec base bytestring jsonifier text
  ];
  description = "Hermes backend for direct agent JSON codecs";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
