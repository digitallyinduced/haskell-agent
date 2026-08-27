{ mkDerivation, aeson, async, base, bytestring, hermes-json, hspec
, lib, safe-exceptions, text, vector
}:
mkDerivation {
  pname = "agent-json";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring hermes-json safe-exceptions text vector
  ];
  testHaskellDepends = [ aeson async base bytestring hspec text ];
  description = "Concrete Hermes JSON decoding for the agent";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
