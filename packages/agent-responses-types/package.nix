{ mkDerivation, aeson, agent-json-codec, base, bytestring, hspec
, lib, scientific, text
}:
mkDerivation {
  pname = "agent-responses-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    agent-json-codec base bytestring scientific text
  ];
  testHaskellDepends = [
    aeson agent-json-codec base bytestring hspec
  ];
  description = "Canonical wire types for Responses-compatible APIs";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
