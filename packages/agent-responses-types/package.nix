{ mkDerivation, aeson, base, hermes-json, lib, scientific, text
, vector
}:
mkDerivation {
  pname = "agent-responses-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base hermes-json scientific text vector
  ];
  description = "Canonical wire types for Responses-compatible APIs";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
