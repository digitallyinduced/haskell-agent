{ mkDerivation, aeson, agent-json, base, containers, hermes-json
, lib, scientific, text
}:
mkDerivation {
  pname = "agent-responses-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-json base containers hermes-json scientific text
  ];
  description = "Canonical wire types for Responses-compatible APIs";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
