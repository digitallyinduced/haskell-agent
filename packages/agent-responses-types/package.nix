{ mkDerivation, aeson, base, lib, scientific, text }:
mkDerivation {
  pname = "agent-responses-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base scientific text ];
  description = "Canonical wire types for Responses-compatible APIs";
  license = lib.licenses.bsd3;
}
