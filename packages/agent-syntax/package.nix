{ mkDerivation, base, containers, filepath, hspec, lib
, skylighting-core, text
}:
mkDerivation {
  pname = "agent-syntax";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers filepath skylighting-core text
  ];
  testHaskellDepends = [ base hspec text ];
  description = "Syntax highlighting for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
