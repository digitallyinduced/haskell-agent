{ mkDerivation, base, bytestring, containers, directory, filepath
, hspec, lib, skylighting-core, text
}:
mkDerivation {
  pname = "agent-syntax";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory filepath skylighting-core text
  ];
  testHaskellDepends = [ base hspec text ];
  benchmarkHaskellDepends = [ base text ];
  description = "Syntax highlighting for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
