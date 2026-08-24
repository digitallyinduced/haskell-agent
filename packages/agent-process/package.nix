{ mkDerivation, base, lib, process, safe-exceptions, unix }:
mkDerivation {
  pname = "agent-process";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base process safe-exceptions unix ];
  description = "Shared process lifecycle utilities for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
