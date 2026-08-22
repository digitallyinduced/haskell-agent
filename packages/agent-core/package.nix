{ mkDerivation, aeson, async, base, bytestring, containers
, directory, filepath, hspec, lib, process, resourcet, retry
, safe-exceptions, scientific, stm, text, time, transformers, unix, vector
, websockets, yaml
}:
mkDerivation {
  pname = "agent-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers directory filepath process
    resourcet retry safe-exceptions scientific stm text time transformers unix
    vector websockets yaml
  ];
  testHaskellDepends = [
    aeson async base bytestring containers directory filepath hspec
    safe-exceptions stm text time unix websockets yaml
  ];
  description = "Provider-neutral infrastructure for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
