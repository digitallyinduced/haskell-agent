{ mkDerivation, aeson, async, base, bytestring, containers
, directory, filepath, hspec, lib, process, resourcet, retry
, safe-exceptions, stm, text, time, transformers, unix, vector
, websockets
}:
mkDerivation {
  pname = "agent-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers directory filepath process
    resourcet retry safe-exceptions stm text time transformers unix
    vector websockets
  ];
  testHaskellDepends = [
    aeson async base bytestring containers directory filepath hspec
    safe-exceptions stm text time unix websockets
  ];
  description = "Provider-neutral infrastructure for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
