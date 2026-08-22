{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, containers, crypton-connection, directory, filepath, hspec, lib
, process, resourcet, retry, safe-exceptions, scientific, stm, text
, text-builder, time, tls, transformers, unix, vector, websockets
, yaml
}:
mkDerivation {
  pname = "agent-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring containers
    crypton-connection directory filepath process resourcet retry
    safe-exceptions scientific stm text time tls transformers unix
    vector websockets yaml
  ];
  testHaskellDepends = [
    aeson async base base64-bytestring bytestring containers
    crypton-connection directory filepath hspec retry safe-exceptions
    stm text time tls unix websockets yaml
  ];
  benchmarkHaskellDepends = [ base text text-builder ];
  description = "Provider-neutral infrastructure for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
