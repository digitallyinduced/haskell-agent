{ mkDerivation, aeson, async, base, bytestring, containers, contravariant
, directory, filelock, filepath, hasql, hasql-pool
, hasql-transaction, hspec, lib, pqi-ffi, process, safe-exceptions
, stm, temporary, text, time, unix, uuid-types, vector
}:
mkDerivation {
  pname = "agent-store";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers contravariant directory filelock
    filepath hasql hasql-pool hasql-transaction pqi-ffi process
    safe-exceptions stm text time unix uuid-types vector
  ];
  testHaskellDepends = [
    async base bytestring hasql hspec safe-exceptions temporary text
    time
  ];
  benchmarkHaskellDepends = [
    async base containers contravariant hasql safe-exceptions temporary
    text time vector
  ];
  description = "PostgreSQL persistence for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
