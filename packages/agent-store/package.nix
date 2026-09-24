{ mkDerivation, aeson, async, base, bytestring, containers
, contravariant, directory, filelock, filepath, hasql, hasql-pool
, hasql-transaction, hspec, lib, mtl, pqi, pqi-ffi, process
, safe-exceptions, stm, temporary, text, time, unix, uuid-types
, vector
}:
mkDerivation {
  pname = "agent-store";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers contravariant directory
    filelock filepath hasql hasql-pool hasql-transaction mtl pqi
    pqi-ffi process safe-exceptions stm text time unix uuid-types
    vector
  ];
  testHaskellDepends = [
    async base bytestring directory filelock filepath hasql hasql-pool
    hspec safe-exceptions temporary text time unix uuid-types
  ];
  benchmarkHaskellDepends = [
    async base containers contravariant hasql safe-exceptions temporary
    text time vector
  ];
  description = "PostgreSQL persistence for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
