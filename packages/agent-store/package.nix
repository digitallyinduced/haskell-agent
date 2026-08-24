{ mkDerivation, aeson, agent-responses-types, base, bytestring
, containers, contravariant, directory, filelock, filepath, hasql
, hasql-pool, hasql-transaction, hspec, lib, pqi-ffi, process
, safe-exceptions, temporary, text, time, unix
}:
mkDerivation {
  pname = "agent-store";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-responses-types base bytestring containers
    contravariant directory filelock filepath hasql hasql-pool
    hasql-transaction pqi-ffi process safe-exceptions text time unix
  ];
  testHaskellDepends = [
    aeson agent-responses-types base bytestring hasql hspec
    safe-exceptions temporary text time
  ];
  description = "PostgreSQL persistence for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
