{ mkDerivation, aeson, agent-cli-runtime, agent-core, agent-json
, agent-store, async, base, bytestring, containers, directory
, filelock, filepath, hspec, http-client, http-client-tls
, http-types, lib, optparse-applicative, process, retry
, safe-exceptions, temporary, text, time, unix, vector
}:
mkDerivation {
  pname = "agent-telegram";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-cli-runtime agent-core agent-json agent-store async
    base bytestring containers directory filelock filepath http-client
    http-client-tls http-types optparse-applicative process retry
    safe-exceptions text time unix vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-core agent-json base bytestring containers directory
    filepath hspec safe-exceptions temporary text
  ];
  benchmarkHaskellDepends = [ base filepath temporary text time ];
  description = "Telegram gateway for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "agent-telegram";
}
