{ mkDerivation, aeson, agent-cli, agent-core, agent-store, async
, base, bytestring, containers, directory, filelock, filepath
, hspec, http-client, http-client-tls, http-types, lib, process
, retry, safe-exceptions, temporary, text, time, unix, vector
}:
mkDerivation {
  pname = "agent-telegram";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-cli agent-core agent-store async base bytestring
    containers directory filelock filepath http-client http-client-tls
    http-types process retry safe-exceptions text time unix vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-core base bytestring containers directory filepath
    hspec safe-exceptions temporary text
  ];
  benchmarkHaskellDepends = [ base filepath temporary text time ];
  description = "Telegram gateway for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "agent-telegram";
}
