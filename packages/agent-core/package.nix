{ mkDerivation, aeson, async, base, bytestring, containers
, directory, filepath, hspec, http-conduit, lib, process
, retry, safe-exceptions, stm, text, time, unix, vector, websockets
}:
mkDerivation {
  pname = "agent-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers directory filepath
    http-conduit process retry safe-exceptions stm text time unix vector
    websockets
  ];
  testHaskellDepends = [
    aeson base bytestring directory filepath hspec safe-exceptions text
    time unix websockets
  ];
  description = "Provider-neutral infrastructure for the agent harness";
  license = lib.licenses.bsd3;
}
