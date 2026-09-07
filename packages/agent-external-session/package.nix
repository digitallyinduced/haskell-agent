{ mkDerivation, aeson, agent-core, agent-json, async, base
, bytestring, containers, crypton, direct-sqlite, directory
, filepath, hspec, lib, network-uri, process, safe-exceptions
, scientific, text, time, unix, vector
}:
mkDerivation {
  pname = "agent-external-session";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json async base bytestring containers
    crypton direct-sqlite directory filepath network-uri process
    safe-exceptions scientific text time unix vector
  ];
  testHaskellDepends = [
    aeson agent-core base bytestring direct-sqlite directory filepath
    hspec process safe-exceptions text
  ];
  description = "Import sessions from external agent harnesses";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
