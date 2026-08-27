{ mkDerivation, aeson, agent-process, async, base
, base64-bytestring, bytestring, containers, directory, entropy
, filepath, hspec, lib, process, safe-exceptions, scientific, text
, unix, uuid-types
}:
mkDerivation {
  pname = "claude-agent-sdk-haskell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-process async base base64-bytestring bytestring
    containers directory entropy process safe-exceptions scientific
    text unix uuid-types
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec
    safe-exceptions scientific text unix
  ];
  benchmarkHaskellDepends = [ base bytestring ];
  description = "Haskell SDK for the Claude Agent SDK stream protocol";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
