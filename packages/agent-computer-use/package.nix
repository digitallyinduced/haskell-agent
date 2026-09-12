{ mkDerivation, aeson, agent-core, agent-json
, agent-responses-types, async, base, base64-bytestring, bytestring
, containers, dbus, directory, entropy, filepath, hspec
, JuicyPixels, lib, process, safe-exceptions, stm, text, unix
, vector
}:
mkDerivation {
  pname = "agent-computer-use";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-responses-types async base
    base64-bytestring bytestring containers dbus directory entropy
    filepath JuicyPixels process safe-exceptions stm text unix vector
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses-types async base
    base64-bytestring bytestring containers dbus directory entropy
    filepath hspec JuicyPixels process safe-exceptions stm text unix
    vector
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-json agent-responses-types async base
    base64-bytestring bytestring containers dbus directory entropy
    filepath JuicyPixels process safe-exceptions stm text unix vector
  ];
  description = "Frontend-independent desktop capture and computer-use tools";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
