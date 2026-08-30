{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, containers, directory, filelock, filepath, hspec, lib, network
, safe-exceptions, stm, temporary, text, time, unix
}:
mkDerivation {
  pname = "agent-runtime-daemon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring containers directory
    filelock filepath network safe-exceptions stm text time unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson async base bytestring containers directory filepath hspec
    network safe-exceptions stm temporary text time unix
  ];
  description = "Durable local runtime daemon foundation";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "agent-runtime-daemon";
}
