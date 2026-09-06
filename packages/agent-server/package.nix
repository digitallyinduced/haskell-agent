{ mkDerivation, aeson, agent-cli, agent-cli-runtime, agent-core
, agent-store, async, base, base64-bytestring, bytestring
, containers, crypton, directory, filepath, hspec, http-types, lib
, memory, optparse-applicative, process, safe-exceptions, stm
, temporary, text, time, unix, uuid-types, wai, wai-extra, warp
}:
mkDerivation {
  pname = "agent-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-cli agent-cli-runtime agent-core agent-store async base
    base64-bytestring bytestring containers crypton directory filepath
    http-types memory optparse-applicative process safe-exceptions stm
    text time unix uuid-types wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-core async base bytestring containers directory
    filepath hspec http-types safe-exceptions stm temporary text time
    unix wai wai-extra
  ];
  description = "Local HTTP API for managing haskell-agent sessions";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
