{ mkDerivation, aeson, agent-core, agent-mcp, agent-runtime
, agent-store, agent-tools, async, base, base64-bytestring
, bytestring, containers, crypton, directory, entropy, filelock
, filepath, hspec, http-client, http-types, lib, memory, network
, optparse-applicative, process, safe-exceptions, stm, temporary
, text, time, unix, uuid-types, wai, wai-extra, warp
}:
mkDerivation {
  pname = "agent-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-core agent-mcp agent-runtime agent-store agent-tools
    async base base64-bytestring bytestring containers crypton
    directory entropy filelock filepath http-types memory network
    optparse-applicative process safe-exceptions stm text time unix
    uuid-types wai warp
  ];
  executableHaskellDepends = [ agent-runtime base ];
  testHaskellDepends = [
    aeson agent-core agent-runtime agent-tools async base
    base64-bytestring bytestring containers crypton directory filepath
    hspec http-client http-types process safe-exceptions stm temporary
    text time unix wai wai-extra warp
  ];
  description = "Local HTTP API for managing haskell-agent sessions";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
