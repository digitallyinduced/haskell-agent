{ mkDerivation, aeson, agent-core, agent-json, agent-process
, agent-responses-types, async, base, base64-bytestring, bytestring
, containers, directory, filepath, hspec, JuicyPixels, lib, process
, QuickCheck, safe-exceptions, scientific, stm, template-haskell
, text, time, transformers, unix, vector
}:
mkDerivation {
  pname = "agent-tools";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-process async base
    base64-bytestring bytestring containers directory filepath
    JuicyPixels process safe-exceptions scientific stm template-haskell
    text time transformers unix vector
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses-types async base
    base64-bytestring bytestring containers directory filepath hspec
    JuicyPixels process QuickCheck safe-exceptions stm text time unix
  ];
  benchmarkHaskellDepends = [
    aeson agent-core async base bytestring directory filepath process
    safe-exceptions text time unix
  ];
  description = "Concrete local tools for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
