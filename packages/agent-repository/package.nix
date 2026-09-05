{ mkDerivation, aeson, async, base, bytestring, containers, crypton
, directory, filelock, filepath, hspec, lib, process
, safe-exceptions, text, time, transformers, unix
}:
mkDerivation {
  pname = "agent-repository";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers crypton directory filelock
    filepath process safe-exceptions text time transformers unix
  ];
  testHaskellDepends = [
    async base bytestring directory filepath hspec process
    safe-exceptions text unix
  ];
  description = "Repository review and delivery operations";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
