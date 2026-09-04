{ mkDerivation, aeson, agent-json, agent-process, async, base
, base64-bytestring, bytestring, containers, directory, entropy
, filepath, hermes-json, hspec, lib, process, safe-exceptions, text
, transformers, unix, uuid-types
}:
mkDerivation {
  pname = "claude-agent-sdk-haskell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-json agent-process async base base64-bytestring
    bytestring containers directory entropy hermes-json process
    safe-exceptions text transformers unix uuid-types
  ];
  testHaskellDepends = [
    aeson agent-json async base bytestring containers directory
    filepath hspec safe-exceptions text unix
  ];
  benchmarkHaskellDepends = [
    async base bytestring safe-exceptions
  ];
  description = "Haskell SDK for the Claude Agent SDK stream protocol";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
