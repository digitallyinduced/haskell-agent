{ mkDerivation, aeson, agent-core, agent-json, agent-process, async
, base, base64-bytestring, bytestring, containers, directory
, filelock, filepath, hspec, http-client, http-client-tls
, http-types, lib, network-uri, process, QuickCheck
, safe-exceptions, scientific, stm, text, time, transformers, unix
, vector
}:
mkDerivation {
  pname = "agent-mcp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-process async base
    base64-bytestring bytestring containers directory filelock filepath
    http-client http-client-tls http-types network-uri process
    safe-exceptions scientific stm text time transformers unix vector
  ];
  testHaskellDepends = [
    aeson agent-core agent-json async base bytestring containers
    directory filepath hspec QuickCheck safe-exceptions stm text time
    unix
  ];
  benchmarkHaskellDepends = [
    base bytestring directory safe-exceptions text unix
  ];
  description = "Model Context Protocol client for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
