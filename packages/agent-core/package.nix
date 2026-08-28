{ mkDerivation, aeson, agent-json, agent-process
, agent-responses-types, async, base, base64-bytestring, bytestring
, containers, crypton-connection, directory, filelock, filepath
, hspec, http-client, http-client-tls, http-types, lib, process
, QuickCheck, resourcet, retry, safe-exceptions, scientific, stm
, template-haskell, text, text-builder, time, tls, transformers
, unix, vector, websockets, yaml
}:
mkDerivation {
  pname = "agent-core";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson agent-json agent-process agent-responses-types async base
    base64-bytestring bytestring containers crypton-connection
    directory filelock filepath http-client http-client-tls http-types
    process resourcet retry safe-exceptions scientific stm
    template-haskell text time tls transformers unix vector websockets
    yaml
  ];
  testHaskellDepends = [
    aeson agent-json agent-responses-types async base base64-bytestring
    bytestring containers crypton-connection directory filepath hspec
    QuickCheck retry safe-exceptions stm text time tls unix websockets
    yaml
  ];
  benchmarkHaskellDepends = [
    aeson agent-json base bytestring directory filepath safe-exceptions
    text text-builder time unix
  ];
  description = "Provider-neutral infrastructure for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
