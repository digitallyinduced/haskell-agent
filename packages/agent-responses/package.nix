{ mkDerivation, aeson, agent-core, agent-json
, agent-responses-types, base, base64-bytestring, bytestring
, containers, hermes-json, hspec, http-client, http-client-tls
, http-conduit, http-types, lib, QuickCheck, retry, safe-exceptions
, scientific, text, vector, wai, warp
}:
mkDerivation {
  pname = "agent-responses";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-responses-types base
    base64-bytestring bytestring containers hermes-json http-client
    http-client-tls http-conduit retry safe-exceptions scientific text
    vector
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses-types base bytestring
    containers hspec http-conduit http-types QuickCheck retry text wai
    warp
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-json agent-responses-types base bytestring
    text
  ];
  description = "Provider-neutral Responses codecs and adapters";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
