{ mkDerivation, aeson, agent-core, agent-responses-types, base
, base64-bytestring, bytestring, containers, hspec, http-client
, http-client-tls, http-conduit, lib, QuickCheck, retry
, safe-exceptions, scientific, text, vector
}:
mkDerivation {
  pname = "agent-responses";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-responses-types base base64-bytestring
    bytestring containers http-client http-client-tls http-conduit
    retry safe-exceptions scientific text vector
  ];
  testHaskellDepends = [
    aeson agent-core agent-responses-types base bytestring containers
    hspec QuickCheck retry text
  ];
  benchmarkHaskellDepends = [
    aeson agent-core agent-responses-types base bytestring containers
    text
  ];
  description = "Provider-neutral Responses codecs and adapters";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
