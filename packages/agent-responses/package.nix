{ mkDerivation, aeson, agent-core, agent-responses-types, base, base64-bytestring
, bytestring, containers, hspec, http-client, http-client-tls
, http-conduit, lib, safe-exceptions, scientific, text, vector
}:
mkDerivation {
  pname = "agent-responses";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-responses-types base base64-bytestring bytestring containers
    http-client http-client-tls http-conduit safe-exceptions scientific
    text vector
  ];
  testHaskellDepends = [
    aeson agent-core agent-responses-types base bytestring hspec text
  ];
  description = "Provider-neutral Responses codecs and adapters";
  license = lib.licenses.bsd3;
}
