{ mkDerivation, aeson, agent-core, base, base64-bytestring
, bytestring, containers, hspec, lib, scientific, text, vector
}:
mkDerivation {
  pname = "agent-responses";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core base base64-bytestring bytestring containers
    scientific text vector
  ];
  testHaskellDepends = [ aeson agent-core base bytestring hspec text ];
  description = "Provider-neutral OpenAI Responses protocol types and adapters";
  license = lib.licenses.bsd3;
}
