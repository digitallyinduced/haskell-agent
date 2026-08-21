{ mkDerivation, aeson, agent-core, base, base64-bytestring
, bytestring, containers, lib, scientific, text, vector
}:
mkDerivation {
  pname = "agent-responses";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core base base64-bytestring bytestring containers
    scientific text vector
  ];
  description = "Provider-neutral OpenAI Responses protocol types and adapters";
  license = lib.licenses.bsd3;
}
