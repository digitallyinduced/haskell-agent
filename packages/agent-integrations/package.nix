{ mkDerivation, aeson, agent-cli-runtime, agent-core, agent-json
, agent-mail, agent-mcp, async, base, base64-bytestring, bytestring
, containers, crypton, directory, entropy, filepath, hspec
, http-client, http-client-tls, http-types, lib, memory, network
, network-uri, safe-exceptions, temporary, text, time, unix
}:
mkDerivation {
  pname = "agent-integrations";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-cli-runtime agent-core agent-json agent-mail agent-mcp
    async base base64-bytestring bytestring containers crypton
    directory entropy filepath http-client http-client-tls http-types
    memory network safe-exceptions temporary text time unix
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-mail agent-mcp async base
    base64-bytestring bytestring directory filepath hspec http-types
    network network-uri safe-exceptions temporary text time unix
  ];
  description = "Typed in-memory integrations for Haskell Agent";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
