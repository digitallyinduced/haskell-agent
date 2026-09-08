{ mkDerivation, aeson, agent-core, agent-json, agent-mcp, async
, base, bytestring, filepath, hspec, lib, safe-exceptions
, temporary, text
}:
mkDerivation {
  pname = "agent-integration-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-mcp base bytestring
    safe-exceptions text
  ];
  testHaskellDepends = [
    agent-core agent-mcp async base filepath hspec safe-exceptions
    temporary
  ];
  description = "Provider-neutral integration embedding API";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
