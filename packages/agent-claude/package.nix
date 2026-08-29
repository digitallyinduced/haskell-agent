{ mkDerivation, aeson, agent-core, agent-json, agent-responses
, agent-responses-types, base, bytestring, claude-agent-sdk-haskell
, containers, directory, filepath, hermes-json, hspec, lib, process
, safe-exceptions, text, unix, uuid-types
}:
mkDerivation {
  pname = "agent-claude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    base bytestring claude-agent-sdk-haskell containers directory
    filepath hermes-json process safe-exceptions text uuid-types
  ];
  testHaskellDepends = [
    agent-core agent-json agent-responses agent-responses-types base
    bytestring claude-agent-sdk-haskell directory filepath hspec
    safe-exceptions text unix
  ];
  description = "Claude Code subscription adapter for Agent.Loop";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
