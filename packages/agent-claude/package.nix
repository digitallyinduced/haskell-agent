{ mkDerivation, aeson, agent-core, agent-json, agent-process
, agent-responses, agent-responses-types, async, base
, base64-bytestring, bytestring, claude-agent-sdk-haskell
, containers, directory, filepath, hermes-json, hspec, lib, process
, safe-exceptions, text, time, unix, uuid-types
}:
mkDerivation {
  pname = "agent-claude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-json agent-process agent-responses
    agent-responses-types async base base64-bytestring bytestring
    claude-agent-sdk-haskell containers directory filepath hermes-json
    process safe-exceptions text time uuid-types
  ];
  testHaskellDepends = [
    aeson agent-core agent-json agent-responses agent-responses-types
    async base bytestring claude-agent-sdk-haskell directory filepath
    hspec safe-exceptions text time unix
  ];
  description = "Claude Code subscription adapter for Agent.Loop";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
