{ mkDerivation, aeson, agent-core, agent-responses, async, base
, bytestring, containers, directory, entropy, filepath, hspec, lib
, process, safe-exceptions, text, unix, uuid-types
}:
mkDerivation {
  pname = "agent-claude-code";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-core agent-responses async base bytestring containers
    directory entropy filepath process safe-exceptions text unix
    uuid-types
  ];
  testHaskellDepends = [
    aeson agent-core agent-responses base bytestring directory filepath
    hspec process safe-exceptions text unix
  ];
  description = "Claude Code subscription subprocess backend";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
