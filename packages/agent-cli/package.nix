{ mkDerivation, aeson, agent-core, agent-openai, agent-openrouter
, agent-xai, ansi-terminal, async, base, base64-bytestring
, bytestring, colour, containers, directory, filepath, haskeline
, hspec, lib, process, safe-exceptions, text, time, unix
}:
mkDerivation {
  pname = "agent-cli";
  version = "0.1.0.0";
  src = packages/agent-cli;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-core agent-openai agent-openrouter agent-xai
    ansi-terminal async base base64-bytestring bytestring colour
    containers directory filepath haskeline process safe-exceptions
    text time unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson agent-core agent-openai ansi-terminal base bytestring colour
    containers directory filepath haskeline hspec process
    safe-exceptions text time unix
  ];
  description = "Command-line interface for the universal agent harness";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "agent-cli";
}
