{ mkDerivation, aeson, agent-cli, agent-cli-runtime, agent-core
, agent-json, agent-mcp, agent-openai, agent-openrouter
, agent-repository, agent-responses-types, agent-runtime-daemon
, agent-store, agent-xai, async, base, base64-bytestring, bytestring, containers
, directory, filelock, filepath, hspec, lib, network-uri
, JuicyPixels, safe-exceptions, stdenv, stm, text, time, transformers, unix
}:
mkDerivation {
  pname = "agent-native-bridge";
  version = "0.1.0.0";
  src = ./.;
  # cabal2nix does not emit foreign-library dependencies, and generation on
  # Linux omits Darwin-only test dependencies. Supply both conditionally.
  libraryHaskellDepends = [
    agent-cli agent-core agent-json agent-mcp agent-store base
    base64-bytestring bytestring containers filepath network-uri text time transformers
  ] ++ lib.optionals stdenv.hostPlatform.isDarwin [
    aeson agent-openai agent-openrouter agent-repository
    agent-responses-types agent-runtime-daemon agent-xai async directory
    filelock JuicyPixels safe-exceptions stm
  ];
  testHaskellDepends = [
    agent-cli agent-core agent-mcp agent-runtime-daemon agent-store
    async base base64-bytestring bytestring containers directory filepath hspec
    safe-exceptions stm text unix
  ] ++ lib.optionals stdenv.hostPlatform.isDarwin [
    aeson agent-cli-runtime agent-openai agent-openrouter
    agent-repository agent-responses-types agent-xai filelock JuicyPixels time
  ];
  description = "Native host integration for the agent harness";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
