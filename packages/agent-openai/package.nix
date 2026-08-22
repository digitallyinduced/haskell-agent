{ mkDerivation, aeson, agent-core, agent-responses, base, base64-bytestring
, bytestring, case-insensitive, containers, directory, exceptions
, filepath, HsOpenSSL, hspec, http-client
, http-conduit, http-streams, http-types
, io-streams, lib, network-uri, retry, safe-exceptions
, temporary, text, time, unix, vector, wai, warp, websockets, wuss
}:
mkDerivation {
  pname = "agent-openai";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-core agent-responses base base64-bytestring bytestring containers
    directory exceptions filepath HsOpenSSL http-client http-conduit http-streams
    io-streams network-uri retry safe-exceptions text time vector
    websockets wuss
  ];
  executableHaskellDepends = [
    agent-core base directory filepath text
  ];
  testHaskellDepends = [
    aeson agent-core agent-responses base base64-bytestring bytestring case-insensitive
    directory filepath hspec http-types retry temporary text time unix
    vector wai warp websockets
  ];
  description = "Haskell client for the OpenAI Responses API";
  license = lib.licenses.bsd3;
  mainProgram = "agent-openai-login";
}
