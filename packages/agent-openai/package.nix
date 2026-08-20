{ mkDerivation, aeson, agent-core, base, base64-bytestring
, bytestring, case-insensitive, containers, directory, exceptions
, filepath, hashable, HsOpenSSL, hspec, http-client
, http-client-tls, http-conduit, http-streams, http-types
, io-streams, lib, network-uri, retry, safe-exceptions, scientific
, temporary, text, time, unix, vector, wai, warp, websockets, wuss
}:
mkDerivation {
  pname = "agent-openai";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson agent-core base base64-bytestring bytestring containers
    directory exceptions filepath hashable HsOpenSSL http-client
    http-client-tls http-conduit http-streams http-types io-streams
    network-uri retry safe-exceptions scientific text time unix vector
    websockets wuss
  ];
  executableHaskellDepends = [ base directory filepath text ];
  testHaskellDepends = [
    aeson agent-core base base64-bytestring bytestring case-insensitive
    directory filepath hspec http-types retry temporary text time unix
    vector wai warp websockets
  ];
  description = "Haskell client for the OpenAI Responses API";
  license = lib.licenses.bsd3;
  mainProgram = "agent-openai-login";
}
