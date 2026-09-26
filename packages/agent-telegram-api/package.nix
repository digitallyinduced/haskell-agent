{ mkDerivation, aeson, agent-json, async, base, bytestring
, containers, directory, filepath, hspec, http-client
, http-client-tls, http-types, lib, retry, safe-exceptions, text
, time, unix
}:
mkDerivation {
  pname = "agent-telegram-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-json async base bytestring containers directory
    filepath http-client http-client-tls http-types retry
    safe-exceptions text time unix
  ];
  testHaskellDepends = [
    aeson agent-json async base bytestring hspec http-client http-types
    safe-exceptions text
  ];
  description = "Reusable Telegram transport and presentation components";
  license = lib.licenses.mit;
}
