{ mkDerivation, aeson, agent-json, agent-server-client
, agent-telegram-api, async, base, bytestring, directory, filepath
, hspec, lib, postgresql-simple, process, safe-exceptions
, temporary, text, time
}:
mkDerivation {
  pname = "agent-telegram-connector";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson agent-json agent-server-client agent-telegram-api async base
    bytestring postgresql-simple safe-exceptions text time
  ];
  testHaskellDepends = [
    aeson agent-json agent-server-client agent-telegram-api base
    bytestring directory filepath hspec postgresql-simple process
    safe-exceptions temporary text time
  ];
  description = "Durable Telegram conversation and agent session connector";
  license = lib.licenses.mit;
}
