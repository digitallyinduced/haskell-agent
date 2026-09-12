{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, crypton, crypton-connection, hspec, http-client, http-client-tls
, http-types, lib, network, safe-exceptions, split, tagsoup, text
, time, tls
}:
mkDerivation {
  pname = "agent-mail";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring crypton
    crypton-connection http-client http-client-tls http-types network
    safe-exceptions split tagsoup text time tls
  ];
  testHaskellDepends = [ aeson base hspec http-types text time ];
  description = "Low-level email types and provider transports";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
