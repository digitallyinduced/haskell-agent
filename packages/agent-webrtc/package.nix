{ mkDerivation, base, bytestring, gstreamer-app, gstreamer-sdp
, gstreamer-webrtc, lib, safe-exceptions, text
}:
mkDerivation {
  pname = "agent-webrtc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring safe-exceptions text ];
  libraryPkgconfigDepends = [
    gstreamer-app gstreamer-sdp gstreamer-webrtc
  ];
  description = "Scoped WebRTC PCM audio transport";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
