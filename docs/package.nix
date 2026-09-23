{ mkDerivation, base, blaze-html, bytestring, containers, directory
, filepath, hspec, http-types, ihp-hsx, lib, tagsoup, text, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "haskell-agent-documentation";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base blaze-html bytestring containers filepath http-types ihp-hsx
    tagsoup text wai
  ];
  executableHaskellDepends = [ base warp ];
  testHaskellDepends = [
    base blaze-html bytestring directory hspec http-types tagsoup text
    wai wai-extra
  ];
  description = "Self-hosted Haskell Agent documentation";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "documentation-server";
}
