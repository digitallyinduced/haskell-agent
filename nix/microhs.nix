{ lib, stdenv, fetchFromGitHub, makeWrapper, yyjson, nativeJson ? false }:

stdenv.mkDerivation {
  pname = if nativeJson then "microhs-native-json" else "microhs";
  version = "0.16.6.0-unstable-2026-09-12";

  src = fetchFromGitHub {
    owner = "augustss";
    repo = "MicroHs";
    rev = "455782164e75998b140d869c1b7cdde0c8a21508";
    hash = "sha256-xVztgPVjlYlvxpFDq2olQrkWb1fTfzzfMJb+1fhn0hE=";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = lib.optional nativeJson yyjson;
  postPatch = lib.optionalString nativeJson ''
    cp ${../experiments/microhs-code-mode/native/native_json.c} generated/native_json.c
    cp ${../experiments/microhs-code-mode/native/native_json_entries.h} generated/native_json_entries.h
    substituteInPlace generated/mhs.c \
      --replace-fail 'static const struct ffi_entry imp_table[] = {' \
      '#include "native_json.c"
static const struct ffi_entry imp_table[] = {
#include "native_json_entries.h"'
  '';
  makeFlags = lib.optional nativeJson "MHSGMPCCLIBS=-lyyjson";
  # Bootstrap the upstream combinator image with the C runtime, not GHC.
  buildFlags = [ "bin/mhs" "bin/cpphs" ];
  installPhase = ''
    runHook preInstall
    make oldinstall PREFIX="$out"
    cp generated/base.pkg "$out/lib/mhs/base.pkg"
    wrapProgram "$out/bin/mhs" \
      --set MHSDIR "$out/lib/mhs" \
      --add-flags "-p$out/lib/mhs/base.pkg" \
      --prefix PATH : "$out/bin"
    ${lib.optionalString nativeJson ''mv "$out/bin/mhs" "$out/bin/mhs-native-json"''}
    runHook postInstall
  '';

  meta = {
    description = "Small Haskell implementation with a combinator runtime";
    homepage = "https://github.com/augustss/MicroHs";
    license = lib.licenses.asl20;
    platforms = lib.platforms.unix;
    mainProgram = if nativeJson then "mhs-native-json" else "mhs";
  };
}
