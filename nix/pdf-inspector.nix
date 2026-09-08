{ lib, rustPlatform, fetchCrate, makeWrapper }:

rustPlatform.buildRustPackage rec {
  pname = "pdf-inspector";
  version = "1.18.0";

  # The published archive includes Cargo.lock (the Git checkout does not).
  # Its .cargo_vcs_info.json records revision 0d92c4c92138ab850e51937414aa241223e67e6e.
  src = fetchCrate {
    inherit pname version;
    hash = "sha256-dE5DKCQUY/rQlpXfgPFpzCyLcByRmJr+ZLubfB3lg8c=";
  };
  cargoHash = "sha256-QnxjtGcACV7V7VKNSE/r9SOZbbiBPyJB6ofrRVEiT/w=";
  # Do not enable OCR, Python bindings, model downloads, or native inference.
  buildNoDefaultFeatures = true;
  cargoBuildFlags = [ "--bin" "pdf2md" "--bin" "detect-pdf" ];
  nativeBuildInputs = [ makeWrapper ];
  # The focused CLI contract is exercised by checks.pdf-inspector below.
  doCheck = false;

  postInstall = ''
    mkdir -p "$out/share/pdf-inspector"
    cp -r external/bcmaps "$out/share/pdf-inspector/"
    for executable in pdf2md detect-pdf; do
      wrapProgram "$out/bin/$executable" \
        --set PDF_INSPECTOR_BCMAPS_DIR "$out/share/pdf-inspector/bcmaps"
    done
  '';

  meta = {
    description = "Local PDF classification and Markdown extraction without OCR";
    homepage = "https://github.com/firecrawl/pdf-inspector";
    license = lib.licenses.mit;
    mainProgram = "pdf2md";
    platforms = lib.platforms.unix;
  };
}
