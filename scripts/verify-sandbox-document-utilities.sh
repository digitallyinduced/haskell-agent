#!/usr/bin/env bash
# Run inside the sandbox, not on its host. Uses only disposable test documents.
set -euo pipefail

for utility in python3 zip unzip pdfinfo pdftotext pdftoppm qpdf jq; do
    command -v "$utility" >/dev/null
done

directory=$(mktemp -d "${TMPDIR:?}/document-utilities.XXXXXXXX")
trap 'rm -rf -- "$directory"' EXIT

python3 - "$directory" <<'PY'
import pathlib
import subprocess
import sys
import zipfile

root = pathlib.Path(sys.argv[1])
original = b"Document processing verification\n"
(root / "original.txt").write_bytes(original)
subprocess.run(["zip", "-q", "source.zip", "original.txt"], cwd=root, check=True)
with zipfile.ZipFile(root / "source.zip") as source:
    assert source.testzip() is None
    with zipfile.ZipFile(root / "classified.zip", "w", zipfile.ZIP_DEFLATED) as target:
        target.writestr("Documents/original.txt", source.read("original.txt"))
subprocess.run(["unzip", "-t", "classified.zip"], cwd=root, check=True)
with zipfile.ZipFile(root / "classified.zip") as result:
    assert result.namelist() == ["Documents/original.txt"]
    assert result.read("Documents/original.txt") == original
subprocess.run(["qpdf", "--empty", str(root / "empty.pdf")], check=True)
subprocess.run(["qpdf", "--check", str(root / "empty.pdf")], check=True)
PY

printf '%s\n' 'Sandbox document utilities verified.'
