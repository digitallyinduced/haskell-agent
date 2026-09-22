"""Build current product decoders and validate an exported example manifest.

From the repository root, after exporting examples:
  runtime=$(nix build .#agent-runtime --no-link --print-out-paths)
  nix develop -c env TMPDIR="$TMPDIR" python3 docs/scripts/verify_configuration_examples.py \
    --runtime-package "$runtime" --examples "$TMPDIR/documentation-examples"

The supplied runtime package must use this flake's GHC/dependency versions.
Its closure supplies dependencies and generated Paths metadata only: the
decoder modules themselves compile from the current working tree. This works
with static-only Nix libraries where GHCi cannot load the cached dependencies.
No global package database or Nix-store file is modified.
"""

import argparse
import os
from pathlib import Path
import re
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[2]


def output(*command):
    return subprocess.check_output(command, text=True, cwd=ROOT).strip()


def run(*command):
    subprocess.run(command, check=True, cwd=ROOT)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime-package", required=True, type=Path)
    parser.add_argument("--examples", required=True, type=Path)
    args = parser.parse_args()
    version = output("ghc", "--numeric-version")
    examples = args.examples.resolve()
    if not (examples / "manifest.json").is_file():
        parser.error("--examples must contain the exported manifest.json")
    closure = output("nix-store", "-qR", str(args.runtime_package.resolve())).splitlines()
    with tempfile.TemporaryDirectory(prefix="documentation-decoders-", dir=os.environ["TMPDIR"]) as temporary:
        build = Path(temporary)
        database = build / "package.conf.d"
        database.mkdir()
        for item in closure:
            package = Path(item)
            # Keep the active compiler's wired-in packages, not copies from
            # a closure compiler with different library directory metadata.
            if re.search(r"-ghc-[0-9]", package.name):
                continue
            source = package / "lib" / ("ghc-" + version) / "lib" / "package.conf.d"
            for conf in source.glob("*.conf"):
                contents = conf.read_text()
                if conf.name.startswith("agent-runtime-"):
                    # Expose only Cabal-generated path metadata in this private
                    # copy; current runtime source still shadows cached modules.
                    contents = re.sub(r"\bPaths_agent_runtime\b", "", contents)
                    contents = contents.replace("exposed-modules:", "exposed-modules:\n    Paths_agent_runtime", 1)
                destination = database / conf.name
                if destination.exists() and destination.read_text() != contents:
                    raise RuntimeError("Conflicting package registration: " + conf.name)
                destination.write_text(contents)
        if not list(database.glob("agent-runtime-*.conf")):
            raise RuntimeError("Runtime package registration not found for active GHC " + version)
        run("ghc-pkg", "recache", "--package-db", str(database))
        executable = build / "verify"
        extensions = [
            "GHC2021", "BlockArguments", "OverloadedStrings", "OverloadedRecordDot",
            "DuplicateRecordFields", "NoFieldSelectors", "LambdaCase",
            "RecordWildCards", "TypeApplications",
        ]
        run("ghc", "--make", "-O0", "-outputdir", str(build), "-o", str(executable),
            "-main-is", "VerifyConfigurationExamples", "-package-db", str(database),
            "-package", "agent-runtime (Paths_agent_runtime)",
            "-ipackages/agent-runtime/src", *["-X" + value for value in extensions],
            "docs/scripts/VerifyConfigurationExamples.hs")
        run(str(executable), "--self-test")
        run(str(executable), str(examples))


if __name__ == "__main__":
    main()
