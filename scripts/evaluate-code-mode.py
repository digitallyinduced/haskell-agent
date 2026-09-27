#!/usr/bin/env python3
"""Load the behavioral evaluation in GHCi using Cabal's resolved dependencies."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
from datetime import datetime, timezone

parser = argparse.ArgumentParser()
parser.add_argument("--results-dir", required=True)
parser.add_argument("--trials", type=int, default=10)
parser.add_argument("--fulfillment", action="store_true",
                    help="Include audited fulfillment alongside the four existing controls")
mode = parser.add_mutually_exclusive_group()
mode.add_argument("--typecheck", action="store_true")
mode.add_argument("--preflight", action="store_true",
                    help="Validate tool contracts and callback limits without model requests")
options = parser.parse_args()
if options.trials < 1:
    parser.error("--trials must be positive")
root = Path(__file__).resolve().parent.parent
destination = Path(options.results_dir).resolve()
if destination.exists() and any(destination.iterdir()):
    parser.error("--results-dir must be empty")
source_paths = sorted(
    [path for path in (root / "packages").rglob("*.hs") if "dist-newstyle" not in path.parts]
    + [path for package in (root / "packages").iterdir()
       for path in (package / "data").rglob("*") if path.is_file()]
    + list((root / "packages").glob("*/*.cabal"))
    + list((root / "scripts").glob("*code*evaluation*.py"))
    + list((root / "scripts").glob("CodeMode*.hs"))
    + [root / "flake.lock", root / "cabal.project"])
source_hashes = {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
                 for path in source_paths}
manifest = {
    "startedUtc": datetime.now(timezone.utc).isoformat(),
    "trialsPerTaskBackend": options.trials,
    "preflightOnly": options.preflight,
    "fulfillment": options.fulfillment,
    "taskCount": 5 if options.fulfillment else 4,
    "model": "gpt-6-sol",
    "effort": "low",
    "repairModel": "gpt-6-luna",
    "repairEffort": "low",
    "workingTreeDirty": bool(subprocess.check_output(["git", "status", "--porcelain"], cwd=root)),
    "gitRevision": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip(),
    "sourceHashes": source_hashes,
}
configuration = json.loads((root / "dist-newstyle/cache/plan.json").read_text())
plan = configuration["install-plan"]
packages = ["agent-cli", "agent-core", "agent-tools", "agent-openai", "agent-mcp", "agent-tui"]
units = [p for p in plan if p.get("pkg-name") in packages and p.get("component-name") == "lib"]
package_db = root / "dist-newstyle/packagedb" / configuration["compiler-id"]
# A registered local library depending on a source-loaded library retains the
# installed unit's type identities. Source-load that reverse dependency too;
# otherwise, for example, connectivity's ApiError differs from core's ApiError.
reachable = {p["id"] for p in units}
while True:
    expanded = reachable | {dependency for p in plan if p["id"] in reachable
                            for dependency in p.get("depends", [])}
    if expanded == reachable:
        break
    reachable = expanded
while True:
    needed = {d for p in units for d in p["depends"]}
    source_ids = {p["id"] for p in units}
    missing = [p for p in plan if p["id"] in reachable and p.get("style") == "local"
        and p not in units and (
            (p["id"] in needed and not (package_db / (p["id"] + ".conf")).exists())
            or bool(set(p.get("depends", [])) & source_ids))]
    if not missing:
        break
    units += missing
dependencies = {d for p in units for d in p["depends"]}
dependencies -= {p["id"] for p in units}
arguments = ["ghci", "-ignore-dot-ghci", "-v0", "-hide-all-packages",
    "-package-db", str(package_db)]
for store_db in (Path.home() / ".cabal/store").glob(configuration["compiler-id"] + "*/package.db"):
    arguments += ["-package-db", str(store_db)]
for dependency in sorted(dependencies):
    arguments += ["-package-id", dependency]
for unit in units:
    arguments += ["-i" + str(root / "packages" / unit["pkg-name"] / "src")]
    arguments += ["-i" + str(root / "packages" / unit["pkg-name"] / "internal")]
    for autogen in (root / "dist-newstyle/build" / (configuration["arch"] + "-" + configuration["os"])
            / configuration["compiler-id"] / (unit["pkg-name"] + "-" + unit["pkg-version"])).glob("**/build/autogen"):
        arguments += ["-i" + str(autogen)]
arguments += ["-X" + x for x in """OverloadedStrings OverloadedRecordDot
DuplicateRecordFields NoFieldSelectors LambdaCase BlockArguments RecordWildCards
NamedFieldPuns TypeApplications ScopedTypeVariables PackageImports DerivingStrategies""".split()]
arguments += ["-i" + str(root / "scripts"), str(root / "scripts/CodeModeEvaluation.hs"), "-e",
    "pure ()" if options.typecheck else
    ("CodeModeEvaluation.runFulfillmentPreflight " if options.fulfillment else "CodeModeEvaluation.runPreflight ")
        + json.dumps(str(destination)) if options.preflight else
    ("CodeModeEvaluation.runFulfillmentEvaluation " if options.fulfillment else "CodeModeEvaluation.runEvaluation ")
        + json.dumps(str(destination)) + " " + str(options.trials)]
environment = dict(os.environ)
environment["agent_tools_datadir"] = str(root / "packages/agent-tools")
with tempfile.TemporaryDirectory(prefix="code-mode-eval-loader-", dir=os.environ["TMPDIR"]) as scratch:
    data = Path(scratch) / "data"
    data.mkdir()
    for package in ["agent-tools", "agent-openai"]:
        for entry in (root / "packages" / package / "data").iterdir():
            if not (data / entry.name).exists():
                (data / entry.name).symlink_to(entry)
    status = None
    try:
        status = subprocess.run(arguments, cwd=scratch, env=environment).returncode
    finally:
        if not options.typecheck:
            destination.mkdir(parents=True, exist_ok=True)
            manifest["finishedUtc"] = datetime.now(timezone.utc).isoformat()
            manifest["exitCode"] = status
            manifest["sourceLoadedPackages"] = sorted(unit["pkg-name"] for unit in units)
            manifest["compilerId"] = configuration["compiler-id"]
            manifest["sourcesUnchanged"] = all(
                path.exists() and hashlib.sha256(path.read_bytes()).hexdigest() ==
                source_hashes[str(path.relative_to(root))] for path in source_paths)
            manifest_path = destination / "run-manifest.json"
            with manifest_path.open("x") as output:
                json.dump(manifest, output, indent=2)
    raise SystemExit(status)
