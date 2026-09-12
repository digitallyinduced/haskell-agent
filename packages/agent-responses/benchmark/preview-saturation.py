#!/usr/bin/env python3
"""Run in nix develop with TMPDIR preserved; builds optimized real modules."""
import csv
import io
import json
import os
from pathlib import Path
import statistics
import subprocess

out = Path(os.environ["TMPDIR"]) / "preview-saturation"
out.mkdir(parents=True, exist_ok=True)
revision = "f37672e5"
source = subprocess.check_output(["git", "show",
    f"{revision}:packages/agent-responses/src/Agent/Responses/LoopBackend/ToolArgumentPreview.hs"], text=True)
(out / "BaselinePreview.hs").write_text(source.replace(
    "module Agent.Responses.LoopBackend.ToolArgumentPreview", "module BaselinePreview"))
extensions = ["OverloadedStrings", "OverloadedRecordDot", "DuplicateRecordFields",
    "NoFieldSelectors", "LambdaCase", "BlockArguments", "DeriveGeneric",
    "RecordWildCards", "NamedFieldPuns", "NumericUnderscores", "TypeApplications",
    "ScopedTypeVariables", "DerivingStrategies", "GeneralizedNewtypeDeriving",
    "DataKinds", "FlexibleContexts", "TupleSections", "PackageImports"]
plan = json.loads(Path("dist-newstyle/cache/plan.json").read_text())
package_ids = {p["pkg-name"]: p["id"] for p in plan["install-plan"]}
environment = subprocess.check_output(
    ["cabal", "exec", "--", "sh", "-c", 'cat "$GHC_ENVIRONMENT"'], text=True)
env_path = out / "package.environment"
env_path.write_text("\n".join(line for line in environment.splitlines()
    if line.startswith(("clear-package-db", "global-package-db", "package-db "))) + "\n")
subprocess.run(["ghc", "-package-env", str(env_path), "-O2", "-threaded", "-rtsopts", "-hide-all-packages",
    *["-X" + e for e in extensions],
    *[x for p in ["base", "text", "containers", "aeson", "bytestring", "hermes-json"]
      for x in ["-package-id", package_ids[p]]],
    "-package", "agent-core", "-package", "agent-json",
    "-package", "agent-responses-types",
    "-ipackages/agent-responses/src", "-i" + str(out),
    "-outputdir", str(out), "packages/agent-responses/benchmark/PreviewSaturation.hs",
    "-o", str(out / "bench")], check=True)
print("tool,deltas,variant,cpu_ms,wall_ms,allocated_bytes", flush=True)
for index, (tool, count, reps) in enumerate([
        ("read_file", 100, 20), ("read_file", 1000, 10),
        ("read_file", 10000, 3), ("read_file", 50000, 1),
        ("shell_command", 100, 20), ("shell_command", 1000, 10),
        ("shell_command", 10000, 3), ("shell_command", 50000, 1),
        ("read_file", 10000, 3), ("shell_command", 10000, 3)]):
    raw = subprocess.check_output([str(out / "bench"), tool, str(count),
        str(reps), "7", "+RTS", "-T", "-N1"], text=True)
    (out / f"run-{index}.csv").write_text(raw)
    rows = list(csv.DictReader(io.StringIO(raw)))
    for variant in ["old", "new"]:
        selected = [row for row in rows if row["variant"] == variant]
        values = [statistics.median(float(row[key]) for row in selected)
            for key in ["cpu_ms", "wall_ms", "allocated_bytes"]]
        print(",".join(map(str, [tool, count, variant, *values])), flush=True)
