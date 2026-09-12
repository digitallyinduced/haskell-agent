#!/usr/bin/env python3
"""Build working journal against the frozen pre-admission-dedup journal."""
import os
from pathlib import Path
import subprocess

out = Path(os.environ["TMPDIR"]) / "journal-admission-bench"
out.mkdir(parents=True, exist_ok=True)
baseline = subprocess.check_output(["git", "show",
    "f37672e5:packages/agent-core/src/Agent/Loop/DisplayJournal.hs"], text=True)
(out / "JournalBefore.hs").write_text(baseline.replace(
    "module Agent.Loop.DisplayJournal", "module JournalBefore"))
extensions = ["OverloadedStrings", "OverloadedRecordDot", "DuplicateRecordFields",
    "NoFieldSelectors", "LambdaCase", "BlockArguments", "DeriveGeneric",
    "RecordWildCards", "NamedFieldPuns", "NumericUnderscores", "TypeApplications",
    "ScopedTypeVariables", "DerivingStrategies", "GeneralizedNewtypeDeriving",
    "DataKinds", "FlexibleContexts", "TupleSections", "PackageImports"]
packages = ["base", "aeson", "bytestring", "text", "containers", "async", "stm",
    "transformers", "time", "vector", "scientific", "hermes-json",
    "safe-exceptions", "deepseq"]
command = ["ghc", "-O2", "-threaded", "-rtsopts", "-hide-all-packages",
    *["-X" + e for e in extensions],
    *[x for p in packages for x in ["-package", p]],
    *["-ipackages/" + p + "/src" for p in
      ["agent-core", "agent-responses-types", "agent-json"]],
    "-i" + str(out), "-outputdir", str(out)]
subprocess.run([*command, "packages/agent-core/benchmark/JournalAdmission.hs",
    "-o", str(out / "bench")], check=True)
# The existing exhaustive mixed-prefix harness compares against the older
# independently written list baseline, including duplicate starts/finishes.
subprocess.run([*command, "-ipackages/agent-core/benchmark",
    "packages/agent-core/benchmark/DisplayJournal.hs",
    "-o", str(out / "verify")], check=True)
subprocess.run([str(out / "verify"), "--verify", "+RTS", "-T"], check=True)
print(out / "bench")
