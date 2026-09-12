#!/usr/bin/env python3
"""Repeated paired medians; build first with journal-admission-build.py."""
import csv
import io
import os
from pathlib import Path
import statistics
import subprocess

root = Path(os.environ["TMPDIR"]) / "journal-admission-bench"
print("round,shape,count,size,variant,cpu_ms,wall_ms,allocated_bytes,live_bytes")
for repeat in (1, 2):
    cases = [(shape, n, 256) for shape in ("arguments", "interleaved") for n in (32, 128, 256)]
    cases += [("text", n, 16) for n in (1000, 10000)]
    for shape, count, size in cases:
        raw = subprocess.check_output([str(root / "bench"), "all", shape,
            str(count), str(size), "11", "+RTS", "-T", "-N1"], text=True)
        (root / f"final-{repeat}-{shape}-{count}.csv").write_text(raw)
        rows = list(csv.DictReader(io.StringIO(raw)))
        for mode in ("old", "new"):
            selected = [r for r in rows if r["variant"] == mode]
            medians = [statistics.median(float(r[key]) for r in selected)
                for key in ("cpu_ms", "wall_ms", "allocated_bytes", "live_bytes")]
            print(",".join(map(str, [repeat, shape, count, size, mode, *medians])), flush=True)
