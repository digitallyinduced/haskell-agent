#!/usr/bin/env python3
"""Compare shell and compiled launchers around the same optimized CLI."""

import argparse
import json
import os
from pathlib import Path
import statistics
import subprocess
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("comparison", type=Path,
                        help="output of nix/benchmarks/cli-wrapper.nix")
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if args.samples < 1 or args.warmups < 0:
        parser.error("provide positive samples and nonnegative warmups")
    launchers = {name: args.comparison.resolve() / name / "bin/agent-cli"
                 for name in ("shell", "native")}
    environments = [
        (str(count), dict(os.environ, PATH=":".join(
            f"/nix/store/{index:032x}-startup-command/bin"
            for index in range(count))))
        for count in (1, 64, 128, 256)
    ]
    environments.extend([
        ("inherited", dict(os.environ)),
        ("256-repeat", environments[-1][1]),
    ])
    rows = []
    expected_output = None
    for workload, environment in environments:
        for index in range(-args.warmups, args.samples):
            order = ("shell", "native") if index % 2 == 0 else ("native", "shell")
            for name in order:
                started = time.monotonic_ns()
                result = subprocess.run(
                    [str(launchers[name]), "--version"], env=environment,
                    check=True, capture_output=True, timeout=30)
                elapsed = (time.monotonic_ns() - started) / 1_000_000
                if expected_output is None:
                    expected_output = result.stdout
                if result.stdout != expected_output:
                    raise RuntimeError("launchers returned different version output")
                if index >= 0:
                    rows.append(dict(workload=workload, launcher=name,
                                     sample=index, elapsed_ms=elapsed,
                                     path_bytes=len(environment["PATH"])))
        for name in order:
            samples = [row for row in rows
                       if row["workload"] == workload and row["launcher"] == name]
            print(json.dumps(dict(workload=workload, launcher=name,
                                  path_bytes=samples[0]["path_bytes"],
                                  median_ms=statistics.median(
                                      row["elapsed_ms"] for row in samples))), flush=True)
    if args.output:
        args.output.write_text(json.dumps(rows, indent=2) + "\n")


if __name__ == "__main__":
    main()
