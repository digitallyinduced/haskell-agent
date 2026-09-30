#!/usr/bin/env python3
"""Compare optimized GatewayCatalogProbe binaries; run inside nix develop."""
import argparse
import json
import math
import statistics
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before")
    parser.add_argument("after")
    parser.add_argument("--samples", type=int, default=7)
    args = parser.parse_args()
    if args.samples < 3:
        parser.error("Use at least three samples")
    results = {"before": [], "after": []}
    checksums = set()
    for sample in range(args.samples):
        order = ("before", "after") if sample % 2 == 0 else ("after", "before")
        for mode in order:
            process = subprocess.run(
                [getattr(args, mode)], capture_output=True, text=True, timeout=15)
            if process.returncode:
                raise SystemExit(f"{mode} failed; diagnostic output suppressed")
            row = json.loads(process.stdout)
            elapsed = row["total_ms"]
            if isinstance(elapsed, bool) or not math.isfinite(elapsed) or elapsed < 0:
                raise SystemExit("Invalid timing")
            checksums.add(row["checksum"])
            results[mode].append(elapsed)
            print(json.dumps({"mode": mode, "sample": sample, **row}), flush=True)
    if len(checksums) != 1:
        raise SystemExit("Catalog sizes changed; comparison rejected")
    print(json.dumps({"median_ms": {
        mode: statistics.median(rows) for mode, rows in results.items()}}))


if __name__ == "__main__":
    main()
