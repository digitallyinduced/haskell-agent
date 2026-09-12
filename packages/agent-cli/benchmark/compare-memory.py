#!/usr/bin/env python3
"""Compare optimized transcript-scrolling binaries inside nix develop (Linux).

Keep stdout/RTS statistics and per-process peak RSS, alternating run order.
Usage: python3 compare-memory.py BASELINE CANDIDATE OUTPUT.json [REPEATS]
"""
import json
import os
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile
import time


def run(binary, workload):
    dimensions = workload if len(workload) == 4 else [*workload, "5"]
    command = [binary, *dimensions, "+RTS", "-N4", "-T", "-s", "-RTS"]
    with tempfile.TemporaryFile() as stdout, tempfile.TemporaryFile() as stderr:
        start = time.monotonic()
        process = subprocess.Popen(command, stdout=stdout, stderr=stderr)
        _, status, usage = os.wait4(process.pid, 0)
        process.returncode = os.waitstatus_to_exitcode(status)
        elapsed = time.monotonic() - start
        stdout.seek(0)
        stderr.seek(0)
        out, err = stdout.read().decode(), stderr.read().decode()
    if process.returncode:
        raise RuntimeError(f"{command}: exit {process.returncode}\n{out}\n{err}")
    return dict(command=command, peak_rss_kib=usage.ru_maxrss,
                process_wall_seconds=elapsed,
                process_cpu_seconds=usage.ru_utime + usage.ru_stime,
                stdout=out, stderr=err)


def main():
    baseline, candidate, destination, *rest = sys.argv[1:]
    repeats = int(rest[0]) if rest else 5
    if repeats < 3:
        raise ValueError("use at least three independent process runs")
    results = []
    for workload in [
        ["history-measured-viewport", "100", "3"],
        ["history-measured-viewport", "600", "3"],
        ["history-measured-viewport", "1200", "3"],
        ["history-measured-viewport-trace", "600", "3"],
        ["history-measured-viewport-trace", "100", "30"],
        ["history-measured-viewport", "100", "30"],
        ["history-measured-viewport-interaction", "100", "30"],
        # Fresh processes expose first-selection cost, before per-block fallback
        # caches have been populated by an earlier interaction sample.
        ["history-measured-viewport-interaction", "100", "30", "1"],
    ]:
        group = {"workload": workload, "baseline": [], "candidate": []}
        for repeat in range(repeats):
            order = [("baseline", baseline), ("candidate", candidate)]
            for label, binary in order[::1 if repeat % 2 == 0 else -1]:
                measurement = run(binary, workload)
                group[label].append(measurement)
                print(label, *workload, measurement["peak_rss_kib"], flush=True)
        before, after = [statistics.median(x["peak_rss_kib"] for x in group[label])
                         for label in ("baseline", "candidate")]
        group["median_peak_rss_reduction_percent"] = 100 * (1 - after / before)
        results.append(group)
        Path(destination).write_text(json.dumps(results, indent=2) + "\n")


if __name__ == "__main__":
    main()
