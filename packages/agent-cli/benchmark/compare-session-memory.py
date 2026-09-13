#!/usr/bin/env python3
"""Linux replay comparison: OLD NEW WORKLOADS.json RESULTS.json [REPEATS].

Workloads are argument arrays, without binary or RTS flags. Use one internal
sample per process. Command paths are recorded; transcript bodies are not.
"""
import json
import os
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile
import time


def run(binary, args):
    command = [binary, *args, "+RTS", "-N4", "-T", "-s", "-RTS"]
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
        raise RuntimeError(f"replay exited {process.returncode}: {err}")
    phases = {}
    for line in out.splitlines():
        words = line.split()
        if words and any(word.startswith("live_bytes=") for word in words):
            phases[words[0]] = {key: float(value) for key, value in
                               (word.split("=", 1) for word in words[1:])}
    if not phases:
        raise RuntimeError("replay produced no memory measurements")
    return dict(command=command, peak_rss_kib=usage.ru_maxrss,
                process_wall_seconds=elapsed,
                process_cpu_seconds=usage.ru_utime + usage.ru_stime,
                phases=phases, stdout=out, stderr=err)


def main():
    old, new, workloads_path, destination, *rest = sys.argv[1:]
    repeats = int(rest[0]) if rest else 5
    if repeats < 3:
        raise ValueError("use at least three independent process runs")
    workloads = json.loads(Path(workloads_path).read_text())
    if not isinstance(workloads, list) or not all(
        isinstance(args, list) and args and all(isinstance(x, str) for x in args)
        and "verify" not in args for args in workloads
    ):
        raise ValueError("expected argument arrays without verify")
    results = []
    for args in workloads:
        group = dict(workload=args, baseline=[], candidate=[])
        for repeat in range(repeats):
            order = [("baseline", old), ("candidate", new)]
            for label, binary in order[::1 if repeat % 2 == 0 else -1]:
                result = run(binary, args)
                group[label].append(result)
                print(label, "workload", len(results), result["peak_rss_kib"], flush=True)
        before, after = [statistics.median(x["peak_rss_kib"] for x in group[label])
                         for label in ("baseline", "candidate")]
        group["median_peak_rss_reduction_percent"] = 100 * (1 - after / before)
        results.append(group)
        Path(destination).write_text(json.dumps(results, indent=2) + "\n")


if __name__ == "__main__":
    main()
