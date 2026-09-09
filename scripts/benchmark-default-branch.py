#!/usr/bin/env python3
"""Compare remote discovery with a populated Git default-branch cache.

Run: nix develop -c python3 scripts/benchmark-default-branch.py
Uses the compiled Git executable, not interpreted Haskell timings. Measures
only discovery subprocess latency, not fetching or total agent startup.
Requires a populated, current refs/remotes/<remote>/HEAD; does not modify it.
"""

import argparse
import statistics
import subprocess
import time


def measure(command):
    started = time.perf_counter()
    result = subprocess.run(command, check=True, capture_output=True, timeout=30)
    elapsed = (time.perf_counter() - started) * 1000
    return elapsed, result.stdout.decode().strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repository", default=".")
    parser.add_argument("--remote", default="origin")
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument("--repetitions", type=int, default=2)
    arguments = parser.parse_args()
    if arguments.samples < 1 or arguments.repetitions < 1:
        parser.error("sample and repetition counts must be positive")
    prefix = ["git", "-C", arguments.repository]
    commands = {
        "remote-discovery": prefix + ["ls-remote", "--symref", arguments.remote, "HEAD"],
        "local-reference": prefix + [
            "symbolic-ref", "--quiet", "--no-recurse",
            f"refs/remotes/{arguments.remote}/HEAD",
        ],
    }
    print(subprocess.check_output(["git", "--version"], text=True).strip())
    for repetition in range(arguments.repetitions):
        samples = {name: [] for name in commands}
        for sample in range(arguments.samples):
            outputs = {}
            # Alternate order to reduce systematic ordering bias.
            for name in list(commands)[::1 if sample % 2 == 0 else -1]:
                elapsed, outputs[name] = measure(commands[name])
                samples[name].append(elapsed)
            advertised = [
                line.split()[1] for line in outputs["remote-discovery"].splitlines()
                if line.startswith("ref: ") and line.endswith("\tHEAD")
            ]
            cached = outputs["local-reference"].removeprefix(
                f"refs/remotes/{arguments.remote}/"
            )
            if advertised != [f"refs/heads/{cached}"]:
                raise RuntimeError("cached and advertised default branches differ")
        for name, values in samples.items():
            print(
                f"pass {repetition + 1}: {name}: "
                f"median {statistics.median(values):.2f} ms "
                f"({arguments.samples} samples)",
                flush=True,
            )


if __name__ == "__main__":
    main()
