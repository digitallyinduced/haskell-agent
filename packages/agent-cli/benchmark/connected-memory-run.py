#!/usr/bin/env python3
"""Drive an already-prepared loopback fixture through an owned tmux session.

Samples only the PID recorded by HeapDiagnostics, not the fixture server or
PostgreSQL. Bounded waits and finally cleanup affect only this runner's pane.
"""

import argparse
import csv
import json
import os
from pathlib import Path
import shlex
import subprocess
import time
import uuid


def bounded(low, high):
    def parse(raw):
        value = int(raw)
        if not low <= value <= high:
            raise argparse.ArgumentTypeError(f"must be between {low} and {high}")
        return value
    return parse


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--turns", type=bounded(0, 1000), default=5)
    parser.add_argument("--start-turn", type=bounded(1, 1000), default=1)
    parser.add_argument("--startup-seconds", type=bounded(1, 120), default=10)
    parser.add_argument("--idle-seconds", type=bounded(1, 120), default=10)
    parser.add_argument("--turn-timeout", type=bounded(1, 600), default=120)
    parser.add_argument("--resume")
    options = parser.parse_args()
    root = options.root.resolve()
    if not root.is_relative_to(Path(os.environ["TMPDIR"]).resolve()):
        parser.error("--root must be inside TMPDIR")
    if not (root / "ready.json").exists() or not (root / "run-cli").is_file():
        parser.error("start connected-memory-fixture.py first")
    if options.start_turn + options.turns > 1001:
        parser.error("last turn must be <= 1000")
    name = "memory-" + uuid.uuid4().hex[:12]
    output = root / ("drive-" + name)
    output.mkdir()
    phase = "startup"

    def tmux(*arguments, check=True):
        return subprocess.run(["tmux", *arguments], check=check,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              text=True, timeout=10)

    def capture(label):
        result = tmux("capture-pane", "-p", "-t", name, check=False)
        (output / f"{label}.txt").write_text(result.stdout)
        return result.stdout

    def status():
        return json.loads((root / "status.json").read_text())

    with (output / "rss.csv").open("w", buffering=1) as log:
        writer = csv.writer(log)
        writer.writerow(["unix_seconds", "phase", "pid", "rss_kib", "peak_rss_kib"])

        def sample():
            heap_path = root / "heap.csv"
            if not heap_path.exists():
                return
            with heap_path.open("rb") as heap:
                heap.seek(0, 2)
                length = heap.tell()
                heap.seek(max(0, length - 4096))
                lines = heap.read().splitlines()
            for line in reversed(lines):
                fields = line.split(b",")
                if len(fields) == 18 and fields[2].isdigit():
                    pid = int(fields[2])
                    break
            else:
                return
            try:
                memory = {}
                for line in Path(f"/proc/{pid}/status").read_text().splitlines():
                    if line.startswith(("VmRSS:", "VmHWM:")):
                        key, value, _ = line.split()
                        memory[key] = int(value)
                if "VmRSS:" in memory:
                    writer.writerow([time.time(), phase, pid,
                                     memory["VmRSS:"], memory["VmHWM:"]])
            except FileNotFoundError:
                pass

        def wait_for(predicate, timeout, description):
            deadline = time.monotonic() + timeout
            while time.monotonic() < deadline:
                sample()
                if predicate():
                    return
                if (output / "exit-code").exists():
                    raise RuntimeError(f"CLI exited before {description}")
                time.sleep(0.25)
            raise TimeoutError(description)

        def idle(seconds):
            deadline = time.monotonic() + seconds
            wait_for(lambda: time.monotonic() >= deadline, seconds + 1, "idle interval")

        def send(text):
            tmux("send-keys", "-t", name, "-l", text)
            time.sleep(0.15)
            tmux("send-keys", "-t", name, "Enter")

        command = [str(root / "run-cli")]
        if options.resume:
            command += ["--resume", options.resume]
        shell = (shlex.join(command) + "; result=$?; printf '%s\\n' \"$result\" > "
                 + shlex.quote(str(output / "exit-code")))
        created = False
        try:
            tmux("new-session", "-d", "-s", name, "-x", "120", "-y", "40",
                 "-c", str(root / "cwd"), shell)
            created = True
            idle(options.startup_seconds)
            startup = capture("startup")
            if "Couldn’t start the agent" in startup:
                raise RuntimeError("CLI startup failed; see startup.txt")
            for turn in range(options.start_turn, options.start_turn + options.turns):
                phase = f"turn-{turn}"
                send(f"MEMORY_TURN_{turn}")
                wait_for(lambda: status()["completed_turn"] == turn,
                         options.turn_timeout, f"turn {turn} completion")
                # Network completion precedes final UI reduction/persistence.
                idle(1)
                def rendered():
                    pane = tmux("capture-pane", "-p", "-t", name).stdout
                    return f"MEMORY_DONE_{turn}" in pane and "✓ Finished" in pane
                wait_for(rendered, options.turn_timeout,
                         f"turn {turn} rendered completion")
                capture(f"turn-{turn}")
                fixture = json.loads((root / "ready.json").read_text())["options"]
                if fixture["tool"] == "shell-write":
                    for round_index in range(fixture["rounds"]):
                        path = root / "cwd" / f"fixture-edit-{turn}-{round_index}.txt"
                        expected = "".join(
                            f"symbol_{index:05d} = synthetic_value_{turn}_{round_index}_{index}\n"
                            for index in range(fixture["write_lines"]))
                        if not path.is_file() or path.read_text() != expected:
                            raise RuntimeError(f"tool output verification failed: {path.name}")
                phase = f"idle-{turn}"
                idle(options.idle_seconds)
            phase = "shutdown"
            capture("before-exit")
            send("/quit")
            wait_for(lambda: (output / "exit-code").exists(), 30, "graceful exit")
            exit_code = int((output / "exit-code").read_text())
            if exit_code != 0:
                raise RuntimeError(f"CLI exit code {exit_code}")
            (output / "result.json").write_text(json.dumps({
                "status": "completed", "turns": options.turns,
                "start_turn": options.start_turn, "fixture": status(),
                "resumed": bool(options.resume)}) + "\n")
            print(json.dumps({"output": str(output), "status": "completed"}))
        finally:
            if created and tmux("has-session", "-t", name, check=False).returncode == 0:
                capture("failure")
                # First request cancellation/exit, then close this owned pane.
                tmux("send-keys", "-t", name, "C-c", check=False)
                tmux("kill-session", "-t", name, check=False)


if __name__ == "__main__":
    main()
