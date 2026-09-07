#!/usr/bin/env python3
"""Measure a compiled agent's startup through a real PTY, without sending a turn."""

import argparse
import errno
import fcntl
import json
import os
from pathlib import Path
import pty
import re
import select
import signal
import statistics
import struct
import subprocess
import termios
import time


ANSI = re.compile(rb"\x1b(?:\][^\x07\x1b]*(?:\x07|\x1b\\)|\[[0-?]*[ -/]*[@-~]|[()][A-Z0-9])")
READY = re.compile(rb"(?:startup: |\xc2\xb7\s*)ready ([0-9.]+)(ms|s)\b")
FRAME_END = b"\x1b[?25h"
PROBE = b"startup-probe"


def sample(args, index):
    env = dict(os.environ, HOME=str(args.home), TERM="xterm-256color",
               HASKELL_AGENT_STARTUP_TIMING="1")
    for key, directory in (
        ("XDG_CONFIG_HOME", ".config"),
        ("XDG_CACHE_HOME", ".cache"),
        ("XDG_DATA_HOME", ".local/share"),
        ("XDG_STATE_HOME", ".local/state"),
    ):
        env[key] = str(args.home / directory)
    # Do not inherit the outer harness's terminal/session identity.
    for key in ("TMUX", "STY"):
        env.pop(key, None)
    if args.cold_store:
        subprocess.run([args.command[0], "storage", "stop"], env=env,
                       cwd=args.cwd, check=True, capture_output=True,
                       timeout=args.timeout)
    started = time.monotonic()
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(args.cwd)
        os.execvpe(args.command[0], args.command, env)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))
    output = bytearray()
    result = {}
    probe_sent = False
    try:
        while time.monotonic() - started < args.timeout:
            readable, _, _ = select.select([fd], [], [], 0.05)
            if not readable:
                continue
            try:
                chunk = os.read(fd, 65536)
            except OSError as exc:
                if exc.errno == errno.EIO:
                    break
                raise
            if not chunk:
                break
            output.extend(chunk)
            elapsed = (time.monotonic() - started) * 1000
            if FRAME_END in output and "first_frame_ms" not in result:
                result["first_frame_ms"] = elapsed
            # Respond to cursor-position queries as a terminal would.
            if b"\x1b[6n" in chunk:
                os.write(fd, b"\x1b[1;1R")
            if ("first_frame_ms" in result and not probe_sent
                    and not termios.tcgetattr(fd)[3] & termios.ECHO):
                os.write(fd, PROBE)
                probe_sent = True
            plain = ANSI.sub(b"", bytes(output))
            if probe_sent and PROBE in plain and "editable_ms" not in result:
                result["editable_ms"] = elapsed
            ready = READY.search(plain)
            if ready and "ready_ms" not in result:
                result["ready_ms"] = elapsed
                result["internal_ready_ms"] = (
                    float(ready[1]) * (1000 if ready[2] == b"s" else 1))
            if all(key in result for key in
                   ("first_frame_ms", "editable_ms", "ready_ms")):
                break
        if args.output_dir:
            args.output_dir.mkdir(parents=True, exist_ok=True)
            (args.output_dir / f"sample-{index}.tty").write_bytes(output)
        missing = {"first_frame_ms", "editable_ms", "ready_ms"} - result.keys()
        if missing:
            raise RuntimeError(
                f"sample {index}: missing {sorted(missing)}; "
                f"last output: {ANSI.sub(b'', bytes(output[-3000:]))!r}")
        return result
    finally:
        # Clear the unsubmitted probe and request a normal exit, allowing scoped
        # workers and persistence to close. Drain output so terminal cleanup
        # cannot block on a full PTY buffer.
        try:
            os.write(fd, b"\x15\x04")
        except OSError:
            pass
        deadline = time.monotonic() + 3
        while time.monotonic() < deadline:
            waited, _ = os.waitpid(pid, os.WNOHANG)
            if waited:
                break
            readable, _, _ = select.select([fd], [], [], 0.02)
            if readable:
                try:
                    os.read(fd, 65536)
                except OSError:
                    pass
        else:
            try:
                os.kill(pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            os.waitpid(pid, 0)
        os.close(fd)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--home", type=Path, required=True,
                        help="dedicated pre-initialized home; do not use your real HOME")
    parser.add_argument("--cwd", type=Path, default=Path.cwd())
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--cold-store", action="store_true",
                        help="stop this dedicated HOME's store before each timed launch")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.command[:1] == ["--"]:
        args.command.pop(0)
    if not args.command or args.samples < 1 or args.warmups < 0:
        parser.error("provide a command, positive samples, and nonnegative warmups")
    args.home = args.home.resolve()
    args.cwd = args.cwd.resolve()
    if args.home == Path.home().resolve():
        parser.error("--home must be a dedicated benchmark HOME, not your actual HOME")
    if not args.home.is_dir():
        parser.error("--home must already exist")
    rows = []
    for index in range(-args.warmups, args.samples):
        row = sample(args, index)
        print(json.dumps({"sample": index, **row}), flush=True)
        if index >= 0:
            rows.append(row)
    print(json.dumps({
        "command": args.command,
        "cold_store": args.cold_store,
        "samples": len(rows),
        "median_ms": {key: statistics.median(row[key] for row in rows)
                      for key in rows[0]},
    }), flush=True)


if __name__ == "__main__":
    main()
