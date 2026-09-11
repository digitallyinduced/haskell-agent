"""Measure ffplay pipe-to-filter delay using its compiled decoder and SDL dummy.

No microphone, speaker, network, or credentials are used. This isolates software
buffering; it does not measure device or acoustic latency. Python only supplies
clocked PCM and timestamps diagnostic records from the native executable.
"""
import math
import os
import re
import statistics
import struct
import subprocess
import threading
import time


def measure(wav):
    format_options = (["-f", "wav", "-ignore_length", "1", "-max_size", "1920"] if wav
                      else ["-f", "s16le", "-ar", "24000", "-ch_layout", "mono"])
    command = ["ffplay", "-nodisp", "-autoexit", "-nostats", "-loglevel", "info",
               *format_options, "-probesize", "32", "-analyzeduration", "1",
               "-af", "ashowinfo", "-i", "pipe:0"]
    environment = dict(os.environ, SDL_AUDIODRIVER="dummy")
    records, errors = [], []
    frame = struct.pack("<480h", *(int(12000 * math.sin(i * math.pi / 12)) for i in range(480)))
    if wav:
        frame = struct.pack("<480f", *(v / 32768 for v in struct.unpack("<480h", frame)))
    with subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.DEVNULL,
                          stderr=subprocess.PIPE, env=environment, bufsize=0) as process:
        def collect():
            for line in process.stderr:
                match = re.search(rb"pts_time:([0-9.]+).*nb_samples:(\d+)", line)
                if match:
                    records.append((time.monotonic(), float(match[1]), int(match[2])))
                errors.append(line)
        reader = threading.Thread(target=collect)
        reader.start()
        start = time.monotonic()
        try:
            if wav:
                process.stdin.write(struct.pack("<4sI4s4sIHHIIHH4sI", b"RIFF", 0xffffffff,
                    b"WAVE", b"fmt ", 16, 3, 1, 24000, 96000, 4, 32, b"data", 0xffffffff))
            for sequence in range(150):
                time.sleep(max(0, start + sequence * .02 - time.monotonic()))
                process.stdin.write(frame)
            process.stdin.close()
            process.wait(timeout=5)
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            reader.join()
        if process.returncode or not records:
            raise RuntimeError(b"".join(errors).decode(errors="replace"))
    delays = [(observed - start - pts) * 1000 for observed, pts, _ in records]
    samples = sum(n for _, _, n in records)
    assert samples == 72000, samples
    return delays[0], statistics.median(delays[len(delays)//2:]), samples


for repetition in range(5):
    for label, options in [("raw", False), ("wav20ms", True)]:
        first, steady, samples = measure(options)
        print(f"mode={label} sample={repetition} first_filter_ms={first:.3f} "
              f"steady_filter_ms={steady:.3f} decoded_samples={samples}", flush=True)
