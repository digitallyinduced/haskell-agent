#!/usr/bin/env python3
"""macOS worker-only RSS/CPU comparison; not an end-to-end latency benchmark.

Usage: python3 memory-comparison.py BUN_EXECUTABLE BUN_WORKER NATIVE_WORKER
Fresh processes, three alternating repetitions, 121 tool names, unique sources,
no forced GC. Production Bun flags and empty child environments match the host.
JSONL reports bytes (MiB = bytes / 1048576) and total worker CPU seconds per run.
RSS includes resident shared pages: do not sum this to infer whole-app memory.
Peak RSS and CPU are kernel wait4 rusage for the worker, excluding this driver.
The driver's JSON cost makes wall_seconds unsuitable for host latency claims.
"""
import ctypes
import json
import os
import queue
import signal
import subprocess
import sys
import threading
import time


def timebase():
    class Timebase(ctypes.Structure):
        _fields_ = [("numer", ctypes.c_uint32), ("denom", ctypes.c_uint32)]
    value = Timebase()
    assert ctypes.CDLL(None).mach_timebase_info(ctypes.byref(value)) == 0
    return {"numer": value.numer, "denom": value.denom}


class TaskInfo(ctypes.Structure):
    _fields_ = [(name, ctypes.c_uint64) for name in (
        "virtual_size", "resident_size", "total_user", "total_system",
        "threads_user", "threads_system")] + [
        (name, ctypes.c_int32) for name in (
            "policy", "faults", "pageins", "cow_faults", "messages_sent",
            "messages_received", "syscalls_mach", "syscalls_unix",
            "csw", "threadnum", "numrunning", "priority")]


libproc = ctypes.CDLL("/usr/lib/libproc.dylib", use_errno=True)
libproc.proc_pidinfo.argtypes = [
    ctypes.c_int, ctypes.c_int, ctypes.c_uint64, ctypes.c_void_p, ctypes.c_int]
libproc.proc_pidinfo.restype = ctypes.c_int
libproc.proc_pid_rusage.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_void_p]
libproc.proc_pid_rusage.restype = ctypes.c_int


class UsageInfo(ctypes.Structure):
    _fields_ = [("uuid", ctypes.c_uint8 * 16)] + [
        (name, ctypes.c_uint64) for name in (
            "user_time", "system_time", "pkg_idle_wkups", "interrupt_wkups",
            "pageins", "wired_size", "resident_size", "phys_footprint",
            "proc_start_abstime", "proc_exit_abstime")]


def usage_info(pid):
    info = UsageInfo()
    if libproc.proc_pid_rusage(pid, 0, ctypes.byref(info)):
        raise OSError(ctypes.get_errno(), "proc_pid_rusage failed")
    return info


def rss(pid):
    info = TaskInfo()
    size = libproc.proc_pidinfo(pid, 4, 0, ctypes.byref(info), ctypes.sizeof(info))
    if size != ctypes.sizeof(info):
        raise OSError(ctypes.get_errno(), "proc_pidinfo(PROC_PIDTASKINFO) failed")
    return info.resident_size


def measure(command, cells, size):
    process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True, env={})
    messages = queue.Queue()
    errors = bytearray()

    def reader():
        try:
            for line in process.stdout:
                messages.put(json.loads(line))
            messages.put(RuntimeError("worker exited: " + errors.decode(errors="replace")))
        except Exception as error:
            messages.put(error)

    def stderr_reader():
        for chunk in iter(lambda: process.stderr.read(1024), b""):
            errors.extend(chunk)
            del errors[:-8192]

    threading.Thread(target=reader, daemon=True).start()
    threading.Thread(target=stderr_reader, daemon=True).start()
    watchdog = threading.Timer(120, lambda: os.killpg(process.pid, signal.SIGKILL))
    watchdog.start()

    def receive():
        message = messages.get(timeout=10)
        if isinstance(message, Exception):
            raise message
        assert message["jsonrpc"] == "2.0", message
        return message

    def send(message):
        process.stdin.write((json.dumps({"jsonrpc": "2.0", **message},
                                       separators=(",", ":")) + "\n").encode())
        process.stdin.flush()

    try:
        assert receive()["method"] == "ready"
        # Fixed settling time, identical for each backend, no explicit GC.
        time.sleep(0.2)
        startup = rss(process.pid)
        startup_footprint = usage_info(process.pid).phys_footprint
        checkpoints = {}
        footprints = {}
        idle_activity = {}
        if cells == 0:
            before = usage_info(process.pid)
            time.sleep(3)
            after = usage_info(process.pid)
            idle_activity = {"interval_seconds": 3, **{
                name: getattr(after, name) - getattr(before, name) for name in (
                    "user_time", "system_time", "pkg_idle_wkups", "interrupt_wkups")}}
        tools = ["echo"] + [f"unused_{i}" for i in range(1, 121)]
        start = time.monotonic()
        for index in range(1, cells + 1):
            source = (f"// unique cell {index}\n"
                      f"const result = await tools.echo({{payload:'x'.repeat({size})}}); "
                      "text(result.payload.length);")
            send({"id": index, "method": "exec", "params": {
                "source": source, "tools": tools, "stored_values": {},
                "image_detail_visible": False}})
            content = []
            calls = 0
            while True:
                message = receive()
                if message.get("method") == "tool/call":
                    assert message["params"]["name"] == "echo"
                    assert message["params"]["arguments"] == {"payload": "x" * size}
                    calls += 1
                    send({"id": message["id"], "result": message["params"]["arguments"]})
                elif message.get("method") == "content":
                    content.append(message["params"]["value"])
                else:
                    assert message.get("id") == index and "error" not in message, message
                    content.extend(message["result"].get("content", []))
                    assert content == [{"type": "text", "text": str(size)}], content
                    assert calls == 1
                    break
            if index in (1, 100, 1000):
                checkpoints[str(index)] = rss(process.pid)
                footprints[str(index)] = usage_info(process.pid).phys_footprint
        wall = time.monotonic() - start
        time.sleep(0.2)
        idle = rss(process.pid)
        idle_footprint = usage_info(process.pid).phys_footprint
        process.stdin.close()
        _, status, usage = os.wait4(process.pid, 0)
        process.returncode = os.waitstatus_to_exitcode(status)
        assert process.returncode == 0, (process.returncode, errors.decode(errors="replace"))
        return {"startup_idle_rss_bytes": startup, "checkpoint_rss_bytes": checkpoints,
                "startup_idle_footprint_bytes": startup_footprint,
                "checkpoint_footprint_bytes": footprints,
                "post_workload_idle_footprint_bytes": idle_footprint,
                "idle_activity_cpu_mach_ticks": idle_activity,
                "mach_ticks_to_nanoseconds": timebase(),
                "post_workload_idle_rss_bytes": idle,
                "peak_rss_bytes": usage.ru_maxrss, "user_cpu_seconds": usage.ru_utime,
                "system_cpu_seconds": usage.ru_stime, "wall_seconds": wall}
    finally:
        watchdog.cancel()
        if process.returncode is None:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        process.stdout.close()
        process.stderr.close()


def main():
    if sys.platform != "darwin" or len(sys.argv) != 4:
        raise SystemExit(__doc__)
    bun, script, native = sys.argv[1:]
    commands = {"bun": [bun, "--smol", "--no-install", "--no-env-file", "--no-addons", script],
                "javascriptcore": [native]}
    for label, cells, size in (("idle", 0, 16), ("small", 1000, 16),
                               ("large", 100, 1048576)):
        for repetition in range(1, 4):
            order = ("bun", "javascriptcore") if repetition % 2 else ("javascriptcore", "bun")
            for backend in order:
                result = measure(commands[backend], cells, size)
                print(json.dumps({"backend": backend, "workload": label,
                                  "repetition": repetition, "cells": cells,
                                  "payload_bytes": size, **result}), flush=True)


if __name__ == "__main__":
    main()
