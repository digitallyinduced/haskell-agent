#!/usr/bin/env python3
"""Bounded, loopback-only synthetic Responses endpoint; never contacts a provider.

Creates a fresh isolated home/cwd and CLI launch wrapper beneath TMPDIR. The
HTTP server logs numeric workload observations, never prompts/headers/tool data.
This exercises a real CLI transport/tool/persistence/UI path, not real model
inference. Large whitespace arguments are a stress control, not typical traffic.
"""

import argparse
import http.server
import json
import os
from pathlib import Path
import re
import shlex
import signal
import subprocess
import sys
import time


def bounded(low, high):
    def parse(raw):
        value = int(raw)
        if not low <= value <= high:
            raise argparse.ArgumentTypeError(f"must be between {low} and {high}")
        return value
    return parse


def write_json(path, value):
    temporary = path.with_suffix(path.suffix + ".new")
    temporary.write_text(json.dumps(value, sort_keys=True) + "\n")
    temporary.replace(path)


def user_text(item):
    content = item.get("content", "")
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "\n".join(part.get("text", "") for part in content
                         if isinstance(part, dict))
    return ""


def request_position(body, rounds):
    """Derive progress from transcript, so retries do not advance the fixture."""
    items = body.get("input", [])
    if not isinstance(items, list):
        raise ValueError("input must be an array")
    start, turn = None, None
    for index, item in enumerate(items):
        if isinstance(item, dict) and item.get("role") == "user":
            match = re.search(r"\bMEMORY_TURN_(\d+)\b", user_text(item))
            if match:
                start, turn = index, int(match.group(1))
    if start is None or not 1 <= turn <= 1000:
        raise ValueError("requires synthetic MEMORY_TURN_1..1000 user marker")
    prefix = f"call_memory_{turn}_"
    completed = {
        item.get("call_id") for item in items[start + 1:]
        if isinstance(item, dict)
        and item.get("type") == "function_call_output"
        and isinstance(item.get("call_id"), str)
        and item["call_id"].startswith(prefix)
    }
    # Require exact contiguous IDs; an unrelated response must not skip work.
    expected = {prefix + str(index) for index in range(len(completed))}
    if completed != expected or len(completed) > rounds:
        raise ValueError("non-contiguous or excessive fixture tool outputs")
    return turn, len(completed)


def stream_events(turn, round_index, options, auxiliary=False):
    response_id = f"resp_memory_{turn}_{round_index}"
    response = dict(id=response_id, object="response", created_at=0,
                    model="memory-fixture", status="in_progress", output=[])
    yield "response.created", dict(response=response)
    yield "response.in_progress", dict(response=response)
    if round_index < options.rounds and not auxiliary:
        call_id = f"call_memory_{turn}_{round_index}"
        item_id = f"fc_memory_{turn}_{round_index}"
        if options.tool == "shell-output":
            # Only bounded integers and generated identifiers enter this code.
            # Flush small chunks to exercise real tool progress callbacks.
            script = ("import sys,time\n"
                      f"total={options.output_bytes}; chunks={options.output_chunks}\n"
                      "for i in range(chunks):\n"
                      " n=total//chunks+(i<total%chunks)\n"
                      " sys.stdout.write('x'*(n-1)+'\\n' if n else '')\n"
                      " sys.stdout.flush()\n"
                      f" time.sleep({options.output_duration_ms}/1000/chunks)\n"
                      f"print('MEMORY_OUTPUT_DONE_{turn}_{round_index}')\n"
                      f"open('output-done-{turn}-{round_index}', 'w').write(str(total))\n")
            name = "run_terminal_cmd"
            parameters = dict(command=shlex.quote(sys.executable) + " -u -c " + shlex.quote(script),
                              timeout=30000,
                              description="Stream bounded synthetic build output.")
        elif options.tool == "shell-write":
            filename = f"fixture-edit-{turn}-{round_index}.txt"
            content = "".join(
                f"symbol_{index:05d} = synthetic_value_{turn}_{round_index}_{index}\n"
                for index in range(options.write_lines))
            # Only generated literal content enters the quoted heredoc; neither
            # paths nor command fragments come from the client's request.
            command = (f"cat > {filename} <<'MEMORY_FIXTURE_EOF'\n"
                       + content + f"MEMORY_FIXTURE_EOF\nwc -c < {filename}")
            name = "run_terminal_cmd"
            parameters = dict(command=command, timeout=10000,
                              description="Write and measure a bounded synthetic fixture file.")
        else:
            name = "read_file"
            parameters = {"target_file": "fixture.txt"}
        # Padding is JSON whitespace, not an extra field visible to the tool.
        arguments = (" " * options.argument_padding
                     + json.dumps(parameters, separators=(",", ":")))
        item = dict(type="function_call", id=item_id, call_id=call_id,
                    name=name, arguments="", status="in_progress")
        yield "response.output_item.added", dict(output_index=0, item=item)
        for offset in range(0, len(arguments), options.chunk_bytes):
            yield "response.function_call_arguments.delta", dict(
                output_index=0, item_id=item_id,
                delta=arguments[offset:offset + options.chunk_bytes])
        yield "response.function_call_arguments.done", dict(
            output_index=0, item_id=item_id, name=name, arguments=arguments)
        final_item = dict(item, arguments=arguments, status="completed")
    else:
        item_id = f"msg_memory_{turn}"
        line = "Synthetic coding result: parsed fixture, checked symbols, no changes.\n"
        text = (line * (options.text_bytes // len(line) + 1))[:options.text_bytes]
        text += f"\nMEMORY_DONE_{turn}\n"
        if auxiliary:
            text = "Synthetic fixture session"
        item = dict(type="message", id=item_id, role="assistant",
                    status="in_progress", content=[])
        part = dict(type="output_text", text="", annotations=[])
        yield "response.output_item.added", dict(output_index=0, item=item)
        yield "response.content_part.added", dict(
            output_index=0, item_id=item_id, content_index=0, part=part)
        for offset in range(0, len(text), options.chunk_bytes):
            yield "response.output_text.delta", dict(
                output_index=0, item_id=item_id, content_index=0,
                delta=text[offset:offset + options.chunk_bytes])
        yield "response.output_text.done", dict(
            output_index=0, item_id=item_id, content_index=0, text=text)
        final_part = dict(part, text=text)
        yield "response.content_part.done", dict(
            output_index=0, item_id=item_id, content_index=0, part=final_part)
        final_item = dict(item, content=[final_part], status="completed")
    yield "response.output_item.done", dict(output_index=0, item=final_item)
    yield "response.completed", dict(
        response=dict(response, status="completed", output=[final_item],
                      usage=dict(input_tokens=100, output_tokens=100, total_tokens=200)))


class FixtureHandler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def setup(self):
        super().setup()
        self.connection.settimeout(15)

    def log_message(self, *_):
        pass

    def reply_json(self, status, value):
        data = json.dumps(value).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(data)
        self.close_connection = True

    def do_GET(self):
        if self.path == "/status":
            self.reply_json(200, self.server.observations)
        else:
            self.reply_json(404, {"error": "fixture endpoint not found"})

    def do_POST(self):
        options = self.server.options
        if self.path != "/v1/responses":
            self.reply_json(404, {"error": "fixture endpoint not found"})
            return
        try:
            size = int(self.headers.get("Content-Length", "0"))
        except ValueError:
            size = 0
        if not 0 < size <= options.max_request_bytes:
            self.reply_json(413, {"error": "missing or excessive Content-Length"})
            return
        if self.server.observations["requests"] >= options.max_requests:
            self.reply_json(429, {"error": "fixture request bound reached"})
            return
        try:
            body = json.loads(self.rfile.read(size))
            if not isinstance(body, dict):
                raise ValueError("expected object")
            turn, round_index = request_position(body, options.rounds)
            # A driven request ends with the runner's literal user marker.
            # Background consumers can include that marker inside a transcript
            # excerpt and can themselves advertise tools; those are still not
            # driven turns. Never let either kind advance completion counters.
            users = [item for item in body.get("input", [])
                     if isinstance(item, dict) and item.get("role") == "user"]
            auxiliary = (not body.get("tools") or not users or
                         re.fullmatch(r"MEMORY_TURN_\d+",
                                      user_text(users[-1]).strip()) is None)
            if options.tool == "shell-output" and not auxiliary:
                for item in body.get("input", []):
                    if (isinstance(item, dict)
                            and item.get("type") == "function_call_output"
                            and str(item.get("call_id", "")).startswith(f"call_memory_{turn}_")):
                        index = int(item["call_id"].rsplit("_", 1)[1])
                        marker = Path(options.fixture_cwd) / f"output-done-{turn}-{index}"
                        if not marker.exists() or marker.read_text() != str(options.output_bytes):
                            raise ValueError("streamed output did not complete")
            if options.tool == "read-file" and not auxiliary:
                for item in body.get("input", []):
                    if (isinstance(item, dict)
                            and item.get("type") == "function_call_output"
                            and str(item.get("call_id", "")).startswith(
                                f"call_memory_{turn}_")):
                        output = json.dumps(item.get("output", ""), ensure_ascii=False)
                        if not all(
                                f"symbol_{index:04d} = synthetic fixture line {index}"
                                in output for index in range(options.file_lines)):
                            raise ValueError("read_file did not return fixture content")
        except (ValueError, TypeError, KeyError):
            self.reply_json(400, {"error": "invalid synthetic fixture request"})
            return
        del body  # Do not retain the received conversation between requests.
        observation = self.server.observations
        observation["requests"] += 1
        observation["request_bytes"] += size
        observation["latest_turn"] = turn
        observation["latest_round"] = round_index
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Connection", "close")
        self.end_headers()
        self.close_connection = True
        start = time.monotonic()
        try:
            for sequence, (kind, payload) in enumerate(stream_events(
                    turn, round_index, options, auxiliary=auxiliary)):
                if self.server.stop_requested or time.monotonic() >= self.server.deadline:
                    raise TimeoutError("fixture lifetime bound reached")
                event = dict(type=kind, sequence_number=sequence, **payload)
                encoded = ("event: " + kind + "\ndata: "
                           + json.dumps(event, separators=(",", ":")) + "\n\n").encode()
                self.wfile.write(encoded)
                self.wfile.flush()
                observation["response_bytes"] += len(encoded)
                if options.chunk_delay_ms and kind.endswith(".delta"):
                    time.sleep(options.chunk_delay_ms / 1000)
            if round_index == options.rounds and not auxiliary:
                observation["completed_turn"] = turn
            observation["completed_requests"] += 1
            outcome = "completed"
        except (BrokenPipeError, ConnectionResetError, TimeoutError):
            observation["disconnected_requests"] += 1
            outcome = "disconnected"
        with self.server.log_path.open("a") as log:
            log.write(json.dumps(dict(unix_seconds=time.time(), turn=turn,
                                      round=round_index, request_bytes=size,
                                      auxiliary=auxiliary,
                                      elapsed_seconds=time.monotonic() - start,
                                      outcome=outcome)) + "\n")
        write_json(self.server.status_path, observation)


def prepare(root, runtime_root, port, options):
    home, cwd = runtime_root / "home", root / "cwd"
    config = root / "home/.haskell-agent"
    config.mkdir(parents=True)
    cwd.mkdir()
    (cwd / "fixture.txt").write_text(
        "".join(f"symbol_{index:04d} = synthetic fixture line {index}\n"
                for index in range(options.file_lines)))
    write_json(config / "models.json", {
        "version": 1,
        "connections": {"memory-fixture": {
            "api": "responses", "base_url": f"http://127.0.0.1:{port}/v1",
            "api_key_optional": True, "request_timeout_seconds": 120}},
        "models": [{"id": "memory-fixture", "connection": "memory-fixture",
                    "model": "memory-fixture", "dialect": "generic-responses",
                    "context_window": 1000000, "label": "loopback memory fixture"}]})
    # Intentionally no ambient credentials, user configuration, or gateway.
    environment = {key: value for key, value in os.environ.items()
                   if key in {"PATH", "LD_LIBRARY_PATH", "LIBRARY_PATH",
                              "AGENT_POSTGRES_BIN", "AGENT_SYNTAX_DIR", "TZDIR"}
                   or (key.startswith("agent_") and key.endswith("_datadir"))}
    environment.update(HOME=str(home), TMPDIR=os.environ["TMPDIR"],
                       LANG="C.UTF-8", LC_ALL="C.UTF-8", TERM="xterm-256color",
                       XDG_CONFIG_HOME=str(home / ".config"),
                       XDG_CACHE_HOME=str(home / ".cache"),
                       XDG_DATA_HOME=str(home / ".local/share"),
                       XDG_STATE_HOME=str(home / ".local/state"),
                       HASKELL_AGENT_INBOX_DIRECTORY=str(runtime_root / "inbox"),
                       HASKELL_AGENT_OBSERVATION_DIRECTORY=str(runtime_root / "observation"),
                       AGENT_HEAP_DIAGNOSTICS=str(root / "heap.csv"))
    if options.force_gc:
        environment["AGENT_HEAP_DIAGNOSTICS_FORCE_GC"] = "1"
    if options.postgres_port:
        environment["AGENT_POSTGRES_PORT"] = str(options.postgres_port)
    command = ["env", "-i"] + [f"{key}={value}" for key, value in environment.items()]
    command += [str(Path(options.cli).resolve()), "--model", "memory-fixture",
                "--cwd", str(cwd), "--no-agents-md", "--no-skills", "--no-computer-use",
                "--no-ghci", "--no-code-mode"]
    if options.tool in {"shell-write", "shell-output"}:
        # This opt-in workload writes only generated paths in the fresh cwd.
        command += ["--yolo"]
    # Arguments before RTS allow the same wrapper to run --resume <ID>.
    script = ("#!/bin/sh\nexec " + shlex.join(command) + ' "$@" +RTS -N4 -T -s'
              + (" -hT -i0.1" if options.heap_profile else "")
              + " -RTS 2>" + shlex.quote(str(root / "cli-stderr.log")) + "\n")
    (root / "run-cli").write_text(script)
    (root / "run-cli").chmod(0o700)


def stop_fixture_postgres(root):
    """Stop only the fresh cluster this fixture home could have created."""
    data = root / "home/.haskell-agent/postgres/data"
    if not (data / "postmaster.pid").is_file():
        return
    binary = str(Path(os.environ["AGENT_POSTGRES_BIN"]) / "pg_ctl") \
        if os.environ.get("AGENT_POSTGRES_BIN") else "pg_ctl"
    with (root / "postgres-stop.log").open("w") as log:
        try:
            result = subprocess.run(
                [binary, "-D", str(data), "-m", "fast", "-w", "-t", "30", "stop"],
                stdout=log, stderr=subprocess.STDOUT, timeout=35, check=False)
            if result.returncode:
                raise RuntimeError("fixture PostgreSQL stop failed; inspect postgres-stop.log")
        except subprocess.TimeoutExpired as error:
            raise RuntimeError("fixture PostgreSQL stop timed out") from error


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True, help="new run directory under TMPDIR")
    parser.add_argument("--cli", required=True, help="optimized CLI or frozen exec wrapper")
    parser.add_argument("--port", type=bounded(0, 65535), default=0)
    parser.add_argument("--postgres-port", type=bounded(1024, 65535))
    parser.add_argument("--rounds", type=bounded(0, 100), default=4)
    parser.add_argument("--tool", choices=["read-file", "shell-write", "shell-output"], default="read-file")
    parser.add_argument("--output-bytes", type=bounded(1024, 1048576), default=262144)
    parser.add_argument("--output-chunks", type=bounded(1, 1000), default=200)
    parser.add_argument("--output-duration-ms", type=bounded(0, 10000), default=3000)
    parser.add_argument("--write-lines", type=bounded(1, 20000), default=100)
    parser.add_argument("--argument-padding", type=bounded(0, 8 * 1024 * 1024), default=0)
    parser.add_argument("--text-bytes", type=bounded(0, 8 * 1024 * 1024), default=4096)
    parser.add_argument("--file-lines", type=bounded(1, 1000), default=100)
    parser.add_argument("--chunk-bytes", type=bounded(1, 65536), default=4096)
    parser.add_argument("--chunk-delay-ms", type=bounded(0, 1000), default=5)
    parser.add_argument("--max-request-bytes", type=bounded(1024, 128 * 1024 * 1024),
                        default=64 * 1024 * 1024)
    parser.add_argument("--max-requests", type=bounded(1, 10000), default=1000)
    parser.add_argument("--max-seconds", type=bounded(1, 7200), default=1800)
    parser.add_argument("--force-gc", action="store_true")
    parser.add_argument("--heap-profile", action="store_true")
    parser.add_argument("--proc-home", action="store_true",
                        help="Linux: short /proc/PID/fd alias to TMPDIR-owned run directory")
    options = parser.parse_args()
    temporary_root = Path(os.environ["TMPDIR"]).resolve()
    root = options.root.resolve()
    if not root.is_relative_to(temporary_root) or root == temporary_root:
        parser.error("--root must be a new child of TMPDIR")
    if root.exists():
        parser.error("--root already exists; use a fresh run directory")
    # Resolve the executable before entering any fixture directory context.
    options.cli = str(Path(options.cli).resolve())
    options.fixture_cwd = str(root / "cwd")
    runtime_root = root
    root_fd = None
    if options.proc_home:
        if not Path("/proc/self/fd").is_dir():
            parser.error("--proc-home requires Linux procfs")
        # The underlying directory remains inside TMPDIR. Keeping this fd open
        # owns the short alias through CLI/resume and exact-cluster shutdown.
        root.mkdir(parents=True, mode=0o700)
        root_fd = os.open(root, os.O_RDONLY | os.O_DIRECTORY)
        runtime_root = Path(f"/proc/{os.getpid()}/fd/{root_fd}")
    socket_directory = runtime_root / "home/.haskell-agent/postgres/run"
    observation_socket = runtime_root / "observation" / ("0" * 24)
    if len(os.fsencode(socket_directory)) > 90 or len(os.fsencode(observation_socket)) >= 104:
        parser.error("run root is too long for PostgreSQL/observation sockets; "
                     "use --proc-home on Linux or a short tool-provided TMPDIR")
    if root_fd is None:
        root.mkdir(parents=True, mode=0o700)
    try:
        serve(root, runtime_root, options)
    finally:
        try:
            stop_fixture_postgres(root)
        finally:
            if root_fd is not None:
                os.close(root_fd)


def serve(root, runtime_root, options):
    with http.server.HTTPServer(("127.0.0.1", options.port), FixtureHandler) as server:
        server.timeout = 0.25
        server.options = options
        server.log_path = root / "requests.jsonl"
        server.status_path = root / "status.json"
        server.observations = dict(requests=0, completed_requests=0,
                                   disconnected_requests=0, request_bytes=0,
                                   response_bytes=0, latest_turn=0,
                                   latest_round=0, completed_turn=0)
        prepare(root, runtime_root, server.server_port, options)
        write_json(server.status_path, server.observations)
        write_json(root / "ready.json", dict(port=server.server_port, pid=os.getpid(),
                                           root=str(root), runtime_root=str(runtime_root),
                                           options=vars(options) | {"root": str(root)}))
        print(json.dumps({"root": str(root), "port": server.server_port}), flush=True)
        server.stop_requested = False

        def request_stop(*_):
            server.stop_requested = True

        signal.signal(signal.SIGTERM, request_stop)
        signal.signal(signal.SIGINT, request_stop)
        server.deadline = time.monotonic() + options.max_seconds
        while not server.stop_requested and time.monotonic() < server.deadline:
            server.handle_request()


if __name__ == "__main__":
    main()
