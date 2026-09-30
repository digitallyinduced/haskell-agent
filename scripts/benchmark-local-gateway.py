#!/usr/bin/env python3
"""Run an optimized catalog client against a private loopback HTTP/TLS fixture.

Run inside nix develop (Python and openssl required):
  python3 -B scripts/benchmark-local-gateway.py --client /path/to/client

The client receives the full /v1/models URL and manager COUNT, uses only
"Bearer benchmark-only", and emits JSON object lines. Each process starts cold;
the client makes two requests per manager. Numeric *_ms fields are summarized
by reuse and first/later manager, never by every individual sample.
No real credentials are read, no HOME is changed, and no delays are injected.
"""
import argparse
from collections import defaultdict
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import math
import os
from pathlib import Path
import socket
import ssl
import statistics
import subprocess
import tempfile
import threading
import time


def catalog_payload(count):
    return json.dumps({"data": [
        {"id": f"benchmark-model-{i}", "protocol": "responses"}
        for i in range(count)]}, separators=(",", ":")).encode()


@contextmanager
def fixture(payload, context=None, requests=None):
    class Handler(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def setup(self):
            super().setup()
            self.connection.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            self.connection.settimeout(10)

        def do_GET(self):
            status = 200 if (
                self.path == "/v1/models"
                and self.headers.get("Authorization") == "Bearer benchmark-only"
            ) else 403
            body = payload if status == 200 else b"{}"
            if requests is not None:
                requests.append(status)
            # One write prevents fixture header/body packet splitting from
            # manufacturing a delayed-ACK stall in the client benchmark.
            headers = (
                f"HTTP/1.1 {status} {'OK' if status == 200 else 'Forbidden'}\r\n"
                "Content-Type: application/json\r\n"
                "Cache-Control: no-store\r\n"
                f"Content-Length: {len(body)}\r\n\r\n"
            ).encode()
            self.wfile.write(headers + body)

        def log_message(self, *_):
            pass

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    if context is not None:
        server.socket = context.wrap_socket(server.socket, server_side=True)
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    try:
        scheme = "https" if context is not None else "http"
        yield f"{scheme}://127.0.0.1:{server.server_port}"
    finally:
        server.shutdown()
        thread.join()
        server.server_close()


def make_certificate(directory):
    cert, key = directory / "cert.pem", directory / "key.pem"
    subprocess.run([
        "openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
        "-keyout", str(key), "-out", str(cert), "-days", "1",
        "-subj", "/CN=localhost",
        "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1",
        "-addext", "basicConstraints=critical,CA:TRUE",
    ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    return cert, key


def child_environment(cert, directory):
    env = dict(os.environ)
    # Preserve bundle workload (important when certificate loading is slow),
    # adding the fixture root only in this child's private bundle.
    bundle = directory / "bundle.pem"
    original = ssl.get_default_verify_paths().cafile
    contents = Path(original).read_bytes() if original else b""
    bundle.write_bytes(contents + b"\n" + cert.read_bytes())
    env["SSL_CERT_FILE"] = str(bundle)
    # Only localhost is used. Do not accidentally send fixture auth to a proxy.
    for name in tuple(env):
        if name.lower() in ("http_proxy", "https_proxy", "all_proxy"):
            del env[name]
    env["NO_PROXY"] = env["no_proxy"] = "localhost,127.0.0.1"
    return env


def parse_rows(output):
    rows = []
    for line in output.splitlines():
        row = json.loads(line)
        if not isinstance(row, dict):
            raise ValueError("client must emit JSON objects")
        for key, value in row.items():
            if key.endswith("_ms") and (
                    isinstance(value, bool) or not isinstance(value, (int, float))
                    or not math.isfinite(value) or value < 0):
                raise ValueError(f"invalid timing field: {key}")
        rows.append(row)
    if not rows:
        raise ValueError("client emitted no measurements")
    return rows


def validate_probe_rows(rows, count, payload_bytes):
    expected = {(sample, reuse) for sample in range(count) for reuse in (0, 1)}
    observed = set()
    for row in rows:
        if not {"sample", "reuse", "checksum", "manager_ms", "http_ms",
                "decode_ms", "total_ms"} <= row.keys():
            raise ValueError("probe omitted required measurement fields")
        if any(type(row[key]) is not int for key in ("sample", "reuse", "checksum")):
            raise ValueError("probe indices and checksum must be integers")
        if row["checksum"] != payload_bytes:
            raise ValueError("probe decoded an unexpected catalog")
        observed.add((row["sample"], row["reuse"]))
    if len(rows) != 2 * count or observed != expected:
        raise ValueError("probe emitted missing or duplicate request measurements")


def summaries(rows):
    groups = defaultdict(lambda: defaultdict(list))
    for row in rows:
        label = tuple((key, row[key]) for key in ("mode", "stage", "phase")
                      if key in row)
        if "reuse" in row:
            label += (("reuse", row["reuse"]),)
        if "sample" in row:
            label += (("manager_phase", "first_manager" if row["sample"] == 0
                       else "later_manager"),)
        for key, value in row.items():
            if key.endswith("_ms"):
                groups[label][key].append(value)
    return [dict(label=dict(label), median_ms={
        key: statistics.median(values) for key, values in metrics.items()
    }, samples={key: len(values) for key, values in metrics.items()})
        for label, metrics in groups.items()]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--client", type=Path, required=True)
    parser.add_argument("--samples", type=int, default=7,
                        help="independent client processes per transport")
    parser.add_argument("--requests", type=int, default=3,
                        help="manager COUNT passed to each client (2 requests each)")
    parser.add_argument("--models", type=int, default=10)
    parser.add_argument("--timeout", type=float, default=30)
    args = parser.parse_args()
    if (min(args.samples, args.requests, args.models) < 1
            or not math.isfinite(args.timeout) or args.timeout <= 0):
        parser.error("counts and timeout must be positive")
    with tempfile.TemporaryDirectory(prefix="gateway-local-") as name:
        directory = Path(name)
        cert, key = make_certificate(directory)
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain(cert, key)
        env = child_environment(cert, directory)
        payload = catalog_payload(args.models)
        # The unmodified trust control matters even without TLS: manager
        # creation may eagerly load the system keychain. Replacing its bundle
        # before timing could otherwise hide the very startup cost under study.
        system_trust_env = dict(env)
        if "SSL_CERT_FILE" in os.environ:
            system_trust_env["SSL_CERT_FILE"] = os.environ["SSL_CERT_FILE"]
        else:
            system_trust_env.pop("SSL_CERT_FILE", None)
        for transport, tls, client_env in (
                ("http-system-trust", None, system_trust_env),
                ("http", None, env), ("https", context, env)):
            collected = []
            elapsed = []
            requests = []
            with fixture(payload, tls, requests) as url:
                for sample in range(args.samples):
                    before_requests = len(requests)
                    started = time.perf_counter()
                    result = subprocess.run(
                        [str(args.client.resolve()), url + "/v1/models",
                         str(args.requests)],
                        env=client_env, capture_output=True, text=True,
                        timeout=args.timeout, check=True)
                    elapsed.append((time.perf_counter() - started) * 1000)
                    observed = requests[before_requests:]
                    if len(observed) != 2 * args.requests or any(
                            status != 200 for status in observed):
                        raise ValueError(
                            "client must make two authorized requests per manager; "
                            f"expected {2 * args.requests}, observed {observed}")
                    rows = parse_rows(result.stdout)
                    validate_probe_rows(rows, args.requests, len(payload))
                    collected.extend(rows)
                    for row in rows:
                        print(json.dumps({"transport": transport, "sample": sample,
                                          "measurement": row}), flush=True)
            print(json.dumps({
                "summary": transport, "models": args.models,
                "payload_bytes": len(payload), "process_samples": args.samples,
                "process_wall_median_ms": statistics.median(elapsed),
                "measurements": summaries(collected),
            }), flush=True)


if __name__ == "__main__":
    main()
