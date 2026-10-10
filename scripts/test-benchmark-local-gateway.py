#!/usr/bin/env python3
import http.client
import os
import subprocess
import json
from pathlib import Path
import runpy
import ssl
import tempfile
import unittest
from urllib.parse import urlsplit


BENCH = runpy.run_path(str(Path(__file__).with_name("benchmark-local-gateway.py")))


class LocalGatewayTest(unittest.TestCase):
    @unittest.skipUnless(os.environ.get("LOCAL_GATEWAY_CLIENT"),
                         "set LOCAL_GATEWAY_CLIENT to test the Haskell TLS client")
    def test_haskell_client_rejects_untrusted_certificate(self):
        with tempfile.TemporaryDirectory(prefix="gateway-trust-") as name:
            directory = Path(name)
            trusted = directory / "trusted"
            untrusted = directory / "untrusted"
            trusted.mkdir()
            untrusted.mkdir()
            trusted_cert, _ = BENCH["make_certificate"](trusted)
            server_cert, server_key = BENCH["make_certificate"](untrusted)
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            context.load_cert_chain(server_cert, server_key)
            env = BENCH["child_environment"](trusted_cert, directory)
            with BENCH["fixture"](BENCH["catalog_payload"](1), context) as url:
                result = subprocess.run(
                    [str(Path(os.environ["LOCAL_GATEWAY_CLIENT"]).resolve()),
                     url + "/v1/models", "1"],
                    env=env, capture_output=True, timeout=10)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout, b"")

    def test_http_auth_and_keepalive(self):
        payload = BENCH["catalog_payload"](3)
        requests = []
        with BENCH["fixture"](payload, requests=requests) as url:
            conn = http.client.HTTPConnection(urlsplit(url).netloc)
            try:
                conn.request("GET", "/v1/models")
                response = conn.getresponse()
                self.assertEqual(response.status, 403)
                response.read()
                first_socket = conn.sock
                for _ in range(2):
                    conn.request("GET", "/v1/models", headers={
                        "Authorization": "Bearer benchmark-only"})
                    response = conn.getresponse()
                    self.assertEqual(response.status, 200)
                    self.assertEqual(response.getheader("Cache-Control"), "no-store")
                    self.assertEqual(response.read(), payload)
                    self.assertIs(conn.sock, first_socket)
            finally:
                conn.close()
        self.assertEqual(requests, [403, 200, 200])

    def test_tls_certificate_verification(self):
        with tempfile.TemporaryDirectory(prefix="gateway-test-") as name:
            directory = Path(name)
            cert, key = BENCH["make_certificate"](directory)
            server_context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            server_context.load_cert_chain(cert, key)
            client_context = ssl.create_default_context(cafile=str(cert))
            with BENCH["fixture"](BENCH["catalog_payload"](1), server_context) as url:
                untrusted = http.client.HTTPSConnection(
                    urlsplit(url).netloc,
                    context=ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT))
                try:
                    with self.assertRaises(ssl.SSLCertVerificationError):
                        untrusted.request("GET", "/v1/models")
                finally:
                    untrusted.close()
                conn = http.client.HTTPSConnection(
                    urlsplit(url).netloc, context=client_context)
                try:
                    conn.request("GET", "/v1/models", headers={
                        "Authorization": "Bearer benchmark-only"})
                    response = conn.getresponse()
                    self.assertEqual(response.status, 200)
                    self.assertEqual(len(json.loads(response.read())["data"]), 1)
                finally:
                    conn.close()

    def test_reject_invalid_timings(self):
        for output in ('', '[]', '{"time_ms":-1}', '{"time_ms":NaN}',
                       '{"time_ms":true}'):
            with self.assertRaises(ValueError):
                BENCH["parse_rows"](output)

    def test_summary_separates_modes(self):
        rows = BENCH["parse_rows"](
            '{"mode":"cold","time_ms":3}\n'
            '{"mode":"cold","time_ms":5}\n'
            '{"mode":"warm","time_ms":1}\n')
        result = BENCH["summaries"](rows)
        self.assertEqual(result[0]["median_ms"]["time_ms"], 4)
        self.assertEqual(result[1]["median_ms"]["time_ms"], 1)

    def test_summary_separates_process_and_connection_warmth(self):
        rows = [
            {"sample": 0, "reuse": 0, "total_ms": 100},
            {"sample": 0, "reuse": 1, "total_ms": 2},
            {"sample": 1, "reuse": 0, "total_ms": 10},
            {"sample": 2, "reuse": 0, "total_ms": 12},
        ]
        result = BENCH["summaries"](rows)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[0]["label"],
                         {"reuse": 0, "manager_phase": "first_manager"})
        self.assertEqual(result[2]["median_ms"]["total_ms"], 11)

    def test_probe_requires_all_rows_and_payload_checksum(self):
        rows = [dict(sample=0, reuse=reuse, checksum=20, manager_ms=1,
                     http_ms=1, decode_ms=1, total_ms=3) for reuse in (0, 1)]
        BENCH["validate_probe_rows"](rows, 1, 20)
        for invalid in (rows[:1], [rows[0], rows[0]],
                        [dict(rows[0], checksum=21), rows[1]],
                        [dict(sample=0, reuse=0), rows[1]]):
            with self.assertRaises(ValueError):
                BENCH["validate_probe_rows"](invalid, 1, 20)


if __name__ == "__main__":
    unittest.main()
