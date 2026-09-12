import asyncio
import json
import sys
import subprocess
import unittest

from experiment import DIRECTORY, executable, exchange, haskell_cell, run_bun, run_haskell, temporary_directory


class MicroHsExperimentTests(unittest.IsolatedAsyncioTestCase):
    async def execute(self, body, **options):
        with temporary_directory() as directory:
            return await run_haskell(haskell_cell(body), directory, **options)

    async def test_two_dependent_calls(self):
        result = await self.execute(
            'first <- callTool "fixture.echo" (JsonObject [("value", JsonNumber "42")])\n'
            'second <- callTool "fixture.echo" first\nemit second')
        self.assertEqual(result["content"], [{"value": 42}])
        self.assertEqual(result["calls"], 2)
        self.assertIn("result", result["completion"])

    async def test_json_round_trip(self):
        value = {"unicode": "Grüße λ 😀", "control": "\x00\b\f\n\r\t\x1f", "quote": '"\\',
                 "values": [None, True, False, -50.25, 1e25, {"nested": []}]}
        result = await self.execute(
            'value <- callTool "fixture.echo" JsonNull\nemit value',
            handler=lambda name, arguments: value)
        self.assertEqual(result["content"], [value])

    async def test_runtime_error_preserves_partial_content(self):
        result = await self.execute('emit (JsonString "before failure")\nerror "deliberate failure"')
        self.assertEqual(result["content"], ["before failure"])
        self.assertIn("deliberate failure", result["completion"]["error"]["message"])

    async def test_denied_tool(self):
        result = await self.execute('callTool "fixture.denied" JsonNull >>= emit')
        self.assertEqual(result["content"], [])
        self.assertIn("approval denied", result["completion"]["error"]["message"])

    async def test_invalid_encoding_does_not_write_partial_frame(self):
        result = await self.execute('emit (JsonObject [("prefix", JsonString "before"), ("invalid", JsonNumber "NaN")])')
        self.assertEqual(result["content"], [])
        self.assertIn("error", result["completion"])

    async def test_unavailable_tool(self):
        result = await self.execute('callTool "unavailable.tool" JsonNull >>= emit')
        self.assertIn("not available", result["completion"]["error"]["message"])

    async def test_type_error_precedes_tool_effect(self):
        calls = []
        with self.assertRaises(ExceptionGroup):
            await self.execute('callTool 123 JsonNull >>= emit',
                               handler=lambda name, arguments: calls.append(name))
        self.assertEqual(calls, [])

    async def test_nontermination_times_out(self):
        calls = []
        with self.assertRaises(TimeoutError):
            await self.execute('callTool "fixture.echo" JsonNull\nlet forever = forever in forever',
                               timeout=10, handler=lambda name, arguments: calls.append(name))
        self.assertEqual(calls, ["fixture.echo"])
        # A cancelled worker must not prevent a subsequent cell from running.
        result = await self.execute('emit (JsonBool True)')
        self.assertEqual(result["content"], [True])

    async def test_bun_baseline(self):
        with temporary_directory() as directory:
            result = await run_bun('text(await tools.fixture.echo({value: 42}));', directory)
        self.assertEqual(result["calls"], 1)
        self.assertEqual(json.loads(result["content"][0]["text"]), {"value": 42})

    async def test_json_codec_specification(self):
        with temporary_directory() as directory:
            result = await asyncio.to_thread(subprocess.run,
                [executable("mhs"), "-q", "-r", "-i" + str(DIRECTORY / "haskell"),
                 str(DIRECTORY / "haskell/JsonSpec.hs")],
                cwd=directory, capture_output=True, text=True, timeout=30, check=True)
        self.assertIn("JSON codec tests passed", result.stdout)

    async def test_invalid_worker_protocol(self):
        with temporary_directory() as directory:
            with self.assertRaises(ExceptionGroup):
                await exchange([sys.executable, "-c", "print('{}', flush=True)"], directory)

    async def test_output_limit(self):
        with temporary_directory() as directory:
            with self.assertRaises(ExceptionGroup):
                await exchange([sys.executable, "-c", "print('x' * (5 * 1024 * 1024), flush=True)"], directory)


if __name__ == "__main__":
    unittest.main()
