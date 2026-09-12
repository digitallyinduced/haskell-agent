import asyncio
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from experiment import temporary_directory
from persistent import PersistentMicroHs


class PersistentTests(unittest.IsolatedAsyncioTestCase):
    async def test_distinct_cells_and_recovery(self):
        with temporary_directory() as directory:
            async with PersistentMicroHs(directory) as worker:
                pid = worker.process.pid
                first = await worker.execute('callTool "fixture.echo" (JsonNumber "42") >>= emit')
                second = await worker.execute(
                    'do { x <- callTool "fixture.echo" (JsonString "second"); '
                    'callTool "fixture.echo" x >>= emit }')
                self.assertEqual(first["content"], [42])
                self.assertEqual(second["content"], ["second"])
                self.assertEqual(second["calls"], 2)
                value = {"text": "λ 😀\n> ", "null": None, "array": [False, 42]}
                echoed = await worker.execute(
                    'callTool "fixture.echo" JsonNull >>= emit',
                    handler=lambda name, arguments: value)
                self.assertEqual(echoed["content"], [value])
                for expression in [
                    'do { callTool "fixture.echo" JsonNull; emit True }',
                    'callTool "fixture.denied" JsonNull >>= emit',
                    'do { emit (JsonString "partial"); error "failure" }',
                    'emit (',
                ]:
                    result = await worker.execute(expression)
                    self.assertIn("error", result["completion"])
                    if "emit True" in expression:
                        self.assertEqual(result["calls"], 0)
                    if '"partial"' in expression:
                        self.assertEqual(result["content"], ["partial"])
                    recovery = await worker.execute("emit (JsonBool True)")
                    self.assertEqual(recovery["content"], [True])
                    self.assertEqual(worker.process.pid, pid)
                with self.assertRaises(TimeoutError):
                    await worker.execute("let loop = loop in loop", timeout=0.5)
                self.assertTrue(worker.closed)
                self.assertIsNotNone(worker.process.returncode)
                with self.assertRaises(RuntimeError):
                    await worker.execute("pure ()")
            async with PersistentMicroHs(directory) as replacement:
                self.assertNotEqual(replacement.process.pid, pid)
                self.assertEqual((await replacement.execute("emit JsonNull"))["content"], [None])
                with patch("persistent.MAXIMUM_BYTES", 1024):
                    with self.assertRaisesRegex(ValueError, "output limit"):
                        await replacement.execute('emit (JsonString (replicate 2048 \'x\'))')
                self.assertTrue(replacement.closed)

    async def test_prompt_and_line_framing(self):
        worker = PersistentMicroHs(".")
        reader = asyncio.StreamReader()
        worker.process = SimpleNamespace(stdout=reader)
        reader.feed_data(b'> {"text": "> not a prompt"}\n> ')
        self.assertIsNone(await worker._event())
        self.assertEqual(await worker._event(), b'{"text": "> not a prompt"}')
        self.assertIsNone(await worker._event())
        reader.feed_eof()
        with self.assertRaisesRegex(RuntimeError, "exited"):
            await worker._event()


if __name__ == "__main__":
    unittest.main()
