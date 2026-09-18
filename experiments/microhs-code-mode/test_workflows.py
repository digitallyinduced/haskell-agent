import unittest

from benchmark_workflows import FixtureHandler, workflows
from experiment import run_bun, temporary_directory
from persistent import PersistentMicroHs


class WorkflowFixtureTests(unittest.IsolatedAsyncioTestCase):
    async def test_async_tool_error_propagation(self):
        async def denied(name, arguments):
            raise ValueError("async denial")
        with temporary_directory() as directory:
            result = await run_bun('await tools.fixture.denied({});', directory, handler=denied)
            self.assertIn("async denial", result["completion"]["error"]["message"])
            async with PersistentMicroHs(directory) as worker:
                result = await worker.execute('callTool "fixture.denied" JsonNull >>= emit', handler=denied)
                self.assertIn("async denial", result["completion"]["error"]["message"])
                self.assertEqual((await worker.execute('emit JsonNull'))["content"], [None])

    async def test_exact_call_arguments_and_latency(self):
        workflow = next(workflows())
        handler = FixtureHandler(workflow, 1)
        for name, arguments, expected in workflow.exchanges:
            self.assertEqual(await handler(name, arguments), expected)
        self.assertEqual(handler.calls, 2)
        self.assertGreaterEqual(handler.wait_ms, 2)

    async def test_wrong_dependent_identifier_rejected(self):
        handler = FixtureHandler(next(workflows()), 0)
        await handler("fixture.customer", {"email": "alex@example.test"})
        with self.assertRaises(AssertionError):
            await handler("fixture.subscription", {"customerId": "wrong"})

    def test_workload_sizes_and_repeat(self):
        cases = list(workflows())
        self.assertEqual(len(cases), 6)
        self.assertEqual(cases[1].expected, cases[3].expected)
        self.assertEqual(len(cases[2].exchanges[0][2]["data"]), 1000)
        self.assertEqual(len(cases[4].exchanges[0][2]["data"]), 1000)
