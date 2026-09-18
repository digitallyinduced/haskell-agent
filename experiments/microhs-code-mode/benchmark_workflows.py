"""Synthetic tool workflows: full-tree native MicroHs versus actual fresh Bun."""

import argparse
import asyncio
from dataclasses import dataclass
import json
import statistics
import time

from experiment import run_bun, temporary_directory
from persistent import PersistentMicroHs


@dataclass
class Workflow:
    name: str
    haskell: str
    javascript: str
    exchanges: list
    expected: object


def workflows():
    call = 'callToolWithDecoder decodeJsonNative'
    # Helpers are deliberately local: their compilation is part of cell latency.
    helpers = ('let { field key (JsonObject fields) = case lookup key fields of '
               '{ Just value -> value; Nothing -> error "missing field" }; '
               'field _ _ = error "expected object"; '
               'entries (JsonArray values) = values; entries _ = error "expected array" }; ')
    def cell(body):
        return 'do { ' + helpers + body + ' }'
    yield Workflow(
        "chain-id",
        cell(f'customer <- {call} "fixture.customer" (JsonObject [("email", JsonString "alex@example.test")]); '
             f'subscription <- {call} "fixture.subscription" (JsonObject [("customerId", field "id" customer)]); '
             'emit (field "plan" subscription)'),
        'const customer = await tools.fixture.customer({email:"alex@example.test"}); '
        'const subscription = await tools.fixture.subscription({customerId:customer.id}); text(subscription.plan);',
        [("fixture.customer", {"email": "alex@example.test"}, {"id": "customer-42", "name": "Alex"}),
         ("fixture.subscription", {"customerId": "customer-42"}, {"plan": "business", "status": "active"})],
        "business")
    for count, suffix in [(100, ""), (1000, ""), (100, "-repeat")]:
        records = [{"id": f"customer-{index}", "name": f"Customer {index}",
                    "status": "active" if index % 3 == 0 else "inactive",
                    "email": f"user{index}@example.test", "country": "DE",
                    "metadata": {"source": "import", "segment": "business"}}
                   for index in range(count)]
        yield Workflow(
            f"filter-{count}{suffix}",
            cell(f'response <- {call} "fixture.customers" (JsonObject [("limit", JsonNumber "{count}")]); '
                 'emit (JsonArray [JsonObject [("id", field "id" row), ("name", field "name" row)] '
                 '| row <- entries (field "data" response), '
                 'case field "status" row of { JsonString status -> status == "active"; _ -> False }])'),
            f'const response = await tools.fixture.customers({{limit:{count}}}); '
            'text(response.data.filter(row => row.status === "active").map(({id,name}) => ({id,name})));',
            [("fixture.customers", {"limit": count}, {"data": records, "hasMore": False})],
            [{"id": record["id"], "name": record["name"]} for record in records if record["status"] == "active"])
    records = [{"id": f"event-{index}", "description": "Detailed event payload. " * 2,
                "metadata": {"source": "api", "tags": ["account", "billing", "audit"]}}
               for index in range(1000)]
    yield Workflow(
        "sparse-1000",
        cell(f'response <- {call} "fixture.events" (JsonObject []); '
             'emit (JsonObject [("cursor", field "nextCursor" response), '
             '("firstId", field "id" (head (entries (field "data" response))))])'),
        'const response = await tools.fixture.events({}); '
        'text({cursor:response.nextCursor,firstId:response.data[0].id});',
        [("fixture.events", {}, {"data": records, "nextCursor": "page-2"})],
        {"cursor": "page-2", "firstId": "event-0"})
    yield Workflow(
        "independent-sequential",
        cell(f'customer <- {call} "fixture.customer" (JsonObject []); '
             f'usage <- {call} "fixture.usage" (JsonObject []); '
             f'plan <- {call} "fixture.plan" (JsonObject []); '
             'emit (JsonObject [("name", field "name" customer), ("used", field "used" usage), '
             '("limit", field "limit" plan)])'),
        'const customer = await tools.fixture.customer({}); const usage = await tools.fixture.usage({}); '
        'const plan = await tools.fixture.plan({}); text({name:customer.name,used:usage.used,limit:plan.limit});',
        [("fixture.customer", {}, {"name": "Alex"}), ("fixture.usage", {}, {"used": 72}),
         ("fixture.plan", {}, {"limit": 100})],
        {"name": "Alex", "used": 72, "limit": 100})


class FixtureHandler:
    def __init__(self, workflow, latency_ms):
        self.workflow = workflow
        self.latency_ms = latency_ms
        self.calls = 0
        self.wait_ms = 0

    async def __call__(self, name, arguments):
        expected_name, expected_arguments, response = self.workflow.exchanges[self.calls]
        if (name, arguments) != (expected_name, expected_arguments):
            raise AssertionError((name, arguments, expected_name, expected_arguments))
        self.calls += 1
        if self.latency_ms:
            started = time.perf_counter()
            await asyncio.sleep(self.latency_ms / 1000)
            self.wait_ms += (time.perf_counter() - started) * 1000
        return response


async def sample(workflow, backend, latency_ms, worker, directory):
    handler = FixtureHandler(workflow, latency_ms)
    if backend == "microhs-native-full-tree":
        result = await worker.execute(workflow.haskell, timeout=120, handler=handler)
        content = result["content"]
    else:
        result = await run_bun(workflow.javascript, directory, timeout=120, handler=handler,
                               tools=list(dict.fromkeys(name for name, _, _ in workflow.exchanges)))
        content = [block["text"] if isinstance(workflow.expected, str) else json.loads(block["text"])
                   for block in result["content"]]
    if ("error" in result["completion"] or content != [workflow.expected]
            or result["calls"] != len(workflow.exchanges) or handler.calls != len(workflow.exchanges)):
        raise AssertionError((workflow.name, backend, result))
    return {"elapsed_ms": result["elapsed_ms"], "ready_ms": result["ready_ms"],
            "simulated_wait_ms": handler.wait_ms,
            "non_wait_ms": result["elapsed_ms"] - handler.wait_ms}


async def benchmark(interpreter, samples):
    with temporary_directory() as directory:
        async with PersistentMicroHs(directory, interpreter=interpreter,
                                     modules=("CodeMode.NativeJson",)) as worker:
            print(json.dumps({"startup_ms": worker.startup_ms,
                              "note": "MicroHs warm; Bun fresh. Native full-tree conversion. Sequential tool dispatch."}), flush=True)
            for latency_ms in [0, 100]:
                for workflow in workflows():
                    measurements = {"microhs-native-full-tree": [], "bun-fresh": []}
                    for index in range(samples):
                        order = list(measurements)
                        if index % 2:
                            order.reverse()
                        for backend in order:
                            measurements[backend].append(
                                await sample(workflow, backend, latency_ms, worker, directory))
                    for backend, results in measurements.items():
                        print(json.dumps({"workflow": workflow.name, "backend": backend,
                                          "tool_latency_ms": latency_ms, "calls": len(workflow.exchanges),
                                          "response_bytes": sum(len(json.dumps(response, ensure_ascii=True).encode())
                                                                for _, _, response in workflow.exchanges),
                                          "samples": samples,
                                          **{key: statistics.median(result[key] for result in results)
                                             for key in results[0]}, "raw": results}), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--interpreter", default="mhs-native-json")
    parser.add_argument("--samples", type=int, default=7)
    arguments = parser.parse_args()
    if arguments.samples < 1:
        parser.error("samples must be positive")
    asyncio.run(benchmark(arguments.interpreter, arguments.samples))
