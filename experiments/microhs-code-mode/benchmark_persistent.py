"""Compare warm REPL cells with fresh MicroHs and Bun workers (trusted fixtures)."""

import argparse
import asyncio
import json
import statistics

from experiment import haskell_cell, run_bun, run_haskell, temporary_directory
from persistent import PersistentMicroHs


async def benchmark(samples):
    with temporary_directory() as directory:
        async with PersistentMicroHs(directory) as worker:
            print(json.dumps({"backend": "microhs-repl", "startup_ms": worker.startup_ms}), flush=True)
            for count in [10, 1000, 10000, 1000]:
                body = (
                    f'do {{ values <- callTool "fixture.numbers" (JsonObject [("count", JsonNumber "{count}")]); '
                    'case values of { '
                    'JsonArray entries -> emit (JsonNumber (show (sum [read value :: Integer | JsonNumber value <- entries]))); '
                    '_ -> error "expected array" } }')
                javascript = f'text((await tools.fixture.numbers({{count: {count}}})).reduce((total, value) => total + value, 0));'
                for backend in ["microhs-repl", "microhs-source", "bun-worker"]:
                    results = []
                    for _ in range(samples):
                        if backend == "microhs-repl":
                            result = await worker.execute(body)
                        elif backend == "microhs-source":
                            result = await run_haskell(haskell_cell(body), directory)
                        else:
                            result = await run_bun(javascript, directory)
                        actual = result["content"]
                        if backend == "bun-worker":
                            actual = [json.loads(block["text"]) for block in actual]
                        if ("error" in result["completion"] or result["calls"] != 1
                                or actual != [count * (count + 1) // 2]):
                            raise AssertionError(result)
                        results.append(result["elapsed_ms"])
                    print(json.dumps({"backend": backend, "count": count, "samples": samples,
                        "elapsed_ms": round(statistics.median(results), 3),
                        "raw_elapsed_ms": results}), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=7)
    arguments = parser.parse_args()
    if arguments.samples < 1:
        parser.error("samples must be positive")
    asyncio.run(benchmark(arguments.samples))
