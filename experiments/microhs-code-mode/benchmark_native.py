"""Compare JSON decoders in the same persistent MicroHs process."""

import argparse
import asyncio
import json
import statistics

from experiment import temporary_directory
from persistent import PersistentMicroHs


async def benchmark(interpreter, samples):
    with temporary_directory() as directory:
        async with PersistentMicroHs(directory, interpreter=interpreter,
                                     modules=("CodeMode.NativeJson",)) as worker:
            probe = await worker.execute('decodeJsonNative "null" >>= either error emit')
            if probe["content"] != [None] or "error" in probe["completion"]:
                raise AssertionError(probe)
            print(json.dumps({"startup_ms": worker.startup_ms}), flush=True)
            for count in [10, 1000, 10000, 1000]:
                measurements = {"readp": [], "native": []}
                for sample in range(samples):
                    # Alternate order to avoid assigning all drift to one decoder.
                    order = ["readp", "native"] if sample % 2 == 0 else ["native", "readp"]
                    for backend in order:
                        decoder = "(pure . decodeJson)" if backend == "readp" else "decodeJsonNative"
                        expression = (
                            f'do {{ values <- callToolWithDecoder {decoder} "fixture.numbers" '
                            f'(JsonObject [("count", JsonNumber "{count}")]); '
                            'case values of { JsonArray entries -> emit (JsonNumber '
                            '(show (sum [read value :: Integer | JsonNumber value <- entries]))); '
                            '_ -> error "expected array" } }')
                        result = await worker.execute(expression)
                        if (result["content"] != [count * (count + 1) // 2]
                                or result["calls"] != 1 or "error" in result["completion"]):
                            raise AssertionError(result)
                        measurements[backend].append(result["elapsed_ms"])
                for backend, durations in measurements.items():
                    print(json.dumps({"backend": backend, "count": count, "samples": samples,
                                      "elapsed_ms": statistics.median(durations),
                                      "raw_elapsed_ms": durations}), flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--interpreter", required=True)
    parser.add_argument("--samples", type=int, default=7)
    arguments = parser.parse_args()
    if arguments.samples < 1:
        parser.error("samples must be positive")
    asyncio.run(benchmark(arguments.interpreter, arguments.samples))
