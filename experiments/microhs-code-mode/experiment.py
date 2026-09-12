"""Trusted-source experiment only: no real tools, credentials, or OS sandbox."""

import argparse
import asyncio
import contextlib
import inspect
import json
import os
from pathlib import Path
import resource
import shutil
import signal
import statistics
import tempfile
import time


DIRECTORY = Path(__file__).resolve().parent
REPOSITORY = DIRECTORY.parent.parent
MAXIMUM_BYTES = 4 * 1024 * 1024


def execute_fixture(name, arguments):
    """Fixed, side-effect-free allowlist. Never dispatch a real harness tool."""
    if name == "fixture.echo":
        return arguments
    if name == "fixture.numbers":
        count = arguments.get("count") if isinstance(arguments, dict) else None
        if type(count) is not int or not 0 <= count <= 10000:
            raise ValueError("count must be an integer between 0 and 10000")
        return list(range(1, count + 1))
    if name == "fixture.denied":
        raise ValueError("fixture approval denied")
    raise ValueError("tool is not available: " + name)


async def exchange(command, directory, request=None, timeout=30, handler=execute_fixture):
    """Run a bounded protocol exchange and always terminate/join its process group."""
    started = time.perf_counter()
    usage_before = resource.getrusage(resource.RUSAGE_CHILDREN)
    process = await asyncio.create_subprocess_exec(
        *command, cwd=directory, env={"TMPDIR": str(directory)},
        stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE, start_new_session=True,
        limit=MAXIMUM_BYTES,
    )
    content = []
    calls = []
    diagnostic = bytearray()
    received_bytes = 0

    async def read_diagnostic():
        while block := await process.stderr.read(8192):
            diagnostic.extend(block)
            if len(diagnostic) > MAXIMUM_BYTES:
                raise ValueError("stderr output limit exceeded")

    async def send(message):
        process.stdin.write((json.dumps(message, ensure_ascii=True, allow_nan=False) + "\n").encode())
        await process.stdin.drain()

    async def receive():
        nonlocal received_bytes
        line = await process.stdout.readline()
        if not line:
            await process.wait()
            raise RuntimeError("worker exited before completion: " + diagnostic.decode(errors="replace"))
        received_bytes += len(line)
        if received_bytes > MAXIMUM_BYTES:
            raise ValueError("stdout output limit exceeded")
        message = json.loads(line)
        if not isinstance(message, dict) or message.get("jsonrpc") != "2.0":
            raise ValueError("invalid JSON-RPC message")
        return message

    async def conversation():
        if (await receive()).get("method") != "ready":
            raise ValueError("worker did not send ready")
        ready_ms = (time.perf_counter() - started) * 1000
        if request is not None:
            await send(request)
        while True:
            message = await receive()
            method = message.get("method")
            if method == "tool/call":
                identifier = message.get("id")
                parameters = message.get("params")
                if not isinstance(identifier, str) or identifier in calls:
                    raise ValueError("invalid or duplicate tool request id")
                if not isinstance(parameters, dict) or not isinstance(parameters.get("name"), str):
                    raise ValueError("invalid tool parameters")
                if "arguments" not in parameters:
                    raise ValueError("missing tool arguments")
                calls.append(identifier)
                reply = {"jsonrpc": "2.0", "id": identifier}
                try:
                    reply["result"] = handler(parameters["name"], parameters["arguments"])
                    if inspect.isawaitable(reply["result"]):
                        reply["result"] = await reply["result"]
                except ValueError as error:
                    reply.pop("result", None)
                    reply["error"] = {"code": -32000, "message": str(error)}
                await send(reply)
            elif method == "content":
                content.append(message["params"]["value"])
            elif method is None and message.get("id") == "cell-1":
                if ("result" in message) == ("error" in message):
                    raise ValueError("completion must contain exactly one result or error")
                return {"content": content, "calls": len(calls), "completion": message,
                        "ready_ms": ready_ms,
                        "elapsed_ms": (time.perf_counter() - started) * 1000}
            else:
                raise ValueError("unexpected worker message")

    try:
        async with asyncio.timeout(timeout):
            async with asyncio.TaskGroup() as group:
                diagnostic_task = group.create_task(read_diagnostic())
                result = await conversation()
                diagnostic_task.cancel()
    finally:
        # Kill the whole group even if its leader has already exited.
        with contextlib.suppress(ProcessLookupError):
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except PermissionError:
                # Darwin reports EPERM for a zombie-only group. Reap and
                # retry, without swallowing permission errors on live groups.
                await asyncio.wait_for(process.wait(), timeout=1)
                os.killpg(process.pid, signal.SIGKILL)
        await process.wait()
        process.stdin.close()
    usage_after = resource.getrusage(resource.RUSAGE_CHILDREN)
    result["cpu_ms"] = 1000 * (
        usage_after.ru_utime + usage_after.ru_stime
        - usage_before.ru_utime - usage_before.ru_stime
    )
    return result


def executable(name):
    path = shutil.which(name)
    if path is None:
        raise RuntimeError(f"{name} missing; enter nix develop .#microhs-code-mode")
    return str(Path(path).absolute())


async def run_haskell(source, directory, cache=False, timeout=30, handler=execute_fixture):
    source_path = Path(directory) / "Cell.hs"
    source_path.write_text(source)
    command = [executable("mhs"), "-q", "-i" + str(DIRECTORY / "haskell")]
    if cache:
        command.append("-C" + str(Path(directory) / "compilation-cache"))
    command += [str(source_path), "-r"]
    return await exchange(command, directory, timeout=timeout, handler=handler)


async def run_bun(source, directory, timeout=30, handler=execute_fixture, tools=None):
    command = [executable("bun"), "--smol", "--no-install", "--no-env-file", "--no-addons",
               str(REPOSITORY / "packages/agent-core/data/code-mode/worker.mjs")]
    request = {"jsonrpc": "2.0", "id": "cell-1", "method": "exec", "params": {
        "source": source, "tools": tools if tools is not None else ["fixture.echo", "fixture.numbers", "fixture.denied"],
        "stored_values": {}, "image_detail_visible": True}}
    return await exchange(command, directory, request=request, timeout=timeout, handler=handler)


def haskell_cell(body):
    return "module Main(main) where\nimport CodeMode\nmain :: IO ()\nmain = runCell $ do\n" + "\n".join(
        "  " + line for line in body.splitlines()) + "\n"


def temporary_directory():
    return tempfile.TemporaryDirectory(prefix="microhs-code-mode-", dir=os.environ["TMPDIR"])


async def benchmark(samples):
    """Fresh worker each sample; cached means disk compilation cache, not warm VM."""
    for count in [10, 1000, 10000, 1000]:
        haskell = haskell_cell(
            f'values <- callTool "fixture.numbers" (JsonObject [("count", JsonNumber "{count}")])\n'
            'case values of\n'
            '  JsonArray entries -> emit (JsonNumber (show (sum [read value :: Integer | JsonNumber value <- entries])))\n'
            '  _ -> error "expected array"')
        javascript = f'text((await tools.fixture.numbers({{count: {count}}})).reduce((total, value) => total + value, 0));'
        for backend in ["microhs-source", "microhs-cached", "bun-worker"]:
            with temporary_directory() as directory:
                async def sample():
                    if backend == "bun-worker":
                        return await run_bun(javascript, directory)
                    return await run_haskell(haskell, directory, cache=backend == "microhs-cached")
                if backend == "microhs-cached":
                    await sample()
                results = []
                for _ in range(samples):
                    result = await sample()
                    if "error" in result["completion"]:
                        raise RuntimeError(result)
                    expected = count * (count + 1) // 2
                    # Bun text() wraps its output in a content block.
                    actual = result["content"]
                    if backend == "bun-worker":
                        actual = [json.loads(block["text"]) for block in actual]
                    if actual != [expected] or result["calls"] != 1:
                        raise AssertionError(result)
                    results.append(result)
                print(json.dumps({"backend": backend, "count": count, "samples": samples,
                    **{field: round(statistics.median(result[field] for result in results), 3)
                       for field in ["ready_ms", "elapsed_ms", "cpu_ms"]}}), flush=True)


async def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", nargs="?", type=Path, help="trusted Haskell module with main = runCell ...")
    parser.add_argument("--benchmark", action="store_true")
    parser.add_argument("--samples", type=int, default=7)
    arguments = parser.parse_args()
    if arguments.samples < 1:
        parser.error("samples must be positive")
    if arguments.benchmark:
        await benchmark(arguments.samples)
    else:
        source = arguments.source or DIRECTORY / "haskell/Example.hs"
        with temporary_directory() as directory:
            result = await run_haskell(source.read_text(), directory)
        print(json.dumps(result, indent=2))
        if "error" in result["completion"]:
            raise SystemExit(1)


if __name__ == "__main__":
    asyncio.run(main())
