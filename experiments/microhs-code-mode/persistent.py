"""A trusted-source, sequential adapter for the pinned MicroHs text REPL.

This is not a sandbox. Cells share a heap and process, and must not manipulate
stdin/stdout, fork children, or change REPL state outside the adapter.
"""

import asyncio
import inspect
import contextlib
import json
import os
import signal
import time

from experiment import DIRECTORY, MAXIMUM_BYTES, executable, execute_fixture


class PersistentMicroHs:
    def __init__(self, directory, startup_timeout=120, interpreter="mhs", modules=()):
        self.directory = directory
        self.interpreter = interpreter
        self.modules = modules
        self.startup_timeout = startup_timeout
        self.process = None
        self.closed = False
        self.buffer = bytearray()
        self.received_bytes = 0
        self.lock = asyncio.Lock()

    async def __aenter__(self):
        if self.process is not None or self.closed:
            raise RuntimeError("create a new MicroHs worker instead of reopening")
        started = time.perf_counter()
        try:
            async with asyncio.timeout(self.startup_timeout):
                self.process = await asyncio.create_subprocess_exec(
                    executable(self.interpreter), "-q", "--stdin",
                    "-i" + str(DIRECTORY / "haskell"),
                    cwd=self.directory, env={"TMPDIR": str(self.directory)},
                    stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.STDOUT, start_new_session=True,
                    limit=MAXIMUM_BYTES,
                )
                await self._prompt()
                await self._write("import CodeMode\n")
                await self._prompt()
                for module in self.modules:
                    if not module or any(not (character.isalnum() or character in "._") for character in module):
                        raise ValueError("invalid module name")
                    await self._write("import " + module + "\n")
                    await self._prompt()
                # Verify that import succeeded, rather than treating any prompt
                # (including one following an import error) as successful setup.
                result = await self.execute("pure ()")
                if "error" in result["completion"]:
                    raise RuntimeError(result)
                self.startup_ms = (time.perf_counter() - started) * 1000
                return self
        except BaseException:
            await self.close()
            raise

    async def __aexit__(self, *exception):
        await self.close()

    async def close(self):
        if self.closed:
            return
        self.closed = True
        if self.process is None:
            return
        with contextlib.suppress(ProcessLookupError):
            try:
                os.killpg(self.process.pid, signal.SIGKILL)
            except PermissionError:
                # Darwin can report EPERM for zombie-only process groups.
                await asyncio.wait_for(self.process.wait(), timeout=1)
                os.killpg(self.process.pid, signal.SIGKILL)
        self.process.stdin.close()
        with contextlib.suppress(BrokenPipeError, ConnectionResetError):
            await self.process.stdin.wait_closed()
        # Drain without retaining bytes: asyncio's process wait may otherwise
        # await a paused pipe transport after an output-limit failure.
        while await self.process.stdout.read(8192):
            pass
        await self.process.wait()

    async def _write(self, text):
        data = text.encode()
        if len(data) > MAXIMUM_BYTES:
            raise ValueError("input limit exceeded")
        self.process.stdin.write(data)
        await self.process.stdin.drain()

    async def _event(self):
        """Return a complete line or None for the pinned REPL's bare prompt."""
        while True:
            if self.buffer.startswith(b"> "):
                del self.buffer[:2]
                return None
            newline = self.buffer.find(b"\n")
            if newline >= 0:
                line = bytes(self.buffer[:newline])
                del self.buffer[:newline + 1]
                return line
            block = await self.process.stdout.read(8192)
            if not block:
                raise RuntimeError("MicroHs exited before prompt/completion")
            self.received_bytes += len(block)
            if self.received_bytes > MAXIMUM_BYTES:
                raise ValueError("REPL output limit exceeded")
            self.buffer.extend(block)

    async def _prompt(self):
        while await self._event() is not None:
            pass

    async def execute(self, expression, timeout=30, handler=execute_fixture):
        """Evaluate one single-line IO () expression, e.g. do { a; b }.

        Compile errors and caught runtime errors return an error completion and
        retain the process. Timeout, cancellation, and protocol errors close it;
        callers must create a replacement. Calls are serialized, never retried.
        """
        if "\n" in expression or "\r" in expression:
            raise ValueError("use a single-line expression with explicit braces")
        async with self.lock:
            if self.closed or self.process is None:
                raise RuntimeError("MicroHs worker is not open")
            self.received_bytes = len(self.buffer)
            started = time.perf_counter()
            content, calls, diagnostics = [], set(), []
            ready_ms, completion = None, None
            try:
                async with asyncio.timeout(timeout):
                    await self._write("runCell (" + expression + ")\n")
                    while True:
                        line = await self._event()
                        if line is None:
                            if completion is None:
                                if ready_ms is not None:
                                    raise ValueError("cell returned without completion")
                                completion = {"jsonrpc": "2.0", "id": "cell-1", "error": {
                                    "code": -32000, "message": "\n".join(diagnostics)}}
                            return {"content": content, "calls": len(calls),
                                    "completion": completion, "ready_ms": ready_ms,
                                    "elapsed_ms": (time.perf_counter() - started) * 1000}
                        if not line.startswith(b"{"):
                            diagnostics.append(line.decode(errors="replace"))
                            continue
                        message = json.loads(line)
                        if not isinstance(message, dict) or message.get("jsonrpc") != "2.0":
                            raise ValueError("invalid JSON-RPC frame")
                        method = message.get("method")
                        if completion is not None:
                            raise ValueError("frame after completion")
                        if method == "ready" and ready_ms is None:
                            ready_ms = (time.perf_counter() - started) * 1000
                        elif ready_ms is None:
                            raise ValueError("frame before ready")
                        elif method == "tool/call":
                            identifier, params = message.get("id"), message.get("params")
                            if not isinstance(identifier, str) or identifier in calls:
                                raise ValueError("invalid or duplicate call id")
                            if (not isinstance(params, dict)
                                    or not isinstance(params.get("name"), str)
                                    or "arguments" not in params):
                                raise ValueError("invalid tool parameters")
                            calls.add(identifier)
                            reply = {"jsonrpc": "2.0", "id": identifier}
                            try:
                                reply["result"] = handler(params["name"], params["arguments"])
                                if inspect.isawaitable(reply["result"]):
                                    reply["result"] = await reply["result"]
                            except ValueError as error:
                                reply.pop("result", None)
                                reply["error"] = {"code": -32000, "message": str(error)}
                            await self._write(json.dumps(reply, ensure_ascii=True, allow_nan=False) + "\n")
                        elif method == "content":
                            content.append(message["params"]["value"])
                        elif method is None and message.get("id") == "cell-1":
                            if ("result" in message) == ("error" in message):
                                raise ValueError("invalid completion")
                            completion = message
                        else:
                            raise ValueError("unexpected frame")
            except BaseException:
                await self.close()
                raise
