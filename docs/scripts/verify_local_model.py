"""Verify a running local Ollama Responses endpoint, not the agent harness.

Uses only Python's standard library. Does not download models, invoke a shell,
read project files, or contact a hosted model.
"""

import argparse
import json
import secrets
import urllib.request
from urllib.parse import urlsplit


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--endpoint", default="http://127.0.0.1:11434")
    parser.add_argument("--model", default="qwen3:0.6b")
    arguments = parser.parse_args()
    endpoint = arguments.endpoint.rstrip("/")
    address = urlsplit(endpoint)
    if (
        address.scheme != "http"
        or address.hostname not in {"127.0.0.1", "localhost", "::1"}
        or address.username
        or address.password
        or address.path
        or address.query
        or address.fragment
    ):
        parser.error("--endpoint must be an unauthenticated loopback HTTP origin")

    # Do not send local verification traffic through an environment HTTP proxy.
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    with opener.open(endpoint + "/api/version", timeout=10) as response:
        print("Ollama version:", json.load(response)["version"])
    with opener.open(endpoint + "/api/tags", timeout=10) as response:
        models = json.load(response)["models"]
    selected = next(
        (model for model in models if model["name"] == arguments.model), None
    )
    if selected is None:
        raise RuntimeError(f"Model is not installed: {arguments.model}")
    print("Model:", selected["name"], "digest:", selected["digest"])

    def create_response(input_items, tools=None):
        payload = {
            "model": arguments.model,
            "input": input_items,
            "stream": True,
            "store": False,
            "max_output_tokens": 256,
            "reasoning": {"effort": "none"},
        }
        if tools is not None:
            payload["tools"] = tools
        request = urllib.request.Request(
            endpoint + "/v1/responses",
            data=json.dumps(payload).encode(),
            headers={"Content-Type": "application/json"},
        )
        completed = None
        with opener.open(request, timeout=120) as response:
            for line in response:
                if not line.startswith(b"data:"):
                    continue
                data = line[5:].strip()
                if data == b"[DONE]":
                    continue
                event = json.loads(data)
                if event.get("type") in {"error", "response.failed"}:
                    raise RuntimeError(f"Response failed: {event}")
                if event.get("type") == "response.completed":
                    completed = event["response"]
        if completed is None:
            raise RuntimeError("Stream ended without response.completed")
        return completed

    def output_text(response):
        return "".join(
            part.get("text", "")
            for item in response["output"]
            if item.get("type") == "message"
            for part in item.get("content", [])
            if part.get("type") == "output_text"
        )

    greeting = create_response("Reply with a short greeting.")
    if not output_text(greeting).strip():
        raise RuntimeError("Generation completed without visible text")
    print("Streaming generation: passed")

    tools = [
        {
            "type": "function",
            "name": "read_verification_value",
            "description": "Return the verification value. Call with no arguments.",
            "parameters": {
                "type": "object",
                "properties": {},
                "additionalProperties": False,
            },
        }
    ]
    instructions = [
        {
            "role": "user",
            "content": (
                "Call read_verification_value to obtain the verification value. "
                "Do not guess it. After receiving the result, repeat it exactly."
            ),
        }
    ]
    call_response = create_response(instructions, tools)
    calls = [
        item for item in call_response["output"]
        if item.get("type") == "function_call"
    ]
    if len(calls) != 1 or calls[0].get("name") != "read_verification_value":
        raise RuntimeError(f"Expected one verification function call, got {calls}")
    call = calls[0]
    if json.loads(call["arguments"]) != {}:
        raise RuntimeError("Verification function received unexpected arguments")
    value = "verification-" + secrets.token_hex(8)
    continuation = instructions + call_response["output"] + [
        {
            "type": "function_call_output",
            "call_id": call["call_id"],
            "output": value,
        }
    ]
    result = create_response(continuation)
    if value not in output_text(result):
        raise RuntimeError("Model did not report the supplied function result")
    print("Function-call and result replay: passed")
    print("This verifies the wire API only; run the documented agent README test separately.")


if __name__ == "__main__":
    main()
