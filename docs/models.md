# Model catalog and local models

The model picker merges the shipped OpenAI, xAI, and OpenRouter catalog with:

```text
~/.haskell-agent/models.json
```

User entries with the same `id` replace shipped entries; new entries are
appended. The `id` is accepted by `/model` and `--model`. Secrets are not
stored in this file: connections name an environment variable containing the
API key.

For example, an unauthenticated local server exposing the streaming OpenAI
Responses API at `POST /v1/responses` can be configured as:

```json
{
  "version": 1,
  "connections": {
    "ollama": {
      "api": "responses",
      "base_url": "http://localhost:11434/v1",
      "api_key_optional": true,
      "request_timeout_seconds": 600
    }
  },
  "models": [
    {
      "id": "qwen-local",
      "connection": "ollama",
      "model": "qwen2.5-coder:32b",
      "dialect": "generic-responses",
      "context_window": 32768,
      "label": "local"
    }
  ]
}
```

Select it with `agent-cli --model qwen-local` or from `/model`. For an
authenticated endpoint, set `"api_key_env": "MY_MODEL_API_KEY"` and export
that variable. Omit `"api_key_optional": true` when a key is required.

Set `context_window` to the endpoint's documented token limit so `/compact`
can bound its summary request and installed snapshot. Inference works without
this metadata, but `/compact` refuses to guess a portable model's limit.

Supported dialects are:

- `codex` for Codex-style prompts and tools
- `grok-build` for the Grok Build protocol
- `generic-responses` for portable Responses-compatible models

Custom connections are selected manually and are not considered for automatic
billing fallback. Built-in connection names (`openai`, `xai`, and
`openrouter`) are reserved. Invalid catalogs are reported at startup.

The built-in `add-model` skill handles requests such as “use the model running
at this URL”, “add this OpenRouter model”, or “OpenAI released a new model”.
Invoke it with `/add-model`, `$add-model`, or a natural-language request.
