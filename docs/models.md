# Model catalog and local models

The model picker merges the shipped OpenAI, xAI, OpenRouter, Meta, and Gemini
catalog
with:

```text
~/.haskell-agent/models.json
```

User entries with the same `id` replace shipped entries; new entries are
appended. The `id` is accepted by `/model` and `--model`. Secrets are not
stored in this file: connections name an environment variable containing the
API key.

The shipped Meta connection calls Meta Model API directly. Export
`MODEL_API_KEY`, then select `muse-spark-1.2` with
`agent-cli --model muse-spark-1.2` or from `/model`. The separate
`meta/muse-spark-1.2` entry uses OpenRouter.

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

Built-in provider credentials are configured separately from the catalog.
Selecting a Gemini entry in `/model` starts browser-based Google sign-in when
no Gemini credential exists; no API key is required. Google AI Studio API keys
remain available through `GOOGLE_API_KEY` or `GEMINI_API_KEY`.

Set `context_window` to the endpoint's documented token limit so `/compact`
can bound its summary request and installed snapshot. Inference works without
this metadata, but `/compact` refuses to guess a portable model's limit.

Supported dialects are:

- `codex` for Codex-style prompts and tools
- `grok-build` for the Grok Build protocol
- `generic-responses` for portable Responses-compatible models

Custom connections are selected manually and are not considered for automatic
billing fallback. Shipped connection names (`openai`, `xai`, `openrouter`,
`meta`, `gemini`, `claude-code`, and `organization-gateway`) are reserved.
Invalid catalogs are reported at startup.

When an organization gateway advertises a private alias that uses a
non-default model protocol, add gateway-only metadata without defining a
connection:

```json
{
  "version": 1,
  "models": [
    {
      "id": "company-coder",
      "connection": "organization-gateway",
      "dialect": "generic-responses",
      "context_window": 131072,
      "label": "company"
    }
  ]
}
```

Gateway-only entries are never added to direct-provider catalogs or pickers,
and they cannot remap the alias sent over the wire. The live `/v1/models`
response remains authoritative: metadata is used only when the gateway
advertises the same alias.

The built-in `add-model` skill handles requests such as “use the model running
at this URL”, “add this OpenRouter model”, or “OpenAI released a new model”.
Invoke it with `/add-model`, `$add-model`, or a natural-language request.
