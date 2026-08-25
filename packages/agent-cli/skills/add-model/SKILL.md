---
name: add-model
description: Configure a local or hosted model, add an OpenRouter model, or register a newly released OpenAI or xAI model.
when-to-use: Use when the user wants to use a model at a URL, names a model from OpenRouter, or asks to add a newly released provider model.
argument-hint: <model name or endpoint>
---
# Add a model

Configure the requested model in `~/.haskell-agent/models.json` without putting
API keys in the file.

## Gather and verify

1. Determine which case applies:
   - a local or custom OpenAI Responses-compatible endpoint;
   - a model served through the built-in OpenRouter connection;
   - a newly released model served through the built-in OpenAI or xAI
     connection.
2. For a hosted or newly released model, use current provider documentation to
   verify the exact model identifier and that it supports the streaming
   Responses API expected by the selected connection. Do not guess identifiers.
3. For a custom endpoint, obtain the exact wire model name and a base URL that
   includes any API version prefix but does not end in `/responses`. Correct an
   obvious `http:/` or `https:/` typo to `http://` or `https://` and tell the
   user. The resulting request URL is `<base_url>/responses`.
4. Determine whether authentication is required. Store only the environment
   variable name in the config, never its secret value.
5. Determine the model's documented context window. Store it as a positive
   integer `context_window`; do not infer it from a display label.

Ask a concise follow-up question only for information that cannot be inferred
or verified.

## Update the overlay

Read the existing `~/.haskell-agent/models.json`. If it does not exist, create
this minimal document before adding entries:

```json
{
  "version": 1,
  "connections": {},
  "models": []
}
```

Preserve unrelated connections, models, and metadata. Update an existing model
entry with the same `id` instead of creating a duplicate. Keep exactly one
default model per built-in connection; do not set `default` unless the user
explicitly asks to change that provider's default.

### Local or custom endpoint

Choose a short, stable connection id containing only letters, digits, `.`, `_`,
or `-`, then add:

```json
{
  "connections": {
    "local-name": {
      "api": "responses",
      "base_url": "http://localhost:8000/v1",
      "api_key_optional": true,
      "request_timeout_seconds": 600
    }
  },
  "models": [
    {
      "id": "user-facing-model-id",
      "connection": "local-name",
      "model": "exact-wire-model-name",
      "dialect": "generic-responses",
      "context_window": 32768,
      "label": "local"
    }
  ]
}
```

For authenticated endpoints, replace `api_key_optional` with
`"api_key_env": "PROVIDER_API_KEY"` and tell the user which variable to export.
Use `codex` or `grok-build` only when the endpoint is known to require that
model-facing protocol; otherwise use `generic-responses`. Replace the
illustrative `32768` context window with the endpoint's documented value.

### OpenRouter

Do not add a connection. Add the exact OpenRouter slug as both the stable `id`
and wire model name by omitting `model`:

```json
{
  "id": "vendor/model-slug",
  "connection": "openrouter",
  "dialect": "generic-responses"
}
```

Use `codex` for OpenAI Codex-style models, `grok-build` for xAI Grok models,
and `generic-responses` for other model families. Add the model's verified
numeric `context_window` to the same object.

### Built-in OpenAI or xAI

Do not add or redefine a connection, and do not use the `model` field. For
OpenAI use:

```json
{
  "id": "exact-openai-model-id",
  "connection": "openai",
  "dialect": "codex"
}
```

For xAI use connection `xai` and dialect `grok-build`, and add its verified
numeric `context_window`. Built-in OpenAI compaction instead uses the
repository's Codex model metadata.

## Validate and finish

1. Validate that the edited file is syntactically valid JSON.
2. If `agent-cli` is available, start it or use its model picker to confirm the
   catalog loads and the new id appears. Do not send a billable inference
   request without user approval.
3. Tell the user the configured id, any environment variable they must export,
   and how to select it:

   `agent-cli --model <id>`

   They can also choose it with `/model` in a running session. If the session
   was already running, tell them to restart it so the catalog is reloaded.
