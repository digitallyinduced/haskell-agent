# Local model tutorial verification

## Scope

`src/Documentation/Pages/LocalModelTutorial.hs` documents a Nix-pinned Ollama
installation and a separate Haskell Agent file-reading exercise. The server's
wire protocol and the harness are separate verification targets.

The optional `scripts/VerifyLocalModel.hs` checks a running loopback Ollama
server with an already-downloaded model. It does not download weights or execute
model-generated code. Its only function implementation returns a generated
verification string. It checks:

1. Server version and installed model digest.
2. Streaming Responses generation ending in `response.completed`.
3. A function call with the expected name and empty arguments.
4. Stateless replay of the function result and a response containing that value.

Run from the repository root after the tutorial's server and download steps:

```sh
nix develop .#docs -c runghc docs/scripts/VerifyLocalModel.hs
```

This does **not** establish that Haskell Agent can use the model. The documented
README exercise must additionally produce a real file-reading tool call and the
fixture heading, rather than a prose claim to have read it.

## Verification record, 2026-09-22

- Platform: Apple silicon macOS.
- Nixpkgs revision: `afe3d8ac4395617bdcdac9f188ac8717a062e014`.
- Ollama package and live `/api/version`: `0.32.13`.
- Server start on loopback: passed, with isolated home/model directories under
  the session temporary directory.
- Responses request decoding: returned the expected missing-model error for
  `qwen3:0.6b`; this is not evidence of successful inference.
- Model registry manifest: weights and metadata total 522,653,277 bytes.
- Tutorial HSX module: loaded successfully under `nix develop .#docs` and GHCi.
- Model download: passed using `ollama pull qwen3:0.6b`.
- Model digest:
  `7df6b6e09427a769808717c0a93cadc4ae99ed4eb8bf5ca557c90846becea435`.
- `ollama show`: completion, tools, and thinking capabilities; reported model
  context limit 40,960. Server explicitly configured for 32,768.
- The tutorial's exact curl generation request: passed with visible greeting
  and `response.completed`.
- The original Python verifier (now `VerifyLocalModel.hs`): passed streaming generation, function-call decoding,
  and stateless function-result replay with a newly generated verification value.
- Cloud features disabled for the inference run with `OLLAMA_NO_CLOUD=1`.
- Harness file-reading exercise: not executed; wire-protocol success alone is
  not reported as end-to-end harness compatibility.
- Temporary server: stopped after verification.

Compatibility claims were checked against Ollama's OpenAI compatibility
reference and its Responses implementation, and the harness's
`Agent.Responses.Request.forceStatelessStreaming` implementation.
