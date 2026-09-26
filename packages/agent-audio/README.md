# Audio application components

`agent-audio` provides bounded OGG voice-recording conversion and multipart
transcription transport without dependencies on the agent runtime, accounts,
provider discovery, or application persistence.

`Agent.Audio.Transcription.transcriptionRequest` takes an already admitted
request, a random multipart boundary, a model, and bounded WAV data.
`performTranscriptionRequest` makes one request, refuses redirects, applies a
two-minute overall deadline, bounds success and failure response bodies, and
returns sanitized transport failures. Asynchronous cancellation is propagated.
Neither function selects credentials or substitutes another provider.

Credential origin validation, credential-change leases, status interpretation,
transcript policy, and user-facing errors remain with the caller. The runtime
gateway dictation adapter retains its streaming-first behavior and HTTP
fallback; application adapters can use HTTP directly.

`Agent.Audio.Conversion.convertOggToWav` accepts caller-owned private paths and
requires `ffmpeg` on `PATH`. It admits regular OGG inputs up to 20 MiB, limits
conversion to one minute, and rejects recordings exceeding ten minutes of
16-kHz mono PCM. The application owns temporary directory creation and cleanup.

Run focused tests inside the repository Nix development shell:

```sh
cabal repl agent-audio:test:agent-audio-tests
# In GHCi:
main
```
