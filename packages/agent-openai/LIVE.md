# Codex Live voice calls (experimental)

The CLI `/voice` command and native macOS embedded-client `/voice` entry point
connect the shared transport to the current coding session. Hardware and live
service testing are still required; compilation or loopback tests alone do not
establish that a particular ChatGPT account can access this Codex endpoint.

## Trying it

Use a build containing both the updated native bridge and desktop sources.
Connect to your organization gateway (with its voice relay deployed), or sign
in locally with ChatGPT for direct mode. Use headphones and submit `/voice` in an idle
CLI or embedded desktop session. Wait for the connected status before speaking.
Ctrl-C in the CLI or Stop in the desktop ends the call and delegated work.
Tool approvals still require the usual on-screen approval; speech cannot grant
them. Dictation remains separate. CLI voice uses in-process GStreamer devices
(CoreAudio on macOS, PulseAudio/PipeWire's PulseAudio service on Linux);
the desktop uses AVAudioEngine and asks for microphone permission.

## Reference

Follows `openai/codex` revision
`818f1cca8ccf8899f0f4d59336baebaccf358eed`, specifically
`codex-rs/codex-api/src/endpoint/realtime_call.rs`,
`codex-rs/codex-api/src/endpoint/realtime_websocket/`, and
`codex-rs/core/src/realtime_conversation.rs`.

Calls use ChatGPT OAuth with
`POST https://chatgpt.com/backend-api/codex/realtime/calls?intent=quicksilver&architecture=avas`.
The bounded SDP answer establishes WebRTC audio; the returned call ID selects
the authenticated `/v1/live/{id}` sideband on `api.openai.com`. The sideband
does not send `session.update` or wait for `session.started`. It carries context
and delegation, not PCM. No API-key fallback or automatic call-creation retry
is performed. The coding model/provider remains unchanged. Gateway sessions
use the configured gateway's authenticated `/v1/voice` WebSocket for signaling
and control. The gateway selects and retains its upstream account; the client
never falls back to local ChatGPT accounts. WebRTC media still travels directly
to OpenAI. Direct-provider sessions use the local ChatGPT sign-in.

The gateway relay is deployed. The initial HTTP 403 was caused by selecting
`marin`, a public Realtime voice unsupported by Codex v3. Comparing the installed
Codex 0.153.4 app-server exposed the mismatch. Changing only the voice to
`juniper` produced HTTP 201 on the gateway's existing account, without device
attestation or desktop-specific headers. Both clients now default to `juniper`.
Call creation and synthetic spoken-request/PCM-response tests have passed. A user
confirmed a working CLI hardware conversation on 2026-09-11; this is not a
cross-device or desktop latency guarantee.
Local WebRTC loopback tests pass with GStreamer 1.26.11 and 1.28.5
outside sandbox isolation (ten PCM frames in each direction and joined cleanup).

`agent-webrtc` uses GStreamer for ICE, DTLS/SRTP and Opus. Nix supplies its
runtime plugins on macOS and Linux. Device-facing audio remains PCM16/24 kHz.

## Delegation and ownership

`delegation.created` carries a client-targeted item containing its ID and task
text. The voice layer has no tool definitions or tool executor. The session adapter
submits that task through the normal coding engine, retaining its tool
approval policy. Progress and results return through
`delegation.context.append` with the same item ID; unrelated context uses
`session.context.append`. Payloads are split at 500-byte UTF-8 boundaries.

`withLiveDelegation` supplies a scoped serial worker and bounded pending queue.
It suppresses duplicate IDs for the call lifetime. The host must bind it to a
specific coding session, correlate the actual coding turn, and define how
hangup detaches from or cancels that turn. Merely accepting a prompt is not a
successful coding result. Do not forward reasoning or arbitrary tool output as
spoken text. Never treat spoken content as approval for a pending permission.

`runLiveCall` combines the transport and delegation worker with bounded audio
and outbound queues. Its host scope receives an opaque call handle immediately
so hangup works during connection establishment. Hosts may initialize capture
early with `submitLiveAudioWhenReady`, which discards setup samples, or wait for
`awaitLiveStarted`. `submitLiveAudio` rejects
pre-start/stopped submissions, incomplete samples, and microphone overrun;
`readLiveAudio` supplies playback without duplicating audio into UI events.
Capture retention is at most one second and playback retention at most two
seconds (both also have a 100-chunk cap). Playback overrun fails the call rather
than blocking its receive loop. Hangup discards pending audio and joins workers.

Hosts must stop their actual devices on all exit paths and cancel/join the call
on session switching and application shutdown. A CLI GStreamer device adapter is
implemented in `Agent.CLI.Voice.Audio`; it initializes capture during signaling,
passes PCM directly without subprocesses, and scopes both devices
to the call. Interrupting playback flushes and closes its device while capture
continues. Each device queue is bounded to half a second. It requires headphones
(no acoustic echo cancellation). Call
controls use the existing session cancellation path. The native audio adapter
uses a typed, callback-scoped C bridge, not UI event payloads for PCM.
Audio is signed PCM16 little-endian, mono, 24 kHz. Playback interruption is
integrated as described below; CLI echo cancellation remains unimplemented.
Dictation's full-recording buffer is
not suitable for an indefinite call. API keys are not accepted by the CLI's
Codex voice transport.

## Validation

### Interruption

The first nonempty user transcript discards queued playback while microphone
capture continues. Assistant transcripts retain their input generation, so a
late completion from an interrupted response cannot resume its audio. CLI
playback replaces the scoped FFplay process; native playback clears the
AVAudioPlayerNode buffers without restarting microphone capture.

Interruption follows the server transcript signal, not a local energy threshold.
This does not establish that speech-onset detection matches Codex or the web
demo. CLI capture does not currently provide acoustic echo cancellation; use
headphones for overlap testing. Hardware overlap testing remains necessary.

### Latency

Component benchmarks and measured limitations are documented in
[`agent-webrtc/benchmark`](../agent-webrtc/benchmark/README.md). They distinguish
first-stream startup from steady streaming and filter arrival from physical
playback. Queue size limits describe maximum retention, not a mandatory delay.

The session adapter currently returns a delegated task's final answer only after
the normal coding turn finishes. Tool execution and approvals can therefore add
substantial time to a coding request, even while the voice connection remains
responsive. Ordinary conversational replies need to be measured separately from
delegated coding work. No comparison against the public web demo has yet been
measured under equivalent conditions.

### Concurrent startup

Gateway connection overlaps WebRTC offer gathering. CLI and native capture
initialize alongside signaling; valid setup samples are discarded by
`submitLiveAudioWhenReady`, never queued for later transmission. CLI playback
waits for actual microphone capture (Bluetooth duplex switching) and media
connection. Hangup cancels and joins preparation workers. `Voice setup:` logs
report elapsed stage times without SDP or credentials. Real-device startup
savings have not yet been measured.

Local tests cover event decoding, UTF-8 splitting, delegation correlation,
duplicate suppression, bounded audio, pre-start rejection, device-scope cleanup,
hangup during connection establishment, exception sanitization, and a loopback
WebSocket conversation. They do not verify the live OpenAI service or audio
hardware.

Run within `nix develop`:

```sh
cabal repl agent-openai:test:agent-openai-test --offline
```

```haskell
import Test.Hspec
import qualified Agent.OpenAI.LiveSpec
hspec Agent.OpenAI.LiveSpec.spec
```
