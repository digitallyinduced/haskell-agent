# Voice latency measurements

Run from the repository root inside the Nix development shell:

```sh
cc -O2 -Wall -Wextra -Werror packages/agent-webrtc/benchmark/latency.c \
  -o "$TMPDIR/voice-media-latency" \
  $(pkg-config --cflags --libs gstreamer-webrtc-1.0 gstreamer-sdp-1.0 gstreamer-app-1.0)
"$TMPDIR/voice-media-latency"
python3 packages/agent-webrtc/benchmark/playback_latency.py
```

These are component benchmarks, **not end-to-end acoustic response measurements**.
Neither opens the microphone or plays sound through a physical device.

## Local results (2026-09-11, Apple Silicon macOS)

Five interleaved samples per condition; PCM16 mono, 24 kHz, 20 ms frames.
The C benchmark compiles with optimization and uses the production peer code.

| Native WebRTC loopback | 200 ms jitter setting (default) | 50 ms candidate |
|---|---:|---:|
| First tone, no warmup, median | 204.300 ms | 55.099 ms |
| Tone after 1 s silence, median | 1.274 ms | 1.275 ms |

The clock starts at submission of the tone frame and stops when decoded PCM
crosses the amplitude threshold. This excludes device capture, network distance,
server turn detection, inference, and physical playback. The warm-stream result
does not imply Internet WebRTC has 1 ms latency. Reducing the setting improved
startup only in this workload, so it was not applied as a conversational fix.

An earlier playback experiment ran native FFplay 9.0 with the original raw-PCM input
options, adding `ashowinfo` and using SDL's dummy device. Python supplies paced
frames and timestamps the filter log; it does not implement the media pipeline.
Every sample decoded all 72,000 input samples.

| FFplay pipe to filter | Baseline | `-avioflags direct` candidate |
|---|---:|---:|
| Median steady offset | 78.234 ms | 78.204 ms |

This is filter arrival time, not DAC/speaker time. The difference is negligible;
the candidate was not applied. Do not add these component measurements together
and label the sum conversational latency. Bluetooth output and server response
time require separate measurements.

## CLI playback packet fix

FFmpeg's raw PCM demuxer requests 2048 samples at 24 kHz (85.3 ms),
independent of the producer's 20 ms writes. The CLI now supplies IEEE-float
WAV with explicit 1920-byte (20 ms) packets. The conversion preserves every
PCM16 value exactly. Integer WAV is unsuitable: FFmpeg probes 64 KiB for
SPDIF, introducing about 1.4 seconds of startup delay.

Five interleaved trials of `playback_latency.py`, native FFplay 9.0:
median steady pipe-to-filter offset **79.320 ms raw → 3.118 ms float WAV**.
All ten trials decoded all 72,000 samples. This does **not** establish a
76 ms reduction at the speaker: a separate three-pair SDL dummy callback
diagnostic showed medians 94.366 ms and 87.364 ms, with a 247.887 ms raw
startup outlier. Device buffering remains; physical output was not measured.
The native macOS app uses AVAudioEngine, not this CLI FFplay adapter.

## Live-path diagnostic

Three separate gateway/Codex calls using the same synthetic spoken request
("Hello. Please say the words audio test successful.") returned first non-silent
decoded PCM 1010.873, 1056.243, and 878.901 ms after the last non-silent input
frame was submitted (median 1010.873 ms). All three calls completed successfully;
none requested coding delegation. The amplitude threshold was 500 in signed
PCM16, with 20 ms input frames and one second of initial silence.

This was a GHCi functional diagnostic of the existing transport, **not an
optimized before/after performance benchmark**. It excludes microphone and
speaker/Bluetooth latency, includes Internet transit and server processing, and
cannot separate server end-of-turn detection from inference. No web-demo
comparison or latency improvement is established by these observations.
