# Clipboard metadata validation

Run from the repository root:

```sh
nix develop -c bash scripts/test-clipboard-types.sh
nix develop -c bash scripts/benchmark-clipboard-types.sh 7
```

Both programs compile the production metadata implementation with the Nix
development shell's C compiler at `-O2`. They create private named pasteboards;
they neither read nor replace the user's general clipboard.

The regression tests cover empty clipboards, browser URL and rich-text
representations, PNG/JPEG/TIFF with accompanying text, Finder file
representations, promised image data, and inspection failure. A promise keeper
asserts that metadata inspection never requests the image payload.

The benchmark compares native metadata inspection with a conservative model of
the removed three-process text-paste failure path. The baseline launches three
real `osascript` processes executing an empty return. It deliberately excludes
the additional latency of failed image/file coercion and Haskell temporary-file
handling. These are **subprocess-overhead measurements**, not end-to-end prompt
latency or an exact timing of the previous implementation.

Inputs contain 32, 1,024, and 65,536 bytes of URL-shaped text, with 32 bytes
repeated to check stability. Fixture creation is outside the measured interval.
Methods are interleaved, all classification results are checked, and a volatile
checksum keeps the result observable. Output reports median elapsed time and
parent-process CPU time over the requested samples; CPU excludes subprocess
CPU. This is a latency change, not a claimed allocation optimization.

## Reference measurement

2026-09-07, aarch64 macOS, Nix Clang 21.1.8, `-O2`, seven samples:

| Text bytes | Three-process elapsed ms | Native elapsed ms | Three-process CPU ms | Native CPU ms |
| ---: | ---: | ---: | ---: | ---: |
| 32 | 152.754 | 0.251 | 0.957 | 0.147 |
| 1,024 | 148.641 | 0.272 | 0.941 | 0.155 |
| 65,536 | 153.209 | 0.231 | 0.944 | 0.145 |
| 32 (repeat) | 156.091 | 0.218 | 0.905 | 0.136 |

The native check removes roughly 150 ms of process startup overhead in this
model, without reading the text payload. It adds a metadata query before
image/file candidates; their existing decoding and AppleScript work remains.
