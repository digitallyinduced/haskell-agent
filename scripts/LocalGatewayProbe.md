# Local catalog transport reproduction

This is a **transport isolation test**, not a complete CLI startup benchmark.
It exercises the `newTlsManager` → `httpLbs` sequence used by
`Agent.CLI.Gateway.Catalog`, with the same bearer/Accept headers, redirect refusal
and five-second request timeout. It deliberately avoids PostgreSQL, session
scratch allocation, UI, real credentials and production traffic.

The local HTTP/HTTPS server returns synthetic model catalogs without injected
latency. HTTPS uses an ephemeral localhost certificate, added only to the child
process's CA bundle; certificate verification remains enabled. No keychain,
global trust or user settings are changed.

## Run

From the repository root:

```sh
mkdir -p .startup-tmp
export TMPDIR="$PWD/.startup-tmp"
nix develop
# Quick typecheck first:
ghci -ignore-dot-ghci scripts/LocalGatewayProbe.hs -e ':quit'
# Optimized timings, not interpreted GHCi timings:
ghc -O2 -threaded -rtsopts -with-rtsopts=-N4 \
  -outputdir .startup-tmp/local-probe-build \
  scripts/LocalGatewayProbe.hs -o .startup-tmp/local-gateway-probe
python3 -B scripts/test-benchmark-local-gateway.py
python3 -B scripts/benchmark-local-gateway.py \
  --client .startup-tmp/local-gateway-probe --samples 7 --models 10
```

Repeat with `--models 1`, `100`, and `1000`, then repeat the 10-model case.
Stay inside the same Nix shell; shell evaluation/build time is not part of
request measurements.

Three controls run: `http-system-trust` preserves the caller's trust
environment; `http` and `https` use the temporary CA bundle. To explicitly
exercise macOS's default certificate-store path rather than a bundle supplied
by the Nix shell, also run:

```sh
env -u SSL_CERT_FILE python3 -B scripts/benchmark-local-gateway.py \
  --client .startup-tmp/local-gateway-probe --samples 7 --models 10
```

This changes only the benchmark subprocess environment, not user settings.

Each independent process makes two requests per manager and creates several
managers. Compare:

- `sample=0,reuse=0`: first manager and first request in a fresh process;
- `sample>0,reuse=0`: new manager/connection in an existing process;
- `reuse=1`: a second request using the same manager.

`manager_ms` measures manager creation, `http_ms` measures `httpLbs` including
connection setup and complete response-body reading, and `decode_ms` includes
generic Aeson decoding and re-encoding to force the whole result. `total_ms`
includes manager creation only for the first request on that manager.
The repeated `manager_ms` on reuse rows is descriptive, not additional work.
JSON decoding here is not the production catalog normalization algorithm.
Process wall time includes both requests, output, cleanup, loader and RTS.

## Interpretation

Do not simulate the unexplained 180 ms with a sleep and call it reproduced.
First establish whether a slow manager initialization or first TLS request
appears on loopback without delay. HTTP controls for TLS; reused connections
control for connection setup. The fixture does not reproduce WAN RTT,
production certificates/proxy/load, or the full launch-to-request critical path.
Only claim a reproduction when the measured slow stage is observed.

## Local results (2026-09-07, macOS ARM64)

Optimized probe, seven independent processes per transport/size, three
managers per process, two requests per manager. Median request totals in ms:

| Models | HTTP, first manager | HTTPS, first manager | HTTPS, later fresh manager | HTTPS, first connection reused |
|---:|---:|---:|---:|---:|
| 1 | 2.07 | 24.16 | 2.99 | 0.18 |
| 10 | 2.27 | 24.09 | 2.78 | 0.20 |
| 1000 | 2.89 | 24.47 | 4.25 | 1.41 |

An earlier independent 10-model run gave 25.28 ms for first HTTPS and
3.13 ms for a later fresh manager. The HTTP control with `SSL_CERT_FILE`
unset gave 2.18 ms for 10 models; no expensive eager manager initialization
was observed. This does not rule out certificate work deferred until TLS.

The reproducible finding is a roughly 21 ms process-first HTTPS overhead
relative to later fresh managers, not the full 180 ms catalog interval seen
in the CLI trace. The remaining gap needs the actual CLI/catalog path and
its environment reproduced; these results do not justify a server-side
optimization or a claim that startup is fixed. Six fixture/parser tests pass.

## Deeper investigation: client socket behavior

Verified the actual benchmark/CLI dependencies: GHC 9.10.3,
`http-client-tls-0.3.6.4`, `crypton-connection-0.4.5`,
`crypton-x509-system-1.6.8`, `tls-2.1.8`.

The catalog refresh constructs a fresh, uncontended MVar and performs pure
credential validation, followed by `newTlsManager` and `httpLbs`. There is no
hidden database query or credential refresh in the catalog timing interval.

Two relevant dependency behaviors:

1. `http-client-tls` passes a lazy, process-global `ConnectionContext` created
   by `unsafePerformIO initConnectionContext`. Its CA store can be forced
   during first TLS rather than manager creation. `CertificateStoreProbe.hs`
   directly loads and forces the certificate list: roughly 23 ms for 162
   certificates on this machine. Merely timing HTTP cannot exclude this cost.
2. `crypton-connection`'s `resolve'/tryToConnect` creates and connects the
   socket **without setting `NoDelay`**. Small consecutive TLS/application
   writes are consequently subject to Nagle/ACK delays.

`GatewayTLSProbe.hs` isolates DNS, TCP, TLS, and the subsequent HTTP status-line
wait. It uses the same connection/TLS dependencies and verified certificates,
with alternating `TCP_NODELAY=0/1`. It sends a credential-free GET to the fixed
production origin, not a model request, and prints neither response nor tokens.
This is a transport diagnostic, not an authenticated full catalog benchmark.

```sh
# Inside nix develop:
ghci -ignore-dot-ghci scripts/GatewayTLSProbe.hs -e ':quit'
ghc -O2 -threaded -rtsopts -with-rtsopts=-N4 \
  -outputdir .startup-tmp/tls-probe-build \
  scripts/GatewayTLSProbe.hs -o .startup-tmp/gateway-tls-probe
.startup-tmp/gateway-tls-probe
```

First alternating run (four samples per setting): post-TLS status-line wait
was 71.88–84.77 ms with Nagle enabled versus 16.01–21.49 ms with `TCP_NODELAY`.
TCP and TLS timings remained similar. This experimentally identifies a
substantial client-side socket-option penalty; packet-level ACK timing is not
captured by this probe. Loopback's near-zero RTT masked this behavior.

A second alternating run reproduced the difference. Across both runs (eight
samples per setting), median post-TLS wait was **78.65 ms → 18.48 ms**;
median total through status line was **151.12 ms → 80.47 ms**. These are
credential-free transport measurements, not authenticated catalog or
launch-to-first-request results. Raw runs are in the local
`.startup-tmp/tls-stages{,-repeat}.jsonl` artifacts.

The fix should target the HTTPS socket creation path while preserving proxy
support, address fallback, certificate validation and cleanup—not bypass
discovery authorization or weaken TLS. Validate the actual catalog and
launch-to-first-request path before claiming the complete 180 ms is explained
or that a production fix is ready.

## Dependency patch and authenticated validation

`flake.nix` now applies `patches/crypton-connection-nodelay.patch`.
It sets `NoDelay` inside the existing `bracketOnError`, before connect.
Failures still close the socket and enter the existing address fallback.
No TLS verification, authorization, or proxy selection code is replaced.
This affects consumers of this flake's patched dependency; an already installed
CLI or a Cabal build using an old dependency does not acquire the fix.

`GatewayCatalogProbe.hs` uses `newTlsManager`/`httpLbs`, the stored credential
only after checking its production origin, no redirects, and full JSON forcing.
It prints only elapsed time and encoded JSON length (called `checksum`, not a
cryptographic content hash). Transport errors are redacted. Credential-file IO
is outside the interval; first-use TLS trust initialization remains inside it.

Build this same source in baseline and patched Nix shells, with distinct output
directories and executables:

```sh
ghci -ignore-dot-ghci scripts/GatewayCatalogProbe.hs -e ':quit'
ghc -O2 -threaded -rtsopts -with-rtsopts=-N4 \
  -outputdir .startup-tmp/catalog-new-build \
  scripts/GatewayCatalogProbe.hs -o .startup-tmp/catalog-new
# Baseline: same command in the unpatched shell, using catalog-old-build/catalog-old.
# Both comparisons run inside nix develop:
python3 -B scripts/benchmark-catalog-pair.py \
  .startup-tmp/catalog-old .startup-tmp/catalog-new
```

Two runs, seven independent processes per implementation each, alternating
order, authenticated GET `/v1/models`, 1,102-byte re-encoded JSON:

| Run | Baseline median | Patched median | Difference |
| --- | ---: | ---: | ---: |
| First | 462.177 ms | 408.445 ms | -53.732 ms |
| Repeat | 202.825 ms | 146.698 ms | -56.127 ms |

Absolute latency was noisy; both runs support a roughly 55 ms reduction in this
catalog workload, not an absolute latency target. Raw samples:
`.startup-tmp/catalog-pair{,-repeat}.jsonl`. This is still not a full
launch-to-first-request measurement.

Patched local HTTP/TLS fixtures also ran with 1, 10, and 1,000 models.
Seven fixture/parser tests pass, including rejection of an untrusted certificate
by the patched Haskell client:

```sh
LOCAL_GATEWAY_CLIENT="$PWD/.startup-tmp/local-gateway-nodelay" \
  python3 -B scripts/test-benchmark-local-gateway.py
```
