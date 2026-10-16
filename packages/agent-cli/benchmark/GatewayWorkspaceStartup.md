# Gateway and worktree startup

Fresh managed-worktree startup prepares the local checkout, upstream fetch,
credentials, authoritative gateway catalog, and host integration acquisition
concurrently. The local path can be displayed immediately. The composer and
its input/attachment queue remain available throughout preparation.

Workspace discovery, project settings, AGENTS.md, skills, tool initialization,
and the first agent action start only after the upstream revision is installed
in the untouched checkout. The fetched commit is pinned by a private Git ref.
The ready result is cached, so later calls cannot reset an active checkout.
Unexpected files, branch changes, or commits stop the handoff and retain the
checkout. Cancellation joins the fetch, terminates its process group, removes
private refs, and removes only an unchanged checkout still owned by startup.
Enrollment starts after synchronization. Disabled fetching and resumed sessions
retain their direct startup paths.

## Reproducing the measurements

`GatewayWorkspaceStartup.hs` compares three schedules in one optimized binary:

* `serial`: complete worktree preparation, then refresh the gateway catalog.
* `prepared-catalog`: overlap the catalog request with synchronous worktree setup.
* `prepared-workspace`: also overlap local checkout creation and upstream fetch.

All schedules use the production Git and gateway implementations. Each triplet
must produce the same final commit and normalized gateway catalog. The order
rotates between samples. Total time includes removing the sample's checkout and
branch; separate timestamps record local-checkout availability and synchronized
checkout availability. No model turn is sent. This benchmark isolates Git and
catalog work; it does not include credentials, integrations, or the remaining
CLI initialization. With fetching disabled, the two prepared schedules use the
same direct worktree path.

```sh
nix develop -c cabal build --offline \
  agent-cli:bench:gateway-workspace-startup-bench agent-cli:exe:agent-cli
benchmark=$(nix develop -c cabal list-bin agent-cli:bench:gateway-workspace-startup-bench)
"$benchmark" "$PWD" local 7 +RTS -N4 -M8G -RTS
"$benchmark" "$PWD" remote 7 +RTS -N4 -M8G -RTS
"$benchmark" "$PWD" remote 7 +RTS -N4 -M8G -RTS
```

Use an existing organization-gateway login. `local` starts at local HEAD;
`remote` fetches the configured remote's default branch. Temporary checkouts
stay under `$TMPDIR` and are removed after each sample. Network failure, a
changed upstream commit, or a changed gateway catalog aborts the comparison.

For process-to-ready measurements, `scripts/benchmark-startup.py` starts the
compiled CLI in a real PTY, types an unsubmitted probe, and waits for the real
startup-ready marker. It records first-frame, editable-composer, and ready
times. `--current-home` uses existing configuration and credentials and cannot
be combined with `--cold-store`.

```sh
binary=$(nix develop -c cabal list-bin agent-cli:exe:agent-cli)
nix develop -c python3 scripts/benchmark-startup.py --current-home \
  --samples 7 --warmups 1 --output-dir "$TMPDIR/startup-current" \
  -- "$binary" --yolo --worktree
```

Managed worktree measurements create ordinary session checkouts. Record their
exact paths and final commits; remove them only after checking identity, clean
state including ignored files, inactive leases, and enrollment metadata.
Timing output includes Git inspection/fetch/checkout stages alongside workspace
discovery, routing, credentials, tools/context, and provider connection.

## Validation and environment

Measurements on 2026-10-16 use an Apple M3 Max, aarch64 macOS, GHC 9.10.3,
optimized Cabal `-O1` libraries/executable and `-O2` benchmark main, with
`+RTS -N4 -M8G`. The installed baseline is revision `0eaf86af4`. The repository
has 1,783 tracked files. Remote samples fetch GitHub over SSH and refresh 14
gateway aliases. These are elapsed I/O timings; network latency varies.

Seven rotating triplets per workload produced these medians, in milliseconds:

| Workload | Serial | Catalog overlap | Checkout/fetch overlap |
| --- | ---: | ---: | ---: |
| Local HEAD, fetching disabled | 753 | 595 | 582 |
| GitHub fetch | 2,058 | 1,937 | 1,900 |
| GitHub fetch, repeated | 2,078 | 2,136 | 1,920 |

In the remote pass, the local checkout became available after 319 ms, compared
with 1,722 ms in the serial schedule. The synchronized checkout was available
after 1,678 ms. A visible local path therefore does not imply that an agent
action can already start: the first action still awaits the final upstream
revision and the remaining initialization.

The two prepared local schedules execute the same work; their small difference
is measurement variation. Remote medians improve by about 158 ms over the
serial schedule in both passes. The additional gain over catalog-only overlap
varies with network timing; early local-checkout availability is the larger
and more consistent effect.

The full CLI comparison alternated the installed baseline and the candidate,
with seven measured starts per binary after one warmup each. Both used the
same current HOME, repository, Nix wrapper environment, and source data
directories. Medians in milliseconds:

| CLI interval | Installed baseline | Candidate |
| --- | ---: | ---: |
| Process start to first frame | 170 | 171 |
| Process start to editable composer | 199 | 209 |
| Process start to ready | 2,791 | 2,474 |
| Internal startup to ready | 2,580 | 2,300 |

Process-to-ready improved by 317 ms (11.4%). Six of seven paired starts were
faster; one was 49 ms slower. Baseline samples ranged from 2,350 to 3,243 ms,
and candidate samples from 2,334 to 2,924 ms. The earlier isolated baseline
median was 3,438 ms, demonstrating why the paired comparison matters. Initial
composer presentation is essentially unchanged. The upstream fetch remains a
network-dependent barrier before the first agent action.

Validation covers 77 worktree examples, 33 gateway examples, 6 integration
examples, and 68 composer examples, all passing. This includes a blocked real
Git transport and cancellation, simultaneous private fetch refs, local-change
retention, managed enrollment after readiness, and input/image queue behavior.
All 239 CLI modules loaded in GHCi, and the optimized CLI and benchmark built.

A real tmux test held the Git transport behind a FIFO. It verified the local
checkout at the original commit, editable text, and an early `/help` visibly
queued before releasing the fetch. After release, the checkout matched the
new upstream commit and `/help` ran exactly once. The CLI then exited normally.
No model turn was sent. All 22 managed measurement checkouts from this pass,
their branches, and their enrollment records were removed after validating
clean state and inactive leases. The benchmark removed its own temporary
checkouts after every sample. No test transport processes remained.
