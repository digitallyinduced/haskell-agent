# CLI heap diagnostics

Use an optimized executable for memory investigations. GHCi is suitable for
validating the diagnostic module, but its interpreter and loaded modules make
its memory footprint unsuitable as an application baseline.

```sh
AGENT_HEAP_DIAGNOSTICS="$TMPDIR/agent_heap_observations.csv" \
  agent-cli +RTS -T -RTS
```

The executable appends numeric CSV observations at startup, approximately once
per second, and when the CLI action exits. It writes a header when the file is
empty. Use a separate file for each concurrently running process. The parent
directory must already exist. Unset the variable to disable the sampler.

Each row contains Unix time, monotonic elapsed nanoseconds, process ID, whether
a major collection was requested, collection counts, cumulative allocations,
maximum residency, the most recent collection's heap statistics, and cumulative
GC/mutator CPU and elapsed times. No prompts, tool contents, or credentials are
recorded. Logging errors fail the diagnostic run rather than silently losing
observations.

## Interpretation

- `last_gc_live_bytes` and related columns describe the **last completed GC**,
  not necessarily a major collection or the current instant. Inspect
  `last_gc_generation`, `gcs`, and `major_gcs`; repeated values while idle can
  represent the same old collection.
- `allocated_bytes` is cumulative allocation, not resident memory.
- `last_gc_mem_in_use_bytes` includes RTS heap capacity, not just live objects.
  It does not account for every native allocation or mapped executable page.
- Compare these values with macOS `vmmap -summary PID` physical footprint.
  Reserved virtual address space is not physical memory.

For a separate, deliberately intrusive major-GC experiment:

```sh
AGENT_HEAP_DIAGNOSTICS="$TMPDIR/agent_heap_forced_collections.csv" \
AGENT_HEAP_DIAGNOSTICS_FORCE_GC=1 \
  agent-cli +RTS -T -RTS
```

This requests a major collection before every sample. It changes allocation,
timing, and heap-return behaviour; do not treat it as a normal performance
baseline. Other threads can collect between the request and the observation,
so inspect the generation and collection counts even in this mode.

Compare the same executable and workload at startup, after MCP discovery,
after a completed turn, and during idle. Repeat runs before attributing a
difference to a particular subsystem. Existing processes cannot acquire this
instrumentation without restarting with the diagnostic-enabled executable.
