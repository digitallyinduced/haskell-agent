# agent-computer-use

Frontend-independent computer-use tools, desktop capture/input backends, and
accessibility observations.

## Boundaries

- `Agent.ComputerUse` supplies the tool adapter, guarded execution, and runtime
  lifecycle. Frontends still provide their approval interaction.
- `Agent.ComputerUse.Backend` defines the desktop backend interface.
- `Agent.ComputerUse.Accessibility` supplies snapshot and delta operations shared
  with the native bridge.
- Input parsing and Linux Portal/X11/logind implementations are private modules.
- Wire-level computer-use verdicts remain in `agent-core`'s
  `Agent.ComputerUse.Protocol`; core does not depend on this implementation.

The test component includes this package's own source directory to exercise
private platform logic with fake backends and resources. It does not depend on
the CLI or request real desktop permissions. Frontend-specific tool-card
projection tests remain in `agent-cli`.

## Development

From the repository root inside `nix develop`:

```sh
cabal repl agent-computer-use:lib:agent-computer-use
cabal repl agent-computer-use:test:agent-computer-use-test
```

In the test REPL, run `main`. The retained optimized benchmarks are
`agent-computer-use:bench:accessibility-delta-bench` and
`agent-computer-use:bench:portal-capture-bench`.
