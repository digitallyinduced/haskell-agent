# Agent bundles

An `AgentBundle` packages one or more named agent startup configurations with
their instructions, tools, skills, and executable environment. The bundle
format is provider-neutral: it may select a logical model alias, but the active
organization gateway remains authoritative for provider routing,
authentication, endpoints, and model resolution. Running a bundle therefore
requires an active organization gateway connection.

The intended workflow is:

1. declare the bundle in a flake;
2. build or inspect the immutable result;
3. run it directly, or add it to a Nix profile for local generation-based
   activation and rollback.

## Declare a bundle with Nix

`haskell-agent.lib.mkAgentBundle` produces a normal derivation containing a
manifest, its runtime closure, and a wrapper executable:

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.haskell-agent.url = "github:digitallyinduced/haskell-agent";

  outputs =
    { nixpkgs, haskell-agent, ... }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
    in
    {
      packages = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          review = haskell-agent.lib.mkAgentBundle {
            inherit pkgs;
            name = "patch-review";
            defaultAgent = "reviewer";

            environments.review.packages = with pkgs; [
              git
              ripgrep
            ];

            # The root contains a SKILL.md, or nested skill directories.
            skills.patch-review = ./skills/patch-review;

            agents.reviewer = {
              description = "Review the current patch";
              instructions = ./reviewer.md;
              model = "review";
              effort = "high";
              environment = "review";
              skills = [ "patch-review" ];
              tools = {
                bash = true;
                ghci = false;
                computerUse = false;
                codeMode = false;
              };
              workspace = {
                worktree = true;
                agentsMd = false;
                ambientSkills = false;
              };
              maxTurns = 20;
            };
          };
        in
        {
          inherit review;
          default = review;
        }
      );
    };
}
```

Use `instructions = ./file.md` for instructions kept in a source file, or
`instructionsText = "..."` for short inline instructions. Exactly one is
required. Like all Nix inputs, instructions and skills enter the Nix store:
never put credentials or other secrets in them.

`environments.<name>.packages` becomes the `PATH` prefix for agent-spawned
tools. It is deliberately not applied to `agent-cli` itself, provider
processes, authentication helpers, or MCP startup, so a bundle cannot shadow
the organization's gateway boundary.

## Inspect and run

Build and inspect the fully defaulted logical plan:

```console
nix build .#review
agent-cli bundle inspect ./result
agent-cli bundle inspect ./result --json
```

Run the wrapper, forwarding any bundle-run arguments after `--`:

```console
nix run .#review -- -p "Review the current patch"
nix run .#review -- --agent reviewer --cwd "$PWD"
```

The wrapper is equivalent to:

```console
agent-cli bundle run /nix/store/.../manifest.json --agent reviewer
```

`bundle inspect` and `bundle run` also accept a manifest file directly. This
is useful while developing a generator, but only a Nix-built output provides
an immutable manifest and GC-rootable runtime closure. Loading fails if a
declared skill root or executable path no longer exists.

Bundle runs default to normal approval prompts in a terminal and denial of
mutating tools without a terminal. Use `--yolo` only when the caller intends
to opt into auto-approval.

## Apply locally with Nix profiles

A Nix profile is the local equivalent of “apply” for this first version:

```console
nix profile add .#review
nix profile upgrade --refresh --all
nix profile rollback
```

Adding or upgrading switches the profile to a new generation atomically and
keeps the entire bundle closure alive as a GC root. `nix profile rollback`
switches back to the previous generation.

This is intentionally local lifecycle management, not a deployment
controller. AgentBundle v1 does not reconcile remote machines, start services,
or maintain fleet state. A NixOS or Home Manager module can consume the same
derivation when declarative machine-wide activation is needed.

## Nix API

Top-level arguments:

| Argument | Meaning |
| --- | --- |
| `pkgs` | Nixpkgs package set for the target system |
| `name` | Safe bundle and executable name |
| `defaultAgent` | Agent selected when `--agent` is omitted |
| `agents` | Named agent startup configurations |
| `environments` | Named, non-empty package lists; default `{}` |
| `skills` | Named skill roots; default `{}` |
| `agentPackage` | `agent-cli` package; defaults to this flake's package |

Agent fields:

| Field | Default |
| --- | --- |
| `description` | omitted |
| `instructions` or `instructionsText` | exactly one required |
| `model` | gateway/runtime default |
| `effort` | gateway/runtime default |
| `environment` | host `PATH` only |
| `skills` | `[]` |
| `tools.bash` | `true` |
| `tools.ghci` | `false` |
| `tools.computerUse` | `false` |
| `tools.codeMode` | `false` |
| `workspace.worktree` | `false` |
| `workspace.agentsMd` | `false` |
| `workspace.ambientSkills` | `false` |
| `maxTurns` | normal CLI default |

Unknown attributes and dangling references fail during Nix evaluation.

## Runtime and trust semantics

- `model` is a logical alias, not a provider selector. Bundles cannot declare
  providers, API endpoints, credentials, or arbitrary environment variables.
- Bundle startup resolves that alias against the active gateway's live model
  list and fails if the alias is unavailable; local model configuration cannot
  authorize it or redirect its transport.
- Instructions are injected as generated workspace/user context at the same
  trust tier as local instruction files; they are not elevated to the
  harness-owned system prompt.
- Declared skills are hermetic by default. Repository, user, packaged, and
  learned skills are added only with `workspace.ambientSkills = true`.
- `AGENTS.md` discovery is off by default and must be enabled explicitly with
  `workspace.agentsMd = true`.
- Tool booleans are startup feature flags, not a security sandbox or enforced
  allowlist. The interactive UI may change available shell modes and models,
  and normal tool approval rules still apply.
- A bundle's transcript may be persisted, but a plain session resume does not
  reconstruct bundle paths, skills, or identity. Relaunch the bundle wrapper
  for a bundle-configured session.
- The generated runtime closure makes declared packages and skills durable; it
  does not restrict filesystem or network access. Use the existing approval
  and sandbox controls for those boundaries.

## Manifest format

The generated JSON uses `format = "haskell-agent-bundle"` and `version = 1`.
The CLI rejects unknown fields at every level, unsupported versions, unsafe
names, duplicate or dangling references, relative paths, missing assets, empty
instructions, and manifests larger than 1 MiB. The Nix function is the
recommended authoring interface; the JSON is primarily an interchange and
inspection format.
