#!/usr/bin/env python3
"""Check the conservative local Cabal dependency graph, including tests/flags.

This deliberately unions component and conditional dependencies: a package
boundary must hold on every platform, not just the current Cabal configuration.
Cabal remains responsible for parsing and solving full version constraints.
"""

from pathlib import Path
import re
import sys


def dependencies(source):
    result = set()
    field_indent = None
    for raw in source.splitlines():
        line = raw.split("--", 1)[0].rstrip()
        if not line.strip():
            continue
        indent = len(line) - len(line.lstrip())
        match = re.match(r"\s*build-depends\s*:(.*)", line, re.I)
        if match:
            field_indent = indent
            value = match.group(1)
        elif field_indent is not None and indent > field_indent:
            value = line
        else:
            field_indent = None
            continue
        for dependency in value.split(","):
            name = re.match(r"\s*([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)", dependency)
            if name:
                result.add(name.group(1))
    return result


def check(root):
    manifests = {}
    for path in sorted((root / "packages").glob("*/*.cabal")):
        source = path.read_text()
        name = re.search(r"^name:\s*(\S+)", source, re.M | re.I)
        if not name:
            raise ValueError(f"missing package name: {path}")
        if name[1] in manifests:
            raise ValueError(f"duplicate local package: {name[1]}")
        manifests[name[1]] = (path, dependencies(source))
        forbidden = set()
        if name[1] in {
            "agent-core", "agent-accounts", "agent-computer-use",
            "agent-tools", "agent-runtime",
        }:
            forbidden |= {"agent-cli", "agent-tui", "agent-cli-runtime"}
        if name[1] == "agent-core":
            forbidden.add("agent-tools")
            contracts = {
                "Agent.Tools.Types", "Agent.Tools.Scheduling",
                "Agent.Tools.ResourceArbiter", "Agent.Tools.OutputArtifact.Memory",
            }
            test_root = path.parent / "test"
            test_modules = {
                ".".join(haskell.relative_to(test_root).with_suffix("").parts)
                for haskell in test_root.rglob("*.hs")
            }
            for haskell in [
                *(path.parent / "src").rglob("*.hs"), *test_root.rglob("*.hs"),
            ]:
                # Core tests must use contracts too, not reverse the dependency.
                imports = set(re.findall(
                    r"^\s*import\s+(?:qualified\s+)?(Agent\.Tools(?:\.[A-Za-z0-9_]+)+)",
                    haskell.read_text(), re.M,
                ))
                allowed = contracts | (test_modules if test_root in haskell.parents else set())
                concrete = imports - allowed
                if concrete:
                    raise ValueError(
                        f"{haskell}: core imports concrete tools: {sorted(concrete)}"
                    )
        bad = dependencies(source) & forbidden
        if bad:
            raise ValueError(f"{path}: forbidden dependencies: {sorted(bad)}")
        if name[1] == "agent-native-bridge":
            native_sources = list((path.parent / "src").rglob("*.hs"))
            native_modules = {
                ".".join(haskell.relative_to(path.parent / "src").with_suffix("").parts)
                for haskell in native_sources
            }
            for haskell in native_sources:
                imports = set(re.findall(
                    r"^\s*import\s+(?:qualified\s+)?(Agent\.(?:CLI|TUI)(?:\.[A-Za-z0-9_]+)+)",
                    haskell.read_text(), re.M,
                ))
                # Native helpers retain legacy names; only imports of helpers
                # owned by this ordinary library are permitted.
                frontend = imports - native_modules
                if frontend:
                    raise ValueError(
                        f"{haskell}: native library imports frontend: {sorted(frontend)}"
                    )
            # The Darwin foreign library still composes legacy CLI entry points.
            # Platform tests also exercise those legacy entry points. Check
            # ordinary libraries and shared common stanzas; do not hide
            # the foreign-library edge from the conservative cycle check below.
            ordinary = []
            include = False
            for line in source.splitlines():
                clean = line.split("--", 1)[0].rstrip()
                if clean and not clean[0].isspace():
                    include = clean == "library" or clean.startswith(("library ", "common "))
                if include:
                    ordinary.append(line)
            bad = dependencies("\n".join(ordinary)) & {
                "agent-cli", "agent-tui", "agent-cli-runtime",
            }
            if bad:
                raise ValueError(
                    f"{path}: ordinary library depends on frontend: {sorted(bad)}"
                )
    if not manifests:
        raise ValueError("no local Cabal packages found")
    names = set(manifests)
    graph = {}
    for name, (path, deps) in manifests.items():
        missing = {dep for dep in deps if dep.startswith("agent-")} - names
        if missing:
            raise ValueError(f"{path}: missing local dependencies: {sorted(missing)}")
        graph[name] = (deps & names) - {name}

    complete = set()
    visiting = []

    def visit(name):
        if name in visiting:
            cycle = visiting[visiting.index(name):] + [name]
            raise ValueError("local package cycle: " + " -> ".join(cycle))
        if name in complete:
            return
        visiting.append(name)
        for dependency in sorted(graph[name]):
            visit(dependency)
        visiting.pop()
        complete.add(name)

    for name in sorted(graph):
        visit(name)
    print(f"Local package graph: {len(graph)} packages, no cycles")


if __name__ == "__main__":
    try:
        check(Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parent.parent)
    except ValueError as error:
        sys.exit(f"package boundary check failed: {error}")
