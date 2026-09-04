#!/usr/bin/env python3
"""Copy and relocate the non-system dylibs used by Mach-O files.

Unlike dylibbundler, this script gives each source path a stable hashed name.
That distinction matters for Nix closures: two ABI-incompatible libraries can
have the same basename (for example Apple's and GNU's libiconv.2.dylib).
"""

from __future__ import annotations

import argparse
from collections import deque
import hashlib
import os
from pathlib import Path
import shutil
import stat
import subprocess
import sys


LOAD_COMMANDS = {
    "LC_LAZY_LOAD_DYLIB",
    "LC_LOAD_DYLIB",
    "LC_LOAD_UPWARD_DYLIB",
    "LC_LOAD_WEAK_DYLIB",
    "LC_REEXPORT_DYLIB",
}
SYSTEM_PREFIXES = (
    "/System/Library/",
    "/Library/Apple/System/Library/",
    "/usr/lib/",
)


def load_commands(path: Path) -> tuple[list[str], list[str], bool]:
    """Return loaded libraries, rpaths, and whether path has an install ID."""
    output = subprocess.check_output(
        ["otool", "-l", os.fspath(path)],
        text=True,
        stderr=subprocess.DEVNULL,
    )
    libraries: list[str] = []
    rpaths: list[str] = []
    has_id = False
    command: str | None = None

    for raw_line in output.splitlines():
        line = raw_line.strip()
        if line.startswith("cmd "):
            command = line.removeprefix("cmd ")
            has_id = has_id or command == "LC_ID_DYLIB"
        elif command in LOAD_COMMANDS and line.startswith("name "):
            libraries.append(line.removeprefix("name ").rsplit(" (offset", 1)[0])
            command = None
        elif command == "LC_RPATH" and line.startswith("path "):
            rpaths.append(line.removeprefix("path ").rsplit(" (offset", 1)[0])
            command = None

    return libraries, rpaths, has_id


def make_writable(path: Path) -> None:
    path.chmod(path.stat().st_mode | stat.S_IWUSR)


def run_install_name_tool(*arguments: str | Path) -> None:
    subprocess.run(
        ["install_name_tool", *(os.fspath(argument) for argument in arguments)],
        check=True,
    )


def bundled_name(source: str) -> str:
    path_hash = hashlib.sha256(source.encode()).hexdigest()[:16]
    return f"{path_hash}-{Path(source).name}"


def relocate(
    destination: Path,
    install_prefix: str,
    initial_targets: list[Path],
) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    queue: deque[tuple[Path, str | None]] = deque(
        (target, None) for target in initial_targets
    )
    processed: set[Path] = set()
    copied: dict[str, tuple[Path, str]] = {}

    while queue:
        target, copied_install_name = queue.popleft()
        target = target.resolve()
        if target in processed:
            continue
        processed.add(target)
        make_writable(target)

        libraries, rpaths, has_id = load_commands(target)
        if has_id:
            install_id = copied_install_name or f"@loader_path/{target.name}"
            run_install_name_tool("-id", install_id, target)

        for dependency in libraries:
            if dependency.startswith(SYSTEM_PREFIXES):
                continue
            if not dependency.startswith("/nix/store/"):
                raise RuntimeError(
                    f"unsupported non-system dependency in {target}: {dependency}"
                )
            if not Path(dependency).is_file():
                raise RuntimeError(
                    f"dependency does not exist for {target}: {dependency}"
                )

            if dependency not in copied:
                name = bundled_name(dependency)
                bundled_path = destination / name
                install_name = f"{install_prefix}{name}"
                shutil.copy2(dependency, bundled_path, follow_symlinks=True)
                make_writable(bundled_path)
                copied[dependency] = (bundled_path, install_name)
                queue.append((bundled_path, install_name))

            _, install_name = copied[dependency]
            run_install_name_tool("-change", dependency, install_name, target)

        for rpath in dict.fromkeys(rpaths):
            if rpath.startswith("/nix/store/"):
                run_install_name_tool("-delete_rpath", rpath, target)

    print(
        f"relocated {len(initial_targets)} Mach-O target(s) "
        f"with {len(copied)} uniquely named dylib(s)",
        file=sys.stderr,
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--destination", type=Path, required=True)
    parser.add_argument("--install-prefix", required=True)
    parser.add_argument("target", nargs="+", type=Path)
    arguments = parser.parse_args()
    relocate(arguments.destination, arguments.install_prefix, arguments.target)


if __name__ == "__main__":
    main()
