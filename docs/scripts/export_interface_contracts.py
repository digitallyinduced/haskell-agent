#!/usr/bin/env python3
"""Export reviewed interface contracts verbatim for the self-hosted docs.

Run from any directory. --check verifies freshness without writing. These are
reference artifacts, not a declaration that the source contracts are complete.
"""
import argparse
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CONTRACTS = {
    "agent-server-openapi.json": "packages/agent-server/openapi.json",
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    for name, source in CONTRACTS.items():
        target = ROOT / "docs/public" / name
        content = (ROOT / source).read_bytes()
        if args.check:
            if not target.exists() or target.read_bytes() != content:
                raise SystemExit(f"Stale interface reference: {target}")
        else:
            target.write_bytes(content)
        print(f"{name}: {len(content)} bytes match {source}")


if __name__ == "__main__":
    main()
