"""Check audit identifiers, repository citations, and report row status counts.

This validates the audit artifact, not product behavior or semantic coverage.
Run from any directory using a Nix-provided Python interpreter.
"""

from collections import Counter
from pathlib import Path
import re
import sys


DIRECTORY = Path(__file__).resolve().parent
REPOSITORY = DIRECTORY.parent.parent
STATUSES = {"Covered", "Partial", "Missing", "Conflict"}
MATRICES = {"cli.md", "configuration.md", "tools.md", "interfaces.md", "website.md"}
REFERENCE = re.compile(
    r"(?P<path>(?:(?:packages|docs|nix|scripts|tests)/[^\s`|;,:()\[\]#]+"
    r"|flake\.nix|README\.md)):(?P<start>\d+)(?:[-–](?P<end>\d+))?"
)


def main():
    identifiers = {}
    references = set()
    errors = []
    total = Counter()
    for name in sorted(MATRICES):
        if not (DIRECTORY / name).is_file():
            errors.append(f"Missing required matrix: {name}")
    for document in sorted(DIRECTORY.glob("*.md")):
        counts = Counter()
        for number, line in enumerate(document.read_text().splitlines(), 1):
            cells = [cell.strip().strip("`") for cell in line.split("|")[1:-1]]
            if cells and re.fullmatch(r"[A-Z]+(?:-[A-Z]+)*-[A-Z]*\d+", cells[0]):
                identifier = cells[0]
                if identifier in identifiers:
                    errors.append(f"{document.name}:{number}: duplicate {identifier}")
                identifiers[identifier] = (document.name, number)
                statuses = [cell for cell in cells if cell in STATUSES]
                if len(statuses) != 1:
                    errors.append(f"{document.name}:{number}: expected one coverage status")
                else:
                    counts[statuses[0]] += 1
            for match in REFERENCE.finditer(line):
                reference = match.group("path")
                start = int(match.group("start"))
                end = int(match.group("end") or start)
                references.add((reference, start, end))
                path = REPOSITORY / reference
                if not path.is_file():
                    errors.append(f"{document.name}:{number}: missing {reference}")
                elif not 1 <= start <= end <= len(path.read_text().splitlines()):
                    errors.append(f"{document.name}:{number}: invalid range {match.group()}")
        if counts:
            total.update(counts)
            print(f"{document.name}: {sum(counts.values())} rows; " +
                  ", ".join(f"{status}={counts[status]}" for status in sorted(STATUSES)))
        elif document.name in MATRICES:
            errors.append(f"{document.name}: no auditable rows found")
    print(f"Total: {sum(total.values())} audit rows; " +
          ", ".join(f"{status}={total[status]}" for status in sorted(STATUSES)))
    print(f"Checked {len(references)} distinct repository line citations.")
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Audit artifact checks passed. This is not a product behavior test.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
