#!/usr/bin/env python3
"""Materialize reviewed interface coverage ranges into individual inventory rows.

The summary table is an input, not a heuristic assessment. Original row prose is
retained as historical context; each row gains the reviewed website evidence.
"""
from pathlib import Path
import re

path = Path(__file__).resolve().parents[1] / "audit/interfaces.md"
text = path.read_text()
reviewed = {}
for line in text.splitlines():
    match = re.match(r"\| (?:Summary )?`?(IF-[A-Z]+)-(\d+)(?:–(\d+))?`? \| (Covered|Partial|Missing|Conflict) \| (.*) \|$", line)
    if match:
        prefix, first, last, status, evidence = match.groups()
        for number in range(int(first), int(last or first) + 1):
            reviewed[f"{prefix}-{number:02}"] = status, evidence

rows = []
count = 0
for line in text.splitlines():
    if re.match(r"\| IF-[A-Z]+-\d+ \|", line):
        cells = [cell.strip() for cell in line.split("|")[1:-1]]
        if len(cells) == 5 and cells[0] in reviewed:
            status, evidence = reviewed[cells[0]]
            original = cells[4].split(" Historical audit context: ", 1)[-1]
            cells[3] = status
            cells[4] = evidence + " Historical audit context: " + original
            line = "| " + " | ".join(cells) + " |"
            count += 1
    rows.append(line)
if count != 109:
    raise SystemExit(f"Expected 109 inventory rows; found {count}; no changes written")
# Summary rows have a non-ID label so
# inventory consumers cannot mistake them for a second individual audit row.
rows = [
    re.sub(r"^\| (?:Summary )?`?(IF-[A-Z]+-\d+(?:–\d+)?)`? \|",
           lambda match: "| Summary `" + match[1] + "` |", line)
    if len(line.split("|")) == 5 else line
    for line in rows
]
path.write_text("\n".join(rows) + "\n")
print(f"Materialized {count} reviewed inventory rows")
