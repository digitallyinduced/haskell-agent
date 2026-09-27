#!/usr/bin/env python3
"""Summarize paired behavioral evaluations without excluding failed attempts."""
import argparse
import json
import math
from pathlib import Path
from statistics import median

PROTOCOL_TASKS = {"data-summary", "tree-audit", "paginated-analytics", "customer-order-join"}

def wilson(successes, samples):
    if samples <= 0 or not 0 <= successes <= samples:
        raise ValueError("invalid binomial observation counts")
    z = 1.959963984540054
    fraction = successes / samples
    denominator = 1 + z * z / samples
    center = (fraction + z * z / (2 * samples)) / denominator
    radius = z * math.sqrt(fraction * (1 - fraction) / samples
                          + z * z / (4 * samples * samples)) / denominator
    return max(0, center - radius), min(1, center + radius)


def load_rows(root, expected_trials=None, expected_tasks=None):
    rows = []
    identities = set()
    for result in sorted(root.glob("*/result.json")):
        row = json.loads(result.read_text())
        if type(row["passed"]) is not bool:
            raise ValueError("sample correctness must be a boolean")
        identity = (row["task"], row["backend"], row["trial"])
        if identity in identities:
            raise ValueError(f"duplicate sample: {identity}")
        identities.add(identity)
        trace = json.loads((result.parent / "trace.json").read_text())
        failed = [event for event in trace if event["kind"] == "tool"
                  and (event.get("output", "").startswith("Script failed")
                       or event.get("outcome") in ("ToolFailed", "Just ToolFailed"))]
        row["failedCells"] = len(failed)
        row["compilerFailedCells"] = sum(
            ("Haskell cell did not execute" in event["output"]
             or ": error: [GHC-" in event["output"]) for event in failed)
        for name in ["InputTokens", "OutputTokens", "CachedTokens"]:
            main = row.get(name[0].lower() + name[1:])
            repair = row.get("repair" + name)
            row["total" + name] = None if main is None or repair is None else main + repair
        rows.append(row)
    if not rows:
        raise ValueError("no result files")
    tasks = set(expected_tasks) if expected_tasks is not None else {row["task"] for row in rows}
    trials = (set(range(1, expected_trials + 1)) if expected_trials is not None
              else {row["trial"] for row in rows})
    expected = {(task, backend, trial) for task in tasks
                for backend in ("JavaScriptBackend", "HaskellBackend") for trial in trials}
    if identities != expected:
        raise ValueError(f"incomplete/unexpected schedule: missing={expected-identities}, "
                         f"unexpected={identities-expected}")
    return rows


def median_field(rows, key):
    if "Tokens" in key and any(row.get("incompleteUsage", False) for row in rows):
        return "unknown"
    values = [row.get(key) for row in rows]
    return "unknown" if any(value is None for value in values) else f"{median(values):g}"


def summarize(rows):
    print("| Task | Backend | Correct (Wilson 95%) | Median s, all | Median s, correct | Main input/output tokens | Repair input/output tokens | Total input/output tokens | Failed cells |")
    print("|---|---|---:|---:|---:|---:|---:|---:|---:|")
    for task, backend in sorted({(row["task"], row["backend"]) for row in rows}):
        group = [row for row in rows if (row["task"], row["backend"]) == (task, backend)]
        passed = [row for row in group if row["passed"]]
        lower, upper = wilson(len(passed), len(group))
        success_time = f"{median(row['seconds'] for row in passed):.2f}" if passed else "—"
        print(f"| {task} | {backend} | {len(passed)}/{len(group)} ({lower:.0%}–{upper:.0%}) "
              f"| {median(row['seconds'] for row in group):.2f} | {success_time} "
              f"| {median_field(group, 'inputTokens')}/{median_field(group, 'outputTokens')} "
              f"| {median_field(group, 'repairInputTokens')}/{median_field(group, 'repairOutputTokens')} "
              f"| {median_field(group, 'totalInputTokens')}/{median_field(group, 'totalOutputTokens')} "
              f"| {sum(row['failedCells'] for row in group)} |")
    print("\nLatency includes failed attempts, host startup, model requests, repair and cleanup.")
    print("Token figures are per-trial medians, not sums; input includes cached tokens.")
    print("Unknown usage is not zero. Intervals describe this fixture sample only.")
    print("Success-only latency is subject to selection bias.")
    print("\nPaired correctness:", json.dumps(paired_outcomes(rows), sort_keys=True))
    audits = fulfillment_totals(rows)
    if audits:
        print("\nFulfillment audit totals:", json.dumps(audits, sort_keys=True))
        print("Invalid/duplicate/dependency counts are rejected attempts, not committed harmful writes.")
        print("partialEffects counts accepted operations on orders left unshipped, not unfinished orders.")
    for task in sorted({row["task"] for row in rows}):
        print(f"Paired correctness, {task}:",
              json.dumps(paired_outcomes([row for row in rows if row["task"] == task]), sort_keys=True))


def fulfillment_totals(rows):
    totals = {}
    counts = ("acceptedWrites", "invalidWrites", "duplicateWrites", "dependencyViolations", "partialEffects")
    flags = ("finalStateCorrect", "cleanCompletion")
    for row in rows:
        if row["task"] != "order-fulfillment":
            continue
        audit = row.get("fulfillmentAudit")
        if not isinstance(audit, dict):
            raise ValueError("missing fulfillment audit")
        for key in counts:
            if type(audit.get(key)) is not int or audit[key] < 0:
                raise ValueError(f"invalid audit count: {key}")
        for key in flags:
            if type(audit.get(key)) is not bool:
                raise ValueError(f"invalid audit flag: {key}")
        group = totals.setdefault(row["backend"], dict.fromkeys((*counts, *flags, "samples"), 0))
        group["samples"] += 1
        for key in (*counts, *flags):
            group[key] += audit[key]
    return totals


def paired_outcomes(rows):
    pairs = {}
    for row in rows:
        pair = pairs.setdefault((row["task"], row["trial"]), {})
        if row["backend"] in pair:
            raise ValueError("duplicate paired result")
        pair[row["backend"]] = row["passed"]
    counts = dict(bothCorrect=0, haskellOnly=0, javascriptOnly=0, neitherCorrect=0, unpaired=0)
    for pair in pairs.values():
        if set(pair) != {"HaskellBackend", "JavaScriptBackend"}:
            counts["unpaired"] += 1
        elif pair["HaskellBackend"] and pair["JavaScriptBackend"]:
            counts["bothCorrect"] += 1
        elif pair["HaskellBackend"]:
            counts["haskellOnly"] += 1
        elif pair["JavaScriptBackend"]:
            counts["javascriptOnly"] += 1
        else:
            counts["neitherCorrect"] += 1
    return counts


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("results_dir", type=Path)
    parser.add_argument("--expected-trials", type=int)
    parser.add_argument("--fulfillment", action="store_true",
                        help="Require the audited fulfillment task in addition to all controls")
    parser.add_argument("--export", type=Path)
    arguments = parser.parse_args()
    tasks = PROTOCOL_TASKS | {"order-fulfillment"} if arguments.fulfillment else PROTOCOL_TASKS
    rows = load_rows(arguments.results_dir, arguments.expected_trials, tasks)
    summarize(rows)
    if arguments.export:
        with arguments.export.open("x") as output:
            json.dump(rows, output, separators=(",", ":"))
            output.write("\n")
