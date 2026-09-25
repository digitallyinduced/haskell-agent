"""Summarize paired Harbor trials without exposing execution logs."""

import argparse
from datetime import datetime
import json
from pathlib import Path


def elapsed(phase):
    if not phase or not phase.get("started_at") or not phase.get("finished_at"):
        return None
    return round(
        (datetime.fromisoformat(phase["finished_at"])
         - datetime.fromisoformat(phase["started_at"])).total_seconds(), 3
    )


def load_results(directory):
    results = {}
    for path in sorted(Path(directory).glob("*/result.json")):
        record = json.loads(path.read_text())
        task = record.get("task_name") or record["trial_name"].rsplit("__", 1)[0]
        if task in results:
            raise ValueError(f"Duplicate task: {task}")
        exception = record.get("exception_info")
        verifier = record.get("verifier_result") or {}
        reward = (verifier.get("rewards") or {}).get("reward")
        if exception:
            status = "exception"
        elif reward is None:
            status = "unscored"
        elif reward == 1:
            status = "passed"
        else:
            status = "failed"
        results[task] = {
            "status": status,
            "reward": reward,
            "exception_type": exception.get("exception_type") if exception else None,
            "agent_seconds": elapsed(record.get("agent_execution")),
            "total_seconds": elapsed(record),
        }
    return results


def compare(first, second, expected_count=10):
    if set(first) != set(second):
        raise ValueError(
            f"Unpaired tasks: first-only={sorted(set(first) - set(second))}; "
            f"second-only={sorted(set(second) - set(first))}"
        )
    if len(first) != expected_count:
        raise ValueError(f"Expected {expected_count} task pairs, found {len(first)}")
    return {
        "task_count": len(first),
        "totals": {
            label: {
                status: sum(row["status"] == status for row in records.values())
                for status in ("passed", "failed", "exception", "unscored")
            }
            for label, records in (("first", first), ("second", second))
        },
        "tasks": [
            {"task": task, "first": first[task], "second": second[task]}
            for task in sorted(first)
        ],
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("first_job", type=Path)
    parser.add_argument("second_job", type=Path)
    parser.add_argument("--expected-count", type=int, default=10)
    arguments = parser.parse_args()
    try:
        result = compare(
            load_results(arguments.first_job),
            load_results(arguments.second_job),
            arguments.expected_count,
        )
    except (ValueError, KeyError, OSError) as error:
        parser.error(str(error))
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
