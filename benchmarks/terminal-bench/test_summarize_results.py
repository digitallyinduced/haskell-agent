import json
from pathlib import Path
import tempfile
import unittest

from summarize_results import compare, elapsed, load_results


class ResultSummaryTests(unittest.TestCase):
    def test_elapsed(self):
        self.assertEqual(elapsed({
            "started_at": "2026-09-24T12:00:00Z",
            "finished_at": "2026-09-24T12:00:02.125Z",
        }), 2.125)
        self.assertIsNone(elapsed(None))

    def test_outcomes_and_no_exception_message(self):
        with tempfile.TemporaryDirectory() as directory:
            for index, (reward, exception) in enumerate([
                (1, None), (0, None),
                (None, {"exception_type": "RuntimeError", "exception_message": "secret"}),
                (None, None),
            ]):
                path = Path(directory) / str(index)
                path.mkdir()
                (path / "result.json").write_text(json.dumps({
                    "trial_name": f"task-{index}__abc",
                    "verifier_result": {"rewards": {"reward": reward}},
                    "exception_info": exception,
                }))
            results = load_results(directory)
            self.assertEqual(
                [row["status"] for row in results.values()],
                ["passed", "failed", "exception", "unscored"],
            )
            self.assertNotIn("secret", json.dumps(results))
            summary = compare(results, results, 4)
            self.assertEqual(summary["totals"]["first"]["exception"], 1)

    def test_missing_pairs_rejected(self):
        with self.assertRaisesRegex(ValueError, "Unpaired"):
            compare({"one": {}}, {})

    def test_wrong_count_rejected(self):
        with self.assertRaisesRegex(ValueError, "Expected 10"):
            compare({}, {})

    def test_duplicate_task_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            for trial in ("one", "two"):
                path = Path(directory) / trial
                path.mkdir()
                (path / "result.json").write_text(json.dumps({"task_name": "same"}))
            with self.assertRaisesRegex(ValueError, "Duplicate"):
                load_results(directory)


if __name__ == "__main__":
    unittest.main()
