"""Regression tests for evaluation accounting; no model requests."""
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest

specification = importlib.util.spec_from_file_location(
    "evaluation_summary", Path(__file__).with_name("summarize-code-mode-evaluation.py"))
summary = importlib.util.module_from_spec(specification)
specification.loader.exec_module(summary)


class EvaluationSummaryTests(unittest.TestCase):
    def test_fulfillment_separates_state_from_clean_completion(self):
        audit = dict(acceptedWrites=6, invalidWrites=0, duplicateWrites=1,
                     dependencyViolations=0, partialEffects=0,
                     finalStateCorrect=True, cleanCompletion=False)
        row = dict(task="order-fulfillment", backend="HaskellBackend",
                   passed=False, fulfillmentAudit=audit)
        totals = summary.fulfillment_totals([row])["HaskellBackend"]
        self.assertEqual(totals["finalStateCorrect"], 1)
        self.assertEqual(totals["cleanCompletion"], 0)
        self.assertEqual(totals["duplicateWrites"], 1)
        self.assertEqual(totals["samples"], 1)
        self.assertEqual(summary.fulfillment_totals([dict(task="tree-audit")]), {})

    def test_missing_or_malformed_audit_is_not_zero(self):
        row = dict(task="order-fulfillment", backend="JavaScriptBackend")
        for audit in (None, {}, {"acceptedWrites": True}, {"acceptedWrites": -1}):
            with self.assertRaises(ValueError):
                summary.fulfillment_totals([dict(row, fulfillmentAudit=audit)])

    def test_wilson_bounds(self):
        lower, upper = summary.wilson(10, 10)
        self.assertAlmostEqual(lower, 0.7224672)
        self.assertAlmostEqual(upper, 1)
        lower, upper = summary.wilson(0, 10)
        self.assertAlmostEqual(lower, 0)
        self.assertAlmostEqual(upper, 0.2775328)

    def test_unknown_usage_is_not_zero(self):
        self.assertEqual(summary.median_field([{}, {"tokens": 0}], "tokens"), "unknown")
        self.assertEqual(summary.median_field([{"tokens": 0}, {"tokens": 10}], "tokens"), "5")

    def test_paired_outcomes(self):
        rows = [
            {"task": "example", "trial": trial, "backend": backend, "passed": passed}
            for trial, outcomes in enumerate([(True, True), (True, False), (False, True), (False, False)])
            for backend, passed in zip(["HaskellBackend", "JavaScriptBackend"], outcomes)
        ]
        self.assertEqual(summary.paired_outcomes(rows), {
            "bothCorrect": 1, "haskellOnly": 1, "javascriptOnly": 1,
            "neitherCorrect": 1, "unpaired": 0})
        with self.assertRaisesRegex(ValueError, "duplicate"):
            summary.paired_outcomes(rows + [rows[0]])

    def test_invalid_interval_counts(self):
        for successes, samples in [(0, 0), (-1, 2), (3, 2)]:
            with self.assertRaises(ValueError):
                summary.wilson(successes, samples)

    def test_schedule_requires_paired_samples_and_retains_failures(self):
        with tempfile.TemporaryDirectory(dir=os.environ["TMPDIR"]) as directory:
            root = Path(directory)
            for backend in ["JavaScriptBackend", "HaskellBackend"]:
                destination = root / backend
                destination.mkdir()
                (destination / "result.json").write_text(json.dumps({
                    "task": "tree-audit", "backend": backend, "trial": 1,
                    "passed": False, "status": "timeout", "seconds": 180}))
                (destination / "trace.json").write_text("[]")
            rows = summary.load_rows(root, 1)
            self.assertEqual(len(rows), 2)
            self.assertTrue(all(not row["passed"] for row in rows))
            with self.assertRaisesRegex(ValueError, "incomplete"):
                summary.load_rows(root, 1, summary.PROTOCOL_TASKS)
            with self.assertRaisesRegex(ValueError, "incomplete"):
                summary.load_rows(root, 2)
            duplicate = root / "duplicate"
            duplicate.mkdir()
            (duplicate / "result.json").write_text(
                (root / "HaskellBackend" / "result.json").read_text())
            (duplicate / "trace.json").write_text("[]")
            with self.assertRaisesRegex(ValueError, "duplicate"):
                summary.load_rows(root, 1)


if __name__ == "__main__":
    unittest.main()
