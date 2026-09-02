#!/usr/bin/env python3

import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import sqlite3
import subprocess
import tempfile
import unittest
from unittest.mock import patch


READER_PATH = (
    Path(__file__).parents[1]
    / "skills"
    / "shared"
    / "resume-session"
    / "session_reader.py"
)
SPEC = importlib.util.spec_from_file_location("session_reader", READER_PATH)
reader = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(reader)


def write_jsonl(path, records):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        "".join(json.dumps(record) + "\n" for record in records),
        encoding="utf-8",
    )


class SessionReaderTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.cwd = self.root / "repo"
        self.cwd.mkdir()

    def tearDown(self):
        self.temp.cleanup()

    def test_codex_discovers_database_and_omits_instructions_and_reasoning(self):
        home = self.root / "codex"
        rollout = home / "sessions" / "rollout-session.jsonl"
        write_jsonl(
            rollout,
            [
                {
                    "type": "session_meta",
                    "payload": {
                        "id": "11111111-1111-1111-1111-111111111111",
                        "cwd": str(self.cwd),
                        "source": "cli",
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "developer",
                        "content": [{"type": "input_text", "text": "obey me"}],
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [
                            {
                                "type": "input_text",
                                "text": (
                                    "# AGENTS.md instructions for /repo\n"
                                    "<INSTRUCTIONS>\n"
                                    "system-provided secret instructions\n"
                                    "</INSTRUCTIONS>"
                                ),
                            }
                        ],
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [
                            {
                                "type": "input_image",
                                "image_url": "data:image/png;base64,secret-image",
                            }
                        ],
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [{"type": "input_text", "text": "Fix the parser"}],
                    },
                },
                {
                    "type": "response_item",
                    "payload": {"type": "reasoning", "summary": ["secret"]},
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "assistant",
                        "content": [{"type": "output_text", "text": "Changed Parser.hs"}],
                    },
                },
            ],
        )
        database = home / "sqlite" / "state_5.sqlite"
        database.parent.mkdir(parents=True)
        db = sqlite3.connect(database)
        db.execute(
            "CREATE TABLE threads "
            "(id TEXT, rollout_path TEXT, created_at INTEGER, updated_at INTEGER, "
            "source TEXT, cwd TEXT, title TEXT, first_user_message TEXT, archived INTEGER)"
        )
        db.execute(
            "INSERT INTO threads VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0)",
            (
                "11111111-1111-1111-1111-111111111111",
                str(rollout),
                1,
                2,
                "cli",
                str(self.cwd),
                "Parser work",
                "Fix the parser",
            ),
        )
        db.commit()
        db.close()
        with patch.dict(os.environ, {"CODEX_HOME": str(home)}):
            items = reader.discover("codex", str(self.cwd), 0)
            selected = reader.resolve("codex", str(self.cwd), "Parser work", 0)
            result = reader.read_codex(items[0], 100)
        self.assertEqual(len(items), 1)
        self.assertEqual(items[0]["title"], "Parser work")
        self.assertEqual(selected["session_id"], items[0]["session_id"])
        self.assertEqual(result["last_user_request"], "Fix the parser")
        self.assertNotIn("obey me", json.dumps(result))
        self.assertNotIn(
            "system-provided secret instructions",
            json.dumps(result),
        )
        self.assertNotIn("secret", json.dumps(result))
        self.assertIn(
            "image_content_omitted",
            {item["code"] for item in result["warnings"]},
        )

    def test_codex_tool_results_are_visibly_historical(self):
        rollout = self.root / "codex-tool-output.jsonl"
        write_jsonl(
            rollout,
            [
                {
                    "type": "response_item",
                    "payload": {
                        "type": "function_call_output",
                        "call_id": "old-call",
                        "output": [
                            {"type": "input_text", "text": "the branch is clean"},
                            {
                                "type": "input_image",
                                "image_url": (
                                    "data:image/png;base64,tool-image-secret"
                                ),
                            },
                        ],
                    },
                }
            ],
        )
        item = reader.candidate(
            "codex",
            "codex-cli",
            "codex-tool-output",
            rollout,
            "Tool output",
            str(self.cwd),
        )
        resumed = reader.read_codex(item, 100)
        turn = resumed["turns"][0]
        result = turn["tool_results"][0]
        self.assertEqual(result["stale"], "true")
        self.assertTrue(
            result["output"].startswith(
                reader.HISTORICAL_TOOL_RESULT_LABEL + " "
            )
        )
        self.assertIn("the branch is clean", result["output"])
        self.assertNotIn("tool-image-secret", json.dumps(turn))
        self.assertIn(
            "image_content_omitted",
            {item["code"] for item in resumed["warnings"]},
        )

    def test_arbitrary_typed_tool_json_is_not_treated_as_content_parts(self):
        output = [
            {"type": "text", "value": "required state"},
            {"id": 1},
        ]
        turn, skipped, omitted_images = reader.codex_turn(
            {"type": "function_call_output", "output": output},
            200,
        )
        self.assertFalse(skipped)
        self.assertEqual(omitted_images, 0)
        self.assertIsNotNone(turn)
        rendered = turn["tool_results"][0]["output"]
        self.assertIn("required state", rendered)
        self.assertIn("'id': 1", rendered)

    def test_codex_database_compares_working_directories_canonically(self):
        home = self.root / "codex-case-insensitive"
        rollout = home / "sessions" / "rollout-session.jsonl"
        write_jsonl(
            rollout,
            [
                {
                    "type": "session_meta",
                    "payload": {
                        "id": "88888888-8888-8888-8888-888888888888",
                        "cwd": str(self.cwd),
                        "source": "cli",
                    },
                }
            ],
        )
        database = home / "state_1.sqlite"
        db = sqlite3.connect(database)
        db.execute(
            "CREATE TABLE threads "
            "(id TEXT, rollout_path TEXT, updated_at INTEGER, source TEXT, "
            "cwd TEXT, archived INTEGER)"
        )
        db.execute(
            "INSERT INTO threads VALUES (?, ?, ?, ?, ?, 0)",
            (
                "88888888-8888-8888-8888-888888888888",
                str(rollout),
                1,
                "cli",
                str(self.cwd).upper(),
            ),
        )
        db.commit()
        db.close()

        def case_insensitive_canonical(path):
            return os.path.realpath(os.path.expanduser(str(path))).casefold()

        with (
            patch.dict(os.environ, {"CODEX_HOME": str(home)}),
            patch.object(reader, "canonical", side_effect=case_insensitive_canonical),
        ):
            items = reader.discover("codex", str(self.cwd), 0)
        self.assertEqual(len(items), 1)
        self.assertEqual(
            items[0]["session_id"], "88888888-8888-8888-8888-888888888888"
        )

    def test_codex_does_not_scan_rollouts_after_authoritative_empty_database(self):
        home = self.root / "codex-empty"
        rollout = home / "sessions" / "fallback.jsonl"
        write_jsonl(
            rollout,
            [
                {
                    "type": "session_meta",
                    "payload": {
                        "id": "44444444-4444-4444-4444-444444444444",
                        "cwd": str(self.cwd),
                        "source": "cli",
                    },
                }
            ],
        )
        database = home / "state_1.sqlite"
        db = sqlite3.connect(database)
        db.execute(
            "CREATE TABLE threads "
            "(id TEXT, rollout_path TEXT, updated_at INTEGER, source TEXT, "
            "cwd TEXT, archived INTEGER)"
        )
        db.commit()
        db.close()
        with (
            patch.dict(os.environ, {"CODEX_HOME": str(home)}),
            patch.object(
                reader,
                "codex_metadata_from_file",
                wraps=reader.codex_metadata_from_file,
            ) as fallback_reader,
        ):
            items = reader.discover("codex", str(self.cwd), 0)
        self.assertEqual(items, [])
        fallback_reader.assert_not_called()

    def test_codex_rejects_database_rollout_outside_its_store(self):
        home = self.root / "codex-contained"
        home.mkdir()
        outside = self.root / "outside-rollout.jsonl"
        write_jsonl(
            outside,
            [
                {
                    "type": "session_meta",
                    "payload": {
                        "id": "55555555-5555-5555-5555-555555555555",
                        "cwd": str(self.cwd),
                        "source": "cli",
                    },
                }
            ],
        )
        database = home / "state_1.sqlite"
        db = sqlite3.connect(database)
        db.execute(
            "CREATE TABLE threads "
            "(id TEXT, rollout_path TEXT, updated_at INTEGER, source TEXT, "
            "cwd TEXT, archived INTEGER)"
        )
        db.execute(
            "INSERT INTO threads VALUES (?, ?, ?, ?, ?, 0)",
            (
                "55555555-5555-5555-5555-555555555555",
                str(outside),
                1,
                "cli",
                str(self.cwd),
            ),
        )
        db.commit()
        db.close()
        with patch.dict(os.environ, {"CODEX_HOME": str(home)}):
            self.assertEqual(reader.discover("codex", str(self.cwd), 0), [])

    def test_codex_discovery_rejects_out_of_range_timestamps(self):
        home = self.root / "codex-invalid-timestamp"
        rollout = home / "sessions" / "rollout-session.jsonl"
        rollout.parent.mkdir(parents=True)
        record = {
            "type": "session_meta",
            "payload": {
                "id": "66666666-6666-6666-6666-666666666666",
                "cwd": str(self.cwd),
                "source": "cli",
                "timestamp": 0,
            },
        }
        encoded = json.dumps(record).replace('"timestamp": 0', '"timestamp": 1e999')
        rollout.write_text(encoded + "\n", encoding="utf-8")
        with patch.dict(os.environ, {"CODEX_HOME": str(home)}):
            items = reader.discover("codex", str(self.cwd), 0)
        self.assertEqual(len(items), 1)
        self.assertIsNone(items[0]["created_at"])
        self.assertAlmostEqual(
            reader.timestamp_value(float("inf"), rollout),
            os.path.getmtime(rollout),
        )
        self.assertIsNone(reader.iso_time(10**1000))

    def test_cwd_matching_rejects_embedded_nul(self):
        self.assertFalse(reader.same_cwd("\0", str(self.cwd)))
        self.assertFalse(reader.same_cwd(str(self.cwd), "\0"))

    def test_codex_fallback_sanitizes_outer_harness_title(self):
        home = self.root / "codex-sanitized-title"
        rollout = home / "sessions" / "rollout-session.jsonl"
        write_jsonl(
            rollout,
            [
                {
                    "type": "session_meta",
                    "payload": {
                        "id": "99999999-9999-9999-9999-999999999999",
                        "cwd": str(self.cwd),
                        "source": "cli",
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [
                            {
                                "type": "input_text",
                                "text": (
                                    "Instructions supplied by the outer agent harness:\n"
                                    "<harness_instructions>private</harness_instructions>\n"
                                    "Current request:\n"
                                    "<current_request>Resume safe work</current_request>"
                                ),
                            }
                        ],
                    },
                },
            ],
        )
        with patch.dict(os.environ, {"CODEX_HOME": str(home)}):
            items = reader.discover("codex", str(self.cwd), 0)
        self.assertEqual(len(items), 1)
        self.assertEqual(items[0]["title"], "Resume safe work")
        self.assertNotIn("private", json.dumps(items))

    def test_codex_uses_compacted_history_and_applies_rollbacks(self):
        rollout = self.root / "codex-compacted.jsonl"
        write_jsonl(
            rollout,
            [
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [{"type": "input_text", "text": "Stale pre-compact"}],
                    },
                },
                {
                    "type": "compacted",
                    "payload": {
                        "replacement_history": [
                            {
                                "type": "message",
                                "role": "user",
                                "content": [
                                    {
                                        "type": "input_text",
                                        "text": "Replacement goal",
                                    }
                                ],
                            },
                            {
                                "type": "message",
                                "role": "assistant",
                                "content": [
                                    {
                                        "type": "output_text",
                                        "text": "Replacement action",
                                    }
                                ],
                            },
                        ]
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [
                            {"type": "input_text", "text": "Rolled-back request"}
                        ],
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "assistant",
                        "content": [
                            {"type": "output_text", "text": "Rolled-back answer"}
                        ],
                    },
                },
                {
                    "type": "event_msg",
                    "payload": {"type": "thread_rolled_back", "num_turns": 1},
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [{"type": "input_text", "text": "Active follow-up"}],
                    },
                },
            ],
        )
        item = reader.candidate(
            "codex", "codex-cli", "codex-compacted", rollout, None, str(self.cwd)
        )
        result = reader.read_codex(item, 100)
        rendered = json.dumps(result)
        self.assertIn("Replacement goal", rendered)
        self.assertIn("Replacement action", rendered)
        self.assertNotIn("Stale pre-compact", rendered)
        self.assertNotIn("Rolled-back request", rendered)
        self.assertNotIn("Rolled-back answer", rendered)
        self.assertEqual(result["last_user_request"], "Active follow-up")

    def test_codex_rollbacks_can_restore_turns_older_than_output_window(self):
        rollout = self.root / "codex-large-rollback.jsonl"
        records = []
        for index in range(250):
            records.extend(
                [
                    {
                        "type": "response_item",
                        "payload": {
                            "type": "message",
                            "role": "user",
                            "content": [
                                {
                                    "type": "input_text",
                                    "text": (
                                        "Request 0 \ud800"
                                        if index == 0
                                        else f"Request {index}"
                                    ),
                                }
                            ],
                        },
                    },
                    {
                        "type": "response_item",
                        "payload": {
                            "type": "message",
                            "role": "assistant",
                            "content": [
                                {"type": "output_text", "text": f"Answer {index}"}
                            ],
                        },
                    },
                ]
            )
        records.append(
            {
                "type": "event_msg",
                "payload": {"type": "thread_rolled_back", "num_turns": 240},
            }
        )
        write_jsonl(rollout, records)
        item = reader.candidate(
            "codex", "codex-cli", "codex-rollback", rollout, None, str(self.cwd)
        )
        result = reader.read_codex(item, 100)
        self.assertEqual(len(result["turns"]), 20)
        self.assertEqual(result["turns"][0]["text"], "Request 0 \ud800")
        self.assertEqual(result["last_user_request"], "Request 9")
        self.assertEqual(result["last_assistant_action"], "Answer 9")

    def test_claude_follows_active_parent_chain_and_bounds_tools(self):
        home = self.root / "claude"
        slug = reader.claude_project_slug(str(self.cwd))
        transcript = home / "projects" / slug / "claude-session.jsonl"
        write_jsonl(
            transcript,
            [
                {
                    "type": "user",
                    "uuid": "u1",
                    "parentUuid": None,
                    "sessionId": "claude-session",
                    "cwd": str(self.cwd),
                    "timestamp": "2026-01-01T00:00:00Z",
                    "message": {
                        "content": (
                            "Instructions supplied by the outer agent harness:\n"
                            "<harness_instructions>outer secret</harness_instructions>\n"
                            "Current request:\n"
                            "<current_request>Add a test</current_request>"
                        )
                    },
                },
                {
                    "type": "attachment",
                    "uuid": "attachment",
                    "parentUuid": "u1",
                    "sessionId": "claude-session",
                    "cwd": str(self.cwd),
                    "timestamp": "2026-01-01T00:00:30Z",
                    "message": {"content": "untrusted attachment metadata"},
                },
                {
                    "type": "assistant",
                    "uuid": "a1",
                    "parentUuid": "attachment",
                    "sessionId": "claude-session",
                    "cwd": str(self.cwd),
                    "timestamp": "2026-01-01T00:01:00Z",
                    "message": {
                        "content": [
                            {"type": "thinking", "thinking": "private chain"},
                            {
                                "type": "tool_use",
                                "name": "Read",
                                "input": {"file_path": "A" * 200},
                            },
                            {
                                "type": "image",
                                "source": {
                                    "type": "base64",
                                    "data": "claude-image-secret",
                                },
                            },
                            {"type": "text", "text": "Reading the spec"},
                        ]
                    },
                },
                {
                    "type": "assistant",
                    "uuid": "stale",
                    "parentUuid": "u1",
                    "sessionId": "claude-session",
                    "cwd": str(self.cwd),
                    "timestamp": "2025-01-01T00:01:00Z",
                    "message": {"content": "stale branch"},
                },
                {
                    "type": "user",
                    "uuid": "meta",
                    "parentUuid": "a1",
                    "sessionId": "claude-session",
                    "cwd": str(self.cwd),
                    "timestamp": "2026-01-01T00:02:00Z",
                    "isMeta": True,
                    "message": {"content": "hidden metadata"},
                },
                {
                    "type": "assistant",
                    "uuid": "sidechain",
                    "parentUuid": "u1",
                    "sessionId": "claude-session",
                    "cwd": str(self.cwd),
                    "timestamp": "2027-01-01T00:00:00Z",
                    "isSidechain": True,
                    "message": {"content": "newer subagent branch"},
                },
                {
                    "type": "custom-title",
                    "sessionId": "claude-session",
                    "timestamp": "2026-01-01T00:03:00Z",
                    "customTitle": "Renamed Claude task",
                },
            ],
        )
        with patch.dict(os.environ, {"CLAUDE_CONFIG_DIR": str(home)}):
            items = reader.discover("claude", str(self.cwd), 0)
            selected = reader.resolve(
                "claude", str(self.cwd), "Renamed Claude task", 0
            )
            result = reader.read_claude(items[0], 40)
        rendered = json.dumps(result)
        self.assertEqual(result["last_user_request"], "Add a test")
        self.assertIn("Reading the spec", rendered)
        self.assertNotIn("stale branch", rendered)
        self.assertNotIn("newer subagent branch", rendered)
        self.assertNotIn("outer secret", rendered)
        self.assertNotIn("untrusted attachment metadata", rendered)
        self.assertNotIn("private chain", rendered)
        self.assertNotIn("hidden metadata", rendered)
        self.assertNotIn("claude-image-secret", rendered)
        self.assertIn(
            "image_content_omitted",
            {item["code"] for item in result["warnings"]},
        )
        self.assertEqual(items[0]["title"], "Renamed Claude task")
        self.assertEqual(selected["session_id"], "claude-session")
        self.assertEqual(items[0]["updated_at"], "2026-01-01T00:03:00Z")
        call = result["turns"][-1]["tool_calls"][0]
        self.assertLessEqual(len(call["arguments"]), 40)

    def test_claude_metadata_skips_json_recursion_errors(self):
        home = self.root / "claude-recursion"
        slug = reader.claude_project_slug(str(self.cwd))
        transcript = home / "projects" / slug / "claude-session.jsonl"
        transcript.parent.mkdir(parents=True)
        transcript.write_text("deep\nmetadata\n", encoding="utf-8")
        recovered = {
            "type": "user",
            "uuid": "u1",
            "parentUuid": None,
            "sessionId": "claude-session",
            "cwd": str(self.cwd),
            "timestamp": "2026-01-01T00:00:00Z",
            "message": {"role": "user", "content": "Recovered Claude work"},
        }
        with (
            patch.dict(os.environ, {"CLAUDE_CONFIG_DIR": str(home)}),
            patch.object(
                reader.json,
                "loads",
                side_effect=[
                    RecursionError("too deeply nested"),
                    recovered,
                ],
            ),
        ):
            items = reader.discover("claude", str(self.cwd), 0)
        self.assertEqual(len(items), 1)
        self.assertEqual(items[0]["session_id"], "claude-session")
        self.assertEqual(items[0]["title"], "Recovered Claude work")

    def test_claude_selected_transcript_streams_and_bounds_active_chain(self):
        transcript = self.root / "claude-session.jsonl"
        records = [
            {
                "type": "user",
                "uuid": "root",
                "parentUuid": None,
                "timestamp": 0,
                "message": {
                    "role": "user",
                    "content": "Original Claude request",
                },
            }
        ]
        parent = "root"
        for index in range(reader.MAX_TURNS + 50):
            uuid = f"assistant-{index}"
            records.append(
                {
                    "type": "assistant",
                    "uuid": uuid,
                    "parentUuid": parent,
                    "timestamp": index + 1,
                    "message": {
                        "role": "assistant",
                        "content": f"Claude answer {index}",
                    },
                }
            )
            parent = uuid
        write_jsonl(transcript, records)
        item = reader.candidate(
            "claude",
            "claude-code",
            "claude-session",
            transcript,
            "Original Claude request",
            str(self.cwd),
        )
        with patch.object(
            reader,
            "read_jsonl",
            side_effect=AssertionError("selected Claude transcript was buffered"),
        ):
            result = reader.read_claude(item, 100)
        self.assertEqual(len(result["turns"]), reader.MAX_TURNS)
        self.assertEqual(result["turns"][0]["text"], "Claude answer 50")
        self.assertEqual(result["last_user_request"], "Original Claude request")
        self.assertEqual(
            result["last_assistant_action"],
            f"Claude answer {reader.MAX_TURNS + 49}",
        )
        self.assertIn(
            "turns_truncated",
            {entry["code"] for entry in result["warnings"]},
        )

    def test_claude_prefers_latest_leaf_when_timestamps_tie(self):
        transcript = self.root / "claude-tied-leaves.jsonl"
        write_jsonl(
            transcript,
            [
                {
                    "type": "user",
                    "uuid": "root",
                    "parentUuid": None,
                    "message": {"content": "Original request"},
                },
                {
                    "type": "assistant",
                    "uuid": "stale-leaf",
                    "parentUuid": "root",
                    "message": {"content": "Stale branch"},
                },
                {
                    "type": "assistant",
                    "uuid": "latest-leaf",
                    "parentUuid": "root",
                    "message": {"content": "Latest branch"},
                },
            ],
        )
        item = reader.candidate(
            "claude",
            "claude-code",
            "claude-tied-leaves",
            transcript,
            "Original request",
            str(self.cwd),
        )
        result = reader.read_claude(item, 100)
        rendered = json.dumps(result)
        self.assertIn("Latest branch", rendered)
        self.assertNotIn("Stale branch", rendered)
        self.assertEqual(result["last_assistant_action"], "Latest branch")

    def test_harness_wrapper_uses_prior_request_when_current_request_is_empty(self):
        wrapped = (
            "Instructions supplied by the outer agent harness:\n"
            "<harness_instructions>do not expose this</harness_instructions>\n\n"
            "Prior conversation imported from the outer agent harness.\n"
            "<prior_conversation>\n"
            "System:\nunsafe system text\n\n"
            "User:\nfirst request\n\n"
            "Assistant:\nintermediate result\n\n"
            "User:\nmerge\n"
            "</prior_conversation>\n\n"
            "Current request:\n<current_request></current_request>"
        )
        self.assertEqual(reader.user_text(wrapped), "merge")
        self.assertEqual(reader.user_text("<user_query></user_query>"), "")

    def test_quoted_resume_tags_are_not_treated_as_harness_wrappers(self):
        for tag in ("current_request", "user_query"):
            with self.subTest(tag=tag):
                text = f"Document `<{tag}>example</{tag}>` in the guide"
                self.assertEqual(reader.user_text(text), text)

    def test_cursor_cli_recovers_json_rows_and_skips_system_fields(self):
        home = self.root / "cursor"
        digest = hashlib.md5(reader.canonical(self.cwd).encode("utf-8")).hexdigest()
        session_id = "33333333-3333-3333-3333-333333333333"
        session = home / "chats" / digest / session_id
        session.mkdir(parents=True)
        (session / "meta.json").write_text(
            json.dumps(
                {
                    "id": session_id,
                    "cwd": str(self.cwd),
                    "title": "Cursor task",
                    "updatedAt": 2,
                }
            )
        )
        db = sqlite3.connect(session / "store.db")
        db.execute("CREATE TABLE blobs (key TEXT, value TEXT)")
        db.execute(
            "INSERT INTO blobs VALUES (?, ?)",
            (
                "conversation",
                json.dumps(
                    {
                        "messages": [
                            {"role": "system", "text": "unsafe instruction"},
                            {
                                "role": "user",
                                "content": [
                                    {
                                        "type": "text",
                                        "text": "Continue Cursor work",
                                    },
                                    {
                                        "type": "image",
                                        "data": "cursor-image-secret",
                                    },
                                ],
                            },
                            {"role": "assistant", "text": "Updated Main.hs"},
                        ],
                        "metadata": {
                            "role": "user",
                            "content": "instruction-like metadata",
                        },
                    }
                ),
            ),
        )
        db.commit()
        db.close()
        with patch.dict(os.environ, {"CURSOR_HOME": str(home)}):
            items = reader.discover("cursor", str(self.cwd), 0)
            other_cwd = self.root / "other-repo"
            other_cwd.mkdir()
            selected = reader.resolve("cursor", str(other_cwd), session_id, 0)
            result = reader.read_cursor(items[0], 100)
        rendered = json.dumps(result)
        self.assertEqual(selected["path"], str(session))
        self.assertIn("Continue Cursor work", rendered)
        self.assertIn("Updated Main.hs", rendered)
        self.assertNotIn("unsafe instruction", rendered)
        self.assertNotIn("instruction-like metadata", rendered)
        self.assertNotIn("cursor-image-secret", rendered)
        self.assertIn(
            "image_content_omitted",
            {item["code"] for item in result["warnings"]},
        )

    def test_cursor_session_ids_are_literal_when_finding_transcripts(self):
        home = self.root / "cursor-literal-session"
        session = home / "chats" / "project" / "selected"
        session.mkdir(parents=True)
        store = sqlite3.connect(session / "store.db")
        store.execute("CREATE TABLE blobs (key TEXT, value TEXT)")
        store.execute(
            "INSERT INTO blobs VALUES (?, ?)",
            (
                "conversation",
                json.dumps(
                    {"messages": [{"role": "user", "text": "Local store work"}]}
                ),
            ),
        )
        store.commit()
        store.close()

        project = home / "projects" / "project"
        project.mkdir(parents=True)
        (project / ".workspace-trusted").write_text(
            json.dumps({"workspacePath": str(self.cwd)}),
            encoding="utf-8",
        )
        transcript = (
            project
            / "agent-transcripts"
            / "literal-session"
            / "literal-session.jsonl"
        )
        write_jsonl(
            transcript,
            [{"role": "user", "text": "Unrelated transcript work"}],
        )
        item = reader.candidate(
            "cursor",
            "cursor-cli",
            "*",
            session,
            "Selected Cursor work",
            str(self.cwd),
        )
        with patch.dict(os.environ, {"CURSOR_HOME": str(home)}):
            self.assertEqual(
                reader.cursor_transcript_for_session(
                    "literal-session", str(self.cwd)
                ),
                transcript,
            )
            result = reader.read_cursor(item, 100)
        rendered = json.dumps(result)
        self.assertIn("Local store work", rendered)
        self.assertNotIn("Unrelated transcript work", rendered)

    def test_cursor_session_ids_are_literal_in_desktop_queries(self):
        database = self.root / "cursor-literal-id.vscdb"
        db = sqlite3.connect(database)
        db.execute("CREATE TABLE cursorDiskKV (key TEXT, value TEXT)")
        db.executemany(
            "INSERT INTO cursorDiskKV VALUES (?, ?)",
            [
                (
                    "composerData:%",
                    json.dumps(
                        {"messages": [{"role": "user", "text": "Selected work"}]}
                    ),
                ),
                (
                    "bubbleId:another-session:1",
                    json.dumps(
                        {"role": "assistant", "text": "Unrelated wildcard match"}
                    ),
                ),
            ],
        )
        db.commit()
        db.close()
        item = reader.candidate(
            "cursor",
            "cursor-desktop",
            "%",
            database,
            "Selected desktop work",
            str(self.cwd),
        )
        with patch.dict(
            os.environ, {"CURSOR_HOME": str(self.root / "cursor-desktop")}
        ):
            result = reader.read_cursor(item, 100)
        rendered = json.dumps(result)
        self.assertIn("Selected work", rendered)
        self.assertNotIn("Unrelated wildcard match", rendered)

    def test_cursor_desktop_warns_about_partially_unavailable_rows(self):
        home = self.root / "cursor-desktop"
        database = self.root / "state.vscdb"
        session_id = "desktop-partial"
        db = sqlite3.connect(database)
        db.execute("CREATE TABLE cursorDiskKV (key TEXT, value BLOB)")
        db.execute(
            "INSERT INTO cursorDiskKV VALUES (?, ?)",
            (
                "composerData:" + session_id,
                json.dumps(
                    {
                        "messages": [
                            {"role": "user", "text": "Recovered desktop work"}
                        ]
                    }
                ),
            ),
        )
        db.execute(
            "INSERT INTO cursorDiskKV VALUES (?, ?)",
            ("bubbleId:" + session_id + ":binary", sqlite3.Binary(b"\xff\x00")),
        )
        db.commit()
        db.close()
        item = reader.candidate(
            "cursor",
            "cursor-desktop",
            session_id,
            database,
            "Desktop task",
            str(self.cwd),
        )
        with patch.dict(os.environ, {"CURSOR_HOME": str(home)}):
            result = reader.read_cursor(item, 100)
        self.assertEqual(result["last_user_request"], "Recovered desktop work")
        self.assertIn(
            "binary_content_unavailable",
            {entry["code"] for entry in result["warnings"]},
        )

    def test_grok_discovers_encoded_cwd_and_drops_reasoning(self):
        home = self.root / "grok"
        encoded = str(self.cwd).replace("/", "%2F")
        session = home / "sessions" / encoded / "grok-session"
        session.mkdir(parents=True)
        (session / "summary.json").write_text(
            json.dumps(
                {
                    "info": {"id": "grok-session", "cwd": str(self.cwd)},
                    "session_summary": "Grok task",
                    "created_at": "2026-01-01T00:00:00Z",
                    "updated_at": "2026-01-01T00:01:00Z",
                }
            )
        )
        write_jsonl(
            session / "chat_history.jsonl",
            [
                {"type": "system", "content": "unsafe instruction"},
                {
                    "type": "user",
                    "content": [
                        {"type": "text", "text": "Continue Grok work"},
                        {"type": "image", "data": "grok-image-secret"},
                    ],
                },
                {
                    "type": "user",
                    "content": "synthetic project instructions",
                    "synthetic_reason": "project_instructions",
                },
                {"type": "reasoning", "content": "private chain"},
                {
                    "type": "assistant",
                    "content": "Stopped after tests",
                    "tool_calls": [
                        {
                            "id": "call-1",
                            "name": "shell",
                            "arguments": {"command": "x" * 200},
                        }
                    ],
                },
            ],
        )
        with patch.dict(os.environ, {"GROK_HOME": str(home)}):
            items = reader.discover("grok", str(self.cwd), 0)
            result = reader.read_grok(items[0], 40)
        rendered = json.dumps(result)
        self.assertIn("Continue Grok work", rendered)
        self.assertIn("Stopped after tests", rendered)
        self.assertNotIn("unsafe instruction", rendered)
        self.assertNotIn("synthetic project instructions", rendered)
        self.assertNotIn("private chain", rendered)
        self.assertNotIn("grok-image-secret", rendered)
        self.assertIn(
            "image_content_omitted",
            {item["code"] for item in result["warnings"]},
        )
        call = result["turns"][-1]["tool_calls"][0]
        self.assertEqual(call["name"], "shell")
        self.assertLessEqual(len(call["arguments"]), 40)

    def test_grok_selected_transcript_streams_and_bounds_turns(self):
        session = self.root / "grok-session"
        session.mkdir()
        write_jsonl(
            session / "chat_history.jsonl",
            [{"type": "user", "content": "Original Grok request"}]
            + [
                {"type": "assistant", "content": f"Grok answer {index}"}
                for index in range(reader.MAX_TURNS + 50)
            ],
        )
        item = reader.candidate(
            "grok",
            "grok-build",
            "grok-session",
            session,
            "Original Grok request",
            str(self.cwd),
        )
        with patch.object(
            reader,
            "read_jsonl",
            side_effect=AssertionError("selected Grok transcript was buffered"),
        ):
            result = reader.read_grok(item, 100)
        self.assertEqual(len(result["turns"]), reader.MAX_TURNS)
        self.assertEqual(result["last_user_request"], "Original Grok request")
        self.assertEqual(
            result["last_assistant_action"],
            f"Grok answer {reader.MAX_TURNS + 49}",
        )
        self.assertIn(
            "turns_truncated",
            {entry["code"] for entry in result["warnings"]},
        )

    def test_cursor_cli_discovery_rejects_mismatched_metadata_cwd(self):
        home = self.root / "cursor-mismatched-cwd"
        digest = hashlib.md5(reader.canonical(self.cwd).encode("utf-8")).hexdigest()
        session = home / "chats" / digest / "mismatched-session"
        session.mkdir(parents=True)
        other_cwd = self.root / "other-repository"
        other_cwd.mkdir()
        (session / "meta.json").write_text(
            json.dumps(
                {
                    "id": "mismatched-session",
                    "cwd": str(other_cwd),
                    "title": "Foreign Cursor task",
                }
            ),
            encoding="utf-8",
        )
        with patch.dict(os.environ, {"CURSOR_HOME": str(home)}):
            self.assertEqual(reader.discover_cursor(str(self.cwd)), [])

    def test_cursor_does_not_assign_pathless_desktop_sessions_to_current_repo(self):
        database = self.root / "state.vscdb"
        db = sqlite3.connect(database)
        db.execute("CREATE TABLE ItemTable (key TEXT, value TEXT)")
        db.execute(
            "INSERT INTO ItemTable VALUES (?, ?)",
            (
                "composer.composerHeaders",
                json.dumps(
                    {
                        "allComposers": [
                            {
                                "composerId": "desktop-session",
                                "name": "Unscoped desktop task",
                                "lastUpdatedAt": 2,
                            }
                        ]
                    }
                ),
            ),
        )
        db.commit()
        db.close()
        with (
            patch.dict(os.environ, {"CURSOR_HOME": str(self.root / "cursor")}),
            patch.object(reader, "cursor_desktop_databases", return_value=[database]),
        ):
            self.assertEqual(reader.discover("cursor", str(self.cwd), 0), [])
        with self.assertRaisesRegex(reader.ReaderError, "ambiguous"):
            reader.resolve("cursor", str(self.cwd), str(database), 0)

    def test_cursor_transcript_discovery_stops_after_metadata_and_title(self):
        home = self.root / "cursor-streaming"
        transcript = (
            home
            / "projects"
            / reader.cursor_project_slug(str(self.cwd))
            / "agent-transcripts"
            / "cursor-session"
            / "cursor-session.jsonl"
        )
        transcript.parent.mkdir(parents=True)
        transcript.touch()
        lines = iter(
            [
                json.dumps({"workspacePath": str(self.cwd)}) + "\n",
                json.dumps({"role": "user", "text": "Resume Cursor work"}) + "\n",
            ]
        )

        class PrefixOnlyStream:
            def __init__(self):
                self.records_read = 0

            def __enter__(self):
                return self

            def __exit__(self, *_):
                return False

            def __iter__(self):
                return self

            def __next__(self):
                self.records_read += 1
                if self.records_read > 2:
                    raise AssertionError("Cursor discovery consumed the transcript tail")
                return next(lines)

        stream = PrefixOnlyStream()
        with (
            patch.dict(os.environ, {"CURSOR_HOME": str(home)}),
            patch.object(reader, "open", return_value=stream, create=True),
        ):
            item = reader.cursor_transcript_candidate(transcript, str(self.cwd))
        self.assertIsNotNone(item)
        self.assertEqual(item["title"], "Resume Cursor work")
        self.assertEqual(stream.records_read, 2)

    def test_cursor_metadata_helpers_are_iterative(self):
        nested_cwd = {"workspacePath": str(self.cwd)}
        nested_title = {"role": "user", "text": "Deep Cursor work"}
        for _ in range(1_200):
            nested_cwd = {"nested": nested_cwd}
            nested_title = {"messages": [nested_title]}
        self.assertEqual(
            reader.nested_strings(nested_cwd, reader.CURSOR_CWD_KEYS),
            [str(self.cwd)],
        )
        self.assertEqual(
            reader.cursor_first_user_title(nested_title), "Deep Cursor work"
        )
        self.assertEqual(
            reader.generic_cursor_turns(nested_title, 100)[0]["text"],
            "Deep Cursor work",
        )

    def test_cursor_transcript_skips_json_recursion_errors(self):
        home = self.root / "cursor-recursion"
        transcript = (
            home
            / "projects"
            / reader.cursor_project_slug(str(self.cwd))
            / "agent-transcripts"
            / "cursor-session"
            / "cursor-session.jsonl"
        )
        transcript.parent.mkdir(parents=True)
        transcript.write_text("deep\nmetadata\ntitle\n", encoding="utf-8")
        with (
            patch.dict(os.environ, {"CURSOR_HOME": str(home)}),
            patch.object(
                reader.json,
                "loads",
                side_effect=[
                    RecursionError("too deeply nested"),
                    {"workspacePath": str(self.cwd)},
                    {"role": "user", "text": "Recovered Cursor work"},
                ],
            ),
        ):
            item = reader.cursor_transcript_candidate(transcript, str(self.cwd))
        self.assertIsNotNone(item)
        self.assertEqual(item["title"], "Recovered Cursor work")

    def test_cursor_transcript_ignores_cwd_values_in_message_payloads(self):
        home = self.root / "cursor-foreign"
        project = home / "projects" / "foreign-project"
        transcript = (
            project
            / "agent-transcripts"
            / "foreign-session"
            / "foreign-session.jsonl"
        )
        write_jsonl(
            transcript,
            [
                {
                    "role": "assistant",
                    "message": {
                        "tool": {
                            "arguments": {"cwd": str(self.cwd)},
                        }
                    },
                },
                {"role": "user", "text": "Foreign Cursor work"},
            ],
        )
        (project / ".workspace-trusted").write_text(
            json.dumps({"workspacePath": str(self.root / "other-repo")}),
            encoding="utf-8",
        )
        with (
            patch.dict(os.environ, {"CURSOR_HOME": str(home)}),
            patch.object(reader, "cursor_desktop_databases", return_value=[]),
        ):
            items = reader.discover("cursor", str(self.cwd), 0)
        self.assertEqual(items, [])

    def test_cursor_discovery_deduplicates_storage_sources_by_session_id(self):
        home = self.root / "cursor-duplicates"
        project = home / "projects" / "project"
        session_id = "duplicate-session"
        transcript = (
            project
            / "agent-transcripts"
            / session_id
            / f"{session_id}.jsonl"
        )
        write_jsonl(
            transcript,
            [{"role": "user", "text": "Duplicate Cursor task"}],
        )
        (project / ".workspace-trusted").write_text(
            json.dumps({"workspacePath": str(self.cwd)}),
            encoding="utf-8",
        )
        database = self.root / "duplicate-state.vscdb"
        db = sqlite3.connect(database)
        db.execute("CREATE TABLE ItemTable (key TEXT, value TEXT)")
        db.execute(
            "INSERT INTO ItemTable VALUES (?, ?)",
            (
                "composer.composerHeaders",
                json.dumps(
                    {
                        "allComposers": [
                            {
                                "composerId": session_id,
                                "name": "Duplicate Cursor task",
                                "workspacePath": str(self.cwd),
                                "lastUpdatedAt": 1,
                            }
                        ]
                    }
                ),
            ),
        )
        db.commit()
        db.close()
        with (
            patch.dict(os.environ, {"CURSOR_HOME": str(home)}),
            patch.object(
                reader,
                "cursor_desktop_databases",
                return_value=[database],
            ),
        ):
            items = reader.discover("cursor", str(self.cwd), 0)
            selected = reader.resolve(
                "cursor",
                str(self.cwd),
                "Duplicate Cursor task",
                0,
            )
        self.assertEqual(len(items), 1)
        self.assertEqual(items[0]["session_id"], session_id)
        self.assertEqual(items[0]["source"], "cursor-transcript")
        self.assertEqual(selected["session_id"], session_id)

    def test_cursor_selected_transcript_streams_and_bounds_turns(self):
        transcript = self.root / "cursor-session.jsonl"
        write_jsonl(
            transcript,
            [{"role": "user", "text": "Original Cursor request"}]
            + [
                {"role": "assistant", "text": f"Cursor answer {index}"}
                for index in range(reader.MAX_TURNS + 50)
            ],
        )
        item = reader.candidate(
            "cursor",
            "cursor-transcript",
            "cursor-session",
            transcript,
            "Original Cursor request",
            str(self.cwd),
        )
        with patch.object(
            reader,
            "read_jsonl",
            side_effect=AssertionError("selected Cursor transcript was buffered"),
        ):
            result = reader.read_cursor(item, 100)
        self.assertEqual(len(result["turns"]), reader.MAX_TURNS)
        self.assertEqual(result["last_user_request"], "Original Cursor request")
        self.assertEqual(
            result["last_assistant_action"],
            f"Cursor answer {reader.MAX_TURNS + 49}",
        )
        self.assertIn(
            "turns_truncated",
            {entry["code"] for entry in result["warnings"]},
        )

    def test_finalise_preserves_last_request_when_old_turns_are_truncated(self):
        turns = [reader.inert_turn("user", "Original goal")]
        turns.extend(
            reader.inert_turn(
                "assistant",
                tool_calls=[{"name": "read", "arguments": "file"}],
            )
            for _ in range(reader.MAX_TURNS)
        )
        result = reader.finalise({"turns": turns}, [])
        self.assertEqual(result["last_user_request"], "Original goal")
        self.assertEqual(len(result["turns"]), reader.MAX_TURNS)
        self.assertIn("turns_truncated", {item["code"] for item in result["warnings"]})

    def test_emit_json_escapes_lone_surrogates(self):
        raw = io.BytesIO()
        stdout = io.TextIOWrapper(raw, encoding="utf-8", errors="strict")
        with patch.object(reader.sys, "stdout", stdout):
            reader.emit({"title": "invalid \ud800 title"}, True)
            stdout.flush()
        rendered = raw.getvalue().decode("utf-8")
        stdout.detach()
        self.assertIn("\\ud800", rendered)
        self.assertEqual(json.loads(rendered)["title"], "invalid \ud800 title")

    def test_parser_preserves_option_like_reference_values(self):
        for reference in ("--json", "-h"):
            with self.subTest(reference=reference):
                args = reader.parser().parse_args(
                    [
                        "codex",
                        "show",
                        f"--reference={reference}",
                        "--cwd",
                        str(self.cwd),
                        "--json",
                    ]
                )
                self.assertEqual(args.reference_option, reference)
                self.assertIsNone(args.reference)

    def test_human_list_escapes_terminal_control_characters(self):
        stdout = io.StringIO()
        with patch.object(reader.sys, "stdout", stdout):
            reader.emit(
                [
                    {
                        "updated_at": "now\x1b]52;c;clipboard\x07",
                        "session_id": "session\nid",
                        "title": "task\u202e",
                    }
                ],
                False,
            )
        rendered = stdout.getvalue()
        self.assertNotIn("\x1b", rendered)
        self.assertNotIn("\x07", rendered)
        self.assertNotIn("\u202e", rendered)
        self.assertIn("\\x1b", rendered)
        self.assertIn("\\x07", rendered)
        self.assertIn("\\n", rendered)
        self.assertIn("\\u202e", rendered)

    @unittest.skipUnless(shutil.which("zstd"), "zstd is not installed")
    def test_codex_reads_compressed_rollout(self):
        source = self.root / "rollout.jsonl"
        compressed = self.root / "rollout.jsonl.zst"
        write_jsonl(
            source,
            [
                {
                    "type": "session_meta",
                    "payload": {
                        "id": "22222222-2222-2222-2222-222222222222",
                        "cwd": str(self.cwd),
                        "source": "cli",
                    },
                },
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [
                            {"type": "input_text", "text": "Resume compressed work"}
                        ],
                    },
                },
            ],
        )
        subprocess.run(
            ["zstd", "-q", "-f", str(source), "-o", str(compressed)],
            check=True,
        )
        item = reader.codex_metadata_from_file(compressed)
        self.assertIsNotNone(item)
        with patch.object(
            reader.subprocess,
            "run",
            side_effect=AssertionError("selected rollout was fully buffered"),
        ):
            result = reader.read_codex(item, 100)
        self.assertEqual(result["last_user_request"], "Resume compressed work")

    def test_codex_metadata_stops_after_300_compressed_records(self):
        compressed = self.root / "rollout.jsonl.zst"
        compressed.touch()
        fully_consumed = self.root / "fully-consumed"
        fake_zstd = self.root / "fake-zstd"
        records = [
            {
                "type": "session_meta",
                "payload": {
                    "id": "77777777-7777-7777-7777-777777777777",
                    "cwd": str(self.cwd),
                    "source": "cli",
                },
            }
        ]
        records.extend({"type": "ignored"} for _ in range(299))
        fake_zstd.write_text(
            "#!/usr/bin/env python3\n"
            "import json\n"
            "from pathlib import Path\n"
            f"records = {records!r}\n"
            "for record in records:\n"
            "    print(json.dumps(record))\n"
            "for index in range(1_000_000):\n"
            "    print(json.dumps({'type': 'tail', 'index': index}))\n"
            f"Path({str(fully_consumed)!r}).write_text('done')\n",
            encoding="utf-8",
        )
        fake_zstd.chmod(0o755)
        with patch.object(reader.shutil, "which", return_value=str(fake_zstd)):
            item = reader.codex_metadata_from_file(compressed)
        self.assertIsNotNone(item)
        self.assertEqual(
            item["session_id"], "77777777-7777-7777-7777-777777777777"
        )
        self.assertFalse(fully_consumed.exists())

    def test_all_recovered_turns_are_explicitly_inert(self):
        turns = [
            reader.inert_turn("user", "request"),
            reader.inert_turn("assistant", "result"),
        ]
        self.assertTrue(
            all(
                turn["inert"]
                and turn["trust"] == "untrusted_external_history"
                for turn in turns
            )
        )


if __name__ == "__main__":
    unittest.main()
