"""Run with the benchmark Nix environment: python -m unittest discover."""

from pathlib import Path
import asyncio
from datetime import datetime
import json
import re
import shlex
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import AsyncMock, patch

from harbor.models.agent.context import AgentContext

from haskell_agent import HaskellAgent


class HaskellAgentTests(unittest.IsolatedAsyncioTestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="haskell-agent-adapter-test-")
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.archive = self.root / "closure.tar"
        self.archive.touch()
        self.credentials = self.root / "auth.json"
        self.credentials.write_text('{"fixture": "credential-value-not-for-logs"}')
        self.arguments = {
            "logs_dir": self.root / "logs",
            "model_name": "test-model",
            "closure_archive": str(self.archive),
            "executable_path": "/nix/store/test-agent/bin/agent-cli",
            "codex_auth_path": str(self.credentials),
            "support_archive": str(self.archive),
            "sudo_executable": "/nix/store/test-sudo/bin/sudo",
            "ca_bundle_path": str(self.archive),
        }
        self.environment = SimpleNamespace(
            exec=AsyncMock(return_value=SimpleNamespace(
                return_code=0, stdout="agent-cli test-version\n", stderr=""
            )),
            upload_file=AsyncMock(),
            download_file=AsyncMock(),
        )

    def test_explicit_model_required(self):
        self.arguments["model_name"] = None
        with self.assertRaisesRegex(ValueError, "explicit model"):
            HaskellAgent(**self.arguments)

    def test_root_account_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "nonroot"):
            HaskellAgent(**self.arguments, run_user="root")

    def test_executable_must_be_in_closure(self):
        self.arguments["executable_path"] = "/usr/bin/agent-cli"
        with self.assertRaisesRegex(ValueError, "Nix closure"):
            HaskellAgent(**self.arguments)

    def test_command_quotes_model_and_clears_environment(self):
        self.arguments["model_name"] = "test-model; echo unexpected"
        agent = HaskellAgent(**self.arguments)
        arguments = shlex.split(agent.execution_command())
        self.assertEqual(arguments[:2], ["env", "-i"])
        self.assertEqual(arguments[arguments.index("--model") + 1], self.arguments["model_name"])
        for flag in ("--no-agents-md", "--no-skills", "--no-computer-use", "--yolo", "--save-session"):
            self.assertIn(flag, arguments)
        self.assertNotIn("credential-value-not-for-logs", agent.execution_command())

    def test_ca_bundle_is_explicit_and_quoted(self):
        agent = HaskellAgent(**self.arguments)
        path = agent.ca_bundle_path
        arguments = shlex.split(agent.execution_command())
        self.assertIn("SSL_CERT_FILE=" + path, arguments)
        self.assertIn("NIX_SSL_CERT_FILE=" + path, arguments)

    def test_ca_bundle_must_exist(self):
        self.arguments["ca_bundle_path"] = str(self.root / "missing.crt")
        with self.assertRaises(FileNotFoundError):
            HaskellAgent(**self.arguments)

    async def test_setup_uploads_credentials_without_command_interpolation(self):
        agent = HaskellAgent(**self.arguments)
        await agent.setup(self.environment)
        self.environment.upload_file.assert_any_await(
            self.credentials, agent.home_directory + "/.codex/auth.json"
        )
        self.assertEqual(agent.version(), "agent-cli test-version")
        for call in self.environment.exec.call_args_list:
            self.assertNotIn("credential-value-not-for-logs", call.kwargs["command"])

    def test_defaults_use_medium_effort_and_minimal_mode(self):
        agent = HaskellAgent(**self.arguments)
        self.assertEqual(agent.effort, "medium")
        self.assertIn("--minimal", shlex.split(agent.execution_command()))

    def test_stock_codex_command_preserves_comparison_settings(self):
        from stock_codex import StockCodex

        agent = StockCodex(**self.arguments)
        command = agent.execution_command()
        arguments = shlex.split(command)
        self.assertEqual(agent.name(), "stock-codex")
        self.assertIn('model_reasoning_effort="medium"', arguments)
        self.assertIn("--dangerously-bypass-approvals-and-sandbox", arguments)
        self.assertIn("--skip-git-repo-check", arguments)
        self.assertIn("--json", arguments)
        self.assertIn("<" + agent.prompt_path, command)
        self.assertNotIn("--max-turns", arguments)

    async def test_setup_installs_support_without_changing_task_permissions(self):
        agent = HaskellAgent(**self.arguments)
        await agent.setup(self.environment)
        self.environment.upload_file.assert_any_await(self.archive, agent.ca_bundle_path)
        commands = "\n".join(c.kwargs["command"] for c in self.environment.exec.call_args_list)
        self.assertIn("install -m 4755 -o root -g root", commands)
        self.assertIn("!pam_session, !pam_acct_mgmt", commands)
        self.assertIn("mkdir -p .haskell-agent && chown benchmark .haskell-agent", commands)
        self.assertNotIn("visudo", commands)
        self.assertNotIn("/app", commands)

    async def test_run_preserves_instruction_without_shell_interpolation(self):
        instruction = "Text with ' quotes\n$(do-not-execute)"
        captured = []

        async def capture_upload(source, destination):
            captured.append((Path(source).read_text(), destination))

        self.environment.upload_file.side_effect = capture_upload
        agent = HaskellAgent(**self.arguments)
        context = AgentContext()
        await agent.run(instruction, self.environment, context)
        self.assertEqual(len(captured), 1)
        self.assertTrue(captured[0][0].endswith("\n\n" + instruction))
        self.assertEqual(captured[0][1], agent.prompt_path)
        self.assertIsNone(context.n_input_tokens)
        self.assertIsNone(context.cost_usd)
        self.assertEqual(context.metadata["exit_code"], 0)
        self.assertNotIn(instruction, self.environment.exec.call_args.kwargs["command"])

    async def test_nonzero_agent_exit_raises(self):
        self.environment.exec.side_effect = [
            SimpleNamespace(return_code=0, stdout="", stderr=""),
            SimpleNamespace(return_code=9, stdout="", stderr="sensitive output"),
        ]
        agent = HaskellAgent(**self.arguments)
        context = AgentContext()
        with self.assertRaisesRegex(RuntimeError, "exit 9") as raised:
            await agent.run("task", self.environment, context)
        self.assertNotIn("sensitive output", str(raised.exception))
        self.assertEqual(context.metadata["exit_code"], 9)

    async def test_execution_timing_excludes_transcript_export(self):
        agent = HaskellAgent(**self.arguments)
        agent.logs_dir.mkdir(parents=True)
        agent._preserve_transcripts = AsyncMock()
        # These boundaries deliberately make export longer than execution.
        with patch("haskell_agent.time.monotonic", side_effect=[100, 103, 104, 111]):
            await agent.run("task", self.environment, AgentContext())
        timing = json.loads((agent.logs_dir / "execution-timing.json").read_text())
        self.assertEqual(timing["execution_seconds"], 3)
        self.assertEqual(timing["transcript_export_seconds"], 7)
        for field in ("execution_started_at", "execution_finished_at"):
            self.assertIsNotNone(datetime.fromisoformat(timing[field]).tzinfo)
        agent._preserve_transcripts.assert_awaited_once()

    async def test_nonzero_exit_preserves_execution_timing(self):
        agent = HaskellAgent(**self.arguments)
        agent.logs_dir.mkdir(parents=True)
        agent._preserve_transcripts = AsyncMock()
        self.environment.exec.side_effect = [
            SimpleNamespace(return_code=0, stdout="", stderr=""),
            SimpleNamespace(return_code=9, stdout="", stderr="private output"),
        ]
        with patch("haskell_agent.time.monotonic", side_effect=[100, 103, 104, 111]):
            with self.assertRaisesRegex(RuntimeError, "exit 9"):
                await agent.run("task", self.environment, AgentContext())
        timing_path = agent.logs_dir / "execution-timing.json"
        timing = json.loads(timing_path.read_text())
        self.assertEqual(timing["execution_seconds"], 3)
        self.assertEqual(timing["transcript_export_seconds"], 7)
        self.assertNotIn("private output", timing_path.read_text())
        agent._preserve_transcripts.assert_awaited_once()

    async def test_setup_failure_does_not_continue(self):
        self.environment.exec.return_value.return_code = 1
        agent = HaskellAgent(**self.arguments)
        with self.assertRaises(RuntimeError):
            await agent.setup(self.environment)
        self.environment.upload_file.assert_not_awaited()

    async def test_pages_are_not_truncated_at_500(self):
        agent = HaskellAgent(**self.arguments)
        agent._session_json = AsyncMock(side_effect=[
            [{"id": "parent"}],
            {"turns": [{"index": i} for i in range(2, 502)],
             "page": {"hasOlder": True, "generationStart": 0}},
            {"turns": [{"index": 0}, {"index": 1}],
             "page": {"hasOlder": False, "generationStart": 0}},
        ])
        self.assertEqual(await agent._export_session_pages(self.environment, self.root), 1)
        self.assertEqual(agent._session_json.call_args.args[1],
                         ["show", "parent", "--limit", "500", "--before", "2"])
        pages = sorted((self.root / "pages/parent").glob("*.json"))
        self.assertEqual(sum(len(json.loads(p.read_text())["turns"]) for p in pages), 502)

    async def test_repeated_cursor_fails_with_partial_pages_retained(self):
        agent = HaskellAgent(**self.arguments)
        page = {"turns": [{"index": 2}], "page": {"hasOlder": True}}
        agent._session_json = AsyncMock(side_effect=[[{"id": "parent"}], page, page])
        with self.assertRaisesRegex(ValueError, "advance"):
            await agent._export_session_pages(self.environment, self.root)
        self.assertEqual(len(list((self.root / "pages/parent").glob("*.json"))), 2)

    async def test_disk_export_preserves_child_paths_and_only_allowlisted_names(self):
        agent = HaskellAgent(**self.arguments)
        root, _ = agent.transcript_files()
        sources = [root + "/parent/meta.json", root + "/parent/agents/child/transcript.json"]
        self.environment.exec.return_value.stdout = "\0".join(sources) + "\0"
        count = await agent._export_disk_transcripts(self.environment, self.root)
        self.assertEqual(count, 2)
        self.environment.download_file.assert_any_await(
            sources[1], self.root / "files/parent/agents/child/transcript.json"
        )
        command = self.environment.exec.call_args.kwargs["command"]
        self.assertIn("find -P", command)
        self.assertIn("-type f", command)
        self.assertNotIn("auth", command)
        self.assertNotIn("postgres", command)

    async def test_codex_collects_jsonl_only_and_never_calls_native_cli(self):
        from stock_codex import StockCodex
        agent = StockCodex(**self.arguments)
        self.environment.exec.return_value.stdout = ""
        context = AgentContext()
        await agent._preserve_transcripts(self.environment, context)
        command = self.environment.exec.call_args.kwargs["command"]
        self.assertIn("/.codex/sessions", command)
        self.assertIn("-name '*.jsonl'", command)
        self.assertNotIn("auth", command)
        self.assertEqual(context.metadata["transcript_export"]["status"], "exported")

    async def test_timeout_waits_for_export_then_propagates(self):
        agent = HaskellAgent(**self.arguments)
        running = asyncio.Event()
        exported = asyncio.Event()

        async def execute(**kwargs):
            if kwargs["command"] == agent.execution_command():
                running.set()
                await asyncio.Future()
            return SimpleNamespace(return_code=0, stdout="", stderr="")

        async def export(*args):
            exported.set()

        self.environment.exec.side_effect = execute
        agent._export_transcripts = AsyncMock(side_effect=export)
        task = asyncio.create_task(agent.run("task", self.environment, AgentContext()))
        await running.wait()
        task.cancel()
        with self.assertRaises(asyncio.CancelledError):
            await task
        self.assertTrue(exported.is_set())
        timing = json.loads((agent.logs_dir / "execution-timing.json").read_text())
        self.assertGreaterEqual(timing["execution_seconds"], 0)
        self.assertGreaterEqual(timing["transcript_export_seconds"], 0)
        self.assertLessEqual(
            datetime.fromisoformat(timing["execution_started_at"]),
            datetime.fromisoformat(timing["execution_finished_at"]),
        )

    async def test_harbor_wait_for_timeout_exports_before_returning(self):
        agent = HaskellAgent(**self.arguments)
        agent._export_transcripts = AsyncMock()

        async def execute(**kwargs):
            if kwargs["command"] == agent.execution_command():
                await asyncio.Future()
            return SimpleNamespace(return_code=0, stdout="", stderr="")

        self.environment.exec.side_effect = execute
        context = AgentContext()
        with self.assertRaises(TimeoutError):
            await asyncio.wait_for(
                agent.run("task", self.environment, context), timeout=0.01
            )
        agent._export_transcripts.assert_awaited_once()
        self.assertEqual(context.metadata["transcript_export"]["status"], "exported")

    async def test_exec_exception_exports_without_masking_original(self):
        agent = HaskellAgent(**self.arguments)
        agent._export_transcripts = AsyncMock(side_effect=ValueError("private detail"))
        self.environment.exec.side_effect = [
            SimpleNamespace(return_code=0, stdout="", stderr=""),
            RuntimeError("execution failed"),
        ]
        with self.assertRaisesRegex(RuntimeError, "execution failed"):
            await agent.run("task", self.environment, AgentContext())
        agent._export_transcripts.assert_awaited_once()

    async def test_repeated_cancellation_does_not_abandon_export(self):
        agent = HaskellAgent(**self.arguments)
        started, release = asyncio.Event(), asyncio.Event()

        async def export(*args):
            started.set()
            await release.wait()

        agent._export_transcripts = AsyncMock(side_effect=export)
        context = AgentContext()
        task = asyncio.create_task(agent._preserve_transcripts(self.environment, context))
        await started.wait()
        task.cancel()
        await asyncio.sleep(0)
        task.cancel()
        release.set()
        with self.assertRaises(asyncio.CancelledError):
            await task
        self.assertEqual(context.metadata["transcript_export"]["status"], "exported")

    async def test_export_has_bounded_timeout_and_safe_error(self):
        agent = HaskellAgent(**self.arguments)
        agent.TRANSCRIPT_TIMEOUT_SEC = 0.01

        async def hang(*args):
            await asyncio.Future()

        agent._export_transcripts = AsyncMock(side_effect=hang)
        context = AgentContext()
        await agent._preserve_transcripts(self.environment, context)
        self.assertEqual(context.metadata["transcript_export"]["error_type"], "TimeoutError")
        agent._export_transcripts.side_effect = RuntimeError("credential-do-not-log")
        await agent._preserve_transcripts(self.environment, context)
        self.assertNotIn("credential-do-not-log", (agent.logs_dir / "transcripts/manifest.json").read_text())

    def test_sql_export_is_fixed_transcript_allowlist(self):
        sql = HaskellAgent.transcript_sql()
        tables = set(re.findall(r"FROM harness\.([a-z_]+)", sql))
        self.assertEqual(tables, {
            "sessions", "session_events", "session_turns", "session_response_items",
            "session_messages", "session_function_calls", "session_function_call_outputs",
            "session_custom_tool_calls", "session_custom_tool_call_outputs",
            "session_reasoning_items", "session_reasoning_summaries",
            "session_item_references", "session_tagged_items", "session_response_content_parts",
        })
        self.assertIn("REPEATABLE READ READ ONLY", sql)
        self.assertNotIn("LIMIT", sql)
        self.assertNotIn("WHERE", sql)  # No current-generation filter.

    async def test_sql_export_uses_private_socket_without_auth_files(self):
        agent = HaskellAgent(**self.arguments, psql_executable="/nix/store/pg/bin/psql")
        await agent._export_persisted_history(self.environment, self.root)
        command = shlex.split(self.environment.exec.call_args.kwargs["command"])
        self.assertIn("HOME=/nonexistent", command)
        self.assertIn("PGPASSFILE=/dev/null", command)
        self.assertIn("-X", command)
        self.assertIn("-w", command)
        self.assertEqual(command[command.index("-h") + 1],
                         agent.home_directory + "/.haskell-agent/postgres/run")
        self.environment.download_file.assert_awaited_once()

    async def test_one_export_source_failure_does_not_skip_other_sources(self):
        agent = HaskellAgent(**self.arguments)
        agent._export_disk_transcripts = AsyncMock(side_effect=RuntimeError("secret"))
        agent._export_persisted_history = AsyncMock(return_value="exported")
        agent._export_session_pages = AsyncMock(return_value=1)
        context = AgentContext()
        await agent._preserve_transcripts(self.environment, context)
        agent._export_persisted_history.assert_awaited_once()
        agent._export_session_pages.assert_awaited_once()
        self.assertEqual(context.metadata["transcript_export"]["status"], "incomplete")


if __name__ == "__main__":
    unittest.main()
