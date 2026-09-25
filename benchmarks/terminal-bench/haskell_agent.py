"""Harbor 0.23 adapter running the native agent in the task environment.

The closure archive is trusted build output: a tar archive whose members are
relative ``nix/store/...`` paths. Credential files are uploaded separately and
not intentionally exported. Transcripts must be reviewed for sensitive content.
"""

from pathlib import Path, PurePosixPath
import asyncio
import fnmatch
import json
import shlex
import tempfile
import re
import time
from datetime import datetime, timezone

from harbor.agents.base import BaseAgent
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext


class HaskellAgent(BaseAgent):
    TRANSCRIPT_TIMEOUT_SEC = 60

    def __init__(
        self,
        *args,
        closure_archive: str,
        executable_path: str,
        codex_auth_path: str,
        support_archive: str,
        sudo_executable: str,
        ca_bundle_path: str,
        run_user: str = "benchmark",
        effort: str = "medium",
        psql_executable: str | None = None,
        **kwargs,
    ):
        super().__init__(*args, **kwargs)
        if not self.model_name:
            raise ValueError("An explicit model is required")
        self.closure_archive = Path(closure_archive).expanduser().resolve(strict=True)
        self.codex_auth_path = Path(codex_auth_path).expanduser().resolve(strict=True)
        self.support_archive = Path(support_archive).expanduser().resolve(strict=True)
        self.ca_bundle_source = Path(ca_bundle_path).expanduser().resolve(strict=True)
        sudo = PurePosixPath(sudo_executable)
        if not str(sudo).startswith("/nix/store/") or ".." in sudo.parts:
            raise ValueError("sudo_executable must be inside the Nix closure")
        self.sudo_executable = str(sudo)
        executable = PurePosixPath(executable_path)
        if not executable.is_absolute() or ".." in executable.parts:
            raise ValueError("executable_path must be an absolute container path")
        if not str(executable).startswith("/nix/store/"):
            raise ValueError("executable_path must be inside the uploaded Nix closure")
        if effort not in {"none", "low", "medium", "high", "xhigh", "max"}:
            raise ValueError("Unsupported reasoning effort")
        if not re.fullmatch(r"[a-z_][a-z0-9_-]*", run_user) or run_user == "root":
            raise ValueError("run_user must be a nonroot POSIX account name")
        self.executable_path = str(executable)
        self.run_user = run_user
        self.effort = effort
        if psql_executable is not None:
            psql = PurePosixPath(psql_executable)
            if not str(psql).startswith("/nix/store/") or ".." in psql.parts:
                raise ValueError("psql_executable must be inside the Nix closure")
        self.psql_executable = psql_executable
        self.install_directory = "/opt/haskell-agent-benchmark"
        self.ca_bundle_path = self.install_directory + "/ca-certificates.crt"
        self.home_directory = self.install_directory + "/home"
        self.prompt_path = self.install_directory + "/instruction.txt"
        self._version = None

    @staticmethod
    def name() -> str:
        return "haskell-agent"

    def version(self) -> str | None:
        return self._version

    async def _checked_exec(self, environment, command, *, phase, **kwargs):
        result = await environment.exec(command=command, **kwargs)
        if result.return_code != 0:
            # Do not interpolate output: commands may emit credential material.
            raise RuntimeError(f"Haskell agent {phase} failed (exit {result.return_code})")
        return result

    async def setup(self, environment: BaseEnvironment) -> None:
        if self.mcp_servers or self.skills_dir:
            raise ValueError("The initial benchmark adapter does not support MCP or skills")
        directory = shlex.quote(self.install_directory)
        home = shlex.quote(self.home_directory)
        await self._checked_exec(
            environment,
            f"test ! -e {directory} && mkdir -p {directory} && chmod 700 {directory}",
            phase="directory setup", user="root",
        )
        archive = self.install_directory + "/closure.tar"
        await environment.upload_file(self.closure_archive, archive)
        await self._checked_exec(
            environment,
            f"tar -xf {shlex.quote(archive)} -C / && rm {shlex.quote(archive)}"
            f" && mkdir -p {home}/.codex {home}/.config {home}/.cache {home}/.local/share",
            phase="closure installation", user="root",
        )
        account = shlex.quote(self.run_user)
        support = self.install_directory + "/support.tar"
        await environment.upload_file(self.support_archive, support)
        await environment.upload_file(self.ca_bundle_source, self.ca_bundle_path)
        await self._checked_exec(
            environment,
            f"tar -xf {shlex.quote(support)} -C / && rm {shlex.quote(support)}"
            " && mkdir -p /usr/local/bin"
            f" && install -m 4755 -o root -g root {shlex.quote(self.sudo_executable)} /usr/local/bin/sudo",
            phase="privilege support installation", user="root",
        )
        # Retain task permissions; provision only the agent's own metadata directory.
        await self._checked_exec(
            environment,
            f"if ! id {account} >/dev/null 2>&1; then"
            f" useradd --no-create-home --home-dir {home} --shell /bin/bash {account}; fi"
            f" && test \"$(id -u {account})\" != 0"
            f" && printf '\\n%s\\n%s\\n' {shlex.quote(self.run_user + ' ALL=(ALL:ALL) NOPASSWD: ALL')}"
            f" {shlex.quote('Defaults:' + self.run_user + ' !pam_session, !pam_acct_mgmt')}"
            " >> /etc/sudoers && chmod 440 /etc/sudoers",
            phase="nonroot account and sudo setup", user="root",
        )
        await environment.upload_file(
            self.codex_auth_path, self.home_directory + "/.codex/auth.json"
        )
        await self._checked_exec(
            environment,
            f"chmod 600 {home}/.codex/auth.json"
            f" && chown -R {shlex.quote(self.run_user)} {directory}",
            phase="credential permissions", user="root",
        )
        result = await self._checked_exec(
            environment, shlex.join([self.executable_path, "--version"]),
            phase="version check", user=self.run_user,
        )
        self._version = (result.stdout or "").strip()
        await self._checked_exec(
            environment,
            "mkdir -p .haskell-agent"
            f" && chown {account} .haskell-agent",
            phase="workspace metadata setup", user="root",
        )
        await self._checked_exec(
            environment,
            "sudo -n true && test -x ."
            f" && test -r {shlex.quote(self.ca_bundle_path)}",
            phase="nonroot access check", user=self.run_user,
        )

    def transcript_files(self):
        # Explicit filenames only; never archive the home or database directory.
        return self.home_directory + "/.haskell-agent/sessions", (
            "meta.json", "transcript.json", "transcript.jsonl",
        )

    async def _export_disk_transcripts(self, environment, destination):
        root, names = self.transcript_files()
        predicates = " -o ".join(f"-name {shlex.quote(n)}" for n in names)
        result = await self._checked_exec(
            environment,
            f"if test -d {shlex.quote(root)} && test ! -L {shlex.quote(root)}; then "
            f"find -P {shlex.quote(root)} -type f \\( {predicates} \\) -print0; fi",
            phase="transcript discovery", user=self.run_user, timeout_sec=10,
        )
        count = 0
        for source in (result.stdout or "").split("\0"):
            if not source:
                continue
            relative = PurePosixPath(source).relative_to(PurePosixPath(root))
            if (".." in relative.parts or not relative.parts
                    or not any(fnmatch.fnmatchcase(relative.name, n) for n in names)):
                raise ValueError("Invalid transcript path")
            target = destination / "files" / Path(*relative.parts)
            target.parent.mkdir(parents=True, exist_ok=True)
            await environment.download_file(source, target)
            count += 1
        return count

    async def _session_json(self, environment, arguments):
        command = shlex.join([
            "env", "-i", f"HOME={self.home_directory}",
            f"XDG_CONFIG_HOME={self.home_directory}/.config",
            f"XDG_DATA_HOME={self.home_directory}/.local/share",
            "PATH=/usr/local/bin:/usr/bin:/bin", "LANG=C.UTF-8",
            self.executable_path, "sessions", *arguments, "--json",
        ])
        result = await self._checked_exec(
            environment, command, phase="session export",
            user=self.run_user, timeout_sec=10,
        )
        return json.loads(result.stdout)

    async def _export_session_pages(self, environment, destination):
        sessions = await self._session_json(environment, ["list"])
        (destination / "sessions.json").write_text(json.dumps(sessions), encoding="utf-8")
        if not isinstance(sessions, list):
            raise ValueError("Unexpected session list")
        for session in sessions:
            session_id = session["id"]
            if not isinstance(session_id, str) or not re.fullmatch(r"[A-Za-z0-9_-]+", session_id):
                raise ValueError("Invalid session id")
            directory = destination / "pages" / session_id
            directory.mkdir(parents=True, exist_ok=True)
            before = None
            page_number = 0
            while True:
                arguments = ["show", session_id, "--limit", "500"]
                if before is not None:
                    arguments += ["--before", str(before)]
                page = await self._session_json(environment, arguments)
                (directory / f"{page_number:06d}.json").write_text(
                    json.dumps(page), encoding="utf-8"
                )
                if not page["page"]["hasOlder"]:
                    break
                cursor = min(turn["index"] for turn in page["turns"])
                if type(cursor) is not int or cursor < 0 or (
                    before is not None and cursor >= before
                ):
                    raise ValueError("Session cursor did not advance")
                before = cursor
                page_number += 1
        return len(sessions)

    @staticmethod
    def transcript_sql():
        # Fixed allowlist reviewed against f87b1e8 Session/Schema.hs and
        # SessionItem.hs. No catalog iteration, credential tables or DB dump.
        tables = (
            "session_events", "session_turns", "session_response_items",
            "session_messages", "session_function_calls",
            "session_function_call_outputs", "session_custom_tool_calls",
            "session_custom_tool_call_outputs", "session_reasoning_items",
            "session_reasoning_summaries", "session_item_references",
            "session_tagged_items", "session_response_content_parts",
        )
        return "\n".join([
            "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;",
            "SET LOCAL statement_timeout = '20s';",
            "SELECT json_build_object('table', 'sessions', 'row', row_to_json(s)) "
            "FROM (SELECT session_id, session_key, provider, model_id, cwd, effort, "
            "title, created_at, updated_at, next_turn_index FROM harness.sessions) s;",
            *[
                f"SELECT json_build_object('table', '{table}', 'row', row_to_json(t)) "
                f"FROM harness.{table} t;"
                for table in tables
            ],
            "COMMIT;",
        ])

    async def _export_persisted_history(self, environment, destination):
        if self.psql_executable is None:
            return "unavailable: psql_executable not configured"
        # A fresh per-trial HOME owns this private socket (trust authentication).
        # env -i, -X and -w prevent pgpass/psqlrc or password prompt inheritance.
        remote = str(self.environment_logs_dir / "persisted-session-history.jsonl")
        command = shlex.join([
            "env", "-i", "HOME=/nonexistent", "PGPASSFILE=/dev/null",
            "PGCONNECT_TIMEOUT=5", self.psql_executable,
            "-X", "-w", "-q", "-A", "-t", "-v", "ON_ERROR_STOP=1",
            "-h", self.home_directory + "/.haskell-agent/postgres/run",
            "-p", "55432", "-U", "ha_owner", "-d", "haskell_agent",
            "-c", self.transcript_sql(),
        ]) + f" >{shlex.quote(remote)} 2>/dev/null"
        await self._checked_exec(
            environment, command, phase="persisted transcript export",
            user=self.run_user, timeout_sec=25,
        )
        await environment.download_file(remote, destination / "persisted-session-history.jsonl")
        return "exported: all persisted turns, response items and compaction generations"

    async def _export_transcripts(self, environment, destination, manifest):
        manifest["scope"] = (
            "Allowlisted on-disk parent/child transcripts, current-generation CLI "
            "pages, and (when psql_executable is configured) a read-only snapshot "
            "of persisted parent turns/response items across compaction generations. "
            "Without SQL export, CLI pages omit provider items and older generations. "
            "Uncommitted/in-flight output cannot be recovered."
        )
        # Each source is independent; keep raw child/history data even if CLI
        # pagination fails. A single outer deadline bounds the entire collection.
        for key, export in (
            ("files", self._export_disk_transcripts),
            ("persisted_history", self._export_persisted_history),
            ("sessions", self._export_session_pages),
        ):
            try:
                manifest[key] = await export(environment, destination)
            except Exception as exc:
                manifest.setdefault("errors", {})[key] = type(exc).__name__

    async def _preserve_transcripts(self, environment, context):
        """Harbor 0.23 has no agent teardown hook; wait_for awaits run's finally."""
        destination = self.logs_dir / "transcripts"
        destination.mkdir(parents=True, exist_ok=True)
        manifest = {"status": "incomplete", "timeout_sec": self.TRANSCRIPT_TIMEOUT_SEC}
        manifest_path = destination / "manifest.json"
        manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

        async def collect():
            try:
                await asyncio.wait_for(
                    self._export_transcripts(environment, destination, manifest),
                    timeout=self.TRANSCRIPT_TIMEOUT_SEC,
                )
                manifest["status"] = "incomplete" if manifest.get("errors") else "exported"
            except Exception as exc:
                # Exception messages and exec output may contain secrets.
                manifest["error_type"] = type(exc).__name__
            finally:
                manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
                context.metadata = {
                    **(context.metadata or {}), "transcript_export": manifest,
                }

        task = asyncio.create_task(collect())
        cancellation = None
        while not task.done():
            try:
                await asyncio.shield(task)
            except asyncio.CancelledError as exc:
                cancellation = exc
        task.result()
        if cancellation is not None:
            raise cancellation

    def execution_command(self) -> str:
        arguments = [
            self.executable_path, "--provider", "openai",
            "--model", self.model_name, "--effort", self.effort,
            "--prompt-file", self.prompt_path,
            "--no-agents-md", "--no-skills", "--no-computer-use",
            "--minimal", "--motion", "off", "--yolo", "--max-turns", "100",
            "--save-session",
        ]
        # No host environment, personal configuration, or integrations are inherited.
        assignments = [
            f"HOME={self.home_directory}",
            f"CODEX_HOME={self.home_directory}/.codex",
            f"XDG_CONFIG_HOME={self.home_directory}/.config",
            f"XDG_CACHE_HOME={self.home_directory}/.cache",
            f"XDG_DATA_HOME={self.home_directory}/.local/share",
            "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
            "LANG=C.UTF-8",
            f"SSL_CERT_FILE={self.ca_bundle_path}",
            f"NIX_SSL_CERT_FILE={self.ca_bundle_path}",
        ]
        stdout_path = str(self.environment_logs_dir / "stdout.txt")
        stderr_path = str(self.environment_logs_dir / "stderr.txt")
        return (
            shlex.join(["env", "-i", *assignments, *arguments])
            + f" >{shlex.quote(stdout_path)} 2>{shlex.quote(stderr_path)}"
        )

    async def run(
        self, instruction: str, environment: BaseEnvironment, context: AgentContext
    ) -> None:
        instruction = (
            "Environment: you run as a nonroot user. Passwordless sudo is available "
            "for operations requiring root. The working directory and task file "
            "permissions have not been changed.\n\n" + instruction
        )
        with tempfile.TemporaryDirectory(prefix="haskell-agent-instruction-") as directory:
            prompt = Path(directory) / "instruction.txt"
            prompt.write_text(instruction, encoding="utf-8")
            await environment.upload_file(prompt, self.prompt_path)
        await self._checked_exec(
            environment,
            f"chmod 644 {shlex.quote(self.prompt_path)}"
            f" && mkdir -p {shlex.quote(str(self.environment_logs_dir))}"
            f" && chown {shlex.quote(self.run_user)}"
            f" {shlex.quote(str(self.environment_logs_dir))}",
            phase="run preparation", user="root",
        )
        # Harbor owns the task timeout. Redirection preserves partial logs on timeout.
        context.metadata = {
            "agent_version": self._version,
            "reasoning_effort": self.effort,
            "usage_accounting": "unavailable",
        }
        execution_started = time.monotonic()
        timing = {"execution_started_at": datetime.now(timezone.utc).isoformat()}
        try:
            result = await environment.exec(
                command=self.execution_command(), user=self.run_user
            )
            context.metadata["exit_code"] = result.return_code
        finally:
            timing["execution_finished_at"] = datetime.now(timezone.utc).isoformat()
            timing["execution_seconds"] = time.monotonic() - execution_started
            export_started = time.monotonic()
            try:
                await self._preserve_transcripts(environment, context)
            finally:
                timing["transcript_export_seconds"] = time.monotonic() - export_started
                (self.logs_dir / "execution-timing.json").write_text(
                    json.dumps(timing, indent=2), encoding="utf-8"
                )
        if result.return_code != 0:
            raise RuntimeError(
                f"Haskell agent execution failed (exit {result.return_code}); see agent logs"
            )
