"""Unmodified Nix-packaged Codex with matched container provisioning."""

import shlex

from haskell_agent import HaskellAgent


class StockCodex(HaskellAgent):
    def transcript_files(self):
        return self.home_directory + "/.codex/sessions", ("*.jsonl",)

    async def _export_transcripts(self, environment, destination, manifest):
        manifest["files"] = await self._export_disk_transcripts(environment, destination)
        manifest["scope"] = (
            "Persisted Codex session JSONL only, in addition to stdout JSON. "
            "Uncommitted/in-flight output cannot be recovered."
        )

    @staticmethod
    def name() -> str:
        return "stock-codex"

    def execution_command(self) -> str:
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
        arguments = [
            self.executable_path, "exec",
            "--dangerously-bypass-approvals-and-sandbox",
            "--skip-git-repo-check", "--model", self.model_name,
            "-c", f'model_reasoning_effort="{self.effort}"', "--json", "-",
        ]
        return (
            shlex.join(["env", "-i", *assignments, *arguments])
            + f" <{shlex.quote(self.prompt_path)}"
            + f" >{shlex.quote(str(self.environment_logs_dir / 'stdout.txt'))}"
            + f" 2>{shlex.quote(str(self.environment_logs_dir / 'stderr.txt'))}"
        )
