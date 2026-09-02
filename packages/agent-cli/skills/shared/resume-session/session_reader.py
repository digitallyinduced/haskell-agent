#!/usr/bin/env python3
"""Read coding-agent sessions as explicitly untrusted, inert history.

This utility intentionally uses only Python's standard library. It recovers
human/assistant text and bounded tool summaries, while dropping system
instructions, hidden reasoning, signatures, and encrypted content.
"""

from __future__ import annotations

import argparse
import glob
import hashlib
import json
import math
import os
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from contextlib import closing
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Iterable
from urllib.parse import unquote

TOOLS = ("claude", "codex", "cursor", "grok")
UUID_RE = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
    re.IGNORECASE,
)
GENERATED_WRAPPER_RE = re.compile(
    r"^\s*<(?:system-reminder|environment_context|system|developer|"
    r"user_instructions|manually_attached_skills|timestamp|local-command-caveat|"
    r"harness_instructions|prior_conversation|current_request|user_query)"
    r"(?:\s|>)",
    re.IGNORECASE,
)
OUTER_HARNESS_RE = re.compile(
    r"^\s*(?:Instructions supplied by the outer agent harness:|"
    r"Prior conversation imported from the outer agent harness\.|"
    r"Current request:)",
    re.IGNORECASE,
)
MAX_TEXT_CHARS = 20_000
MAX_TURNS = 200
CURSOR_CONVERSATION_KEYS = {"messages", "turns", "conversation", "bubbles"}
CURSOR_CWD_KEYS = {
    "cwd",
    "fspath",
    "folderpath",
    "rootpath",
    "sourcereporootpath",
    "workspacepath",
}
CLAUDE_CHAIN_TYPES = {"user", "assistant", "system", "attachment"}


class ReaderError(RuntimeError):
    pass


class AmbiguousReference(ReaderError):
    def __init__(self, reference: str, candidates: list[dict[str, Any]]):
        super().__init__(f"session reference is ambiguous: {reference}")
        self.reference = reference
        self.candidates = candidates


def warning(code: str, message: str) -> dict[str, str]:
    return {"code": code, "message": message}


def canonical(path: str | Path) -> str:
    return os.path.normcase(os.path.realpath(os.path.expanduser(str(path))))


def same_cwd(left: str | None, right: str) -> bool:
    return bool(left) and canonical(left) == canonical(right)


def path_is_within(path: str | Path, root: str | Path) -> bool:
    try:
        Path(path).resolve().relative_to(Path(root).resolve())
        return True
    except (OSError, ValueError):
        return False


def safe_string(value: Any) -> str:
    if isinstance(value, str):
        return value
    if value is None:
        return ""
    return str(value)


def clipped(text: str, limit: int = MAX_TEXT_CHARS) -> str:
    text = text.replace("\x00", "\N{REPLACEMENT CHARACTER}")
    return text if len(text) <= limit else text[:limit] + "\n[truncated]"


def one_line(value: Any, limit: int = 100) -> str:
    text = " ".join(safe_string(value).split())
    return text if len(text) <= limit else text[: max(0, limit - 1)] + "…"


def json_preview(value: Any, limit: int) -> str:
    try:
        text = json.dumps(value, ensure_ascii=False, sort_keys=True)
    except RecursionError:
        text = "[nested value omitted]"
    except (TypeError, ValueError):
        text = safe_string(value)
    return one_line(text, limit)


def numeric_timestamp_seconds(value: Any) -> float | None:
    if not isinstance(value, (int, float)):
        return None
    try:
        seconds = float(value)
    except (OverflowError, ValueError):
        return None
    if not math.isfinite(seconds):
        return None
    if seconds > 10_000_000_000:
        seconds /= 1000
    try:
        datetime.fromtimestamp(seconds, timezone.utc)
    except (OSError, OverflowError, ValueError):
        return None
    return seconds


def iso_time(value: Any, fallback_path: str | Path | None = None) -> str | None:
    if isinstance(value, str) and value:
        return value
    if (seconds := numeric_timestamp_seconds(value)) is not None:
        return datetime.fromtimestamp(seconds, timezone.utc).isoformat()
    if fallback_path:
        try:
            return datetime.fromtimestamp(
                os.path.getmtime(fallback_path), timezone.utc
            ).isoformat()
        except (OSError, OverflowError, ValueError):
            pass
    return None


def timestamp_value(value: Any, fallback_path: str | Path | None = None) -> float:
    if (seconds := numeric_timestamp_seconds(value)) is not None:
        return seconds
    if isinstance(value, str):
        try:
            return datetime.fromisoformat(value.replace("Z", "+00:00")).timestamp()
        except (OSError, OverflowError, ValueError):
            pass
    try:
        fallback = os.path.getmtime(fallback_path) if fallback_path else 0
        return fallback if math.isfinite(fallback) else 0
    except OSError:
        return 0


def read_json(path: str | Path) -> Any:
    with open(path, encoding="utf-8", errors="replace") as handle:
        return json.load(handle)


def parse_jsonl_lines(
    lines: Iterable[str], max_records: int | None
) -> tuple[list[dict[str, Any]], int, bool]:
    records: list[dict[str, Any]] = []
    malformed = 0
    if max_records is not None and max_records <= 0:
        return records, malformed, True
    for line in lines:
        if not line.strip():
            continue
        try:
            value = json.loads(line)
        except (json.JSONDecodeError, RecursionError, ValueError):
            malformed += 1
            continue
        if isinstance(value, dict):
            records.append(value)
            if max_records is not None and len(records) >= max_records:
                return records, malformed, True
        else:
            malformed += 1
    return records, malformed, False


def consume_jsonl(
    path: str | Path, consume: Callable[[dict[str, Any]], None]
) -> int:
    path = Path(path)

    def consume_lines(lines: Iterable[str]) -> int:
        malformed = 0
        for line in lines:
            if not line.strip():
                continue
            try:
                value = json.loads(line)
            except (json.JSONDecodeError, RecursionError, ValueError):
                malformed += 1
                continue
            if isinstance(value, dict):
                consume(value)
            else:
                malformed += 1
        return malformed

    if not path.name.endswith(".jsonl.zst"):
        with open(path, encoding="utf-8", errors="replace") as handle:
            return consume_lines(handle)

    executable = shutil.which("zstd")
    if executable is None:
        raise ReaderError(f"zstd is required to read compressed Codex rollout {path}")
    with tempfile.TemporaryFile() as errors:
        try:
            process = subprocess.Popen(
                [executable, "-dc", str(path)],
                stdout=subprocess.PIPE,
                stderr=errors,
                text=True,
                encoding="utf-8",
                errors="replace",
            )
        except OSError as exc:
            raise ReaderError(f"failed to run zstd for {path}: {exc}") from exc
        if process.stdout is None:
            process.kill()
            process.wait()
            raise ReaderError(f"failed to capture zstd output for {path}")
        try:
            malformed = consume_lines(process.stdout)
            process.stdout.close()
            process.wait()
            errors.seek(0)
            detail = one_line(
                errors.read(1000).decode("utf-8", errors="replace"), 300
            )
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            process.stdout.close()
        if process.returncode != 0:
            raise ReaderError(
                f"zstd failed to decompress {path}: {detail or 'unknown error'}"
            )
        return malformed


def read_jsonl(
    path: str | Path, max_records: int | None = None
) -> tuple[list[dict[str, Any]], int]:
    path = Path(path)
    if max_records is None:
        records: list[dict[str, Any]] = []
        malformed = consume_jsonl(path, records.append)
        return records, malformed
    if path.name.endswith(".jsonl.zst"):
        executable = shutil.which("zstd")
        if executable is None:
            raise ReaderError(
                f"zstd is required to read compressed Codex rollout {path}"
            )
        try:
            process = subprocess.Popen(
                [executable, "-dc", str(path)],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
                encoding="utf-8",
                errors="replace",
            )
        except OSError as exc:
            raise ReaderError(f"failed to run zstd for {path}: {exc}") from exc
        if process.stdout is None:
            process.kill()
            process.wait()
            raise ReaderError(f"failed to capture zstd output for {path}")
        try:
            records, malformed, stopped_early = parse_jsonl_lines(
                process.stdout, max_records
            )
            if stopped_early:
                # Close the stream before terminating so the decompressor cannot
                # remain blocked while writing output that we no longer need.
                process.stdout.close()
                if process.poll() is None:
                    process.terminate()
                    try:
                        process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait()
            else:
                process.wait()
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            process.stdout.close()
        if not stopped_early and process.returncode != 0:
            raise ReaderError(f"zstd failed to decompress {path}: unknown error")
        return records, malformed
    else:
        try:
            with open(path, encoding="utf-8", errors="replace") as handle:
                records, malformed, _ = parse_jsonl_lines(handle, max_records)
            return records, malformed
        except OSError:
            raise


def open_sqlite_readonly(path: str | Path) -> sqlite3.Connection:
    resolved = Path(path).expanduser().resolve()
    try:
        database = sqlite3.connect(f"{resolved.as_uri()}?mode=ro", uri=True)
        database.execute("PRAGMA query_only = ON")
        return database
    except (OSError, sqlite3.Error) as exc:
        raise ReaderError(f"failed to open SQLite store {resolved}: {exc}") from exc


def table_columns(database: sqlite3.Connection, table: str) -> set[str]:
    try:
        return {
            safe_string(row[1])
            for row in database.execute(f'PRAGMA table_info("{table}")')
        }
    except sqlite3.Error:
        return set()


def decode_jsonish(raw: Any) -> Any:
    if isinstance(raw, memoryview):
        raw = raw.tobytes()
    if isinstance(raw, bytes):
        try:
            raw = raw.decode("utf-8")
        except UnicodeDecodeError:
            return None
    if not isinstance(raw, str):
        return raw if isinstance(raw, (dict, list)) else None
    text = raw.strip()
    if text and len(text) % 2 == 0 and all(
        character in "0123456789abcdefABCDEF" for character in text
    ):
        try:
            return json.loads(bytes.fromhex(text).decode("utf-8"))
        except (
            ValueError,
            UnicodeDecodeError,
            json.JSONDecodeError,
            RecursionError,
        ):
            pass
    try:
        return json.loads(text)
    except (ValueError, json.JSONDecodeError, RecursionError):
        return None


def content_text(content: Any) -> str:
    if isinstance(content, str):
        return clipped(content)
    if not isinstance(content, list):
        return ""
    parts: list[str] = []
    for block in content:
        if not isinstance(block, dict):
            continue
        block_type = safe_string(block.get("type")).lower()
        if block_type in {
            "thinking",
            "reasoning",
            "redacted_thinking",
            "encrypted_content",
            "signature",
            "input_image",
            "image",
        }:
            continue
        if block_type in {"text", "input_text", "output_text"}:
            text = block.get("text", block.get("content"))
            if isinstance(text, str):
                parts.append(text)
    return clipped("\n".join(parts))


def tagged_user_request(text: str) -> str | None:
    saw_tag = False
    for tag in ("user_query", "current_request"):
        queries = re.findall(
            rf"<{tag}>\s*(.*?)\s*</{tag}>",
            text,
            flags=re.IGNORECASE | re.DOTALL,
        )
        if queries:
            saw_tag = True
            rendered = "\n".join(
                query for query in queries if query.strip()
            ).strip()
            if rendered:
                return clipped(rendered)
    return "" if saw_tag else None


def prior_conversation_user_text(text: str) -> str:
    conversations = re.findall(
        r"<prior_conversation>\s*(.*?)\s*</prior_conversation>",
        text,
        flags=re.IGNORECASE | re.DOTALL,
    )
    labels = (
        r"(?:User|Assistant|System|Developer|Assistant tool call|"
        r"Assistant computer call|Tool result[^\n]*|Computer result|Context item)"
    )
    for conversation in reversed(conversations):
        entries = re.findall(
            rf"(?:\A|\n{{2,}})User:\n(.*?)(?=\n{{2,}}{labels}:\n|\Z)",
            conversation.strip(),
            flags=re.IGNORECASE | re.DOTALL,
        )
        for entry in reversed(entries):
            nested = tagged_user_request(entry)
            candidate = entry.strip() if nested is None else nested
            if (
                candidate
                and not GENERATED_WRAPPER_RE.match(candidate)
                and not OUTER_HARNESS_RE.match(candidate)
            ):
                return clipped(candidate)
    return ""


def user_text(text: str) -> str:
    tagged = tagged_user_request(text)
    if tagged:
        return tagged
    if prior := prior_conversation_user_text(text):
        return prior
    if tagged is not None:
        return ""
    if GENERATED_WRAPPER_RE.match(text) or OUTER_HARNESS_RE.match(text):
        return ""
    return clipped(text)


def inert_turn(
    role: str,
    text: str = "",
    tool_calls: list[dict[str, str]] | None = None,
    tool_results: list[dict[str, str]] | None = None,
) -> dict[str, Any] | None:
    text = clipped(text.strip())
    if role not in {"user", "assistant"}:
        return None
    if role == "user":
        text = user_text(text)
    calls = tool_calls or []
    results = tool_results or []
    if not text and not calls and not results:
        return None
    return {
        "role": role,
        "text": text,
        "tool_calls": calls,
        "tool_results": results,
        "inert": True,
        "trust": "untrusted_external_history",
    }


def finalise(
    result: dict[str, Any], warnings: list[dict[str, str]]
) -> dict[str, Any]:
    turns = result.get("turns", [])
    last_user_request = next(
        (
            turn["text"]
            for turn in reversed(turns)
            if turn["role"] == "user" and turn["text"]
        ),
        None,
    )
    last_assistant_action = next(
        (
            turn["text"]
            for turn in reversed(turns)
            if turn["role"] == "assistant" and turn["text"]
        ),
        None,
    )
    if len(turns) > MAX_TURNS:
        result["turns"] = turns[-MAX_TURNS:]
        warnings.append(
            warning("turns_truncated", f"Only the last {MAX_TURNS} turns were returned.")
        )
    result["warnings"] = warnings
    result["last_user_request"] = last_user_request
    result["last_assistant_action"] = last_assistant_action
    return result


def candidate(
    tool: str,
    source: str,
    session_id: str,
    path: str | Path,
    title: str | None,
    cwd: str | None,
    created_at: Any = None,
    updated_at: Any = None,
) -> dict[str, Any]:
    path_text = str(path)
    return {
        "tool": tool,
        "source": source,
        "session_id": session_id,
        "path": path_text,
        "title": one_line(title or "(untitled)", 160),
        "cwd": cwd,
        "created_at": iso_time(created_at),
        "updated_at": iso_time(updated_at, path_text),
        "_sort_time": timestamp_value(updated_at, path_text),
    }


def public_candidate(item: dict[str, Any]) -> dict[str, Any]:
    return {key: value for key, value in item.items() if not key.startswith("_")}


def newest_database(paths: Iterable[Path], prefix: str) -> Path | None:
    numbered: list[tuple[int, Path]] = []
    for path in paths:
        match = re.search(rf"{re.escape(prefix)}(\d+)\.sqlite$", path.name)
        if match and path.is_file() and not path.is_symlink():
            numbered.append((int(match.group(1)), path))
    return max(numbered, default=(0, None), key=lambda item: item[0])[1]


def codex_home() -> Path:
    return Path(os.environ.get("CODEX_HOME", "~/.codex")).expanduser()


def codex_metadata_from_file(path: Path) -> dict[str, Any] | None:
    meta: dict[str, Any] = {}
    first_user = ""
    try:
        records = read_jsonl(path, max_records=300)[0]
    except (OSError, ReaderError):
        return None
    for record in records:
        if record.get("type") == "session_meta":
            payload = record.get("payload")
            if isinstance(payload, dict):
                meta = payload
        if record.get("type") == "response_item":
            payload = record.get("payload")
            if isinstance(payload, dict) and payload.get("type") == "message":
                if payload.get("role") == "user":
                    first_user = user_text(content_text(payload.get("content")))
                    if first_user:
                        break
    match = re.search(
        r"([0-9a-f-]{36})\.jsonl(?:\.zst)?$", path.name, re.IGNORECASE
    )
    session_id = safe_string(meta.get("id") or meta.get("session_id"))
    if not session_id and match:
        session_id = match.group(1)
    return candidate(
        "codex",
        "codex-" + safe_string(meta.get("source") or "cli"),
        session_id or path.stem,
        path,
        first_user,
        safe_string(meta.get("cwd")) or None,
        meta.get("timestamp"),
        None,
    )


def discover_codex(cwd: str) -> list[dict[str, Any]]:
    home = codex_home()
    database = newest_database(
        list(home.glob("state_*.sqlite")) + list((home / "sqlite").glob("state_*.sqlite")),
        "state_",
    )
    found: list[dict[str, Any]] = []
    database_usable = False
    if database:
        try:
            db = open_sqlite_readonly(database)
            columns = table_columns(db, "threads")
            required = {"id", "rollout_path", "source", "cwd", "archived"}
            if not required.issubset(columns):
                raise sqlite3.OperationalError("unsupported Codex threads schema")
            title_expr = "title" if "title" in columns else "''"
            first_expr = (
                "first_user_message" if "first_user_message" in columns else "''"
            )
            created_expr = "created_at" if "created_at" in columns else "NULL"
            updated_expr = (
                "updated_at_ms"
                if "updated_at_ms" in columns
                else "updated_at"
                if "updated_at" in columns
                else "NULL"
            )
            rows = db.execute(
                f"SELECT id, rollout_path, {created_expr}, {updated_expr}, source, cwd, "
                f"{title_expr}, {first_expr} FROM threads "
                "WHERE archived = 0 AND source IN ('cli', 'vscode') "
                f"ORDER BY {updated_expr} DESC, id ASC"
            ).fetchall()
            database_usable = True
            db.close()
            for sid, path, created, updated, source, row_cwd, title, first in rows:
                if not same_cwd(safe_string(row_cwd) or None, cwd):
                    continue
                rollout = Path(safe_string(path)).expanduser()
                if not rollout.is_absolute():
                    rollout = home / rollout
                if not rollout.exists() and rollout.name.endswith(".jsonl"):
                    compressed = Path(str(rollout) + ".zst")
                    if compressed.exists():
                        rollout = compressed
                if (
                    rollout.is_file()
                    and not rollout.is_symlink()
                    and path_is_within(rollout, home)
                ):
                    found.append(
                        candidate(
                            "codex",
                            "codex-" + safe_string(source or "cli"),
                            safe_string(sid),
                            rollout,
                            user_text(safe_string(title or first)),
                            safe_string(row_cwd),
                            created,
                            updated,
                        )
                    )
        except (ReaderError, sqlite3.Error):
            found = []
    if database_usable:
        return found
    paths = glob.glob(str(home / "sessions" / "**" / "*.jsonl"), recursive=True)
    paths += glob.glob(
        str(home / "sessions" / "**" / "*.jsonl.zst"), recursive=True
    )
    for raw in paths:
        path = Path(raw)
        if (
            not path.is_file()
            or path.is_symlink()
            or not path_is_within(path, home)
        ):
            continue
        item = codex_metadata_from_file(path)
        if (
            item
            and item.get("source") in {"codex-cli", "codex-vscode"}
            and same_cwd(item.get("cwd"), cwd)
        ):
            found.append(item)
    return found


def codex_turn(
    payload: Any, max_tool_chars: int
) -> tuple[dict[str, Any] | None, bool]:
    if not isinstance(payload, dict):
        return None, True
    kind = safe_string(payload.get("type"))
    if kind == "message":
        role = safe_string(payload.get("role"))
        turn = inert_turn(role, content_text(payload.get("content")))
        return turn, role not in {"user", "assistant"}
    if kind == "local_shell_call":
        call = {
            "name": "local_shell",
            "arguments": json_preview(payload.get("action"), max_tool_chars),
        }
        return inert_turn("assistant", tool_calls=[call]), False
    if kind in {"function_call", "custom_tool_call"}:
        call = {
            "name": safe_string(payload.get("name") or kind),
            "arguments": json_preview(
                payload.get("arguments", payload.get("input")), max_tool_chars
            ),
        }
        return inert_turn("assistant", tool_calls=[call]), False
    if kind in {"function_call_output", "custom_tool_call_output"}:
        result = {
            "call_id": safe_string(payload.get("call_id")),
            "output": one_line(payload.get("output"), max_tool_chars),
            "stale": "true",
        }
        return inert_turn("assistant", tool_results=[result]), False
    return None, True


class BoundedTurns:
    def __init__(self):
        self.turns: list[dict[str, Any]] = []
        self.prefix_last_text: dict[str, str] = {}
        self.truncated = False

    def clear(self) -> None:
        self.turns.clear()
        self.prefix_last_text.clear()
        self.truncated = False

    def append(self, turn: dict[str, Any]) -> None:
        self.turns.append(turn)
        if len(self.turns) <= MAX_TURNS + 1:
            return
        removed = self.turns.pop(0)
        role = safe_string(removed.get("role"))
        text = safe_string(removed.get("text"))
        if role in {"user", "assistant"} and text:
            self.prefix_last_text[role] = text
        self.truncated = True

    def drop_last_user_turns(self, number: int) -> bool:
        if number <= 0:
            return True
        if self.truncated:
            return False
        positions = [
            index
            for index, turn in enumerate(self.turns)
            if turn.get("role") == "user"
        ]
        if positions:
            cut = positions[max(0, len(positions) - number)]
            del self.turns[cut:]
        return True

    def recent(self) -> list[dict[str, Any]]:
        return list(self.turns)

    def last_text(self, role: str) -> str | None:
        return next(
            (
                safe_string(turn.get("text"))
                for turn in reversed(self.turns)
                if turn.get("role") == role and turn.get("text")
            ),
            self.prefix_last_text.get(role),
        )


class CodexTurnJournal:
    def __init__(self, database: sqlite3.Connection):
        self.database = database
        self.database.execute("PRAGMA journal_mode = OFF")
        self.database.execute("PRAGMA synchronous = OFF")
        self.database.execute(
            "CREATE TABLE turns ("
            "sequence INTEGER PRIMARY KEY, "
            "role TEXT NOT NULL, "
            "text TEXT NOT NULL, "
            "payload TEXT NOT NULL)"
        )
        self.database.execute(
            "CREATE INDEX turns_role_sequence ON turns (role, sequence)"
        )

    def clear(self) -> None:
        self.database.execute("DELETE FROM turns")

    def append(self, turn: dict[str, Any]) -> None:
        self.database.execute(
            "INSERT INTO turns (role, text, payload) VALUES (?, ?, ?)",
            (
                safe_string(turn.get("role")),
                safe_string(turn.get("text")),
                json.dumps(turn, ensure_ascii=False, separators=(",", ":")),
            ),
        )

    def drop_last_user_turns(self, number: int) -> bool:
        if number <= 0:
            return True
        row = self.database.execute(
            "SELECT COUNT(*) FROM turns WHERE role = 'user'"
        ).fetchone()
        user_count = int(row[0]) if row else 0
        if user_count <= 0:
            return True
        offset = min(number, user_count) - 1
        row = self.database.execute(
            "SELECT sequence FROM turns WHERE role = 'user' "
            "ORDER BY sequence DESC LIMIT 1 OFFSET ?",
            (offset,),
        ).fetchone()
        if row:
            self.database.execute(
                "DELETE FROM turns WHERE sequence >= ?", (row[0],)
            )
        return True

    def recent(self) -> list[dict[str, Any]]:
        rows = self.database.execute(
            "SELECT payload FROM ("
            "SELECT sequence, payload FROM turns ORDER BY sequence DESC LIMIT ?"
            ") ORDER BY sequence ASC",
            (MAX_TURNS + 1,),
        ).fetchall()
        return [json.loads(row[0]) for row in rows]

    def last_text(self, role: str) -> str | None:
        row = self.database.execute(
            "SELECT text FROM turns WHERE role = ? AND text <> '' "
            "ORDER BY sequence DESC LIMIT 1",
            (role,),
        ).fetchone()
        return safe_string(row[0]) if row else None


def process_codex_stream(
    path: str | Path, max_tool_chars: int, turns: Any
) -> tuple[int, int, bool]:
    skipped = 0
    requires_journal = False

    def consume_record(record: dict[str, Any]) -> None:
        nonlocal requires_journal, skipped
        record_type = record.get("type")
        payload = record.get("payload")
        if record_type == "compacted":
            replacement = (
                payload.get("replacement_history")
                if isinstance(payload, dict)
                else None
            )
            if isinstance(replacement, list):
                turns.clear()
                skipped = 0
                requires_journal = False
                for replacement_payload in replacement:
                    turn, unsafe = codex_turn(
                        replacement_payload, max_tool_chars
                    )
                    skipped += int(unsafe)
                    if turn:
                        turns.append(turn)
            return
        if requires_journal:
            return
        if record_type == "response_item":
            turn, unsafe = codex_turn(payload, max_tool_chars)
            skipped += int(unsafe)
            if turn:
                turns.append(turn)
        elif (
            record_type == "event_msg"
            and isinstance(payload, dict)
            and payload.get("type") == "thread_rolled_back"
        ):
            number = payload.get("num_turns")
            applied = turns.drop_last_user_turns(
                number
                if isinstance(number, int) and not isinstance(number, bool)
                else 0
            )
            requires_journal = not applied
        elif record_type not in {"session_meta", "event_msg"}:
            skipped += 1

    malformed = consume_jsonl(path, consume_record)
    return malformed, skipped, requires_journal


def read_codex(item: dict[str, Any], max_tool_chars: int) -> dict[str, Any]:
    bounded = BoundedTurns()
    malformed, skipped, requires_journal = process_codex_stream(
        item["path"], max_tool_chars, bounded
    )
    if requires_journal:
        with tempfile.TemporaryDirectory(
            prefix="resume-codex-turns-"
        ) as temporary:
            turns_database: sqlite3.Connection | None = None
            try:
                turns_database = sqlite3.connect(
                    str(Path(temporary) / "turns.sqlite")
                )
                journal = CodexTurnJournal(turns_database)
                malformed, skipped, _ = process_codex_stream(
                    item["path"], max_tool_chars, journal
                )
                turns = journal.recent()
                last_user_request = journal.last_text("user")
                last_assistant_action = journal.last_text("assistant")
            except sqlite3.Error as exc:
                raise ReaderError(
                    f"temporary Codex turn journal failed: {exc}"
                ) from exc
            finally:
                if turns_database is not None:
                    turns_database.close()
    else:
        turns = bounded.recent()
        last_user_request = bounded.last_text("user")
        last_assistant_action = bounded.last_text("assistant")
    warnings: list[dict[str, str]] = []
    if malformed:
        warnings.append(warning("malformed_records", f"Skipped {malformed} malformed record(s)."))
    if skipped:
        warnings.append(
            warning(
                "unsafe_records_skipped",
                f"Skipped {skipped} instruction, reasoning, or unsupported record(s).",
            )
        )
    result = {**public_candidate(item), "turns": turns}
    result = finalise(result, warnings)
    result["last_user_request"] = last_user_request
    result["last_assistant_action"] = last_assistant_action
    return result


def claude_home() -> Path:
    return Path(os.environ.get("CLAUDE_CONFIG_DIR", "~/.claude")).expanduser()


def claude_project_slug(cwd: str) -> str:
    return canonical(cwd).replace(os.sep, "-")


def claude_parent(record: dict[str, Any]) -> str:
    return safe_string(record.get("parentUuid") or record.get("logicalParentUuid"))


class ClaudeChainIndex:
    def __init__(self, database: sqlite3.Connection, path: Path):
        self.database = database
        self.path = path
        self.sequence = 0
        self.unindexable = 0
        self.database.execute("PRAGMA journal_mode = OFF")
        self.database.execute("PRAGMA synchronous = OFF")
        self.database.execute(
            "CREATE TABLE claude_messages ("
            "uuid BLOB PRIMARY KEY, "
            "sequence INTEGER NOT NULL, "
            "parent_uuid BLOB NOT NULL, "
            "sort_time REAL NOT NULL, "
            "payload TEXT NOT NULL)"
        )
        self.database.execute(
            "CREATE INDEX claude_messages_parent "
            "ON claude_messages (parent_uuid)"
        )
        self.database.execute(
            "CREATE TABLE claude_visited (uuid BLOB PRIMARY KEY)"
        )

    @staticmethod
    def key(value: Any) -> bytes:
        return safe_string(value).encode(
            "utf-8",
            errors="surrogatepass",
        )

    def consume(self, record: dict[str, Any]) -> None:
        raw_uuid = record.get("uuid")
        if (
            record.get("type") not in CLAUDE_CHAIN_TYPES
            or not raw_uuid
            or record.get("isSidechain")
        ):
            return
        try:
            payload = json.dumps(
                record,
                ensure_ascii=True,
                separators=(",", ":"),
            )
        except (RecursionError, TypeError, ValueError):
            self.unindexable += 1
            return
        self.sequence += 1
        parent = claude_parent(record)
        self.database.execute(
            "INSERT INTO claude_messages "
            "(uuid, sequence, parent_uuid, sort_time, payload) "
            "VALUES (?, ?, ?, ?, ?) "
            "ON CONFLICT(uuid) DO UPDATE SET "
            "parent_uuid = excluded.parent_uuid, "
            "sort_time = excluded.sort_time, "
            "payload = excluded.payload",
            (
                self.key(raw_uuid),
                self.sequence,
                self.key(parent) if parent else b"",
                timestamp_value(record.get("timestamp"), self.path),
                payload,
            ),
        )

    def active_chain_from_leaf(self) -> Iterable[dict[str, Any]]:
        leaf = self.database.execute(
            "SELECT messages.uuid "
            "FROM claude_messages AS messages "
            "WHERE NOT EXISTS ("
            "SELECT 1 FROM claude_messages AS children "
            "WHERE children.parent_uuid = messages.uuid"
            ") "
            "ORDER BY messages.sort_time DESC, messages.sequence ASC "
            "LIMIT 1"
        ).fetchone()
        if leaf is None:
            leaf = self.database.execute(
                "SELECT uuid FROM claude_messages "
                "ORDER BY sort_time DESC, sequence ASC LIMIT 1"
            ).fetchone()
        if leaf is None:
            return
        uuid = leaf[0]
        while uuid:
            inserted = self.database.execute(
                "INSERT OR IGNORE INTO claude_visited (uuid) VALUES (?)",
                (uuid,),
            )
            if inserted.rowcount != 1:
                break
            row = self.database.execute(
                "SELECT parent_uuid, payload FROM claude_messages WHERE uuid = ?",
                (uuid,),
            ).fetchone()
            if row is None:
                break
            try:
                record = json.loads(row[1])
            except (json.JSONDecodeError, RecursionError, ValueError):
                record = None
            if isinstance(record, dict):
                yield record
            uuid = row[0]


def claude_metadata(path: Path) -> dict[str, Any] | None:
    try:
        handle = open(path, encoding="utf-8", errors="replace")
    except OSError:
        return None
    cwd = None
    session_id = path.stem
    first_user = ""
    titles = {
        "custom-title": "",
        "ai-title": "",
        "summary": "",
    }
    title_fields = {
        "custom-title": "customTitle",
        "ai-title": "aiTitle",
        "summary": "summary",
    }
    created = None
    updated = None
    with handle:
        for line in handle:
            try:
                record = json.loads(line)
            except (json.JSONDecodeError, RecursionError, ValueError):
                continue
            if not isinstance(record, dict):
                continue
            if record.get("isSidechain"):
                continue
            cwd = cwd or record.get("cwd")
            session_id = safe_string(record.get("sessionId") or session_id)
            timestamp = record.get("timestamp")
            created = created or timestamp
            updated = timestamp or updated
            record_type = safe_string(record.get("type"))
            title_field = title_fields.get(record_type)
            if title_field and isinstance(record.get(title_field), str):
                titles[record_type] = record[title_field]
            if (
                record_type == "user"
                and not record.get("isMeta")
                and not record.get("isCompactSummary")
                and not first_user
            ):
                message = record.get("message")
                if isinstance(message, dict):
                    first_user = user_text(content_text(message.get("content")))
    title = next(
        (
            titles[record_type]
            for record_type in ("custom-title", "ai-title", "summary")
            if titles[record_type]
        ),
        first_user,
    )
    return candidate(
        "claude", "claude-code", session_id, path, title, safe_string(cwd) or None,
        created, updated
    )


def discover_claude(cwd: str) -> list[dict[str, Any]]:
    projects = claude_home() / "projects"
    direct = projects / claude_project_slug(cwd)
    paths = list(direct.glob("*.jsonl")) if direct.is_dir() else []
    if not paths:
        paths = list(projects.glob("*/*.jsonl"))
    found = [
        item
        for path in paths
        if path.is_file()
        and not path.is_symlink()
        and path_is_within(path, projects)
        and (item := claude_metadata(path))
    ]
    return [item for item in found if same_cwd(item.get("cwd"), cwd)]


def claude_turn(
    record: dict[str, Any], max_tool_chars: int
) -> tuple[dict[str, Any] | None, int]:
    if record.get("isMeta") or record.get("isCompactSummary"):
        return None, 1
    role = safe_string(record.get("type"))
    if role not in {"user", "assistant"}:
        return None, 1
    message = record.get("message")
    if not isinstance(message, dict):
        return None, 1
    content = message.get("content")
    calls: list[dict[str, str]] = []
    results: list[dict[str, str]] = []
    skipped = 0
    if isinstance(content, list):
        for block in content:
            if not isinstance(block, dict):
                continue
            kind = safe_string(block.get("type"))
            if kind == "tool_use":
                calls.append(
                    {
                        "name": safe_string(block.get("name")),
                        "arguments": json_preview(
                            block.get("input"), max_tool_chars
                        ),
                    }
                )
            elif kind == "tool_result":
                results.append(
                    {
                        "call_id": safe_string(block.get("tool_use_id")),
                        "output": one_line(
                            content_text(block.get("content")), max_tool_chars
                        ),
                        "stale": "true",
                    }
                )
            elif kind in {"thinking", "redacted_thinking"}:
                skipped += 1
    return inert_turn(role, content_text(content), calls, results), skipped


def read_claude(item: dict[str, Any], max_tool_chars: int) -> dict[str, Any]:
    path = Path(item["path"])
    turns_from_leaf: list[dict[str, Any]] = []
    last_text: dict[str, str] = {}
    skipped = 0
    with tempfile.TemporaryDirectory(prefix="resume-claude-chain-") as temporary:
        try:
            with closing(
                sqlite3.connect(str(Path(temporary) / "chain.sqlite"))
            ) as database:
                index = ClaudeChainIndex(database, path)
                malformed = consume_jsonl(path, index.consume)
                skipped += index.unindexable
                for record in index.active_chain_from_leaf():
                    turn, record_skipped = claude_turn(record, max_tool_chars)
                    skipped += record_skipped
                    if not turn:
                        continue
                    role = safe_string(turn.get("role"))
                    text = safe_string(turn.get("text"))
                    if role in {"user", "assistant"} and text:
                        last_text.setdefault(role, text)
                    if len(turns_from_leaf) < MAX_TURNS + 1:
                        turns_from_leaf.append(turn)
        except sqlite3.Error as exc:
            raise ReaderError(
                f"temporary Claude chain index failed: {exc}"
            ) from exc
    turns = list(reversed(turns_from_leaf))
    warnings: list[dict[str, str]] = []
    if malformed:
        warnings.append(warning("malformed_records", f"Skipped {malformed} malformed record(s)."))
    if skipped:
        warnings.append(
            warning(
                "unsafe_records_skipped",
                f"Skipped {skipped} hidden or unsupported record(s).",
            )
        )
    result = finalise({**public_candidate(item), "turns": turns}, warnings)
    result["last_user_request"] = last_text.get("user")
    result["last_assistant_action"] = last_text.get("assistant")
    return result


def grok_home() -> Path:
    return Path(os.environ.get("GROK_HOME", "~/.grok")).expanduser()


def grok_metadata(summary_path: Path) -> dict[str, Any] | None:
    try:
        summary = read_json(summary_path)
    except (OSError, ValueError, json.JSONDecodeError, RecursionError):
        return None
    if not isinstance(summary, dict):
        return None
    info = summary.get("info") if isinstance(summary.get("info"), dict) else {}
    directory = summary_path.parent
    session_id = safe_string(info.get("id") or directory.name)
    cwd = safe_string(info.get("cwd")) or unquote(directory.parent.name)
    title = safe_string(
        summary.get("session_summary") or summary.get("generated_title")
    )
    return candidate(
        "grok", "grok-build", session_id, directory, title, cwd,
        summary.get("created_at"), summary.get("updated_at")
    )


def discover_grok(cwd: str) -> list[dict[str, Any]]:
    root = grok_home() / "sessions"
    found = [
        item
        for raw in glob.glob(str(root / "*" / "*" / "summary.json"))
        if (path := Path(raw)).is_file()
        and not path.is_symlink()
        and path_is_within(path, root)
        and (item := grok_metadata(path))
    ]
    return [item for item in found if same_cwd(item.get("cwd"), cwd)]


def grok_turn(
    record: dict[str, Any], max_tool_chars: int
) -> tuple[dict[str, Any] | None, int]:
    kind = safe_string(record.get("type")).lower()
    if record.get("synthetic_reason"):
        return None, 1
    if kind in {"user", "assistant"}:
        calls: list[dict[str, str]] = []
        skipped = 0
        if kind == "assistant" and isinstance(record.get("tool_calls"), list):
            for raw_call in record["tool_calls"]:
                if not isinstance(raw_call, dict):
                    skipped += 1
                    continue
                calls.append(
                    {
                        "call_id": safe_string(
                            raw_call.get("id") or raw_call.get("call_id")
                        ),
                        "name": safe_string(raw_call.get("name") or "tool"),
                        "arguments": json_preview(
                            raw_call.get("arguments", raw_call.get("input")),
                            max_tool_chars,
                        ),
                    }
                )
        return (
            inert_turn(
                kind, content_text(record.get("content")), tool_calls=calls
            ),
            skipped,
        )
    if kind in {"tool_call", "backend_tool_call"}:
        call = {
            "name": safe_string(
                record.get("name") or record.get("tool_name") or kind
            ),
            "arguments": json_preview(
                record.get("arguments", record.get("input")), max_tool_chars
            ),
        }
        return inert_turn("assistant", tool_calls=[call]), 0
    if kind == "tool_result":
        result = {
            "call_id": safe_string(
                record.get("call_id") or record.get("tool_call_id")
            ),
            "output": one_line(
                record.get("output", record.get("content")), max_tool_chars
            ),
            "stale": "true",
        }
        return inert_turn("assistant", tool_results=[result]), 0
    return None, 1


def read_grok(item: dict[str, Any], max_tool_chars: int) -> dict[str, Any]:
    session_dir = Path(item["path"])
    transcript = session_dir / "chat_history.jsonl"
    transcript_safe = (
        transcript.is_file()
        and not transcript.is_symlink()
        and path_is_within(transcript, session_dir)
    )
    bounded = BoundedTurns()
    skipped = 0

    def consume_record(record: dict[str, Any]) -> None:
        nonlocal skipped
        turn, record_skipped = grok_turn(record, max_tool_chars)
        skipped += record_skipped
        if turn:
            bounded.append(turn)

    malformed = consume_jsonl(transcript, consume_record) if transcript_safe else 0
    turns = bounded.recent()
    warnings: list[dict[str, str]] = []
    if not transcript_safe:
        warnings.append(
            warning(
                "grok_transcript_unavailable",
                "The Grok session metadata exists, but a safe chat_history.jsonl "
                "is unavailable.",
            )
        )
    if malformed:
        warnings.append(warning("malformed_records", f"Skipped {malformed} malformed record(s)."))
    if skipped:
        warnings.append(
            warning(
                "unsafe_records_skipped",
                f"Skipped {skipped} hidden or unsupported record(s).",
            )
        )
    result = finalise({**public_candidate(item), "turns": turns}, warnings)
    result["last_user_request"] = bounded.last_text("user")
    result["last_assistant_action"] = bounded.last_text("assistant")
    return result


def cursor_root() -> Path:
    return Path(os.environ.get("CURSOR_HOME", "~/.cursor")).expanduser()


def cursor_desktop_databases() -> list[Path]:
    paths = [
        Path("~/Library/Application Support/Cursor/User/globalStorage/state.vscdb"),
        Path("~/.config/Cursor/User/globalStorage/state.vscdb"),
    ]
    appdata = os.environ.get("APPDATA")
    if appdata:
        paths.append(Path(appdata) / "Cursor/User/globalStorage/state.vscdb")
    return [
        path.expanduser()
        for path in paths
        if path.expanduser().is_file() and not path.expanduser().is_symlink()
    ]


def nested_strings(value: Any, wanted: set[str]) -> list[str]:
    found: list[str] = []
    pending: list[tuple[bool, Any]] = [(False, value)]
    while pending:
        emit, current = pending.pop()
        if emit:
            found.append(current)
        elif isinstance(current, dict):
            actions: list[tuple[bool, Any]] = []
            for key, child in current.items():
                if key.lower() in wanted and isinstance(child, str):
                    actions.append((True, child))
                elif isinstance(child, (dict, list)):
                    actions.append((False, child))
            pending.extend(reversed(actions))
        elif isinstance(current, list):
            pending.extend((False, child) for child in reversed(current))
    return found


def cursor_header_values(database: sqlite3.Connection) -> list[dict[str, Any]]:
    columns = table_columns(database, "composerHeaders")
    if {"composerId", "value"}.issubset(columns):
        updated = "lastUpdatedAt" if "lastUpdatedAt" in columns else "NULL"
        archived = (
            "COALESCE(isArchived, 0) = 0" if "isArchived" in columns else "1"
        )
        subagent = (
            "COALESCE(isSubagent, 0) = 0" if "isSubagent" in columns else "1"
        )
        values: list[dict[str, Any]] = []
        for session_id, raw_updated, raw_value in database.execute(
            f"SELECT composerId, {updated}, value FROM composerHeaders "
            f"WHERE {archived} AND {subagent}"
        ):
            value = decode_jsonish(raw_value)
            if not isinstance(value, dict):
                value = {}
            value.setdefault("composerId", session_id)
            value.setdefault("lastUpdatedAt", raw_updated)
            values.append(value)
        return values
    try:
        row = database.execute(
            "SELECT value FROM ItemTable WHERE key = 'composer.composerHeaders'"
        ).fetchone()
    except sqlite3.Error:
        return []
    headers = decode_jsonish(row[0]) if row else None
    return (
        [value for value in headers.get("allComposers", []) if isinstance(value, dict)]
        if isinstance(headers, dict)
        else []
    )


def cursor_first_user_title(value: Any) -> str | None:
    pending = [value]
    while pending:
        current = pending.pop()
        if isinstance(current, list):
            pending.extend(reversed(current))
            continue
        if not isinstance(current, dict):
            continue
        role = safe_string(current.get("role") or current.get("type")).lower()
        if role in {
            "system",
            "developer",
            "instruction",
            "instructions",
            "thinking",
            "reasoning",
            "redacted_thinking",
            "signature",
            "encrypted_content",
        }:
            continue
        if role in {"human", "user"}:
            text = current.get(
                "text", current.get("content", current.get("message"))
            )
            if isinstance(text, dict):
                text = text.get("text", text.get("content"))
            rendered = (
                content_text(text)
                if isinstance(text, list)
                else text
                if isinstance(text, str)
                else ""
            )
            if title := user_text(rendered.strip()):
                return title
        children = [
            child
            for key, child in current.items()
            if key.lower() in CURSOR_CONVERSATION_KEYS
            and isinstance(child, (dict, list))
        ]
        pending.extend(reversed(children))
    return None


def cursor_record_matches_cwd(value: Any, cwd: str) -> bool:
    pending = [value]
    while pending:
        current = pending.pop()
        if isinstance(current, dict):
            children: list[Any] = []
            for key, child in current.items():
                if (
                    key.lower() in CURSOR_CWD_KEYS
                    and isinstance(child, str)
                    and same_cwd(child, cwd)
                ):
                    return True
                if isinstance(child, (dict, list)):
                    children.append(child)
            pending.extend(reversed(children))
        elif isinstance(current, list):
            pending.extend(reversed(current))
    return False


def cursor_transcript_candidate(path: Path, cwd: str) -> dict[str, Any] | None:
    transcript_root = cursor_root() / "projects"
    if (
        not path.is_file()
        or path.is_symlink()
        or not path_is_within(path, transcript_root)
    ):
        return None
    matched_cwd = False
    title = None
    try:
        with open(path, encoding="utf-8", errors="replace") as handle:
            for line in handle:
                if not line.strip():
                    continue
                try:
                    record = json.loads(line)
                except (json.JSONDecodeError, RecursionError, ValueError):
                    continue
                if not isinstance(record, dict):
                    continue
                if not matched_cwd:
                    matched_cwd = cursor_record_matches_cwd(record, cwd)
                if title is None:
                    title = cursor_first_user_title(record)
                if matched_cwd and title:
                    break
    except OSError:
        return None
    if not matched_cwd:
        return None
    return candidate(
        "cursor",
        "cursor-transcript",
        path.stem,
        path,
        title,
        cwd,
        None,
        None,
    )


def cursor_cli_candidate(
    session_dir: Path, fallback_cwd: str | None = None
) -> dict[str, Any] | None:
    if (
        not session_dir.is_dir()
        or session_dir.is_symlink()
        or not path_is_within(session_dir, cursor_root() / "chats")
    ):
        return None
    meta_path = session_dir / "meta.json"
    try:
        meta = (
            read_json(meta_path)
            if meta_path.is_file() and not meta_path.is_symlink()
            else {}
        )
    except (OSError, ValueError, json.JSONDecodeError, RecursionError):
        meta = {}
    if not isinstance(meta, dict):
        meta = {}
    return candidate(
        "cursor",
        "cursor-cli",
        safe_string(meta.get("id") or session_dir.name),
        session_dir,
        meta.get("name") or meta.get("title"),
        safe_string(meta.get("cwd")) or fallback_cwd,
        meta.get("createdAt"),
        meta.get("updatedAt"),
    )


def discover_cursor(cwd: str) -> list[dict[str, Any]]:
    root = cursor_root()
    digest = hashlib.md5(canonical(cwd).encode("utf-8")).hexdigest()
    found: list[dict[str, Any]] = []
    for session_dir in (root / "chats" / digest).glob("*"):
        if item := cursor_cli_candidate(session_dir, cwd):
            found.append(item)
    for database in cursor_desktop_databases():
        try:
            db = open_sqlite_readonly(database)
            values = cursor_header_values(db)
            db.close()
        except (ReaderError, sqlite3.Error, ValueError, TypeError):
            continue
        for header in values:
            if not isinstance(header, dict) or header.get("isDraft"):
                continue
            paths = nested_strings(
                header, {"path", "cwd", "rootpath", "folderpath", "fsPath".lower()}
            )
            if paths and not any(same_cwd(path, cwd) for path in paths):
                continue
            # A header without a filesystem path cannot safely be assigned to
            # whichever repository happens to be open now.
            if not paths:
                continue
            session_id = safe_string(header.get("composerId"))
            if session_id:
                found.append(
                    candidate(
                        "cursor", "cursor-desktop", session_id, database,
                        header.get("name") or header.get("title"), cwd,
                        header.get("createdAt"), header.get("lastUpdatedAt")
                    )
                )
    for raw in glob.glob(
        str(root / "projects" / "*" / "agent-transcripts" / "*" / "*.jsonl")
    ):
        path = Path(raw)
        if item := cursor_transcript_candidate(path, cwd):
            found.append(item)
    return found


def consume_cursor_turns(
    value: Any,
    max_tool_chars: int,
    consume: Callable[[dict[str, Any]], None],
) -> None:
    pending = [value]
    while pending:
        current = pending.pop()
        if isinstance(current, list):
            pending.extend(reversed(current))
            continue
        if not isinstance(current, dict):
            continue
        role = safe_string(current.get("role") or current.get("type")).lower()
        if role in {
            "system",
            "developer",
            "instruction",
            "instructions",
            "thinking",
            "reasoning",
            "redacted_thinking",
            "signature",
            "encrypted_content",
        }:
            continue
        if role in {"human", "user"}:
            role = "user"
        elif role in {"ai", "assistant"}:
            role = "assistant"
        if role in {"user", "assistant"}:
            text = current.get(
                "text", current.get("content", current.get("message"))
            )
            if isinstance(text, dict):
                text = text.get("text", text.get("content"))
            rendered = (
                content_text(text)
                if isinstance(text, list)
                else text
                if isinstance(text, str)
                else ""
            )
            calls: list[dict[str, str]] = []
            results: list[dict[str, str]] = []
            content = current.get("content")
            blocks = content if isinstance(content, list) else []
            for block in blocks:
                if not isinstance(block, dict):
                    continue
                kind = safe_string(block.get("type")).lower()
                if kind in {"tool_use", "tool_call"}:
                    calls.append(
                        {
                            "call_id": safe_string(
                                block.get("id") or block.get("call_id")
                            ),
                            "name": safe_string(block.get("name") or "tool"),
                            "arguments": json_preview(
                                block.get("input", block.get("arguments")),
                                max_tool_chars,
                            ),
                        }
                    )
                elif kind in {"tool_result", "tool_output"}:
                    results.append(
                        {
                            "call_id": safe_string(
                                block.get("tool_use_id") or block.get("call_id")
                            ),
                            "output": one_line(
                                content_text(block.get("content")), max_tool_chars
                            ),
                            "stale": "true",
                        }
                    )
            top_calls = current.get("tool_calls")
            if isinstance(top_calls, list):
                for raw_call in top_calls:
                    if not isinstance(raw_call, dict):
                        continue
                    call = (
                        raw_call["function"]
                        if isinstance(raw_call.get("function"), dict)
                        else raw_call
                    )
                    calls.append(
                        {
                            "call_id": safe_string(
                                raw_call.get("id") or call.get("call_id")
                            ),
                            "name": safe_string(call.get("name") or "tool"),
                            "arguments": json_preview(
                                call.get("arguments", call.get("input")),
                                max_tool_chars,
                            ),
                        }
                    )
            turn = inert_turn(role, rendered, calls, results)
            if turn:
                consume(turn)
                continue
        elif role in {"tool", "tool_result", "tool_output"}:
            result = {
                "call_id": safe_string(
                    current.get("tool_call_id") or current.get("call_id")
                ),
                "output": one_line(
                    content_text(current.get("content"))
                    or current.get("output")
                    or current.get("text"),
                    max_tool_chars,
                ),
                "stale": "true",
            }
            turn = inert_turn("assistant", tool_results=[result])
            if turn:
                consume(turn)
                continue
        children = [
            child
            for key, child in current.items()
            if key.lower() in CURSOR_CONVERSATION_KEYS
            and isinstance(child, (dict, list))
        ]
        pending.extend(reversed(children))


def generic_cursor_turns(
    value: Any, max_tool_chars: int
) -> list[dict[str, Any]]:
    turns: list[dict[str, Any]] = []
    consume_cursor_turns(value, max_tool_chars, turns.append)
    return turns


def read_cursor(item: dict[str, Any], max_tool_chars: int) -> dict[str, Any]:
    path = Path(item["path"])
    bounded = BoundedTurns()
    warnings: list[dict[str, str]] = []

    def consume_value(value: Any) -> None:
        consume_cursor_turns(value, max_tool_chars, bounded.append)

    if path.is_file() and not path.is_symlink() and path.suffix == ".jsonl":
        transcript = path
    else:
        transcript = next(
            (
                candidate
                for candidate in (cursor_root() / "projects").glob(
                    f"*/agent-transcripts/{item['session_id']}/"
                    f"{item['session_id']}.jsonl"
                )
                if candidate.is_file()
                and not candidate.is_symlink()
                and path_is_within(candidate, cursor_root() / "projects")
            ),
            None,
        )
    if transcript:
        malformed = consume_jsonl(transcript, consume_value)
        if malformed:
            warnings.append(
                warning("malformed_records", f"Skipped {malformed} malformed record(s).")
            )
    elif path.is_dir():
        store = path / "store.db"
        if (
            store.is_file()
            and not store.is_symlink()
            and path_is_within(store, path)
        ):
            try:
                unavailable = 0
                with closing(open_sqlite_readonly(store)) as db:
                    columns = table_columns(db, "blobs")
                    key = next(
                        (name for name in ("id", "key", "hash") if name in columns),
                        None,
                    )
                    data = next(
                        (name for name in ("data", "value", "blob") if name in columns),
                        None,
                    )
                    if key and data:
                        for _, raw in db.execute(
                            f'SELECT "{key}", "{data}" FROM blobs ORDER BY "{key}"'
                        ):
                            value = decode_jsonish(raw)
                            if value is None:
                                unavailable += 1
                            else:
                                consume_value(value)
                if unavailable:
                    warnings.append(
                        warning(
                            "binary_content_unavailable",
                            f"{unavailable} Cursor blob(s) were binary, protobuf, "
                            "or non-JSON and were not inferred.",
                        )
                    )
            except (ReaderError, sqlite3.Error) as exc:
                warnings.append(warning("cursor_store_error", one_line(exc, 200)))
    else:
        try:
            query = (
                "SELECT value FROM cursorDiskKV "
                "WHERE key = ? OR key LIKE ? ORDER BY key"
            )
            parameters = (
                "composerData:" + item["session_id"],
                "bubbleId:" + item["session_id"] + ":%",
            )
            with closing(open_sqlite_readonly(path)) as db:
                for row in db.execute(query, parameters):
                    value = decode_jsonish(row[0])
                    if value is not None:
                        consume_value(value)
        except (ReaderError, sqlite3.Error) as exc:
            warnings.append(warning("cursor_store_error", one_line(exc, 200)))
    turns = bounded.recent()
    if not turns:
        warnings.append(
            warning(
                "cursor_transcript_unavailable",
                "Cursor metadata was found, but no safe text transcript was recoverable; "
                "binary/protobuf content was not inferred.",
            )
        )
    result = finalise({**public_candidate(item), "turns": turns}, warnings)
    result["last_user_request"] = bounded.last_text("user")
    result["last_assistant_action"] = bounded.last_text("assistant")
    return result


DISCOVERERS = {
    "claude": discover_claude,
    "codex": discover_codex,
    "cursor": discover_cursor,
    "grok": discover_grok,
}
READERS = {
    "claude": read_claude,
    "codex": read_codex,
    "cursor": read_cursor,
    "grok": read_grok,
}


def sort_and_dedupe(items: list[dict[str, Any]]) -> list[dict[str, Any]]:
    unique: dict[tuple[str, str], dict[str, Any]] = {}
    for item in items:
        key = (safe_string(item.get("source")), safe_string(item.get("session_id")))
        old = unique.get(key)
        if old is None or item.get("_sort_time", 0) > old.get("_sort_time", 0):
            unique[key] = item
    return sorted(unique.values(), key=lambda item: item.get("_sort_time", 0), reverse=True)


def discover(tool: str, cwd: str, within_min: int) -> list[dict[str, Any]]:
    items = sort_and_dedupe(DISCOVERERS[tool](cwd))
    if within_min > 0:
        cutoff = datetime.now(timezone.utc).timestamp() - within_min * 60
        items = [item for item in items if item.get("_sort_time", 0) >= cutoff]
    return items


def candidate_from_path(tool: str, raw_path: str) -> dict[str, Any] | None:
    path = Path(raw_path).expanduser()
    if not path.exists() or path.is_symlink():
        return None
    if (
        tool == "codex"
        and path.is_file()
        and (path.suffix == ".jsonl" or path.name.endswith(".jsonl.zst"))
    ):
        return codex_metadata_from_file(path)
    if tool == "claude" and path.is_file() and path.suffix == ".jsonl":
        return claude_metadata(path)
    if tool == "grok":
        summary = path / "summary.json" if path.is_dir() else path
        if (
            summary.name == "summary.json"
            and summary.is_file()
            and not summary.is_symlink()
        ):
            return grok_metadata(summary)
    cursor_session_directory = path.is_dir() and any(
        (path / name).is_file() and not (path / name).is_symlink()
        for name in ("store.db", "meta.json")
    )
    if tool == "cursor" and (
        cursor_session_directory
        or path.suffix == ".jsonl"
        or (path.is_file() and path.name in {"store.db", "meta.json"})
    ):
        directory = path.parent if path.name in {"store.db", "meta.json"} else path
        sid = path.stem if path.suffix == ".jsonl" else (
            directory.name if directory.is_dir() else path.stem
        )
        source = "cursor-transcript" if path.suffix == ".jsonl" else "cursor"
        return candidate("cursor", source, sid, directory, None, None)
    return None


def find_id_anywhere(tool: str, reference: str) -> dict[str, Any] | None:
    if tool == "codex":
        paths = glob.glob(
            str(codex_home() / "**" / f"*{reference}*.jsonl"), recursive=True
        )
        paths += glob.glob(
            str(codex_home() / "**" / f"*{reference}*.jsonl.zst"),
            recursive=True,
        )
        for raw in paths:
            path = Path(raw)
            if (
                path.is_file()
                and not path.is_symlink()
                and path_is_within(path, codex_home())
                and (item := codex_metadata_from_file(path))
            ):
                return item
    elif tool == "claude":
        paths = list((claude_home() / "projects").glob(f"*/*{reference}*.jsonl"))
        for path in paths:
            if (
                path.is_file()
                and not path.is_symlink()
                and path_is_within(path, claude_home() / "projects")
            ):
                return claude_metadata(path)
    elif tool == "grok":
        paths = list((grok_home() / "sessions").glob(f"*/*{reference}*/summary.json"))
        for path in paths:
            if (
                path.is_file()
                and not path.is_symlink()
                and path_is_within(path, grok_home() / "sessions")
            ):
                return grok_metadata(path)
    elif tool == "cursor":
        for session_dir in (cursor_root() / "chats").glob("*/*"):
            item = cursor_cli_candidate(session_dir)
            if item and reference.casefold() in {
                safe_string(item.get("session_id")).casefold(),
                session_dir.name.casefold(),
            }:
                return item
        for database in cursor_desktop_databases():
            try:
                db = open_sqlite_readonly(database)
                headers = cursor_header_values(db)
                db.close()
            except (ReaderError, sqlite3.Error, ValueError, TypeError):
                continue
            for header in headers:
                if (
                    header.get("isDraft")
                    or safe_string(header.get("composerId")).casefold()
                    != reference.casefold()
                ):
                    continue
                paths = nested_strings(
                    header,
                    {"path", "cwd", "rootpath", "folderpath", "fspath"},
                )
                return candidate(
                    "cursor",
                    "cursor-desktop",
                    reference,
                    database,
                    header.get("name") or header.get("title"),
                    paths[0] if paths else None,
                    header.get("createdAt"),
                    header.get("lastUpdatedAt"),
                )
        transcript = next(
            iter((cursor_root() / "projects").glob(f"*/agent-transcripts/{reference}/{reference}.jsonl")),
            None,
        )
        if (
            transcript
            and transcript.is_file()
            and not transcript.is_symlink()
            and path_is_within(transcript, cursor_root() / "projects")
        ):
            return candidate("cursor", "cursor-transcript", reference, transcript, None, None)
    return None


def resolve(
    tool: str, cwd: str, reference: str | None, within_min: int
) -> dict[str, Any]:
    ref = (reference or "").strip()
    if ref and ref.lower() != "latest":
        if tool == "cursor" and Path(ref).name == "state.vscdb":
            raise ReaderError(
                "a Cursor desktop state.vscdb path is ambiguous; pass a "
                "session ID or title instead"
            )
        direct = candidate_from_path(tool, ref)
        if direct:
            return direct
    items = discover(tool, cwd, within_min)
    if not ref or ref.lower() == "latest":
        if not items:
            raise ReaderError(f"no recent {tool} sessions found for {cwd}")
        return items[0]
    exact = [
        item
        for item in items
        if ref.casefold() == safe_string(item.get("session_id")).casefold()
    ]
    if len(exact) == 1:
        return exact[0]
    if UUID_RE.fullmatch(ref):
        if item := find_id_anywhere(tool, ref):
            return item
        raise ReaderError(f"{tool} session id not found: {ref}")
    needle = ref.casefold()
    matches = [
        item
        for item in items
        if needle in safe_string(item.get("title")).casefold()
        or needle in safe_string(item.get("session_id")).casefold()
    ]
    if not matches:
        raise ReaderError(f"no {tool} session matching {ref!r} found for {cwd}")
    if len(matches) > 1:
        raise AmbiguousReference(ref, matches)
    return matches[0]


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("tool", choices=TOOLS)
    result.add_argument("operation", choices=("list", "show"))
    result.add_argument("reference", nargs="?")
    result.add_argument("--cwd", default=os.getcwd())
    result.add_argument("--within-min", type=int, default=0)
    result.add_argument("--max-tool-chars", type=int, default=300)
    result.add_argument("--json", action="store_true")
    return result


def stdout_safe_text(value: Any) -> str:
    return safe_string(value).encode(
        "utf-8", errors="backslashreplace"
    ).decode("utf-8")


def emit(value: Any, as_json: bool) -> None:
    if as_json:
        json.dump(value, sys.stdout, ensure_ascii=True, indent=2)
        sys.stdout.write("\n")
    else:
        if isinstance(value, list):
            for item in value:
                sys.stdout.write(
                    stdout_safe_text(
                        f"{item.get('updated_at') or '-'}  "
                        f"{item.get('session_id')}  {item.get('title')}"
                    )
                    + "\n"
                )
        else:
            print(json.dumps(value, ensure_ascii=True, indent=2))


def main() -> int:
    args = parser().parse_args()
    try:
        if args.within_min < 0:
            raise ReaderError("--within-min must be non-negative")
        if args.max_tool_chars < 20:
            raise ReaderError("--max-tool-chars must be at least 20")
        cwd = canonical(args.cwd)
        if args.operation == "list":
            value = [
                public_candidate(item)
                for item in discover(args.tool, cwd, args.within_min)
            ]
        else:
            selected = resolve(args.tool, cwd, args.reference, args.within_min)
            value = READERS[args.tool](selected, args.max_tool_chars)
        emit(value, args.json)
        return 0
    except AmbiguousReference as exc:
        payload = {
            "error": str(exc),
            "reference": exc.reference,
            "candidates": [public_candidate(item) for item in exc.candidates],
        }
        emit(payload, True)
        return 3
    except (ReaderError, OSError, json.JSONDecodeError) as exc:
        emit({"error": str(exc)}, True)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
