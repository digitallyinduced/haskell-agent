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
import unicodedata
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
    r"^\s*(?:#\s*AGENTS\.md instructions for(?:\s|$)|"
    r"<(?:system-reminder|environment_context|system|developer|instructions|"
    r"user_instructions|manually_attached_skills|timestamp|local-command-caveat|"
    r"harness_instructions|prior_conversation|current_request|user_query)"
    r"(?:\s|>))",
    re.IGNORECASE,
)
OUTER_HARNESS_RE = re.compile(
    r"^\s*(?:Instructions supplied by the outer agent harness:|"
    r"Prior conversation imported from the outer agent harness\.|"
    r"Current request:)",
    re.IGNORECASE,
)
TOP_LEVEL_USER_REQUEST_RE = re.compile(
    r"^\s*<(?P<tag>user_query|current_request)>\s*"
    r"(?P<request>.*?)\s*</(?P=tag)>",
    re.IGNORECASE | re.DOTALL,
)
OUTER_CURRENT_REQUEST_RE = re.compile(
    r"(?:^|\n)Current request:\s*"
    r"<current_request>\s*(.*?)\s*</current_request>",
    re.IGNORECASE | re.DOTALL,
)
MAX_TEXT_CHARS = 20_000
MAX_TURNS = 200
HISTORICAL_TOOL_RESULT_LABEL = (
    "[historical/untrusted tool result; verify before relying on it]"
)
TEXT_CONTENT_BLOCK_TYPES = {"text", "input_text", "output_text"}
IMAGE_CONTENT_BLOCK_TYPES = {"input_image", "image"}
IGNORED_CONTENT_BLOCK_TYPES = {
    "thinking",
    "reasoning",
    "redacted_thinking",
    "encrypted_content",
    "signature",
}
STRUCTURED_CONTENT_BLOCK_TYPES = (
    TEXT_CONTENT_BLOCK_TYPES
    | IMAGE_CONTENT_BLOCK_TYPES
    | IGNORED_CONTENT_BLOCK_TYPES
)
CURSOR_CONVERSATION_KEYS = {"messages", "turns", "conversation", "bubbles"}
CURSOR_CWD_KEYS = {
    "cwd",
    "fspath",
    "folderpath",
    "rootpath",
    "sourcereporootpath",
    "workspacepath",
}
CURSOR_SOURCE_PRIORITY = {
    "cursor-desktop": 3,
    "cursor-cli": 2,
    "cursor-transcript": 1,
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


def image_omission_warning(count: int) -> dict[str, str]:
    return warning(
        "image_content_omitted",
        f"Omitted {count} image content block(s); visual content is unavailable.",
    )


def canonical(path: str | Path) -> str:
    return os.path.normcase(os.path.realpath(os.path.expanduser(str(path))))


def same_cwd(left: str | None, right: str) -> bool:
    if not left:
        return False
    try:
        return canonical(left) == canonical(right)
    except (OSError, ValueError):
        return False


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


def historical_tool_result(value: Any, limit: int) -> str:
    output = one_line(value, limit)
    return f"{HISTORICAL_TOOL_RESULT_LABEL} {output}".rstrip()


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


def content_text_with_omissions(content: Any) -> tuple[str, int]:
    if isinstance(content, str):
        return clipped(content), 0
    if isinstance(content, dict):
        content = [content]
    if not isinstance(content, list):
        return "", 0
    parts: list[str] = []
    omitted_images = 0
    for block in content:
        if not isinstance(block, dict):
            continue
        block_type = safe_string(block.get("type")).lower()
        if block_type in IMAGE_CONTENT_BLOCK_TYPES:
            omitted_images += 1
            continue
        if block_type in IGNORED_CONTENT_BLOCK_TYPES:
            continue
        if block_type in TEXT_CONTENT_BLOCK_TYPES:
            text = block.get("text", block.get("content"))
            if isinstance(text, str):
                parts.append(text)
    return clipped("\n".join(parts)), omitted_images


def content_text(content: Any) -> str:
    return content_text_with_omissions(content)[0]


def tool_result_content(value: Any) -> tuple[Any, int]:
    blocks = [value] if isinstance(value, dict) else value
    if not isinstance(blocks, list) or not any(
        isinstance(block, dict)
        and safe_string(block.get("type")).lower()
        in STRUCTURED_CONTENT_BLOCK_TYPES
        for block in blocks
    ):
        return value, 0
    return content_text_with_omissions(blocks)


def tagged_user_request(text: str) -> str | None:
    if match := TOP_LEVEL_USER_REQUEST_RE.match(text):
        return clipped(match.group("request").strip())
    if not (
        GENERATED_WRAPPER_RE.match(text)
        or OUTER_HARNESS_RE.match(text)
    ):
        return None
    queries = OUTER_CURRENT_REQUEST_RE.findall(text)
    if not queries:
        return None
    return clipped(queries[-1].strip())


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
) -> tuple[dict[str, Any] | None, bool, int]:
    if not isinstance(payload, dict):
        return None, True, 0
    kind = safe_string(payload.get("type"))
    if kind == "message":
        role = safe_string(payload.get("role"))
        text, omitted_images = content_text_with_omissions(
            payload.get("content")
        )
        turn = inert_turn(role, text)
        return turn, role not in {"user", "assistant"}, omitted_images
    if kind == "local_shell_call":
        call = {
            "name": "local_shell",
            "arguments": json_preview(payload.get("action"), max_tool_chars),
        }
        return inert_turn("assistant", tool_calls=[call]), False, 0
    if kind in {"function_call", "custom_tool_call"}:
        call = {
            "name": safe_string(payload.get("name") or kind),
            "arguments": json_preview(
                payload.get("arguments", payload.get("input")), max_tool_chars
            ),
        }
        return inert_turn("assistant", tool_calls=[call]), False, 0
    if kind in {"function_call_output", "custom_tool_call_output"}:
        output, omitted_images = tool_result_content(payload.get("output"))
        result = {
            "call_id": safe_string(payload.get("call_id")),
            "output": historical_tool_result(output, max_tool_chars),
            "stale": "true",
        }
        return (
            inert_turn("assistant", tool_results=[result]),
            False,
            omitted_images,
        )
    return None, True, 0


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
            "text BLOB NOT NULL, "
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
                safe_string(turn.get("text")).encode(
                    "utf-8",
                    errors="surrogatepass",
                ),
                json.dumps(turn, ensure_ascii=True, separators=(",", ":")),
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
            "SELECT text FROM turns WHERE role = ? AND length(text) > 0 "
            "ORDER BY sequence DESC LIMIT 1",
            (role,),
        ).fetchone()
        if not row:
            return None
        text = row[0]
        if isinstance(text, memoryview):
            text = text.tobytes()
        return (
            text.decode("utf-8", errors="surrogatepass")
            if isinstance(text, bytes)
            else safe_string(text)
        )


def process_codex_stream(
    path: str | Path, max_tool_chars: int, turns: Any
) -> tuple[int, int, int, bool]:
    skipped = 0
    omitted_images = 0
    requires_journal = False

    def consume_record(record: dict[str, Any]) -> None:
        nonlocal omitted_images, requires_journal, skipped
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
                omitted_images = 0
                requires_journal = False
                for replacement_payload in replacement:
                    turn, unsafe, images = codex_turn(
                        replacement_payload, max_tool_chars
                    )
                    skipped += int(unsafe)
                    omitted_images += images
                    if turn:
                        turns.append(turn)
            return
        if requires_journal:
            return
        if record_type == "response_item":
            turn, unsafe, images = codex_turn(payload, max_tool_chars)
            skipped += int(unsafe)
            omitted_images += images
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
    return malformed, skipped, omitted_images, requires_journal


def read_codex(item: dict[str, Any], max_tool_chars: int) -> dict[str, Any]:
    bounded = BoundedTurns()
    malformed, skipped, omitted_images, requires_journal = process_codex_stream(
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
                malformed, skipped, omitted_images, _ = process_codex_stream(
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
    if omitted_images:
        warnings.append(image_omission_warning(omitted_images))
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
            "sequence = excluded.sequence, "
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
            "ORDER BY messages.sort_time DESC, messages.sequence DESC "
            "LIMIT 1"
        ).fetchone()
        if leaf is None:
            leaf = self.database.execute(
                "SELECT uuid FROM claude_messages "
                "ORDER BY sort_time DESC, sequence DESC LIMIT 1"
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
) -> tuple[dict[str, Any] | None, int, int]:
    if record.get("isMeta") or record.get("isCompactSummary"):
        return None, 1, 0
    role = safe_string(record.get("type"))
    if role not in {"user", "assistant"}:
        return None, 1, 0
    message = record.get("message")
    if not isinstance(message, dict):
        return None, 1, 0
    content = message.get("content")
    text, omitted_images = content_text_with_omissions(content)
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
                output, result_images = tool_result_content(
                    block.get("content")
                )
                omitted_images += result_images
                results.append(
                    {
                        "call_id": safe_string(block.get("tool_use_id")),
                        "output": historical_tool_result(
                            output, max_tool_chars
                        ),
                        "stale": "true",
                    }
                )
            elif kind in {"thinking", "redacted_thinking"}:
                skipped += 1
    return inert_turn(role, text, calls, results), skipped, omitted_images


def read_claude(item: dict[str, Any], max_tool_chars: int) -> dict[str, Any]:
    path = Path(item["path"])
    turns_from_leaf: list[dict[str, Any]] = []
    last_text: dict[str, str] = {}
    skipped = 0
    omitted_images = 0
    with tempfile.TemporaryDirectory(prefix="resume-claude-chain-") as temporary:
        try:
            with closing(
                sqlite3.connect(str(Path(temporary) / "chain.sqlite"))
            ) as database:
                index = ClaudeChainIndex(database, path)
                malformed = consume_jsonl(path, index.consume)
                skipped += index.unindexable
                for record in index.active_chain_from_leaf():
                    turn, record_skipped, record_images = claude_turn(
                        record, max_tool_chars
                    )
                    skipped += record_skipped
                    omitted_images += record_images
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
    if omitted_images:
        warnings.append(image_omission_warning(omitted_images))
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
) -> tuple[dict[str, Any] | None, int, int]:
    kind = safe_string(record.get("type")).lower()
    if record.get("synthetic_reason"):
        return None, 1, 0
    if kind in {"user", "assistant"}:
        calls: list[dict[str, str]] = []
        skipped = 0
        text, omitted_images = content_text_with_omissions(
            record.get("content")
        )
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
            inert_turn(kind, text, tool_calls=calls),
            skipped,
            omitted_images,
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
        return inert_turn("assistant", tool_calls=[call]), 0, 0
    if kind == "tool_result":
        output, omitted_images = tool_result_content(
            record.get("output", record.get("content"))
        )
        result = {
            "call_id": safe_string(
                record.get("call_id") or record.get("tool_call_id")
            ),
            "output": historical_tool_result(output, max_tool_chars),
            "stale": "true",
        }
        return (
            inert_turn("assistant", tool_results=[result]),
            0,
            omitted_images,
        )
    return None, 1, 0


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
    omitted_images = 0

    def consume_record(record: dict[str, Any]) -> None:
        nonlocal omitted_images, skipped
        turn, record_skipped, record_images = grok_turn(
            record, max_tool_chars
        )
        skipped += record_skipped
        omitted_images += record_images
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
    if omitted_images:
        warnings.append(image_omission_warning(omitted_images))
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


def cursor_project_slug(cwd: str) -> str:
    absolute = os.path.abspath(os.path.expanduser(cwd))
    return re.sub(r"[^A-Za-z0-9]+", "-", absolute).strip("-")


def cursor_project_matches_cwd(project_dir: Path, cwd: str) -> bool:
    projects = cursor_root() / "projects"
    if (
        not project_dir.is_dir()
        or project_dir.is_symlink()
        or not path_is_within(project_dir, projects)
    ):
        return False
    metadata_path = project_dir / ".workspace-trusted"
    if (
        metadata_path.is_file()
        and not metadata_path.is_symlink()
        and path_is_within(metadata_path, project_dir)
    ):
        try:
            metadata = read_json(metadata_path)
        except (OSError, ValueError, json.JSONDecodeError, RecursionError):
            metadata = None
        if isinstance(metadata, dict):
            workspace = metadata.get("workspacePath")
            if isinstance(workspace, str) and workspace.strip():
                workspace = os.path.expanduser(workspace.strip())
                if os.path.isabs(workspace):
                    return same_cwd(workspace, cwd)
    return project_dir.name.casefold() == cursor_project_slug(cwd).casefold()


def literal_path_component(value: str) -> bool:
    return (
        bool(value)
        and value not in {".", ".."}
        and "\x00" not in value
        and "/" not in value
        and "\\" not in value
    )


def cursor_transcript_for_session(
    session_id: str, cwd: str | None
) -> Path | None:
    if not literal_path_component(session_id) or not cwd:
        return None
    projects = cursor_root() / "projects"
    try:
        project_directories = list(projects.iterdir())
    except OSError:
        return None
    for project_dir in project_directories:
        if not cursor_project_matches_cwd(project_dir, cwd):
            continue
        session_dir = project_dir / "agent-transcripts" / session_id
        transcript = session_dir / f"{session_id}.jsonl"
        if (
            session_dir.is_dir()
            and not session_dir.is_symlink()
            and path_is_within(session_dir, project_dir)
            and transcript.is_file()
            and not transcript.is_symlink()
            and path_is_within(transcript, session_dir)
        ):
            return transcript
    return None


def cursor_transcript_candidate(path: Path, cwd: str) -> dict[str, Any] | None:
    transcript_root = cursor_root() / "projects"
    if (
        not path.is_file()
        or path.is_symlink()
        or not path_is_within(path, transcript_root)
    ):
        return None
    try:
        relative = Path(canonical(path)).relative_to(
            Path(canonical(transcript_root))
        )
    except ValueError:
        return None
    if len(relative.parts) < 3 or relative.parts[1] != "agent-transcripts":
        return None
    project_dir = transcript_root / relative.parts[0]
    if not cursor_project_matches_cwd(project_dir, cwd):
        return None
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
                if title is None:
                    title = cursor_first_user_title(record)
                if title:
                    break
    except OSError:
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
            paths = nested_strings(header, CURSOR_CWD_KEYS)
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
) -> int:
    omitted_images = 0
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
                nested_text = text.get("text", text.get("content"))
                if nested_text is not None:
                    text = nested_text
            if isinstance(text, (dict, list)):
                rendered, text_images = content_text_with_omissions(text)
                omitted_images += text_images
            else:
                rendered = text if isinstance(text, str) else ""
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
                    output, result_images = tool_result_content(
                        block.get("content")
                    )
                    omitted_images += result_images
                    results.append(
                        {
                            "call_id": safe_string(
                                block.get("tool_use_id") or block.get("call_id")
                            ),
                            "output": historical_tool_result(
                                output, max_tool_chars
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
            raw_output = (
                current.get("content")
                or current.get("output")
                or current.get("text")
            )
            if isinstance(raw_output, (dict, list)):
                output, result_images = tool_result_content(raw_output)
                omitted_images += result_images
            else:
                output = raw_output
            result = {
                "call_id": safe_string(
                    current.get("tool_call_id") or current.get("call_id")
                ),
                "output": historical_tool_result(
                    output, max_tool_chars
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
    return omitted_images


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
    omitted_images = 0

    def consume_value(value: Any) -> None:
        nonlocal omitted_images
        omitted_images += consume_cursor_turns(
            value, max_tool_chars, bounded.append
        )

    if path.is_file() and not path.is_symlink() and path.suffix == ".jsonl":
        transcript = path
    else:
        transcript = cursor_transcript_for_session(
            safe_string(item.get("session_id")),
            safe_string(item.get("cwd")) or None,
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
            except (ReaderError, sqlite3.Error, UnicodeError, ValueError) as exc:
                warnings.append(warning("cursor_store_error", one_line(exc, 200)))
    else:
        try:
            unavailable = 0
            bubble_prefix = "bubbleId:" + item["session_id"] + ":"
            query = (
                "SELECT value FROM cursorDiskKV "
                "WHERE key = ? OR substr(key, 1, length(?)) = ? ORDER BY key"
            )
            parameters = (
                "composerData:" + item["session_id"],
                bubble_prefix,
                bubble_prefix,
            )
            with closing(open_sqlite_readonly(path)) as db:
                for row in db.execute(query, parameters):
                    value = decode_jsonish(row[0])
                    if value is None:
                        unavailable += 1
                    else:
                        consume_value(value)
            if unavailable:
                warnings.append(
                    warning(
                        "binary_content_unavailable",
                        f"{unavailable} Cursor row(s) were binary, protobuf, "
                        "or non-JSON and were not inferred.",
                    )
                )
        except (ReaderError, sqlite3.Error, UnicodeError, ValueError) as exc:
            warnings.append(warning("cursor_store_error", one_line(exc, 200)))
    if omitted_images:
        warnings.append(image_omission_warning(omitted_images))
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


def cursor_candidate_rank(
    item: dict[str, Any],
) -> tuple[float, bool, int]:
    return (
        item.get("_sort_time", 0),
        item.get("title") != "(untitled)",
        CURSOR_SOURCE_PRIORITY.get(safe_string(item.get("source")), 0),
    )


def sort_and_dedupe(items: list[dict[str, Any]]) -> list[dict[str, Any]]:
    unique: dict[tuple[str, str], dict[str, Any]] = {}
    for item in items:
        session_id = safe_string(item.get("session_id"))
        key = (
            ("cursor", session_id.casefold())
            if item.get("tool") == "cursor" and session_id
            else (safe_string(item.get("source")), session_id)
        )
        old = unique.get(key)
        if old is None:
            unique[key] = item
            continue
        if item.get("tool") == "cursor":
            if cursor_candidate_rank(item) > cursor_candidate_rank(old):
                unique[key] = item
        elif item.get("_sort_time", 0) > old.get("_sort_time", 0):
            unique[key] = item
    return sorted(
        unique.values(),
        key=lambda item: item.get("_sort_time", 0),
        reverse=True,
    )


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
    result.add_argument("--reference", dest="reference_option")
    result.add_argument("--cwd", default=os.getcwd())
    result.add_argument("--within-min", type=int, default=0)
    result.add_argument("--max-tool-chars", type=int, default=300)
    result.add_argument("--json", action="store_true")
    return result


def terminal_safe_text(value: Any) -> str:
    pieces: list[str] = []
    for character in safe_string(value):
        if unicodedata.category(character) in {"Cc", "Cf", "Cs", "Zl", "Zp"}:
            pieces.append(
                character.encode("unicode_escape").decode("ascii")
            )
        else:
            pieces.append(character)
    return "".join(pieces)


def emit(value: Any, as_json: bool) -> None:
    if as_json:
        json.dump(value, sys.stdout, ensure_ascii=True, indent=2)
        sys.stdout.write("\n")
    else:
        if isinstance(value, list):
            for item in value:
                sys.stdout.write(
                    terminal_safe_text(
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
            if args.reference is not None and args.reference_option is not None:
                raise ReaderError(
                    "pass the session reference either positionally or with "
                    "--reference, not both"
                )
            reference = (
                args.reference_option
                if args.reference_option is not None
                else args.reference
            )
            selected = resolve(args.tool, cwd, reference, args.within_min)
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
