"""Check selected source registries against rendered documentation.

Run in `nix develop .#docs` against a fresh documentation server:
  python3 docs/scripts/verify_documentation_coverage.py --url http://127.0.0.1:4321
  python3 docs/scripts/verify_documentation_coverage.py --self-test

This checks presence of names, not instructional quality. It intentionally does
not infer tool availability or certify external services. Configuration examples
tagged data-config-schema="harness", "models" or "settings" can be exported with
--export-examples "$TMPDIR/documentation-examples" for the Haskell decoder check.
"""

import argparse
from html.parser import HTMLParser
import json
import os
from pathlib import Path
import re
import sys
import unittest
from urllib.request import urlopen


ROOT = Path(__file__).resolve().parents[2]
COMMANDS = "packages/agent-cli/src/Agent/CLI/Command/Catalog.hs"
OPTIONS = "packages/agent-cli/src/Agent/CLI/Options.hs"
CONFIGURATION = "packages/agent-runtime/src/Agent/Runtime/Config.hs"
MODELS = "packages/agent-runtime/src/Agent/Runtime/ModelConfig.hs"
SETTINGS = "packages/agent-runtime/src/Agent/Runtime/Project.hs"
TOOLS = "packages/agent-tools/src"


class Document(HTMLParser):
    def __init__(self):
        super().__init__()
        self.article = False
        self.fragments = []
        self.links = []
        self.example = None
        self.examples = []

    def handle_starttag(self, tag, attributes):
        attributes = dict(attributes)
        if tag == "article":
            self.article = True
        if tag == "a" and "href" in attributes:
            self.links.append(attributes["href"])
        if self.article and tag == "code" and "data-config-schema" in attributes:
            self.example = (attributes["data-config-schema"], [])

    def handle_endtag(self, tag):
        if tag == "article":
            self.article = False
        if tag == "code" and self.example is not None:
            schema, fragments = self.example
            self.examples.append((schema, "".join(fragments)))
            self.example = None
        if self.article and tag in {"p", "td", "th", "li", "h2", "h3"}:
            self.fragments.append("\n")

    def handle_data(self, value):
        if self.article:
            self.fragments.append(value)
        if self.example is not None:
            self.example[1].append(value)

    @property
    def text(self):
        return "".join(self.fragments)


def declaration(source, name):
    """Extract one top-level declaration; missing markers fail closed."""
    match = re.search(rf"(?m)^{re.escape(name)}\s*=\s*", source)
    if match is None:
        raise ValueError(f"Cannot locate declaration {name}; update registry extractor")
    remainder = source[match.end():]
    boundary = re.search(r"(?m)^[a-z][A-Za-z0-9_']*\s*(?:::|=)", remainder)
    return remainder[:boundary.start()] if boundary else remainder


def slash_names(source):
    body = declaration(source, "slashCommands")
    entries = re.findall(
        r'\b(?:cmd|codexCmd|grokToolCmd\s+"[^"]+")\s+"([^"]+)"\s+\[([^\]]*)\]', body)
    if not entries:
        raise ValueError("Slash command registry is empty or changed shape")
    constructors = re.findall(r'[\[,]\s*([a-zA-Z][A-Za-z0-9_]*)\s+"', body)
    if len(constructors) != len(entries):
        raise ValueError("Unrecognized slash registry entry; update registry extractor")
    return {"/" + name for name, aliases in entries
            for name in [name, *re.findall(r'"([^"]+)"', aliases)]}


def option_names(source):
    body = declaration(source, "optionUpdateParser")
    names = re.findall(
        r'\b(?:optionUpdate|flagUpdate|boolFlagUpdate|codeModeFlagUpdate|screenFlagUpdate|Options\.long)\s+"([^"]+)"',
        body,
    )
    if not names:
        raise ValueError("Run option registry is empty or changed shape")
    return {"--" + name for name in names}


def decoder_names(source):
    """Extract literal object keys from finite decoder declarations.

    The lookahead chooses the final string before a decoder expression, so
    defaultKey "stdio" "transport" is not mistaken for a `stdio` field.
    """
    sections = re.findall(
        r"(?ms)^([a-z][A-Za-z0-9_]*Decoder)\s*::[^\n]*\n"
        r"(.*?)(?=^[a-z][A-Za-z0-9_']*\s*::|\Z)", source)
    keys = set()
    for _, body in sections:
        for invocation in re.finditer(r'\b(?:defaultKey|optionalKey|Hermes\.atKey)\b', body):
            # Restrict to this application through its following decoder.
            tail = body[invocation.start():]
            match = re.match(
                r'(?:defaultKey|optionalKey|Hermes\.atKey)\s+'
                r'(?:[^\n]*?\s+)?'
                r'"([A-Za-z][A-Za-z0-9_]*)"\s+'
                r'(?:\((?:lenient\s+)?)*'
                r'(?:Hermes\.|[a-z][A-Za-z0-9_]*Decoder\b|rawJsonDecoder\b)',
                tail,
            )
            if match:
                keys.add(match[1])
            else:
                raise ValueError("Unrecognized decoder key application: " + tail[:100])
    if not keys:
        raise ValueError("Configuration decoder inventory is empty or changed shape")
    return keys


def tool_names(sources):
    """Inventory agent-tools JSON descriptors, not dynamic MCP/provider tools.

    Resolve literal names and same-module literal constants. An unsupported
    name expression fails instead of silently removing a descriptor.
    """
    names = set()
    for source in sources:
        # Imports use commas/parentheses rather than a name argument.
        for match in re.finditer(
                r'\b(?:jsonTool|jsonAppToolWithExecution)\s+(?=["a-z])', source):
            tail = source[match.end():]
            literal = re.match(r'"([a-z][a-z0-9_]*)"', tail)
            if literal:
                names.add(literal.group(1))
                continue
            identifier = re.match(r'([a-z]\w*)', tail)
            constant = re.search(
                r'^' + re.escape(identifier.group(1)) + r'\s*=\s*"([a-z][a-z0-9_]*)"\s*$',
                source, re.M) if identifier else None
            if constant is None:
                raise ValueError("Unrecognized built-in tool name: " + tail[:80])
            names.add(constant.group(1))
    if not names:
        raise ValueError("Built-in JSON descriptor inventory is empty")
    return names


def missing_names(names, text):
    return sorted(name for name in names
                  if re.search(r"(?<![\w/-])" + re.escape(name) + r"(?![\w-])", text) is None)


def fetch_document(address):
    with urlopen(address, timeout=20) as response:
        if response.status != 200:
            raise ValueError(f"Unexpected HTTP status for {address}: {response.status}")
        document = Document()
        document.feed(response.read().decode("utf-8"))
        return document


def verify(address, output):
    home = fetch_document(address + "/")
    routes = sorted({link for link in home.links
                     if link.startswith("/") and not link.startswith(("//", "/text/"))
                     and "#" not in link and link.endswith("/")})
    if not routes:
        raise ValueError("No documentation routes found")
    documents = {route: fetch_document(address + route) for route in routes}
    all_text = "\n".join(document.text for document in documents.values())
    registries = [
        ("slash commands and aliases", slash_names((ROOT / COMMANDS).read_text())),
        ("launch options", option_names((ROOT / OPTIONS).read_text())),
        ("machine configuration keys", decoder_names((ROOT / CONFIGURATION).read_text())),
        ("model catalog keys", decoder_names((ROOT / MODELS).read_text())),
        ("persisted settings keys", decoder_names((ROOT / SETTINGS).read_text())),
        ("agent-tools JSON descriptor names",
         tool_names(path.read_text() for path in (ROOT / TOOLS).rglob("*.hs"))),
    ]
    failures = []
    for label, names in registries:
        absent = missing_names(names, all_text)
        print(f"{label}: {len(names)} unique names, {len(absent)} missing")
        if absent:
            failures.append(f"{label}: {', '.join(absent)}")
    if output is not None:
        destination = output.resolve()
        temporary_root = Path(os.environ["TMPDIR"]).resolve()
        if not destination.is_relative_to(temporary_root) or destination == temporary_root:
            raise ValueError("Example export must use a subdirectory of TMPDIR")
        destination.mkdir(parents=True, exist_ok=True)
        manifest = []
        for route, document in documents.items():
            for schema, contents in document.examples:
                if schema == "illustration":
                    continue
                if schema not in {"harness", "models", "settings"}:
                    raise ValueError(f"Unknown configuration schema {schema!r} at {route}")
                json.loads(contents)
                filename = f"{len(manifest):04d}-{schema}.json"
                (destination / filename).write_text(contents)
                manifest.append({"schema": schema, "file": filename, "route": route})
        if not manifest:
            raise ValueError("No decoder-tagged examples found; refusing an empty validation")
        (destination / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
        print(f"Exported {len(manifest)} examples to {destination}")
    if failures:
        raise ValueError("\n".join(failures))
    print("Registry presence checks passed; prose and runtime behavior require separate review.")


class RegressionTests(unittest.TestCase):
    def test_tool_descriptors(self):
        names = tool_names(['jsonTool "read_file" description\n'
                            'jsonAppToolWithExecution chartName description\n'
                            'chartName = "render_chart"\n'])
        self.assertEqual(names, {"read_file", "render_chart"})
        self.assertEqual(missing_names(names, "read_file"), ["render_chart"])
        with self.assertRaises(ValueError):
            tool_names(["jsonTool unknownName description"])
        with self.assertRaises(ValueError):
            tool_names([])

    def test_new_command_requires_documentation(self):
        source = ('slashCommands = [cmd "help" ["h"] "/help" "Help" True, '
                  'grokToolCmd "scheduler_create" "loop" [] "/loop" "Repeat" True]\nnext :: Int\n')
        names = slash_names(source)
        self.assertEqual(names, {"/help", "/h", "/loop"})
        self.assertEqual(missing_names(names, "/help /h /loop"), [])
        self.assertEqual(missing_names(names | {"/new-command"}, "/help /h /loop"), ["/new-command"])

    def test_exact_option_boundary(self):
        self.assertEqual(missing_names({"--model"}, "--model-id"), ["--model"])
        self.assertEqual(missing_names({"--model"}, "Use --model NAME"), [])

    def test_decoders_and_default_strings(self):
        source = ('testDecoder :: Hermes.Decoder A\ntestDecoder = do\n'
                  ' x <- defaultKey "stdio" "transport" Hermes.text\n'
                  ' y <- defaultKey 12\n "timeout" Hermes.int\n'
                  ' z <- optionalKey "token" Hermes.text\n'
                  ' pure (x,y,z)\nnext :: Int\n')
        self.assertEqual(decoder_names(source), {"transport", "timeout", "token"})

    def test_article_excludes_navigation(self):
        document = Document()
        document.feed('<nav>/hidden</nav><article>/shown<code data-config-schema="harness">'
                      '{"theme":"midnight"}</code></article>')
        self.assertNotIn("/hidden", document.text)
        self.assertEqual(document.examples, [("harness", '{"theme":"midnight"}')])

    def test_registry_change_fails_closed(self):
        with self.assertRaises(ValueError):
            slash_names("commandsHaveMoved = []")
        with self.assertRaises(ValueError):
            slash_names('slashCommands = [cmd "help" [] "/help" "Help" True, newCmd "missing" []]')
        with self.assertRaises(ValueError):
            decoder_names("fieldDecoder :: Hermes.Decoder A\nfieldDecoder = optionalKey dynamic Hermes.text")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", default=os.environ.get("DOCUMENTATION_URL", "http://127.0.0.1:4321"))
    parser.add_argument("--export-examples", type=Path)
    parser.add_argument("--self-test", action="store_true")
    arguments = parser.parse_args()
    if arguments.self_test:
        unittest.main(argv=[sys.argv[0]])
    else:
        try:
            verify(arguments.url.rstrip("/"), arguments.export_examples)
        except (ValueError, OSError) as error:
            print(error, file=sys.stderr)
            sys.exit(1)
