{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Projects (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/projects/"
    , pageTitle = "Project instructions and planning"
    , pageDescription = "Give the agent repository conventions and agree on a plan before implementation."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <h2 id="start-in-the-correct-directory">Start in the correct directory</h2>
        <pre><code class="language-sh">agent-cli --cwd /absolute/path/to/project</code></pre>
        <p>Tools operate relative to the session's working directory unless a tool call
        specifies another allowed location. Confirm the directory before requesting
        edits, especially when working with multiple checkouts.</p>
        <h2 id="add-project-instructions">Add project instructions</h2>
        <p>Put shared contributor guidance in <code>AGENTS.md</code>. The agent discovers and injects
        these instructions by default. <code>/init</code> asks it to create a contributor guide
        for the repository; review that file before relying on it.</p>
        <p>Useful instructions include:</p>
        <pre><code>{"# Development\n\n- Use the checked-in development environment.\n- Run the focused test suite for the module being changed.\n- Keep public API changes separate from internal refactoring.\n\n# Review\n\n- Explain any test failures that remain.\n- Do not publish a release without explicit approval." :: Text}</code></pre>
        <p>Instructions should state conventions that are not obvious from the code.
        Keep credentials and temporary task status out of this file.</p>
        <p>An <code>AGENTS.override.md</code> takes precedence over <code>AGENTS.md</code> in the same directory.
        You can disable discovery for a particular launch with <code>--no-agents-md</code>.
        Instructions guide the model; they are not an operating-system security boundary.</p>
        <h2 id="instruction-discovery">Discovery order and directory scope</h2>
        <p>The harness finds the nearest ancestor containing a <code>.git</code> marker, then
        loads project instructions from that root down to the session's working directory.
        Both a Git directory and a worktree's <code>.git</code> file count as markers.
        Without a root marker, only the current directory is searched for project instructions.
        Global instructions are loaded before project files.</p>
        <p>This is an ancestor-chain search, not a recursive scan of the repository. Starting
        in the repository root does not preload every package's instructions. Before changing
        a deeper directory, have the agent inspect its additional instruction files.
        State local exceptions explicitly; deeper instructions are intended to specialize
        the broader project guidance rather than repeat it.</p>
        <table><thead><tr><th>Dialect instruction home</th><th>Global order</th><th>Project discovery</th></tr></thead><tbody>
            <tr><td>Codex</td><td><code>~/.codex</code>, then <code>~/.haskell-agent</code></td><td>One preferred AGENTS file per directory</td></tr>
            <tr><td>Grok Build</td><td><code>~/.grok</code>, <code>~/.claude</code>, <code>~/.cursor</code>, then <code>~/.haskell-agent</code></td><td>AGENTS files plus compatibility names and rules directories</td></tr>
            <tr><td>Claude compatibility home</td><td><code>~/.claude</code>, then <code>~/.haskell-agent</code></td><td>Broader compatibility discovery</td></tr>
            <tr><td>Harness home</td><td><code>~/.haskell-agent</code> only</td><td>Broader compatibility discovery; unrelated vendor homes are not scanned</td></tr>
        </tbody></table>
        <p>The model's dialect selects this behavior; an endpoint being “OpenAI-compatible”
        does not by itself guarantee Codex instruction discovery. For portable guidance
        across dialects, prefer <code>~/.haskell-agent/AGENTS.md</code> and repository
        <code>AGENTS.md</code> files.</p>
        <h2 id="compatibility-files">Compatibility filenames and rules</h2>
        <p>Broader discovery reads the preferred AGENTS file, followed by these names in order:</p>
        <pre><code>{"Agents.md\nClaude.md\nCLAUDE.md\nCLAUDE.local.md\nAGENT.md\n.claude/CLAUDE.md\n.claude/CLAUDE.local.md" :: Text}</code></pre>
        <p>It then reads immediate Markdown files in <code>.grok/rules</code>,
        <code>.claude/rules</code>, and <code>.cursor/rules</code>, in that directory order.
        Entries within each rules directory are sorted by filename. Global compatibility
        homes use their immediate <code>rules</code> directory after their named files.</p>
        <p>Rules discovery accepts files whose extension is <code>.md</code> (case-insensitive).
        It does not recursively descend into rules subdirectories and does not load
        <code>.mdc</code> files through this mechanism. Matching files are loaded as text;
        do not assume another tool's front-matter path filters are evaluated here.
        Duplicate paths resolving to the same file are deduplicated in broader discovery,
        keeping the first occurrence.</p>
        <h2 id="override-and-budget">Overrides, empty files, and the size budget</h2>
        <p>A readable, non-empty <code>AGENTS.override.md</code> replaces
        <code>AGENTS.md</code> in its own directory. Under broader discovery it also suppresses
        the alternate <code>Agents.md</code> spelling, but does not suppress Claude files or
        rules directories. It does not replace instructions in every ancestor directory.</p>
        <p>Whitespace-only files are skipped. An empty or unreadable override does not
        suppress a usable base file; unreadable existing files produce discovery warnings.
        Files are combined under a <strong>32 KiB UTF-8 content budget</strong> across global
        and project instructions. Earlier files consume the budget first. The final fitting
        file may be truncated and later files omitted; multibyte characters count by encoded
        bytes, not by character count.</p>
        <p>Keep global guidance especially short so it cannot crowd out repository-specific
        instructions. Put lengthy procedures in a <a href="/customization/skills/">skill</a>
        or a referenced document that the agent can read when needed. Referencing a file in
        prose is not an automatic include directive.</p>
        <h2 id="nested-instructions-example">Example: specialize a package's test instructions</h2>
        <p>Suppose your checkout contains:</p>
        <pre><code>{"project/\n  .git/\n  AGENTS.md\n  packages/\n    parser/\n      AGENTS.md\n      src/" :: Text}</code></pre>
        <p>Keep repository-wide review and environment rules in <code>project/AGENTS.md</code>.
        In <code>packages/parser/AGENTS.md</code>, state the actual package-specific test command,
        for example in a Nix/Cabal project whose package is named <code>parser</code>:</p>
        <pre><code>{"# Parser package\n\n- Run `nix develop -c cabal test parser` after changing parser behavior.\n- Add a regression case for every accepted or rejected syntax change.\n- Preserve the public parse-error format unless the task explicitly changes it." :: Text}</code></pre>
        <pre><code class="language-sh">agent-cli --cwd /absolute/path/to/project/packages/parser</code></pre>
        <p>On a fresh launch, the root file is loaded before the parser file. Ask:
        “Before editing, summarize the repository and parser-package instructions that apply,
        and identify the test command.” Expect both scopes, with the package's specific
        testing requirement. If you instead start at the root and later request parser work,
        explicitly ask the agent to inspect <code>packages/parser/AGENTS.md</code> first.</p>
        <h2 id="instruction-troubleshooting">When instructions appear to be missing</h2>
        <ol>
            <li>Verify the actual checkout and working directory with <code>/copy-path</code>.
            Another worktree does not automatically contain uncommitted instruction edits.</li>
            <li>Check for a closer <code>.git</code> marker that changes the ancestor search boundary.</li>
            <li>Check the active dialect and filename. A Claude compatibility file is not
            part of Codex's narrow project search.</li>
            <li>Inspect <code>AGENTS.override.md</code>, empty files, permissions, and startup
            discovery warnings before assuming the base file was loaded.</li>
            <li>Check total instruction size. Move long global procedures out of the
            automatic instruction payload so deeper files fit within 32 KiB.</li>
            <li>Confirm launch did not use <code>--no-agents-md</code>. After editing instruction
            files, verify with a fresh launch or explicitly ask the current agent to reread
            the changed files; do not assume every existing conversation hot-reloads them.</li>
        </ol>
        <h2 id="plan-before-making-a-broad-change">Plan before making a broad change</h2>
        <pre><code>/plan Replace the session search implementation while preserving its behavior.</code></pre>
        <p>Plan mode permits exploration but restricts edits to the session's <code>plan.md</code>.
        Review the proposed approach, request corrections if necessary, and approve it
        before implementation. <code>/view-plan</code> displays the saved plan.</p>
        <p>For small, well-defined work, a direct request may be enough. For uncertain
        architecture or a cross-cutting change, agree on scope and acceptance checks
        before authorizing edits.</p>
        <h2 id="review-the-result">Review the result</h2>
        <p>For the search replacement example, ask the plan to identify the existing
        behavior, representative queries, and compatibility checks. Before approval,
        you should be able to answer which files will change and what evidence will
        show that result ordering and empty-query behavior remain correct.</p>
        <pre><code>Revise the plan to include regression tests for an empty query and multiple matches. Do not implement yet.</code></pre>
        <p><strong>Expected result:</strong> a revised plan, not implementation edits.
        If the proposal assumes requirements you have not agreed on, correct the
        assumptions before approving it.</p>
        <p>Ask the agent to explain what changed and which checks it actually ran. Then:</p>
        <pre><code>{"/diff\n/review" :: Text}</code></pre>
        <p>Inspect both the diff and test output. A model-generated explanation does not
        replace tests or review.</p>
        <p>Use a <a href="/guides/parallel-agents/">managed worktree</a> when the task should not
        modify your existing checkout.</p>
    |]
    }
