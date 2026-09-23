{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Tutorials (pages) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

pages :: [Page]
pages = [fixBug, investigateRepository, parallelChanges]

fixBug :: Page
fixBug = Page
    { pagePath = "/tutorials/fix-a-bug/"
    , pageTitle = "Fix a bug with a regression test"
    , pageDescription = "Reproduce a small defect, ask for a bounded correction, and verify the resulting patch."
    , pageGroup = "Tutorials"
    , pageBody = [hsx|
        <p>This tutorial creates a disposable Python repository with one deliberately failing
        test. You will give the agent a reproducible failure rather than asking it to guess
        what is wrong. The same sequence applies to a real issue in your project.</p>
        <h2 id="prerequisites">Prerequisites</h2>
        <p>Install Nix with flakes enabled and <a href="/getting-started/installation/">Haskell Agent</a>.
        Complete <a href="/getting-started/authentication/">authentication</a> before starting.
        The fixture uses Python's standard library; it requires no Python package installation.
        Run the following commands in a shell, not in the agent composer.</p>
        <h2 id="create-fixture">1. Create an isolated fixture</h2>
        <pre><code class="language-sh">{fixture}</code></pre>
        <p>Keep this shell open: <code>$TUTORIAL_DIRECTORY</code> identifies the temporary
        checkout. The flake supplies both Python and Git, and <code>flake.lock</code> records
        the resolved dependency revision. The commit is local; nothing is published.</p>
        <h2 id="reproduce">2. Reproduce the failure</h2>
        <pre><code class="language-sh">nix develop -c python -m unittest -v</code></pre>
        <p>The command must exit unsuccessfully. The ordinary two-item test passes, but the
        empty-input test raises <code>ZeroDivisionError</code> instead of the required
        <code>ValueError</code>. If Python cannot start, fix the environment before asking
        the agent to change application code.</p>
        <h2 id="request-correction">3. Request the smallest correction</h2>
        <pre><code class="language-sh">agent-cli --cwd "$TUTORIAL_DIRECTORY"</code></pre>
        <p>Do not add <code>--worktree</code> here: you already created a disposable checkout
        with the fixture you want the agent to inspect. Submit:</p>
        <pre><code>{bugRequest}</code></pre>
        <p>Review any tool approval before accepting it. The agent should inspect the files,
        run the test, modify <code>statistics.py</code>, and rerun the test. Its exact wording
        and tool sequence depend on the selected model.</p>
        <h2 id="verify-correction">4. Verify the patch independently</h2>
        <p>In the agent, run <code>/diff</code>. Then leave with <code>/quit</code> and execute:</p>
        <pre><code class="language-sh">{"nix develop -c python -m unittest -v\nnix develop -c git diff --check\nnix develop -c git diff -- statistics.py test_statistics.py\nnix develop -c git status --short" :: Text}</code></pre>
        <p>Both tests should pass. Check that the correction explicitly rejects an empty
        list, preserves the average for nonempty input, and does not weaken or delete the
        tests. A plausible implementation is:</p>
        <pre><code>{correctedImplementation}</code></pre>
        <p>The exact exception message is not specified by this fixture. If your real
        application promises an error message, include that contract in a test too.</p>
        <h2 id="recover">5. Diagnose an incomplete result</h2>
        <table>
            <thead><tr><th>Observation</th><th>Next action</th></tr></thead>
            <tbody>
                <tr><td>Tests still fail</td><td>Give the agent the complete failing command and traceback. Ask it to explain the remaining failure before another edit.</td></tr>
                <tr><td>The test was changed to accept division by zero</td><td>Reject the change: the required contract is a ValueError for empty input. Ask it to restore the test and fix the implementation.</td></tr>
                <tr><td>No modified files appear</td><td>Use <code>/copy-path</code> to copy the active checkout path and compare it with the fixture directory.</td></tr>
                <tr><td>The patch includes unrelated changes</td><td>Ask for a narrowed patch. Review the diff before reverting anything that might be your own work.</td></tr>
            </tbody>
        </table>
        <p>For an actual project, finish with its broader test suite and normal review process.
        This two-test exercise demonstrates the workflow; it does not prove correctness for
        every numeric input or production requirement.</p>
    |]
    }

fixture :: Text
fixture = "TUTORIAL_DIRECTORY=$(mktemp -d -t agent-documentation-tutorial.XXXXXX)\n\
    \cd \"$TUTORIAL_DIRECTORY\"\n\
    \cat > flake.nix <<'EOF'\n\
    \{\n\
    \  inputs.nixpkgs.url = \"github:NixOS/nixpkgs/nixos-26.05\";\n\
    \  outputs = { self, nixpkgs }: let\n\
    \    systems = [ \"aarch64-darwin\" \"x86_64-darwin\" \"aarch64-linux\" \"x86_64-linux\" ];\n\
    \  in {\n\
    \    devShells = nixpkgs.lib.genAttrs systems (system: let\n\
    \      pkgs = import nixpkgs { inherit system; };\n\
    \    in { default = pkgs.mkShell { packages = [ pkgs.python3 pkgs.git ]; }; });\n\
    \  };\n\
    \}\n\
    \EOF\n\
    \cat > statistics.py <<'EOF'\n\
    \def average(values):\n\
    \    return sum(values) / len(values)\n\
    \EOF\n\
    \cat > test_statistics.py <<'EOF'\n\
    \import unittest\n\
    \from statistics import average\n\
    \\n\
    \class AverageTests(unittest.TestCase):\n\
    \    def test_two_values(self):\n\
    \        self.assertEqual(average([2, 4]), 3)\n\
    \\n\
    \    def test_empty_input(self):\n\
    \        with self.assertRaises(ValueError):\n\
    \            average([])\n\
    \EOF\n\
    \printf '__pycache__/\\n' > .gitignore\n\
    \nix develop -c sh -c 'git init && git add flake.nix flake.lock .gitignore statistics.py test_statistics.py'\n\
    \nix develop -c git -c user.name='Documentation Example' -c user.email='example@example.invalid' commit -m 'Add reproducible average fixture'"

bugRequest :: Text
bugRequest = "Run `nix develop -c python -m unittest -v` and reproduce the failure.\n\
    \The contract is: average([]) raises ValueError; nonempty input returns its arithmetic mean.\n\
    \Make the smallest correction in statistics.py. Do not weaken the tests, add dependencies,\n\
    \or commit changes. Rerun the tests and report the exact command and result."

correctedImplementation :: Text
correctedImplementation = "def average(values):\n\
    \    if not values:\n\
    \        raise ValueError(\"average requires at least one value\")\n\
    \    return sum(values) / len(values)"

investigateRepository :: Page
investigateRepository = Page
    { pagePath = "/tutorials/investigate-a-repository/"
    , pageTitle = "Investigate an unfamiliar repository"
    , pageDescription = "Build an evidence-based map of a codebase before approving implementation."
    , pageGroup = "Tutorials"
    , pageBody = [hsx|
        <p>This workflow produces a map of one request path, not a speculative description of
        every module. Use it when joining a project or evaluating a change to an unfamiliar subsystem.</p>
        <h2 id="prepare">1. Establish the baseline</h2>
        <p>You need a local checkout, authenticated agent, and the project's documented development
        environment. Record existing modifications before starting. If the project provides Git in
        its flake:</p>
        <pre><code class="language-sh">{"nix develop -c git status --short\nnix develop -c git rev-parse HEAD\nagent-cli --cwd /absolute/path/to/project" :: Text}</code></pre>
        <p>Replace the path with the checkout you inspected. If its flake has a named shell, use that
        shell for the Git commands. Do not create new instructions with <code>/init</code> yet:
        first understand the conventions already present in <code>AGENTS.md</code> and the README.</p>
        <h2 id="trace">2. Trace one operation</h2>
        <p>Choose an operation the project actually implements, such as session search. Substitute
        your operation in this request:</p>
        <pre><code>{"Investigate how session search works. Do not edit files or install dependencies.\nFind the user entry point, request parsing, query implementation, result ordering,\nand tests. Cite file paths and line numbers for each link in the chain.\nSeparate confirmed behavior from assumptions. List the documented test command,\nbut do not run it until you have explained its environment and side effects." :: Text}</code></pre>
        <p>Open the cited files yourself. A useful result connects specific functions and tests;
        a list of directories alone is not sufficient. Ask for missing links:</p>
        <pre><code>Show the call site that connects the command handler to the search function. Which test proves the ordering claim?</code></pre>
        <h2 id="check">3. Check the evidence</h2>
        <p>Once you understand the test command, authorize the focused check. Prefer the repository's
        established workflow; for this Haskell repository, that means GHCi in the Nix shell rather
        than rebuilding the whole application for an exploratory check.</p>
        <pre><code>Run the focused tests using the repository instructions. Report the exact command, result, and any environment failure separately from application failures. Do not change files to make the environment pass.</code></pre>
        <p>If a test requires credentials, writes to a database, or contacts a service, inspect
        that requirement before allowing it. A read-only investigation request is guidance to the
        model, not an operating-system sandbox.</p>
        <h2 id="plan">4. Turn findings into a bounded plan</h2>
        <pre><code>/plan Propose a change to session search that preserves current ordering and empty-query behavior. Identify the files to change and regression tests. Do not implement yet.</code></pre>
        <p><code>/view-plan</code> displays the saved proposal. Plan mode restricts edits to the
        session plan file; the preceding investigation was an ordinary conversation.
        Reject assumptions not supported by code or requirements. Approve implementation only
        after the proposal states observable acceptance criteria.</p>
        <h2 id="retain">5. Retain and resume the investigation</h2>
        <pre><code>{"/rename Session search investigation\n/session\n/export /absolute/path/to/private-investigation.md" :: Text}</code></pre>
        <p>Choose an actual private export path. Review the export before sharing because it may
        contain source excerpts and tool results. Use <code>/quit</code> to leave without deleting
        the conversation; later use <code>agent-cli --resume SESSION_ID</code>.</p>
        <p>Recheck Git status at the end. Explain any difference from the baseline before treating
        the investigation as read-only. If a citation does not exist, ask the agent to reread the
        file and correct its report rather than carrying the assumption into implementation.</p>
    |]
    }

parallelChanges :: Page
parallelChanges = Page
    { pagePath = "/tutorials/parallel-changes/"
    , pageTitle = "Review and integrate parallel changes"
    , pageDescription = "Assign independent changes, inspect each checkout, and integrate explicitly."
    , pageGroup = "Tutorials"
    , pageBody = [hsx|
        <p>Parallel agents are useful when work has separate ownership boundaries. This tutorial
        uses one documentation change and one test change. Do not split two mutually dependent
        implementation edits merely to increase concurrency.</p>
        <h2 id="baseline">1. Choose a common baseline</h2>
        <p>Start from a clean Git repository with a known base commit and an authenticated agent.
        Record the base and status using the project's Nix development environment:</p>
        <pre><code class="language-sh">{"nix develop -c git status --short\nnix develop -c git rev-parse HEAD\nagent-cli" :: Text}</code></pre>
        <p>Managed worktrees normally start from the selected remote's latest default commit,
        not your uncommitted files or necessarily your current branch. Read the
        <a href="/guides/parallel-agents/">worktree policy</a> before relying on another checkout
        containing local changes. If the tasks require an unpublished change, agree on how to
        make that baseline available before delegating.</p>
        <h2 id="assign">2. Assign bounded ownership</h2>
        <pre><code>/agents limit 2</code></pre>
        <pre><code>{"Use separate worktree agents for two tasks. Agent one may change documentation\nfor the empty-input behavior only. Agent two may add regression tests only.\nNeither may modify the implementation or publish changes. Before editing, have\neach report its checkout path and starting commit. Report any baseline mismatch.\nWhen finished, each must report changed files, the exact checks run, and its diff.\nDo not automatically merge or remove either checkout." :: Text}</code></pre>
        <p>Replace “empty-input behavior” with the established contract in your project.
        Use <code>/agents</code> to inspect progress. If both agents need the same file, stop
        and reassign ownership rather than hoping their edits will combine safely.</p>
        <h2 id="inspect">3. Inspect each result before integration</h2>
        <p>Obtain the actual checkout path from each report. In a second shell, substitute it
        below. Run the commands from your main project's environment:</p>
        <pre><code class="language-sh">{"DOCUMENTATION_CHECKOUT=/absolute/path/reported/by/documentation-agent\nnix develop -c git -C \"$DOCUMENTATION_CHECKOUT\" status --short\nnix develop -c git -C \"$DOCUMENTATION_CHECKOUT\" diff --check\nnix develop -c git -C \"$DOCUMENTATION_CHECKOUT\" diff HEAD\nnix develop -c git -C \"$DOCUMENTATION_CHECKOUT\" log -3 --oneline" :: Text}</code></pre>
        <p>Repeat for the test checkout. <code>git diff HEAD</code> includes staged and unstaged
        tracked changes, but not untracked file contents. Read new files listed by
        <code>git status</code> separately. If the agent committed changes, inspect the reported
        commit with <code>git show COMMIT_ID</code>; an empty working-tree diff does not mean no work occurred.</p>
        <h2 id="integrate">4. Integrate only reviewed changes</h2>
        <p>Choose one integration method explicitly. One straightforward method is to ask the
        coordinating agent to create a separate local commit for each reviewed change, report
        the identifiers, and leave the checkouts intact. Review those commits before cherry-picking:</p>
        <pre><code class="language-sh">{"nix develop -c git status --short\nnix develop -c git show DOCUMENTATION_COMMIT\nnix develop -c git show TEST_COMMIT\nnix develop -c git cherry-pick DOCUMENTATION_COMMIT\nnix develop -c git cherry-pick TEST_COMMIT" :: Text}</code></pre>
        <p>Replace the uppercase identifiers with the actual reviewed commit hashes.
        Start with a clean integration checkout. If a conflict occurs, inspect it; do not
        automatically prefer either side. <code>git cherry-pick --abort</code> abandons the
        currently active cherry-pick if you decide not to resolve it. It does not undo an
        earlier successful cherry-pick.</p>
        <h2 id="validate">5. Validate the combined result</h2>
        <p>Run the project's focused tests and broader checks in the integration checkout,
        even if each agent reported passing tests. Inspect the combined diff against the
        baseline commit you recorded:</p>
        <pre><code class="language-sh">{"nix develop -c git diff --check BASE_COMMIT HEAD\nnix develop -c git diff --stat BASE_COMMIT HEAD\nnix develop -c git diff BASE_COMMIT HEAD" :: Text}</code></pre>
        <p>Replace <code>BASE_COMMIT</code> with that actual hash. Confirm that both changes
        are present and that neither introduced unrelated edits. Ask for a final report
        distinguishing checks run in individual worktrees from checks run after integration.</p>
        <h2 id="retain">6. Keep recovery paths until review finishes</h2>
        <p>Do not remove a worker checkout while it contains the only copy of uncommitted work.
        Use <code>agent-cli worktree protect /absolute/path/to/managed/worktree</code> to retain
        a managed checkout during external review. Preview collection with
        <code>agent-cli worktree gc --dry-run</code>. Local commits and saved conversations
        are not off-machine backups; publishing remains a separate, explicitly authorized action.</p>
    |]
    }
