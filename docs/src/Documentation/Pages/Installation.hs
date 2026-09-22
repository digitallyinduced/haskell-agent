{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Installation (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/getting-started/installation/"
    , pageTitle = "Installation and first task"
    , pageDescription = "Install, connect a model, investigate a project, make a small change, and review your first session."
    , pageGroup = "Start here"
    , pageBody = [hsx|
        <h2 id="requirements">Requirements</h2>
        <p>The current pinned Nix packages support Linux on x86-64 and ARM64, and macOS
        on Apple silicon. Although the flake declares an Intel macOS output, its current
        nixpkgs pin no longer supports that platform. You need:</p>
        <ul>
            <li>Nix with flakes enabled.</li>
            <li>A terminal and a project directory.</li>
            <li>A supported provider account, API key, or compatible model endpoint.</li>
            <li>Git when using managed worktrees.</li>
        </ul>
        <p>You do not need to install GHC to use the packaged agent. Its optional persistent
        GHCi tool requires a separate GHC environment.</p>
        <h2 id="try-without-installing">Try without installing</h2>
        <p>Run the agent from the directory you want it to work in:</p>
        <pre><code class="language-sh">nix run --accept-flake-config github:digitallyinduced/haskell-agent</code></pre>
        <p><code>--accept-flake-config</code> accepts the repository's declared binary-cache settings.
        The first invocation may download the runtime and dependencies.</p>
        <h2 id="install-in-your-nix-profile">Install in your Nix profile</h2>
        <pre><code class="language-sh">{"nix profile add --accept-flake-config github:digitallyinduced/haskell-agent\nagent-cli --version" :: Text}</code></pre>
        <p>Verify the bundled PostgreSQL runtime, which provides local durable storage:</p>
        <pre><code class="language-sh">{"agent-cli storage start\nagent-cli storage doctor\nagent-cli storage stop" :: Text}</code></pre>
        <p>Run this check before opening agent sessions; do not stop storage while other
        sessions are using it.</p>
        <h2 id="open-a-project">1. Open a project</h2>
        <p>The remaining steps use the installed <code>agent-cli</code>. Replace the example
        path with an existing project you are allowed to inspect. In a Git project, check
        <code>git status</code> first so you can distinguish existing work from new changes.</p>
        <pre><code class="language-sh">agent-cli --cwd /absolute/path/to/project</code></pre>
        <p>Without a profile installation, use the same options after the flake separator:</p>
        <pre><code class="language-sh">nix run --accept-flake-config github:digitallyinduced/haskell-agent -- --cwd /absolute/path/to/project</code></pre>
        <p><strong>Expected result:</strong> an interactive session opens. Commands beginning
        with <code>/</code> below belong in its prompt, not in your shell.</p>
        <h2 id="connect-a-model">2. Connect a model</h2>
        <p>Open <code>/login</code> and connect a supported provider account. Then use
        <code>/model</code> to select a model available to that account. Provider-specific
        credentials and billing are explained in <a href="/getting-started/authentication/">Authentication</a>.</p>
        <pre><code>{"/login\n/model\n/session-info" :: Text}</code></pre>
        <p>Enter these commands one at a time, completing each interface before continuing.
        <strong>Expected result:</strong> <code>/session-info</code> reports the active model and
        session details. Never paste an API key into the ordinary conversation.</p>
        <h2 id="inspect-before-editing">3. Inspect before editing</h2>
        <p>Submit this prompt:</p>
        <pre><code>{"Read the project instructions and explain the directory structure.\nIdentify the normal test command. Do not modify files." :: Text}</code></pre>
        <p>The agent streams its response and displays tool activity. Review any approval
        request before permitting the proposed action. Read the actual operation, not
        just the explanation; deny a request that does not fit your task.</p>
        <p><strong>Expected result:</strong> a summary naming relevant files and a test command
        supported by the repository, or an explicit explanation that the command could
        not be established. Asking for no edits is an instruction to the model, not an
        operating-system read-only boundary.</p>
        <p>If the repository has an <code>AGENTS.md</code>, its guidance is discovered by default.
        If it does not, <code>/init</code> can ask the agent to create one. That is an optional
        write operation: review the generated guide before relying on it.
        See <a href="/guides/projects/">project instructions and planning</a>.</p>
        <h2 id="make-a-small-change">4. Make a small change</h2>
        <p>A documentation improvement is a useful first task because its scope is easy
        to inspect. After confirming the test command above, submit:</p>
        <pre><code>{"Update README.md with a short section explaining how to run the existing tests.\nUse only commands supported by this repository. Do not change application code.\nRun the documentation checks if the project defines them, and report what you ran.\nDo not commit or push." :: Text}</code></pre>
        <p>If your project uses a different documentation file, substitute that path.
        <strong>Expected result:</strong> a focused documentation change, a summary of the
        evidence for each command, and check results or a clear statement that checks
        could not run. For a larger change, start with <code>/plan</code> and approve the
        proposal before implementation.</p>
        <h2 id="review-your-work">5. Review and correct the result</h2>
        <pre><code>/diff</code></pre>
        <p>Inspect every changed file. The diff can include work that existed before the
        session, so compare it with your initial <code>git status</code>. Ask for a correction
        if the change exceeds the agreed scope:</p>
        <pre><code>{"Keep the README test instructions, but remove changes unrelated to that task.\nPreserve all edits that were already present before this session.\nExplain the remaining diff and any checks you could not run." :: Text}</code></pre>
        <p>Review the resulting diff again. Neither a successful response nor a generated
        review proves the change is correct. Commit only after your own review.
        Conversation commands such as <code>/clear</code> do not undo filesystem changes.</p>
        <h2 id="return-to-your-session">6. Leave and return</h2>
        <p>Give this session a recognizable name, then leave it:</p>
        <pre><code>{"/rename First project walkthrough\n/quit" :: Text}</code></pre>
        <p>Start <code>agent-cli</code> again and enter <code>/resume</code> to choose the saved
        conversation. <strong>Expected result:</strong> you can continue the earlier task
        without copying its prompts into a new session. Session persistence does not
        back up your project files. See <a href="/guides/sessions/">Sessions</a> for export
        and recovery options.</p>
        <h2 id="first-session-problems">If a step does not work</h2>
        <ul>
            <li><strong>The program does not start:</strong> check Nix installation and the
            storage diagnostics above before changing provider settings.</li>
            <li><strong>A model request is rejected:</strong> inspect <code>/login</code>,
            <code>/model</code>, and <code>/usage</code>; account access and billing vary by provider.</li>
            <li><strong>A tool is blocked:</strong> read the approval or sandbox explanation.
            Do not disable protections merely to complete the tutorial.</li>
            <li><strong>The agent selected the wrong files:</strong> stop requesting edits,
            clarify the project path and scope, and review the diff before continuing.</li>
        </ul>
        <p>See <a href="/troubleshooting/">Troubleshooting</a> for diagnostics and
        <a href="/security/approvals/">Approvals and sandboxing</a> for execution boundaries.</p>
        <h2 id="run-one-task-and-exit">Run one task and exit</h2>
        <pre><code class="language-sh">{"agent-cli --cwd /absolute/path/to/project \\\n  -p \"Explain this project's entry points without modifying files.\"" :: Text}</code></pre>
        <p>Add <code>--save-session</code> if you want a one-shot run persisted as a session.
        Non-interactive runs cannot rely on an interactive approval prompt; see
        <a href="/security/approvals/">approvals</a> before automating writes.</p>
        <h2 id="update">Update</h2>
        <pre><code class="language-sh">nix profile upgrade --refresh --accept-flake-config haskell-agent</code></pre>
        <p><code>nix profile add</code> does not upgrade an existing profile entry. If the entry has
        a different name, inspect <code>nix profile list</code> and use that name for the upgrade.</p>
        <p>Apple silicon Macs running macOS 14 or newer also have a standalone release
        archive. See the repository's
        <a href="https://github.com/digitallyinduced/haskell-agent#macos-without-nix">release installation instructions</a>
        for that separate distribution.</p>
    |]
    }
