{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Introduction (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/"
    , pageTitle = "Haskell Agent documentation"
    , pageDescription = "Start working with an independent agent harness for coding and digital work."
    , pageGroup = "Start here"
    , pageBody = [hsx|
        <p>Haskell Agent is an independent agent harness that connects language models to
        your projects, tools, and ongoing work. Use it to investigate a codebase,
        implement changes, run checks, and return to a task later without starting over.</p>
        <p>You choose the model and provider. The harness manages the conversation,
        tool execution, approvals, saved sessions, and parallel agents.</p>
        <h2 id="start-here">Start here</h2>
        <ol>
            <li><strong><a href="/getting-started/installation/">Install Haskell Agent</a></strong> and open a project.</li>
            <li><strong><a href="/getting-started/authentication/">Connect a provider</a></strong> using a supported account or API key.</li>
            <li><strong><a href="/guides/terminal/">Use the terminal</a></strong> to give instructions, inspect progress, and review changes.</li>
        </ol>
        <pre><code class="language-sh">nix run --accept-flake-config github:digitallyinduced/haskell-agent</code></pre>
        <p>Run that command from a project directory with Nix flakes enabled. It opens
        the terminal client without adding an entry to your Nix profile.
        The <a href="/getting-started/installation/">first-session walkthrough</a> takes you
        from installation through authentication, a small edit, review, and resuming
        saved work.</p>
        <h2 id="your-first-session">Your first session</h2>
        <p>Inside the agent, enter <code>/login</code> to connect an account, then
        <code>/model</code> to choose a model. Complete each interface before continuing.
        Use <code>/session-info</code> to inspect the selected model and current session.</p>
        <p>Begin with investigation rather than an open-ended request to change the
        project. Ask the agent to identify evidence, not just offer an explanation:</p>
        <pre><code>{"Read this project's instructions and README. Identify its entry points\nand the supported test command, with file references. Do not modify files." :: Text}</code></pre>
        <p>You should receive a repository-specific explanation and see the tool activity
        used to establish it. If the answer names the wrong directory or invents a test
        command, correct that before asking for edits. Review any tool approval request
        against the task you actually gave.</p>
        <h2 id="work-on-a-project">Work on a project</h2>
        <p>Once the agent has identified the test command, give it a bounded change:</p>
        <pre><code>{"Add a short README section describing the existing test command.\nDo not change application code. Run any applicable documentation checks,\nreport their results, and do not commit or push." :: Text}</code></pre>
        <p>Use <code>/diff</code> to inspect the result. Ask for a correction if unrelated
        files changed, and review check output yourself before committing.
        <code>/quit</code> leaves the session; <code>/resume</code> lets you return later.
        Use
        <a href="/guides/projects/">project instructions and plans</a> to establish conventions,
        and <a href="/guides/parallel-agents/">isolated worktrees and parallel agents</a> when work
        needs to be separated.</p>
        <h2 id="make-it-your-own">Make it your own</h2>
        <p>For a complete worked task, start with a tutorial:</p>
        <ul>
            <li><a href="/tutorials/fix-a-bug/">Fix a bug</a>: reproduce a failing test, make a bounded correction, and review the result.</li>
            <li><a href="/tutorials/investigate-a-repository/">Investigate a repository</a>: trace an entry point to its tests and produce an evidence-backed report.</li>
            <li><a href="/tutorials/parallel-changes/">Integrate parallel changes</a>: assign independent work, inspect each result, and validate the combined changes.</li>
            <li><a href="/tutorials/local-model/">Connect a local model</a>: start local inference, register its model, and test generation and tool use.</li>
            <li><a href="/tutorials/connect-mcp/">Connect an MCP server</a>: configure a connection, authenticate, and verify a read-only operation.</li>
        </ul>
        <table>
            <thead><tr><th>Task</th><th>Guide</th></tr></thead>
            <tbody>
                <tr><td>Choose a provider or connect a local model</td><td><a href="/customization/models/">Models</a></td></tr>
                <tr><td>Resume, search, or export previous work</td><td><a href="/guides/sessions/">Sessions</a></td></tr>
                <tr><td>Reuse a documented workflow</td><td><a href="/customization/skills/">Skills</a></td></tr>
                <tr><td>Connect external tools and services</td><td><a href="/customization/mcp/">MCP integrations</a></td></tr>
                <tr><td>Configure code intelligence</td><td><a href="/customization/language-servers/">Language servers</a></td></tr>
                <tr><td>Fetch documentation with domain restrictions</td><td><a href="/customization/web-access/">Web access</a></td></tr>
                <tr><td>Maintain reusable lessons across sessions</td><td><a href="/customization/learned-skills/">Learned skills</a></td></tr>
                <tr><td>Work through a Telegram bot</td><td><a href="/guides/telegram/">Telegram</a></td></tr>
                <tr><td>Dictate a prompt</td><td><a href="/guides/voice/">Voice input</a></td></tr>
                <tr><td>Control tool execution</td><td><a href="/security/approvals/">Approvals and sandboxing</a></td></tr>
                <tr><td>Find a command</td><td><a href="/reference/commands/">Command reference</a></td></tr>
                <tr><td>Diagnose a failure</td><td><a href="/troubleshooting/">Troubleshooting</a></td></tr>
            </tbody>
        </table>
        <h2 id="about-these-guides">About these guides</h2>
        <p>These guides cover the terminal client and the workflows available in this
        repository. Capabilities vary by model, provider, platform, and configured
        tools; a command's presence does not imply every provider supports every tool.
        Use <code>/help</code> inside a session for the current command list.</p>
    |]
    }
