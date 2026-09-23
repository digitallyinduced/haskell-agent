{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Approvals (page) where

import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/security/approvals/"
    , pageTitle = "Approvals and sandboxing"
    , pageDescription = "Understand tool authorization, plan-mode limits, computer-use consent, and shell escalation."
    , pageGroup = "Security"
    , pageBody = [hsx|
        <p>Tools can modify files, execute programs, and contact external services.
        Review the requested operation and its scope before approving it. A successful
        model response is not evidence that an operation is safe.</p>
        <h2 id="choose-the-approval-policy">Choose the approval policy</h2>
        <pre><code>/permissions</code></pre>
        <table><thead><tr><th>Choice</th><th>Effect</th><th>Scope</th></tr></thead><tbody>
            <tr><td>Ask before changes (<code>p</code>)</td><td>Prompt before mutating tools</td><td>Interactive policy; ordinary read-only tools do not need this mutation prompt</td></tr>
            <tr><td>Read-only for this session (<code>r</code>)</td><td>Block mutating tools instead of asking</td><td>Current session</td></tr>
            <tr><td>Full access for this project (<code>f</code>)</td><td>Automatically approve ordinary mutating tools</td><td>Project setting, not just the next call</td></tr>
        </tbody></table>
        <p>Use the arrow keys and Enter, or the listed shortcut. Escape cancels the selection.
        Project auto-approval is saved in <code>&lt;project&gt;/.haskell-agent/settings.json</code>.
        Check this policy when returning to a project: an earlier full-access selection may
        still apply. Explicit launch flags can override the inherited choice.</p>
        <p>This opens the policy selector. <code>/always-approve</code> toggles project auto-approval.
        At launch, <code>--yolo</code> auto-approves ordinary tool calls:</p>
        <pre><code class="language-sh">agent-cli --yolo</code></pre>
        <p>Use this only when you intentionally accept the consequences of unattended
        execution. It is not necessary for normal interactive use.</p>
        <p>Without a TTY, <code>--no-yolo</code> denies mutating tools instead of waiting for an
        approval that nobody can answer. Do not solve an unexpected denial by
        automatically enabling broader permissions.</p>
        <h2 id="approval-decisions">Understand an individual approval card</h2>
        <table><thead><tr><th>Decision</th><th>What it authorizes</th></tr></thead><tbody>
            <tr><td>Allow once</td><td>This invocation only</td></tr>
            <tr><td>Always approve all tools for this project</td><td>Persistent project auto-approval; considerably broader than the displayed call</td></tr>
            <tr><td>Always allow this tool this session</td><td>Later calls to that tool in the session; not necessarily the same arguments or target</td></tr>
            <tr><td>Deny</td><td>Do not execute this invocation</td></tr>
        </tbody></table>
        <p>Some sensitive operations use a one-time approval rather than offering reusable
        permission. Read the actual card. A shell tool allowance is especially broad because
        later shell invocations can execute different programs.</p>
        <h2 id="policy-scenarios">Choose the narrowest policy for the task</h2>
        <ol>
            <li><strong>Repository investigation:</strong> select read-only, ask for file references,
            and expect an explanation without implementation edits. A test runner may be denied
            because it executes commands or writes build output; read-only does not mean every
            program named “test” is allowed.</li>
            <li><strong>One proposed edit:</strong> select ask-before-changes, inspect the patch paths,
            and allow once. Inspect <code>/diff</code> afterwards. Authorizing a patch does not
            authorize a later push or deployment.</li>
            <li><strong>Trusted unattended work:</strong> deliberately choose full access only after
            checking the checkout and external connections. It removes ordinary mutation prompts,
            not every safety or operating-system boundary.</li>
        </ol>
        <p>Tool visibility, mutation approval, plan mode, filesystem isolation, and macOS privacy
        permissions answer different questions. Changing one does not necessarily fix a denial
        from another. MCP read-only classification also depends on the server's declaration:
        connect only servers whose implementation you trust.</p>
        <h2 id="planning-is-a-separate-restriction">Planning is a separate restriction</h2>
        <p><a href="/guides/projects/">Plan mode</a> permits exploration and writing the session
        plan, not arbitrary implementation changes. Approving a plan transitions to
        implementation; an ordinary auto-approval setting does not remove plan-mode
        restrictions.</p>
        <h2 id="shell-isolation-and-escalation">Shell isolation and escalation</h2>
        <p>Shell commands normally retain the harness's configured sandbox behavior.
        An approval decision and operating-system isolation are distinct controls.
        Their behavior can differ by platform and host.</p>
        <p>On macOS, the Seatbelt wrapper can conflict with a tool that creates its own
        sandbox, such as a Swift Package Manager manifest evaluation. An agent may
        request a separately justified <code>require_escalated</code> invocation for that exact
        command.</p>
        <p>Shell escalation requires fresh confirmation unless full access (<code>--yolo</code>)
        authorizes it automatically. The authorization applies to that invocation;
        ordinary commands still use their default sandbox behavior. Removing the
        harness wrapper is not root access and does not bypass macOS privacy
        permissions.</p>
        <p>Do not approve an unexplained retry outside isolation. A build command can
        execute project-controlled scripts and spawn child processes.</p>
        <h2 id="computer-use">Computer use</h2>
        <p>Supported interactive OpenAI sessions on Linux and macOS can expose local
        desktop control. Disable it at startup if you do not need it:</p>
        <pre><code class="language-sh">agent-cli --no-computer-use</code></pre>
        <p>Use <code>/computer-use on</code> or <code>/computer-use off</code> during a session.
        Computer use requests separate approval even under <code>--yolo</code>. A session-wide
        workflow allowance is available, but provider safety checks still require
        fresh approval. Turning the capability off or back on clears the workflow
        allowance.</p>
        <p>On macOS, the terminal application also needs Screen Recording and
        Accessibility permissions in System Settings.</p>
        <h2 id="example-review-an-approval">Example: assess a requested command</h2>
        <p>If a test command requests escalation after a sandbox failure, compare the
        proposed command with the failed invocation. Check its working directory,
        arguments, and explanation. Approve only if you intend to let that specific
        command execute with the stated access.</p>
        <p>If the request also adds an installation, upload, or unrelated cleanup,
        reject it and ask for a narrower command. <strong>Expected result:</strong>
        either the approved check runs and reports its actual result, or the agent
        explains what remains unverified. Approval is not evidence that a test passed.</p>
        <h2 id="secrets-and-stored-data">Secrets and stored data</h2>
        <p>Use masked secret prompts when available. The <code>ask_secret</code> tool gives the model
        a private temporary-file path rather than the entered secret; the temporary
        file is removed when the tool runtime closes. This does not make arbitrary
        subsequent commands safe to run with that file.</p>
        <p>Conversations, exports, tool output, and local configuration can contain
        private data. Review them before sharing. Connecting a hosted model or MCP
        service can send the relevant request data to that service.</p>
        <h2 id="execution-boundaries">Read the execution boundary before escalating</h2>
        <p>Approval, filesystem access, shell isolation and tool availability are separate checks.
        Broad approval does not guarantee a tool exists or bypass a hard denial.
        See <a href="/reference/tool-execution/#filesystem-and-classification">filesystem roots,
        symlink checks, scratch paths and conservative shell/GHCi classification</a> for the
        concrete rules and recovery steps. Native host grants and computer-use consent remain
        separate from a model's request to execute a tool.</p>
        <p>For connected mailbox operations, use the
        <a href="/reference/tool-execution/#email-mutations">draft, reply and send approval
        checklist</a>. An uncertain send requires Sent-mailbox reconciliation, not an automatic
        retry; approval to read a message does not authorize its embedded instructions.</p>
    |]
    }
