{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Terminal (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/terminal/"
    , pageTitle = "Using the terminal"
    , pageDescription = "Submit prompts, steer work, choose terminal rendering, and inspect agent output."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <h2 id="choose-an-interface">Choose an interface</h2>
        <pre><code class="language-sh">{"agent-cli --fullscreen\nagent-cli --minimal" :: Text}</code></pre>
        <p>Fullscreen uses a retained terminal interface with a composer and conversation
        view. Minimal mode uses terminal-native append-only rendering. Use <code>/terminal</code>
        to inspect detected terminal capabilities when display behavior is unexpected.</p>
        <figure>
            <img src="/terminal-overview.svg" alt="Annotated agent-cli help showing model, working directory, worktree, terminal mode, and approval options" width="1100" height="610" />
            <figcaption>Selected real <code>agent-cli --help</code> output, re-typeset with annotations—not an interactive session screenshot. <a href="/terminal-overview.txt">Read the captured text and version.</a></figcaption>
        </figure>
        <h2 id="give-a-useful-instruction">Give a useful instruction</h2>
        <p>Describe the outcome, constraints, and how success should be checked:</p>
        <pre><code>{"Add validation for empty project names. Follow the existing error format,\nadd a regression test, and run the focused tests. Do not change the public API." :: Text}</code></pre>
        <p>The response and tool results appear in the conversation. Tool calls may stop
        for <a href="/security/approvals/">approval</a>. An instruction to inspect or explain is
        different from permission to execute every possible follow-up operation.</p>
        <h2 id="steer-or-queue-a-follow-up">Steer or queue a follow-up</h2>
        <p>For example, if an implementation starts changing unrelated code, send the
        steering instruction below. The queued review is a separate follow-up, not
        a request to interrupt the current test run.</p>
        <p>While a fullscreen turn is running, a plain text prompt steers the current
        turn. Use explicit commands to make your intention clear:</p>
        <pre><code>{"/steer Keep the existing public function names.\n/queue After the tests finish, review the documentation changes." :: Text}</code></pre>
        <p><code>/queue</code> without a prompt lists waiting prompts. When the agent is idle, either
        prompt command starts a new turn.</p>
        <p>Steering is text input for the active turn's next model-input boundary, not an
        interrupt signal to its currently executing tool. A draft with pending image
        attachments takes the normal queued-input path instead of text-only steering.
        Explicit <code>/queue</code> also stays on that path and is consumed after the
        active turn returns. Inspect pending images with <code>/attachments</code> before
        submitting; do not assume an image was inserted into an already-running request.</p>
        <p>For an immediate interruption rather than steering, use <kbd>Ctrl+O</kbd>
        or supported <kbd>Ctrl+Enter</kbd>. With an empty draft this may send the oldest
        queued prompt; inspect <code>/queue</code> first. Canceling does not undo completed
        tools or remote mutations. Ask for the last confirmed result before retrying.</p>
        <h2 id="provider-waits">Waiting, cancellation, and retry</h2>
        <p>A short provider cooldown differs from a subscription usage reset.
        The automatic short cooldown retry accepts waits up to 120 seconds and does not
        retry that same cooldown indefinitely. Longer limits may lead to a usage-reset wait
        or an offered fallback. Read the wait notice and <code>/usage</code> rather than
        treating quiet output as a hung tool.</p>
        <p>Canceling a cooldown wait in an interactive session returns the pending prompt
        as a draft; canceling a one-shot wait exits the run. Inspect whether anything
        already executed before sending again. The subscription fallback path does not
        silently switch from subscription credentials to API-key billing.
        <code>/retry</code> retries the retained failed turn, not an edited replacement.</p>
        <p>The retry restores that turn's plan state and retained inputs, including its
        attachments; newly staged images or an edited draft do not replace those inputs.
        If the failed turn was checkpointed, retry continues from that checkpoint instead
        of constructing a fresh submission. The already-recorded user prompt is not duplicated
        in history. None of this guarantees idempotence of remote effects: inspect the last
        confirmed tool result before retrying a send, deployment or other mutation.</p>
        <h2 id="exit-aliases">Exit and compatibility commands</h2>
        <p><code>/quit</code> is the explicit exit command. Bare <code>exit</code>,
        <code>quit</code>, <code>:q</code>, <code>:q!</code>, <code>:quit</code>,
        <code>:wq</code>, and <code>:wq!</code> also exit; these are not prompts to
        the model. <code>:reload</code> is a developer runtime-reload command, not
        <code>/reload-auth</code>; <code>:yolo</code>
        is the compatibility approval toggle. Prefer named slash commands in shared
        instructions so their intent is clear.</p>
        <p><code>/exit</code> is also an alias of <code>/quit</code>. Normal exit follows
        the graceful cleanup path: it prints a resume hint to stderr when persistence
        provides a session ID, closes tools, and interrupts owned collaboration agents,
        snapshots them and joins their supervisors. Do not confuse this with deleting the
        conversation or undoing completed changes. Read the hint before closing the terminal
        and verify the resumed session's last confirmed result. Force-killing a process
        cannot promise the same cleanup or a freshly printed hint.</p>
        <h2 id="inspect-and-reuse-output">Inspect and reuse output</h2>
        <table>
            <thead><tr><th>Command</th><th>Action</th></tr></thead>
            <tbody>
                <tr><td><code>/diff</code></td><td>Show the Git diff, including untracked files</td></tr>
                <tr><td><code>/review</code></td><td>Ask for a review of current changes</td></tr>
                <tr><td><code>/find validation</code></td><td>Search this conversation in a pager</td></tr>
                <tr><td><code>/transcript</code></td><td>Open the session transcript</td></tr>
                <tr><td><code>/copy</code></td><td>Copy an assistant response</td></tr>
                <tr><td><code>/copy-code 1</code></td><td>Copy the first fenced code block from the last response</td></tr>
                <tr><td><code>/edit-prompt</code></td><td>Edit a draft without submitting it</td></tr>
            </tbody>
        </table>
        <h3 id="copy-output">Copy the right response or code block</h3>
        <p><code>/copy</code> (alias <code>/copy-last</code>) selects the latest assistant
        response; <code>/copy 2</code> selects the previous one. Indexes start at 1,
        counting backward from the latest. Add a destination to write a file:
        <code>/copy 2 /absolute/path/to/answer.md</code>. A path without an index uses
        the latest response. The remaining text is the destination, so use an
        unambiguous absolute path rather than a filename beginning with a number.</p>
        <p><code>/copy-code</code> defaults to code block 1 of the last response;
        <code>/copy-code 2</code> chooses its second fenced block. A missing or
        out-of-range block reports an error rather than copying different content.
        <code>/copy-diff</code> copies the last diff block from the assistant's last
        response, not a fresh Git diff. Use <code>/diff</code> to inspect current
        checkout changes. If no assistant response exists, there is nothing to copy.</p>
        <h2 id="example-review-a-change">Example: inspect, change, verify</h2>
        <ol>
            <li>Ask: <code>Find where project names are validated. Explain the current behavior without editing files.</code></li>
            <li>Check the cited files, then submit the bounded validation request above.</li>
            <li>When the turn finishes, run <code>/diff</code> and inspect the implementation and regression test.</li>
            <li>Ask: <code>List the exact tests you ran, their results, and anything you could not verify.</code></li>
        </ol>
        <p><strong>Expected result:</strong> a focused change, a test for the empty-name case,
        and evidence of the test result. If the agent only describes a proposed fix,
        it has not necessarily edited or tested anything. Ask it to distinguish
        completed work from recommendations before continuing.</p>
        <h2 id="terminal-diagnostics">Read terminal diagnostics</h2>
        <p><code>/terminal</code> (alias <code>/ghostty</code>) prints detected
        capabilities, not a terminal installer. <code>kitty-keyboard</code> concerns
        enhanced key reporting; <code>inline-images</code> concerns local image display,
        not model vision; <code>osc52-clipboard</code> enables terminal clipboard writes.
        <code>native-progress</code>, <code>notifications</code>,
        <code>semantic-prompts</code> and <code>synchronized-output</code> describe optional
        terminal integrations. <code>tmux-passthrough</code> indicates the tmux path.
        A <code>no</code> is a capability limitation, not a failed provider login. When
        keys or clipboard writes fail, compare outside a multiplexer before changing
        agent permissions.</p>
        <h2 id="open-desktop">Open the current session in a desktop client</h2>
        <p><code>/desktop</code> is macOS-only and requires a persisted conversation and
        the separately supplied native application registered as
        <code>dev.haskell-agent.macos</code>. It sends a
        <code>haskell-agent://session/SESSION_ID</code> deep link through macOS
        <code>open</code>; it does not install that application. If launch fails, check
        that the app is installed and up to date, or continue with <code>/resume</code>
        in the CLI. A successful launch means the OS accepted the request, not that
        the desktop client completed every subsequent operation.</p>
        <h2 id="pager-search">Search the saved transcript</h2>
        <p><code>/transcript</code> (or <code>/log</code>) opens the saved conversation
        in <code>$PAGER</code>, falling back to <code>less -R</code>. In the default pager,
        type <code>/text</code> then Enter to search, <code>n</code> for the next match,
        <code>N</code> for the previous match, and <code>q</code> to return. A custom pager
        has its own controls.</p>
        <p><code>/find timeout</code> first filters transcript blocks case-insensitively
        across their titles, bodies, details and timestamps, then opens matching blocks
        in the pager. With no argument, <code>/find</code> opens the complete transcript
        so you can use the pager's search. No matches and no saved transcript have
        separate informational messages. A missing pager reports an error: install
        <code>less</code> or configure <code>PAGER</code>. This is viewing, not exporting;
        use <code>/export</code> to retain a Markdown file.</p>
        <h2 id="themes">Preview and save a theme</h2>
        <p>In fullscreen mode, <code>/theme</code> or <code>/t</code> opens a live preview:
        arrow keys preview, Enter applies and saves the choice, and Escape cancels.
        Available themes are Auto (native terminal colors), Midnight, Daylight,
        Tokyo Night, Rose Pine Moon, and Oscura Midnight. For example,
        <code>/theme tokyo-night</code> selects Tokyo Night directly. Selection is saved
        to your harness configuration for later sessions. If saving fails, fix the
        reported configuration error before retrying. Minimal mode reports that theme
        selection requires fullscreen mode.</p>
        <h2 id="clipboard-recovery">When copying does not reach the clipboard</h2>
        <p>Copy commands use the terminal's OSC 52 clipboard capability, not an automatic
        fallback to an operating-system clipboard program. If the capability is absent,
        they report <code>terminal clipboard is unavailable</code>. A success message means
        the sequence was emitted; a terminal or multiplexer can still block it. Check
        <code>/terminal</code> and your terminal's clipboard permissions.</p>
        <p>To avoid that dependency, save an assistant response with
        <code>/copy 1 /absolute/path/to/answer.txt</code>, or use <code>/export</code> to save
        Markdown and copy from your editor. For <code>/copy-diff</code>, save the last
        response and extract its diff block; do not mistake it for a new Git diff.</p>
        <h2 id="reuse-and-edit-prompts">Reuse and edit prompts</h2>
        <p><code>/history</code> reads the shared prompt history, not just the current
        session's transcript. In the fullscreen picker, type to filter, select a result,
        and press Enter. Selection places the original prompt into the composer without
        submitting it; review paths and assumptions before sending. Cancel leaves the
        conversation unchanged. An empty history reports that no prompt history is available.</p>
        <p><code>/edit-prompt</code> opens the current draft in <code>$VISUAL</code>, then
        <code>$EDITOR</code>, falling back to <code>vi</code>. Save and exit successfully to
        return the edited text to the composer, still unsent. An invalid program or failed
        editor exit reports an error rather than sending the draft. To abandon changes,
        use your editor's discard-and-exit operation; that editor controls file saving.</p>
        <h2 id="select-text-with-the-mouse">Select text with the mouse</h2>
        <p>In fullscreen mode, disable mouse capture for native terminal selection:</p>
        <pre><code>/mouse off</code></pre>
        <p>This also disables application mouse clicks and wheel scrolling. Restore them
        with <code>/mouse on</code>; the preference is saved. Keyboard navigation remains
        available in either mode.</p>
        <h2 id="attach-images">Attach images</h2>
        <p>When a screenshot or copied raster is already on the clipboard at session
        start, fullscreen mode shows a short hint above the prompt:
        <code>Image in clipboard · Ctrl+V to paste</code>. The hint is metadata-only: it
        inspects advertised pasteboard types, not image bytes, and it does not fire for
        Finder file copies. Ctrl/Cmd+V then attaches the image as usual.</p>
        <p>Use <code>/paste</code> to attach a clipboard image and preview it in the terminal.
        <code>/attachments</code> lists queued images; <code>/clear-attachments</code> drops them.</p>
        <p>The explicit image command first tries existing clipboard image-file paths, then
        bitmap data. Text-only clipboard content is rejected with advice to paste text normally.
        <code>/paste --send Describe this screenshot.</code> submits the newly read image
        immediately with that caption; without a caption it uses <code>See attached image.</code>.
        Ordinary <code>/paste</code> stages images for a later prompt rather than sending them.</p>
        <p>Ctrl/Cmd+V prefers nonempty clipboard text, falling back to images. Idle bracketed
        paste can classify bitmap data first and then image-file paths; ordinary multiline
        text stays in the editor. During a running turn, nonempty bracketed text is inserted
        locally into the draft instead of waiting for clipboard classification. For example,
        paste a two-line error and stack trace, add your question, then submit the complete
        draft. Pasting itself is not consent to execute pasted commands.</p>
        <p>Clipboard access reads the machine running the CLI. Linux needs
        <code>wl-paste</code> from wl-clipboard for Wayland or <code>xclip</code> for X11,
        with access to that desktop session; macOS uses native clipboard readers. Over SSH,
        upload an image and paste its server-side path rather than assuming access to your
        laptop clipboard. If the reader reports text, missing tools, or an unloadable file,
        repair that input and check <code>/attachments</code> before retrying.</p>
        <p>Pending images are live request state, not a durable draft archive. Identical image
        bytes are deduplicated; limits are 16 pending images, 64 MiB total and 20 MiB per image.
        Inspect rejection notices and remove unnecessary images instead of repeatedly pasting.
        A normal submitted prompt consumes its pending attachments. A prompt consisting only
        of recognized image paths attaches/previews them first; follow it with your question.</p>
        <p>An image shown by the agent's display tool is not automatically supplied to the
        model. The model's image-inspection tool must attach it to the model context.
        Image support depends on the provider and terminal.</p>
        <h2 id="configure-without-interrupting-the-topic">Configure without interrupting the topic</h2>
        <pre><code>/meta set the concurrent agent limit to 3</code></pre>
        <p>Meta Console is separate from the coding conversation. Its typed configuration
        plan is validated and previewed before execution. <code>Cmd+K</code> (or <code>Alt+K</code> in
        terminals reporting that sequence) opens its prompt.</p>
        <p><code>/configure REQUEST</code> is an alias. Supported typed changes are connecting
        or selecting a provider account; adding, updating, removing, enabling or disabling MCP
        servers; MCP OAuth login and secret-environment entry; MCP startup strategy; web-fetch
        enablement, domain allowlist, timeout and byte limits; LSP enablement and server
        add/update/remove or secret-environment entry; and setting or clearing the concurrent
        agent limit. Account selection uses the account picker instead of silently choosing
        a similarly named account. Secret values belong in the host's secret-entry flow, not
        in your natural-language request.</p>
        <p>The allowed session-command subset is <code>/model NAME</code>,
        <code>/title-model NAME</code> or <code>--auto</code>, <code>/effort LEVEL</code>,
        <code>/fast</code>, <code>/shell ghci|bash|both|none</code>, <code>/computer-use [on|off]</code>,
        <code>/codemod</code>, <code>/always-approve</code>, <code>/agents limit N</code> and
        <code>/skills reload</code>. The concurrent limit must be 1–256. These still obey
        their ordinary availability and permission rules. Meta Console is not an arbitrary
        shell-command executor or a general editor for every configuration key.</p>
        <p>An empty request or invalid/conflicting plan is rejected. A clarification asks you
        to resolve ambiguity; an informational answer need not change configuration. Review the
        preview's exact account, server names and scope before approving. Canceling approval
        applies no plan. If execution fails after approval, inspect the affected configuration
        and connection status before retrying; do not assume that earlier successful actions
        were rolled back. Restate only the remaining intended change or use the direct manager.</p>
        <p>Use <code>/help NAME</code> for a specific command, and <code>/quit</code> to exit.</p>
    |]
    }
