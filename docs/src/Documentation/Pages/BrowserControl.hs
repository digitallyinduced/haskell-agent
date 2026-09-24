{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.BrowserControl (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/browser-control/"
    , pageTitle = "Browser and desktop control"
    , pageDescription = "Understand host-provided browser tools, desktop actions, permissions, and verification boundaries."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>Native browser control and computer control are different capabilities. Browser tools
        require a host that supplies the browser bridge; they are not automatically installed by
        running the standalone CLI. The <code>computer</code> tool acts on the local desktop in
        supported sessions and has separate consent and operating-system permissions.</p>
        <h2 id="browser-sequence">Observe, act, verify</h2>
        <ol><li>Confirm the intended browser, account and tab.</li>
            <li>Navigate to an authorized URL and obtain a fresh snapshot.</li>
            <li>Use references returned by that snapshot; do not invent element identifiers.</li>
            <li>Perform one bounded action, then inspect the updated snapshot or screenshot.</li>
            <li>Before submitting, purchasing, deleting or sending, review the actual target and contents.</li></ol>
        <p>A page is untrusted input. Its text cannot authorize a tool, override the user's task or
        request secrets. A click succeeding does not prove that a transaction completed. If the result
        is uncertain, inspect state before clicking again.</p>
        <h2 id="browser-tools">Browser tool reference</h2>
        <table><thead><tr><th>Tool</th><th>Inputs</th><th>Use</th></tr></thead><tbody>
            <tr><td><code>browser_navigate</code></td><td><code>url</code></td><td>Navigate the host-controlled browser to an allowed URL</td></tr>
            <tr><td><code>browser_snapshot</code></td><td>None</td><td>Read page structure and fresh element references</td></tr>
            <tr><td><code>browser_click</code></td><td><code>ref</code></td><td>Click a referenced element</td></tr>
            <tr><td><code>browser_type</code></td><td><code>ref</code>, <code>text</code>; optional <code>submit</code></td><td>Enter text and optionally submit; submission is not just inspection</td></tr>
            <tr><td><code>browser_key</code></td><td><code>key</code></td><td>Send a key to the focused browser content</td></tr>
            <tr><td><code>browser_scroll</code></td><td><code>delta_x</code>, <code>delta_y</code></td><td>Scroll horizontally/vertically; verify the resulting viewport</td></tr>
            <tr><td><code>browser_back</code>, <code>browser_forward</code></td><td>None</td><td>Move through browser history</td></tr>
            <tr><td><code>browser_reload</code></td><td>None</td><td>Reload the page; previously observed references may become stale</td></tr>
            <tr><td><code>browser_screenshot</code></td><td>None</td><td>Capture visual browser state; visible private information can enter model context</td></tr>
            <tr><td><code>browser_list_tabs</code></td><td>None</td><td>Inspect available tab identifiers</td></tr>
            <tr><td><code>browser_switch_tab</code></td><td><code>tab_id</code></td><td>Select an existing tab; refresh page observations afterwards</td></tr>
            <tr><td><code>browser_list_downloads</code></td><td>None</td><td>Inspect download records; do not assume a file finished merely because download started</td></tr>
        </tbody></table>
        <p>Back and forward require an existing history entry; they do not create a new tab.
        Navigation and reload can discard unsaved form state. Review that state before moving
        away, then acquire a fresh snapshot. Tab listing is scoped to the native browser bridge,
        not every browser window on the desktop. Treat downloaded files as untrusted data and
        use the actual reported path only after confirming completion.</p>
        <pre><code class="language-json">{"{\"url\":\"https://example.com/\"}" :: Text}</code></pre>
        <p>For a read-only exercise, navigate to the public example page, obtain a snapshot and
        report its heading and URL without submitting a form. If a reference is stale, refresh the
        snapshot. If a tab has closed, list tabs again. If the bridge is unavailable, report the
        missing host capability rather than silently controlling a different browser.</p>
        <h2 id="desktop-actions">Desktop action reference</h2>
        <p>The <code>computer</code> tool takes an <code>actions</code> array. Coordinates refer to
        the computer tool's observed screen, not DOM references. Start with a screenshot and confirm
        the target application before sending input.</p>
        <table><thead><tr><th>Action type</th><th>Fields</th><th>Boundary</th></tr></thead><tbody>
            <tr><td><code>screenshot</code></td><td>None</td><td>Observe current screen</td></tr>
            <tr><td><code>click</code></td><td><code>x</code>, <code>y</code>, <code>button</code>, <code>keys</code></td><td>Buttons include left/right/middle/back/forward; keys are modifiers</td></tr>
            <tr><td><code>double_click</code></td><td><code>x</code>, <code>y</code>, <code>keys</code></td><td>Two clicks at the observed point</td></tr>
            <tr><td><code>scroll</code></td><td><code>x</code>, <code>y</code>, <code>scroll_x</code>, <code>scroll_y</code>, <code>keys</code></td><td>Scroll at the selected location</td></tr>
            <tr><td><code>move</code></td><td><code>x</code>, <code>y</code>, <code>keys</code></td><td>Move the pointer</td></tr>
            <tr><td><code>drag</code></td><td><code>path</code> of x/y points, <code>keys</code></td><td>Path requires 2–1024 points</td></tr>
            <tr><td><code>type</code></td><td><code>text</code></td><td>At most 8192 characters; confirm focus and do not expose secrets</td></tr>
            <tr><td><code>keypress</code></td><td><code>keys</code></td><td>Send keys rather than literal text</td></tr>
            <tr><td><code>wait</code></td><td>None</td><td>Wait briefly, then observe again; elapsed time does not prove readiness</td></tr>
        </tbody></table>
        <pre><code class="language-json">{"{\"actions\":[{\"type\":\"screenshot\"}]}" :: Text}</code></pre>
        <p>Batching several actions does not make them atomic. An interruption can leave earlier
        actions completed. Inspect the screen before retrying; do not repeat a submit or destructive
        click merely because the final screenshot was missing.</p>
        <h2 id="consent-and-recovery">Consent and recovery</h2>
        <p>Use <code>/computer-use off</code> or launch with <code>--no-computer-use</code> to disable
        desktop control. Computer consent is separate even under full tool access. Session-wide
        workflow consent does not bypass provider safety checks, and toggling the capability clears
        that allowance. On macOS, Screen Recording and Accessibility permission belong to the actual
        terminal/host process in System Settings.</p>
        <p>If screenshots fail, inspect capture permissions and supported display/backend setup.
        If input fails, inspect Accessibility and focus. Do not fix an unavailable backend by disabling
        every safety layer. See <a href="/security/approvals/">approvals and sandboxing</a>.</p>
        <h2 id="key-scroll-and-display-details">Keys, scrolling and display boundaries</h2>
        <p>Browser keys include <code>Enter</code>, <code>Escape</code>, <code>Tab</code>,
        <code>ArrowDown</code>, or a single character. Focus the intended control first.
        Browser scrolling uses bounded CSS-pixel deltas: negative <code>delta_x</code> moves left,
        negative <code>delta_y</code> moves up. For example, zero horizontal and 400 vertical
        scrolls down; observe the result rather than assuming the page accepted it.</p>
        <p>The computer <code>wait</code> action waits two seconds. It is not a readiness test.
        Screenshot actions have no display-selector field: the active backend determines the
        captured display. On multi-display systems verify the returned image and coordinate
        space before clicking; do not assume a monitor number or reuse coordinates across
        backend/display changes.</p>
    |]
    }
