{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Keybindings (page) where

import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/keybindings/"
    , pageTitle = "Keyboard reference"
    , pageDescription = "Fullscreen composer, scrollback, approval, and picker shortcuts with terminal compatibility notes."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>These shortcuts describe the fullscreen interface. The footer shows the actions available
        in the current context; an open dialog can change what a key does. Minimal mode uses a different
        terminal interface. This page documents built-in behavior, not a configurable keymap format.</p>
        <h2 id="composer">Composer</h2>
        <table>
            <thead><tr><th>Key</th><th>Action</th></tr></thead>
            <tbody>
                <tr><td><kbd>Enter</kbd></td><td>Send when idle. While running, steer the turn, or queue a separate follow-up when Apple Intelligence is available</td></tr>
                <tr><td><kbd>Shift+Enter</kbd></td><td>Insert a newline when supported by the terminal</td></tr>
                <tr><td><kbd>Escape</kbd> / <kbd>Ctrl+C</kbd></td><td>Cancel a running turn from the composer</td></tr>
                <tr><td><kbd>Tab</kbd></td><td>Move into scrollback navigation when the normal composer footer offers it</td></tr>
                <tr><td><kbd>Page Up</kbd> / <kbd>Page Down</kbd></td><td>Scroll conversation history</td></tr>
                <tr><td><kbd>Cmd+K</kbd> / <kbd>Alt+K</kbd></td><td>Open Meta Console when the terminal reports the corresponding sequence</td></tr>
                <tr><td><kbd>Ctrl+R</kbd></td><td>Start or stop dictation; availability depends on voice configuration</td></tr>
            </tbody>
        </table>
        <p>In scrollback, <kbd>Ctrl+U</kbd>/<kbd>Ctrl+D</kbd> move half a page, and
        <kbd>Home</kbd>/<kbd>End</kbd> go to the oldest history/latest output.
        <kbd>Left</kbd>, <kbd>Right</kbd>, and <kbd>Enter</kbd> each toggle the selected
        block's expanded state. These keys are context-sensitive: Ctrl+D here
        navigates, but in an empty composer it exits.</p>
        <h2 id="inline-editor">Minimal-mode editor</h2>
        <p>The inline editor has its own decoder, not the fullscreen scrollback map.
        Use <kbd>Ctrl+A</kbd>/<kbd>Ctrl+E</kbd> for endpoints,
        <kbd>Ctrl+B</kbd>/<kbd>Ctrl+F</kbd> for character movement, and
        <kbd>Alt+B</kbd>/<kbd>Alt+F</kbd> for word movement.
        <kbd>Ctrl+P</kbd>/<kbd>Ctrl+N</kbd> are its Up/Down alternatives.
        <kbd>Ctrl+U</kbd>, <kbd>Ctrl+K</kbd>, and <kbd>Ctrl+W</kbd> kill to the start,
        end, or previous word; <kbd>Ctrl+Y</kbd> yanks. <kbd>Ctrl+L</kbd> clears the
        display, <kbd>Ctrl+R</kbd> invokes dictation, and <kbd>Ctrl+C</kbd> interrupts.
        <kbd>Shift+Enter</kbd> inserts a newline and <kbd>Shift+Tab</kbd> cycles mode
        when the terminal reports those sequences. Do not assume a fullscreen-only
        shortcut is implemented by this editor.</p>
        <h2 id="cancel-or-exit">Cancel versus exit</h2>
        <p>During a turn, the first Ctrl+C requests cancellation. Pressing it again
        while cancellation is already pending requests exit. At an idle composer the
        first Ctrl+C shows a warning; a second within two seconds confirms exit.
        Escape cancels a running turn, dismisses an open slash menu when idle, and
        otherwise preserves the draft. It is not a universal clear-input shortcut.</p>
        <h2 id="editing-and-interruption">Editing, history, and interruptive send</h2>
        <p><kbd>Ctrl+Enter</kbd> or <kbd>Ctrl+O</kbd> sends immediately, interrupting the
        active turn rather than merely steering it. With an empty draft it can promote
        the oldest queued prompt. Check <code>/queue</code> first if you are unsure what
        will run. Terminal support for modified Enter varies; Ctrl+O is the alternative.</p>
        <table><thead><tr><th>Key in composer</th><th>Action</th></tr></thead><tbody>
            <tr><td><kbd>Ctrl+Q</kbd></td><td>Request exit</td></tr>
            <tr><td><kbd>Ctrl+D</kbd></td><td>Exit on an empty draft; otherwise delete the character after the cursor</td></tr>
            <tr><td><kbd>Left</kbd> / <kbd>Right</kbd>, <kbd>Ctrl+B</kbd> / <kbd>Ctrl+F</kbd></td><td>Move one character</td></tr>
            <tr><td><kbd>Home</kbd> / <kbd>End</kbd>, <kbd>Ctrl+A</kbd> / <kbd>Ctrl+E</kbd></td><td>Move to the beginning/end of the current line</td></tr>
            <tr><td><kbd>Alt+Left</kbd> / <kbd>Alt+Right</kbd>, <kbd>Alt+B</kbd> / <kbd>Alt+F</kbd></td><td>Move by words; Meta variants also work when reported</td></tr>
            <tr><td><kbd>Backspace</kbd> / <kbd>Delete</kbd></td><td>Delete before/after the cursor</td></tr>
            <tr><td><kbd>Ctrl+W</kbd>, modified <kbd>Backspace</kbd></td><td>Kill the previous word; Ctrl, Alt, or Meta Backspace are recognized</td></tr>
            <tr><td><kbd>Alt+D</kbd> / <kbd>Meta+D</kbd></td><td>Kill the following word</td></tr>
            <tr><td><kbd>Ctrl+U</kbd> / <kbd>Ctrl+K</kbd></td><td>Kill to the start/end of the line</td></tr>
            <tr><td><kbd>Ctrl+Y</kbd></td><td>Insert the kill buffer; consecutive kills accumulate until another key breaks the chain</td></tr>
            <tr><td><kbd>Ctrl+_</kbd></td><td>Undo a draft edit, not a conversation turn or file edit</td></tr>
            <tr><td><kbd>Ctrl+L</kbd></td><td>Invalidate the rendering cache and redraw</td></tr>
            <tr><td><kbd>Up</kbd> / <kbd>Down</kbd></td><td>Move between draft lines, then prompt history at its boundaries</td></tr>
            <tr><td><kbd>Ctrl+V</kbd> / <kbd>Meta+V</kbd></td><td>Paste clipboard text or attach supported images; a fullscreen hint appears when a raster is already on the clipboard</td></tr>
            <tr><td><kbd>Shift+Tab</kbd></td><td>Cycle prompt mode while awaiting input</td></tr>
        </tbody></table>
        <p>When slash suggestions are open, <kbd>Up</kbd>/<kbd>Down</kbd> select a
        suggestion and <kbd>Tab</kbd> accepts it instead of focusing scrollback.
        <kbd>Escape</kbd> dismisses that menu while idle and otherwise preserves the draft.
        During a running turn it cancels the turn. Do not assume idle Escape clears text.</p>
        <h2 id="scrollback">Scrollback navigation</h2>
        <table>
            <thead><tr><th>Key</th><th>Action</th></tr></thead>
            <tbody>
                <tr><td><kbd>Up</kbd> / <kbd>Down</kbd></td><td>Move between blocks</td></tr>
                <tr><td><kbd>Ctrl+J</kbd> / <kbd>Ctrl+K</kbd></td><td>Move by lines</td></tr>
                <tr><td><kbd>Page Up</kbd> / <kbd>Page Down</kbd></td><td>Move by pages</td></tr>
                <tr><td><kbd>Ctrl+Y</kbd></td><td>Copy the selected content</td></tr>
                <tr><td><kbd>Tab</kbd> / <kbd>Escape</kbd>, or typing</td><td>Return to the prompt</td></tr>
            </tbody>
        </table>
        <h2 id="dialogs">Pickers and approvals</h2>
        <p>Use arrow keys to select and <kbd>Enter</kbd> to confirm. Searchable pickers accept typing
        to filter. <kbd>Escape</kbd> cancels a picker; in an approval dialog it denies the request.
        Read the dialog rather than treating confirmation as a routine navigation step.</p>
        <h3 id="agent-picker">Inspect another agent without sending work</h3>
        <p><code>/agents</code> (alias <code>/a</code>) opens a viewport selector, not a
        terminate-agent dialog. In fullscreen mode, <kbd>Up</kbd>/<kbd>Down</kbd> and
        <kbd>Shift+Tab</kbd>/<kbd>Tab</kbd> move selection; <kbd>Enter</kbd> chooses the
        agent, and <kbd>Escape</kbd> or <kbd>q</kbd> cancels. Page keys scroll the
        overlay. Entries show the agent path, status and a short transcript preview.
        Choose the root again to return to its conversation.</p>
        <p>The minimal-mode picker supports up/down, confirm and cancel; selection wraps.
        With non-TTY stdin it only prints the agent tree and makes no selection.
        Selecting a viewport does not interrupt a child, change its instructions or close
        its worktree. Ask for the explicit lifecycle operation instead. Use
        <a href="/guides/sessions/#session-browser">session-browser keys</a> for
        resume/search/delete and <a href="/customization/mcp/#manage-connections">MCP
        manager keys</a> for server actions; these are distinct dialogs.</p>
        <h2 id="account-picker">Account dashboard controls</h2>
        <p>The standalone terminal login dashboard uses <kbd>Up</kbd>/<kbd>Down</kbd>
        or <kbd>k</kbd>/<kbd>j</kbd> to select an account, wrapping at the list ends.
        <kbd>Enter</kbd> or <kbd>r</kbd> refreshes the selected account's usage;
        it does not select that account for a conversation. <kbd>a</kbd> adds an
        account, <kbd>g</kbd> connects a gateway, <kbd>e</kbd> toggles the selected
        account, <kbd>d</kbd> starts disconnect/delete, and <kbd>i</kbd> imports
        an eligible discovered account. Letter actions also accept uppercase.
        <kbd>Escape</kbd> or <kbd>q</kbd> closes the dashboard. With no accounts,
        refresh closes it and selected-account actions do nothing; use add instead.</p>
        <p>Fullscreen <code>/login</code> instead presents selectable menu entries:
        connect, gateway, refresh all, or an account's action menu. Use the dialog's
        navigation and confirm keys, not the standalone letter bindings. The account
        menu offers refresh, enable/disable, import when applicable, disconnect with
        confirmation, and back. Cancel returns from the account menu to the dashboard;
        cancel there closes it. Secret entry remains in the dedicated overlay.
        Non-TTY login prints refreshed account information rather than exposing this
        interactive dashboard.</p>
        <h2 id="try-navigation">Example: inspect output without losing your prompt</h2>
        <ol>
            <li>After a response completes, press <kbd>Tab</kbd> when the footer offers scrollback.</li>
            <li>Use <kbd>Up</kbd> and <kbd>Down</kbd> to select an earlier block.</li>
            <li>Use <kbd>Ctrl+Y</kbd> if you want to copy it.</li>
            <li>Press <kbd>Escape</kbd> to return to composing your next instruction.</li>
        </ol>
        <h2 id="terminal-compatibility">If a shortcut does not arrive</h2>
        <p>Terminal applications and multiplexers can intercept keys or send indistinguishable sequences.
        Run <code>/terminal</code> to inspect detected capabilities. Use <code>/meta</code> instead of
        its shortcut, <code>/copy</code> to copy a response, and <code>/edit-prompt</code> to edit a draft.
        For native mouse text selection, use <code>/mouse off</code>; restore application mouse handling
        with <code>/mouse on</code>.</p>
    |]
    }
