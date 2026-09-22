{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.NativeIntegration (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

page :: Page
page = Page
    { pagePath = "/reference/native-integration/"
    , pageTitle = "Native and embedded integrations"
    , pageDescription = "Understand native bridge capabilities, ownership, host responsibilities, and distribution-specific integration boundaries."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>This checkout supplies native bridge and Haskell embedding interfaces. An exported
        interface is not a promise that a complete macOS, Windows, iOS, Android or web client
        is shipped here. Installation and screen-by-screen guidance must come from the
        distribution providing that client. Do not infer a stable cross-version ABI from
        the presence of an exported function.</p>
        <h2 id="choose-interface">Choose the integration boundary</h2>
        <table><thead><tr><th>Need</th><th>Interface</th></tr></thead><tbody>
            <tr><td>HTTP clients and remote automation</td><td><a href="/reference/server/">REST/SSE server</a>; Haskell consumers can use <code>agent-server-client</code>.</td></tr>
            <tr><td>Local durable subprocess scheduling</td><td><a href="/reference/runtime-daemon/">Unix-socket daemon</a>.</td></tr>
            <tr><td>Native host with direct engine callbacks</td><td>Darwin <code>agent-native-bridge</code> and its C header.</td></tr>
            <tr><td>Distribution-owned service integrations</td><td><code>agent-integration-api</code>, compiled into the distribution.</td></tr>
        </tbody></table>
        <h2 id="bridge-contract">Build against the exact bridge contract</h2>
        <pre><code class="language-sh">nix build .#agent-native-bridge</code></pre>
        <p>The native foreign-library output is Darwin-only. Read
        <code>packages/agent-native-bridge/include/HaskellAgentBridge.h</code> from the same
        revision as the library. It defines callback types, buffer lengths, ownership,
        return codes and operation-specific JSON schemas. Do not mix a newer header with
        an older library. The <code>agent-native-bridge-library</code> output is the Haskell
        package and is not interchangeable with a ready-made GUI app.</p>
        <p><a href="/HaskellAgentBridge.h">Read or download the complete C ABI reference</a>,
        bundled locally with this documentation. It includes every declaration and
        contract comment, including callback schemas, result capacities and operation-specific
        return values. This is a verbatim source export, not a hand-maintained subset.
        Compare it with your installed distribution before compiling a consumer.</p>
        <ol>
            <li>Initialize the runtime, create an engine with a retained event callback,
            and preserve callback context for the lifetime required by the header.</li>
            <li>Stage options and attachments against the intended turn identifier; stage
            native voice only after turn options. Discard abandoned staging.</li>
            <li>Submit a typed request and distinguish admission from eventual task completion.
            Observe events rather than treating an accepted submission as a finished turn.</li>
            <li>Resolve only live interaction requests and the exact advertised choice.
            Keep arbitrary model text out of approval decisions.</li>
            <li>Cancel operations when their owner closes, await the specified completion
            boundary, destroy owned operation handles, then destroy the engine and runtime.</li>
        </ol>
        <p>Callbacks can arrive outside a GUI main thread. Follow each function's contract
        rather than guessing lifetime or freeing a borrowed buffer. Copy data that must
        outlive its callback. A cancellation request does not roll back a completed commit,
        push, external message or other remote effect.</p>
        <h2 id="first-host-call">First host call: syntax highlighting without an engine</h2>
        <p>This small C consumer exercises a synchronous, read-only interface before adding
        engine lifetime and asynchronous callbacks. It emits byte ranges, not substrings.
        Save it as <code>highlight.c</code> and check its types against the pinned header:</p>
        <pre><code class="language-sh">cc -fsyntax-only -I packages/agent-native-bridge/include highlight.c</code></pre>
        <pre><code class="language-c">{highlightExample}</code></pre>
        <p>The command only checks the consumer's C declarations; it does not link or run
        the bridge. Your native build must link the matching Darwin foreign-library artifact.
        Set <code>AGENT_SYNTAX_DIR</code> to the distribution's syntax definitions before
        initialization. Highlighting returns 0 for spans, 1 for plain-text fallback,
        2 for invalid pointers/UTF-8 and 3 for internal failure. Discard all collected spans
        for any nonzero status. Language input is limited to 4096 bytes and source to
        256 KiB or 5000 lines. Callback offsets and lengths are UTF-8 bytes, not character
        indices; ranges omit line-feed separators and never split a Unicode scalar.
        The example prints spans immediately for inspection; a real editor should collect
        them first and commit them only after a zero return code.</p>
        <h3 id="chart-capability">Enable charts only after implementing the renderer</h3>
        <p>Chart presentation is disabled by default. With a live engine, call
        <code>ha_engine_set_chart_rendering_enabled(engine, 1)</code> only after your host
        can decode the versioned chart document. Pass 0 to disable it for subsequently
        starting turns. This is independent of the operating system and does not alter
        a turn already running. Return values are 0 success, 1 null engine, 2 an enabled
        value other than 0 or 1, and 3 runtime failure. Serialize this synchronous,
        thread-safe call against engine destruction; it retains no callbacks or buffers.</p>
        <p>After enabling, process tool-finish events by call ID. If HAEV kind 5 has
        flag bit 3, its third length-prefixed field is the chart JSON. Copy the entire
        field before returning from the callback, validate its version before rendering,
        and retain the ordinary tool output as a readable fallback. Reject unsupported
        documents without losing the tool result. A missing chart field is not an error:
        the tool may have produced only text. Do not infer chart support from successful
        syntax highlighting; those are separate capabilities.</p>
        <h3 id="repository-check-lifetime">Run and cancel an argv-based repository check</h3>
        <ol>
            <li>Obtain the repository snapshot for the path being reviewed and retain its
            snapshot ID. Prepare <code>ha_utf8_string</code> arguments, a retained callback
            context and an output handle slot before calling <code>ha_repository_check_start</code>.</li>
            <li>For a check such as <code>git diff --check</code>, pass executable
            <code>git</code> and two separate arguments <code>diff</code> and <code>--check</code>,
            not a shell command string. Review the executable and arguments before starting:
            this API executes a program; it is not inherently read-only.</li>
            <li>On return status 0, own the opaque handle. Stream callback 1 is stdout
            and 2 is stderr. Copy callback-scoped bytes if retaining them; preserve order
            within each stream but do not assume a total order across both streams.</li>
            <li>The exit callback reports the process exit code, or -1 plus error text
            when launch fails. Distinguish its cancellation flag from test success.
            No callback failure is retried; output pipes are still drained.</li>
            <li>To stop, call <code>ha_repository_check_cancel</code> outside its callbacks.
            It targets the process group, including descendants, and joins teardown with
            a short termination-escalation grace period. Cancellation is not rollback.</li>
            <li>Call <code>ha_repository_check_destroy</code> exactly once from an owner
            thread. It waits for readers/process completion and guarantees no callbacks
            after return. Only then release callback context.</li>
        </ol>
        <p>Callbacks may begin before start returns; the output handle is stored first.
        Keep callbacks prompt and schedule UI updates rather than blocking teardown.
        Calling cancel or destroy reentrantly from that check's own callback is a no-op;
        schedule it on another thread after the callback returns. Every executable and
        argument must be nonempty UTF-8. Limits are 4096 arguments, 1 MiB per argument
        and 8 MiB total argument bytes. A null argument array is allowed only with count
        zero. An accepted start is not a passed check: wait for the exit callback.</p>
        <h2 id="event-contract">Decode engine events and own callbacks</h2>
        <p><code>ha_event_callback</code> receives opaque context, borrowed byte pointer and
        explicit length. Copy bytes before returning. JSON is not the only event format:
        native loop events use binary <code>HAEV</code>, followed by version (one byte),
        kind (one byte), flags (unsigned 16-bit big-endian), then unsigned 32-bit
        big-endian length-prefixed UTF-8 turn ID and kind-specific fields.</p>
        <table><thead><tr><th>Kind</th><th>Meaning</th></tr></thead><tbody>
            <tr><td>1 / 2 / 3</td><td>Reasoning text / assistant text / status.</td></tr>
            <tr><td>4</td><td>Tool start and argument updates. Replace the card identified by call ID. Flags bit 0 encrypted arguments, bit 1 truncated, bit 2 asynchronous.</td></tr>
            <tr><td>5</td><td>Tool finish. Bits 1/2 indicate truncated/asynchronous; bit 3 adds a third field after call ID and output: complete chart JSON, at most 256 KiB. Chart JSON itself is never truncated.</td></tr>
            <tr><td>6 / 7</td><td>One provider response's usage / aggregate user-turn usage. Decimal UTF-8 input/output/cached counts, then optional provider USD cost. Missing cost is not zero or a local estimate. Kind 7 is terminal and precedes turn.completed/failed when an outcome is available.</td></tr>
        </tbody></table>
        <p>Ignore unknown flag bits. Preserve ordering within a task; different tasks may
        invoke callbacks concurrently on worker threads. Never block a worker waiting for
        the GUI thread to synchronously call back into a destroying engine. Validate
        version, frame length and every field boundary before decoding; the header does
        not make a partial frame safe to interpret.</p>
        <h2 id="capabilities">Native operation families</h2>
        <p>The header is the parameter-level reference. This table identifies which workflow
        belongs to each family and the safety condition the host must preserve.</p>
        <table><thead><tr><th>Family</th><th>Operations and host responsibility</th></tr></thead>
        <tbody>{foldMap familyRow families}</tbody></table>
        <h2 id="review-and-delivery">Repository review and delivery</h2>
        <p>Load a repository snapshot and diff before offering path/hunk application.
        Refresh after edits rather than applying stale selections. Commit, push and pull-request
        creation are separate mutations. Present the exact push/PR preview to the user and
        confirm that preview; if repository state changes, obtain a fresh preview instead
        of reusing stale confirmation data. Check remote status when a response is lost.
        Cancellation cannot establish that a remote server did not accept a push.</p>
        <h2 id="authority">Accounts, gateway and integration authority</h2>
        <p>Native account OAuth has separate start and poll stages; secret entry belongs in
        the host's protected interface, not a model prompt. Gateway connection is a separate
        organization identity. Retain its authenticated authority through the complete
        operation, not only setup. Changing or closing that authority must retire its owned
        resources instead of silently continuing with local credentials.</p>
        <p><code>IntegrationProvider</code> is a compiled, distribution-supplied implementation,
        not a dynamic untrusted plugin loader. The ordinary public distribution's empty
        provider deliberately supplies no local integrations. Local, remote, combined and
        explicit local-overlay endpoints can be supplied by a distribution. Organization-local
        execution requires explicit opt-in via <code>OrganizationIntegrationProvider</code>;
        an organization acquisition does not evaluate the ordinary local provider.</p>
        <p>If an integration exists in another distribution but is unavailable here, first
        establish which distribution owns it. Do not substitute another account or weaken
        authorization. On closure, integrations must unsubscribe their MCP host, cancel
        and join owned workers, and reject callbacks after cleanup.</p>
        <h2 id="voice-and-mobile">Voice, mobile and host capabilities</h2>
        <p>Native conversational voice is distinct from <a href="/guides/voice/">dictation</a>.
        The host provides capture/playback callbacks and supplies PCM audio only while the
        live call accepts it. WebRTC is a provider-independent media layer with offer/answer,
        connection and PCM stream operations; it owns no microphone, credentials, HTTP
        client or tool execution. Its media format is PCM16, 24 kHz, mono. Do not advertise
        a calling UI solely because this library is present.</p>
        <p>Mobile bridge operations register a runner, list/revoke/wake pairings and exchange
        relay messages. Pairing and credential ownership are security boundaries, not a
        substitute for a phone application's setup instructions. Test revocation and disconnected
        runner behavior in the actual consuming client. Browser/computer/chart callbacks
        likewise require a capable host and the relevant consent.</p>
        <h2 id="validation">Integration acceptance checklist</h2>
        <ul>
            <li>Match header/library revisions and exercise creation/destruction without leaked workers.</li>
            <li>Test stale callbacks, closed owners, cancellation, unavailable capabilities and queue limits.</li>
            <li>Test account/organization switching without credential or transcript leakage.</li>
            <li>Test approval denial, stale repository previews and uncertain remote outcomes.</li>
            <li>Verify screenshots and controls in the actual application; bridge tests do not certify its UI.</li>
        </ul>
        <p>This is an interface guide derived from source. It does not certify every exported
        callback schema or an external application's behavior. The full header and native
        tests remain required reading for an embedder.</p>
    |]
    }

familyRow :: (Text, Text) -> Html
familyRow (name, description) = [hsx|<tr><td>{name}</td><td>{description}</td></tr>|]

families :: [(Text, Text)]
families =
    [ ("Lifecycle and staging", "Runtime init/exit; engine create/destroy; turn context/images/options, interaction mode and staging discard. Never reuse staging for an unintended turn.")
    , ("Requests and tasks", "JSON request, ping, session list/show, model list and turn agents; list/cancel tasks and set concurrency. Track asynchronous completion.")
    , ("Conversations", "Search, rename, delete, archive; observe with owned start/cancel/destroy handle; load around a message; fork/export/import. Keep canonical context separate from display-only output.")
    , ("Human interaction", "Register callback and resolve interaction. Match request identity and preserve full prompt/options.")
    , ("Accounts", "List, OAuth start/poll, API-key connection, enable and delete. Protect credential material.")
    , ("Organization gateway", "Account/status, connect start/poll/exchange and disconnect. Preserve authority throughout each operation.")
    , ("MCP connections", "List/icons/create/rename/enable/remove/authorize, cancel/destroy connection operation. Keep named connections distinct from raw servers.")
    , ("MCP servers", "List/read/status/add/edit/enable/disable/remove/restart. Explain reconnect and tool availability to the user.")
    , ("Distribution integrations", "Admin list/call and connections list/search/begin/submit/poll/cancel/disconnect. Availability depends on distribution and authority.")
    , ("Host capabilities", "Browser/computer callback registration, chart rendering switch, syntax highlighting. Do not imply permission or implementation from registration alone.")
    , ("Data browser", "Catalog list and row loading. Preserve data scope, pagination and authorization.")
    , ("Skills", "Installed list/read; learned list/read/create/update/archive/restore/rollback/history. Respect scope and revision conflicts.")
    , ("Repository review", "Snapshot/diff, apply path/hunks, commit. Refresh stale repository state.")
    , ("Repository delivery", "Repository/session PR status, PR summary, delivery status, push/PR preview and confirm, cancel all. Confirm the exact preview.")
    , ("Checks", "Start/cancel/destroy repository check. Retain callback owner until safe teardown.")
    , ("Mobile", "Session open/close, runner register, pairings list/revoke/wake, relay open/send/receive. Preserve pairing authorization.")
    , ("Voice", "Stage voice, submit audio and receive playback/reset/end callbacks. Distinct from composer transcription.")
    ]

highlightExample :: Text
highlightExample = "#include \"HaskellAgentBridge.h\"\n\
    \#include <stdio.h>\n\
    \static void span(void *context, size_t offset, size_t length, int32_t kind) {\n\
    \    (void)context;\n\
    \    printf(\"%zu %zu %d\\n\", offset, length, (int)kind);\n\
    \}\n\
    \int main(void) {\n\
    \    const uint8_t language[] = \"haskell\";\n\
    \    const uint8_t source[] = \"main = putStrLn \\\"hello\\\"\\n\";\n\
    \    if (ha_runtime_init() != 0) return 1;\n\
    \    int32_t status = ha_syntax_highlight(language, sizeof(language) - 1,\n\
    \        source, sizeof(source) - 1, span, NULL);\n\
    \    ha_runtime_exit();\n\
    \    return status == 0 ? 0 : 1;\n\
    \}"
