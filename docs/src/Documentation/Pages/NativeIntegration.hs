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
        <p><a href="https://github.com/digitallyinduced/haskell-agent/blob/master/packages/agent-native-bridge/include/HaskellAgentBridge.h">Read the complete C ABI reference in the source repository</a>.
        Select the revision matching your library. It includes every declaration and
        contract comment, including callback schemas, result capacities and operation-specific
        return values. The documentation links to the source rather than maintaining a copy.
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
        <h2 id="engine-lifetime">Create an engine and inspect the protocol</h2>
        <p>The following C program sends one read-only <code>ping</code>, waits at most
        five seconds for its callback, and destroys the engine before releasing callback
        storage. It does not submit a model turn or require a provider account. Save as
        <code>native-protocol.c</code> and type-check it in the documentation development
        shell with <code>cc -std=c11 -Wall -Wextra -fsyntax-only -I packages/agent-native-bridge/include native-protocol.c</code>.
        Syntax checking does not link or execute the native library.</p>
        <pre><code class="language-c">{engineExample}</code></pre>
        <p>A zero send status means admission, not completion. This example accepts the
        first callback only because it submits exactly one ping to a fresh engine; a real
        host must parse JSON and match the response <code>id</code>. Expected output has
        <code>ok: true</code> and <code>result</code> containing <code>runtime: haskell</code>
        and <code>protocol: 4</code>. Nonzero send statuses are 1 null engine, 2 invalid
        buffer, 3 internal failure, or 4 invalid request envelope. Copy callback buffers
        before returning; they need not be NUL-terminated.</p>
        <p>Runtime initialization is process-global and reference-counted. Balance each
        successful initialization with exit only after all engines and independently owned
        operation handles are closed. <code>ha_cli_main</code> is a different, process-owning
        entry point: call it once on the initial thread with writable, NUL-terminated
        argv before any runtime initialization. It normally terminates the process and is
        not a command to call from an AppKit window. Rejected arguments return 64;
        an already initialized runtime returns 70. Do not mix the two entry paths.</p>
        <h2 id="requests-and-staging">Submit a turn with explicit context</h2>
        <p>Engine requests have string <code>id</code>, string <code>method</code>, and an
        optional <code>params</code> object. Responses carry the same id, boolean
        <code>ok</code>, and either <code>result</code> or an <code>error</code> string.
        The id correlates a request; <code>turnId</code> identifies execution and staging.
        Keep both unique within the host's active requests.</p>
        <pre><code class="language-json">{turnRequestExample}</code></pre>
        <ol>
            <li>Select the absolute project directory. For a new session omit
            <code>sessionId</code>; to resume, supply its observed ID. <code>worktree: true</code>
            is accepted only for a new session. Supply <code>provider</code> and
            <code>model</code> together or omit both; an optional <code>effort</code> must
            be a recognized effort value.</li>
            <li>Before sending, call <code>ha_engine_stage_turn_options</code> with that
            turn ID. Interaction modes are ASK=0, PLAN=1, YOLO=2; shell modes are
            NONE=0, BASH=1, GHCI=2, BOTH=3. Unstaged turns default to ASK and BASH.
            Staging replaces earlier options for the same ID.</li>
            <li>Stage images with <code>ha_engine_stage_turn_images</code>: each entry
            has a nonempty MIME string and nonempty encoded bytes. This copies the
            ordered batch; an empty batch clears it. Turn IDs must be valid nonempty
            UTF-8, at most 1024 bytes.</li>
            <li>After options, optionally stage context with
            <code>ha_engine_stage_turn_context</code>. At most 32 integration attachments
            supply connection ID, server name and display name, not credentials.
            Each context string is at most 4096 UTF-8 bytes and contains no NUL.
            Token 0 means no window and requires empty window strings. A nonzero
            window token requires an application name and a host-resolvable window
            until turn completion; an expired attachment must fail rather than open
            unrestricted computer access.</li>
            <li>Serialize staging and <code>turn.start</code> for the same ID. Matching
            valid admission consumes staging. Rejected input discards staging;
            explicitly call <code>ha_engine_discard_turn_staging</code> when abandoning
            a draft, including staged voice. This is idempotent. Context staging
            without options returns 5 and leaves existing context unchanged.</li>
        </ol>
        <p>For a running turn, <code>ha_engine_set_turn_interaction_mode</code> affects
        future authorization only. It neither approves pending input nor reverses
        authorized actions. Status 5 means the turn is not initialized or no longer
        running; do not display the new mode as applied. All engine calls must be
        serialized against destruction.</p>
        <h3 id="request-reference">Read-only request methods</h3>
        <table><thead><tr><th>Method</th><th>Parameters and result</th></tr></thead><tbody>
            <tr><td><code>ping</code></td><td>No parameters; runtime name and protocol version.</td></tr>
            <tr><td><code>sessions.list</code></td><td>No parameters; visible session summaries with archive/runtime status, filtered to the current gateway boundary.</td></tr>
            <tr><td><code>sessions.show</code></td><td>Required session <code>id</code>; optional integer <code>before</code> and <code>limit</code>. Default limit 50, clamped to 1–200; returns a history page after checking the same authority boundary.</td></tr>
            <tr><td><code>models.list</code></td><td>Required <code>cwd</code>, optional <code>sessionId</code>; catalog is resolved under the current credential and gateway identity.</td></tr>
            <tr><td><code>turn.agents</code></td><td>Optional <code>turnId</code>, required while multiple turns run. Returns the selected running agent snapshot: path, status, model and steps (state/title/detail). With no selected turn and none running, returns an empty array. An explicitly inactive ID fails. This is a snapshot, not a subscription.</td></tr>
        </tbody></table>
        <p>An unknown method produces an asynchronous <code>ok: false</code> response.
        Do not infer supported methods from CLI slash commands. The source decoders are
        <code>NativeRequest.hs</code> and <code>NativeRequestHandler.hs</code> under
        <code>packages/agent-native-bridge/ffi/Agent/CLI/MacOS/</code>.</p>
        <h3 id="task-lifecycle">Observe admission, execution and cancellation</h3>
        <p>After submitting, consume turn events by turn ID. Use
        <code>ha_engine_list_tasks</code> to reconcile pending UI rows: callback status
        0 supplies task ID, nullable session ID and state (0 queued, 1 running);
        status 1 completes the list and -1 reports failure. A new task can have no
        session ID yet. <code>ha_engine_set_task_limit(engine, 2)</code>, for example,
        limits future cross-session scheduling to two tasks; allowed limits are
        1–32, default 3. Lowering it does not cancel existing tasks.</p>
        <p>Pass the copied task ID to <code>ha_engine_cancel_task</code>. Return 0 only
        accepts the cancellation command, even if the task has already ended. Keep
        observing terminal events and inspect completed effects before retrying work.
        List callbacks run on the engine command worker; do not synchronously destroy
        the engine from them.</p>
        <h2 id="conversation-workflows">Find, inspect and transfer a conversation</h2>
        <ol>
            <li>Call <code>ha_engine_search_conversations</code> with a query and limit,
            clamped to 1–100. Active and archived conversations are searched; deleted
            ones are excluded. Collect status 0 rows until terminal 1 or failure -1.</li>
            <li>Use the chosen session ID, not its display title, for
            <code>ha_engine_session_rename</code>, <code>ha_engine_session_archive</code>
            or <code>ha_engine_session_delete</code>. Present destructive deletion
            separately from reversible archive. A zero immediate return accepts the
            request; update the UI only after the result callback succeeds.</li>
            <li>For an anchored transcript, call <code>ha_session_load_around</code>
            with session ID, center turn index and radius (clamped to 500). Status 0 carries a turn,
            status 1 completes the page, -1 fails. Only terminal success supplies
            meaningful <code>has_older</code> and <code>has_newer</code>. Usage -1 means
            unreported, not zero. Preserve transcript effects and extensible response
            item JSON rather than reducing everything to assistant text.</li>
            <li>Fork with <code>ha_session_fork</code> through the inclusive durable turn index and
            use the new session ID returned by the transfer result. Export with
            <code>ha_session_export</code>: concatenate status 0 chunks into one
            version-1 <code>haskell-agent.session-transfer</code> JSON document only
            after terminal 1. Discard partial files on -1.</li>
            <li>Import that complete document through <code>ha_session_import</code>
            and use its returned ID. Treat exports as sensitive conversation data,
            not an executable script. Import does not authorize replaying tool calls.</li>
        </ol>
        <h3 id="session-observation">Observe a CLI-owned session without taking ownership</h3>
        <p>Start <code>ha_session_observation_start</code> with the session ID, callback
        and output-handle slot. Return 0 owns a nonnull handle; 1 is invalid input and
        2 initialization failure, with no callback. Callbacks may arrive before start
        returns. Copy each batch into a temporary UI projection: RESET replaces the
        live turn, events update it, and READY atomically publishes it. Every event
        in the batch shares a sequence; sequences can skip but never decrease within
        an owner instance. A changed owner identifies a different CLI process.</p>
        <p><code>generation_start</code> identifies compaction and
        <code>durable_turn_count</code> the saved boundary. PERSISTED advances that
        boundary only after a write; do not duplicate live content into saved history.
        TOOL_OUTPUT replaces provisional output and TOOL_RETRACTED removes its card.
        RESPONSE_DISCARDED removes display items since RESPONSE_RESTARTED or RESET.
        RESET/READY low flag bits indicate running, waiting, completed or interrupted;
        bit 8 means truncated catch-up. Ignore unknown bits.</p>
        <p>UNAVAILABLE and DISCONNECTED are reconnecting, nonterminal states.
        CANCELLED and FAILURE are terminal. Cancel stops observation, not the CLI.
        Destroy the handle exactly once outside its callback; destruction joins it
        and guarantees no later callback. This read-only interface provides neither
        steering nor approval authority over the observed process.</p>
        <h2 id="human-interaction">Implement questions and plan decisions</h2>
        <p>Install <code>ha_engine_set_interaction_callback</code> before submitting a
        turn. Copy the turn ID, interaction ID, prompt and all option labels, then
        dispatch UI work asynchronously. The callback must return promptly; returning
        does not answer the question. Kind 1 is plan entry (enter/stay), 2 plan exit
        (approve/request changes/cancel), and 3 a question.</p>
        <p>For example, a free-text question is resolved with the copied IDs,
        <code>selected_index = -1</code> and nonempty answer bytes; -1 with empty
        text cancels. Ordinary choices use zero-based indices. Plan-exit option 1
        carries change-request notes in custom text. Status 4 means absent, resolved
        or out-of-range: dismiss/refetch instead of submitting another approval.
        Replacing/clearing the callback waits for in-flight invocation and cancels
        unanswered interactions; never replace it reentrantly.</p>
        <p>Tool approvals use a separate JSON path. An <code>approval.requested</code>
        event identifies the turn and an approval object with id, callId, name,
        summary, arguments, argumentsEncrypted, async, truncated and onceOnly.
        Show the exact request, respecting encrypted/truncated fields, and send
        <code>approval.resolve</code> with <code>approvalId</code> and decision
        <code>allow_once</code>, <code>allow_tool</code> or <code>deny</code>.
        A once-only request accepts only allow_once or deny; allow_once must also
        carry <code>onceOnly: true</code> to confirm client support. A stale or
        duplicate resolution fails instead of applying to another request.</p>
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
        <h2 id="account-workflow">Connect and manage a provider account</h2>
        <p>Start with <code>ha_accounts_list</code>. Status 0 rows include provider,
        billing, selection ID, account ID, label/detail, managed ID, source, enabled
        and <code>can_manage</code>; status 1 ends the list and -1 fails it. Usage
        windows follow their account row and use its selection ID; reset timestamps
        are Unix seconds. Externally discovered accounts can be visible but not
        manageable. Never pass a display label where a managed ID is required.</p>
        <ol>
            <li>For OAuth, call <code>ha_account_oauth_start</code> with the provider.
            On challenge status 0 copy the verification URL, user code, device IDs,
            polling interval and expiry; open the URL without logging its secrets.</li>
            <li>Pass those challenge fields back to <code>ha_account_oauth_poll</code>.
            Result status 1 is pending, 0 success, -1 error. Preserve callback
            storage through each outstanding call; stopping the UI's polling must
            not free a callback still in flight.</li>
            <li>For API-key authentication, use <code>ha_account_api_key_connect</code>
            with protected input, not a conversation. After successful connection,
            reload the account list and verify the intended provider and identity.</li>
            <li>For a manageable row, use its managed ID with
            <code>ha_account_set_enabled</code> or <code>ha_account_delete</code>.
            Re-list after success. Removing a local credential is not revocation at
            the provider; use the provider's controls when revocation is required.</li>
        </ol>
        <h2 id="gateway-workflow">Connect an organization gateway</h2>
        <p>These process-global calls need no engine. First inspect
        <code>ha_gateway_status</code>: 0 connected with base URL, 1 disconnected,
        -1 error. <code>ha_gateway_account</code> separately provides organization
        ID/name, user name and optional PNG; copy and validate the image before use.</p>
        <ol>
            <li>Call <code>ha_gateway_connect_start</code> with the selected base URL
            and client name. Challenge status 0 is not connected yet. Display its
            verification URI/user code and preserve the device code privately.</li>
            <li>Poll with the same base URL/device code. Status 0 is authorized,
            1 pending, 2 slow down, -1 failure. Honor a nonzero replacement retry
            interval and stop at expiry rather than continuously restarting login.</li>
            <li>Alternatively, a browser authorization flow uses
            <code>ha_gateway_connect_exchange</code> with client ID, authorization
            code, PKCE verifier and redirect URI. Successful exchange validates and
            persists the credential inside the runtime; no bearer token is returned.</li>
            <li>Re-read status/account before loading organization data. Disconnect
            with <code>ha_gateway_disconnect</code>, then discard organization-bound
            UI projections. Do not mutate gateway credentials from an integration
            callback: workers retain an authority boundary until cleanup.</li>
        </ol>
        <h2 id="mcp-connection-workflow">Manage a named remote MCP connection</h2>
        <p>List with <code>ha_mcp_connections_list</code> or its icon variant and retain
        the latest revision. A connection ID is immutable and distinct from both its
        display name and endpoint; two accounts may share an endpoint. Status 0 is
        a row, 1 terminal success, -1 failure, -2 cancelled. State codes are
        configured=0, connecting=1, authorization-required=2, ready=3, failed=4.
        Only ready confirms initialization/tool discovery.</p>
        <ol>
            <li>Create using revision, display name and endpoint. An accepted operation
            returns an owned handle. Wait for its row and terminal callback, then
            destroy the handle off the UI thread.</li>
            <li>Authorize using the returned ID and fresh revision. Open the URLs
            delivered to the authorization callback without logging/persisting them.
            The runtime owns the loopback listener and timeout.</li>
            <li>Rename, set enabled, or remove using the latest revision. A conflict
            returns the current revision: reload and let the user review the new
            state, not blindly retry a mutation.</li>
            <li>Cancel is nonblocking; destroy cancels and joins. Do not destroy from
            an operation callback. Cancellation may follow a committed catalog
            change, so reload before offering retry.</li>
        </ol>
        <p>Inputs are copied, nonempty UTF-8 without NUL, at most 1 MiB per text field.
        Immediate codes are 0 accepted, 1 missing callback/output, 2 invalid input,
        3 start failure. Rejected calls do not callback and leave the output handle
        null. Icon metadata is negotiated locally, not fetched by listing; treat
        icon URLs and bytes as untrusted content.</p>
        <h2 id="mcp-server-workflow">Edit a raw MCP server catalog entry</h2>
        <p>This is distinct from the named HTTP-connection workflow. List with
        <code>ha_mcp_servers_list</code>; argument and environment-key callbacks arrive
        before their row. Environment values are never returned. Read/status emits
        exactly one row or failure, not an additional completion item. Status means
        configured for the next turn, not a live process probe.</p>
        <ol>
            <li>Read the row and revision. Prepare command, argv slices, working
            directory, environment key/value entries and startup/request timeouts.</li>
            <li>Add or edit with the observed revision. Text is limited to 1 MiB
            each; arrays to 4096 entries. Environment values are write-only secrets.
            Edit preserves enabled state; use enable/disable explicitly.</li>
            <li>After successful mutation, ask
            <code>ha_engine_mcp_server_restart</code> to discard the engine's warm
            fleet. It is rejected while a turn is active. Wait until idle, re-read
            revision, and request restart again; the next turn starts the catalog.</li>
        </ol>
        <p>Do not report a successful catalog write as a successful server connection.
        A restart return 0 promises one result callback even during shutdown;
        return 3 after shutdown admission closes promises none.</p>
        <h2 id="integration-workflow">Drive distribution-specific connection setup</h2>
        <p><code>ha_engine_integration_admin_list</code> returns operation definitions;
        call <code>ha_engine_integration_admin_call</code> only for an advertised
        operation and its parameter schema. Its result is one JSON value (status 0)
        or error (-1), not conversational output. Keep marked sensitive fields out
        of logs. An empty distribution provider is a supported empty catalog, not
        permission to discover an alternative local credential.</p>
        <p>For typed setup, list/search connections, call
        <code>ha_engine_connection_begin</code> with a selected provider/identifier,
        and render the returned snapshot. Phases are catalog=0, search=1,
        credentials=2, challenge=3, redirect=4, selection=5, connected=6, waiting=7.
        Copy its session ID, fields and items. Submit identifier/value answers only
        for the current setup session; poll after its advertised milliseconds.
        Cancel abandons setup, while disconnect removes an established connection.</p>
        <p>Secret fields use kind 1 and are never echoed. The secure-store callback
        supplies exactly 32 key bytes for the opaque identity scope; do not derive
        the key from a user-facing title. Inputs are limited to 16 KiB per string
        and 64 answers. Return 0 accepts exactly one terminal callback; 1 is null
        engine, 2 invalid input, 3 closed engine. Keep both callback contexts until
        completion. Engine destruction cancels and joins accepted workers.</p>
        <h2 id="host-browser-computer">Provide browser and computer capabilities</h2>
        <p><strong>Browser:</strong> install both request and cancellation callbacks with
        <code>ha_engine_set_browser_callback</code> before starting a turn. Both null
        disables the capability; mixed nullability is invalid. Validate
        <code>struct_size</code>, operation, scope/call IDs and buffer bounds. Copy
        the request before dispatching to a web-view thread. For each accepted
        command, call its completion exactly once, including cancellation; a cancel
        notification is not completion. Return a supported failure status when the
        host cannot perform the operation, rather than inventing success.</p>
        <p>For example, NAVIGATE acts only in the designated browser scope and returns
        bounded text; SCREENSHOT returns PNG only on success. Text results are at
        most 256 KiB and PNG bytes at most 16 MiB; validate pixel limits from the
        header. Result buffers are copied during completion. Replacing registration
        waits for accepted requests to drain, so never replace/destroy from the host
        callback itself. Registration does not implement URL policy or user consent
        for the host.</p>
        <p><strong>Computer:</strong> register <code>ha_engine_set_computer_callback</code>
        and explicitly enable computer use on the turn. The version-3 host receives
        OPEN, LIST, BIND, OBSERVE_OR_ACT and CLOSE operations with bounded borrowed
        request/result buffers. Open supplies an owned host session token; subsequent
        operations must stay within that session's authorized target. Implement
        OPEN_ATTACHED by resolving the exact staged window token; if unavailable,
        fail without falling back to unrestricted OPEN. Optional capability queries
        must not capture pixels or change the target.</p>
        <p>Unlike browser replacement, existing computer sessions retain the old
        callback/context generation until their final CLOSE returns. Keep that
        storage alive after installing a replacement. Engine destruction stops
        workers and closes remaining sessions before releasing registrations.
        OS capture/accessibility permission and the host's consent UI still need
        testing in the actual application.</p>
        <h2 id="data-browser-workflow">Load a bounded custom-data preview</h2>
        <ol>
            <li>Call <code>ha_data_catalog_list</code> with the workspace path. Each
            object precedes its column records; completion is 2 and failure -1.
            Copy scope, table/view kind, object name and column types/nullability.</li>
            <li>Select a returned object and call <code>ha_data_rows_load</code>
            with its scope (user=0, repository=1, checkout=2), offset 0 and, for
            example, limit 50. Limits are 1–500 and offsets must be nonnegative.</li>
            <li>Assemble value callbacks by zero-based row and column. Kinds are
            null=0, text=1, JSON number=2, boolean=3, encoded JSON=4. Null has no
            bytes; do not render it as the literal string “null”.</li>
            <li>On terminal success 1 use row count and <code>has_more</code> to
            offer the next offset. Failure -1 invalidates the incomplete preview.
            Arbitrary SQL and server-side filter expressions are not parameters
            of this ABI; do not manufacture a query interface from object names.</li>
        </ol>
        <p>These operations are read-only and resolve objects through the custom-scope
        catalog. Immediate returns are 0 accepted, 1 missing callback, 2 invalid
        pointer/length, 3 invalid scope/name/offset/limit. Keep callback context until
        its single terminal event; schema metadata is not credential material.</p>
        <h2 id="skill-workflow">Inspect installed skills and revise learned skills</h2>
        <p>Installed filesystem skills and learned skills are separate resources.
        <code>ha_installed_skill_list</code> discovers bundled, personal and project
        skills using the session's precedence rules. Save the emitted absolute
        SKILL.md identity and use it with <code>ha_installed_skill_read</code>;
        this is not an arbitrary file reader. Both workspace and identity paths
        must be absolute UTF-8 without NUL, at most 32768 bytes. Status 0 is an
        item, 2 a discovery warning, 1 completion, -1 failure, -2 missing identity.
        Display warnings rather than implying the list is complete.</p>
        <ol>
            <li>List learned skills with the current workspace, selected scope and
            bounded limit (1–1000). Scope -1 lists applicable scopes; 0/1/2 select
            user/repository/checkout. Read revision 0 to obtain the current content.</li>
            <li>Create a new lower-case hyphenated slug with title, description,
            instructions and activation (always=0, relevant=1, manual=2). Optional
            applicability text is not secret storage.</li>
            <li>For update, archive, restore or rollback, pass the exact positive
            observed revision. Status -3 is a conflict containing the current
            revision: reload, compare, and get confirmation again. Never reinterpret
            the token as “latest”. History lists revisions; rollback selects a
            historical revision through the explicit operation.</li>
        </ol>
        <p>Learned read emits one item or error; list/history emit items then one
        terminal. Mutation status 0 succeeds; -1 fails, -2 is missing, -3 conflict,
        -4 already exists, -5 revision missing. Character bounds are slug 80,
        title 200, description 1000, applicability 2000, instructions 30000 and
        change summary 1000. These operations have no cancellation handle:
        closing a panel must not release its callback context before completion.</p>
        <h2 id="review-and-delivery">Repository review and delivery</h2>
        <p>Load a repository snapshot and diff before offering path/hunk application.
        Refresh after edits rather than applying stale selections. Commit, push and pull-request
        creation are separate mutations. Present the exact push/PR preview to the user and
        confirm that preview; if repository state changes, obtain a fresh preview instead
        of reusing stale confirmation data. Check remote status when a response is lost.
        Cancellation cannot establish that a remote server did not accept a push.</p>
        <h3 id="repository-mutation-workflow">Stage selected changes and commit</h3>
        <ol>
            <li>Call <code>ha_repository_snapshot</code>. It emits snapshot/root/HEAD
            fingerprints, changed-file rows, then one result. HEAD can be empty
            on an unborn branch; rename source paths can be absent.</li>
            <li>Call <code>ha_repository_diff</code> with the snapshot, exact changed
            path, and WORKTREE=0 or STAGED=1. Assemble ordered patch chunks
            (at most 64 KiB each) and number hunks in emitted order.</li>
            <li>After user review, call <code>ha_repository_apply_path</code> or
            <code>ha_repository_apply_hunks</code> with STAGE=0, UNSTAGE=1 or
            RESTORE=2. Pass hunk indices, not caller-authored patch text. Binary,
            deletion and rename diffs cannot use hunk mutation; restoring never
            deletes an untracked file.</li>
            <li>Reload the snapshot and staged diff before
            <code>ha_repository_commit</code>. Supply that snapshot and reviewed
            message. Callback status 0 succeeds; -1 fails, -2 means stale,
            -3 cancelled. A stale operation makes no repository modification.</li>
        </ol>
        <p>Paths must exactly match a changed path and be normalized repository-relative
        literals, not globs/pathspecs. Text bounds are 8 MiB each, 16 MiB combined,
        and at most 4096 hunks. Locks coordinate cooperating runtimes but do not
        protect against arbitrary direct file writes by another process. Refresh
        after external edits rather than promising that the UI snapshot is locked.</p>
        <h3 id="delivery-confirmation-workflow">Confirm an exact push or pull request</h3>
        <p>Read delivery status for the snapshot to show branch, HEAD, upstream and
        ahead/behind counts. Request <code>ha_repository_push_preview</code>, display
        its commit/destination, then pass its confirmation token to
        <code>ha_repository_push_confirm</code> only after approval. Tokens are random,
        one-use, in-memory and expire after ten minutes; they bind repository,
        snapshot/HEAD, upstream configuration and remote OID. The runtime rechecks
        them, uses a server-side lease and refuses history rewrites.</p>
        <p>For PR creation, preview with snapshot/base/title/body, show the exact
        content, and confirm its separate token. Bounds are base 1 KiB, title
        512 characters, body 1 MiB. Status -2 requires a fresh snapshot/preview;
        -4 means invalid, expired or used token. Neither is authority to silently
        generate a new approval. After an uncertain network response inspect remote
        state before retrying.</p>
        <p><code>ha_repository_pr_status</code> returns found=0, no PR=1,
        unavailable=-1 or cancelled=-3. State codes are open=1, draft=2,
        merged=3, closed=4; CI is unknown=0, none=1, pending=2, passed=3, failed=4.
        <code>ha_session_pr_status</code> lists at most 20 associated PRs and then
        terminal 1; an item state 0 retains a validated link whose current state
        is unavailable. <code>ha_pull_request_summary</code> accepts a canonical
        GitHub PR URL and returns review metadata, not a diff or credential.</p>
        <p><code>ha_repository_cancel_all</code> joins repository workers and stops
        new admission during its barrier; it must run outside repository callbacks.
        Accepted work gets one terminal result, with -3 if cancelled before terminal
        completion. It does not own repository-check handles and cannot undo a
        completed remote effect.</p>
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

turnRequestExample :: Text
turnRequestExample = "{\"id\":\"request-1\",\"method\":\"turn.start\",\"params\":{\"turnId\":\"turn-1\",\"prompt\":\"Explain this project without changing files.\",\"cwd\":\"/absolute/project\",\"worktree\":false,\"computerUse\":false}}"

engineExample :: Text
engineExample = "#include \"HaskellAgentBridge.h\"\n\
    \#include <pthread.h>\n\
    \#include <stdio.h>\n\
    \#include <string.h>\n\
    \#include <time.h>\n\
    \struct response_state {\n\
    \    pthread_mutex_t mutex;\n\
    \    pthread_cond_t condition;\n\
    \    int received;\n\
    \    size_t length;\n\
    \    uint8_t bytes[4096];\n\
    \};\n\
    \static void receive_event(void *context, const uint8_t *bytes, size_t length) {\n\
    \    struct response_state *state = context;\n\
    \    pthread_mutex_lock(&state->mutex);\n\
    \    if (!state->received) {\n\
    \        state->length = length <= sizeof(state->bytes) ? length : 0;\n\
    \        if (state->length != 0) memcpy(state->bytes, bytes, state->length);\n\
    \        state->received = 1;\n\
    \        pthread_cond_signal(&state->condition);\n\
    \    }\n\
    \    pthread_mutex_unlock(&state->mutex);\n\
    \}\n\
    \int main(void) {\n\
    \    struct response_state state = {\n\
    \        .mutex = PTHREAD_MUTEX_INITIALIZER,\n\
    \        .condition = PTHREAD_COND_INITIALIZER\n\
    \    };\n\
    \    const uint8_t request[] = \"{\\\"id\\\":\\\"probe\\\",\\\"method\\\":\\\"ping\\\"}\";\n\
    \    if (ha_runtime_init() != 0) return 1;\n\
    \    void *engine = ha_engine_create(receive_event, &state);\n\
    \    if (engine == NULL) { ha_runtime_exit(); return 1; }\n\
    \    int32_t status = ha_engine_send_json(engine, request, sizeof(request) - 1);\n\
    \    pthread_mutex_lock(&state.mutex);\n\
    \    struct timespec deadline;\n\
    \    int clock_status = clock_gettime(CLOCK_REALTIME, &deadline);\n\
    \    if (clock_status == 0) deadline.tv_sec += 5;\n\
    \    while (status == 0 && clock_status == 0 && !state.received) {\n\
    \        if (pthread_cond_timedwait(&state.condition, &state.mutex, &deadline) != 0) break;\n\
    \    }\n\
    \    int complete = state.received && state.length != 0;\n\
    \    if (complete) { fwrite(state.bytes, 1, state.length, stdout); putchar('\\n'); }\n\
    \    pthread_mutex_unlock(&state.mutex);\n\
    \    ha_engine_destroy(engine);\n\
    \    ha_runtime_exit();\n\
    \    pthread_cond_destroy(&state.condition);\n\
    \    pthread_mutex_destroy(&state.mutex);\n\
    \    return complete ? 0 : 1;\n\
    \}"
