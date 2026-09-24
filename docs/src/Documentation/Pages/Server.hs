{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Server (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

page :: Page
page = Page
    { pagePath = "/reference/server/"
    , pageTitle = "HTTP server"
    , pageDescription = "Run the REST and event-stream interface, manage sessions and approvals, and configure secure deployment."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p><code>agent-server</code> exposes the runtime over HTTP. It is an API server, not
        a browser conversation application. Sessions are durable PostgreSQL records; turn
        execution and event replay have bounded process-local state.</p>
        <h2 id="start">Start a local server</h2>
        <p>From a checkout with model credentials configured for the current user:</p>
        <pre><code class="language-sh">{"nix run .#agent-server -- --workspace-root /absolute/path/to/project\n# In another terminal:\ncurl --fail http://127.0.0.1:4096/healthz\ncurl --fail http://127.0.0.1:4096/readyz\ncurl --fail http://127.0.0.1:4096/openapi.json" :: Text}</code></pre>
        <p>Health reports that the HTTP application responds; readiness also checks its backend.
        A ready server does not establish that every model account has available quota.
        Without an explicit workspace root, the current directory is the local allowed root.
        Requested directories are canonicalized; symlinks cannot escape an allowed root.</p>
        <h2 id="first-turn">Create a session and a turn</h2>
        <p>List models first and substitute an available identifier. Replace the path and
        <code>SESSION_ID</code> with your project and the returned session identifier:</p>
        <pre><code class="language-sh">{workflow}</code></pre>
        <p>Creation with a repository may return before checkout preparation finishes. Observe
        <code>session.setup.started</code>, step, completed, or failed events. The first turn
        waits for setup. Only one turn may be active in a session; independent sessions can
        run concurrently. Turns move from queued to running, optionally waiting for input,
        and finally completed, failed, or cancelled.</p>
        <h2 id="request-fields">Session and turn request fields</h2>
        <p>A basic session request accepts <code>cwd</code>, <code>model</code>,
        <code>title</code> and <code>effort</code> (none, low, medium, high, xhigh or max,
        subject to model support). Patch accepts exactly one of <code>title</code> or
        <code>archived</code>. Fork accepts <code>throughTurn</code> (nonnegative integer),
        <code>title</code> and <code>cwd</code>; inspect the installed schema and server
        validation for allowed combinations.</p>
        <p>Turn creation accepts <code>input</code>, optional UUID
        <code>clientRequestId</code>, <code>images</code> and <code>files</code>.
        Supply a stable client request UUID for idempotent creation and retain it when
        retrying that same admission. Do not assign a new identifier merely because a
        network response was lost.</p>
        <p>An image entry has <code>mimeType</code> and base64 <code>data</code>; supported
        MIME types are JPEG, PNG, GIF, WebP and BMP, with at most one image. File entries
        have <code>name</code>, <code>mimeType</code> and base64 <code>data</code>.
        Images and files together are limited to five attachments and 20 MiB decoded;
        encoded JSON must also fit the server's request limit. Opaque files are materialized
        temporarily under the session directory. Unknown request properties are rejected.</p>
        <h2 id="schemas">Response schemas and client decoding</h2>
        <p>Download the server's <code>/openapi.json</code> from the same running release.
        It describes 17 paths (some support several methods) and 17 named schemas.
        A <a href="/agent-server-openapi.json">local OpenAPI reference copy</a> is bundled
        with this documentation for offline use; the running server's copy takes precedence
        when its release differs.
        Resolve <code>$ref</code> under <code>components.schemas</code>; a nullable field
        is different from an omitted optional property. Preserve unknown fields when
        storing objects and tolerate additions in responses.</p>
        <table><thead><tr><th>Schema</th><th>Fields and interpretation</th></tr></thead><tbody>
            <tr><td>Model</td><td>Required string <code>id</code>, <code>provider</code>, <code>connection</code>, <code>transportModel</code>, <code>dialect</code>; nullable <code>label</code> and integer <code>contextWindow</code>. Submit the catalog ID, not a guessed transport model.</td></tr>
            <tr><td>Session</td><td>String <code>id</code>, timestamps <code>createdAt</code>/<code>updatedAt</code>, provider/connection/model/dialect/cwd/effort/title; Boolean <code>titleIsManual</code>/<code>archived</code>; nullable transportModel. <code>usage</code> has integer input/output/cached token counts.</td></tr>
            <tr><td>SessionPage</td><td><code>data</code> array of Session and nullable string <code>nextCursor</code>. A null cursor ends pagination; do not fabricate it from a session identifier.</td></tr>
            <tr><td>HistoryPage</td><td><code>session</code>, <code>data</code> array of integer <code>index</code> plus <code>turn</code>, integer <code>generationStart</code>/<code>total</code>, Boolean <code>hasOlder</code>/<code>hasNewer</code>, nullable integer <code>nextCursor</code>. Turn <code>items</code> is canonical history; <code>displayItems</code> is rendering-only failed partial output.</td></tr>
            <tr><td>Turn</td><td>UUID <code>id</code>/<code>clientRequestId</code>, string <code>sessionId</code>, status queued/running/waiting_for_input/completed/failed/cancelled, createdAt, nullable startedAt/finishedAt/error, input and userText. Terminal status does not imply success.</td></tr>
            <tr><td>TurnResult / TurnOutput / TurnCompletion</td><td>Result contains <code>turn</code> and nullable <code>output</code>. Output has responseId, nullable assistantText, assistantTextTruncated, completion. Completion status is completed/incomplete, with optional reason and nullable reasoningTokens. Do not present truncated text as the complete canonical transcript.</td></tr>
            <tr><td>Agent</td><td>String path/status, nullable model, array of step objects. Steps are not fully constrained by OpenAPI; render defensively rather than assuming one provider's shape.</td></tr>
            <tr><td>HumanRequest / ResolveRequest</td><td>UUID id/turnId, sessionId, kind, prompt, string options, createdAt. Resolve with required string decision and optional string value. Use the currently advertised request and choices; never infer permission from free-form assistant output.</td></tr>
            <tr><td>ErrorEnvelope</td><td><code>error</code> contains string code/message/requestId and optional arbitrary details. Retain requestId for diagnostics; redact credentials and private content before sharing details.</td></tr>
        </tbody></table>
        <p>The remaining named request schemas are <code>CreateSession</code>,
        <code>PatchSession</code>, <code>ForkSession</code> and <code>CreateTurn</code>,
        described above and in the mutation section. OpenAPI currently leaves nested
        history turns and agent steps partly open; it is not an exhaustive schema for
        every provider event. Consult <code>Agent.Server.Types</code> in the matching
        checkout when implementing those nested objects, and keep SSE decoding separate
        from REST response decoding.</p>
        <pre><code class="language-sh">{"curl --fail http://127.0.0.1:4096/openapi.json -o agent-server-openapi.json\njq '.components.schemas.TurnResult' agent-server-openapi.json\njq '.paths[\"/v1/sessions/{sessionId}/turns\"]' agent-server-openapi.json" :: Text}</code></pre>
        <h2 id="routes">Route reference</h2>
        <p>All paths below are relative to the server URL. The server's
        <code>/openapi.json</code> is the machine-readable request and response reference for
        the installed revision. Do not forward arbitrary CLI arguments through JSON.</p>
        <table><thead><tr><th>Method and path</th><th>Operation</th></tr></thead>
        <tbody>{foldMap routeRow routes}</tbody></table>
        <h2 id="history-and-mutations">History and session changes</h2>
        <p>Session listing accepts <code>archive=active</code> and <code>limit=50</code>.
        History also accepts a limit; use each response's <code>nextCursor</code> as the
        next request's cursor. History separates canonical <code>items</code> from
        display-only <code>displayItems</code>. Never send failed display-only output back
        as model context. Patch one field per request to rename or archive. Mutation and
        fork requests can return <code>409 session_busy</code> while a turn is active.
        Historical forks inherit their title and directory and can be renamed afterward.</p>
        <h2 id="human-requests">Approvals and other input</h2>
        <p>Fetch <code>/v1/requests</code>, inspect the complete prompt and advertised options,
        and resolve the intended request. Use the option supplied by that request rather
        than assuming every request accepts the same decision. For an approval advertising
        <code>allow_once</code>:</p>
        <pre><code class="language-sh">{"curl --fail http://127.0.0.1:4096/v1/requests\ncurl --fail -X POST http://127.0.0.1:4096/v1/requests/REQUEST_ID/resolve \\\n  -H 'Content-Type: application/json' -d '{\"decision\":\"allow_once\"}'" :: Text}</code></pre>
        <p>Plan feedback can include a <code>value</code>. Requests over 64 KiB encoded JSON or
        100 options are rejected, not truncated. Do not approve an unseen suffix. If a
        request disappeared or was resolved elsewhere, refresh it rather than replaying
        a stale decision. Cancel work with a POST to its turn's <code>/cancel</code>;
        cancellation does not undo completed file or remote-service mutations.</p>
        <h2 id="events">Events and reconnecting</h2>
        <pre><code class="language-sh">{"curl --no-buffer http://127.0.0.1:4096/v1/events\n# Reconnect using the last SSE id that your client applied:\ncurl --no-buffer -H 'Last-Event-ID: 42' http://127.0.0.1:4096/v1/events" :: Text}</code></pre>
        <p>Persist the last applied event identifier. On <code>replay.reset</code>, refetch
        sessions, turns and requests before continuing: retention expiry or a slow consumer
        can invalidate incremental state. Retry/discard/failure/tool-retraction events
        include display-only boundaries. The latest 1,000 terminal turn records are retained
        in process memory; durable history is separate. Do not depend on a server restart
        preserving an active turn or the same replay window.</p>
        <h3 id="event-payloads">SSE envelope and payload reference</h3>
        <p>Ordinary SSE records contain an integer <code>id</code>, an
        <code>event</code> name and JSON <code>data</code>. That JSON is the envelope:
        <code>id</code>, <code>type</code>, nullable string <code>turnId</code> and
        <code>sessionId</code>, timestamp <code>at</code>, and the nested <code>data</code>
        payload described below. Unknown future event names should not crash the
        client. Apply known events before advancing your saved cursor; do not mistake
        a keepalive comment for an application event.</p>
        <p><code>replay.reset</code> is a control exception: its data is directly
        <code>{"{\"reason\":\"event_gap\",\"refetch\":true}" :: Text}</code>, with no
        ordinary envelope or new cursor. Discard incremental assumptions and refetch
        before applying subsequent events. Checkout step statuses are
        <code>running</code>, <code>completed</code> and <code>failed</code>.</p>
        <table><thead><tr><th>Event names</th><th>Nested data fields</th></tr></thead><tbody>
            <tr><td><code>turn.queued</code>, <code>turn.started</code>, <code>agent.turn.started</code></td><td>Empty object. Supervisor turn lifecycle differs from individual agent/model turn lifecycle.</td></tr>
            <tr><td><code>turn.completed</code>, <code>turn.failed</code>, <code>turn.cancelled</code></td><td>Nullable string <code>error</code>; fetch the turn result rather than treating this as its full output.</td></tr>
            <tr><td><code>response.text.delta</code>, <code>response.reasoning.delta</code>, <code>turn.activity</code>, <code>warning</code></td><td>String <code>text</code>, Boolean <code>truncated</code>. Plan deltas also use response.text.delta.</td></tr>
            <tr><td><code>provider.limit</code></td><td>String <code>text</code>, Boolean <code>warning</code> and <code>truncated</code>.</td></tr>
            <tr><td><code>response.restarted</code></td><td>String <code>reason</code>, Boolean <code>truncated</code>, <code>displayOnly: true</code>.</td></tr>
            <tr><td><code>agent.turn.finished</code></td><td>String <code>responseId</code>, nullable string <code>assistantText</code>, Boolean <code>assistantTextTruncated</code>, <code>usage</code>, <code>completion</code>.</td></tr>
            <tr><td><code>tool.started</code>, <code>tool.updated</code>, <code>tool.arguments.updated</code></td><td>Strings <code>callId</code>, <code>name</code>, <code>kind</code>; Booleans <code>argumentsEncrypted</code>, <code>async</code>, <code>argumentsTruncated</code>; nullable string <code>arguments</code>, null when encrypted.</td></tr>
            <tr><td><code>tool.output.updated</code></td><td>Strings <code>name</code>, <code>output</code>, Boolean <code>truncated</code>.</td></tr>
            <tr><td><code>tool.finished</code></td><td>Strings <code>callId</code>, <code>kind</code>, <code>output</code>; Booleans <code>async</code>, <code>truncated</code>; integer <code>imageCount</code>. Image URLs/data are deliberately omitted.</td></tr>
            <tr><td><code>tool.retracted</code></td><td>String <code>callId</code> and <code>displayOnly: true</code>.</td></tr>
            <tr><td><code>response.attempt.discarded</code>, <code>response.attempt.failed</code>, <code>model.context.reset</code></td><td><code>displayOnly: true</code>; update presentation state, never append discarded material to canonical model history.</td></tr>
            <tr><td><code>agent.started</code></td><td>Strings <code>agentId</code>, <code>label</code>; nullable strings <code>parentId</code>, <code>model</code>.</td></tr>
            <tr><td><code>agent.output</code></td><td>Strings <code>agentId</code>, <code>output</code>, Boolean <code>truncated</code>.</td></tr>
            <tr><td><code>agent.finished</code></td><td>String <code>agentId</code>; <code>status</code> is running, completed, failed or cancelled.</td></tr>
            <tr><td><code>request.created</code></td><td>The HumanRequest object described above.</td></tr>
            <tr><td><code>request.resolved</code></td><td>String <code>requestId</code>; remove that pending request and refetch if your state is stale.</td></tr>
            <tr><td><code>session.setup.started</code>, <code>session.setup.completed</code></td><td>Strings <code>repository</code> and <code>branch</code>.</td></tr>
            <tr><td><code>session.setup.step</code></td><td>Checkout operation <code>id</code>, <code>command</code> and <code>status</code>.</td></tr>
            <tr><td><code>session.setup.failed</code></td><td>String <code>message</code>; inspect checkout diagnostics before another setup attempt.</td></tr>
        </tbody></table>
        <p><code>usage</code> contains integer <code>inputTokens</code>,
        <code>outputTokens</code> and <code>cachedTokens</code>. Completion has
        <code>status: completed</code>, or <code>status: incomplete</code> with string
        <code>reason</code> and nullable <code>reasoningTokens</code>. Tool kinds are
        <code>function</code>, <code>custom</code>, <code>computer</code> or
        <code>computer_function</code>. Public event text is bounded to 16,383
        characters before an ellipsis; honor each truncation flag rather than
        presenting a shortened result as complete.</p>
        <h3 id="agent-snapshots">Agent snapshot details</h3>
        <p>Each public agent has string <code>path</code> and <code>status</code>,
        nullable string <code>model</code>, and <code>steps</code>. Each step has
        <code>state</code> (running, completed, failed or info), string <code>title</code>
        and nullable string <code>detail</code>. At most 100 agents and 100 steps per
        agent are projected. Transcripts and retained UI state are deliberately
        omitted, not missing data that another query parameter can enable.</p>
        <p>The public JSON projection has a 64 Ki-character text budget and 2,048-node
        budget. It redacts encrypted-content/encrypted-function-argument keys and may mark an
        object <code>projectionTruncated: true</code>. Clients must tolerate shortened
        arrays and omitted fields; this endpoint is a bounded status view, not a
        lossless session export.</p>
        <h3 id="nested-history">Nested history records</h3>
        <p>Each history <code>data</code> entry has an integer <code>index</code> and
        a <code>turn</code> object. The turn contains <code>at</code> (UTC timestamp),
        <code>userText</code>, nullable <code>assistantText</code>, <code>error</code>,
        <code>responseId</code>, <code>effect</code>, <code>items</code>,
        <code>displayItems</code>, nullable <code>usage</code>, and
        <code>providerTelemetry</code>. Effects are <code>append</code>,
        <code>replace</code> or <code>reset</code>; do not flatten them into an
        append-only model transcript. Usage here uses <code>input</code>,
        <code>output</code>, <code>cached</code>, unlike SSE's token-counter names.</p>
        <p>On the latest page, queued/running/waiting turns with nonblank input can
        appear as provisional entries with an extra <code>status</code> field,
        empty item arrays, null usage and no assistant text. Their indices and the
        overlaid total are not a substitute for durable completion. Refetch after
        completion rather than appending a second copy of the provisional prompt.</p>
        <p>Both item arrays contain tagged Responses objects, not plain strings.
        Messages use <code>type: message</code>, <code>role</code> and
        <code>content</code>; function calls use <code>call_id</code>, <code>name</code>
        and a JSON-encoded <code>arguments</code> string; outputs correlate by
        <code>call_id</code>. Other tags represent custom/computer calls, reasoning,
        references, agent messages, additional tools, local shell, tool search,
        web search, image generation and compaction. Preserve unknown tags as
        opaque data rather than treating them as assistant text or executable
        instructions. Content parts can represent images/files as well as text;
        do not automatically fetch embedded URLs.</p>
        <p>This is a bounded public projection, not a lossless provider archive:
        encrypted-content/encrypted-function-argument keys are redacted, strings and nested
        structures can be cut, and an object can carry <code>projectionTruncated:
        true</code>. The recursive budget is 65,536 text characters and 2,048
        nodes; individual strings are limited to 16,383 characters. Clients must
        tolerate missing/truncated nested fields and never replay this projection
        as canonical model input. Redaction matches <code>encrypted_content</code>,
        <code>encryptedcontent</code>, <code>encrypted_function_args</code> and
        <code>encryptedfunctionargs</code>, case-insensitively, replacing their values with
        <code>&lt;redacted&gt;</code>. Ordinary <code>arguments</code>, text and tool output
        are not secret-scrubbed. Apply the session's access controls to the whole response.</p>
        <h3 id="history-item-reference">History item field reference</h3>
        <p>These are the current typed encodings in <code>Agent.Responses.Types.Items</code>,
        not a closed provider schema. A question mark below means optional before projection;
        after projection, even normally present fields can be absent. Unless stated otherwise,
        identifiers, names, status values and textual payloads are strings. Item status usually
        means <code>in_progress</code>, <code>completed</code> or <code>incomplete</code>;
        preserve unfamiliar values instead of mapping them to success. Item completion is
        not the same as completion of the enclosing turn.</p>
        <table><thead><tr><th>Item type</th><th>Fields before public projection</th></tr></thead>
        <tbody>{foldMap routeRow historyItemFields}</tbody></table>
        <p>Computer actions are tagged objects: <code>screenshot</code> and <code>wait</code>
        have no additional typed fields; <code>click</code> has integer <code>x</code>,
        <code>y</code>, string <code>button</code> and string-array <code>keys</code>;
        <code>double_click</code> and <code>move</code> have coordinates and keys;
        <code>type</code> has <code>text</code>; <code>keypress</code> has keys;
        <code>scroll</code> adds integer <code>scroll_x</code> and <code>scroll_y</code>;
        <code>drag</code> has <code>path</code>, an array of integer x/y points, and keys.
        A safety check has <code>id</code>, optional <code>code</code> and
        <code>message</code>, with extension fields permitted. Historical actions and
        acknowledgments are display records, never permission to perform them again.</p>
        <p>Message metadata <code>internal_chat_message_metadata_passthrough</code>, when
        present, contains optional <code>turn_id</code>, arbitrary JSON <code>create_time</code>
        and <code>executed_tool_calls</code>, and string-array <code>content_item_kinds</code>.
        Do not use these internal hints as an authorization or stable pagination contract.</p>
        <p>The registry also recognizes <code>file_search_call</code>,
        <code>code_interpreter_call</code>, <code>local_shell_call_output</code>,
        <code>shell_call</code>, <code>shell_call_output</code>, <code>apply_patch_call</code>,
        <code>apply_patch_call_output</code>, <code>mcp_list_tools</code>,
        <code>mcp_approval_request</code>, <code>mcp_approval_response</code>,
        <code>mcp_call</code>, <code>program</code> and <code>program_output</code>.
        They have no typed payload model here: the current <code>TaggedObject</code>
        decoder/encoder retains only <code>type</code>. Unknown item, content and action tags
        use the same type-only fallback. Do not invent fields from another provider's API
        manual or claim that decoding and re-encoding retains arbitrary provider data.
        A client may retain the raw JSON it actually receives, subject to its own storage
        policy, but cannot recover fields already discarded upstream.</p>
        <h3 id="history-content-reference">Message content field reference</h3>
        <p>A message's <code>content</code> is either a string or an array of tagged parts.
        Agent-message content is an array. Reasoning content, when present, is also an array.
        Roles include <code>user</code>, <code>assistant</code>, <code>system</code> and
        <code>developer</code>, with unknown roles retained. Keep these roles distinct in
        the display; a historical system message does not instruct the client application.</p>
        <table><thead><tr><th>Content type</th><th>Fields before public projection</th></tr></thead>
        <tbody>{foldMap routeRow historyContentFields}</tbody></table>
        <p>Raw JSON fields such as annotations, log probabilities, audio descriptors,
        search tools and tool outputs intentionally have no exhaustive nested schema in
        this library. Display only shapes your application implements; offer an unsupported
        content placeholder otherwise. Never automatically navigate a URL, render supplied
        HTML, decode an unbounded image, play audio or execute a tool from history.</p>
        <h3 id="history-consumer">Example: display assistant text without replaying tools</h3>
        <p>Save the following as <code>HistoryDisplay.hs</code> and load it with
        <code>nix develop .#docs -c ghci HistoryDisplay.hs</code>. Pass a decoded item from
        the history response to <code>assistantText</code>; render each returned
        <code>Text</code> through your UI's escaping API, not as HTML. Empty output means
        “no supported assistant text,” not an empty successful response.</p>
        <pre><code class="language-haskell">{historyDisplayExample}</code></pre>
        <ol>
            <li>Fetch a history page and retain its turn indices. Refetch provisional entries
            after completion; do not blindly append them again.</li>
            <li>Check truncation markers and show an incomplete-record notice. Keep
            <code>items</code> and <code>displayItems</code> separate rather than assuming both
            arrays form one canonical transcript.</li>
            <li>For a normal assistant message, the example returns its string content or
            supported text parts. It ignores calls, tool output, images, reasoning and unknown
            tags; give those separate, non-executing UI representations where implemented.</li>
            <li>Test at least a text message, unknown tag, non-object item, missing content,
            image part, redacted value and truncated object. None should trigger a network
            fetch, tool execution or failure of the entire page.</li>
        </ol>
        <p>This is a source-reviewed display example, not an authenticated server-client
        integration test. Pin the runtime revision and revisit the tables when its
        <code>Items</code>, <code>Items.Known</code> or <code>Content</code> types change.</p>
        <p>Telemetry entries contain nullable <code>duration_ms</code>,
        <code>api_duration_ms</code>, <code>cost_usd</code>, <code>stop_reason</code>,
        <code>provider_turns</code>, <code>structured_output</code>, and a
        <code>models</code> map. Each model entry has <code>input_tokens</code>,
        <code>output_tokens</code>, <code>cache_read_input_tokens</code>,
        <code>cache_creation_input_tokens</code>, and nullable
        <code>web_search_requests</code>, <code>cost_usd</code>,
        <code>context_window</code>, <code>max_output_tokens</code>,
        <code>canonical_model</code>, <code>provider</code>. Null means unreported,
        not zero cost.</p>
        <h2 id="authentication">Authentication and network exposure</h2>
        <p>Default loopback mode validates the Host header and rejects browser origins unless
        explicitly allowed. A non-loopback bind requires <code>--allow-remote</code> and
        authentication. In single-user mode choose exactly one token source:
        <code>AGENT_SERVER_TOKEN</code> or <code>--token-file</code>. A token on loopback also
        enables bearer authentication. Use a regular non-symlink owner-only token file;
        do not put a secret literal in shell history, a URL, or the Nix store.</p>
        <pre><code class="language-sh">{"nix run .#agent-server -- --host 127.0.0.1 --token-file \"$HOME/.config/haskell-agent/server-token\"" :: Text}</code></pre>
        <p>Clients send <code>Authorization: Bearer …</code>. The server does not terminate TLS.
        Keep it behind trusted TLS termination, preserve authentication and SSE streaming,
        and configure the exact permitted browser origin. CORS is not authentication.
        Organization gateway identity is an additional boundary; switching credentials
        invalidates operations that no longer belong to the admitted identity.</p>
        <h2 id="options">All server options</h2>
        <p>Flags are read at startup. Numeric capacities must be positive; tenant limits
        cannot exceed their corresponding global limits. Restart with a reviewed configuration
        to change them. Paths below must satisfy the relevant ownership and canonical-path checks.</p>
        <table><thead><tr><th>Option</th><th>Default</th><th>Meaning</th></tr></thead>
        <tbody>{foldMap optionRow options}</tbody></table>
        <h2 id="tenants">Multi-tenant deployment</h2>
        <p>Use the exported <code>nixosModules.agent-server</code> module, not a hand-launched
        sandbox runner. It establishes the dedicated account, trusted immutable runner,
        cgroup delegation and service confinement that the runner checks. Configure
        <code>services.haskell-agent.server</code> with <code>enable</code>,
        <code>tenantRegistryFile</code>, explicit <code>workspaceRoots</code>, and sufficient
        <code>maxActiveTenants</code>. Provision credentials outside the Nix store.</p>
        <pre><code class="language-json">{tenantRegistry}</code></pre>
        <p>Tenant and credential IDs are canonical UUIDs. Registry and credential files are
        regular non-symlink mode-0600 files owned by the service user. Tokens contain at least
        32 bytes and are unique. Workspace roots must exist, be non-overlapping, match the
        module allowlist, and exclude credentials and server state. Parent paths must be
        root- or server-owned and not group/other writable. Use canonical absolute non-root
        paths without dot components or systemd percent specifiers.</p>
        <p>Each tenant receives a separate database and restricted database role. Model-controlled
        execution runs in a tenant gVisor sandbox with writable workspace and guest state;
        provider and database credentials remain on the host. Sandbox tools are auto-approved
        by default, but host mutations such as MCP retain their approval policy. Plan and
        dangerous-command restrictions still apply. Network access means a shell command
        can have external effects even inside a sandbox: do not provision unintended production
        credentials there. Failed sandbox admission never falls back to host execution.</p>
        <p>Operators still own TLS, rate limits, disk/database quotas and backups. Each sandbox
        process tree is limited to two CPUs, 2 GiB RAM without swap and 512 processes.
        Outbound networking denies private/host/metadata destinations; no inbound service
        or SSH is provided. Review the repository's <code>docs/agent-server.md</code> and
        <code>nix/modules/agent-server.nix</code> when deploying the pinned revision.</p>
        <h3 id="tenant-provisioning">Provision a tenant safely</h3>
        <ol>
            <li>Pin a supported Linux flake revision and import <code>nixosModules.agent-server</code>.
            Set the service's registry path and explicit workspace allowlist; retain
            the dedicated default <code>haskell-agent-server</code> user/group unless
            you deliberately provision equivalent isolation.</li>
            <li>Create disjoint canonical workspace directories and private runtime
            secret files before starting the service. Generate independent high-entropy
            tokens through your secret manager, not literal Nix strings. Install
            registry and token files as service-owned mode 0600 regular files,
            with protected parents. Use the registry example above with newly
            assigned canonical tenant and credential UUIDs.</li>
            <li>Deploy the reviewed NixOS configuration. The module supplies the private
            state directory (default <code>/var/lib/haskell-agent-server</code>),
            trusted generation-specific runner and cgroup confinement. Do not copy a
            mutable runner into place or launch it manually to bypass admission.</li>
            <li>Before enabling external traffic, check readiness and authenticated
            protected reads for each tenant using privately provisioned clients.
            Confirm an invalid token is rejected and each tenant sees only its own
            sessions. Exercise sandbox admission with a bounded read-only task;
            this is a required deployment check, not a test claimed here.</li>
            <li>Configure TLS, rate limits, quotas and backups, then allow clients to
            submit work. Registry/token changes should use a controlled stop,
            secret deployment and restart; do not assume hot reload.</li>
        </ol>
        <h3 id="tenant-backup-recovery">Tenant backups and sandbox recovery</h3>
        <p>Back up the tenant workspace, persistent tenant state (including its
        home/guest state), and PostgreSQL database with its ownership/grants as a
        consistent recovery set. Preserve tenant UUIDs, registry mapping and the
        pinned service revision separately; protect credentials in the secret
        manager. A workspace-only backup cannot restore session history, and an
        SSE cursor cannot restore a process-memory turn.</p>
        <ol>
            <li>Quiesce submissions, settle or cancel active turns, reconcile possible
            external effects, and stop the service before filesystem snapshots.
            Use your PostgreSQL backup tooling for database consistency, not a live
            copy of database files.</li>
            <li>Restore first in an isolated environment at the matching revision.
            Restore workspace/state ownership and database roles/grants without
            broadening cross-tenant access. Re-provision private registry/token
            files; do not restore obsolete exposed tokens into production.</li>
            <li>Validate readiness, tenant isolation, durable session/history reads
            and sandbox admission before reopening submissions. Reconnect clients
            and refetch state; do not blindly replay pre-failure POST requests.</li>
        </ol>
        <p>If sandbox startup fails, inspect service logs, exact runner generation,
        path ownership and the delegated cpu/memory/pids cgroup boundary. The
        current runner is gVisor, not a microVM. A stale tenant cgroup blocks
        replacement launches. If cleanup cannot prove descendant quiescence, it
        fail-stops while retaining the tenant lock until the supervisor kills the
        complete process group. Stop and investigate the service boundary; never
        delete a live lock or enable host execution as a workaround. These are
        operator recovery procedures, not a claim of a tested backup/restore system.</p>
        <h2 id="haskell-client">Embed the Haskell client</h2>
        <p>Add the pinned <code>agent-server-client</code> package to your application's
        Cabal dependencies. The HTTP client is separate from the server executable.
        Provision an owner-only credential file for the intended server identity, then
        construct the client once and reuse it. This source-reviewed example lists pending
        human requests without approving them:</p>
        <pre><code>{"{-# LANGUAGE OverloadedStrings #-}\nimport Agent.Server.Client\n\nmain :: IO ()\nmain = do\n    created <- newAgentServerClient AgentServerClientConfig\n        { agentServerBaseUrl = \"http://127.0.0.1:4096\"\n        , agentServerCredentialFile = \"/absolute/private/server-token\"\n        }\n    case created of\n        Left err -> print err\n        Right client -> listAgentServerRequests client >>= print" :: Text}</code></pre>
        <p>Construction validates the URL and reads the credential file; it does not prove
        remote authorization. Never log the token or redirect an authenticated request to
        another authority. Handle <code>AgentServerCredentialError</code>,
        <code>AgentServerTransportError</code>, <code>AgentServerHttpError</code>,
        <code>AgentServerDecodeError</code> and <code>AgentServerProtocolError</code>
        separately: fixing local credentials differs from retrying a read after transport
        failure, and a decoding error can indicate a client/server revision mismatch.</p>
        <p>The client exports create-session/create-turn, turn/result/list/cancel,
        request-list/request-list-for-turn/resolve, history and turn-stream operations.
        Their request/response records are re-exported from
        <code>Agent.Server.Client.Protocol</code>. Use
        <code>streamAgentServerTurn client turnId lastEventId onEvent</code> with a callback
        that returns an error when it cannot apply an event. Handle completed, failed,
        cancelled and <code>AgentServerStreamNeedsRefetch</code> distinctly; refetch
        durable history/state after a replay gap instead of presenting a partial transcript
        as complete. Polling a result is not equivalent to resolving an approval.</p>
        <h2 id="token-rotation">Rotate a server token</h2>
        <ol>
            <li>Schedule an interruption; stop new submissions and let existing turns settle
            or cancel them and reconcile effects. Do not assume a token reload endpoint.</li>
            <li>Stop the server. Replace its configured token file using a secure secret
            deployment mechanism, retaining required ownership and mode; if using the
            environment source, replace that secret instead, not both sources.</li>
            <li>Update authorized clients' secret stores and restart the server. Recreate
            Haskell client objects because construction reads their bearer token.</li>
            <li>Verify a protected read succeeds with the new credential and rejects the old
            one. Health alone is not an authentication test. Reconnect event streams and
            reconcile current state before resuming submissions.</li>
        </ol>
        <p>This deliberately bounded maintenance procedure does not promise zero-downtime
        rotation. Preserve incident evidence if a credential was exposed and inspect
        previous requests and remote mutations rather than merely replacing the secret.</p>
        <h2 id="recovery">Failure recovery</h2>
        <ul>
            <li>401: check the chosen token source and authenticated tenant, not model credentials.</li>
            <li>403 or rejected workspace: check origin/Host and canonical workspace ownership.</li>
            <li>409 session_busy: inspect the active turn, wait or deliberately cancel it.</li>
            <li>Queue/subscriber limit: reduce concurrency; do not blindly resubmit a mutation.</li>
            <li>Readiness failure: inspect service logs and database access before accepting work.</li>
            <li>Uncertain POST outcome: query the session/turn before repeating an external action.</li>
        </ul>
        <p>Errors contain <code>error.code</code>, <code>error.message</code> and
        <code>error.requestId</code>. Retain the request ID, but redact tokens and sensitive
        prompt/tool content from reports. These procedures are source-verified, not a
        claim that a production tenant deployment was exercised during documentation checks.</p>
    |]
    }

routeRow :: (Text, Text) -> Html
routeRow (route, description) = [hsx|<tr><td><code>{route}</code></td><td>{description}</td></tr>|]

optionRow :: (Text, Text, Text) -> Html
optionRow (name, defaultValue, description) = [hsx|<tr><td><code>{name}</code></td><td>{defaultValue}</td><td>{description}</td></tr>|]

routes :: [(Text, Text)]
routes =
    [ ("GET /healthz", "HTTP health.")
    , ("GET /readyz", "Backend readiness.")
    , ("GET /openapi.json", "Installed OpenAPI schema.")
    , ("GET /v1/models", "Models visible to the authenticated boundary.")
    , ("GET /v1/sessions", "List sessions using keyset pagination.")
    , ("POST /v1/sessions", "Create a durable session and optional repository setup.")
    , ("GET /v1/sessions/:id", "Read a session.")
    , ("PATCH /v1/sessions/:id", "Rename or archive; one field per request.")
    , ("DELETE /v1/sessions/:id", "Delete an inactive session.")
    , ("GET /v1/sessions/:id/history", "Read paginated canonical and display history.")
    , ("POST /v1/sessions/:id/fork", "Fork current transcript or a durable turn boundary.")
    , ("POST /v1/sessions/:id/turns", "Queue a typed turn request.")
    , ("GET /v1/turns", "List retained turn execution records.")
    , ("GET /v1/turns/:id", "Inspect execution status.")
    , ("GET /v1/turns/:id/result", "Read the turn result.")
    , ("POST /v1/turns/:id/cancel", "Cancel queued, running or waiting work.")
    , ("GET /v1/turns/:id/agents", "Inspect child agents.")
    , ("GET /v1/requests", "List human-input requests.")
    , ("POST /v1/requests/:id/resolve", "Resolve an advertised request option.")
    , ("GET /v1/events", "Subscribe to replayable Server-Sent Events.")
    ]

options :: [(Text, Text, Text)]
options =
    [ ("--host", "127.0.0.1", "Listening address.")
    , ("--port", "4096", "Port 1–65535.")
    , ("--allow-remote", "false", "Permit non-loopback with authentication.")
    , ("--token-file", "none", "Owner-only single-user bearer file; alternative to AGENT_SERVER_TOKEN.")
    , ("--tenant-registry", "none", "Enable versioned multi-tenant credential registry.")
    , ("--tenant-state-root", "none", "Server-owned tenant storage.")
    , ("--sandbox-runner", "none", "Trusted runner; use the NixOS service boundary.")
    , ("--yolo", "false", "Auto-approve server-turn mutations; distinct from sandbox execution policy.")
    , ("--cors-origin", "none", "Repeat for each explicitly allowed browser origin.")
    , ("--workspace-root", "current directory", "Repeatable canonical local workspace root; registry controls tenant workspaces.")
    , ("--max-concurrent-turns", "3", "Global running-turn capacity.")
    , ("--max-concurrent-turns-per-tenant", "2", "Per-tenant running capacity.")
    , ("--max-queued-turns", "100", "Global queue capacity.")
    , ("--max-queued-turns-per-tenant", "25", "Per-tenant queue capacity.")
    , ("--max-active-tenants", "16", "Active tenant runtime capacity.")
    , ("--max-event-subscribers", "256", "Global SSE connections.")
    , ("--max-event-subscribers-per-tenant", "8", "Per-tenant SSE connections.")
    , ("--event-replay-limit", "1000", "Events retained per access boundary.")
    , ("--maximum-request-bytes", "33554432", "Maximum encoded JSON request body (32 MiB).")
    ]

workflow :: Text
workflow = "curl --fail http://127.0.0.1:4096/v1/models\n\
    \curl --fail -X POST http://127.0.0.1:4096/v1/sessions \\\n\
    \  -H 'Content-Type: application/json' \\\n\
    \  -d '{\"cwd\":\"/absolute/path/to/project\",\"model\":\"gpt-5.6-sol\"}'\n\
    \curl --fail -X POST http://127.0.0.1:4096/v1/sessions/SESSION_ID/turns \\\n\
    \  -H 'Content-Type: application/json' \\\n\
    \  -d '{\"input\":\"Explain the repository. Do not modify files.\"}'"

tenantRegistry :: Text
tenantRegistry = "{\"version\":1,\"tenants\":[{\"id\":\"018f6a14-7d52-7a52-9c00-66d5e7d70334\",\"workspaceRoot\":\"/srv/agent-workspaces/acme\",\"credentials\":[{\"id\":\"018f6a14-7d52-7a52-9c00-66d5e7d70335\",\"tokenFile\":\"/run/credentials/acme-agent-token\"}]}]}"

historyItemFields :: [(Text, Text)]
historyItemFields =
    [ ("message", "id?, role, content (string or parts), status?, phase?, internal_chat_message_metadata_passthrough?.")
    , ("agent_message", "id?, author?, recipient?, content (parts), internal_chat_message_metadata_passthrough?.")
    , ("function_call", "id?, call_id, name, namespace?, provider?, arguments (JSON-encoded string), encrypted_function_args? (string array before redaction), status?, async? (boolean).")
    , ("function_call_output", "id?, call_id, name?, namespace?, provider?, output (arbitrary JSON), status?, async? (boolean). Local execution outcome is not serialized here.")
    , ("custom_tool_call", "id?, call_id, name, namespace?, input (string), status?, async? (boolean).")
    , ("custom_tool_call_output", "id?, call_id, name?, output (arbitrary JSON), status?, async? (boolean).")
    , ("computer_call", "id?, call_id, actions (array), pending_safety_checks? (array), status?; extension fields may also be retained.")
    , ("computer_call_output", "id?, call_id, output ({type: computer_screenshot, image_url: string, detail: original}), acknowledged_safety_checks? (array), status?; extension fields may also be retained.")
    , ("reasoning", "id?, summary (array of {type: string, text?: string}), content? (parts), encrypted_content? (redacted), status?.")
    , ("item_reference", "id. A reference does not contain the referenced item's body.")
    , ("additional_tools", "id?, role, tools (array of arbitrary JSON tool definitions).")
    , ("local_shell_call", "id?, call_id?, status?, action?. The exec action has command (string array), timeout_ms? (integer), working_directory?, env? (string-to-string object), user?.")
    , ("tool_search_call", "id?, call_id?, status?, execution?, arguments? (arbitrary JSON, unlike function_call.arguments).")
    , ("tool_search_output", "id?, call_id?, status?, execution?, tools (array of arbitrary JSON).")
    , ("web_search_call", "id?, status?, action?. search has query? and queries? (string array); open_page has url?; find_in_page has url? and pattern?.")
    , ("image_generation_call", "id?, status?, revised_prompt?, result? (string). Treat the result as provider data, not an automatically displayable image.")
    , ("compaction / context_compaction", "id?, encrypted_content? (redacted). The decoder accepts compaction_summary as an alias of compaction; encoding normalizes it to compaction.")
    , ("compaction_trigger", "No additional typed fields.")
    ]

historyContentFields :: [(Text, Text)]
historyContentFields =
    [ ("input_text", "text; prompt_cache_breakpoint? (arbitrary JSON).")
    , ("output_text", "text; annotations? and logprobs? (arrays of arbitrary JSON).")
    , ("text / reasoning_text / summary_text", "text. Preserve the content type when choosing a display region.")
    , ("refusal", "refusal (string); do not infer refusal from text heuristics.")
    , ("input_image", "detail?, file_id?, image_url?, prompt_cache_breakpoint? (arbitrary JSON).")
    , ("input_file", "detail?, file_data?, file_id?, file_url?, filename?, prompt_cache_breakpoint? (arbitrary JSON).")
    , ("input_audio", "input_audio (arbitrary JSON).")
    , ("encrypted_content", "encrypted_content (redacted); no displayable plaintext is supplied.")
    ]

historyDisplayExample :: Text
historyDisplayExample = "{-# LANGUAGE OverloadedStrings #-}\n\
    \module HistoryDisplay (assistantText) where\n\
    \import Data.Aeson (Value(..))\n\
    \import qualified Data.Aeson.KeyMap as KeyMap\n\
    \import Data.Foldable (toList)\n\
    \import Data.Text (Text)\n\
    \\n\
    \assistantText :: Value -> [Text]\n\
    \assistantText (Object item)\n\
    \  | KeyMap.lookup \"type\" item == Just (String \"message\")\n\
    \  , KeyMap.lookup \"role\" item == Just (String \"assistant\") =\n\
    \      case KeyMap.lookup \"content\" item of\n\
    \        Just (String text) -> [text]\n\
    \        Just (Array parts) -> concatMap displayPart (toList parts)\n\
    \        _ -> []\n\
    \assistantText _ = []\n\
    \\n\
    \displayPart :: Value -> [Text]\n\
    \displayPart (Object part)\n\
    \  | Just (String kind) <- KeyMap.lookup \"type\" part\n\
    \  , kind `elem` [\"output_text\", \"text\"]\n\
    \  , Just (String text) <- KeyMap.lookup \"text\" part = [text]\n\
    \displayPart _ = []\n"
