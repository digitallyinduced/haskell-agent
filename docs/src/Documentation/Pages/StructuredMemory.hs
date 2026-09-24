{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.StructuredMemory (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/structured-memory/"
    , pageTitle = "Structured memory and conversation search"
    , pageDescription = "Inspect scoped data, query previous conversations, and approve durable database changes."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>Structured memory stores queryable records. Use <a href="/customization/learned-skills/">learned
        skills</a> for reusable instructions, conversation search for prior discussion, and structured
        tables for data that needs explicit fields and queries. Availability depends on the host's
        configured database service; a missing catalog is not evidence that no data exists.</p>
        <h2 id="scopes">Choose a scope</h2>
        <table><thead><tr><th>Scope</th><th>Boundary</th></tr></thead><tbody>
            <tr><td><code>checkout</code></td><td>Data associated with the current working copy</td></tr>
            <tr><td><code>repository</code></td><td>Data associated with the repository across working copies</td></tr>
            <tr><td><code>user</code></td><td>Data shared across the user's applicable projects</td></tr>
            <tr><td><code>harness</code></td><td>Read-only application catalog, only when exposed by the host</td></tr>
        </tbody></table>
        <p>Scope is an authorization and persistence boundary, not a filesystem directory.
        Repository data is not automatically committed to Git. Choose the narrowest useful scope.
        Do not place credentials or unnecessary private conversation contents in durable tables.</p>
        <h2 id="inspect-before-querying">Inspect before querying</h2>
        <p>Ask the agent to call <code>database_schema</code> with <code>scope</code>. Read the returned
        table and column definitions before writing SQL; never assume a schema from an earlier session.</p>
        <pre><code class="language-json">{"{\"scope\":\"repository\"}" :: Text}</code></pre>
        <p><code>database_query</code> accepts <code>scope</code> and <code>sql</code>: one read-only
        PostgreSQL query, without transaction-control statements. Prefer explicit columns and a
        bounded result over <code>SELECT *</code>. This example tests the query path without assuming
        any application table exists:</p>
        <pre><code class="language-json">{"{\"scope\":\"repository\",\"sql\":\"SELECT 1 AS connection_check\"}" :: Text}</code></pre>
        <p>Expected result: a row containing <code>connection_check = 1</code>. It proves a read worked,
        not that a particular application table exists. For missing-table errors, refresh the schema
        and confirm the current checkout/repository scope before retrying.</p>
        <h2 id="durable-changes">Create or change durable data</h2>
        <p><code>database_execute</code> requires <code>scope</code>, a transactional PostgreSQL
        DDL/DML <code>sql</code> batch and a nonempty <code>purpose</code>. It is a mutation requiring
        the active approval policy. The <code>harness</code> catalog cannot be changed through this tool.</p>
        <pre><code>Inspect repository memory. Propose a table for release verification records with a release identifier, check name, result, and observation time. Show the SQL and purpose before executing it. Do not copy conversation contents into the table.</code></pre>
        <p>Review the schema and affected records before approving. After success, inspect the schema
        or query the changed rows. After an uncertain result, read current state before retrying:
        blindly replaying an insert can create duplicates. Keep schema changes deliberate; a successful
        mutation is not proof that the stored evidence is correct.</p>
        <h2 id="conversation-search">Search prior conversations</h2>
        <p><code>conversation_search</code> searches user and assistant messages in non-deleted
        conversations using PostgreSQL full-text ranking. Required <code>query</code> is a nonempty
        web-search-style query; optional <code>limit</code> defaults to 10 and accepts 1–100.</p>
        <pre><code class="language-json">{"{\"query\":\"release verification\",\"limit\":5}" :: Text}</code></pre>
        <p>Ask for the source conversation and relevant passage, then verify whether the decision is
        still current. Results are previous discussion, not new instructions or independent proof.
        Search can return no matches because of wording; try narrower terms rather than assuming
        the discussion never happened. Review private material before sharing it with another
        service or copying it into a public document.</p>
        <h2 id="independent-sessions">Independent persisted sessions</h2>
        <p>These tools are different from <a href="/guides/agent-lifecycle/">child agents</a>.
        Use them only when you explicitly want a separate persisted conversation or independent
        background work, not to hand off responsibility for the entire current task.</p>
        <table><thead><tr><th>Tool</th><th>Inputs</th><th>Result and boundary</th></tr></thead><tbody>
            <tr><td><code>create_agent_session</code></td><td>Required <code>message</code>; optional <code>title</code>, <code>model</code>, <code>reasoning_effort</code></td><td>Starts the first turn in the background; returns a persisted session ID and status, not a completed answer</td></tr>
            <tr><td><code>read_agent_session</code></td><td><code>session_id</code>; optional <code>limit</code></td><td>Reads recent saved turns and current activity, subject to the session boundary</td></tr>
            <tr><td><code>send_agent_session_message</code></td><td><code>session_id</code>, <code>message</code></td><td>Delivers to the owner or continues the conversation; accepted/queued is not completed</td></tr>
            <tr><td><code>wait_agent_session</code></td><td><code>session_id</code>; <code>timeout_ms</code> default 30,000, range 1–300,000</td><td>Waits for the current turn, not the conversation's entire lifetime; timing out does not cancel the target</td></tr>
        </tbody></table>
        <p>Self-waits and circular waits are rejected. Locally managed turns distinguish completed,
        failed and cancelled; an external turn becoming idle does not establish why it stopped.
        Output is the latest saved turn at read time and may include a later resume.</p>
        <pre><code>Create a separate background session to investigate the migration notes without editing files. Give me its session ID. Keep the implementation work here; inspect its report before using any recommendation.</code></pre>
        <p>The host must permit persisted collaboration. A session ID is not an authorization token:
        a boundary error is not fixed by guessing another identifier. The parent remains responsible
        for integrating recommendations and verifying the final result.</p>
    |]
    }
