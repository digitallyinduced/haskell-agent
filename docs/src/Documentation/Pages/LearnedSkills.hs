{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.LearnedSkills (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/customization/learned-skills/"
    , pageTitle = "Learned skills"
    , pageDescription = "Keep evidence-backed working procedures across sessions with scoped, versioned learned skills."
    , pageGroup = "Customization"
    , pageBody = [hsx|
        <p>Learned skills store reusable instructions in the agent's durable database. Unlike
        <a href="/customization/skills/">filesystem skills</a>, they are managed through tools,
        retain revision history, and can apply to one checkout, a repository, or all your projects.
        They are for procedures and constraints—not a replacement for conversation history or a
        place to store credentials.</p>
        <h2 id="choose-a-scope">Choose a scope</h2>
        <table>
            <thead><tr><th>Scope</th><th>Use for</th><th>Example</th></tr></thead>
            <tbody>
                <tr><td><code>checkout</code></td><td>Instructions specific to the current working copy</td><td>A temporary test environment unique to this checkout</td></tr>
                <tr><td><code>repository</code></td><td>A procedure shared across the repository's clones and worktrees</td><td>How to verify a particular subsystem</td></tr>
                <tr><td><code>user</code></td><td>A stable preference that applies across projects</td><td>A preferred development feedback loop</td></tr>
            </tbody>
        </table>
        <p>Use the narrowest scope that remains useful. When instructions conflict, checkout
        guidance overrides repository guidance, which overrides user guidance. Repository scope
        does not mean a file is committed to Git; use <code>SKILL.md</code> or <code>AGENTS.md</code>
        when you need instructions distributed with source control.</p>
        <h2 id="activation-modes">Activation modes</h2>
        <table>
            <thead><tr><th>Mode</th><th>Behavior</th></tr></thead>
            <tbody>
                <tr><td><code>relevant</code></td><td>Default. Listed for task-based retrieval; full instructions are loaded when applicable</td></tr>
                <tr><td><code>always</code></td><td>Full instructions are included in every new applicable session; reserve this for stable, broadly necessary guidance</td></tr>
                <tr><td><code>manual</code></td><td>Listed for explicit retrieval rather than automatic task matching</td></tr>
            </tbody>
        </table>
        <p>Priority controls ordering and defaults to <code>0</code>; accepted values are integers
        from <code>-100</code> to <code>100</code>. Higher priority is not permission to override
        the user's current request or the repository's instructions.</p>
        <h2 id="capture-a-verified-procedure">Capture a verified procedure</h2>
        <ol>
            <li>Complete and verify the actual task first. Record the command, conditions, and result.</li>
            <li>Ask the agent to search existing learned skills before proposing a new one.</li>
            <li>Review the proposed scope, applicability, and exact instructions.</li>
            <li>Approve the creation only when the lesson is reusable and supported by evidence.</li>
            <li>Ask the agent to read it back with its scope and revision number.</li>
        </ol>
        <pre><code>Search existing learned skills for this repository's integration-test procedure. If none covers the verified procedure from this session, propose a repository-scoped relevant skill. Include the exact command and its prerequisites, and show me the instructions before saving.</code></pre>
        <p>This is a request to the agent, not a slash command. The model uses the learned-skill
        tools below. The expected result is a stored skill with a revision, not merely a promise
        that the model will remember. Mutations require approval under the active tool policy.</p>
        <h2 id="management-tools">Management tools</h2>
        <table>
            <thead><tr><th>Tool</th><th>Purpose</th><th>Important inputs</th></tr></thead>
            <tbody>
                <tr><td><code>skill_search</code></td><td>Search active skills in applicable scopes</td><td><code>query</code>; optional <code>limit</code> from 1 to 50, default 10</td></tr>
                <tr><td><code>view_skill</code></td><td>Read full instructions and history</td><td><code>name</code>, learned-skill <code>scope</code>, optional <code>revision</code></td></tr>
                <tr><td><code>skill_create</code></td><td>Create a versioned procedure</td><td>Scope, slug, title, description, applicability, instructions, change summary, evidence</td></tr>
                <tr><td><code>skill_update</code></td><td>Revise an existing procedure</td><td>Scope, slug, <code>expected_revision</code>, changed fields, change summary, evidence</td></tr>
                <tr><td><code>skill_archive</code></td><td>Remove an obsolete skill from active retrieval</td><td>Scope, slug, <code>expected_revision</code>, change summary, evidence</td></tr>
                <tr><td><code>skill_rollback</code></td><td>Restore an earlier revision's contents and status</td><td>Scope, slug, current <code>expected_revision</code>, earlier <code>target_revision</code>, change summary, evidence</td></tr>
            </tbody>
        </table>
        <p>A create request has this shape. This is a tool payload for reference—not a JSON file to
        place in your repository. Replace the illustrative evidence with an actual verified result.</p>
        <pre><code class="language-json">{creationExample}</code></pre>
        <h2 id="revise-and-restore">Revise and restore</h2>
        <p>Read the current revision before updating. The <code>expected_revision</code> check
        prevents one session from silently overwriting a newer edit from another. If the revision
        changed, reread the skill and reconcile the changes rather than blindly retrying.</p>
        <p>Archiving keeps the immutable history but excludes the skill from active search and
        future loading. Rollback creates a <em>new</em> revision containing the selected earlier
        contents and status; it does not erase intervening history. To restore an archived skill,
        select an earlier active revision.</p>
        <pre><code>Read repository skill integration-test-procedure and show its revision history. Compare the current instructions with revision 1. Do not change it until I confirm which procedure is correct.</code></pre>
        <h2 id="post-task-review">Automatic post-task review</h2>
        <p>The bundled <code>post-task-learning-review</code> guidance applies before the
        top-level agent finishes substantial work. It considers surprising failure causes,
        repository conventions, reliable checks and recurring preferences. It first searches
        existing learned skills and prefers updating one over creating a duplicate.</p>
        <p>The procedure allows at most two learned-skill mutations per task and requires a
        reusable, non-obvious, evidence-backed lesson likely to change future behavior.
        Ordinary task summaries, temporary state, speculation and already-documented repository
        instructions do not qualify. If nothing meaningful was learned, the expected outcome
        is no mutation and no review announcement. This guidance does not bypass approval.</p>
        <h2 id="verify-and-troubleshoot">Verify and troubleshoot</h2>
        <ul>
            <li><strong>Not found:</strong> confirm the scope and current repository/checkout.
            Search excludes archived skills; use the known name and scope to inspect history.</li>
            <li><strong>Not automatically applied:</strong> inspect activation and applicability.
            A manual skill is intentionally not selected just because a task seems related.</li>
            <li><strong>Duplicate guidance:</strong> update or archive an existing skill instead of
            creating another copy. Inspect narrower scopes for an override.</li>
            <li><strong>Old instructions remain in this conversation:</strong> archiving affects
            future retrieval, not text already included in conversation history. Explicitly correct
            the current session and verify behavior in a new session.</li>
        </ul>
        <p>Never store tokens, passwords, private message contents, or one-off task status as a
        learned procedure. Review evidence and instructions before approving a durable mutation.</p>
    |]
    }

creationExample :: Text
creationExample = "{\n  \"scope\": \"repository\",\n  \"slug\": \"integration-test-procedure\",\n  \"title\": \"Run integration tests in the project environment\",\n  \"description\": \"Use the verified project environment for integration tests.\",\n  \"applies_when\": \"Changing code covered by the integration suite.\",\n  \"instructions\": \"Enter the project Nix shell and run the verified integration-test command. Report failures before proceeding.\",\n  \"activation\": \"relevant\",\n  \"priority\": 0,\n  \"change_summary\": \"Record the verified test procedure.\",\n  \"evidence\": \"Replace with the actual command and observed result.\"\n}"
