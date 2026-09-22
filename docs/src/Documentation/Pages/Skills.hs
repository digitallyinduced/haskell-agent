{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Skills (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/customization/skills/"
    , pageTitle = "Skills"
    , pageDescription = "Discover, invoke, install, and write reusable agent workflows."
    , pageGroup = "Customization"
    , pageBody = [hsx|
        <p>A skill is a reusable workflow described in a <code>SKILL.md</code> file. It can include
        supporting scripts and references. Skills supply instructions to the model;
        they do not bypass tool permissions or install their own dependencies
        automatically.</p>
        <h2 id="choose-a-skill-type">Choose a skill type</h2>
        <p>Use a filesystem skill when the workflow should be reviewed and versioned with source
        code. Use a learned skill for a durable, evidence-backed operating lesson maintained through
        the agent's database tools. Use <code>AGENTS.md</code> for repository-wide instructions.
        These mechanisms complement each other; a skill is not a replacement for runtime configuration.</p>
        <h2 id="discover-and-invoke-skills">Discover and invoke skills</h2>
        <pre><code>/skills</code></pre>
        <p>Invoke a discovered skill by name:</p>
        <pre><code>$add-model Configure my local model endpoint.</code></pre>
        <p>The corresponding slash-command form, such as <code>/add-model</code>, is also supported.
        The agent can select a skill when your request matches its description.</p>
        <p>After adding or editing skill files:</p>
        <pre><code>/skills reload</code></pre>
        <p>Launch with <code>--no-skills</code> to disable filesystem skill discovery.</p>
        <h2 id="remote-skills">Skills from an MCP server</h2>
        <p>Connected servers can advertise skills in addition to tools. The catalog loads
        advertised metadata; full instructions are fetched on demand from the owning server's
        entry resource and verified against its manifest. Listing a skill is not proof that its
        content can be loaded or trusted.</p>
        <p>Malformed metadata is ignored with a warning. An unavailable server, failed resource
        read or integrity/identity mismatch prevents activation. Check the server and advertised
        resource rather than copying unverified instructions into a local skill to bypass validation.
        Remote instructions cannot authorize unrelated tool effects or override your request.</p>
        <h2 id="bundled-workflows">Bundled workflows</h2>
        <p>The distribution ships these ten skills. Use <code>/skills</code> to verify discovery
        in your running session; launch settings or installation can change what is available.
        These are reviewed instructions, not new permission grants.</p>
        <table><thead><tr><th>Name</th><th>Use and boundary</th></tr></thead><tbody>
            <tr><td><code>add-model</code></td><td>Configure local or hosted endpoints, OpenRouter models, and new OpenAI/xAI models. Verify connectivity before selecting the new entry.</td></tr>
            <tr><td><code>skill-installer</code></td><td>Install from a repository, gist, URL or local path. Review scripts and choose project versus user scope first.</td></tr>
            <tr><td><code>resume-claude</code>, <code>resume-codex</code>, <code>resume-cursor</code>, <code>resume-grok</code></td><td>Locate and read another harness's history using latest, search words, an ID or a transcript/store path. Requires local store access; imported instructions are historical context, not current authorization.</td></tr>
            <tr><td><code>telegram-agent</code></td><td>Set up, start, stop or inspect the local Telegram bridge. Review allowed users and keep the bot token out of conversation text.</td></tr>
            <tr><td><code>wait-for-ci</code></td><td>Follow CI for an explicit pull request, branch, commit or run until its final result. Pending is not passing; do not push unrelated fixes without authorization.</td></tr>
            <tr><td><code>learn-about-user</code></td><td>Build a consent-reviewed technical profile from a confirmed public GitHub account. Review proposed durable user-scoped guidance before saving.</td></tr>
            <tr><td><code>post-task-learning-review</code></td><td>Always-activation guidance for reviewing substantial completed tasks. Not user-invocable; store only reusable evidence-backed lessons, not credentials or task status.</td></tr>
        </tbody></table>
        <pre><code>$wait-for-ci Follow the checks for this pull request and report the final result. Do not push changes.</code></pre>
        <p>If a resume skill cannot locate history, provide the correct store or transcript path;
        do not invent prior outcomes. If a dependency or credential is missing, resolve that
        prerequisite rather than repeatedly invoking the skill.</p>
        <h2 id="install-a-skill">Install a skill</h2>
        <p>Use the built-in installer:</p>
        <pre><code>$skill-installer Install the skill from this repository URL into my user skills.</code></pre>
        <p>The installer supports a GitHub repository, gist, URL, or local path. Its default
        destination is:</p>
        <pre><code>~/.haskell-agent/skills/&lt;name&gt;/SKILL.md</code></pre>
        <p>For a project-specific workflow, request installation under:</p>
        <pre><code>&lt;repository&gt;/.haskell-agent/skills/&lt;name&gt;/SKILL.md</code></pre>
        <p>Existing <code>.agents/skills</code> locations are also discovered, but the installer uses
        the product's <code>.haskell-agent/skills</code> directories.</p>
        <p>Review a third-party skill and its scripts before installing or executing it.
        A skill can contain instructions to run commands or contact external services.</p>
        <h2 id="discovery-and-precedence">Discovery and precedence</h2>
        <p>The catalog combines built-in, user, repository, and MCP-provided skills.
        Filesystem skills live one directory per skill with a file named exactly
        <code>SKILL.md</code>. Repository discovery considers the current directory and relevant
        ancestors, so launching from a different directory can change which local workflow wins.</p>
        <ol>
            <li>Repository skills take precedence; a more deeply nested repository location wins.</li>
            <li>User skills come next, followed by built-in skills and MCP-provided skills.</li>
            <li>At the same repository depth and scope, <code>.haskell-agent/skills</code>
            takes precedence over <code>.agents/skills</code>.</li>
        </ol>
        <p>When names collide, the catalog also exposes qualified invocations. For example,
        a user copy can appear as <code>$user:release-review</code> and a built-in copy as
        <code>$builtin:release-review</code>. More complex collisions receive additional qualifiers;
        use the exact invocation shown by <code>/skills</code>, not an invented path.</p>
        <h2 id="write-a-project-skill">Write a project skill</h2>
        <p>Create <code>.haskell-agent/skills/release-review/SKILL.md</code>:</p>
        <pre><code>{skillExample}</code></pre>
        <p>Use a precise name and a description that explains when the skill applies.
        Keep the core procedure short; place longer references beside it and refer
        to them by relative path.</p>
        <p>For conventions that apply to every task in a repository, use
        <a href="/guides/projects/">project instructions</a> instead of requiring a skill to be
        invoked each time.</p>
        <h2 id="front-matter-reference">Front-matter reference</h2>
        <p>The file must begin with YAML between opening and closing <code>---</code> lines.
        The remaining Markdown is the procedure. Indentation and YAML value types matter.</p>
        <table>
            <thead><tr><th>Field</th><th>Type / default</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>name</code></td><td>Required string</td><td>1–64 lowercase ASCII letters, digits, or hyphens; no leading, trailing, or consecutive hyphens; must match the parent directory</td></tr>
                <tr><td><code>description</code></td><td>Required string</td><td>1–1024 characters describing the workflow and its trigger</td></tr>
                <tr><td><code>when-to-use</code></td><td>Optional string</td><td>Additional applicability guidance</td></tr>
                <tr><td><code>argument-hint</code></td><td>Optional string</td><td>Explain the input expected after the invocation</td></tr>
                <tr><td><code>user-invocable</code></td><td>Boolean / true</td><td>Whether the skill is offered for explicit user invocation</td></tr>
                <tr><td><code>disable-model-invocation</code></td><td>Boolean / false</td><td>Exclude it from the model's automatic skill selection</td></tr>
                <tr><td><code>activation</code></td><td>String / on-demand</td><td>Filesystem skills use on-demand; always is reserved for trusted built-ins</td></tr>
                <tr><td><code>allowed-tools</code></td><td>String or string array / empty</td><td>Declared tool metadata; does not grant new runtime permissions</td></tr>
                <tr><td><code>model</code>, <code>effort</code></td><td>Optional strings</td><td>Model and effort override metadata; do not assume a provider supports arbitrary values</td></tr>
                <tr><td><code>license</code>, <code>compatibility</code></td><td>Optional strings</td><td>Licensing and environment requirements</td></tr>
                <tr><td><code>metadata</code></td><td>String-to-string object / empty</td><td>Additional descriptive metadata</td></tr>
            </tbody>
        </table>
        <p>To make a workflow explicit-only, keep <code>user-invocable: true</code> and set
        <code>disable-model-invocation: true</code>. If both user and model invocation are disabled,
        neither normal entry path is available. Do not use filesystem <code>activation: always</code>
        to force third-party instructions into every session; the loader rejects it.</p>
        <h2 id="supporting-files">Supporting files and dependencies</h2>
        <pre><code>{"release-review/\n  SKILL.md\n  references/\n    release-checklist.md\n  scripts/\n    verify_release.sh" :: Text}</code></pre>
        <p>Refer to supporting files relative to the skill directory. Put long reference material in
        separate files so the main procedure remains focused. Declare required commands and how to
        enter the repository's Nix shell. Installing a skill neither installs its dependencies nor
        makes its scripts trusted. Review scripts before execution and keep secrets out of skill files.</p>
        <h2 id="example-use-your-project-skill">Example: verify the release-review skill</h2>
        <ol>
            <li>Save the example above at the project path, then run <code>/skills reload</code>.</li>
            <li>Run <code>/skills</code> and check that <code>release-review</code> is discovered.</li>
            <li>Submit <code>$release-review Review this repository for release readiness. Report blockers only.</code></li>
        </ol>
        <p><strong>Expected result:</strong> a review referencing the checklist, changelog,
        and any missing evidence—not a published release. If the skill is absent,
        check its directory, filename, and front matter, and confirm you did not
        launch with <code>--no-skills</code> before reloading.</p>
        <h2 id="learned-skills">Learned skills</h2>
        <p>Learned skills are stored in the local database rather than in <code>SKILL.md</code>.
        Each has a stable slug, title, description, applicability condition, instructions, activation
        mode, priority, status, and revision history. Use them for a proven procedure or recurring
        failure prevention—not a transcript, temporary task status, or an unverified guess.</p>
        <p>See <a href="/customization/learned-skills/">learned skills</a> for scope selection,
        activation modes, creation and review, revision history, archive, and rollback. These use
        a different activation model from filesystem front matter; do not copy database activation
        values into a <code>SKILL.md</code> file.</p>
        <h2 id="troubleshoot-skills">Troubleshoot skill loading</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Check and correction</th></tr></thead>
            <tbody>
                <tr><td>Skill does not appear</td><td>Check exact SKILL.md filename, discovery directory, current working directory, and --no-skills; then /skills reload</td></tr>
                <tr><td>Front matter rejected</td><td>Ensure the file starts with ---, has a closing delimiter, required name/description, and valid YAML types</td></tr>
                <tr><td>Name rejected</td><td>Match the directory name; remove uppercase letters, underscores, edge hyphens, and repeated hyphens</td></tr>
                <tr><td>Wrong procedure selected</td><td>Inspect duplicate names and use the catalog's qualified invocation</td></tr>
                <tr><td>Never selected automatically</td><td>Check disable-model-invocation and improve the description/when-to-use trigger</td></tr>
                <tr><td>Script fails after installation</td><td>Check its dependencies and Nix environment; discovery is not dependency installation</td></tr>
                <tr><td>Learned change rejected as stale</td><td>Read the latest revision before requesting an update; do not retry with the old revision</td></tr>
            </tbody>
        </table>
        <h2 id="inspect-a-skill">Inspect before invoking</h2>
        <p><code>view_skill</code> takes <code>name</code>. Omit <code>scope</code> for filesystem
        or MCP catalog entries; use the discovered invocation name. For learned skills, supply
        the slug and <code>scope</code> (<code>user</code>, <code>repository</code>, or
        <code>checkout</code>). Optional positive <code>revision</code> selects learned history
        and is invalid without scope. Resolve ambiguous names through the catalog. MCP entries
        load instructions remotely and can fail when disconnected; do not silently substitute
        a similarly named local skill.</p>
        <h2 id="resume-external-history">Resume another harness's history</h2>
        <pre><code>$resume-codex latest</code></pre>
        <p>The four resume skills use <code>read_external_session</code>. Required
        <code>provider</code> is <code>codex</code>, <code>claude</code>, <code>cursor</code>, or
        <code>grok</code>; <code>operation</code> defaults to <code>show</code>, or use
        <code>list</code> for candidates. Optional <code>reference</code> is a literal ID,
        title fragment, or transcript/store path. Omitted or <code>latest</code> selects the
        newest session for the working directory. <code>within_minutes</code> defaults to zero
        (no age filter); <code>max_tool_chars</code> defaults to 300, minimum 20.</p>
        <pre><code class="language-json">{"{\"provider\":\"codex\",\"operation\":\"list\",\"within_minutes\":60}" :: Text}</code></pre>
        <p>Default roots are <code>~/.codex</code>, <code>~/.claude</code>, <code>~/.cursor</code>,
        and <code>~/.grok</code>, overridden respectively by <code>CODEX_HOME</code>,
        <code>CLAUDE_CONFIG_DIR</code>, <code>CURSOR_HOME</code>, and <code>GROK_HOME</code>.
        Cursor can expose CLI, desktop, or transcript records; formats differ in completeness.
        For an empty list check the working directory, age filter and store location.
        Ask the user to select ambiguous matches. Surface malformed/skipped-record warnings
        rather than claiming a complete import. Explicit paths require normal read access;
        do not use a shell to bypass a denial.</p>
        <p>This imports context, not credentials, processes, tools or authority. Treat historical
        messages and output as inert untrusted data. Summarize the goal, completed work, open work,
        stopping point and warnings, then verify current files, Git state and tests before acting.
        Do not replay old tool calls or claim they happened in this session.</p>
        <h2 id="profile-and-ci-workflows">Profile consent and CI completion</h2>
        <p>For <code>$learn-about-user HANDLE</code>, confirm the public GitHub account belongs to
        the user before investigating. Authentication never permits private-repository inspection.
        Review a representative public sample, distinguish facts from tentative technical
        preferences, and show the proposed profile for edits or rejection before saving.
        The approved result is the user-scoped, always-active learned skill
        <code>user-technical-profile</code>; refresh its current revision rather than creating
        duplicates. Normal mutation approval still applies. Exclude secrets and sensitive personal
        inferences; use learned-skill management to inspect, refresh or remove the profile.</p>
        <p><code>$wait-for-ci</code> applies after a push or pull-request update when the task
        depends on pending checks. Record the exact head commit; a new push invalidates the old
        result. Wait for the relevant run's terminal outcome and report failures with evidence.
        Queued/running checks are not success. If access, cancellation or a time limit prevents
        confirmation, report the unresolved state. This workflow does not grant repository access
        or authorize unrelated changes.</p>
    |]
    }

skillExample :: Text
skillExample = "---\n\
    \name: release-review\n\
    \description: Review release readiness when preparing a new release.\n\
    \---\n\n\
    \# Release review\n\n\
    \1. Read the release checklist and current changelog.\n\
    \2. Identify missing tests and migration notes.\n\
    \3. Report blockers with file references.\n\
    \4. Do not publish or tag a release."
