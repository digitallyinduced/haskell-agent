{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.ParallelAgents (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/parallel-agents/"
    , pageTitle = "Worktrees and parallel agents"
    , pageDescription = "Isolate project changes and delegate bounded tasks to concurrent agents."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>For context inheritance, model selection, messages, cancellation, and result
        verification, see the <a href="/guides/agent-lifecycle/">agent lifecycle guide</a>.
        This page focuses on checkout isolation and worktree retention.</p>
        <h2 id="start-an-isolated-checkout">Start an isolated checkout</h2>
        <p>From a Git repository:</p>
        <pre><code class="language-sh">agent-cli --worktree</code></pre>
        <p>Or use <code>/worktree</code> to start a fresh session in a new managed worktree.
        <code>/copy-path</code> copies the active worktree path.</p>
        <p>By default, managed worktrees fetch and branch from the selected remote's latest
        default commit, <strong>not from your current uncommitted changes</strong>. Repositories
        without a remote use local <code>HEAD</code>.</p>
        <p>The remote is chosen from the current branch's configured remote, then
        <code>upstream</code>, <code>origin</code>, or the repository's sole remote. A fetch failure aborts
        creation instead of silently using a stale commit.</p>
        <p>To disable fetching, merge this setting into <code>~/.haskell-agent/config.json</code>:</p>
        <pre><code class="language-json">{"{\n  \"version\": 1,\n  \"worktree\": {\n    \"fetchLatestUpstream\": false\n  }\n}" :: Text}</code></pre>
        <p>The policy applies to initial worktrees, <code>/worktree</code>, and subagent worktrees.</p>
        <h2 id="delegate-independent-tasks">Delegate independent tasks</h2>
        <p>Ask for explicit boundaries:</p>
        <pre><code>{"Use one agent to inspect the parser tests and another to review the public API.\nNeither should edit files. Combine their findings before proposing changes." :: Text}</code></pre>
        <p>Use <code>/agents</code> to inspect agent activity. Configure the concurrent subagent cap:</p>
        <pre><code>/agents limit 3</code></pre>
        <p>Or set it at startup:</p>
        <pre><code class="language-sh">agent-cli --max-concurrent-agents 3</code></pre>
        <p>Separate research tasks are a good starting point. If multiple agents must
        edit, assign non-overlapping ownership or ask for separate worktrees. Shared
        checkouts do not automatically prevent conflicting edits.</p>
        <p><strong>Expected result for the research example:</strong> two scoped findings
        reports and a combined recommendation, with no file edits. Inspect
        <code>/agents</code> while work is running. If both agents are investigating the
        same files for the same purpose, ask the coordinating agent to narrow their
        assignments rather than increasing the concurrency limit.</p>
        <pre><code>Have the parser reviewer report test gaps only. Have the API reviewer report compatibility risks only. Combine both reports without editing files.</code></pre>
        <h2 id="branch-the-conversation">Branch the conversation</h2>
        <p><code>/fork</code> creates a peer session from the current chat. Explicitly request checkout
        isolation when needed:</p>
        <pre><code>/fork --worktree Investigate an alternative implementation.</code></pre>
        <p>Without an isolation flag, the dialog initially selects “Use a new worktree”;
        “Share current workspace” is the other choice. Escape cancels. Use
        <code>--no-worktree</code> only when both conversations should see the same files.
        The fork prints a new session ID and switches into that peer conversation;
        an optional directive becomes its starting instruction.</p>
        <ol>
            <li>Record the original ID with <code>/session</code> before forking.</li>
            <li>Try the alternative in the peer; inspect its diff and test results.</li>
            <li>Return with <code>/resume ORIGINAL_ID</code> and summarize the alternative.</li>
            <li>For an isolated checkout, review and deliberately integrate the desired
            commits or changes. Forking does not automatically merge them. For a shared
            checkout, the files have already changed for both conversations: avoid
            overlapping edits and inspect the current diff before continuing.</li>
        </ol>
        <p>Review how changes should be integrated before asking agents to merge or
        publish them.</p>
        <h2 id="retention-and-recovery">Retention and recovery</h2>
        <p>Managed worktrees are collected only when clean, incorporated into another
        branch, and inactive for at least 24 hours, subject to additional safety
        checks. Dirty and unmerged work does not expire simply because it is old.</p>
        <p>Preview cleanup without removing checkouts:</p>
        <pre><code class="language-sh">agent-cli worktree gc --dry-run</code></pre>
        <p>Protect a checkout while using it outside the agent:</p>
        <pre><code class="language-sh">agent-cli worktree protect /absolute/path/to/managed/worktree</code></pre>
        <p>Resuming a collected session restores its checkout. Recognized ignored build
        and cache directories are disposable and are not restored. Recovery depends
        on the original shared Git repository and local worktree registry; it is not
        a substitute for backups.</p>
        <h2 id="repair-the-worktree-base">Choose and repair the checkout base</h2>
        <p>Do not combine <code>--worktree</code> with <code>--resume</code>. Run from a
        Git repository, or set <code>--cwd</code> to one. A remote fetch failure aborts
        creation instead of silently using stale commits. Fix authentication, connectivity,
        or remote configuration before retrying.</p>
        <p>Default-branch discovery caches <code>refs/remotes/REMOTE/HEAD</code>. The server
        is queried when this reference is absent or invalid, or the cached branch no longer
        exists. If the remote changes its default but keeps the old branch, refresh the
        cache explicitly. For a new default of <code>main</code> on <code>origin</code>:</p>
        <pre><code class="language-sh">{"git fetch origin refs/heads/main:refs/remotes/origin/main &&\ngit remote set-head origin --auto" :: Text}</code></pre>
        <p>Substitute your actual remote and branch. Fetch first: <code>set-head --auto</code>
        requires the remote-tracking reference, which isolated agent fetches do not create.</p>
        <h2 id="collection-safety">Understand collection decisions</h2>
        <p>Inactivity means saved-session activity, not commit age or merge time.
        Automatic adoption requires managed-root location, reciprocal linked-Git metadata,
        and saved-session ownership evidence. Archived sessions and sessions in checkout
        subdirectories contribute activity. Missing or ambiguous evidence retains the
        checkout; adoption does not reset an old checkout's clock to today.</p>
        <p><code>agent-cli worktree enroll PATH</code> explicitly enrolls a checkout and
        starts its inactivity clock now. <code>worktree protect PATH</code> prevents
        collection; <code>worktree unprotect PATH</code> removes that protection.
        A saved conversation alone does not protect a checkout forever.</p>
        <p>Local incorporation proof requires exact ancestry into another surviving branch,
        excluding the checkout's own branch and upstream. An equal-tip copy alone is not
        enough except for the resolved default branch. Squash/rebase merges require
        authenticated GitHub evidence identifying a merged PR at the exact checkout HEAD,
        a locally available reachable merge commit, and preservation of changed paths'
        final content and modes. Ambiguity retains the checkout. GC does not fetch, so
        cached references can miss a recent merge.</p>
        <p>Explicitly ignored known caches such as <code>node_modules</code>,
        <code>dist-newstyle</code>, and <code>.venv</code> are disposable, as is an ignored
        <code>result</code> symlink directly into the Nix store. Known agent settings and
        generated OpenAI files are disposable only when byte-identical regular copies
        remain in the primary checkout. Other ignored data, including <code>.env</code>
        and local databases, prevents collection. Protect caches containing irreplaceable data.</p>
        <p>Use <code>worktree gc --dry-run</code> first. It reports simulated adoption,
        retained reasons, eligibility, and gross apparent checkout bytes without writing
        registry entries or snapshots. The estimate is not net freed disk space: filesystem
        sharing and snapshot overhead differ. Database failures defer adoption rather than
        guessing from directory age. Failed and not-examined candidates are separate from
        deliberately retained ones.</p>
        <h2 id="enroll-an-existing-checkout">Enroll an existing managed checkout</h2>
        <p>Enrollment is explicit consent to the retention policy, including the ignored-file
        exclusion; it is not a way to enroll an arbitrary repository directory. Pass the
        managed checkout root, not a subdirectory or a path containing traversal components.
        The checkout must be a genuine linked Git worktree under the managed root, with
        reciprocal Git metadata and no symlinked checkout/repository/administration paths.
        The primary checkout is rejected.</p>
        <p>Run <code>agent-cli worktree enroll /absolute/managed/checkout</code> only after
        inspecting the target. First enrollment starts its inactivity clock now and creates
        an unprotected record. Re-enrollment of the same identity preserves the existing
        record; it is not a request to reset that clock. Changed repository identity, broken
        metadata, or an active maintenance/session lease is an error. Close the owning
        session or repair the actual Git metadata before retrying; do not remove lease files
        to bypass an active owner.</p>
        <h2 id="restore-a-checkout">Restore a collected checkout</h2>
        <pre><code class="language-sh">agent-cli worktree restore /absolute/path/to/managed/worktree</code></pre>
        <p>Restoration recreates a detached HEAD and does not reset a branch that moved
        since collection. It refuses an existing destination directory. An interrupted
        restore that leaves a directory requires manual inspection and recovery rather
        than overwriting it. Retained Git recovery references preserve historical commits,
        including checkout reflogs and recovery state; disposable caches are not restored.</p>
        <p>Keep the original shared repository and
        <code>~/.haskell-agent/worktrees/.registry</code>. Neither local snapshots nor
        conversation export is an off-machine backup. Active leases, unsupported Git
        states, active merge/rebase operations, incomplete snapshots, or detected concurrent
        edits block deletion. External editors do not participate in leases: protect their
        checkouts, and stop pre-upgrade agent processes before explicit collection.</p>
    |]
    }
