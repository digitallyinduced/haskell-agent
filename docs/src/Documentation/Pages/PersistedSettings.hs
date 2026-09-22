{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.PersistedSettings (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/persisted-settings/"
    , pageTitle = "Persisted settings"
    , pageDescription = "Inspect remembered models, account choices, approval policy, and user preferences without confusing them with machine configuration."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>Interactive choices are saved separately from <code>config.json</code> and
        <code>models.json</code>. Prefer the corresponding command or selector to changing
        these records manually. In particular, a remembered model is not a model definition,
        and an account selection record is not a credential.</p>
        <h2 id="locations">Locations and ownership</h2>
        <table><thead><tr><th>Location</th><th>Purpose</th></tr></thead><tbody>
            <tr><td><code>&lt;checkout&gt;/.haskell-agent/settings.json</code></td><td>Checkout approval policy, concurrent-agent limit, remembered model and provider account selections</td></tr>
            <tr><td><code>~/.haskell-agent/settings.json</code></td><td>User-level remembered model, title-model selection and mouse-capture preference</td></tr>
        </tbody></table>
        <p>The checkout root is the current Git working tree's top level, including a linked
        worktree. Outside Git it is the working directory. Paths are canonicalized.
        Do not copy a full-access checkout policy into an untrusted repository.</p>
        <h2 id="root-fields">Root fields</h2>
        <table><thead><tr><th>Field</th><th>Type / default</th><th>Meaning</th></tr></thead><tbody>
            <tr><td><code>version</code></td><td>Integer / 1</td><td>Written schema version. Preserve it; this loader does not enforce the same version rejection as machine configuration.</td></tr>
            <tr><td><code>autoApprove</code></td><td>Boolean / false</td><td>Persistent checkout approval policy, controlled by /permissions or /always-approve</td></tr>
            <tr><td><code>mouseCapture</code></td><td>Boolean / true</td><td>User terminal preference for fullscreen mouse capture</td></tr>
            <tr><td><code>lastModel</code></td><td>Optional model object / absent</td><td>Last selected model; can inherit from the primary clone and user settings</td></tr>
            <tr><td><code>titleModel</code></td><td>Optional model object or supported string / absent</td><td>User-level automatic naming model; absent restores automatic selection</td></tr>
            <tr><td><code>lastAccounts</code></td><td>Account object array / empty</td><td>Remembered account identity per provider; excludes secrets and quota responses</td></tr>
            <tr><td><code>maxConcurrentAgents</code></td><td>Optional integer / absent</td><td>Project limit below explicit CLI priority and above machine priority; use a positive value</td></tr>
        </tbody></table>
        <p>For a new checkout settings file, this example keeps ordinary approval prompts
        enabled and requests a concurrency limit of three. Merge fields into an existing
        file rather than replacing its other preferences:</p>
        <pre><code class="language-json" data-config-schema="settings">{"{\n  \"version\": 1,\n  \"autoApprove\": false,\n  \"maxConcurrentAgents\": 3\n}" :: Text}</code></pre>
        <h2 id="saved-model">Saved model objects</h2>
        <p><code>lastModel</code> and an explicitly pinned <code>titleModel</code> use the
        following shape. These values are normally written by the model picker:</p>
        <table><thead><tr><th>Field</th><th>Rule</th></tr></thead><tbody>
            <tr><td><code>provider</code></td><td>Required supported provider identifier</td></tr>
            <tr><td><code>model</code></td><td>Required nonblank local model identifier</td></tr>
            <tr><td><code>connection</code></td><td>Defaults to provider identifier; must be nonblank</td></tr>
            <tr><td><code>transportModel</code></td><td>Defaults to model; records the wire identifier</td></tr>
            <tr><td><code>dialect</code></td><td>Defaults to the provider's legacy dialect; explicit values must be recognized and compatible</td></tr>
        </tbody></table>
        <p>Do not paste a <code>models.json</code> entry here: that schema uses
        <code>id</code> and configuration metadata instead. Use <code>/model NAME</code>
        to select a catalog entry. Use <code>/title-model NAME</code> to pin naming, or
        <code>/title-model --auto</code> to clear it. The special title string
        <code>apple-foundationmodel</code> selects supported on-device Apple naming;
        ordinary catalog models use the object form.</p>
        <h2 id="inheritance">Model inheritance and worktrees</h2>
        <ol>
            <li>An existing checkout <code>lastModel</code> wins.</li>
            <li>If absent, use the primary clone's remembered model.</li>
            <li>If still absent, use the user-level remembered model.</li>
        </ol>
        <p>Only this model preference is filled in by that inheritance procedure. It does
        not merge primary-clone auto-approval or account arrays into a worktree.
        A top-level interactive model switch writes the checkout, primary clone when
        different, and user preference. Startup, resume, and delegated/session-local
        switches are not that persistence event. Explicit launch/session choices still
        determine the current invocation.</p>
        <p>To forget an inherited model completely, stop affected sessions and remove
        only <code>lastModel</code> from each applicable settings location. Removing it
        from the worktree alone may simply reveal the primary-clone preference.
        Choosing another model interactively is usually simpler.</p>
        <h2 id="account-records">Account selection records</h2>
        <table><thead><tr><th>Field in lastAccounts</th><th>Rule</th></tr></thead><tbody>
            <tr><td><code>provider</code></td><td>Required recognized provider identifier</td></tr>
            <tr><td><code>selectionId</code></td><td>Required nonblank credential-source selection identifier</td></tr>
            <tr><td><code>accountId</code></td><td>String, default empty; provider account identity</td></tr>
        </tbody></table>
        <p>The successful choice replaces the remembered entry for that provider. Never
        put a token in either identifier. To reset a preference, stop the session and
        remove that provider's entry, preserving the others. This neither disconnects
        the account nor revokes its provider token. Use the login interface to manage
        credentials separately.</p>
        <h2 id="recovery">Malformed files and recovery</h2>
        <p>A missing, unreadable, or invalid settings file yields defaults. Within an
        otherwise decodable file, malformed or obsolete model selections are discarded
        independently, and invalid account entries are removed from the decoded list.
        This is intentionally different from the explicit machine-configuration errors.</p>
        <ol>
            <li>Stop sessions that could save preferences while you repair the file.</li>
            <li>Keep a private backup and inspect the exact checkout/user location.</li>
            <li>Correct one field or restore the backup; do not delete credential stores.</li>
            <li>Restart, inspect <code>/permissions</code> and <code>/session-info</code>,
            then verify the intended model with a small task.</li>
        </ol>
        <p>If a remembered choice vanished, inspect JSON types and the separate model
        catalog before assuming a provider account was deleted. If a model reappears,
        check inheritance rather than repeatedly removing the wrong file.</p>
    |]
    }
