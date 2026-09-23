{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Telegram (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/telegram/"
    , pageTitle = "Telegram"
    , pageDescription = "Set up an allowlisted Telegram gateway, manage conversations and approvals, and deploy it on NixOS."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>The Telegram gateway connects messages to durable agent sessions on your machine.
        It runs separately from the terminal interface. Only allowlisted Telegram users can
        interact with it; provider credentials and the bot token are separate secrets.</p>
        <h2 id="prerequisites">Prerequisites</h2>
        <ul>
            <li>A Telegram bot created through BotFather and its bot token.</li>
            <li>Your numeric Telegram user ID—not your username.</li>
            <li>A configured model provider account on the gateway host.</li>
            <li>An existing project directory and the agent's Nix-managed environment.</li>
        </ul>
        <p>From a repository checkout, enter a shell containing the packaged executable:</p>
        <pre><code class="language-sh">nix shell .#agent-telegram</code></pre>
        <p>Run the following commands inside that shell. Keep the machine running while using the bot.</p>
        <h2 id="setup-and-first-message">Setup and first message</h2>
        <p>Replace the path and numeric ID with your own values:</p>
        <pre><code class="language-sh">{setupExample}</code></pre>
        <p>Setup prompts for the token without echoing it, validates it against Telegram, and
        stores it separately from non-secret configuration. Never paste the token into an agent
        conversation, commit it, or include it in a screenshot.</p>
        <ol>
            <li>Confirm that <code>status</code> reports the gateway running.</li>
            <li>Open a private chat with the bot and send <code>/status</code>.</li>
            <li>Send <code>Explain the top-level files in this project. Do not edit anything.</code></li>
            <li>Send <code>/session</code> and retain the identifier if you need to diagnose the conversation.</li>
        </ol>
        <p>A Telegram reply verifies delivery. A response that cites the intended project files
        verifies the configured working directory. These are separate checks.</p>
        <h2 id="command-line-reference">Command-line reference</h2>
        <p><code>agent-telegram run</code> stays in the foreground; no subcommand also runs
        the configured gateway. Use this under an existing supervisor or for diagnosis,
        not alongside a second background process for the same bot. <code>start</code>
        launches the background gateway, <code>stop</code> stops it and <code>status</code>
        inspects it. <code>--help</code> shows usage; <code>--version</code> reports the build.</p>
        <table><thead><tr><th>Setup option</th><th>Use</th></tr></thead><tbody>
            <tr><td><code>--provider NAME</code></td><td>openai, xai, openrouter, gemini or claude-code.</td></tr>
            <tr><td><code>--model NAME</code></td><td>Override the provider's default model; verify availability for the service account.</td></tr>
            <tr><td><code>--cwd PATH</code></td><td>Project directory on the gateway host.</td></tr>
            <tr><td><code>--effort LEVEL</code></td><td>Optional model reasoning effort.</td></tr>
            <tr><td><code>--allowed-user ID</code></td><td>Repeat for numeric user IDs.</td></tr>
            <tr><td><code>--yolo</code></td><td>Automatically approve mutations.</td></tr>
            <tr><td><code>--deny-mutations</code></td><td>Deny mutations rather than prompting. Choose one approval policy; do not combine policy flags.</td></tr>
            <tr><td><code>--all-group-messages</code></td><td>Consider allowed-user ambient group messages.</td></tr>
            <tr><td><code>--workers N</code></td><td>Concurrent chat workers, 1–64; default 8. Work within one conversation remains ordered.</td></tr>
            <tr><td><code>--start</code></td><td>Start after setup completes.</td></tr>
        </tbody></table>
        <p>To change setup values, stop the gateway, rerun setup with the complete intended
        configuration, inspect status/configuration, then restart. For a NixOS instance,
        edit its declaration instead of competing with generated settings.</p>
        <h2 id="allowlisted-users">Allowlisted users</h2>
        <p>Repeat <code>--allowed-user</code> during setup to add multiple IDs. To inspect or
        change the local allowlist afterward:</p>
        <pre><code class="language-sh">{"agent-telegram users list\nagent-telegram users add 123456789\nagent-telegram users remove 987654321\nagent-telegram stop\nagent-telegram start" :: Text}</code></pre>
        <p>Local CLI allowlist edits require a gateway restart. In a group, an already allowed
        member can use <code>/allow</code> with a name, username, or replied-to message;
        <code>/users</code> lists allowed and observed people, and <code>/deny</code> removes access.</p>
        <p>Allowlisting grants access to an agent operating on the gateway host. Grant it only
        to people authorized to use that project's files and connected services.</p>
        <p>Chat commands and gateway tools update the running gateway's global allowlist,
        not a chat-only membership list. They save gateway state and attempt to update an
        existing gateway <code>config.json</code> with mode <code>0600</code>. A configuration
        write failure is logged as <code>allowlist_config_persist_failed</code>; inspect
        it before restarting rather than assuming a successful chat response proves both
        files were updated. The last allowed user cannot be removed. With NixOS-managed
        configuration, also update the declarative <code>allowedUsers</code> setting:
        an in-chat edit does not edit your Nix configuration.</p>
        <h2 id="groups-and-topics">Groups and topics</h2>
        <p>By default, mention the bot, address a command to its username, or reply to one of its
        messages. Ambient group messages are ignored. Each private chat, group, and forum topic
        maps to a persisted session.</p>
        <p>For ambient group participation, pass <code>--all-group-messages</code> during setup.
        Telegram must also deliver those messages: disable BotFather privacy mode and re-add the
        bot when required. This does not remove the user allowlist.</p>
        <p>The bot remains in a group or channel only when an allowlisted Telegram administrator
        added it. An anonymous-admin add is accepted only when an allowlisted user is already an
        administrator of the chat.</p>
        <h3 id="routing-edge-cases">Edits, reactions and missing group updates</h3>
        <p>New and edited messages still require an allowlisted sender and an authorized
        group. An edit can replace a pending message with the same chat/message identity;
        it does not undo a turn already run or reverse its side effects. To correct completed
        work, send an explicit follow-up naming what should change and inspect the result.</p>
        <p>Only original, unforwarded text without voice/media can act as immediate
        <code>stop</code> control input. Quoted text, an edited message, a caption or forwarded
        <code>stop</code> is not an interrupt. For example, editing yesterday's message into
        <code>/stop</code> is not equivalent to sending a fresh <code>/stop</code> now.
        Commands explicitly addressed to another bot are ignored.</p>
        <p>Reactions from allowlisted users are represented as text identifying the target
        message and new emoji/custom emoji, including a distinct reaction-removed message.
        Reaction updates use the chat's unthreaded key because their classification has no
        topic identifier; do not assume a reaction resumes the original forum topic's session.
        Reactions without an identifiable allowed user are ignored. A thumbs-up reaction is
        conversation input, not a substitute for an approval button.</p>
        <p>If mentions work but ordinary group conversation does not, first check the
        configured ambient-response policy. Then check whether Telegram delivers those
        updates to the bot under its privacy and membership settings. The gateway cannot
        process an update Telegram never sends. Test with an explicit mention from an
        allowlisted user in the intended topic before broadening delivery or access.
        Ambient input allows the agent to remain silent rather than requiring a reply to
        every message.</p>
        <h2 id="conversation-controls">Conversation controls</h2>
        <table>
            <thead><tr><th>Message</th><th>Effect</th></tr></thead>
            <tbody>
                <tr><td><code>/start</code></td><td>Show a short introduction and available conversation controls</td></tr>
                <tr><td><code>/new</code></td><td>Start a fresh session for this conversation</td></tr>
                <tr><td><code>/session</code></td><td>Show the persisted session identifier</td></tr>
                <tr><td><code>/status</code></td><td>Report current work</td></tr>
                <tr><td><code>/retry</code></td><td>Requeue the latest failed turn</td></tr>
                <tr><td><code>/stop</code> or <code>stop</code></td><td>Interrupt the current turn without shutting down the gateway</td></tr>
            </tbody>
        </table>
        <p>In a group, reply <code>stop</code> to the bot or address
        <code>/stop@your_bot_username</code>. Cancellation is scoped to that chat or topic;
        other conversations keep running. The plain word is case-insensitive and may have
        surrounding whitespace.</p>
        <h2 id="approvals-and-delivery">Approvals and delivery</h2>
        <p>Mutating tools ask through inline approval buttons by default.
        <code>--deny-mutations</code> disables them; <code>--yolo</code> auto-approves them.
        Approval callbacks are bound to the originating conversation and an allowlisted user.</p>
        <p>Updates and pending replies are persisted before processing. Pending work, retries,
        callback bindings, delivery checkpoints, and dead letters survive restarts. Messages in
        one conversation are processed in order; separate chats use a bounded worker pool.</p>
        <p>If delivery is uncertain after a failure, inspect the conversation before retrying a
        request that could send a message, change a file, or modify an external service.</p>
        <h2 id="gateway-tools">Telegram tool payload reference</h2>
        <p>These are model-facing tools available during managed Telegram turns with a
        gateway bridge, not Telegram slash commands or public HTTP endpoints. The current
        conversation supplies chat/topic and requesting-user context; none accepts a
        <code>chat_id</code> override. Examples describe payloads, not completed sends.</p>
        <table><thead><tr><th>Tool</th><th>JSON fields</th><th>Outcome and recovery</th></tr></thead><tbody>
            <tr><td><code>send_telegram_document</code></td><td>Required <code>path</code> string; optional <code>caption</code>, <code>filename</code> strings.</td><td>Sends a downloadable file; success reports sent, usually with message ID. Inspect the chat before repeating an uncertain send.</td></tr>
            <tr><td><code>send_telegram_photo</code></td><td>Required <code>path</code> string; optional <code>caption</code>, <code>filename</code> strings.</td><td>Sends a Telegram photo. If Telegram rejects the format, explain the error and offer document delivery rather than claiming success.</td></tr>
            <tr><td><code>send_telegram_voice</code></td><td>Required <code>path</code> string; optional <code>caption</code>, <code>filename</code> strings.</td><td>Sends prepared audio as a voice reply; it does not synthesize audio from text or promise arbitrary codec support.</td></tr>
            <tr><td><code>react_to_telegram_message</code></td><td>Required <code>emoji</code> string; optional integer <code>message_id</code>.</td><td>One standard Telegram reaction emoji; defaults to triggering message. Missing/nonpositive target fails. Inspect target and emoji restrictions before retrying.</td></tr>
            <tr><td><code>ask_telegram_choice</code></td><td>Required <code>question</code> string and <code>options</code> array of strings.</td><td>Displays 1–8 short inline choices and waits for the authorized user's answer. Not approval to perform unrelated mutations.</td></tr>
            <tr><td><code>allow_telegram_user</code></td><td>Optional <code>query</code> string (name, @username or numeric ID), optional integer <code>user_id</code>.</td><td>Changes allowlist; use one unambiguous target. Omitting target uses replied-to user context. Inspect list afterward.</td></tr>
            <tr><td><code>deny_telegram_user</code></td><td>Optional <code>query</code> string, optional integer <code>user_id</code>.</td><td>Revokes allowlist membership. Verify exact identity and resulting list; do not infer success solely from a natural-language acknowledgment.</td></tr>
            <tr><td><code>list_telegram_users</code></td><td>Empty object.</td><td>Lists allowed and recently observed chat users, allowing ambiguity to be resolved before granting access.</td></tr>
        </tbody></table>
        <p>For all three send tools, <code>path</code> must be an absolute existing file
        under the private session temporary directory. Canonical path checks reject
        symlink escapes outside that root. Generate or copy the authorized output into
        the actual session directory; <code>filename</code> changes the download name,
        not the source path or destination chat.</p>
        <pre><code class="language-json">{"{\"path\":\"/actual/session-temp/report.pdf\",\"caption\":\"Requested report\",\"filename\":\"report.pdf\"}" :: Text}</code></pre>
        <pre><code class="language-json">{"{\"question\":\"Which output do you want?\",\"options\":[\"Summary\",\"Full report\"]}" :: Text}</code></pre>
        <pre><code class="language-json">{"{\"emoji\":\"👍\",\"message_id\":123}" :: Text}</code></pre>
        <pre><code class="language-json">{"{\"user_id\":123456789}" :: Text}</code></pre>
        <p>Substitute real paths and IDs; ask the agent for the tool operation rather
        than pasting these objects as chat commands. Inspect its tool result and the
        actual Telegram message or allowlist. File transport/API errors are separate
        from generation success and may require a supported format or smaller artifact.</p>
        <h3 id="choice-recovery">Inline choice and approval recovery</h3>
        <p>Choice labels are trimmed, empty labels removed, and only the first eight
        retained; display labels are capped at 48 characters. Choose distinct short
        labels to avoid ambiguous buttons. A question with no nonempty option fails.
        Bindings expire after 30 minutes and are tied to the requesting user and chat.
        Another user clicking the button cannot substitute their consent.</p>
        <p>If a button is expired, from a previous turn, or no longer resolves after
        restart, inspect <code>/status</code> and the gateway journal first. Ask for a
        new question in the active conversation rather than repeatedly clicking or
        interpreting silence as approval. Approval and filesystem-access callbacks are
        host-mediated requests, not extra public send-tool fields. Check the requested
        tool/path and scope before choosing; an ordinary question answer grants no
        broad tool permission. For uncertain delivery, follow the recovery sequence
        below before issuing another send.</p>
        <h2 id="recover-failed-delivery">Recover failed work and delivery</h2>
        <ol>
            <li>Send <code>/status</code> in the affected chat/topic. It reports the session,
            queued actions, retrying actions and failed turns available for retry.
            Inspect approval messages and host logs separately before submitting another request.</li>
            <li>Inspect the host's gateway logs or systemd journal. Redact tokens, message
            text and attachment paths before sharing diagnostics.</li>
            <li>Verify remote side effects separately: a failed reply delivery does not
            establish that the preceding tool operation failed.</li>
            <li>When retry is appropriate, send <code>/retry</code> in that conversation.
            It requeues the latest retained failed action, not an arbitrary earlier message.</li>
        </ol>
        <p>The response distinguishes no failed turn, a failure without a retryable action,
        and a queued retry. Pending actions, checkpoints and dead letters are in
        <code>~/.haskell-agent/gateways/telegram/state.json</code>. Preserve it during
        diagnosis; do not edit or delete it while the gateway is writing. A restart retains
        delivery state but cannot prove that an uncertain external mutation is safe to repeat.</p>
        <h3 id="checkpoints-and-dead-letters">Checkpoints, dead letters and restoration</h3>
        <p>A failed pending action is retried with persisted retry metadata. At five
        failures it moves to a dead letter containing its update ID, chat, error,
        failure timestamp and retained action; leaving a chat is exempt from this
        five-attempt cutoff. <code>/retry</code> selects the latest failure for the current
        conversation, removes it from the dead-letter list and queues the retained
        action under the new update ID. This may be a reply delivery rather than a
        model turn. It is not a general-purpose replay of any historical message.</p>
        <p>Long text replies are split at 4096 rendered characters. Delivery checkpoints
        record the next chunk after each successful send or edit, keyed by chat,
        topic and update ID. Restarting the same pending delivery can skip its recorded
        chunks. A crash after Telegram accepts a send but before its checkpoint is
        saved can still duplicate a message. A manually requeued action has a new
        update ID, so do not assume its old chunk checkpoint suppresses duplicates.</p>
        <ol>
            <li>For suspected state corruption or data loss, stop the gateway and verify
            no other host is polling with the same bot token. Preserve the damaged
            state and logs with restricted access before attempting recovery.</li>
            <li>Restore a consistent backup of the gateway state and its corresponding
            session database using the <a href="/guides/deployment/#restore-checklist">isolated
            restore checklist</a>. Do not restore only a checkpoint map or erase the
            queue to make an error disappear.</li>
            <li>Before reconnecting, compare the restored checkpoint time with Telegram
            conversation history and external tool effects. Messages or mutations after
            that time may already have succeeded and can be replayed from older state.</li>
            <li>Resume only one gateway after resolving those differences. Check
            <code>/status</code>, logs and the destination conversation. Use
            <code>/retry</code> only for a specifically reconciled retained failure.</li>
        </ol>
        <p>There is no documented selective dead-letter purge or arbitrary checkpoint
        repair command. Do not improvise JSON edits against production state; retain
        a backup and escalate an unreconciled queue to a maintainer. Treat state,
        dead-letter payloads, session transcripts and backups as sensitive content.
        Apply your retention policy to all of them together rather than assuming
        temporary attachment cleanup erases the conversation or failed action.</p>
        <p>Approval buttons must belong to the originating conversation and be pressed by an
        allowed user. A stale, already resolved or mismatched button is not permission to
        repeat a mutation. Inspect status and request a fresh operation if necessary.</p>
        <h2 id="group-access-example">Grant and remove group access</h2>
        <p>An already allowed group member can reply to the intended person's message with
        <code>/allow</code>, then inspect <code>/users</code>. Reply-based selection avoids
        guessing between similar display names. Names/usernames must resolve to the intended
        observed member; inspect the result before granting host access. Use <code>/deny</code>
        for that member to revoke access. For deterministic local administration use
        <code>agent-telegram users add ID</code> or <code>users remove ID</code> and restart.</p>
        <p>Revocation changes subsequent admission; it is not a cancellation command
        for an already running turn and does not roll back completed tool effects.
        If active work must stop, an allowed user should interrupt the affected
        conversation separately, then inspect its result and any external changes.
        Treat a removed user's earlier instructions as potentially already executed.</p>
        <h2 id="attachments">Attachments</h2>
        <p>The gateway accepts photos, documents, audio, video, video notes, animations,
        stickers, locations, contacts, venues, polls, dice, edited messages, and reactions.
        Images are provided natively to multimodal providers; other files use Responses
        <code>input_file</code> content or a private local-path fallback.</p>
        <p>Gateway tools can send documents, photos, and voice files, react to messages, ask
        inline-button questions, and manage users. Bot credentials remain in the parent gateway
        process and are not inherited by the agent child.</p>
        <p>Inbound downloaded voice and file payloads are bounded to 20 MiB. Oversized
        input is rejected rather than silently read in full. Telegram delivery and the
        selected model can impose additional format/capability limits. For a first attachment
        check, send a small non-sensitive image or text document and ask the agent to
        identify it without modifying files. Confirm an actual attachment-backed result,
        not merely a plausible description.</p>
        <p>For outbound files, identify the exact local file and intended conversation in
        your request, review any approval, and verify receipt before retrying. Photos,
        documents and voice are distinct Telegram delivery types. Provider file fallback
        may expose a private local path to the agent; it is not a public download URL.</p>
        <h3 id="media-failures">Media fallback, transcription and cleanup</h3>
        <p>Telegram voice messages use the gateway's xAI transcription path before the
        coding turn. This is separate from choosing a model for the resulting text.
        An empty transcription is an error, not an empty successful instruction.
        If a voice message fails while text works, check xAI authentication and the
        transcription error first; changing the coding model alone does not replace
        this transcription path. Send the intended text explicitly if voice cannot be
        decoded, rather than asking the model to guess what was said.</p>
        <p>Downloads are bounded to 20 MiB per payload and media batches use at most four
        concurrent downloads. Files are placed in the session temporary directory with
        update/index-based names. Missing MIME information falls back to
        <code>application/octet-stream</code>; a filename extension is not proof that the
        provider can decode its contents. Locations, contacts, venues, polls and dice can
        contribute descriptive content without a downloadable binary file.</p>
        <p>For example, a small PDF sent through a Responses-capable connection can be
        supplied as file content; on a connection without that path, the agent may instead
        need to inspect its private local file using available tools. Ask it to identify
        which evidence it actually read. If a video or animated sticker cannot be understood,
        provide a supported still image or a text transcript; successful Telegram download
        does not guarantee model-level video or animation understanding.</p>
        <p>Temporary voice downloads are removed after transcription, including failure.
        Partially downloaded media batches are cleaned up on failure. Do not treat
        temporary paths as a durable archive or assume cleanup of a download removes
        transcript text, conversation history, backups or copies made by tools. Keep
        original files separately if they must survive session cleanup, and apply your
        own retention policy to gateway state and backups.</p>
        <h2 id="nixos-deployment">NixOS deployment</h2>
        <p>Add the agent flake as an input, import its module, and declare an instance.
        The example below is a NixOS module fragment; <code>haskell-agent</code> is the flake input.</p>
        <pre><code>{nixosExample}</code></pre>
        <p>The module creates a dedicated user and private home, loads the token with systemd
        credentials, supplies Bash/Git/PostgreSQL 18, and manages an isolated PostgreSQL cluster.
        The token file stays outside the Nix store. Provision provider credentials separately,
        using the instance's login state or <code>environmentFiles</code>.</p>
        <p>Instance names start with a lowercase letter, use lowercase letters, digits, and
        hyphens, and have at most 16 characters. Paths must be absolute. The project must exist
        before startup; if automatic approvals are enabled, it must be writable by the service
        user. Do not enable automatic approvals simply to work around directory ownership.</p>
        <p>See the complete <a href="/guides/deployment/#telegram-options">NixOS option
        reference</a> for model/effort overrides, worker-host environment, PostgreSQL,
        credentials and <a href="/guides/deployment/#declarative-mcp">declarative MCP ownership</a>.</p>
        <pre><code class="language-sh">{"systemctl status haskell-agent-telegram-assistant.service\njournalctl -u haskell-agent-telegram-assistant.service -n 100 --no-pager" :: Text}</code></pre>
        <p>The default instance home is
        <code>/var/lib/haskell-agent-telegram-assistant</code>. Its
        <code>.haskell-agent/gateways/telegram/</code> directory contains configuration and
        delivery state; <code>.haskell-agent/postgres/</code> contains the managed database.
        Back up database contents with PostgreSQL tools, not an ordinary copy of a live data
        directory. Back up gateway <code>state.json</code> separately and protect secret recovery.</p>
        <h2 id="diagnose-telegram">Diagnose Telegram</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Check</th></tr></thead>
            <tbody>
                <tr><td>Setup rejects token</td><td>Use the BotFather token for this bot and check host connectivity to Telegram; never paste the token into a support report</td></tr>
                <tr><td>Private messages ignored</td><td>Confirm the gateway is running and the sender's numeric ID is allowlisted</td></tr>
                <tr><td>Group messages ignored</td><td>Mention or reply to the bot; check privacy mode only if ambient messages are intentionally enabled</td></tr>
                <tr><td>Bot leaves the group</td><td>Check whether an allowlisted Telegram administrator added it</td></tr>
                <tr><td>Provider request fails</td><td>Configure the model account for the gateway's service user; a valid Telegram token is not a model credential</td></tr>
                <tr><td>Turn waits without progressing</td><td>Check for an unanswered inline approval; use <code>/status</code> and cancel with <code>/stop</code> if appropriate</td></tr>
                <tr><td>NixOS service cannot start</td><td>Inspect the journal, token path, existing working directory, ownership, and provider environment file</td></tr>
            </tbody>
        </table>
    |]
    }

setupExample :: Text
setupExample = "agent-telegram setup --provider openai --cwd /path/to/project \\\n  --allowed-user 123456789\nagent-telegram start\nagent-telegram status"

nixosExample :: Text
nixosExample = "{ pkgs, ... }: {\n  imports = [ haskell-agent.nixosModules.telegram ];\n  services.haskell-agent.telegram.instances.assistant = {\n    enable = true;\n    workingDirectory = \"/srv/project\";\n    tokenFile = \"/run/secrets/telegram-bot-token\";\n    allowedUsers = [ 123456789 ];\n    provider = \"openai\";\n    yolo = false;\n    extraPackages = with pkgs; [ nix ripgrep ];\n  };\n}"
