{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.RuntimeDaemon (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/runtime-daemon/"
    , pageTitle = "Runtime daemon"
    , pageDescription = "Connect a local client to the durable task scheduler, frame commands, resume events, and recover interrupted tasks."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>The daemon is a per-user Unix-socket service for client developers. It is not
        the <a href="/reference/server/">HTTP server</a>, and its protocol is not newline-delimited
        JSON. The shipped runner launches one-shot CLI processes; an embedding application
        can supply a typed <code>TaskRunner</code> instead.</p>
        <h2 id="start">Start and locate the daemon</h2>
        <pre><code class="language-sh">{"nix shell .#agent-cli .#agent-runtime-daemon\nagent-runtime-daemon" :: Text}</code></pre>
        <p>The endpoint is <code>~/.haskell-agent/runtime/daemon.sock</code>. Set
        <code>HASKELL_AGENT_RUNTIME_DIR</code> before startup to relocate its directory;
        journal storage is beside the socket. <code>HASKELL_AGENT_CLI</code> selects the
        executable used for tasks, otherwise <code>agent-cli</code> is found on PATH.
        Keep the daemon in the foreground while developing a client. Stop it with Ctrl+C
        before changing its configuration.</p>
        <p>The runtime directory is mode 0700 and socket mode 0600. Connections are authenticated
        using Unix peer credentials. A symlinked directory, foreign-owned socket or existing
        non-socket endpoint is rejected. Do not fix startup by making the directory public
        or deleting an unrelated live socket. An exclusive lock serializes listener startup
        and stale-socket cleanup.</p>
        <h2 id="framing">Framing and handshake</h2>
        <p>Encode each message as UTF-8 JSON, prefixing its byte length as an unsigned
        four-byte big-endian integer. The default maximum frame is 1 MiB. A plain
        <code>echo</code> or HTTP request is not a valid client. Send this object as the
        first framed message; keep a stable client ID:</p>
        <pre><code class="language-json">{"{\"type\":\"hello\",\"hello\":{\"clientId\":\"documentation-client\",\"versions\":[3],\"resumeAfter\":null}}" :: Text}</code></pre>
        <p>Version 3 is currently supported. Expect <code>welcome</code> with negotiated
        version, current sequence and heartbeat interval, or <code>version_rejected</code>.
        Do not send task commands before completing negotiation.</p>
        <h2 id="events">Snapshots, events and reconnect</h2>
        <ol>
            <li>Without a cursor, receive a snapshot. With <code>resumeAfter</code>, receive
            retained events after the last durably applied sequence.</li>
            <li>For <code>snapshot_chunk</code> or <code>event_chunk</code>, decode each
            base64 slice, concatenate by zero-based chunk index, then parse the combined JSON.
            Do not parse each slice as a whole snapshot.</li>
            <li>Apply events in sequence order, persist your cursor, then send
            <code>{"{\"type\":\"ack\",\"sequence\":42}" :: Text}</code>.</li>
            <li>Answer a heartbeat with <code>{"{\"type\":\"pong\",\"sequence\":42}" :: Text}</code>
            using its sequence. Reconnect with the last applied cursor after disconnection.</li>
        </ol>
        <p>A cursor outside retention receives a fresh snapshot. Replace stale local state
        before applying subsequent events. Queues and reads/writes have bounds and deadlines;
        a slow client can be disconnected. Acknowledging data before durably applying it
        can make your client skip state after a crash.</p>
        <h2 id="commands">Command reference</h2>
        <p>Wrap every command in a client-generated command ID. For a read-only first check:</p>
        <pre><code class="language-json">{"{\"type\":\"command\",\"id\":\"list-1\",\"command\":{\"version\":1,\"type\":\"list\"}}" :: Text}</code></pre>
        <p>The reply is <code>command_result</code> correlated with that ID. Successful
        task results contain <code>version: 1</code>. Unknown command versions/types,
        invalid fields and a full command queue fail without changing scheduler state.</p>
        <table><thead><tr><th>Type</th><th>Fields</th><th>Behavior</th></tr></thead><tbody>
            <tr><td><code>submit</code></td><td><code>task_id</code>, <code>prompt</code>, <code>cwd</code>; optional <code>session_id</code>, <code>provider</code>, <code>model</code>, <code>effort</code>, <code>worktree</code></td><td>Create a task. Task IDs cannot be reused. Tasks resuming one session are serialized.</td></tr>
            <tr><td><code>cancel</code></td><td><code>task_id</code></td><td>Cancel queued or executing work; return durable task state. Completed side effects are not reversed.</td></tr>
            <tr><td><code>list</code></td><td>None</td><td>Return retained durable tasks, not an unbounded historical archive.</td></tr>
            <tr><td><code>set_limit</code></td><td><code>limit</code></td><td>Set concurrency to an integer from 1 through 32.</td></tr>
            <tr><td><code>retry</code></td><td><code>task_id</code></td><td>Retry eligible failed/cancelled/interrupted work only while original input remains in this process.</td></tr>
            <tr><td><code>approval</code></td><td><code>task_id</code>, <code>approval_id</code>, <code>decision</code></td><td>Recognized but unsupported by the shipped runner; fails visibly and never resolves approval.</td></tr>
        </tbody></table>
        <h2 id="submit">Submit a bounded task</h2>
        <pre><code class="language-json">{"{\"type\":\"command\",\"id\":\"submit-1\",\"command\":{\"version\":1,\"type\":\"submit\",\"task_id\":\"inspect-1\",\"prompt\":\"Describe the top-level files. Do not modify anything.\",\"cwd\":\"/absolute/path/to/project\",\"worktree\":false}}" :: Text}</code></pre>
        <p>Replace the project path. Provider and model must either both be present or both
        absent. Worktree creation is valid only for a fresh session. Task/session/approval
        IDs have a 256-character maximum, prompt 8,192 and path 4,096. An omitted session
        means a fresh session. A successful mutation is journaled as <code>task_changed</code>
        before the command reports success.</p>
        <p>The runner executes direct arguments without a shell, closes stdin and always
        passes <code>--no-yolo</code>. A task requiring interactive approval fails closed.
        The accepted approval decision spellings (<code>approve</code>, <code>deny</code>,
        <code>approve_session</code>) do not make the unsupported approval command functional.
        Use an interactive client for work requiring a human decision.</p>
        <h2 id="limits">Output and execution limits</h2>
        <p>Process tasks have a six-hour wall-clock deadline. Stdout and stderr are drained
        concurrently with bounded output and log storage. Descendants retaining pipes after
        the leader exits are subject to bounded TERM/KILL cleanup. A saturated log queue
        records <code>[output truncated: scheduler log queue was full]</code>; missing log
        text is not evidence that the corresponding operation did not occur.</p>
        <h2 id="recovery">Restart and retry safely</h2>
        <p>Snapshots and events are durably flushed. On startup queued/running tasks become
        interrupted; they are never automatically rerun. Original unredacted prompts are
        retained only in memory for eligible retries, not persisted as recoverable task input.
        After restart, retry reports <code>task input is unavailable</code>. Review filesystem
        and external-service outcomes, then submit the intended input under a new task ID.</p>
        <p>Completed or active tasks cannot be retried. Completion discards their raw input.
        Corrupt/oversized recovery data and invalid journal ownership fail startup closed.
        Stop the service and preserve the journal for diagnosis; do not edit sequence numbers
        or erase evidence merely to make startup succeed. Back up the journal only when the
        writer is stopped or through a coordinated filesystem snapshot.</p>
        <h2 id="embedding">Embedding and verification</h2>
        <h3 id="read-only-client">A read-only Python client</h3>
        <p>Save this as <code>inspect-daemon.py</code> and run it with Python 3 and the
        socket path as its sole argument. Start the daemon first under the same user.
        It negotiates version 3, requests a task list, services heartbeat messages and
        exits on the matching command result. It does not submit model work or resolve
        approvals. This is a source-reviewed protocol example, not a recorded live run.</p>
        <pre><code>{inspectionClient}</code></pre>
        <p>The example intentionally discards replay/snapshot messages: it only prints
        the independent list result, so it sends no replay acknowledgments and must not
        save a resume cursor. A stateful client must assemble chunks and apply snapshots
        and events before acknowledging their sequence. Never acknowledge data merely
        because bytes arrived. Timeout/EOF is not proof that an earlier mutation failed;
        reconnect and inspect its task ID before retrying.</p>
        <h3 id="supervision">Supervise one writer per user</h3>
        <p>Use a per-user service manager with a pinned daemon executable, a fixed
        <code>HASKELL_AGENT_CLI</code> path, the same HOME and socket configuration as
        clients, and a restart-on-failure policy. Do not run a second writer against
        the same socket/journal. Preserve the owner-only directory and socket modes
        rather than making the socket group/world writable to fix access failures.</p>
        <p>Before an upgrade, stop admission in clients, wait for tasks to settle or
        cancel and inspect them, stop the daemon, back up its complete journal directory,
        then replace the pinned executable and restart. Verify handshake and the read-only
        list request before admitting work. Roll back executable and journal together only
        when no newer writes must be preserved; interruption is not automatic task replay.</p>
        <p>Haskell clients can use the types in <code>Agent.Runtime.Daemon.Protocol</code>.
        An embedder can supply <code>TaskRunner</code> while retaining scheduling, persistence
        and wire protocol. There is intentionally no approval-resolver hook in this adapter.
        Pin the package revision and test handshake, reconnect, cancellation, full queues,
        interrupted recovery and unsupported approval before shipping a client.
        This reference is source-verified; it is not a live daemon interoperability report.</p>
    |]
    }

inspectionClient :: Text
inspectionClient = "import json, socket, struct, sys\n\
    \with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:\n\
    \    sock.settimeout(15)\n\
    \    sock.connect(sys.argv[1])\n\
    \    def send(value):\n\
    \        data = json.dumps(value).encode('utf-8')\n\
    \        sock.sendall(struct.pack('>I', len(data)) + data)\n\
    \    def exact(size):\n\
    \        data = bytearray()\n\
    \        while len(data) < size:\n\
    \            part = sock.recv(size - len(data))\n\
    \            if not part:\n\
    \                raise EOFError('daemon disconnected')\n\
    \            data.extend(part)\n\
    \        return data\n\
    \    def receive():\n\
    \        size, = struct.unpack('>I', exact(4))\n\
    \        if not 0 < size <= 1024 * 1024:\n\
    \            raise ValueError('invalid frame length')\n\
    \        return json.loads(exact(size))\n\
    \    send({'type': 'hello', 'hello': {'clientId': 'docs-inspection',\n\
    \          'versions': [3], 'resumeAfter': None}})\n\
    \    welcome = receive()\n\
    \    if welcome['type'] != 'welcome' or welcome['welcome']['version'] != 3:\n\
    \        raise RuntimeError(welcome)\n\
    \    send({'type': 'command', 'id': 'list-1',\n\
    \          'command': {'version': 1, 'type': 'list'}})\n\
    \    for _ in range(10000):\n\
    \        message = receive()\n\
    \        if message['type'] == 'heartbeat':\n\
    \            send({'type': 'pong', 'sequence': message['sequence']})\n\
    \        elif message['type'] == 'command_result' and message['id'] == 'list-1':\n\
    \            print(json.dumps(message, indent=2))\n\
    \            if not message['ok']:\n\
    \                raise SystemExit(1)\n\
    \            break\n\
    \    else:\n\
    \        raise RuntimeError('no list result within message bound')"
