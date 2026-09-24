{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.NativeMedia (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/native-media/"
    , pageTitle = "Native mobile and audio interfaces"
    , pageDescription = "Integrate account-bound mobile relays, native voice callbacks, and scoped WebRTC audio without confusing them with shipped applications."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>This is an embedder reference for the source interfaces, not a phone
        installation guide or a promise that a particular distribution includes
        a calling application. Begin with the <a href="/reference/native-integration/">native
        integration contract</a> and use the header from the same revision as your
        library. The host supplies its own interface, device permissions, capture,
        playback, authentication presentation and lifecycle supervision.</p>
        <h2 id="choose-media-interface">Choose the correct interface</h2>
        <table><thead><tr><th>Requirement</th><th>Interface and boundary</th></tr></thead><tbody>
            <tr><td>Insert dictated text into the composer</td><td>The <a href="/guides/voice/">dictation workflow</a>. It is not a continuously connected voice call.</td></tr>
            <tr><td>Run an agent voice turn in a native host</td><td><code>ha_engine_stage_voice</code> plus audio callbacks. The engine owns call orchestration; the host owns audio devices.</td></tr>
            <tr><td>Implement provider-independent media transport</td><td><code>Agent.WebRTC</code>. No microphone, provider account, signaling HTTP client or agent tool execution is included.</td></tr>
            <tr><td>Connect a runner to an already paired mobile client</td><td><code>ha_mobile_*</code>. The bridge uses the saved main gateway account and transports encrypted relay frames; it is not the pairing UI or encryption protocol implementation.</td></tr>
        </tbody></table>
        <h2 id="mobile-operation-contract">Mobile admission, completion and ownership</h2>
        <p>Every asynchronous mobile call has two results. The immediate return is
        0 when admitted, 1 for invalid arguments or 3 when unavailable; a nonzero
        return promises no callback. After admission, retain the callback and its
        context until its terminal callback. A callback can arrive before the
        initiating function returns: prepare state first, then call. Callbacks
        are serialized within one operation but independent operations may run
        concurrently on runtime threads.</p>
        <table><thead><tr><th>Callback status</th><th>Action</th></tr></thead><tbody>
            <tr><td>0</td><td>Terminal success. Inspect the fields appropriate to the operation.</td></tr>
            <tr><td>1</td><td>Terminal invalid input. Correct the input; do not retry unchanged.</td></tr>
            <tr><td>2</td><td>Terminal account unavailable/changed, including invalid or closed handles. Retire this session and re-establish the intended account before opening another.</td></tr>
            <tr><td>3</td><td>Terminal transport/server failure. No raw server error text is exposed. Check state before retrying a mutation.</td></tr>
            <tr><td>4</td><td>Nonterminal pairing row from list only. Copy the pairing ID from <code>value</code> and its display name from <code>name</code>; continue waiting for a terminal status.</td></tr>
        </tbody></table>
        <p>Inputs are copied before return. Text is UTF-8, at most 4096 input bytes.
        Callback byte buffers are borrowed only during that invocation; a null
        pointer is permitted at length zero. Copy retained data and dispatch UI
        work asynchronously. Session open returns a nonzero owned handle and
        the inherited gateway base URL in <code>value</code>. Relay open returns
        another owned handle. Register returns a runner ID in <code>value</code>;
        receive returns frame bytes. Other successful fields are empty or zero.
        Neither access tokens nor raw HTTP exceptions cross this ABI.</p>
        <h2 id="mobile-sequence">Example: inspect a pairing and open its relay</h2>
        <ol>
            <li>Initialize the native runtime and sign in through the ordinary
            gateway workflow. Call <code>ha_mobile_session_open</code> with a
            retained callback context. On immediate 0, wait for terminal 0 and
            store its session handle. Opening does not register a runner.</li>
            <li>If this host is registering as a runner, call
            <code>ha_mobile_runner_register</code> with a stable device UUID and
            a nonempty display name of at most 160 characters. The UUID is 36
            characters with hyphens at positions 8, 13, 18 and 23 and hexadecimal
            digits elsewhere. Retain the returned runner ID on success.</li>
            <li>Call <code>ha_mobile_pairings_list</code>. Collect status-4 rows
            into a new list and publish it only after terminal 0. An empty list
            is a valid result, not permission to invent a pairing ID. The
            implementation rejects responses over 1024 rows or with malformed IDs.</li>
            <li>Let the user select an existing pairing ID. Optionally call
            <code>ha_mobile_pairing_wake</code> and await its terminal response.
            A successful wake request does not prove the runner or mobile
            application is online.</li>
            <li>Call <code>ha_mobile_relay_open</code> with that same pairing ID.
            On terminal 0 retain the relay handle, then issue a receive operation
            with its own retained completion context. Do not treat pairing IDs,
            runner IDs and numeric relay handles as interchangeable.</li>
            <li>Send only frames produced by the consuming application's
            authenticated encryption protocol. A successful send means admission
            to the bounded outgoing queue, not delivery or acknowledgement by
            the remote application. Correlate application-level acknowledgements
            inside that protocol before retrying a digital action.</li>
            <li>On user closure, stop issuing operations, close each relay handle,
            then close the session handle. Drain already admitted callbacks before
            freeing their contexts or exiting the runtime.</li>
        </ol>
        <pre><code>{mobileTrace}</code></pre>
        <p>The trace is an example of control flow, not a literal wire payload.
        The library exposes no mobile pairing-creation or end-to-end encryption
        command here. Those responsibilities remain with the gateway and consuming
        application; passing arbitrary JSON to the relay is not a valid substitute.</p>
        <h3 id="mobile-recovery">Revocation, bounds and uncertain outcomes</h3>
        <p><code>ha_mobile_pairing_revoke</code> takes the selected pairing UUID
        and awaits a terminal result. The HTTP implementation accepts an already
        absent pairing (DELETE 404) as success. After an uncertain response,
        list pairings again instead of reporting success. Closing a mobile session
        leaves the main gateway credentials intact; disconnecting or replacing
        that account invalidates sessions that inherited its authority.</p>
        <p>Relay frames are at most 1 MiB. The internal incoming and outgoing queues
        each hold eight frames, so consumers must drain receive results and bound
        their own queues. HTTP operations, relay establishment and enqueueing a
        send have a 20-second outer bound; HTTP response timeout is 15 seconds.
        Receive has no independent idle timeout: it waits for data or closure.
        Close its relay to release a pending receive. Do not free a receive's
        context merely because the UI stopped waiting.</p>
        <p><code>ha_mobile_handle_close</code> is synchronous and idempotent.
        Closing a session stops its relays, but each returned relay handle still
        needs closing. Close invalidates operations; it is not a callback-drain
        barrier or a guarantee that an already queued remote effect did not happen.
        Keep operation contexts until their terminal callback. After account
        changes, create fresh sessions rather than reusing old numeric handles.
        If an already admitted open completes during shutdown, close its returned
        handle too; do not discard a successful completion just because its view
        has disappeared.</p>
        <h2 id="voice-turn-sequence">Example: stage one native voice turn</h2>
        <ol>
            <li>Choose a unique nonempty UTF-8 turn ID. Voice staging currently
            limits this ID to 512 bytes, stricter than the ordinary turn-option
            limit. Allocate a callback context owning a bounded playback queue,
            capture-worker lifecycle and a synchronized optional call handle.</li>
            <li>Call <code>ha_engine_stage_turn_options</code> for this turn,
            then <code>ha_engine_stage_voice</code> with the same ID. Check each
            immediate result. Voice staging requires existing options and changes
            this turn into a voice call instead of using its initial text prompt.</li>
            <li>Submit the matching <code>turn.start</code> through the normal
            request path. Admission is not connection. If submission is abandoned,
            discard staging for that turn; do not leave an audio callback retained
            for an unrelated future request.</li>
            <li>Only on audio event 0, after server acknowledgement, start capture
            and publish the callback's call handle to that worker. Configure signed
            little-endian PCM16, 24,000 Hz, mono. Device setup must be bounded.</li>
            <li>Copy incoming event-1 audio into bounded playback storage and
            schedule it on the host audio device. Do not retain the borrowed
            callback pointer or synchronously wait for GUI-thread work.</li>
            <li>On event 3 discard scheduled playback without stopping capture.
            This resets playback for interruption; it is not the end of the call.</li>
            <li>On event 2 stop and join capture, clear all shared references to
            the call handle, and stop playback before returning. The bridge frees
            the handle immediately after this callback. Then observe the ordinary
            turn outcome; stopping media does not undo delegated tool effects.</li>
        </ol>
        <table><thead><tr><th>Call</th><th>Results and limits</th></tr></thead><tbody>
            <tr><td><code>ha_engine_stage_voice</code></td><td>0 accepted; 1 invalid pointer, callback or turn-ID size; 2 missing staged options or invalid UTF-8; 3 internal failure.</td></tr>
            <tr><td><code>ha_voice_submit_audio</code></td><td>Thread-safe nonblocking copy. 0 accepted; 1 invalid input; 2 closed/queue overrun, stopping the call; 3 internal failure. Each frame is nonempty, even-sized and at most 24,000 bytes.</td></tr>
            <tr><td>Audio callback return</td><td>Return 0 for success and nonzero to fail the call. Stop is delivered even when start fails. Never wait for engine or turn completion inside a callback.</td></tr>
        </tbody></table>
        <p>A 20 ms capture frame contains 480 samples, or 960 bytes. The 24,000-byte
        ceiling is a maximum, not a recommended device-buffer size. On queue
        rejection stop capture; do not build an unbounded retry buffer. The call
        handle is valid only between start and the return from stop. A stale pointer
        is not a recoverable “closed” probe: never call through one.</p>
        <h3 id="voice-host-tests">Host acceptance cases</h3>
        <p>Before exposing a calling control, test device-denied start followed by
        stop, playback interruption without capture cancellation, queue overrun,
        close during capture, account replacement and staging discard. Verify that
        capture has joined before stop returns and no audio outlives the owner.
        The ABI's validation tests do not exercise real devices. Provider access and
        organization authority are separate from microphone permission; see
        <a href="/guides/voice/#voice-call">the voice-call workflow</a>.</p>
        <h2 id="webrtc-sequence">WebRTC negotiation and scoped audio</h2>
        <p>The <code>agent-webrtc</code> Haskell library exports an opaque
        <code>Peer</code>. Acquire it only with <code>withPeer</code>; all users
        and child workers must finish before its scope exits. The offerer calls
        <code>createOffer</code>, sends the returned SDP through the host's own
        signaling transport, receives an answer and calls <code>setRemoteAnswer</code>.
        The answerer calls <code>setRemoteOffer</code> before
        <code>createAnswer</code>. Both call <code>awaitConnected</code> before
        starting normal capture/playback. Creating a description also installs
        the local description and waits for gathering; do not add a second
        invented local-description API.</p>
        <p>The following in-process example connects two peers and transports
        silence. It needs the pinned package's native media dependencies but no
        account, microphone or external signaling service. It is a consumer
        example, not evidence of a live native application test.</p>
        <pre><code class="language-haskell">{webrtcExample}</code></pre>
        <p>Include <code>agent-webrtc</code>, <code>async</code> and
        <code>bytestring</code> in the consuming package. The repository's Nix
        package supplies the native GStreamer dependencies; do not replace them
        with an unrelated global installation. A returned <code>Just</code> byte
        count demonstrates receipt, not a particular audio-device latency or
        microphone permission. <code>Nothing</code> indicates the example's
        five-second media deadline expired.</p>
        <h3 id="webrtc-errors">Media validation and shutdown</h3>
        <p>Remote SDP must be nonempty UTF-8, contain no NUL byte and occupy at
        most 65,536 encoded bytes. Negotiation polling and connection establishment
        use 15-second bounds. They raise failures for unavailable plugins,
        invalid encoding or size, disconnection, negotiation failure or timeout.
        These failures are not provider-authentication errors.</p>
        <p><code>pushAudio</code> accepts nonempty, even-sized PCM16 frames up to
        24,000 bytes; queue failure raises an exception rather than silently
        accumulating data. <code>pullAudio</code> waits for a nonempty frame and
        has no application-level idle deadline. Wrap long-running consumers in
        the host's cancellation scope and join them before leaving
        <code>withPeer</code>. The example's structured concurrency ensures its
        sender is cancelled and joined if receive fails or times out. Do not
        retain <code>Peer</code> past release or create detached audio workers.</p>
        <h2 id="source-and-validation">Source and validation boundary</h2>
        <p>The canonical contracts are
        <a href="https://github.com/digitallyinduced/haskell-agent/blob/master/packages/agent-native-bridge/include/HaskellAgentBridge.h">HaskellAgentBridge.h</a>,
        <a href="https://github.com/digitallyinduced/haskell-agent/blob/master/packages/agent-native-bridge/ffi/Agent/CLI/MacOS/MobileGateway.hs">MobileGateway.hs</a>,
        <a href="https://github.com/digitallyinduced/haskell-agent/blob/master/packages/agent-native-bridge/ffi/Agent/CLI/MacOS/MobileGatewayBridge.hs">MobileGatewayBridge.hs</a>,
        <a href="https://github.com/digitallyinduced/haskell-agent/blob/master/packages/agent-native-bridge/ffi/Agent/CLI/MacOS/Voice.hs">Voice.hs</a>
        and <a href="https://github.com/digitallyinduced/haskell-agent/blob/master/packages/agent-webrtc/src/Agent/WebRTC.hs">Agent.WebRTC</a>.
        Select the same repository revision as your binary. Tests in
        <code>MobileGatewaySpec.hs</code> cover credential invalidation and invalid
        inputs; the WebRTC package's native peer checks cover local media transport.
        Neither certifies an external application's pairing UI, encryption protocol,
        microphone permissions, account access or deployed relay connectivity.</p>
    |]
    }

mobileTrace :: Text
mobileTrace = "session_open -> admitted 0 -> callback 0, session handle\n\
    \pairings_list(session) -> admitted 0 -> callback 4, pairing ID/name\n\
    \                                      -> callback 0, list complete\n\
    \relay_open(session, selected ID) -> admitted 0 -> callback 0, relay handle\n\
    \relay_receive(relay) -> admitted 0 -> callback 0, encrypted frame\n\
    \handle_close(relay); drain its pending callbacks\n\
    \handle_close(session); drain its pending callbacks"

webrtcExample :: Text
webrtcExample = "import Agent.WebRTC\n\
    \import Control.Concurrent (threadDelay)\n\
    \import Control.Concurrent.Async (concurrently)\n\
    \import Control.Monad (replicateM_)\n\
    \import qualified Data.ByteString as BS\n\
    \import System.Timeout (timeout)\n\n\
    \receiveSilence :: IO (Maybe Int)\n\
    \receiveSilence = withPeer $ \\offerer -> withPeer $ \\answerer -> do\n\
    \    offer <- createOffer offerer\n\
    \    setRemoteOffer answerer offer\n\
    \    answer <- createAnswer answerer\n\
    \    setRemoteAnswer offerer answer\n\
    \    awaitConnected offerer\n\
    \    awaitConnected answerer\n\
    \    let send = replicateM_ 50 $ do\n\
    \            pushAudio offerer (BS.replicate 960 0)\n\
    \            threadDelay 20000\n\
    \    result <- timeout 5000000 (concurrently send (pullAudio answerer))\n\
    \    pure (BS.length . snd <$> result)"
