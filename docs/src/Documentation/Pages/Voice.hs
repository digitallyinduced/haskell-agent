{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Voice (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/voice/"
    , pageTitle = "Voice and dictation"
    , pageDescription = "Dictate editable prompts or start a conversational voice call, with distinct authentication and approval boundaries."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>Dictation converts speech into editable prompt text while keeping the terminal session
        open. It is a composer input method: review the transcript before submitting it as an
        instruction to the agent.</p>
        <h2 id="voice-call">Start a conversational voice call</h2>
        <p>For a live conversation rather than composer transcription, run:</p>
        <pre><code>/voice</code></pre>
        <p>Use headphones and grant microphone permission to the terminal application.
        A local call requires a ChatGPT subscription sign-in; an API key alone is not a
        substitute. In an organization-bound session, the call uses that organization's
        gateway instead of silently switching to a local account. Listen for connection
        confirmation before speaking sensitive material.</p>
        <p>The call can delegate work to the coding agent in the current session. Delegated
        work remains serialized with typed turns and keeps the existing tools and approvals.
        Complete required approvals in the application; spoken intent does not bypass them.
        Use Ctrl+C or the application's Stop control to hang up. Ending a call does not
        roll back work already performed.</p>
        <p>If connection fails, inspect <code>/accounts</code> and <code>/usage</code>,
        model access, microphone permission and audio devices. A rate-limited voice
        account needs an available ChatGPT account. Voice signaling keeps one admitted
        credential; it does not retry an ambiguously created call with another identity.
        This workflow is separate from the transcription routing table below.</p>
        <p>Embedding a native call instead? See the
        <a href="/reference/native-media/">native voice, mobile and media contracts</a>
        for capture/playback ownership, pairing and transport lifetimes. An exported
        media interface does not imply that a phone or desktop application ships here.</p>
        <h2 id="dictate-a-prompt">Dictate a prompt</h2>
        <ol>
            <li>Open an interactive agent session and select a supported model.</li>
            <li>Place the cursor where you want the transcript inserted.</li>
            <li>Press <kbd>Ctrl</kbd> + <kbd>R</kbd> and speak a short instruction.</li>
            <li>Press <kbd>Enter</kbd> to stop recording, or <kbd>Esc</kbd> to cancel.</li>
            <li>On macOS the transcript is inserted at the cursor. Correct paths, identifiers,
            and punctuation, then submit the completed prompt normally.</li>
        </ol>
        <p>For a first check, dictate “Explain this repository without changing files.” Confirm
        that the words appear in the composer and that the session remains open. Do not test by
        speaking credentials or sensitive customer information.</p>
        <h2 id="provider-routing">Provider routing</h2>
        <table>
            <thead><tr><th>Active model</th><th>Transcription route</th></tr></thead>
            <tbody>
                <tr><td>OpenAI</td><td>OpenAI subscription or API-key route; subscription authentication is preferred when both are configured</td></tr>
                <tr><td>Grok / xAI</td><td>Configured xAI subscription or API-key credential</td></tr>
                <tr><td>Claude</td><td>A locally configured OpenAI account; xAI is used when no OpenAI credential exists because Claude has no transcription API</td></tr>
                <tr><td>OpenRouter or Gemini</td><td>Dictation is currently unavailable</td></tr>
                <tr><td>Connected organization gateway</td><td>Only the gateway's authenticated <code>/v1/audio/transcriptions</code> endpoint; no fallback to local provider credentials</td></tr>
            </tbody>
        </table>
        <p>Choosing Claude does not mean audio is sent to Anthropic. Check the routing above
        before recording material subject to a provider-specific data policy. Model access and
        transcription access are separate capabilities.</p>
        <h2 id="credentials-and-language">Credentials and language</h2>
        <p>OpenAI dictation can use a managed account or credentials from
        <code>CODEX_ACCESS_TOKEN</code>, <code>CODEX_AUTH_JSON</code>,
        <code>$CODEX_HOME/auth.json</code> (default <code>~/.codex/auth.json</code>),
        <code>OPENAI_API_KEY</code>, or <code>CODEX_API_KEY</code>. Configure credentials through
        <a href="/getting-started/authentication/">authentication</a>; do not paste them into a prompt.</p>
        <p>xAI defaults to English. Set <code>XAI_STT_LANGUAGE</code> in the environment before
        launching the agent to request another language, for example Portuguese:</p>
        <pre><code class="language-sh">{"XAI_STT_LANGUAGE=pt agent-cli --provider xai" :: Text}</code></pre>
        <h2 id="streaming-and-fallback">Streaming and fallback</h2>
        <p>OpenAI subscription authentication uses the subscription-backed streaming protocol.
        If streaming fails, the same captured recording can be submitted to the buffered ChatGPT
        transcription route. API keys use the public OpenAI Realtime API. Both OpenAI routes can
        update the composer during recording.</p>
        <p>The organization gateway streams partial transcripts where supported. Older gateways
        and interrupted streams can use a final-only upload on the same endpoint with the same
        captured recording. That fallback does not switch to a local account or a different
        provider destination.</p>
        <h2 id="diagnose-dictation">Diagnose dictation</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Check and recovery</th></tr></thead>
            <tbody>
                <tr><td>Ctrl+R does not start recording</td><td>Focus the prompt composer, check terminal key interception, and select a supported provider</td></tr>
                <tr><td>No audio or empty transcript</td><td>Check the selected input device and macOS microphone permission for the terminal application; retry a short non-sensitive phrase</td></tr>
                <tr><td>Authentication or account failure</td><td>Inspect <code>/accounts</code> and <code>/usage</code>; sign in again or select an available account instead of repeating a long recording</td></tr>
                <tr><td>Claude dictation cannot start</td><td>Configure a supported OpenAI or xAI transcription credential; Claude login alone is insufficient</td></tr>
                <tr><td>No partial words appear</td><td>A final-only route may be active; stop recording and check the completed transcript</td></tr>
                <tr><td>Gateway transcription fails</td><td>Check organization transcription access and gateway connectivity; local credentials will not be used as a bypass</td></tr>
                <tr><td>Names or code are transcribed incorrectly</td><td>Edit the transcript before submission; dictation does not guarantee exact identifiers</td></tr>
            </tbody>
        </table>
        <p>For other composer controls, see <a href="/reference/keybindings/">keybindings</a>.</p>
    |]
    }
