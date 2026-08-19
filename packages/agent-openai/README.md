# agent-openai

Haskell client for OpenAI-compatible Responses APIs, imported from `codex-hs`.

- REST + WebSocket streaming
- OpenAI credential adapters for local OAuth pools and static API bearers
- Local OAuth account pool with round-robin, cooldown tracking, and automatic token refresh
- Tool DSL for function tools + built-in `web_search` / `computer` tools

The library is IHP-agnostic — it has no dependency on IHP, a database, or any
specific persistence layer. You bring your own persistence (if any) and pass it
in as a plain `AuthState -> IO (Either ApiError AuthState)` callback.

## Quick start

```haskell
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OpenAI.Client (createCodexMessageWithProvider)
import Agent.OpenAI.Credential (poolTokenProvider)
import Agent.OpenAI.Responses.Types

main :: IO ()
main = do
    -- Load tokens from wherever (env, file, DB, ...) and build a pool.
    pool <- OpenAI.newPool initialStates
        (OpenAI.refreshAccessTokenHTTP oauthClientId)
    provider <- poolTokenProvider pool

    let request = defaultResponseCreateParams
            { model = Just "gpt-5.1-codex"
            , instructions = Just "You are a helpful assistant."
            , input = Just (ResponseInputText "Say hi")
            , reasoning = Just ReasoningConfig
                { context = Nothing
                , effort = Just "low"
                , generateSummary = Nothing
                , reasoningMode = Nothing
                , summary = Nothing
                , extraFields = mempty
                }
            }

    result <- createCodexMessageWithProvider provider request
    print result
```

Against an OpenAI-compatible Responses host (for example digitallyinduced
llm-router) there is no ChatGPT OAuth pool and no broker. Use a static
bearer and an explicit base URL:

```haskell
import Agent.OpenAI.Client (createCodexMessageWithProviderAt)
import Agent.OpenAI.Credential (staticBearerProvider)

main :: IO ()
main = do
    let provider = staticBearerProvider routerApiKey
        -- POST {baseUrl}/responses, e.g. https://llm-router.example/v1/responses
        baseUrl = "https://llm-router.example/v1"

    result <- createCodexMessageWithProviderAt baseUrl provider request
    print result
```

`staticBearerProvider` never refreshes or fails over. A 429 becomes
`CredentialsExhausted` with the server retry interval; a rejected key is a hard
authentication error.

`Agent.Provider.TokenProvider` from `agent-core` is the transport boundary. A
custom implementation exposes one
function: an initial call receives `Nothing`; a failover call receives the
rejected credential plus a structured account failure and returns a usable
replacement. `Credential` carries the access token, optional ChatGPT
account id (omit it for static bearer / router calls), and an optional opaque
broker lease id. Its `Show` instance always redacts credential secrets.
Construct the provider once per process and share it; providers retain
defensive cooldown and authentication-recovery state.

```haskell
getNextToken
    :: TokenProvider
    -> Maybe FailedCredential
    -> IO (Either ApiError Credential)
```

`runWithTokenProvider` is the shared REST/WebSocket retry orchestrator. It
classifies only account-scoped failures, reports the rejected credential, and
reruns the supplied operation with the provider's replacement. Generic network
or server failures do not poison account health.

For replay-safe WebSocket operations, `withCodexWsRetrying` applies that same
loop to both handshake and in-band 401/403/usage-limit failures. Its callback
can be executed again from the beginning, so callers must keep externally
visible side effects idempotent.

## Persistence + cross-process locking

`Agent.OpenAI.Auth.refreshAccessTokenHTTP` is a pure HTTP call to
`https://auth.openai.com/oauth/token`. If you run multiple worker processes
sharing a token pool in Postgres, wrap it with a row-level lock so two workers
can't both POST the same stale refresh token (OpenAI rotates it on every use;
the second POST gets `refresh_token_reused`):

```haskell
myRefresh :: AuthState -> IO (Either ApiError AuthState)
myRefresh stale = withTransaction do
    current <- selectForUpdate "codex_auth" stale.accountId
    if current.refreshToken /= stale.refreshToken
        then pure (Right current)  -- another worker already rotated
        else do
            result <- OpenAI.refreshAccessTokenHTTP oauthClientId current
            traverse_ (persist stale.accountId) result
            pure result
```

Pass `myRefresh` to `newPool` instead of the configured
`refreshAccessTokenHTTP oauthClientId` callback.

## Modules

| Module | What it does |
|---|---|
| `Agent.OpenAI.Responses.Types` | Lossless, wire-aligned Responses API request, response, item, tool, and stream-event types |
| `Agent.OpenAI.Error` | OpenAI error-envelope decoding and Responses error normalization |
| `Agent.OpenAI.Http` | Successful HTTP/SSE response-body decoding and failed-response normalization |
| `Agent.OpenAI.Auth` | Pool, round-robin, cooldown, pure HTTP refresh, JWT exp parsing |
| `Agent.OpenAI.Credential` | ChatGPT OAuth-pool and static OpenAI bearer adapters |
| `Agent.OpenAI.Login` | Headless OAuth device-code login compatible with the Codex CLI |
| `Agent.OpenAI.Client` | REST client (SSE or JSON) + optional base URL + rate-limit failover |
| `Agent.OpenAI.WebSocketClient` | OpenAI WebSocket endpoint, authentication, request encoding, and event decoding |
| `Agent.OpenAI.LoopBackend` | Provider-neutral `Backend` over `sendWsRequestWithEvents`, including function and custom tool-call mapping |
| `Agent.OpenAI.ToolDSL` | Minimal `PropertySchema` + `buildTool` for function tools |

## Production login

Run the device-code flow directly on a headless server. It creates an
independent refresh-token family instead of copying another machine's rotating
credentials:

```console
OPENAI_OAUTH_CLIENT_ID=... nix run .#agent-openai-login -- --output /root/.codex/auth.json
```

Open the printed URL, enter the one-time code, and finish signing in. The
credentials file is written atomically with mode `0600`.

Services using a centralized broker can avoid handling refresh tokens entirely:

```haskell
provider <- Agent.Broker.newBrokerTokenProvider Agent.Broker.BrokerOptions
    { baseUrl = "https://codex-auth.example.com"
    , serviceToken = tokenFromSecretStore
    }
```

The broker provider asks the broker for its currently preferred account on
every acquisition instead of copying every broker account into another local
round-robin pool. When the broker returns a `lease_id`, the provider echoes it
with failure feedback so delayed authentication failures cannot invalidate a
newer credential generation. A broker exhaustion response with `retry_at` is
surfaced as `CredentialsExhausted`, preserving the earliest useful retry time
across processes.

## Development

```
nix develop
cabal build
cabal repl
```
