-- | Map the provider-neutral loop onto the OpenRouter Responses transport.
--
-- OpenRouter does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. The loop threads that history explicitly so a
-- resumed session can seed it and the CLI can persist it.
module Agent.OpenRouter.LoopBackend
    ( openRouterBackend
    , openRouterBackendWithParams
    , openRouterBackendWith
    , openRouterBackendWithFixedParams
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , statelessResponsesBackendWithParams
    , tokenProviderStatelessResponsesBackend
    , tokenProviderStatelessResponsesBackendWithParams
    )
import Agent.Responses.Types
import Agent.Provider (TokenProvider)
import Agent.OpenRouter.Client (createResponseWithEvents)
import Agent.OpenRouter.Options (ClientOptions)

-- | Close over OpenRouter options, a token provider, and the request fields
-- the loop does not own (model, instructions, tools, reasoning). Credentials
-- stay cached; an auth rejection triggers one provider reload and retry.
-- Params are re-read each turn so the REPL can change reasoning effort
-- without dropping the local transcript.
openRouterBackend
    :: ClientOptions
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
openRouterBackend options provider =
    tokenProviderStatelessResponsesBackend provider
        (createResponseWithEvents options)

openRouterBackendWithParams
    :: ClientOptions
    -> TokenProvider
    -> ResponseCreateParams
    -> Backend
openRouterBackendWithParams options provider =
    tokenProviderStatelessResponsesBackendWithParams provider
        (createResponseWithEvents options)

-- | Same mapping as 'openRouterBackend', with an injectable transport for tests
-- and downstream integrations.
openRouterBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openRouterBackendWith = statelessResponsesBackend

openRouterBackendWithFixedParams
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Backend
openRouterBackendWithFixedParams =
    statelessResponsesBackendWithParams
