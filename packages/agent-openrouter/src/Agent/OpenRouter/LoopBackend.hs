-- | Map the provider-neutral loop onto the OpenRouter Responses transport.
--
-- OpenRouter does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. Callers own the 'IORef' so a resumed session can
-- seed history and the CLI can persist it.
module Agent.OpenRouter.LoopBackend
    ( openRouterBackend
    , openRouterBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , tokenProviderStatelessResponsesBackend
    )
import Agent.Responses.Types
import Agent.Provider (TokenProvider)
import Agent.OpenRouter.Client (createResponseWithEvents)
import Agent.OpenRouter.Options (ClientOptions)
import Data.IORef

-- | Close over OpenRouter options, a token provider, and the request fields
-- the loop does not own (model, instructions, tools, reasoning). Credentials
-- stay cached; an auth rejection triggers one provider reload and retry.
-- Params are re-read each turn so the REPL can change reasoning effort
-- without dropping the local transcript.
openRouterBackend
    :: ClientOptions
    -> TokenProvider
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openRouterBackend options provider =
    tokenProviderStatelessResponsesBackend provider
        (createResponseWithEvents options)

-- | Same mapping as 'openRouterBackend', with an injectable transport for tests.
openRouterBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openRouterBackendWith = statelessResponsesBackend
