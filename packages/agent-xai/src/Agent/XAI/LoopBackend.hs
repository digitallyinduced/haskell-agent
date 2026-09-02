-- | Map the provider-neutral loop onto the xAI Grok subscription transport.
--
-- The proxy does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. The loop threads that history explicitly so a
-- resumed session can seed it and the CLI can persist it.
module Agent.XAI.LoopBackend
    ( xaiBackend
    , xaiBackendWithClientOptions
    , xaiBackendWith
    , xaiCompactionCheckpointOriginItem
    , isXaiCompactionCheckpointOriginItem
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.LoopBackend
    ( isServerCompactionCheckpoint
    , statelessResponsesBackend
    , tokenProviderStatelessResponsesBackend
    )
import Agent.Responses.Request
    ( filterRequestCompactionCheckpointsByOrigin
    )
import Agent.Responses.Types
import Agent.Provider (TokenProvider)
import Agent.XAI.Client (createResponseWithEvents)
import Agent.XAI.Options (ClientOptions)

-- | Close over xAI options, a token provider, and the request fields the loop
-- does not own (model, instructions, tools, reasoning). Credentials stay
-- cached; an auth rejection from the proxy triggers one provider reload and
-- retry. Params are re-read each turn so the REPL can change reasoning effort
-- without dropping the local transcript.
xaiBackend
    :: ClientOptions
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
xaiBackend options =
    xaiBackendWithClientOptions (const options)

-- | Resolve transport options from the final request. This lets callers keep
-- request metadata such as the server compaction hint aligned with model
-- catalog values that can change during a session.
xaiBackendWithClientOptions
    :: (ResponseCreateParams -> ClientOptions)
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
xaiBackendWithClientOptions optionsForRequest provider =
    tokenProviderStatelessResponsesBackend provider
        (\credential request onEvent -> do
            let projectedRequest = projectXaiCheckpoints request
            fmap (fmap markXaiServerCompactionCheckpoint) $
                createResponseWithEvents
                    (optionsForRequest projectedRequest)
                    credential
                    projectedRequest
                    onEvent)

-- | Same mapping as 'xaiBackend', with an injectable transport for tests and
-- downstream integrations.
xaiBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
xaiBackendWith send =
    statelessResponsesBackend \request onEvent ->
        fmap (fmap markXaiServerCompactionCheckpoint)
            (send (projectXaiCheckpoints request) onEvent)

-- | Local-only marker kept immediately after xAI's opaque checkpoint.
xaiCompactionCheckpointOriginItem :: ResponseItem
xaiCompactionCheckpointOriginItem =
    compactionCheckpointOriginItem "xai"

isXaiCompactionCheckpointOriginItem :: ResponseItem -> Bool
isXaiCompactionCheckpointOriginItem item =
    responseItemCompactionCheckpointOrigin item == Just "xai"

-- Ordinary backend requests may contain history from a previous provider.
-- Require explicit xAI provenance before replaying an opaque checkpoint.
projectXaiCheckpoints :: ResponseCreateParams -> ResponseCreateParams
projectXaiCheckpoints =
    filterRequestCompactionCheckpointsByOrigin (== Just "xai")

-- Mark only a checkpoint emitted by this xAI response. Existing request
-- history may contain an indistinguishable checkpoint from another provider.
markXaiServerCompactionCheckpoint :: Response -> Response
markXaiServerCompactionCheckpoint response =
    let Response{..} = response
    in Response
        { output = markLatestCheckpoint output
        , ..
        }
  where
    markLatestCheckpoint items =
        case break isServerCompactionCheckpoint (reverse items) of
            (_, []) -> items
            (after, checkpoint : before) ->
                reverse before
                    <> ( checkpoint
                        : xaiCompactionCheckpointOriginItem
                        : reverse after
                       )
