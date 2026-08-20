-- | Map the provider-neutral loop onto the xAI Grok subscription transport.
--
-- The proxy does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. Callers own the 'IORef' so a resumed session can
-- seed history and the CLI can persist it.
module Agent.XAI.LoopBackend
    ( xaiBackend
    , xaiBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend(..), LoopEvent, TurnInput, TurnOutput)
import Agent.OpenAI.LoopBackend
    ( responseToTurnOutput
    , streamEventToLoopEvent
    , turnInputsToItems
    , withRequestInput
    )
import Agent.OpenAI.Responses.Types
import Agent.Provider (Credential)
import Agent.XAI.Client (createResponseWithEvents)
import Agent.XAI.Options (ClientOptions)
import Data.IORef

-- | Close over xAI options, credential, and the request fields the loop does
-- not own (model, instructions, tools, reasoning). The params action is
-- re-run each turn so the REPL can change reasoning effort without dropping
-- the local transcript.
xaiBackend
    :: ClientOptions
    -> Credential
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
xaiBackend options credential =
    xaiBackendWith (createResponseWithEvents options credential)

-- | Same mapping as 'xaiBackend', with an injectable transport for tests.
xaiBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
xaiBackendWith send getParams transcript = Backend \_previousResponseId inputs onEvent -> do
    baseParams <- getParams
    submitXaiTurn send baseParams transcript inputs onEvent

submitXaiTurn
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> IORef [ResponseItem]
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitXaiTurn send baseParams transcript inputs onEvent = do
    history <- readIORef transcript
    let newItems = turnInputsToItems inputs
        requestItems = history <> newItems
        request = withRequestInput baseParams requestItems
    result <- send request \event ->
        mapM_ onEvent (streamEventToLoopEvent event)
    case result of
        Left err -> pure (Left err)
        Right response -> do
            writeIORef transcript (requestItems <> response.output)
            pure (Right (responseToTurnOutput response))
