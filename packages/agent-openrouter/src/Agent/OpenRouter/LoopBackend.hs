-- | Map the provider-neutral loop onto the OpenRouter Responses transport.
--
-- OpenRouter does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items.
module Agent.OpenRouter.LoopBackend
    ( openRouterBackend
    , openRouterBackendWith
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
import Agent.OpenRouter.Client (createResponseWithEvents)
import Agent.OpenRouter.Options (ClientOptions)
import Data.IORef

-- | Close over OpenRouter options, credential, and the request fields the
-- loop does not own (model, instructions, tools, reasoning).
openRouterBackend :: ClientOptions -> Credential -> ResponseCreateParams -> IO Backend
openRouterBackend options credential =
    openRouterBackendWith (createResponseWithEvents options credential)

-- | Same mapping as 'openRouterBackend', with an injectable transport for tests.
openRouterBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> IO Backend
openRouterBackendWith send baseParams = do
    transcript <- newIORef []
    pure $ Backend \_previousResponseId inputs onEvent ->
        submitOpenRouterTurn send baseParams transcript inputs onEvent

submitOpenRouterTurn
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> IORef [ResponseItem]
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitOpenRouterTurn send baseParams transcript inputs onEvent = do
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
