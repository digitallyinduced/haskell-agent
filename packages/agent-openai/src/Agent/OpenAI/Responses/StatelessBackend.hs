-- | Shared loop adapter for stateless Responses transports.
--
-- Stateless providers cannot continue a server-side response chain. Keep the
-- canonical Responses item transcript locally and replay it on every turn.
module Agent.OpenAI.Responses.StatelessBackend
    ( statelessResponsesBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend(..))
import Agent.OpenAI.LoopBackend
    ( responseToTurnOutput
    , streamEventToLoopEvent
    , turnInputsToItems
    , withRequestInput
    )
import Agent.OpenAI.Responses.Types
import Data.IORef

-- | Build a provider-neutral loop backend from a stateless Responses send
-- function. The loop's @previous_response_id@ is intentionally ignored;
-- successful response output is appended to the locally owned transcript.
statelessResponsesBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
statelessResponsesBackendWith send getParams transcript =
    Backend \_previousResponseId inputs onEvent -> do
        baseParams <- getParams
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
