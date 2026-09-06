-- | Native turn status, completion, and usage event delivery.
module Agent.CLI.MacOS.TurnEvents
    ( finishTurnEvent
    , taskResultSessionId
    , taskResultState
    , nativeLoopEvent
    , turnStatusEvent
    , sendTurnStatus
    , turnFailedEvent
    ) where

import Agent.CLI.MacOS.EngineEvents
import Agent.CLI.MacOS.NativeLoopEvent (encodeNativeUsageEvent)
import Agent.CLI.MacOS.TurnState
import Agent.Loop (LoopEvent(..))
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Foreign.Ptr (FunPtr, Ptr)

finishTurnEvent
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> TaskResult
    -> IO ()
finishTurnEvent callback context turnId = \case
    TaskFailure message ->
        sendEvent callback context $
            turnFailedEvent turnId message
    TaskOutcome outcome -> do
        forM_
            (encodeNativeUsageEvent
                True
                turnId
                outcome.turnOutcomeUsage
                outcome.turnOutcomeProviderCostUSD)
            (sendBinaryEvent callback context)
        case outcome.turnOutcomeError of
            Just err ->
                sendEvent callback context (turnFailedEvent turnId err)
            Nothing ->
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.completed" :: Text)
                        , "turnId" Aeson..= turnId
                        , "sessionId" Aeson..=
                            outcome.turnOutcomeSessionId
                        ]

taskResultSessionId :: Maybe Text -> TaskResult -> Maybe Text
taskResultSessionId fallback = \case
    TaskFailure _ -> fallback
    TaskOutcome outcome -> outcome.turnOutcomeSessionId <|> fallback

taskResultState :: TaskResult -> Text
taskResultState = \case
    TaskFailure _ -> "failed"
    TaskOutcome outcome ->
        case outcome.turnOutcomeError of
            Just _ -> "failed"
            Nothing -> "succeeded"

nativeLoopEvent :: Text -> LoopEvent -> Maybe Aeson.Value
nativeLoopEvent turnId = \case
    ActivityUpdated status -> Just $ turnStatusEvent turnId status
    WarningRaised warning -> Just $ turnStatusEvent turnId warning
    ResponseRestarted message -> Just $ turnStatusEvent turnId message
    _ -> Nothing

turnStatusEvent :: Text -> Text -> Aeson.Value
turnStatusEvent turnId status =
    Aeson.object
        [ "event" Aeson..= ("turn.status" :: Text)
        , "turnId" Aeson..= turnId
        , "status" Aeson..= status
        ]

sendTurnStatus
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> Text
    -> IO ()
sendTurnStatus callback context turnId =
    sendEvent callback context . turnStatusEvent turnId

turnFailedEvent :: Text -> Text -> Aeson.Value
turnFailedEvent turnId message =
    Aeson.object
        [ "event" Aeson..= ("turn.failed" :: Text)
        , "turnId" Aeson..= turnId
        , "error" Aeson..= message
        ]
