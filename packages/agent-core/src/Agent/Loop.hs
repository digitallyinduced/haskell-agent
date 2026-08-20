-- | Provider-neutral agent loop: submit a user turn, dispatch tool calls,
-- feed results back, repeat until the model answers in text or hits a cap.
--
-- Transports close over model, instructions, and tool schemas. This module
-- only sees 'ToolCall' / 'ToolCallResult' and a 'Backend' callback.
module Agent.Loop
    ( Backend(..)
    , LoopConfig(..)
    , LoopEvent(..)
    , LoopError(..)
    , LoopResult(..)
    , TurnInput(..)
    , TurnOutput(..)
    , defaultLoopMaxTurns
    , defaultLoopDispatch
    , runLoop
    , runLoopWith
    ) where

import Agent.Error (ApiError)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolHandler
    , dispatchToolCall
    )
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (SomeException)
import Data.Text (Text)
import qualified Data.Text as Text

data TurnInput
    = UserMessage Text
    | CompletedTool ToolCallResult
    deriving (Eq, Show)

data TurnOutput = TurnOutput
    { responseId :: !Text
    , toolCalls :: ![ToolCall]
    , assistantText :: !(Maybe Text)
    } deriving (Eq, Show)

newtype Backend = Backend
    { submitTurn
        :: Maybe Text
        -> [TurnInput]
        -> (LoopEvent -> IO ())
        -> IO (Either ApiError TurnOutput)
    }

data LoopEvent
    = TextDelta Text
    | ReasoningDelta Text
    | TurnStarted
    | TurnFinished TurnOutput
    | ToolStarted ToolCall
    | ToolFinished ToolCallResult
    deriving (Eq, Show)

data LoopConfig = LoopConfig
    { loopBackend :: !Backend
    , loopHandlers :: ![ToolHandler]
    , loopDispatch :: !ToolDispatchConfig
    , loopMaxTurns :: !Int
    , loopOnEvent :: !(LoopEvent -> IO ())
    , loopApprove :: !(ToolCall -> IO Bool)
    }

data LoopResult = LoopResult
    { finalResponseId :: !Text
    , finalText :: !(Maybe Text)
    , turnsUsed :: !Int
    } deriving (Eq, Show)

data LoopError
    = LoopTransport ApiError
    | LoopMaxTurns TurnOutput
    | LoopNoResponseId
    deriving (Eq, Show)

defaultLoopMaxTurns :: Int
defaultLoopMaxTurns = 50

-- | CLI-facing formatter: unknown tools, handler errors, and crashes stay
-- in-band as tool output so the model can continue.
defaultLoopDispatch :: ToolDispatchConfig
defaultLoopDispatch = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "Unknown tool: " <> name
    , toolDispatchFormatResult = \case
        Left err -> "Error: " <> err
        Right output -> output
    , toolDispatchFormatException = \name exception ->
        "Tool " <> name <> " crashed: " <> Text.pack (show exception)
    , toolDispatchOnException = \_name (_ :: SomeException) -> pure ()
    }

runLoop
    :: LoopConfig
    -> Maybe Text
    -> Text
    -> IO (Either LoopError LoopResult)
runLoop config previousResponseId prompt =
    runLoopWith config previousResponseId [UserMessage prompt]

-- | Like 'runLoop', but the first turn may include leading context items
-- (for example AGENTS.md) before the user message.
runLoopWith
    :: LoopConfig
    -> Maybe Text
    -> [TurnInput]
    -> IO (Either LoopError LoopResult)
runLoopWith config0 previousResponseId initialInputs = do
    -- Tools run with mapConcurrently. Serialize onEvent so a printer
    -- (hPutStrLn on String is not atomic) cannot interleave characters.
    eventLock <- newMVar ()
    let config = config0
            { loopOnEvent = \event ->
                withMVar eventLock \_ -> config0.loopOnEvent event
            }
        go prev turnsUsed inputs lastOutput
            | turnsUsed >= config.loopMaxTurns =
                pure $ case lastOutput of
                    Just turn -> Left (LoopMaxTurns turn)
                    Nothing -> Left LoopNoResponseId
            | otherwise = do
                config.loopOnEvent TurnStarted
                result <- config.loopBackend.submitTurn prev inputs config.loopOnEvent
                case result of
                    Left err -> pure (Left (LoopTransport err))
                    Right turn
                        | Text.null turn.responseId ->
                            pure (Left LoopNoResponseId)
                        | otherwise -> do
                            config.loopOnEvent (TurnFinished turn)
                            let nextTurnsUsed = turnsUsed + 1
                            if null turn.toolCalls
                                then pure $ Right LoopResult
                                    { finalResponseId = turn.responseId
                                    , finalText = turn.assistantText
                                    , turnsUsed = nextTurnsUsed
                                    }
                                else do
                                    results <- mapConcurrently (runOne config) turn.toolCalls
                                    go (Just turn.responseId) nextTurnsUsed
                                        (map CompletedTool results) (Just turn)
    go previousResponseId 0 initialInputs Nothing

runOne :: LoopConfig -> ToolCall -> IO ToolCallResult
runOne config call = do
    config.loopOnEvent (ToolStarted call)
    approved <- config.loopApprove call
    result <- if approved
        then dispatchToolCall config.loopDispatch config.loopHandlers call
        else pure ToolCallResult
            { callId = call.callId
            , output = "Tool call rejected by user."
            , callKind = call.callKind
            }
    config.loopOnEvent (ToolFinished result)
    pure result
