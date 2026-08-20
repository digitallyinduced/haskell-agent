-- | Provider-neutral agent loop: submit a user turn, dispatch tool calls,
-- feed results back, repeat until the model answers in text or hits a cap.
--
-- Transports close over model, instructions, and tool schemas. This module
-- only sees 'ToolCall' / 'ToolCallResult' and a 'Backend' callback.
module Agent.Loop
    ( Backend(..)
    , ImageAttachment(..)
    , LoopConfig(..)
    , LoopEvent(..)
    , LoopError(..)
    , LoopResult(..)
    , TurnInput(..)
    , TurnOutput(..)
    , defaultLoopMaxTurns
    , defaultLoopDispatch
    , runLoop
    , runLoopInputs
    ) where

import Agent.Cancel (CancelFlag, isCancelled, resetCancel, waitCancel)
import Agent.Error (ApiError)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolHandler
    , dispatchToolCall
    )
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Image bytes attached to a user turn (PNG/JPEG/…).
data ImageAttachment = ImageAttachment
    { imageMime :: !Text
    , imageBytes :: !ByteString
    } deriving (Eq, Show)

data TurnInput
    = UserMessage Text
    | UserMultimodal
        { userText :: !Text
        , userImages :: ![ImageAttachment]
        }
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
    -- | 'Left' denies with that tool-output message; 'Right True' runs the
    -- tool; 'Right False' uses the usual user-rejection string.
    , loopApprove :: !(ToolCall -> IO (Either Text Bool))
      -- | Soft-cancel latch. When set, the loop stops after the current tool
      -- batch instead of asking the model for another step.
    , loopCancel :: !CancelFlag
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
    -- | Soft-cancel after tools ran. Carries the tool results that must be
    -- committed to the local transcript so function_call items are not left
    -- without matching outputs.
    | LoopCancelled [ToolCallResult]
    deriving (Eq, Show)

defaultLoopMaxTurns :: Int
defaultLoopMaxTurns = 500

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
    runLoopInputs config previousResponseId [UserMessage prompt]

-- | Same as 'runLoop', but the first turn may be multimodal.
runLoopInputs
    :: LoopConfig
    -> Maybe Text
    -> [TurnInput]
    -> IO (Either LoopError LoopResult)
runLoopInputs config0 previousResponseId firstInputs = do
    -- Tools run with mapConcurrently. Serialize onEvent so a printer
    -- (hPutStrLn on String is not atomic) cannot interleave characters.
    eventLock <- newMVar ()
    resetCancel config0.loopCancel
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
                cancelled <- isCancelled config.loopCancel
                if cancelled
                    then pure (Left (LoopCancelled []))
                    else do
                        config.loopOnEvent TurnStarted
                        -- Race the model call against cancel so Ctrl-C / Esc
                        -- can stop reasoning mid-stream, not only between tools.
                        raced <- race
                            (waitCancel config.loopCancel)
                            (config.loopBackend.submitTurn prev inputs config.loopOnEvent)
                        case raced of
                            Left () ->
                                pure (Left (LoopCancelled []))
                            Right (Left err) ->
                                pure (Left (LoopTransport err))
                            Right (Right turn)
                                | Text.null turn.responseId ->
                                    pure (Left LoopNoResponseId)
                                | otherwise -> do
                                    -- A cancel that landed during submitTurn
                                    -- after the race chose Right still counts.
                                    cancelledMid <- isCancelled config.loopCancel
                                    if cancelledMid
                                        then pure (Left (LoopCancelled []))
                                        else do
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
                                                    cancelledAfter <- isCancelled config.loopCancel
                                                    if cancelledAfter
                                                        then pure (Left (LoopCancelled results))
                                                        else go (Just turn.responseId) nextTurnsUsed
                                                            (map CompletedTool results) (Just turn)
    go previousResponseId 0 firstInputs Nothing

runOne :: LoopConfig -> ToolCall -> IO ToolCallResult
runOne config call = do
    config.loopOnEvent (ToolStarted call)
    approved <- config.loopApprove call
    result <- case approved of
        Left denial ->
            pure ToolCallResult
                { callId = call.callId
                , output = denial
                , callKind = call.callKind
                }
        Right False ->
            pure ToolCallResult
                { callId = call.callId
                , output = "Tool call rejected by user."
                , callKind = call.callKind
                }
        Right True ->
            dispatchToolCall config.loopDispatch config.loopHandlers call
    config.loopOnEvent (ToolFinished result)
    pure result
