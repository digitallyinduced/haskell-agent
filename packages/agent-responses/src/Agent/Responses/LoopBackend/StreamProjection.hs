-- | Project one provider response attempt into live loop events.
module Agent.Responses.LoopBackend.StreamProjection
    ( streamEventToLoopEvent
    , streamEventToLoopEventWithRawReasoning
    , StreamProjectionState
    , emptyStreamProjectionState
    , streamEventToLoopEventsStep
    , newStreamEventToLoopEvents
    , toolArgumentActivityChunkChars
    , runawayToolArgumentWarningChars
    , streamOutputObserved
    ) where

import Agent.Loop (LoopEvent(..))
import Agent.Responses.LoopBackend.Output (responseItemToToolCall)
import Agent.Responses.LoopBackend.ToolArgumentPreview
import Agent.Responses.Types
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (isJust, maybeToList)
import qualified Data.Text as Text

streamEventToLoopEvent :: ResponseStreamEvent -> Maybe LoopEvent
streamEventToLoopEvent = streamEventToLoopEventWithRawReasoning True

-- | Convert a Responses stream event, optionally suppressing raw
-- @response.reasoning_text.delta@ events. Summary deltas are always exposed.
streamEventToLoopEventWithRawReasoning
    :: Bool
    -> ResponseStreamEvent
    -> Maybe LoopEvent
streamEventToLoopEventWithRawReasoning showRawReasoning = \case
    -- Publish a tool block as soon as the provider announces the output item.
    -- The loop will still execute the call only after the complete response
    -- has been assembled; this event is purely a live UI projection.
    ResponseOutputItemAddedEvent { item }
        | Just call <- responseItemToToolCall item ->
            Just (ToolStarted call)
    -- Providers commonly send the call arguments in deltas and include the
    -- complete call in the corresponding done event. Replace the placeholder
    -- block's metadata/body before the core loop starts executing it.
    ResponseOutputItemDoneEvent { item }
        | Just call <- responseItemToToolCall item ->
            Just (ToolUpdated call)
    ResponseReasoningSummaryPartAddedEvent
        { summaryIndex = Just index }
        | index > 0 ->
            Just (ReasoningDelta "\n\n")
    ResponseCodexRateLimitsEvent { rateLimits = limits } ->
            codexRateLimitsWarning limits
    OtherResponseStreamEvent
        { otherEventType = StreamEventUnknown eventType } ->
            Just (ActivityUpdated
                ("Warning: unsupported provider event " <> eventType))
    OtherResponseStreamEvent { otherEventType, eventDelta } ->
        case eventDelta of
            Just text | Text.null text -> Nothing
            Just text -> case otherEventType of
                EventOutputTextDelta -> Just (TextDelta text)
                EventReasoningTextDelta
                    | showRawReasoning -> Just (ReasoningDelta text)
                    | otherwise -> Nothing
                EventReasoningSummaryTextDelta -> Just (ReasoningDelta text)
                _ -> Nothing
            Nothing -> Nothing
    _ -> Nothing

codexRateLimitsWarning :: CodexRateLimits -> Maybe LoopEvent
codexRateLimitsWarning limits =
    if reached || not (null lowWindows)
        then Just (WarningRaised
            (headline <> foldMap formatWindows (nonEmpty lowWindows)))
        else Nothing
  where
    windows =
        [ ("primary", used)
        | used <- maybeToList limits.primaryUsedPercent
        ]
        <> [ ("secondary", used)
           | used <- maybeToList limits.secondaryUsedPercent
           ]
    lowWindows = filter ((>= 90) . snd) windows
    reached =
        limits.limitReached == Just True
            || limits.allowed == Just False
            || any ((>= 100) . snd) windows
    headline
        | reached =
            "Codex usage limit reached. Check /usage for reset details."
        | otherwise = "Codex usage is low"
    nonEmpty [] = Nothing
    nonEmpty values = Just values
    formatWindows values =
        ": " <> Text.intercalate " · "
            [ label <> " " <> formatRemaining (max 0 (100 - used))
                <> "% left"
            | (label, used) <- values
            ]
            <> ". Check /usage for reset details."
    formatRemaining value
        | value == fromIntegral (round value :: Int) =
            Text.pack (show (round value :: Int))
        | otherwise = Text.pack (show value)

-- | Stateful projection of one streamed response attempt into loop events.
--
-- On top of 'streamEventToLoopEventWithRawReasoning' this surfaces safe
-- streamed arguments as repaintable tool previews and reports coarse activity
-- for encrypted or computer-use calls. It also warns when a model gets stuck
-- in a degenerate repetition loop inside one call (observed as multi-minute
-- 128k-output-token samples whose arguments repeat @\\u0000@ or a hallucinated
-- path segment).
--
-- Build one projector per response attempt so counters describe a single
-- provider sample.
newStreamEventToLoopEvents
    :: Bool
    -> IO (ResponseStreamEvent -> IO [LoopEvent])
newStreamEventToLoopEvents showRawReasoning = do
    stateRef <- newIORef emptyStreamProjectionState
    pure \event ->
        atomicModifyIORef' stateRef \state ->
            streamEventToLoopEventsStep showRawReasoning state event

-- | Immutable state for projecting one response attempt.
--
-- The constructor is intentionally private so callers cannot accidentally
-- carry only part of the projection state across an attempt boundary.
data StreamProjectionState = StreamProjectionState
    { streamToolArguments :: !ToolArgumentStreamState
    }

emptyStreamProjectionState :: StreamProjectionState
emptyStreamProjectionState =
    StreamProjectionState emptyToolArgumentStreamState

-- | Pure projection of one provider event. The returned state belongs to the
-- same response attempt; start from 'emptyStreamProjectionState' when a retry
-- or reconnect begins a new sample.
streamEventToLoopEventsStep
    :: Bool
    -> StreamProjectionState
    -> ResponseStreamEvent
    -> (StreamProjectionState, [LoopEvent])
streamEventToLoopEventsStep showRawReasoning state event =
    ( StreamProjectionState nextArguments
    , maybeToList
        (statefulStreamEventToLoopEvent showRawReasoning event)
        <> argumentEvents
    )
  where
    (nextArguments, argumentEvents) =
        toolArgumentStreamStep event state.streamToolArguments

-- Function and custom-tool done items are projected by
-- 'toolArgumentStreamStep' so a sparse provider item can be reconciled with
-- arguments accumulated from deltas. Every other event retains the pure
-- projection used by stateless callers.
statefulStreamEventToLoopEvent
    :: Bool
    -> ResponseStreamEvent
    -> Maybe LoopEvent
statefulStreamEventToLoopEvent showRawReasoning event = case event of
    ResponseOutputItemDoneEvent { item = FunctionCallItem _ } -> Nothing
    ResponseOutputItemDoneEvent { item = CustomToolCallItem _ } -> Nothing
    _ -> streamEventToLoopEventWithRawReasoning showRawReasoning event

-- | Whether a stream event proves the provider has begun producing response
-- output. These events make replay unsafe even when they do not map to a
-- visible loop delta.
streamOutputObserved :: ResponseStreamEvent -> Bool
streamOutputObserved event = case event of
    ResponseCompletedEvent{} -> True
    ResponseDoneEvent{} -> True
    ResponseIncompleteEvent { responseValue } ->
        not (null responseValue.output)
    ResponseFailedEvent { responseValue } ->
        not (null responseValue.output)
    ResponseOutputItemAddedEvent{} -> True
    ResponseOutputItemDoneEvent{} -> True
    ResponseFunctionCallArgumentsDeltaEvent{} -> True
    ResponseFunctionCallArgumentsDoneEvent{} -> True
    ResponseCustomToolInputDeltaEvent{} -> True
    ResponseCustomToolInputDoneEvent{} -> True
    ResponseReasoningSummaryPartAddedEvent{} -> True
    ResponseReasoningSummaryTextDoneEvent{} -> True
    OtherResponseStreamEvent { otherEventType }
        | streamEventTypeText otherEventType == unparsedStreamEventTypeText ->
            False
    _ ->
        responseStreamEventType event /= EventCodexRateLimits
            && isJust (streamEventToLoopEvent event)
