-- | Display-only history for uncommitted response attempts, and bounded
-- delivery of live events. This history must never enter backend/model state.
module Agent.Loop.DisplayJournal
    ( DisplayJournalEntry
    , replayableDisplayEvent
    , recordDisplayEvent
    , displayEventsFromJournal
    , discardCurrentDisplayAttempt
    , LoopEventPump
    , emitLoopEvent
    ) where

import Agent.Loop.EventPump
    ( EventPump
    , emitAppendedText
    , emitEvent
    , emitLatestText
    )
import Agent.Loop.Output (LoopEvent(..))
import Agent.ToolDispatch (ToolCallResult(..), setToolCallArguments)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Events that can be normalized into stable, display-only response items.
-- Reasoning is intentionally omitted: durable session history has always
-- treated provider scratchpad as live-only.
replayableDisplayEvent :: LoopEvent -> Bool
replayableDisplayEvent = \case
    TextDelta _ -> True
    ToolStarted _ -> True
    ToolUpdated _ -> True
    ToolArgumentsUpdated _ -> True
    ToolOutputUpdated _ _ -> True
    ToolFinished _ -> True
    ToolRetracted _ -> True
    _ -> False

-- The journal and each text chunk list are stored newest-first. Keeping
-- adjacent deltas as chunks avoids repeatedly copying the accumulated prefix.
-- A restart is the boundary between the current provider attempt and older
-- attempts that remain visible.
data DisplayJournalEntry
    = DisplayTextChunks ![Text]
    | DisplayEvent !LoopEvent

recordDisplayEvent
    :: LoopEvent
    -> [DisplayJournalEntry]
    -> [DisplayJournalEntry]
recordDisplayEvent event events = case event of
    TextDelta delta ->
        case events of
            DisplayTextChunks chunks : rest ->
                DisplayTextChunks (delta : chunks) : rest
            _ -> DisplayTextChunks [delta] : events
    ToolUpdated call ->
        DisplayEvent event : removeCurrentToolUpdates call.callId events
    ToolArgumentsUpdated call ->
        DisplayEvent event : removeCurrentToolUpdates call.callId events
    ToolOutputUpdated callId output ->
        DisplayEvent (ToolOutputUpdated callId (boundLoopToolOutput output))
            : removeCurrentToolOutput callId events
    ToolFinished result ->
        DisplayEvent
            (ToolFinished
                result
                    { output = boundLoopToolOutput result.output
                    })
            : removeCurrentToolOutput result.callId events
    ToolRetracted callId ->
        removeCurrentToolEvents callId events
    _ -> DisplayEvent event : events

displayEventsFromJournal :: [DisplayJournalEntry] -> [LoopEvent]
displayEventsFromJournal =
    map entryToEvent . reverse
  where
    entryToEvent = \case
        DisplayTextChunks chunks ->
            TextDelta (Text.concat (reverse chunks))
        DisplayEvent event -> event

discardCurrentDisplayAttempt
    :: [DisplayJournalEntry]
    -> [DisplayJournalEntry]
discardCurrentDisplayAttempt =
    dropWhile \case
        DisplayEvent (ResponseRestarted _) -> False
        _ -> True

removeCurrentToolUpdates
    :: Text
    -> [DisplayJournalEntry]
    -> [DisplayJournalEntry]
removeCurrentToolUpdates callId =
    filterCurrentAttempt \case
        DisplayEvent (ToolUpdated call) -> call.callId /= callId
        DisplayEvent (ToolArgumentsUpdated call) -> call.callId /= callId
        _ -> True

removeCurrentToolOutput
    :: Text
    -> [DisplayJournalEntry]
    -> [DisplayJournalEntry]
removeCurrentToolOutput callId =
    filterCurrentAttempt \case
        DisplayEvent (ToolOutputUpdated identifier _) ->
            identifier /= callId
        _ -> True

removeCurrentToolEvents
    :: Text
    -> [DisplayJournalEntry]
    -> [DisplayJournalEntry]
removeCurrentToolEvents callId =
    filterCurrentAttempt \case
        DisplayEvent (ToolStarted call) -> call.callId /= callId
        DisplayEvent (ToolUpdated call) -> call.callId /= callId
        DisplayEvent (ToolArgumentsUpdated call) -> call.callId /= callId
        DisplayEvent (ToolOutputUpdated identifier _) ->
            identifier /= callId
        DisplayEvent (ToolFinished result) -> result.callId /= callId
        _ -> True

filterCurrentAttempt
    :: (DisplayJournalEntry -> Bool)
    -> [DisplayJournalEntry]
    -> [DisplayJournalEntry]
filterCurrentAttempt keep = go
  where
    go [] = []
    go allEvents@(DisplayEvent (ResponseRestarted _) : _) = allEvents
    go (event : rest)
        | keep event = event : go rest
        | otherwise = go rest

data LoopEventCoalescingKey
    = AssistantTextDelta
    | AssistantReasoningDelta
    | ToolArgumentsSnapshot !Text
    | ToolOutputSnapshot !Text
    | NativeAgentOutputDelta !Text
    deriving (Eq)

type LoopEventPump = EventPump LoopEventCoalescingKey LoopEvent

emitLoopEvent :: LoopEventPump -> LoopEvent -> IO ()
emitLoopEvent pump = \case
    TextDelta text ->
        emitAppendedText pump AssistantTextDelta TextDelta text
    ReasoningDelta text ->
        emitAppendedText pump AssistantReasoningDelta ReasoningDelta text
    ToolArgumentsUpdated call ->
        emitLatestText
            pump
            (ToolArgumentsSnapshot call.callId)
            (\arguments ->
                ToolArgumentsUpdated (setToolCallArguments arguments call))
            call.arguments
    ToolOutputUpdated callId output ->
        emitLatestText
            pump
            (ToolOutputSnapshot callId)
            (ToolOutputUpdated callId)
            (boundLoopToolOutput output)
    NativeAgentOutput identifier output ->
        emitAppendedText
            pump
            (NativeAgentOutputDelta identifier)
            (NativeAgentOutput identifier)
            output
    event ->
        emitEvent pump event

-- Tool output callbacks carry cumulative snapshots. Keep the coalesced value
-- bounded even when a provider sends one giant snapshot; the complete result
-- remains available through the normal tool-result or artifact path.
boundLoopToolOutput :: Text -> Text
boundLoopToolOutput output
    | Text.length output <= loopEventTailPayloadBudgetCodeUnits =
        Text.copy output
    | otherwise =
        toolOutputOmissionMarker
            <> Text.copy (Text.takeEnd loopEventTailPayloadCodeUnits output)

loopEventTailPayloadCodeUnits :: Int
loopEventTailPayloadCodeUnits =
    max 0
        ( loopEventTailPayloadBudgetCodeUnits
            - Text.length toolOutputOmissionMarker
        )

toolOutputOmissionMarker :: Text
toolOutputOmissionMarker = "[earlier tool output truncated]\n"

loopEventTailPayloadBudgetCodeUnits :: Int
loopEventTailPayloadBudgetCodeUnits =
    (8 * 1024 * 1024 - 64) `div` 4
