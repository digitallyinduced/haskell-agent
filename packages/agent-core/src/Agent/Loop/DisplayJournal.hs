-- | Display-only history for uncommitted response attempts, and bounded
-- delivery of live events. This history must never enter backend/model state.
module Agent.Loop.DisplayJournal
    ( DisplayJournal
    , emptyDisplayJournal
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
import qualified Data.Set as Set
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

-- Admission remains constant-time, replacing only superseded adjacent snapshots.
-- Successful responses clear the journal without projecting it; other
-- normalization waits until failed output is retained. Entries and text chunks
-- are newest-first. Tails deliberately
-- remain lazy, preserving prefix release and sharing during retractions.
data DisplayJournal
    = EmptyDisplayJournal
    | DisplayJournalText ![Text] DisplayJournal
    | DisplayJournalEvent !LoopEvent DisplayJournal

emptyDisplayJournal :: DisplayJournal
emptyDisplayJournal = EmptyDisplayJournal

recordDisplayEvent
    :: LoopEvent
    -> DisplayJournal
    -> DisplayJournal
recordDisplayEvent event journal = case event of
    TextDelta delta -> case journal of
        DisplayJournalText chunks rest ->
            DisplayJournalText (delta : chunks) rest
        _ -> DisplayJournalText [delta] journal
    ToolOutputUpdated callId output ->
        recordSnapshot (ToolOutputUpdated callId (boundLoopToolOutput output))
            journal
    ToolFinished result ->
        DisplayJournalEvent
            (ToolFinished result { output = boundLoopToolOutput result.output }) journal
    ToolRetracted callId -> retractRawTool callId journal
    _ -> recordSnapshot event journal

-- Release superseded adjacent snapshots at admission, not just when a failed
-- attempt is projected. Cumulative argument previews otherwise retain every
-- earlier prefix until commit. Inspect only the head: scanning across text,
-- other calls, or restart boundaries would make admission depend on history
-- size. Nonadjacent snapshots retain the existing projection-time deduplication.
recordSnapshot :: LoopEvent -> DisplayJournal -> DisplayJournal
recordSnapshot event = \case
    DisplayJournalEvent previous rest
        | replaces previous -> DisplayJournalEvent event rest
    journal -> DisplayJournalEvent event journal
  where
    replaces previous = case (event, previous) of
        (ToolUpdated call, ToolUpdated old) -> call.callId == old.callId
        (ToolUpdated call, ToolArgumentsUpdated old) -> call.callId == old.callId
        (ToolArgumentsUpdated call, ToolUpdated old) -> call.callId == old.callId
        (ToolArgumentsUpdated call, ToolArgumentsUpdated old) ->
            call.callId == old.callId
        (ToolOutputUpdated callId _, ToolOutputUpdated oldId _) ->
            callId == oldId
        _ -> False

-- Retractions keep the former lazy-filter behavior: forcing the new journal
-- immediately releases a removed leading prefix, and the remaining tail is
-- filtered as demanded. Do not retain retracted payloads in a raw operation log.
-- Distinct text nodes stay distinct, even if filtering makes them adjacent.
retractRawTool :: Text -> DisplayJournal -> DisplayJournal
retractRawTool callId = go
  where
    go EmptyDisplayJournal = EmptyDisplayJournal
    go boundary@(DisplayJournalEvent (ResponseRestarted _) _) = boundary
    go (DisplayJournalText chunks rest) =
        DisplayJournalText chunks (go rest)
    go (DisplayJournalEvent event rest)
        | belongsToTool event = go rest
        | otherwise = DisplayJournalEvent event (go rest)
    belongsToTool = \case
        ToolStarted call -> call.callId == callId
        ToolUpdated call -> call.callId == callId
        ToolArgumentsUpdated call -> call.callId == callId
        ToolOutputUpdated identifier _ -> identifier == callId
        ToolFinished result -> result.callId == callId
        _ -> False

-- One newest-first pass keeps the latest snapshot of each kind per call ID.
-- A finish suppresses earlier output snapshots, but is itself always retained.
-- Sets reset at restart boundaries: providers may reuse IDs on later attempts.
-- Consing retained entries yields chronological output without an ordered map.
-- Raw text nodes remain distinct even when retraction removed their separator.
displayEventsFromJournal :: DisplayJournal -> [LoopEvent]
displayEventsFromJournal journal = go journal Set.empty Set.empty []
  where
    go EmptyDisplayJournal _ _ result = result
    go (DisplayJournalText chunks rest) updates outputs result =
        go rest updates outputs
            (TextDelta (Text.concat (reverse chunks)) : result)
    go (DisplayJournalEvent event rest) updates outputs result =
        case event of
            ToolUpdated call -> keepUpdate call.callId
            ToolArgumentsUpdated call -> keepUpdate call.callId
            ToolOutputUpdated callId _
                | Set.member callId outputs -> go rest updates outputs result
                | otherwise ->
                    go rest updates (Set.insert callId outputs) (event : result)
            ToolFinished callResult ->
                go rest updates (Set.insert callResult.callId outputs) (event : result)
            ResponseRestarted _ ->
                go rest Set.empty Set.empty (event : result)
            _ -> go rest updates outputs (event : result)
      where
        keepUpdate callId
            | Set.member callId updates = go rest updates outputs result
            | otherwise =
                go rest (Set.insert callId updates) outputs (event : result)

discardCurrentDisplayAttempt
    :: DisplayJournal
    -> DisplayJournal
discardCurrentDisplayAttempt = \case
    EmptyDisplayJournal -> EmptyDisplayJournal
    boundary@(DisplayJournalEvent (ResponseRestarted _) _) -> boundary
    DisplayJournalText _ rest -> discardCurrentDisplayAttempt rest
    DisplayJournalEvent _ rest -> discardCurrentDisplayAttempt rest

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
