-- Frozen list journal from 1d17af7e9. Do not replace its scans with production
-- helpers: it is the retained before-change performance/semantics baseline.
module DisplayJournalBaseline
    ( DisplayJournalEntry
    , recordDisplayEvent
    , displayEventsFromJournal
    , discardCurrentDisplayAttempt
    ) where

import Agent.Loop.Output (LoopEvent(..))
import Agent.ToolDispatch (ToolCallResult(..))
import Data.Text (Text)
import qualified Data.Text as Text

data DisplayJournalEntry
    = DisplayTextChunks ![Text]
    | DisplayEvent !LoopEvent

recordDisplayEvent :: LoopEvent -> [DisplayJournalEntry] -> [DisplayJournalEntry]
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
            (ToolFinished result { output = boundLoopToolOutput result.output })
            : removeCurrentToolOutput result.callId events
    ToolRetracted callId ->
        removeCurrentToolEvents callId events
    _ -> DisplayEvent event : events

displayEventsFromJournal :: [DisplayJournalEntry] -> [LoopEvent]
displayEventsFromJournal = map entryToEvent . reverse
  where
    entryToEvent = \case
        DisplayTextChunks chunks -> TextDelta (Text.concat (reverse chunks))
        DisplayEvent event -> event

discardCurrentDisplayAttempt :: [DisplayJournalEntry] -> [DisplayJournalEntry]
discardCurrentDisplayAttempt =
    dropWhile \case
        DisplayEvent (ResponseRestarted _) -> False
        _ -> True

removeCurrentToolUpdates :: Text -> [DisplayJournalEntry] -> [DisplayJournalEntry]
removeCurrentToolUpdates callId =
    filterCurrentAttempt \case
        DisplayEvent (ToolUpdated call) -> call.callId /= callId
        DisplayEvent (ToolArgumentsUpdated call) -> call.callId /= callId
        _ -> True

removeCurrentToolOutput :: Text -> [DisplayJournalEntry] -> [DisplayJournalEntry]
removeCurrentToolOutput callId =
    filterCurrentAttempt \case
        DisplayEvent (ToolOutputUpdated identifier _) -> identifier /= callId
        _ -> True

removeCurrentToolEvents :: Text -> [DisplayJournalEntry] -> [DisplayJournalEntry]
removeCurrentToolEvents callId =
    filterCurrentAttempt \case
        DisplayEvent (ToolStarted call) -> call.callId /= callId
        DisplayEvent (ToolUpdated call) -> call.callId /= callId
        DisplayEvent (ToolArgumentsUpdated call) -> call.callId /= callId
        DisplayEvent (ToolOutputUpdated identifier _) -> identifier /= callId
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

boundLoopToolOutput :: Text -> Text
boundLoopToolOutput output
    | Text.length output <= loopEventTailPayloadBudgetCodeUnits = Text.copy output
    | otherwise =
        toolOutputOmissionMarker
            <> Text.copy (Text.takeEnd loopEventTailPayloadCodeUnits output)

loopEventTailPayloadCodeUnits :: Int
loopEventTailPayloadCodeUnits =
    max 0
        (loopEventTailPayloadBudgetCodeUnits - Text.length toolOutputOmissionMarker)

toolOutputOmissionMarker :: Text
toolOutputOmissionMarker = "[earlier tool output truncated]\n"

loopEventTailPayloadBudgetCodeUnits :: Int
loopEventTailPayloadBudgetCodeUnits = (8 * 1024 * 1024 - 64) `div` 4
