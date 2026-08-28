-- | Session-command recognition and legacy checkpoint replay.
module Agent.OpenAI.Compaction.Commands
    ( compactTranscriptAtLastCheckpoint
    , isCompactSessionTurn
    , clearSessionUserText
    , newSessionUserText
    , isClearSessionTurn
    , isNewSessionTurn
    , isTranscriptResetTurn
    ) where

import Agent.Responses.Types (ResponseItem(..), ResponseItemType(..))
import Data.Text (Text)
import qualified Data.Text as Text

compactTranscriptAtLastCheckpoint :: [ResponseItem] -> [ResponseItem]
compactTranscriptAtLastCheckpoint items = go [] (reverse items)
  where
    go _ [] = items
    go after (item : before) =
        case item of
            CompactionItemValue{} -> item : after
            ContextCompactionItemValue{} -> item : after
            KnownResponseItem ItemCompaction _ -> item : after
            KnownResponseItem ItemContextCompaction _ -> item : after
            _ -> go (item : after) before

isCompactSessionTurn :: Text -> Bool
isCompactSessionTurn text =
    let stripped = Text.strip text
    in stripped == "/compact" || Text.isPrefixOf "/compact " stripped

clearSessionUserText :: Text
clearSessionUserText = "/clear"

newSessionUserText :: Text
newSessionUserText = "/new"

isClearSessionTurn :: Text -> Bool
isClearSessionTurn text = Text.strip text == clearSessionUserText

isNewSessionTurn :: Text -> Bool
isNewSessionTurn text = Text.strip text == newSessionUserText

isTranscriptResetTurn :: Text -> Bool
isTranscriptResetTurn text =
    isCompactSessionTurn text
        || isClearSessionTurn text
        || isNewSessionTurn text
