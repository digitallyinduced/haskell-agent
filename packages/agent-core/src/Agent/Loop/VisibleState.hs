-- | Pure bookkeeping for output not yet absorbed by a response commit.
-- This is display-only state; it must never enter backend/model history.
module Agent.Loop.VisibleState
    ( VisibleLoopState(..)
    , emptyVisibleLoopState
    , recordVisibleLoopEvent
    , visibleAssistantText
    , visibleDisplayEvents
    ) where

import Agent.Loop.DisplayJournal
import Agent.Loop.Output (LoopEvent(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- Attempts and their chunks are newest-first. Strict fields force each
-- transition without flattening text or the deliberately lazy journal tails.
data VisibleLoopState = VisibleLoopState
    { finishedTextAttempts :: ![[Text]]
    , currentTextChunks :: ![Text]
    , displayJournal :: !DisplayJournal
    , providerAttemptActive :: !Bool
    }

-- Also used at the response-commit checkpoint, before TurnFinished is emitted.
emptyVisibleLoopState :: VisibleLoopState
emptyVisibleLoopState = VisibleLoopState [] [] emptyDisplayJournal False

recordVisibleLoopEvent :: LoopEvent -> VisibleLoopState -> VisibleLoopState
recordVisibleLoopEvent event state = case event of
    TurnStarted -> state
        { providerAttemptActive = True
        , displayJournal = emptyDisplayJournal
        }
    TurnFinished _ -> state
        { providerAttemptActive = False
        , displayJournal = emptyDisplayJournal
        }
    TextDelta delta -> state
        { currentTextChunks = delta : state.currentTextChunks
        , displayJournal = journalWithEvent
        }
    -- A restarted attempt stays visible until a response commits or the turn
    -- ends. Discard removes only the current attempt, not earlier restarts.
    ResponseRestarted _ -> state
        { finishedTextAttempts =
            if null state.currentTextChunks
                then state.finishedTextAttempts
                else state.currentTextChunks : state.finishedTextAttempts
        , currentTextChunks = []
        , displayJournal = recordDisplayEvent event state.displayJournal
        }
    ResponseAttemptDiscarded -> state
        { currentTextChunks = []
        , displayJournal = discardCurrentDisplayAttempt state.displayJournal
        }
    _ -> state { displayJournal = journalWithEvent }
  where
    journalWithEvent
        | state.providerAttemptActive && replayableDisplayEvent event =
            recordDisplayEvent event state.displayJournal
        | otherwise = state.displayJournal

visibleAssistantText :: VisibleLoopState -> Maybe Text
visibleAssistantText state
    | Text.null text = Nothing
    | otherwise = Just text
  where
    text = Text.intercalate "\n\n" $ filter (not . Text.null) $
        map (Text.concat . reverse)
            (reverse state.finishedTextAttempts <> [state.currentTextChunks])

visibleDisplayEvents :: VisibleLoopState -> [LoopEvent]
visibleDisplayEvents state = displayEventsFromJournal state.displayJournal
