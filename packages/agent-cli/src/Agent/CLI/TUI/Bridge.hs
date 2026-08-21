-- | Pure fullscreen bridge decisions used by the Brick runtime.
module Agent.CLI.TUI.Bridge
    ( eventFollows
    , historyMove
    , isSendNowKey
    , mergeUiEvents
    , nativeProgressSignal
    , normalizeAgentSelection
    ) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.TUI.Model (UiEvent(..), UiState(..))
import Agent.Loop (LoopEvent(..))
import Data.Text (Text)
import qualified Graphics.Vty as V

eventFollows :: UiEvent -> Bool
eventFollows = \case
    UiLoop _ -> True
    UiUserSubmitted _ -> True
    UiAssistantHistory _ -> True
    UiSystemMessage _ -> True
    UiErrorMessage _ -> True
    UiConversationCleared -> True
    _ -> False

-- | Merge adjacent high-frequency updates before they reach Brick. Structural
-- events deliberately return 'Nothing' so turn/tool ordering remains exact.
mergeUiEvents :: UiEvent -> UiEvent -> Maybe UiEvent
mergeUiEvents older newer = case (older, newer) of
    (UiLoop (TextDelta left), UiLoop (TextDelta right)) ->
        Just (UiLoop (TextDelta (left <> right)))
    (UiLoop (ReasoningDelta left), UiLoop (ReasoningDelta right)) ->
        Just (UiLoop (ReasoningDelta (left <> right)))
    (UiLoop (ActivityUpdated _), UiLoop (ActivityUpdated latest)) ->
        Just (UiLoop (ActivityUpdated latest))
    (UiTick, UiTick) ->
        Just UiTick
    _ ->
        Nothing

nativeProgressSignal :: UiEvent -> UiState -> Maybe Bool
nativeProgressSignal event state = case event of
    UiLoop TurnStarted -> Just True
    UiLoop (TurnFinished _) -> Just state.uiRunning
    UiTurnEnded _ -> Just False
    UiSetAwaitingInput True -> Just False
    UiTick
        | state.uiRunning
        , state.uiFrame == 0 ->
            Just True
    _ -> Nothing

historyMove
    :: Int
    -> [Text]
    -> Maybe Int
    -> Text
    -> Text
    -> (Text, Maybe Int, Text)
historyMove delta entries currentIndex currentText savedDraft
    | null entries = (currentText, currentIndex, savedDraft)
    | next < 0 = (savedDraft, Nothing, savedDraft)
    | next >= length entries = (currentText, currentIndex, savedDraft)
    | otherwise =
        let
            text = entries !! next
            draft = case currentIndex of
                Nothing -> currentText
                Just _ -> savedDraft
        in (text, Just next, draft)
  where
    current = maybe (-1) id currentIndex
    next = current + delta

-- | Interruptive submission while a turn is active. Modified Enter is the
-- primary chord; Ctrl-O is a terminal-safe fallback for environments that
-- cannot distinguish Ctrl-Enter from Enter.
isSendNowKey :: V.Event -> Bool
isSendNowKey = \case
    V.EvKey V.KEnter [V.MCtrl] -> True
    V.EvKey (V.KChar 'o') [V.MCtrl] -> True
    _ -> False

normalizeAgentSelection
    :: AgentTarget
    -> [AgentEntry]
    -> AgentTarget
normalizeAgentSelection selected entries
    | any ((== selected) . (.agentTarget)) entries = selected
    | otherwise = AgentRoot
