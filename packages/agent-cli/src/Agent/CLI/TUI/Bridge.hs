-- | Pure fullscreen bridge decisions used by the Brick runtime.
module Agent.CLI.TUI.Bridge
    ( eventFollows
    , historyMove
    , nativeProgressSignal
    , normalizeAgentSelection
    ) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.UI.Model (UiEvent(..), UiState(..))
import Agent.Loop (LoopEvent(..))
import Data.Text (Text)

eventFollows :: UiEvent -> Bool
eventFollows = \case
    UiLoop _ -> True
    UiUserSubmitted _ -> True
    UiAssistantHistory _ -> True
    UiSystemMessage _ -> True
    UiErrorMessage _ -> True
    UiConversationCleared -> True
    _ -> False

nativeProgressSignal :: UiEvent -> UiState -> Maybe Bool
nativeProgressSignal event state = case event of
    UiLoop TurnStarted -> Just True
    UiLoop (TurnFinished _) -> Just False
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

normalizeAgentSelection
    :: AgentTarget
    -> [AgentEntry]
    -> AgentTarget
normalizeAgentSelection selected entries
    | any ((== selected) . (.agentTarget)) entries = selected
    | otherwise = AgentRoot
