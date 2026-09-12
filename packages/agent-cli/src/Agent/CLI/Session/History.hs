-- | Terminal presentation of persisted conversation history.
module Agent.CLI.Session.History (hydrateUiHistory) where

import Agent.Runtime.Session (SessionTurn(..))
import Agent.OpenAI.Compaction (isCompactSessionTurn, isTranscriptResetTurn)
import Agent.TUI.Model (UiEvent(..), UiState, initialUiState, reduceUi)
import qualified Data.Text as Text

hydrateUiHistory :: [SessionTurn] -> UiState
hydrateUiHistory = foldl' addTurn initialUiState
  where
    addTurn state turn
        | isCompactSessionTurn turn.turnUserText =
            addCompactTurn state turn
        | isTranscriptResetTurn turn.turnUserText =
            addResetTurn state turn
        | otherwise =
            addRegularTurn state turn

    -- Compaction replaces the model's inference context, not the transcript
    -- presented to the user. Keep earlier blocks scrollable and append the
    -- compaction summary as the live UI does.
    addCompactTurn state turn =
        case turn.turnAssistantText of
            Nothing -> state
            Just text -> reduceUi (UiSystemMessage text) state

    addResetTurn state turn =
        let cleared = reduceUi UiConversationCleared state
        in case turn.turnAssistantText of
            Nothing -> cleared
            Just text -> reduceUi (UiHistory text) cleared

    addRegularTurn state turn =
        let withUser =
                if Text.null (Text.strip turn.turnUserText)
                    then state
                    else reduceUi
                        (UiUserSubmitted turn.turnUserText)
                        state
            withAssistant = case turn.turnAssistantText of
                Nothing -> withUser
                Just text ->
                    reduceUi (UiAssistantHistory text) withUser
        in case turn.turnError of
            Nothing -> withAssistant
            Just err -> reduceUi (UiErrorMessage err) withAssistant
