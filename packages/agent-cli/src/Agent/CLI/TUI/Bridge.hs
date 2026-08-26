-- | Pure fullscreen bridge decisions used by the Brick runtime.
module Agent.CLI.TUI.Bridge
    ( eventFollows
    , fullscreenHistoryLimit
    , historyMove
    , isSendNowKey
    , mergeUiEvents
    , nativeProgressSignal
    , normalizeAgentSelection
    , pushHistory
    , reconcileAgentSelection
    , trimHistory
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    , lookupAgentEntry
    )
import Agent.TUI.Model (UiEvent(..), UiState(..))
import Agent.Loop (LoopEvent(..))
import Data.Text (Text)
import qualified Graphics.Vty as V

-- | Keep enough prompt recall for normal interactive use without retaining an
-- unbounded copy of the persistent Haskeline history in the fullscreen state.
fullscreenHistoryLimit :: Int
fullscreenHistoryLimit = 100

trimHistory :: [Text] -> [Text]
trimHistory entries =
    maybe entries id (overflowPrefix fullscreenHistoryLimit entries)
  where
    -- Preserve the original list when it already fits. On overflow, force the
    -- bounded spine now: a lazy 'take' would leave the retained prefix pointing
    -- into the complete on-disk history until the user walked far enough.
    overflowPrefix 0 [] = Nothing
    overflowPrefix 0 _ = Just []
    overflowPrefix _ [] = Nothing
    overflowPrefix remaining (entry : rest) =
        case overflowPrefix (remaining - 1) rest of
            Nothing -> Nothing
            Just boundedRest ->
                boundedRest `seq` Just (entry : boundedRest)

pushHistory :: Text -> [Text] -> [Text]
pushHistory text = trimHistory . (text :)

eventFollows :: UiEvent -> Bool
eventFollows = \case
    UiLoop _ -> True
    UiUserSubmitted _ -> True
    UiAssistantHistory _ -> True
    UiSystemMessage _ -> True
    UiRecapStarted -> True
    UiRecapReady _ -> True
    UiRecapUnavailable _ -> True
    UiErrorMessage _ -> True
    UiRetryCountdown{} -> True
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
    _ ->
        Nothing

nativeProgressSignal :: Bool -> UiEvent -> UiState -> Maybe Bool
nativeProgressSignal blocked event state
    | blocked = Nothing
    | otherwise = case event of
        UiLoop TurnStarted -> Just True
        UiLoop (TurnFinished _) -> Just state.uiRunning
        UiTurnEnded _ -> Just False
        UiSetAwaitingInput True -> Just False
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
-- | Modifier lists are matched with 'elem' because enhanced keyboard
-- protocols may report additional modifier bits alongside Ctrl. Shift+Enter
-- keeps its newline meaning even when Ctrl is also reported.
isSendNowKey :: V.Event -> Bool
isSendNowKey = \case
    V.EvKey V.KEnter modifiers ->
        V.MCtrl `elem` modifiers && V.MShift `notElem` modifiers
    V.EvKey (V.KChar 'o') modifiers -> V.MCtrl `elem` modifiers
    _ -> False

normalizeAgentSelection
    :: AgentTarget
    -> [AgentEntry]
    -> AgentTarget
normalizeAgentSelection selected entries
    | selected == AgentRoot = selected
    | otherwise =
        maybe AgentRoot (const selected) (lookupAgentEntry selected entries)

-- | Normalize the current shared selection against an available target set.
reconcileAgentSelection
    :: [AgentTarget]
    -> AgentTarget
    -> AgentTarget
reconcileAgentSelection available current
    | current `elem` available = current
    | otherwise = AgentRoot
