-- | Pure motion demand, scheduling, and completion-flash state.
module Agent.CLI.TUI.Motion
    ( advanceCompletionFlashes
    , appMotionDemand
    , appMotionTiming
    , completionFlashTransitions
    , elapsedMillisSince
    , hasBackgroundActivity
    , isBackgroundAgentActive
    , motionDemandFor
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , uiEventRestartsMotionSchedule
    , userActionPending
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    )
import Agent.CLI.TUI.Types
    ( AppState(..)
    , FullscreenRuntime(..)
    )
import Agent.Loop (LoopEvent(..))
import Agent.TUI.Model
    ( BlockId
    , BlockKind(..)
    , BlockState(..)
    , NoticeKind(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiNotice(..)
    , UiState(..)
    , conversationIsEmpty
    , uiNeedsTick
    , uiNextDeadlineMillis
    )
import Agent.TUI.Motion
    ( MotionDemand(..)
    , MotionMode(..)
    , motionDelayMicros
    )
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Word (Word64)

motionDemandFor
    :: MotionMode
    -> Bool
    -- ^ A permission, question, or approval is waiting on the user.
    -> Bool
    -- ^ Background child-agent work remains.
    -> Bool
    -- ^ At least one completed block is still flashing.
    -> UiState
    -> MotionDemand
motionDemandFor mode waitingForUser backgroundActive completionFlashing ui =
    case mode of
        MotionFull ->
            maximum
                [ semanticDemand
                , if not waitingForUser && ui.uiRunning
                    then MotionFast
                    else MotionNone
                , if waitingForUser
                    then MotionSlow
                    else MotionNone
                , if not waitingForUser && progressNoticeActive ui
                    then MotionFast
                    else MotionNone
                , if completionFlashing
                    then MotionFast
                    else MotionNone
                , if backgroundActive
                    || conversationIsEmpty ui
                    then MotionSlow
                    else MotionNone
                ]
        MotionReduced ->
            maximum
                [ semanticDemand
                , if completionFlashing
                    then MotionSlow
                    else MotionNone
                ]
        MotionOff ->
            semanticDemand
  where
    semanticDemand =
        if uiNeedsTick ui then MotionSlow else MotionNone

appMotionDemand :: AppState -> MotionDemand
appMotionDemand state =
    motionDemandFor
        state.appRuntime.runtimeMotionMode
        (userActionPending state)
        (hasBackgroundActivity state.appAgentEntries)
        (not (Map.null state.appCompletionFlashes))
        state.appUi

appMotionTiming :: AppState -> (MotionDemand, Int)
appMotionTiming state =
    ( demand
    , motionDelayMicros
        state.appRuntime.runtimeMotionMode
        demand
        (appNextDeadlineMillis state)
    )
  where
    demand = appMotionDemand state

appNextDeadlineMillis :: AppState -> Maybe Int
appNextDeadlineMillis state =
    minimumMaybe $
        maybeToList (uiNextDeadlineMillis state.appUi)
            <> Map.elems state.appCompletionFlashes
  where
    maybeToList = maybe [] pure
    minimumMaybe [] = Nothing
    minimumMaybe values = Just (minimum values)

userActionPending :: AppState -> Bool
userActionPending state =
    isJust state.appTextPrompt
        || isJust state.appChoice
        || isJust state.appResume
        || isJust state.appUi.uiPermission

progressNoticeActive :: UiState -> Bool
progressNoticeActive ui =
    maybe False ((== NoticeProgress) . (.noticeKind)) ui.uiNotice

hasBackgroundActivity :: [AgentEntry] -> Bool
hasBackgroundActivity =
    any isBackgroundAgentActive

isBackgroundAgentActive :: AgentEntry -> Bool
isBackgroundAgentActive entry =
    entry.agentTarget /= AgentRoot
        && Text.toLower entry.agentStatus `elem` ["pending", "running"]

completionFlashTransitions :: UiState -> UiState -> [BlockId]
completionFlashTransitions previous next =
    [ block.blockId
    | block <- toList next.uiBlocks
    , Just oldBlock <- [Map.lookup block.blockId previousById]
    , blockWasLive oldBlock.blockState
    , block.blockState == BlockComplete
    , block.blockKind
        `elem` [BlockThinking, BlockTool, BlockTodo, BlockShell, BlockEdit]
    ]
  where
    previousById =
        Map.fromList
            [ (block.blockId, block)
            | block <- toList previous.uiBlocks
            ]
    blockWasLive blockState =
        blockState `elem` [BlockRunning, BlockStreaming]

advanceCompletionFlashes
    :: Int
    -> Map.Map BlockId Int
    -> Map.Map BlockId Int
advanceCompletionFlashes rawElapsedMillis =
    Map.mapMaybe \remaining ->
        let next = remaining - max 0 rawElapsedMillis
        in if next <= 0 then Nothing else Just next

uiEventRestartsMotionSchedule
    :: UiEvent
    -> UiState
    -> UiState
    -> Map.Map BlockId Int
    -> Bool
uiEventRestartsMotionSchedule event previous next newFlashes =
    explicitReset
        || (previous.uiCompletionRemainingMillis == 0
            && next.uiCompletionRemainingMillis > 0)
        || not (Map.null newFlashes)
  where
    explicitReset = case event of
        UiLoop TurnStarted -> True
        UiLoop (WarningRaised _) -> True
        UiLoop (ResponseRestarted _) -> True
        UiSetNotice (Just _) -> True
        UiInputPromoted _ -> True
        UiTurnRestarted -> True
        _ -> False

nextMotionSchedule
    :: Bool
    -> MotionDemand
    -> Int
    -> (MotionDemand, Int, Int)
    -> (MotionDemand, Int, Int)
nextMotionSchedule
    resetSchedule
    demand
    delayMicros
    current@(currentDemand, currentDelay, generation)
    | resetSchedule
        || currentDemand /= demand
        || currentDelay /= delayMicros =
        (demand, delayMicros, generation + 1)
    | otherwise =
        current

elapsedMillisSince :: Word64 -> Word64 -> (Int, Word64)
elapsedMillisSince previous now
    | now <= previous =
        (0, previous)
    | otherwise =
        let elapsedMillis = (now - previous) `div` 1000000
        in
        ( fromIntegral elapsedMillis
        , previous + elapsedMillis * 1000000
        )

nativeProgressKeepaliveDue :: Bool -> Int -> UiState -> Bool
nativeProgressKeepaliveDue blocked previousBucket ui =
    not blocked
        && ui.uiRunning
        && ui.uiElapsedMillis `div` 5000 > previousBucket
