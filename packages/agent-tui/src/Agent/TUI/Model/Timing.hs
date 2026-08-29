-- | Retained UI clocks, deadlines, and generation-rate queries.
module Agent.TUI.Model.Timing
    ( advanceUiTime
    , uiNextDeadlineMillis
    , uiNeedsTick
    , uiTokensPerSecond
    , uiTokensPerSecondEstimated
    , retryCountdownText
    ) where

import Agent.Loop
    ( liveTokensPerSecond )
import Agent.TUI.Model.Types
import Agent.TUI.Motion (transientNoticeDurationMillis)
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

advanceUiTime :: Int -> UiState -> UiState
advanceUiTime rawElapsedMillis state =
    let
        elapsedMillis = max 0 rawElapsedMillis
        completionRemainingMillis =
            max 0 (state.uiCompletionRemainingMillis - elapsedMillis)
        noticeElapsedMillis =
            case state.uiNotice of
                Just notice
                    | notice.noticeTransient ->
                        state.uiNoticeElapsedMillis + elapsedMillis
                _ -> 0
        notice
            | noticeElapsedMillis >= transientNoticeDurationMillis
            , maybe False (.noticeTransient) state.uiNotice = Nothing
            | otherwise = state.uiNotice
        countdown = advanceRetryCountdown elapsedMillis state
    in countdown
        { uiElapsedMillis =
            if state.uiRunning
                then state.uiElapsedMillis + elapsedMillis
                else state.uiElapsedMillis
        , uiGenerationMillis =
            if state.uiGenerating
                then state.uiGenerationMillis + elapsedMillis
                else state.uiGenerationMillis
        , uiActivity =
            if state.uiCompletionRemainingMillis > 0
                && completionRemainingMillis == 0
                then "Ready"
                else state.uiActivity
        , uiCompletionRemainingMillis = completionRemainingMillis
        , uiNotice = notice
        , uiNoticeElapsedMillis =
            if notice == Nothing then 0 else noticeElapsedMillis
        }

-- | Live generation speed while the model is streaming; otherwise the last
-- completed model response.
uiTokensPerSecond :: UiState -> Maybe Double
uiTokensPerSecond state
    | state.uiGenerating =
        liveTokensPerSecond state.uiGenerationChars state.uiGenerationMillis
            <|> state.uiLastTokensPerSecond
    | otherwise = state.uiLastTokensPerSecond

-- | Whether the displayed rate is the live streamed-text estimate rather than
-- a retained provider-reported rate.
uiTokensPerSecondEstimated :: UiState -> Bool
uiTokensPerSecondEstimated state =
    state.uiGenerating
        && liveTokensPerSecond
            state.uiGenerationChars
            state.uiGenerationMillis
            /= Nothing

uiNeedsTick :: UiState -> Bool
uiNeedsTick state =
    state.uiRunning
        || state.uiCompletionRemainingMillis > 0
        || maybe False ((> 0) . (.retryCountdownRemainingMillis))
            state.uiRetryCountdown
        || maybe False (.noticeTransient) state.uiNotice

uiNextDeadlineMillis :: UiState -> Maybe Int
uiNextDeadlineMillis state =
    minimumMaybe $ completionDeadline <> countdownDeadline <> noticeDeadline
  where
    completionDeadline =
        [state.uiCompletionRemainingMillis
        | state.uiCompletionRemainingMillis > 0]
    countdownDeadline =
        case state.uiRetryCountdown of
            Just countdown ->
                [ min countdown.retryCountdownRemainingMillis
                    (millisecondsUntilNextDisplayedSecond
                        countdown.retryCountdownRemainingMillis)
                ]
            Nothing -> []
    noticeDeadline =
        case state.uiNotice of
            Just notice
                | notice.noticeTransient ->
                    [max 0
                        (transientNoticeDurationMillis
                            - state.uiNoticeElapsedMillis)]
            _ -> []
    minimumMaybe [] = Nothing
    minimumMaybe values = Just (minimum values)

advanceRetryCountdown :: Int -> UiState -> UiState
advanceRetryCountdown elapsedMillis state =
    case state.uiRetryCountdown of
        Nothing -> state
        Just countdown ->
            let
                remaining =
                    max 0
                        (countdown.retryCountdownRemainingMillis - elapsedMillis)
                body =
                    retryCountdownText countdown.retryCountdownPrefix remaining
                        countdown.retryCountdownSuffix
                blocks =
                    case Map.lookup countdown.retryCountdownBlockId
                        state.uiBlockIndices of
                        Nothing -> state.uiBlocks
                        Just index ->
                            Seq.adjust (\block -> block { blockBody = body })
                                index state.uiBlocks
            in state
                { uiBlocks = blocks
                , uiRetryCountdown =
                    if remaining == 0
                        then Nothing
                        else Just countdown
                            { retryCountdownRemainingMillis = remaining }
                }

retryCountdownText :: Text -> Int -> Text -> Text
retryCountdownText prefix remainingMillis suffix =
    prefix
        <> (if remainingMillis <= 0
                then "Try again now"
                else "Try again in "
                    <> formatCountdownSeconds
                        ((remainingMillis + 999) `div` 1000))
        <> suffix

formatCountdownSeconds :: Int -> Text
formatCountdownSeconds rawSeconds =
    let
        total = max 0 rawSeconds
        hours = total `div` 3600
        minutes = (total `mod` 3600) `div` 60
        seconds = total `mod` 60
        showText = Text.pack . show
        pad2 value
            | value < 10 = "0" <> showText value
            | otherwise = showText value
    in if hours > 0
        then showText hours <> "h" <> pad2 minutes <> "m" <> pad2 seconds <> "s"
        else if minutes > 0
            then showText minutes <> "m" <> pad2 seconds <> "s"
            else showText seconds <> "s"

millisecondsUntilNextDisplayedSecond :: Int -> Int
millisecondsUntilNextDisplayedSecond remainingMillis =
    let remainder = remainingMillis `mod` 1000
    in if remainder == 0 then 1000 else remainder
