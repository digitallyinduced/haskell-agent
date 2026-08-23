-- | Shared, renderer-independent motion vocabulary for terminal UIs.
--
-- Animation phases are derived from elapsed milliseconds rather than a
-- renderer-specific tick count. This keeps visual cadence and semantic
-- lifetimes stable when the scheduler changes frequency.
module Agent.TUI.Motion
    ( MotionDemand(..)
    , MotionGlyphSet(..)
    , MotionMode(..)
    , backgroundIndicator
    , backgroundPulseFrames
    , completionFlashDurationMillis
    , completionStatusDurationMillis
    , foregroundIndicator
    , foregroundSpinnerFrames
    , motionFrameAt
    , motionDelayMicros
    , motionIntervalMicros
    , nativeProgressAnimationEnabled
    , quietIndicator
    , quietSpinnerFrames
    , transientNoticeDurationMillis
    , waitingIndicator
    , waitingPulseFrames
    ) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

-- | User-selected animation policy.
data MotionMode
    = MotionFull
    | MotionReduced
    | MotionOff
    deriving (Eq, Ord, Show)

-- | The fastest visible or semantic clock currently required by the UI.
data MotionDemand
    = MotionNone
    | MotionSlow
    | MotionFast
    deriving (Eq, Ord, Show)

data MotionGlyphSet
    = MotionUnicode
    | MotionAscii
    deriving (Eq, Ord, Show)

foregroundSpinnerFrames :: MotionGlyphSet -> [Text]
foregroundSpinnerFrames =
    NonEmpty.toList . foregroundSpinnerFamily

foregroundSpinnerFamily :: MotionGlyphSet -> NonEmpty Text
foregroundSpinnerFamily = \case
    MotionUnicode ->
        "⠋" :| ["⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧"]
    MotionAscii ->
        "|" :| ["/", "-", "\\"]

quietSpinnerFrames :: MotionGlyphSet -> [Text]
quietSpinnerFrames =
    NonEmpty.toList . quietSpinnerFamily

quietSpinnerFamily :: MotionGlyphSet -> NonEmpty Text
quietSpinnerFamily = \case
    MotionUnicode ->
        "⋅" :| [":", "⸬", "⁙"]
    MotionAscii ->
        "." :| [":", "*", ":"]

backgroundPulseFrames :: MotionGlyphSet -> [Text]
backgroundPulseFrames =
    NonEmpty.toList . backgroundPulseFamily

backgroundPulseFamily :: MotionGlyphSet -> NonEmpty Text
backgroundPulseFamily = \case
    MotionUnicode ->
        "○" :| ["◎", "◉", "◎"]
    MotionAscii ->
        "o" :| ["O", "@", "O"]

-- | The intermediate shapes make the pulse legible in monochrome terminals;
-- colour renderers can additionally vary brightness.
waitingPulseFrames :: MotionGlyphSet -> [Text]
waitingPulseFrames =
    NonEmpty.toList . waitingPulseFamily

waitingPulseFamily :: MotionGlyphSet -> NonEmpty Text
waitingPulseFamily = \case
    MotionUnicode ->
        "◇" :| ["◈", "◆", "◈"]
    MotionAscii ->
        "." :| ["*", "#", "*"]

foregroundIndicator
    :: MotionGlyphSet
    -> MotionMode
    -> Int
    -> Text
foregroundIndicator =
    motionIndicator
        foregroundFrameDurationMillis
        foregroundSpinnerFamily
        staticForeground

quietIndicator
    :: MotionGlyphSet
    -> MotionMode
    -> Int
    -> Text
quietIndicator =
    motionIndicator
        quietFrameDurationMillis
        quietSpinnerFamily
        staticQuiet

backgroundIndicator
    :: MotionGlyphSet
    -> MotionMode
    -> Int
    -> Text
backgroundIndicator =
    motionIndicator
        backgroundFrameDurationMillis
        backgroundPulseFamily
        staticBackground

waitingIndicator
    :: MotionGlyphSet
    -> MotionMode
    -> Int
    -> Text
waitingIndicator =
    motionIndicator
        waitingFrameDurationMillis
        waitingPulseFamily
        staticWaiting

motionIndicator
    :: Int
    -> (MotionGlyphSet -> NonEmpty Text)
    -> (MotionGlyphSet -> Text)
    -> MotionGlyphSet
    -> MotionMode
    -> Int
    -> Text
motionIndicator frameDurationMillis frameFamily staticFrame glyphs mode elapsedMillis =
    case mode of
        MotionFull ->
            motionFrameAt
                frameDurationMillis
                elapsedMillis
                (frameFamily glyphs)
        _ ->
            staticFrame glyphs

motionFrameAt :: Int -> Int -> NonEmpty a -> a
motionFrameAt frameDurationMillis elapsedMillis frames =
    NonEmpty.toList frames
        !! ((max 0 elapsedMillis `div` max 1 frameDurationMillis)
            `mod` NonEmpty.length frames)

-- | Scheduler cadence. Full motion deliberately stays below 30 FPS until the
-- Brick/Vty cursor-blink path has been audited at that rate.
motionIntervalMicros :: MotionMode -> MotionDemand -> Int
motionIntervalMicros mode demand = case (mode, demand) of
    (_, MotionNone) -> 1000000
    (MotionFull, MotionSlow) -> 160000
    (MotionFull, MotionFast) -> 80000
    (MotionReduced, _) -> 500000
    (MotionOff, _) -> 1000000

-- | Wake no later than the next semantic deadline, even when accessibility
-- policy caps cosmetic animation at a slower cadence.
motionDelayMicros
    :: MotionMode
    -> MotionDemand
    -> Maybe Int
    -- ^ Milliseconds until the earliest semantic deadline.
    -> Int
motionDelayMicros mode demand deadlineMillis =
    case deadlineMillis of
        Nothing ->
            cadence
        Just remaining ->
            min cadence (max 1 remaining * 1000)
  where
    cadence = motionIntervalMicros mode demand

nativeProgressAnimationEnabled :: MotionMode -> Bool
nativeProgressAnimationEnabled = (== MotionFull)

completionFlashDurationMillis :: Int
completionFlashDurationMillis = 400

completionStatusDurationMillis :: Int
completionStatusDurationMillis = 1000

transientNoticeDurationMillis :: Int
transientNoticeDurationMillis = 3000

foregroundFrameDurationMillis :: Int
foregroundFrameDurationMillis = 160

quietFrameDurationMillis :: Int
quietFrameDurationMillis = 160

backgroundFrameDurationMillis :: Int
backgroundFrameDurationMillis = 320

waitingFrameDurationMillis :: Int
waitingFrameDurationMillis = 320

staticForeground :: MotionGlyphSet -> Text
staticForeground = \case
    MotionUnicode -> "●"
    MotionAscii -> "*"

staticQuiet :: MotionGlyphSet -> Text
staticQuiet = \case
    MotionUnicode -> "·"
    MotionAscii -> "."

staticBackground :: MotionGlyphSet -> Text
staticBackground = \case
    MotionUnicode -> "○"
    MotionAscii -> "o"

staticWaiting :: MotionGlyphSet -> Text
staticWaiting = \case
    MotionUnicode -> "◆"
    MotionAscii -> "!"
