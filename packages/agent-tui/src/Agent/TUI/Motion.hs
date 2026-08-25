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
    , accentBarGlyph
    , pulseBrightness
    , quietIndicator
    , quietSpinnerFrames
    , shineOpacity
    , transientNoticeDurationMillis
    , waitingIndicator
    , waitingPulseFrames
    , waveBrightness
    , waveRows
    , waveRowsFor
    ) where

import Data.Fixed (mod')
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
waitingIndicator glyphs _mode _elapsed =
    -- Keep a static diamond; color renderers breathe its brightness instead
    -- of cycling glyphs.
    staticWaiting glyphs

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

-- | Scheduler cadence. Fast is ~30 FPS for live rails; slow is ~12 FPS
-- for the empty-state sheen.
motionIntervalMicros :: MotionMode -> MotionDemand -> Int
motionIntervalMicros mode demand = case (mode, demand) of
    (_, MotionNone) -> 1000000
    (MotionFull, MotionSlow) -> 80000
    (MotionFull, MotionFast) -> 33000
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

-- | Full-height scrollback accent rail. The heavy vertical bar lets a
-- traveling wave paint every row without shifting layout.
accentBarGlyph :: MotionGlyphSet -> Text
accentBarGlyph = \case
    MotionUnicode -> "┃"
    MotionAscii -> "|"

-- | Rows per wave cycle on a live accent rail. Lower is a tighter/faster
-- spatial period.
waveRows :: Int
waveRows = 32

-- | Tighten the spatial period on short rails so a 4–6 row thinking
-- block still shows a traveling highlight instead of a flat pulse.
waveRowsFor :: Int -> Int
waveRowsFor height =
    max 8 (min waveRows (max 1 height * 3))

-- | 0.15 rad/tick at 30fps.
waveRadiansPerSecond :: Double
waveRadiansPerSecond = 0.15 * 30

-- | 0.08 rad/tick at 30fps, about 1.3s per visible pulse.
pulseRadiansPerSecond :: Double
pulseRadiansPerSecond = 0.08 * 30

-- | Traveling-wave brightness in @[0, 1]@ for one rail row.
--
-- @sin²(t + phase)@ with a per-row phase so the bright band walks down
-- the rail independently of block height.
waveBrightness
    :: Int
    -- ^ Elapsed milliseconds.
    -> Int
    -- ^ Row within the block, 0 = top.
    -> Int
    -- ^ Rows per full wave cycle.
    -> Double
waveBrightness elapsedMillis row rowsPerCycle =
    let
        rows = fromIntegral (max 1 rowsPerCycle)
        phase = (fromIntegral row / rows) * 2 * pi
        t = seconds elapsedMillis * waveRadiansPerSecond
        sine = sin (t + phase)
    in sine * sine

-- | Temporal pulse in @[0, 1]@ shared by every "waiting on you" cue.
pulseBrightness :: Int -> Double
pulseBrightness elapsedMillis =
    let
        t = seconds elapsedMillis * pulseRadiansPerSecond
        sine = sin t
    in sine * sine

-- | Diagonal sheen opacity in @[0, 1]@ for empty-state logo art.
--
-- A raised-cosine band sweeps bottom-left → top-right, then parks
-- off-screen while a small global pulse keeps the glyph breathing.
shineOpacity
    :: Double
    -- ^ Normalized diagonal position, 0 = bottom-left, 1 = top-right.
    -> Double
    -- ^ Seconds since the animation started.
    -> Double
shineOpacity diag secs =
    let
        p = (secs `mod'` shineCycleSeconds) / shineCycleSeconds
        q = min 1.0 (p / shineSweepFrac)
        bandPos = negate shineBandHalf + q * (1.0 + 2.0 * shineBandHalf)
        pulse =
            shinePulseAmount
                * (0.5 - 0.5 * cos (2 * pi * secs / shinePulseSeconds))
        distance = abs (diag - bandPos)
        shine
            | distance < shineBandHalf =
                0.5 * (1.0 + cos (pi * distance / shineBandHalf))
            | otherwise = 0.0
    in max 0.0 (min 1.0 (pulse + shinePeak * shine))

shineBandHalf :: Double
shineBandHalf = 0.38

shineCycleSeconds :: Double
shineCycleSeconds = 4.0

shineSweepFrac :: Double
shineSweepFrac = 0.32

shinePeak :: Double
shinePeak = 0.33

shinePulseAmount :: Double
shinePulseAmount = 0.06

shinePulseSeconds :: Double
shinePulseSeconds = 5.0

seconds :: Int -> Double
seconds elapsedMillis =
    fromIntegral (max 0 elapsedMillis) / 1000
