-- | Focus-driven clipboard-image tip: while the terminal is focused and no
-- image is attached, hint that Ctrl+V pastes a raster already on the
-- pasteboard. The trigger is opportunistic polling on existing event-loop
-- iterations, a cheap change-count gate, and a fire cooldown. Nothing
-- schedules a wakeup of its own.
module Agent.CLI.TUI.ClipboardTip
    ( ClipboardFocusTipState(..)
    , emptyClipboardFocusTipState
    , ClipboardImageProbe(..)
    , inactiveClipboardImageProbe
    , liveClipboardImageProbe
    , clipboardImageTipPollIntervalNanos
    , clipboardImageTipFireCooldownNanos
    , clipboardImageTipDurationMillis
    , clipboardImageTipPrefix
    , clipboardImageTipChord
    , clipboardImageTipSuffix
    , clipboardImageTipLabel
    , clipboardImageTipDueToPoll
    , clipboardImageTipInCooldown
    , pollClipboardFocusTip
    , shouldFireClipboardImageTip
    , noteFiredClipboardImageTip
    , advanceClipboardImageTip
    , clipboardImageTipEligible
    ) where

import Agent.CLI.Clipboard
    ( ClipboardImageSnapshot(..)
    , clipboardChangeCount
    , clipboardImageSnapshot
    )
import Data.Text (Text)
import Data.Word (Word64)

data ClipboardFocusTipState = ClipboardFocusTipState
    { clipboardTipLastPollAt :: !(Maybe Word64)
    , clipboardTipLastSeenChangeCount :: !(Maybe Word64)
    , clipboardTipLastFiredAt :: !(Maybe Word64)
    , clipboardTipLastFiredChangeCount :: !(Maybe Word64)
    }
    deriving (Eq, Show)

emptyClipboardFocusTipState :: ClipboardFocusTipState
emptyClipboardFocusTipState =
    ClipboardFocusTipState
        { clipboardTipLastPollAt = Nothing
        , clipboardTipLastSeenChangeCount = Nothing
        , clipboardTipLastFiredAt = Nothing
        , clipboardTipLastFiredChangeCount = Nothing
        }

data ClipboardImageProbe = ClipboardImageProbe
    { clipboardProbeChangeCount :: IO (Maybe Word64)
    , clipboardProbeSnapshot :: IO ClipboardImageSnapshot
    }

inactiveClipboardImageProbe :: ClipboardImageProbe
inactiveClipboardImageProbe =
    ClipboardImageProbe
        { clipboardProbeChangeCount = pure Nothing
        , clipboardProbeSnapshot =
            pure ClipboardImageSnapshot
                { snapshotChangeCount = Nothing
                , snapshotHasPasteableImage = False
                }
        }

liveClipboardImageProbe :: ClipboardImageProbe
liveClipboardImageProbe =
    ClipboardImageProbe
        { clipboardProbeChangeCount = clipboardChangeCount
        , clipboardProbeSnapshot = clipboardImageSnapshot
        }

clipboardImageTipPollIntervalNanos :: Word64
clipboardImageTipPollIntervalNanos = 1_000_000_000

clipboardImageTipFireCooldownNanos :: Word64
clipboardImageTipFireCooldownNanos = 30_000_000_000

clipboardImageTipDurationMillis :: Int
clipboardImageTipDurationMillis = 3000

clipboardImageTipPrefix :: Text
clipboardImageTipPrefix = "Image in clipboard · "

clipboardImageTipChord :: Text
clipboardImageTipChord = "Ctrl+V"

clipboardImageTipSuffix :: Text
clipboardImageTipSuffix = " to paste"

clipboardImageTipLabel :: Text
clipboardImageTipLabel =
    clipboardImageTipPrefix <> clipboardImageTipChord <> clipboardImageTipSuffix

clipboardImageTipDueToPoll :: ClipboardFocusTipState -> Word64 -> Bool
clipboardImageTipDueToPoll state now =
    case state.clipboardTipLastPollAt of
        Nothing -> True
        Just lastPoll ->
            now - lastPoll >= clipboardImageTipPollIntervalNanos

clipboardImageTipInCooldown :: ClipboardFocusTipState -> Word64 -> Bool
clipboardImageTipInCooldown state now =
    case state.clipboardTipLastFiredAt of
        Nothing -> False
        Just lastFired ->
            now - lastFired < clipboardImageTipFireCooldownNanos

clipboardImageTipIsNewChangeCount
    :: ClipboardFocusTipState
    -> Maybe Word64
    -> Bool
clipboardImageTipIsNewChangeCount state changeCount =
    case changeCount of
        Nothing -> False
        Just _ -> changeCount /= state.clipboardTipLastSeenChangeCount

-- | One throttled poll. Classifies only when the cheap generation changed.
-- A fireable image does not advance 'clipboardTipLastSeenChangeCount' until
-- 'noteFiredClipboardImageTip', so a refused show stays retryable.
pollClipboardFocusTip
    :: Word64
    -> ClipboardImageProbe
    -> ClipboardFocusTipState
    -> IO (Maybe ClipboardImageSnapshot, ClipboardFocusTipState)
pollClipboardFocusTip now probe state
    | not (clipboardImageTipDueToPoll state now) =
        pure (Nothing, state)
    | otherwise = do
        let polled = state { clipboardTipLastPollAt = Just now }
        changeCount <- probe.clipboardProbeChangeCount
        if not (clipboardImageTipIsNewChangeCount polled changeCount)
            then pure (Nothing, polled)
            else do
                snapshot <- probe.clipboardProbeSnapshot
                let next
                        | snapshot.snapshotHasPasteableImage = polled
                        | otherwise =
                            polled
                                { clipboardTipLastSeenChangeCount = changeCount }
                pure (Just snapshot, next)

shouldFireClipboardImageTip
    :: ClipboardFocusTipState
    -> ClipboardImageSnapshot
    -> Word64
    -> Bool
shouldFireClipboardImageTip state snapshot now =
    snapshot.snapshotHasPasteableImage
        && not (clipboardImageTipInCooldown state now)
        && ( snapshot.snapshotChangeCount == Nothing
            || snapshot.snapshotChangeCount
                /= state.clipboardTipLastFiredChangeCount
           )

noteFiredClipboardImageTip
    :: ClipboardFocusTipState
    -> ClipboardImageSnapshot
    -> Word64
    -> ClipboardFocusTipState
noteFiredClipboardImageTip state snapshot now =
    state
        { clipboardTipLastFiredAt = Just now
        , clipboardTipLastFiredChangeCount =
            case snapshot.snapshotChangeCount of
                Just count -> Just count
                Nothing -> state.clipboardTipLastFiredChangeCount
        , clipboardTipLastSeenChangeCount =
            case snapshot.snapshotChangeCount of
                Just count -> Just count
                Nothing -> state.clipboardTipLastSeenChangeCount
        }

advanceClipboardImageTip :: Int -> Maybe Int -> Maybe Int
advanceClipboardImageTip rawElapsedMillis remaining =
    case remaining of
        Nothing -> Nothing
        Just current ->
            let next = current - max 0 rawElapsedMillis
            in if next <= 0 then Nothing else Just next

-- | Agent-level gate: the composer is visible, no image is already attached,
-- and the terminal is not known to be unfocused.
clipboardImageTipEligible
    :: Bool
    -> Int
    -> Bool
    -> Bool
    -> Bool
clipboardImageTipEligible pendingAction attachmentCount hasPreviews unfocused =
    not pendingAction
        && attachmentCount == 0
        && not hasPreviews
        && not unfocused
