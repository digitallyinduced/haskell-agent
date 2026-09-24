module Agent.CLI.TUIClipboardTipSpec (spec) where

import Agent.CLI.Clipboard (ClipboardImageSnapshot(..))
import Agent.CLI.TUI.App
    ( ClipboardImageProbe(..)
    , advanceClipboardImageTip
    , clipboardImageTipEligible
    , clipboardImageTipFireCooldownNanos
    , clipboardImageTipLabel
    , clipboardImageTipPollIntervalNanos
    , emptyClipboardFocusTipState
    , noteFiredClipboardImageTip
    , pollClipboardFocusTip
    , shouldFireClipboardImageTip
    )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Test.Hspec

spec :: Spec
spec = do
    describe "clipboard image tip copy" do
        it "names the Ctrl+V paste chord" do
            clipboardImageTipLabel
                `shouldBe` "Image in clipboard · Ctrl+V to paste"

    describe "clipboardImageTipEligible" do
        it "requires a visible idle composer without attachments" do
            clipboardImageTipEligible False 0 False False `shouldBe` True
            clipboardImageTipEligible True 0 False False `shouldBe` False
            clipboardImageTipEligible False 1 False False `shouldBe` False
            clipboardImageTipEligible False 0 True False `shouldBe` False
            clipboardImageTipEligible False 0 False True `shouldBe` False

    describe "pollClipboardFocusTip" do
        it "throttles cheap reads to one per interval" do
            cheapReads <- newIORef (0 :: Int)
            let probe = countingProbe cheapReads (Just 1) False
                t0 = 0
            state0 <- snd <$> pollClipboardFocusTip t0 probe emptyClipboardFocusTipState
            _ <- pollClipboardFocusTip
                (t0 + clipboardImageTipPollIntervalNanos `div` 2)
                probe
                state0
            readIORef cheapReads `shouldReturn` 1
            _ <- pollClipboardFocusTip
                (t0 + clipboardImageTipPollIntervalNanos)
                probe
                state0
            readIORef cheapReads `shouldReturn` 2

        it "skips classification when the generation is unchanged" do
            classifyCalls <- newIORef (0 :: Int)
            let probe = classifyingProbe classifyCalls (Just 5) False
                t0 = 0
            (first, state1) <-
                pollClipboardFocusTip t0 probe emptyClipboardFocusTipState
            first `shouldBe`
                Just (ClipboardImageSnapshot (Just 5) False)
            readIORef classifyCalls `shouldReturn` 1
            (again, _) <-
                pollClipboardFocusTip
                    (t0 + clipboardImageTipPollIntervalNanos)
                    probe
                    state1
            again `shouldBe` Nothing
            readIORef classifyCalls `shouldReturn` 1

        it "fires once for a new image and does not reclassify the same generation" do
            classifyCalls <- newIORef (0 :: Int)
            let probe = classifyingProbe classifyCalls (Just 3) True
                t0 = 0
            (got, state1) <-
                pollClipboardFocusTip t0 probe emptyClipboardFocusTipState
            snapshot <- maybe (fail "expected a snapshot") pure got
            snapshot.snapshotHasPasteableImage `shouldBe` True
            shouldFireClipboardImageTip state1 snapshot t0 `shouldBe` True
            let fired = noteFiredClipboardImageTip state1 snapshot t0
                later =
                    t0
                        + clipboardImageTipFireCooldownNanos
                        + clipboardImageTipPollIntervalNanos
            (again, _) <- pollClipboardFocusTip later probe fired
            again `shouldBe` Nothing
            readIORef classifyCalls `shouldReturn` 1

        it "retries a refused show until the tip actually lands" do
            classifyCalls <- newIORef (0 :: Int)
            let probe = classifyingProbe classifyCalls (Just 7) True
                t0 = 0
            (got, state1) <-
                pollClipboardFocusTip t0 probe emptyClipboardFocusTipState
            snapshot <- maybe (fail "expected a snapshot") pure got
            shouldFireClipboardImageTip state1 snapshot t0 `shouldBe` True
            (retry, state2) <-
                pollClipboardFocusTip
                    (t0 + clipboardImageTipPollIntervalNanos)
                    probe
                    state1
            retry `shouldBe` Just snapshot
            readIORef classifyCalls `shouldReturn` 2
            let fired = noteFiredClipboardImageTip state2 snapshot
                    (t0 + clipboardImageTipPollIntervalNanos)
                later =
                    t0
                        + clipboardImageTipFireCooldownNanos
                        + 2 * clipboardImageTipPollIntervalNanos
            (after, _) <- pollClipboardFocusTip later probe fired
            after `shouldBe` Nothing

        it "blocks a second fire until the cooldown elapses" do
            let t0 = 0 :: Word64
                first = ClipboardImageSnapshot (Just 1) True
                second = ClipboardImageSnapshot (Just 2) True
                fired =
                    noteFiredClipboardImageTip emptyClipboardFocusTipState first t0
            shouldFireClipboardImageTip
                fired
                second
                (t0 + 5_000_000_000)
                `shouldBe` False
            shouldFireClipboardImageTip
                fired
                second
                (t0 + clipboardImageTipFireCooldownNanos + 1)
                `shouldBe` True

        it "never fires for a non-image snapshot" do
            shouldFireClipboardImageTip
                emptyClipboardFocusTipState
                (ClipboardImageSnapshot (Just 3) False)
                0
                `shouldBe` False

        it "expires the visible tip after its duration" do
            advanceClipboardImageTip 1000 (Just 3000) `shouldBe` Just 2000
            advanceClipboardImageTip 3000 (Just 3000) `shouldBe` Nothing
            advanceClipboardImageTip 10 Nothing `shouldBe` Nothing

countingProbe
    :: IORef Int
    -> Maybe Word64
    -> Bool
    -> ClipboardImageProbe
countingProbe cheapReads changeCount hasImage =
    ClipboardImageProbe
        { clipboardProbeChangeCount = do
            n <- readIORef cheapReads
            writeIORef cheapReads (n + 1)
            pure changeCount
        , clipboardProbeSnapshot =
            pure (ClipboardImageSnapshot changeCount hasImage)
        }

classifyingProbe
    :: IORef Int
    -> Maybe Word64
    -> Bool
    -> ClipboardImageProbe
classifyingProbe classifyCalls changeCount hasImage =
    ClipboardImageProbe
        { clipboardProbeChangeCount = pure changeCount
        , clipboardProbeSnapshot = do
            n <- readIORef classifyCalls
            writeIORef classifyCalls (n + 1)
            pure (ClipboardImageSnapshot changeCount hasImage)
        }
