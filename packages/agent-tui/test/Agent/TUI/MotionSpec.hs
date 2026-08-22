module Agent.TUI.MotionSpec (spec) where

import Agent.TUI.Motion
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "terminal motion vocabulary" do
    it "keeps every animation frame exactly one terminal cell wide" do
        let families glyphs =
                [ foregroundSpinnerFrames glyphs
                , quietSpinnerFrames glyphs
                , backgroundPulseFrames glyphs
                , waitingPulseFrames glyphs
                ]
        mapM_
            (\frame -> do
                V.safeWcswidth (Text.unpack frame) `shouldBe` 1
                let image = V.text' V.defAttr frame
                V.imageWidth image `shouldBe` 1
                V.imageHeight image `shouldBe` 1)
            (concat (concatMap families [MotionUnicode, MotionAscii]))

    it "keeps reduced and off indicators static" do
        let samples indicator mode =
                [ indicator MotionUnicode mode elapsed
                | elapsed <- [0, 80, 320, 1000, 10000]
                ]
        samples foregroundIndicator MotionReduced
            `shouldSatisfy` allEqual
        samples foregroundIndicator MotionOff
            `shouldSatisfy` allEqual
        samples quietIndicator MotionReduced
            `shouldSatisfy` allEqual
        samples backgroundIndicator MotionOff
            `shouldSatisfy` allEqual
        samples waitingIndicator MotionReduced
            `shouldSatisfy` allEqual

    it "advances and wraps every full-motion frame family" do
        mapM_
            (\glyphs -> do
                cyclesThrough
                    foregroundIndicator
                    glyphs
                    160
                    (foregroundSpinnerFrames glyphs)
                cyclesThrough
                    quietIndicator
                    glyphs
                    160
                    (quietSpinnerFrames glyphs)
                cyclesThrough
                    backgroundIndicator
                    glyphs
                    320
                    (backgroundPulseFrames glyphs)
                cyclesThrough
                    waitingIndicator
                    glyphs
                    320
                    (waitingPulseFrames glyphs))
            [MotionUnicode, MotionAscii]

    it "uses fast cadence only when full motion is enabled" do
        motionIntervalMicros MotionFull MotionFast
            `shouldSatisfy`
                (< motionIntervalMicros MotionFull MotionSlow)
        motionIntervalMicros MotionReduced MotionFast
            `shouldBe` motionIntervalMicros MotionReduced MotionSlow
        motionIntervalMicros MotionOff MotionFast
            `shouldBe` motionIntervalMicros MotionOff MotionSlow

    it "wakes at semantic deadlines before a reduced-motion cadence" do
        motionDelayMicros MotionReduced MotionSlow Nothing
            `shouldBe` 500000
        motionDelayMicros MotionReduced MotionSlow (Just 400)
            `shouldBe` 400000
        motionDelayMicros MotionOff MotionSlow (Just 250)
            `shouldBe` 250000

    it "disables terminal-native animation outside full motion" do
        nativeProgressAnimationEnabled MotionFull `shouldBe` True
        nativeProgressAnimationEnabled MotionReduced `shouldBe` False
        nativeProgressAnimationEnabled MotionOff `shouldBe` False

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (first : rest) = all (== first) rest

cyclesThrough
    :: (MotionGlyphSet -> MotionMode -> Int -> Text.Text)
    -> MotionGlyphSet
    -> Int
    -> [Text.Text]
    -> Expectation
cyclesThrough indicator glyphs frameMillis frames =
    [ indicator glyphs MotionFull (index * frameMillis)
    | index <- [0 .. length frames]
    ]
        `shouldBe` (frames <> take 1 frames)
