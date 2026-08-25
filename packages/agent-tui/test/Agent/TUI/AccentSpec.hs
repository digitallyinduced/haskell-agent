module Agent.TUI.AccentSpec (spec) where

import Agent.TUI.Accent (accentRail)
import Agent.TUI.Motion (MotionGlyphSet(..), accentBarGlyph)
import qualified Agent.TUI.Theme as Theme
import Brick
    ( Widget
    , renderWidget
    , str
    , vBox
    , viewport
    )
import Brick.Types (ViewportType(..))
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "accent rail" do
    it "paints a static one-column bar across the content height" do
        let widget :: Widget ()
            widget = sampleRail Nothing 4
            image =
                V.picImage $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [widget]
                        (8, 4)
        V.imageWidth image `shouldBe` 8
        V.imageHeight image `shouldBe` 4
        show image `shouldSatisfy` containsAccentBar

    it "walks brightness down the rail so adjacent wave rows differ" do
        let rendered elapsed =
                let widget :: Widget ()
                    widget = sampleRail (Just elapsed) 8
                in show $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [widget]
                        (8, 8)
            early = rendered 0
            later = rendered 400
        early `shouldNotBe` later
        early `shouldSatisfy` isInfixOf "RGBColor"
        later `shouldSatisfy` isInfixOf "RGBColor"

    it "fits inside a vertical viewport" do
        let widget :: Widget ()
            widget =
                viewport () Vertical $
                    sampleRail Nothing 3
            image =
                V.picImage $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [widget]
                        (20, 6)
        V.imageHeight image `shouldSatisfy` (> 0)

sampleRail :: Maybe Int -> Int -> Widget ()
sampleRail waveElapsed rows =
    accentRail MotionUnicode Theme.toolAttr True Theme.waveTrough waveElapsed $
        vBox (replicate rows (str "body"))

containsAccentBar :: String -> Bool
containsAccentBar haystack =
    let glyph = Text.unpack (accentBarGlyph MotionUnicode)
    in glyph `isInfixOf` haystack
        || show glyph `isInfixOf` haystack
