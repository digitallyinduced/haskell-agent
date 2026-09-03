module Agent.TUI.AccentSpec (spec) where

import Agent.TUI.Accent (accentRail, highlightImageRow, highlightWidgetRow)
import Agent.TUI.Motion (MotionGlyphSet(..), accentBarGlyph)
import qualified Agent.TUI.Theme as Theme
import Brick
    ( Widget
    , padRight
    , renderWidget
    , str
    , vBox
    , viewport
    , Padding(Max)
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

    it "tints only the hovered image row" do
        let hover = V.defAttr `V.withBackColor` V.brightBlack
            img =
                V.vertCat
                    [ V.string V.defAttr "alpha"
                    , V.string V.defAttr "bravo"
                    , V.string V.defAttr "charlie"
                    ]
            tinted0 = highlightImageRow hover 0 img
            tinted1 = highlightImageRow hover 1 img
        V.imageHeight tinted1 `shouldBe` 3
        highlightImageRow hover 5 img `shouldBe` img
        tinted0 `shouldNotBe` img
        tinted1 `shouldNotBe` img
        tinted0 `shouldNotBe` tinted1
        show tinted1 `shouldSatisfy` isInfixOf "ISOColor 8"

    it "remaps colliding muted foregrounds on the hover band" do
        let hover = V.defAttr `V.withBackColor` V.brightBlack
            muted = V.defAttr `V.withForeColor` V.brightBlack
            tinted = highlightImageRow hover 0 (V.string muted "dim")
        -- V.white is ISO 7; V.brightBlack is ISO 8; style 16 is dim.
        show tinted `shouldSatisfy` isInfixOf "ISOColor 7"
        show tinted `shouldSatisfy` isInfixOf "ISOColor 8"
        show tinted `shouldSatisfy` isInfixOf "SetTo 16"

    it "paints a widget hover band on a single padded row" do
        let hovered row =
                highlightWidgetRow Theme.transcriptHoverAttr row $
                    padRight Max $
                        vBox [str "one", str "two", str "three"]
            renderHover row =
                V.picImage $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [hovered row :: Widget ()]
                        (8, 3)
            plain =
                V.picImage $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [ padRight Max (vBox [str "one", str "two", str "three"])
                            :: Widget ()
                        ]
                        (8, 3)
            image0 = renderHover 0
            image1 = renderHover 1
        V.imageHeight image1 `shouldBe` 3
        image0 `shouldNotBe` plain
        image1 `shouldNotBe` plain
        image0 `shouldNotBe` image1
        show image1 `shouldSatisfy` isInfixOf "ISOColor 8"

sampleRail :: Maybe Int -> Int -> Widget ()
sampleRail waveElapsed rows =
    accentRail
        MotionUnicode
        Theme.toolAttr
        True
        Theme.waveTrough
        Theme.Auto
        waveElapsed $
        vBox (replicate rows (str "body"))

containsAccentBar :: String -> Bool
containsAccentBar haystack =
    let glyph = Text.unpack (accentBarGlyph MotionUnicode)
    in glyph `isInfixOf` haystack
        || show glyph `isInfixOf` haystack
