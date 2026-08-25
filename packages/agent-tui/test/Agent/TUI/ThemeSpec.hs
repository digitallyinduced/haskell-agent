module Agent.TUI.ThemeSpec (spec) where

import Agent.Syntax (SyntaxClass(..))
import Agent.TUI.Motion (MotionMode(..))
import qualified Agent.TUI.Theme as Theme
import Brick (AttrName)
import Brick.AttrMap (attrMapLookup)
import Data.List (nub)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (Color(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "structural theme attributes" do
        it "uses the terminal ANSI palette without fixing a background" do
            map terminalForeground
                [ Theme.borderActiveAttr
                , Theme.controlLinkAttr
                , Theme.controlLinkHoverAttr
                , Theme.controlLinkActiveAttr
                , Theme.lambdaDimAttr
                , Theme.lambdaTrailAttr
                , Theme.lambdaGlowAttr
                , Theme.lambdaSparkAttr
                , Theme.todoPendingAttr
                ]
                `shouldBe`
                    [ V.Default
                    , V.SetTo V.brightBlack
                    , V.Default
                    , V.Default
                    , V.SetTo V.brightBlack
                    , V.SetTo V.brightBlack
                    , V.Default
                    , V.SetTo V.brightWhite
                    , V.Default
                    ]

    describe "syntax theme attributes" do
        it "leaves every syntax background to the terminal theme" do
            map
                ( V.attrBackColor
                    . (`attrMapLookup` Theme.terminalDefault)
                    . Theme.syntaxClassAttr
                )
                allSyntaxClasses
                `shouldBe` replicate (length allSyntaxClasses) V.Default

        it "uses reverse video for selections on light and dark themes" do
            V.attrStyle
                (attrMapLookup Theme.selectedAttr Theme.terminalDefault)
                `shouldBe` V.SetTo V.reverseVideo

        it "gives user messages a palette gray panel distinct from assistant messages" do
            let user = attrMapLookup Theme.userAttr Theme.terminalDefault
                userMuted = attrMapLookup Theme.userMutedAttr Theme.terminalDefault
                assistant = attrMapLookup Theme.assistantAttr Theme.terminalDefault
            V.attrBackColor user `shouldBe` V.SetTo V.brightBlack
            V.attrBackColor userMuted `shouldBe` V.SetTo V.brightBlack
            V.attrBackColor assistant `shouldBe` V.Default
            V.attrForeColor userMuted `shouldBe` V.Default

        it "does not introduce colors in monochrome mode" do
            map
                ( \syntaxClass ->
                    let attr =
                            attrMapLookup
                                (Theme.syntaxClassAttr syntaxClass)
                                Theme.monochrome
                    in (V.attrForeColor attr, V.attrBackColor attr)
                )
                allSyntaxClasses
                `shouldBe`
                    replicate
                        (length allSyntaxClasses)
                        (V.Default, V.Default)

        it "does not paint user message panels in monochrome mode" do
            V.attrBackColor
                (attrMapLookup Theme.userAttr Theme.monochrome)
                `shouldBe` V.Default
            V.attrBackColor
                (attrMapLookup Theme.userMutedAttr Theme.monochrome)
                `shouldBe` V.Default

    describe "live accent wave" do
        it "fades a named accent through distinct RGB values" do
            let tool = attrMapLookup Theme.toolAttr Theme.terminalDefault
                samples =
                    [ Theme.waveForeground V.defAttr tool (fromIntegral step / 20)
                    | step <- [0 .. 20 :: Int]
                    ]
                colors = map V.attrForeColor samples
            length (nub colors) `shouldSatisfy` (> 5)
            case (samples, reverse samples) of
                (first : _, lastSample : _) ->
                    V.attrForeColor first
                        `shouldNotBe` V.attrForeColor lastSample
                _ ->
                    expectationFailure "expected wave samples"
            colors `shouldSatisfy` all isRgbForeground

        it "uses a magenta running peak and dark trough" do
            Theme.runningWavePeak `shouldBe` RGBColor 187 154 247
            Theme.waveTrough `shouldBe` RGBColor 36 40 59
            Theme.wavePeakFor Theme.thinkingAttr
                `shouldBe` Theme.thinkingWavePeak
            Theme.wavePeakFor Theme.toolAttr
                `shouldBe` Theme.runningWavePeak
            let trough = Theme.waveForegroundFrom Theme.waveTrough Theme.runningWavePeak 0.0
                peak = Theme.waveForegroundFrom Theme.waveTrough Theme.runningWavePeak 1.0
            V.attrForeColor trough `shouldBe` V.SetTo Theme.waveTrough
            V.attrForeColor peak `shouldBe` V.SetTo Theme.runningWavePeak

        it "reads the page background from COLORFGBG" do
            Theme.waveTroughFromColorFgBg (Just "15;0")
                `shouldBe` RGBColor 0 0 0
            Theme.waveTroughFromColorFgBg (Just "0;15")
                `shouldBe` RGBColor 255 255 255
            Theme.waveTroughFromColorFgBg Nothing
                `shouldBe` Theme.waveTrough

        it "breathes the waiting diamond without leaving the unit range" do
            let samples =
                    [ Theme.waitingPulseAttr True MotionFull Theme.waveTrough elapsed
                    | elapsed <- [0, 200, 650, 1300]
                    ]
            length (nub (map V.attrForeColor samples))
                `shouldSatisfy` (> 1)
            Theme.waitingPulseAttr True MotionOff Theme.waveTrough 0
                `shouldBe`
                    Theme.waitingPulseAttr True MotionOff Theme.waveTrough 1300
            V.attrForeColor
                (Theme.waitingPulseAttr False MotionFull Theme.waveTrough 200)
                `shouldBe` V.Default

terminalForeground :: AttrName -> V.MaybeDefault V.Color
terminalForeground =
    V.attrForeColor . (`attrMapLookup` Theme.terminalDefault)

isRgbForeground :: V.MaybeDefault V.Color -> Bool
isRgbForeground = \case
    V.SetTo RGBColor{} -> True
    _ -> False

allSyntaxClasses :: [SyntaxClass]
allSyntaxClasses = [minBound .. maxBound]
