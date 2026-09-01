module Agent.TUI.ThemeSpec (spec) where

import Agent.Syntax (SyntaxClass(..))
import Agent.TUI.Motion (MotionMode(..))
import qualified Agent.TUI.Theme as Theme
import Brick (AttrName)
import Brick.AttrMap (attrMapLookup, mapAttrName, setDefaultAttr)
import Data.Bits ((.|.))
import Data.List (nub)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (Color(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "structural theme attributes" do
        it "uses terminal ANSI slots rather than fixed RGB colors" do
            map terminalForeground
                [ Theme.borderActiveAttr
                , Theme.controlLinkAttr
                , Theme.controlLinkHoverAttr
                , Theme.controlLinkActiveAttr
                , Theme.lambdaDimAttr
                , Theme.lambdaTrailAttr
                , Theme.lambdaGlowAttr
                , Theme.lambdaSparkAttr
                , Theme.selectedAttr
                , Theme.selectedMutedAttr
                , Theme.todoPendingAttr
                ]
                `shouldBe`
                    [ V.SetTo V.brightBlack
                    , V.SetTo V.brightBlack
                    , V.Default
                    , V.SetTo V.brightWhite
                    , V.SetTo V.brightBlack
                    , V.SetTo V.brightBlack
                    , V.Default
                    , V.SetTo V.brightWhite
                    , V.SetTo V.brightWhite
                    , V.SetTo V.white
                    , V.Default
                    ]

    describe "syntax theme attributes" do
        it "keeps the daylight page background while dimming overlays" do
            V.attrBackColor
                (attrMapLookup Theme.dimAttr (Theme.themeAttrMap Theme.Daylight))
                `shouldBe` V.SetTo (RGBColor 250 247 242)

        it "keeps daylight controls on the page instead of the terminal background" do
            V.attrBackColor
                (attrMapLookup
                    Theme.controlLinkAttr
                    (Theme.themeAttrMap Theme.Daylight))
                `shouldBe` V.SetTo (RGBColor 250 247 242)
            V.attrBackColor
                (attrMapLookup
                    Theme.controlLinkHoverAttr
                    (Theme.themeAttrMap Theme.Daylight))
                `shouldBe` V.SetTo (RGBColor 255 255 255)

        it "sets a daylight background on semantic text attributes" do
            map
                ( V.attrBackColor
                    . (\name ->
                        attrMapLookup
                            name
                            (Theme.themeAttrMap Theme.Daylight))
                )
                [ Theme.baseAttr
                , Theme.headerAttr
                , Theme.footerAttr
                , Theme.mutedAttr
                , Theme.assistantAttr
                , Theme.thinkingAttr
                , Theme.toolAttr
                , Theme.errorAttr
                , Theme.successAttr
                , Theme.codeAttr
                , Theme.linkAttr
                , Theme.syntaxKeywordAttr
                ]
                `shouldSatisfy` all (/= V.Default)

        it "leaves every syntax background to the terminal theme" do
            map
                ( V.attrBackColor
                    . (`attrMapLookup` Theme.terminalDefault)
                    . Theme.syntaxClassAttr
                )
                allSyntaxClasses
                `shouldBe` replicate (length allSyntaxClasses) V.Default

        it "uses a readable neutral panel for selections" do
            let selected =
                    attrMapLookup Theme.selectedAttr Theme.terminalDefault
                selectedMuted =
                    attrMapLookup Theme.selectedMutedAttr Theme.terminalDefault
            V.attrForeColor selected `shouldBe` V.SetTo V.brightWhite
            V.attrBackColor selected `shouldBe` V.SetTo V.brightBlack
            V.attrStyle selected `shouldBe` V.SetTo V.bold
            V.attrForeColor selectedMuted `shouldBe` V.SetTo V.white
            V.attrBackColor selectedMuted `shouldBe` V.SetTo V.brightBlack

        it "keeps idle chrome quieter than focused chrome" do
            let border = attrMapLookup Theme.borderAttr Theme.terminalDefault
                active =
                    attrMapLookup Theme.borderActiveAttr Theme.terminalDefault
            V.attrForeColor border `shouldBe` V.SetTo V.brightBlack
            V.attrForeColor active `shouldBe` V.SetTo V.brightBlack
            V.attrStyle border `shouldBe` V.SetTo V.dim
            V.attrStyle active `shouldBe` V.Default

        it "gives user messages a palette gray panel distinct from assistant messages" do
            let user = attrMapLookup Theme.userAttr Theme.terminalDefault
                userMuted = attrMapLookup Theme.userMutedAttr Theme.terminalDefault
                assistant = attrMapLookup Theme.assistantAttr Theme.terminalDefault
            V.attrForeColor user `shouldBe` V.SetTo V.brightWhite
            V.attrBackColor user `shouldBe` V.SetTo V.brightBlack
            V.attrForeColor userMuted `shouldBe` V.SetTo V.white
            V.attrBackColor userMuted `shouldBe` V.SetTo V.brightBlack
            V.attrBackColor assistant `shouldBe` V.Default

        it "keeps muted transcript text readable on the palette hover band" do
            let theme = Theme.terminalDefault
                hover =
                    attrMapLookup
                        Theme.transcriptHoverAttr
                        theme
                hoveredTheme =
                    mapAttrName
                        Theme.transcriptHoverMutedAttr
                        Theme.mutedAttr
                        (setDefaultAttr hover theme)
                hoveredMuted =
                    attrMapLookup Theme.mutedAttr hoveredTheme
            V.attrBackColor hover `shouldBe` V.SetTo V.brightBlack
            V.attrForeColor hover `shouldBe` V.SetTo V.white
            V.attrForeColor hoveredMuted `shouldBe` V.SetTo V.white
            V.attrBackColor hoveredMuted `shouldBe` V.SetTo V.brightBlack
            V.attrStyle hoveredMuted `shouldBe` V.SetTo V.dim

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
            map
                ( \name ->
                    let attr = attrMapLookup name Theme.monochrome
                    in (V.attrForeColor attr, V.attrBackColor attr)
                )
                [ Theme.userAttr
                , Theme.userMutedAttr
                , Theme.selectedAttr
                , Theme.selectedMutedAttr
                , Theme.transcriptHoverAttr
                ]
                `shouldBe` replicate 5 (V.Default, V.Default)

        it "retains reverse video for muted monochrome hover text" do
            let theme = Theme.monochrome
                hover =
                    attrMapLookup
                        Theme.transcriptHoverAttr
                        theme
                hoveredTheme =
                    mapAttrName
                        Theme.transcriptHoverMutedAttr
                        Theme.mutedAttr
                        (setDefaultAttr hover theme)
                hoveredMuted =
                    attrMapLookup Theme.mutedAttr hoveredTheme
            V.attrStyle hoveredMuted
                `shouldBe` V.SetTo (V.reverseVideo .|. V.dim)

    describe "live accent wave" do
        it "derives daylight motion colors from its palette" do
            Theme.waveTroughForTheme
                Theme.Daylight
                (RGBColor 0 0 0)
                `shouldBe` RGBColor 250 247 242
            Theme.wavePeakForTheme Theme.Daylight Theme.toolAttr
                `shouldBe` RGBColor 144 80 150
            Theme.wavePeakForTheme Theme.Daylight Theme.thinkingAttr
                `shouldBe` RGBColor 110 105 100
            V.attrBackColor
                (Theme.waitingPulseAttrForTheme
                    Theme.Daylight
                    True
                    MotionOff
                    (RGBColor 250 247 242)
                    0)
                `shouldBe` V.SetTo (RGBColor 250 247 242)

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
            let trough = Theme.waveForegroundFrom True Theme.waveTrough Theme.runningWavePeak 0.0
                peak = Theme.waveForegroundFrom True Theme.waveTrough Theme.runningWavePeak 1.0
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
            V.attrForeColor
                (Theme.waveForegroundFrom False Theme.waveTrough Theme.runningWavePeak 0.8)
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
