module Agent.TUI.ThemeSpec (spec) where

import Agent.Syntax (SyntaxClass(..))
import qualified Agent.TUI.Theme as Theme
import Brick (AttrName)
import Brick.AttrMap (attrMapLookup)
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = do
    describe "structural theme attributes" do
        it "keeps active chrome and controls on the Solarized neutral ramp" do
            map solarizedForeground
                [ Theme.borderActiveAttr
                , Theme.controlLinkAttr
                , Theme.controlLinkHoverAttr
                , Theme.controlLinkActiveAttr
                , Theme.lambdaDimAttr
                , Theme.lambdaTrailAttr
                , Theme.lambdaGlowAttr
                , Theme.lambdaSparkAttr
                ]
                `shouldBe`
                    [ V.SetTo (V.RGBColor 101 123 131)
                    , V.SetTo (V.RGBColor 101 123 131)
                    , V.SetTo (V.RGBColor 131 148 150)
                    , V.SetTo (V.RGBColor 147 161 161)
                    , V.SetTo (V.RGBColor 69 94 100)
                    , V.SetTo (V.RGBColor 88 110 117)
                    , V.SetTo (V.RGBColor 101 123 131)
                    , V.SetTo (V.RGBColor 131 148 150)
                    ]

    describe "syntax theme attributes" do
        it "keeps native text selection visible over fenced code" do
            let background attribute =
                    V.attrBackColor
                        (attrMapLookup attribute Theme.solarizedDark)
            background Theme.codeAttr
                `shouldNotBe` background Theme.selectedAttr

        it "keeps every Solarized syntax class on the code-block background" do
            let codeBackground =
                    V.attrBackColor
                        (attrMapLookup Theme.codeAttr Theme.solarizedDark)
            map
                ( V.attrBackColor
                    . (`attrMapLookup` Theme.solarizedDark)
                    . Theme.syntaxClassAttr
                )
                allSyntaxClasses
                `shouldBe` replicate (length allSyntaxClasses) codeBackground

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

solarizedForeground :: AttrName -> V.MaybeDefault V.Color
solarizedForeground =
    V.attrForeColor . (`attrMapLookup` Theme.solarizedDark)

allSyntaxClasses :: [SyntaxClass]
allSyntaxClasses = [minBound .. maxBound]
