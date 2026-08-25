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

terminalForeground :: AttrName -> V.MaybeDefault V.Color
terminalForeground =
    V.attrForeColor . (`attrMapLookup` Theme.terminalDefault)

allSyntaxClasses :: [SyntaxClass]
allSyntaxClasses = [minBound .. maxBound]
