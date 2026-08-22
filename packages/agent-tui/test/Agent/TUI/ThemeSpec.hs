module Agent.TUI.ThemeSpec (spec) where

import Agent.Syntax (SyntaxClass(..))
import qualified Agent.TUI.Theme as Theme
import Brick.AttrMap (attrMapLookup)
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "syntax theme attributes" do
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

allSyntaxClasses :: [SyntaxClass]
allSyntaxClasses = [minBound .. maxBound]
