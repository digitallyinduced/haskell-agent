module Agent.TextBufferSpec (spec) where

import Agent.TextBuffer
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "TextBuffer" do
    it "preserves the arrival order of many small chunks" do
        let chunks = replicate 10000 "x"
            buffered =
                foldl'
                    (flip appendTextBuffer)
                    emptyTextBuffer
                    chunks
        textBufferToText buffered
            `shouldBe` Text.replicate 10000 "x"

    it "ignores empty chunks and compacts without changing content" do
        let buffered =
                appendTextBuffer "second" $
                    appendTextBuffer "" $
                        textBufferFromText "first "
        textBufferNull buffered `shouldBe` False
        compactTextBuffer buffered `shouldBe` textBufferFromText "first second"
        textBufferNull (appendTextBuffer "" emptyTextBuffer) `shouldBe` True
