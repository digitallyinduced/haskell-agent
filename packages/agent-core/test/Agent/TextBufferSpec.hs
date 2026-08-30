module Agent.TextBufferSpec (spec) where

import Agent.TextBuffer
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary(..), listOf, resize, (===), (.&&.))

newtype ChunkText = ChunkText Text.Text
    deriving (Show)

instance Arbitrary ChunkText where
    arbitrary = ChunkText . Text.pack <$> resize 32 (listOf arbitrary)

newtype ChunkList = ChunkList [ChunkText]
    deriving (Show)

instance Arbitrary ChunkList where
    arbitrary = ChunkList <$> resize 20 (listOf arbitrary)

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
        textBufferLength buffered `shouldBe` 10000
        textBufferChunkCount buffered `shouldBe` 10000

    it "ignores empty chunks and compacts without changing content" do
        let buffered =
                appendTextBuffer "second" $
                    appendTextBuffer "" $
                        textBufferFromText "first "
        textBufferNull buffered `shouldBe` False
        compactTextBuffer buffered `shouldBe` textBufferFromText "first second"
        textBufferLength buffered `shouldBe` Text.length "first second"
        textBufferChunkCount (compactTextBuffer buffered) `shouldBe` 1
        textBufferNull (appendTextBuffer "" emptyTextBuffer) `shouldBe` True

    modifyMaxSuccess (const 300) $
        prop "preserves chunk order for arbitrary streams" $
            \(ChunkList chunks) ->
                let texts = [text | ChunkText text <- chunks]
                    buffered = foldl' (flip appendTextBuffer) emptyTextBuffer texts
                in textBufferToText buffered === Text.concat texts

    modifyMaxSuccess (const 300) $
        prop "compaction preserves content and is idempotent" $
            \(ChunkList chunks) ->
                let texts = [text | ChunkText text <- chunks]
                    buffered = foldl' (flip appendTextBuffer) emptyTextBuffer texts
                    compacted = compactTextBuffer buffered
                in textBufferToText compacted === textBufferToText buffered
                    .&&. compactTextBuffer compacted === compacted
