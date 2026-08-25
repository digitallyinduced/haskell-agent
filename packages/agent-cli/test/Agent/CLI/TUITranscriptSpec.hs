module Agent.CLI.TUITranscriptSpec (spec) where

import Agent.CLI.TUI.Transcript
    ( transcriptChunkCacheKey
    , transcriptChunkSize
    , transcriptChunks
    )
import Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    )
import qualified Data.Sequence as Seq
import Test.Hspec

spec :: Spec
spec = describe "fullscreen transcript caching" do
    it "splits retained history into fixed-size chunks" do
        map Seq.length (transcriptChunks (Seq.fromList (map block [1 .. 65])))
            `shouldBe` [32, 32, 1]

    it "caches a complete stable chunk by its endpoint IDs" do
        transcriptChunkCacheKey
            (const True)
            []
            completeChunk
            `shouldBe` Just (BlockId 1, BlockId transcriptChunkSize)

    it "does not cache the growing final partial chunk" do
        transcriptChunkCacheKey
            (const True)
            []
            (Seq.take (transcriptChunkSize - 1) completeChunk)
            `shouldBe` Nothing

    it "does not cache chunks containing unstable blocks" do
        transcriptChunkCacheKey
            ((/= BlockId 7) . (.blockId))
            []
            completeChunk
            `shouldBe` Nothing

    it "does not cache expanded or interactive chunks" do
        let expanded =
                Seq.adjust
                    (\value -> value { blockExpanded = True })
                    5
                    completeChunk
        transcriptChunkCacheKey (const True) [] expanded
            `shouldBe` Nothing
        transcriptChunkCacheKey (const True) [BlockId 12] completeChunk
            `shouldBe` Nothing
        transcriptChunkCacheKey (const True) [BlockId 99] completeChunk
            `shouldBe` Just (BlockId 1, BlockId transcriptChunkSize)
  where
    completeChunk =
        Seq.fromList (map block [1 .. transcriptChunkSize])

block :: Int -> UiBlock
block number = UiBlock
    { blockId = BlockId number
    , blockKind = BlockAssistant
    , blockTitle = "Assistant"
    , blockBody = "body"
    , blockTimestamp = ""
    , blockDetail = ""
    , blockState = BlockComplete
    , blockExpanded = False
    , blockCallId = Nothing
    }
