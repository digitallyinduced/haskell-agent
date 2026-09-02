module Agent.CLI.TUITranscriptSpec (spec) where

import Agent.CLI.TUI.Transcript
    ( coalesceInspectionBlocks
    , transcriptChunkCacheKey
    , transcriptChunkSize
    , transcriptChunks
    )
import Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    )
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
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

    it "coalesces adjacent completed inspection blocks under the newest ID" do
        let first =
                (groupableInspection 1)
                    { blockTitle = "Read"
                    , blockDetail = "src/A.hs"
                    , blockBody = "module A where"
                    }
            second =
                (groupableInspection 2)
                    { blockTitle = "Listed"
                    , blockDetail = "src"
                    , blockBody = "A.hs"
                    }
            grouped =
                toList
                    (coalesceInspectionBlocks (Seq.fromList [first, second]))
        case grouped of
            [summary] -> do
                summary.blockId `shouldBe` BlockId 2
                summary.blockTitle `shouldBe` "Read 1 file, Listed 1 dir"
                summary.blockBody
                    `shouldBe` "  ◇ Read src/A.hs\n  ◇ Listed src"
            _ -> expectationFailure "expected one inspection summary"

    it "retains expanded inspection details" do
        let first =
                (groupableInspection 1)
                    { blockTitle = "Searched needle"
                    , blockBody = "src/A.hs:1"
                    }
            second =
                (groupableInspection 2)
                    { blockTitle = "Searched another"
                    , blockBody = "src/B.hs:2"
                    , blockExpanded = True
                    }
            grouped =
                toList
                    (coalesceInspectionBlocks (Seq.fromList [first, second]))
        case grouped of
            [summary] -> do
                summary.blockTitle `shouldBe` "Searched 2 queries"
                summary.blockState `shouldBe` BlockComplete
                summary.blockBody `shouldSatisfy`
                    Text.isInfixOf "    src/A.hs:1"
                summary.blockBody `shouldSatisfy`
                    Text.isInfixOf "    src/B.hs:2"
            _ -> expectationFailure "expected one inspection summary"

    it "does not recoalesce an inspection summary with later calls" do
        let first =
                (groupableInspection 1)
                    { blockTitle = "Read"
                    , blockDetail = "src/A.hs"
                    }
            second =
                (groupableInspection 2)
                    { blockTitle = "Listed"
                    , blockDetail = "src"
                    }
            search =
                (groupableInspection 3)
                    { blockTitle = "Searched"
                    , blockDetail = "needle"
                    }
            initial =
                coalesceInspectionBlocks (Seq.fromList [first, second])
            continued =
                toList
                    (coalesceInspectionBlocks (initial Seq.|> search))
        case continued of
            [summary, later] -> do
                summary.blockTitle `shouldBe` "Read 1 file, Listed 1 dir"
                summary.blockBody
                    `shouldBe` "  ◇ Read src/A.hs\n  ◇ Listed src"
                summary.blockInspectionGroupable `shouldBe` False
                later.blockTitle `shouldBe` "Searched"
            _ -> expectationFailure "expected a summary and a later call"

    it "does not merge running, failed, or non-adjacent inspection blocks" do
        let inspect ident state =
                (groupableInspection ident)
                    { blockTitle = "Read src/A.hs"
                    , blockState = state
                    }
            input = Seq.fromList
                [ inspect 1 BlockComplete
                , inspect 2 BlockRunning
                , block 3
                , inspect 4 BlockComplete
                , inspect 5 BlockFailed
                ]
        map (.blockId) (toList (coalesceInspectionBlocks input))
            `shouldBe` map BlockId [1, 2, 3, 4, 5]

    it "keeps image inspections standalone" do
        let viewed =
                (block 1)
                    { blockKind = BlockInspect
                    , blockTitle = "Viewed screenshot.png"
                    }
            readFile =
                (groupableInspection 2)
                    { blockTitle = "Read src/A.hs"
                    }
        map (.blockId)
            (toList (coalesceInspectionBlocks (Seq.fromList [viewed, readFile])))
            `shouldBe` map BlockId [1, 2]

    it "keeps lifecycle-sensitive inspections standalone" do
        let taskOutput =
                (block 1)
                    { blockKind = BlockInspect
                    , blockTitle = "Read task task-1"
                    }
            session =
                (block 2)
                    { blockKind = BlockInspect
                    , blockTitle = "Read agent session session-1"
                    }
        map (.blockId)
            (toList
                (coalesceInspectionBlocks
                    (Seq.fromList [taskOutput, session])))
            `shouldBe` map BlockId [1, 2]
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
    , blockInspectionGroupable = False
    }

groupableInspection :: Int -> UiBlock
groupableInspection number =
    (block number)
        { blockKind = BlockInspect
        , blockInspectionGroupable = True
        }
