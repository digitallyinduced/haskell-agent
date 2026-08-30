{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.TranscriptSpec (spec) where

import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.Dialect (DialectId(CodexDialect))
import Agent.CLI.Transcript
    ( foldTranscriptTurns
    , markdownFence
    , renderTranscriptMarkdown
    , searchTranscriptBlocks
    )
import Agent.Provider (Provider(..))
import Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Transcript" do
    it "folds append, replace, and reset effects" do
        let turns =
                [ (0, turn TranscriptAppend "first")
                , (1, turn TranscriptReplace "checkpoint")
                , (2, turn TranscriptReset "reset")
                , (3, turn TranscriptAppend "after")
                ]
            bodies = map (.blockBody) (foldTranscriptTurns turns)
        bodies `shouldBe` ["reset", "after"]

    it "renders all block content and state" do
        let meta = testMeta
            block = testBlock
                { blockKind = BlockTool
                , blockTitle = "run command"
                , blockBody = "output"
                , blockDetail = "exit 1"
                , blockState = BlockFailed
                }
            output = renderTranscriptMarkdown meta [block]
        output `shouldSatisfy` Text.isInfixOf "## Tool"
        output `shouldSatisfy` Text.isInfixOf "run command"
        output `shouldSatisfy` Text.isInfixOf "output"
        output `shouldSatisfy` Text.isInfixOf "exit 1"
        output `shouldSatisfy` Text.isInfixOf "State: failed"

    it "uses a fence longer than embedded backtick runs" do
        let output = markdownFence "before ``` after"
        Text.isPrefixOf "````\n" output `shouldBe` True
        Text.isSuffixOf "\n````" output `shouldBe` True

    it "fences conversational bodies containing Markdown fences" do
        let block = testBlock { blockBody = "answer ``` with output" }
            output = renderTranscriptMarkdown testMeta [block]
        output `shouldSatisfy` Text.isInfixOf "````\nanswer ``` with output\n````"

    it "searches visible transcript fields case-insensitively" do
        let first = testBlock
                { blockTitle = "Compiler"
                , blockBody = "GHC loaded the module"
                }
            second = testBlock
                { blockTitle = "Tests"
                , blockDetail = "1300 examples passed"
                }
            blocks = [first, second]
        searchTranscriptBlocks "ghc" blocks `shouldBe` [first]
        searchTranscriptBlocks "EXAMPLES" blocks `shouldBe` [second]
        searchTranscriptBlocks "   " blocks `shouldBe` blocks

turn :: TranscriptEffect -> Text -> SessionTurn
turn effect text = SessionTurn
    { turnAt = testTime
    , turnUserText = ""
    , turnAssistantText = Just text
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnEffect = effect
    , turnItems = []
    , turnUsage = Nothing
    }

testMeta :: SessionMeta
testMeta = SessionMeta
    { metaVersion = 1
    , metaId = "session-1"
    , metaCreatedAt = testTime
    , metaUpdatedAt = testTime
    , metaProvider = OpenAIProvider
    , metaConnection = "openai"
    , metaModel = "gpt-test"
    , metaTransportModel = Nothing
    , metaDialect = CodexDialect
    , metaLegacySubagentTarget = Nothing
    , metaCwd = unsafeEncodeUtf "."
    , metaEffort = "medium"
    , metaTitle = "Test session"
    , metaTitleIsManual = False
    , metaTitleRefreshIndex = 0
    , metaTitleUserTurns = 0
    , metaLastResponseId = Nothing
    , metaInputTokens = 0
    , metaOutputTokens = 0
    , metaCachedTokens = 0
    , metaLastRecap = Nothing
    , metaLastTurnSummary = Nothing
    , metaLastRecapMainTurns = 0
    }

testBlock :: UiBlock
testBlock = UiBlock
    { blockId = BlockId 1
    , blockKind = BlockAssistant
    , blockTitle = "Assistant"
    , blockBody = "answer"
    , blockTimestamp = ""
    , blockDetail = ""
    , blockState = BlockComplete
    , blockExpanded = False
    , blockCallId = Nothing
    }

testTime :: UTCTime
testTime = read "2026-08-29 00:00:00 UTC"
