{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.TranscriptExportSpec (spec) where

import Agent.CLI.Session (SessionTurn(..), TranscriptEffect(..))
import Agent.CLI.TranscriptExport
    ( defaultExportFileName
    , renderTranscriptMarkdown
    , resolveExportPath
    , saveTranscriptNoClobber
    , visibleSessionTurns
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import Agent.OsPath (unsafeToFilePath)
import Test.Hspec

spec :: Spec
spec = do
    describe "visibleSessionTurns" do
        it "starts at the most recent reset marker" do
            map turnUserText
                (visibleSessionTurns
                    [turn "old" TranscriptAppend, turn "clear" TranscriptReset, turn "new" TranscriptAppend])
                `shouldBe` ["clear", "new"]

    describe "renderTranscriptMarkdown" do
        it "renders role headings and excludes superseded turns" do
            let output = renderTranscriptMarkdown
                    [ turnWithAnswer "old" (Just "stale") TranscriptAppend
                    , turnWithAnswer "clear" (Just "Conversation cleared.") TranscriptReset
                    , turnWithAnswer "new" (Just "fresh") TranscriptAppend
                    ]
            output `shouldContain` "# Agent conversation"
            output `shouldContain` "## User"
            output `shouldContain` "fresh"
            output `shouldNotContain` "stale"

    describe "path and save helpers" do
        it "expands tilde and refuses to overwrite" do
            tmp <- getTemporaryDirectory
            let root = unsafeEncodeUtf (tmp <> "/agent-export-spec")
                target = root `appendPath` "conversation.md"
            createDirectoryIfMissing True (unsafeToFilePath root)
            resolved <- resolveExportPath root "~/conversation.md"
            resolved `shouldSatisfy` either (const False) (const True)
            saveTranscriptNoClobber target "first" `shouldReturn` Right ()
            saveTranscriptNoClobber target "second"
                `shouldSatisfy` isLeft
            doesFileExist (unsafeToFilePath target) `shouldReturn` True
            removePathForcibly (unsafeToFilePath root)

        it "uses a stable default filename" do
            defaultExportFileName "2026-01-01-abcd"
                `shouldBe` "agent-session-2026-01-01-abcd.md"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

appendPath :: OsPath -> Text -> OsPath
appendPath path name = path </> unsafeEncodeUtf (Text.unpack name)

turn :: Text -> TranscriptEffect -> SessionTurn
turn text effect = turnWithAnswer text Nothing effect

turnWithAnswer :: Text -> Maybe Text -> TranscriptEffect -> SessionTurn
turnWithAnswer user assistant effect = SessionTurn
    { turnAt = UTCTime (fromGregorian 2026 1 1) 0
    , turnUserText = user
    , turnAssistantText = Just assistant
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnEffect = effect
    , turnItems = []
    , turnUsage = Nothing
    }
