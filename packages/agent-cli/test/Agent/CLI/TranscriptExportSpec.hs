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
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import System.Directory
    ( doesFileExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import qualified System.Directory.OsPath as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = do
    describe "visibleSessionTurns" do
        it "starts at the most recent reset marker" do
            map (.turnUserText)
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
            output `shouldSatisfy` Text.isInfixOf "# Agent conversation"
            output `shouldSatisfy` Text.isInfixOf "## User"
            output `shouldSatisfy` Text.isInfixOf "fresh"
            output `shouldSatisfy` (not . Text.isInfixOf "stale")

    describe "path and save helpers" do
        it "expands tilde, writes UTF-8, and refuses to overwrite" $
          withTempDir "agent-export-spec-" \root -> do
            home <- Directory.getHomeDirectory
            let
                target = root `appendPath` "conversation.md"
            resolved <- resolveExportPath root "~/conversation.md"
            resolved `shouldBe`
                Right (home `appendPath` "conversation.md")
            saveTranscriptNoClobber target "first 🌍" `shouldReturn` Right ()
            saveTranscriptNoClobber target "second"
                >>= (`shouldSatisfy` isLeft)
            doesFileExist (unsafeToFilePath target) `shouldReturn` True
            TextIO.readFile (unsafeToFilePath target)
                `shouldReturn` "first 🌍"

        it "uses a stable default filename" do
            defaultExportFileName "2026-01-01-abcd"
                `shouldBe` "agent-session-2026-01-01-abcd.md"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

appendPath :: OsPath -> Text -> OsPath
appendPath path name = path </> unsafeEncodeUtf (Text.unpack name)

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    root <- getTemporaryDirectory
    bracket
        (unsafeEncodeUtf <$> mkdtemp (root FilePath.</> prefix <> "XXXXXX"))
        (removePathForcibly . unsafeToFilePath)
        action

turn :: Text -> TranscriptEffect -> SessionTurn
turn text effect = turnWithAnswer text Nothing effect

turnWithAnswer :: Text -> Maybe Text -> TranscriptEffect -> SessionTurn
turnWithAnswer user assistant effect = SessionTurn
    { turnAt = UTCTime (fromGregorian 2026 1 1) 0
    , turnUserText = user
    , turnAssistantText = assistant
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnEffect = effect
    , turnItems = []
    , turnUsage = Nothing
    }
