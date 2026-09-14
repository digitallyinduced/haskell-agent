module Agent.CLI.InputHistorySpec (spec) where

import Agent.CLI.Input.History
import Control.Concurrent.Async (mapConcurrently_)
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import Data.List (sort)
import qualified Data.Text as Text
import qualified System.Console.Haskeline.History as Haskeline
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = describe "compact REPL history" do
    it "reads the Haskeline format, including Unicode, duplicates and newlines" $
        withHistoryPath \path -> do
            let entries = ["newest", "日本語🙂", "duplicate", "duplicate", "one\ntwo", "", "last\r"]
            Haskeline.writeHistory path $
                foldr Haskeline.addHistory Haskeline.emptyHistory entries
            expected <- map Text.pack . Haskeline.historyLines <$> Haskeline.readHistory path
            readReplHistoryAt path `shouldReturn` expected
    it "matches Haskeline decoding of malformed UTF-8 and an unterminated line" $
        withHistoryPath \path -> do
            ByteString.writeFile path (ByteString.pack [97, 255, 10, 98, 13, 10, 99])
            expected <- map Text.pack . Haskeline.historyLines <$> Haskeline.readHistory path
            readReplHistoryAt path `shouldReturn` expected
    it "writes bytes identical to Haskeline without losing existing entries" $
        withHistoryPath \path -> do
            let reference = path <> ".reference"
                entries = ["older", "日本語🙂", "duplicate", "duplicate"]
                added = "one\ntwo\r\nthree"
            Haskeline.writeHistory path $
                foldr Haskeline.addHistory Haskeline.emptyHistory entries
            original <- Haskeline.readHistory path
            Haskeline.writeHistory reference (Haskeline.addHistory added original)
            appendReplHistoryAt path (Text.pack added)
            expected <- ByteString.readFile reference
            ByteString.readFile path `shouldReturn` expected
    it "returns empty history for missing files and leaves submission filtering to callers" $
        withHistoryPath \path -> do
            readReplHistoryAt path `shouldReturn` []
            appendReplHistoryAt path "   "
            readReplHistoryAt path `shouldReturn` ["   "]
            appendReplHistoryAt path "\t"
            readReplHistoryAt path `shouldReturn` ["\t", "   "]
    it "keeps consecutive identical submissions" $
        withHistoryPath \path -> do
            appendReplHistoryAt path "duplicate"
            appendReplHistoryAt path "duplicate"
            readReplHistoryAt path `shouldReturn` ["duplicate", "duplicate"]
    it "serializes concurrent appends and keeps private permissions" $
        withHistoryPath \path -> do
            let entries = map (Text.pack . show) [1 :: Int .. 20]
            mapConcurrently_ (appendReplHistoryAt path) entries
            actual <- readReplHistoryAt path
            sort actual `shouldBe` sort entries
            mode <- fileMode <$> getFileStatus path
            mode .&. 0o777 `shouldBe` 0o600

withHistoryPath :: (FilePath -> IO a) -> IO a
withHistoryPath action =
    withSystemTempDirectory "agent_history_test" \directory ->
        action (directory </> "history")
