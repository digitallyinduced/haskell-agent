module Agent.OpenAI.CompactionManagerSpec (spec) where

import Agent.OpenAI.Compaction.Manager
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Compaction manager" do
    it "skips storage and compression below the continuation threshold" do
        prepareCompaction strategy 100 (Just 1) (Just "previous") ["pending"] ["pending"]
            (fail "unexpected load") (const (fail "unexpected send"))
            `shouldReturn` (["pending"], Just "previous")
    it "compacts at the context limit even when the configured threshold is larger" do
        installed <- newIORef []
        let source = CompactionSource [Text.replicate 95 "x"] (writeIORef installed)
        prepareCompaction strategy 500 (Just 95) (Just "previous") ["pending"] ["pending"]
            (pure source) (const (pure ["summary"]))
            `shouldReturn` (["summary", "pending"], Nothing)
        readIORef installed `shouldReturn` ["summary"]
    it "checks request overhead on the continuation fast path" do
        let overheadStrategy = strategy { estimateRequest = const 101 }
        prepareCompaction overheadStrategy 100 (Just 1) (Just "previous") ["p"] ["p"]
            (fail "unexpected load") (const (fail "unexpected send"))
            `shouldThrow` anyIOException
    it "isolates pending inputs and installs before returning a new request" do
        installed <- newIORef []
        let source = CompactionSource ["observed"] (writeIORef installed)
            checkedStrategy = strategy
                { compressHistory = \request history send -> do
                    request `shouldBe` []
                    history `shouldBe` ["observed"]
                    send history
                }
        result <- prepareCompaction checkedStrategy 5 Nothing Nothing ["pending"] ["pending"]
            (pure source) (const (pure ["summary"]))
        readIORef installed `shouldReturn` ["summary"]
        result `shouldBe` (["summary", "pending"], Nothing)
    it "does not install a failed compression" do
        installed <- newIORef False
        let source = CompactionSource ["observed"] (const (writeIORef installed True))
        prepareCompaction strategy 5 Nothing Nothing ["pending"] ["pending"]
            (pure source) (const (fail "provider failure"))
            `shouldThrow` anyIOException
        readIORef installed `shouldReturn` False
    it "rejects an oversized custom strategy result before installation" do
        installed <- newIORef False
        let source = CompactionSource ["observed"] (const (writeIORef installed True))
        prepareCompaction strategy 5 Nothing Nothing ["pending"] ["pending"]
            (pure source) (const (pure [Text.replicate 200 "x"]))
            `shouldThrow` anyIOException
        readIORef installed `shouldReturn` False
    it "propagates installation failure instead of returning a usable continuation" do
        let source = CompactionSource ["observed"] (const (fail "storage failure"))
        prepareCompaction strategy 5 Nothing (Just "previous") ["pending"] ["pending"]
            (pure source) (const (pure ["summary"]))
            `shouldThrow` anyIOException
    it "checks complete durable history even when a stateless request was trimmed" do
        installed <- newIORef []
        let source = CompactionSource [Text.replicate 80 "x"] (writeIORef installed)
        prepareCompaction strategy 50 Nothing Nothing ["p"] ["p"]
            (pure source) (const (pure ["summary"]))
            `shouldReturn` (["summary", "p"], Nothing)
        readIORef installed `shouldReturn` ["summary"]
    it "rejects oversized pending input when there is no history to compact" do
        prepareCompaction strategy 50 Nothing Nothing [Text.replicate 200 "x"]
            [Text.replicate 200 "x"] (pure (CompactionSource [] (const (pure ()))))
            (const (fail "unexpected send")) `shouldThrow` anyIOException

strategy :: CompactionStrategy [Text] Text [Text]
strategy = CompactionStrategy
    { estimateRequest = sum . map Text.length
    , estimateItem = Text.length
    , contextWindow = const 100
    , requestItems = id
    , replaceItems = const id
    , compressHistory = \_ history send -> send history
    , replacementHistory = \_ _ _ _ response -> pure response
    }
