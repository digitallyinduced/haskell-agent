module Agent.CLI.ConversationStoreSpec (spec) where

import Agent.CLI.Session.ConversationStore
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (throwString)
import Data.IORef
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "ConversationStore" do
        it "hydrates a cold transcript only for the scoped read" do
            loads <- newIORef (0 :: Int)
            let items = [messageItem "cold"]
                checkpoint = TranscriptCheckpoint "turn:1" do
                    modifyIORef' loads (+ 1)
                    pure items
            store <- newColdConversationStore
                (Just "response-1")
                checkpoint
                []

            withConversationTranscript store (`shouldBe` items)

            readIORef loads `shouldReturn` 1
            conversationResidency store `shouldReturn` ConversationCold
            readConversationPreviousResponseId store
                `shouldReturn` Just "response-1"

        it "returns to cold state when a scoped reader throws" do
            let checkpoint = TranscriptCheckpoint "turn:1" $
                    pure [messageItem "cold"]
            store <- newColdConversationStore Nothing checkpoint []

            withConversationTranscript store
                (\_ -> throwString "reader failed")
                `shouldThrow` anyException

            conversationResidency store `shouldReturn` ConversationCold

        it "remains cold when checkpoint hydration fails" do
            let checkpoint = TranscriptCheckpoint "broken" $
                    throwString "checkpoint unavailable"
            store <- newColdConversationStore Nothing checkpoint []

            withConversationTranscript store (\_ -> pure ())
                `shouldThrow` anyException

            conversationResidency store `shouldReturn` ConversationCold

        it "does not let a scoped reader evict a newer commit" do
            let checkpoint = TranscriptCheckpoint "turn:1" $
                    pure [messageItem "cold"]
                newer = [messageItem "newer"]
            store <- newColdConversationStore Nothing checkpoint []

            generation <- withConversationTranscript store \_ ->
                commitConversationTranscript store newer

            conversationResidency store `shouldReturn` ConversationResident
            currentTranscriptGeneration store `shouldReturn` generation
            withConversationTranscript store (`shouldBe` newer)

        it "rejects a stale explicit eviction" do
            store <- newConversationStore Nothing [messageItem "initial"] []
            stale <- currentTranscriptGeneration store
            current <- commitConversationTranscript store [messageItem "newer"]
            let checkpoint = TranscriptCheckpoint "stale" $
                    pure [messageItem "initial"]

            evictConversationTranscript store stale checkpoint
                `shouldReturn` False
            currentTranscriptGeneration store `shouldReturn` current
            withConversationTranscript store
                (`shouldBe` [messageItem "newer"])

        it "evicts the expected committed generation" do
            let items = [messageItem "durable"]
            store <- newConversationStore Nothing [] []
            generation <- commitConversationTranscript store items
            let checkpoint = TranscriptCheckpoint "turn:2" (pure items)

            evictConversationTranscript store generation checkpoint
                `shouldReturn` True
            conversationResidency store `shouldReturn` ConversationCold
            withConversationTranscript store (`shouldBe` items)
            conversationResidency store `shouldReturn` ConversationCold

        it "coalesces concurrent hydration and safely re-evicts" do
            loads <- newIORef (0 :: Int)
            firstEntered <- newEmptyMVar
            secondEntered <- newEmptyMVar
            releaseFirst <- newEmptyMVar
            releaseSecond <- newEmptyMVar
            let items = [messageItem "shared"]
                checkpoint = TranscriptCheckpoint "turn:1" do
                    modifyIORef' loads (+ 1)
                    pure items
                firstReader transcript = do
                    putMVar firstEntered ()
                    takeMVar releaseFirst
                    pure transcript
                secondReader transcript = do
                    putMVar secondEntered ()
                    takeMVar releaseSecond
                    pure transcript
            store <- newColdConversationStore Nothing checkpoint []

            withAsync
                    (withConversationTranscript store firstReader)
                    \firstReaderAsync -> do
                takeMVar firstEntered
                withAsync
                        (withConversationTranscript store secondReader)
                        \secondReaderAsync -> do
                    takeMVar secondEntered
                    putMVar releaseFirst ()
                    wait firstReaderAsync `shouldReturn` items
                    conversationResidency store
                        `shouldReturn` ConversationResident
                    putMVar releaseSecond ()
                    wait secondReaderAsync `shouldReturn` items
            readIORef loads `shouldReturn` 1
            conversationResidency store `shouldReturn` ConversationCold

        it "defers a newer checkpoint until active readers release" do
            readerEntered <- newEmptyMVar
            releaseReader <- newEmptyMVar
            newerLoads <- newIORef (0 :: Int)
            let items = [messageItem "shared"]
                original = TranscriptCheckpoint "turn:1" (pure items)
                newer = TranscriptCheckpoint "turn:2" do
                    modifyIORef' newerLoads (+ 1)
                    pure items
                reader transcript = do
                    putMVar readerEntered ()
                    takeMVar releaseReader
                    pure transcript
            store <- newColdConversationStore Nothing original []
            generation <- currentTranscriptGeneration store

            withAsync
                    (withConversationTranscript store reader)
                    \readerAsync -> do
                takeMVar readerEntered
                evictConversationTranscript store generation newer
                    `shouldReturn` False
                conversationResidency store
                    `shouldReturn` ConversationResident
                putMVar releaseReader ()
                wait readerAsync `shouldReturn` items

            conversationResidency store `shouldReturn` ConversationCold
            withConversationTranscript store (`shouldBe` items)
            readIORef newerLoads `shouldReturn` 1

        it "retargets a cold transcript without hydrating the old snapshot" do
            oldLoads <- newIORef (0 :: Int)
            newLoads <- newIORef (0 :: Int)
            let old = TranscriptCheckpoint "source" do
                    modifyIORef' oldLoads (+ 1)
                    pure [messageItem "old"]
                new = TranscriptCheckpoint "fork" do
                    modifyIORef' newLoads (+ 1)
                    pure [messageItem "fork"]
            store <- newColdConversationStore Nothing old []

            retargetConversationCheckpoint store new

            readIORef oldLoads `shouldReturn` 0
            withConversationTranscript store
                (`shouldBe` [messageItem "fork"])
            readIORef oldLoads `shouldReturn` 0
            readIORef newLoads `shouldReturn` 1

        it "keeps response id and attachments independent of hydration" do
            loads <- newIORef (0 :: Int)
            let image = ImageAttachment "image/png" "png"
                checkpoint = TranscriptCheckpoint "turn:1" do
                    modifyIORef' loads (+ 1)
                    pure [messageItem "cold"]
            store <- newColdConversationStore Nothing checkpoint []

            writeConversationPreviousResponseId store (Just "response-2")
            modifyConversationAttachments store \_ -> ([image], ())

            readConversationPreviousResponseId store
                `shouldReturn` Just "response-2"
            readConversationAttachments store `shouldReturn` [image]
            readIORef loads `shouldReturn` 0
            conversationResidency store `shouldReturn` ConversationCold

        it "resets all conversation fields and invalidates stale generations" do
            let image = ImageAttachment "image/png" "png"
            store <- newConversationStore
                (Just "response-1")
                [messageItem "resident"]
                [image]
            stale <- currentTranscriptGeneration store
            resetConversationStore store
            let checkpoint = TranscriptCheckpoint "stale" $
                    pure [messageItem "resident"]

            evictConversationTranscript store stale checkpoint
                `shouldReturn` False
            readConversationPreviousResponseId store `shouldReturn` Nothing
            readConversationAttachments store `shouldReturn` []
            withConversationTranscript store (`shouldBe` [])

messageItem :: Text -> ResponseItem
messageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }
