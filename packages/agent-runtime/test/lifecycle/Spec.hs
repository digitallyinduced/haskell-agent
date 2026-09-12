module Main (main) where

import Agent.Runtime.ConversationStore.Lifecycle
import Agent.Loop (BackendContinuation(..), BackendRevision(..), BackendSnapshot(..), ImageAttachment(..))
import Agent.Responses.Types
import Data.Text (Text)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "pure conversation lifecycle sequences" do
        it "requests an inert checkpoint and counts nested leases through retarget and final release" do
            case acquireTranscript initial of
                LoadCheckpoint checkpoint -> checkpoint `shouldBe` "original"
                Acquired{} -> expectationFailure "expected a load"
            let first = hydratedTranscript "original" items initial
            case acquireTranscript first of
                LoadCheckpoint{} -> expectationFailure "must reuse hydrated items"
                Acquired second release shared -> do
                    release `shouldBe` True
                    shared `shouldBe` items
                    second.stateTranscript `shouldBe`
                        ResidentTranscript items (HydratedResident "original" 2)
                    let forked = retargetCheckpoint "fork" second
                        oneLeft = fst (releaseHydratedTranscript initial.stateGeneration forked)
                        cold = fst (releaseHydratedTranscript initial.stateGeneration oneLeft)
                    oneLeft.stateTranscript `shouldBe`
                        ResidentTranscript items (HydratedResident "fork" 1)
                    cold `shouldBe` initial { stateTranscript = ColdTranscript "fork" }

        it "defers explicit eviction until all readers release, without changing generation" do
            let hydrated = hydratedTranscript "original" items initial
                (pending, evicted) = evictTranscript initial.stateGeneration "persisted" hydrated
                (cold, ()) = releaseHydratedTranscript initial.stateGeneration pending
            evicted `shouldBe` False
            pending.stateTranscript `shouldBe`
                ResidentTranscript items (HydratedResident "persisted" 1)
            cold `shouldBe` initial { stateTranscript = ColdTranscript "persisted" }
            evictTranscript initial.stateGeneration "ignored" cold `shouldBe` (cold, False)

        it "ignores delayed eviction and release after a newer commit and hydration" do
            let old = hydratedTranscript "original" items initial
                (committed, generation) = commitTranscript [messageItem "new"] old
                (cold, evicted) = evictTranscript generation "new" committed
                current = hydratedTranscript "new" [messageItem "new"] cold
            evicted `shouldBe` True
            evictTranscript old.stateGeneration "stale" current `shouldBe` (current, False)
            releaseHydratedTranscript old.stateGeneration current `shouldBe` (current, ())
            current.stateAttachments `shouldBe` initial.stateAttachments
            current.stateContinuation `shouldBe` Nothing
            fst (releaseHydratedTranscript generation current) `shouldBe` cold

        it "reset invalidates old leases even after a subsequent cold acquisition" do
            let old = hydratedTranscript "original" items initial
                reset = resetState old
                (cold, _) = evictTranscript reset.stateGeneration "empty" reset
                current = hydratedTranscript "empty" [] cold
            reset.stateGeneration `shouldBe` TranscriptGeneration 1
            reset.stateAttachments `shouldBe` []
            reset.stateContinuation `shouldBe` Nothing
            reset.stateTranscript `shouldBe` ResidentTranscript [] CommittedResident
            evictTranscript old.stateGeneration "stale" current `shouldBe` (current, False)
            releaseHydratedTranscript old.stateGeneration current `shouldBe` (current, ())
            fst (releaseHydratedTranscript reset.stateGeneration current) `shouldBe` cold

        it "assigns authoritative revisions across backend commits, replacement and reset" do
            let candidate = BackendSnapshot items (BackendRevision 999)
                    (Just (BackendContinuation "claude" "token"))
                (first, snapshot1) = commitBackendState candidate initial
                (second, snapshot2) = commitBackendState
                    candidate { backendRevision = BackendRevision 0 } first
                (replaced, generation) = commitTranscript [] second
                reset = resetState replaced
            snapshot1.backendRevision `shouldBe` BackendRevision 1
            snapshot2.backendRevision `shouldBe` BackendRevision 2
            snapshotFromState second items `shouldBe` snapshot2
            generation `shouldBe` TranscriptGeneration 3
            replaced.stateContinuation `shouldBe` Nothing
            replaced.stateAttachments `shouldBe` initial.stateAttachments
            reset.stateGeneration `shouldBe` TranscriptGeneration 4
            reset.stateAttachments `shouldBe` []
            first.stateAttachments `shouldBe` initial.stateAttachments

        it "leaves committed residents alone when acquiring, releasing or retargeting" do
            let (committed, _) = commitTranscript items initial
            retargetCheckpoint "ignored" committed `shouldBe` committed
            releaseHydratedTranscript committed.stateGeneration committed `shouldBe` (committed, ())
            case acquireTranscript committed of
                LoadCheckpoint{} -> expectationFailure "committed transcript needs no load"
                Acquired unchanged release shared -> do
                    unchanged `shouldBe` committed
                    release `shouldBe` False
                    shared `shouldBe` items

initial :: ConversationState Text
initial = ConversationState
    { stateGeneration = TranscriptGeneration 0
    , stateTranscript = ColdTranscript "original"
    , stateContinuation = openAiContinuation (Just "response")
    , stateAttachments = [ImageAttachment "image/png" "image"]
    }

items :: [ResponseItem]
items = [messageItem "original"]

messageItem :: Text -> ResponseItem
messageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }
