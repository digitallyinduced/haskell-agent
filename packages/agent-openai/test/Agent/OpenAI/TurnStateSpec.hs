module Agent.OpenAI.TurnStateSpec (spec) where

import Agent.OpenAI.RequestIdentity
    ( CodexRequestIdentity(..)
    , CodexRequestKind(..)
    , isHeaderSafeIdentifier
    )
import Agent.OpenAI.TurnState
import Test.Hspec

spec :: Spec
spec = describe "Agent.OpenAI.TurnState" do
    describe "resolveCodexRequestIdentity" do
        it "shares one turn identifier across continuations and retries" do
            turnState <- newCodexTurnState
            first <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "session-1")
            continuation <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "session-1")
            compaction <- resolveCodexRequestIdentity turnState CodexCompactionRequest (Just "session-1")
            continuation.turnId `shouldBe` first.turnId
            continuation.turnStartedAtUnixMs `shouldBe` first.turnStartedAtUnixMs
            compaction.turnId `shouldBe` first.turnId
            compaction.requestKind `shouldBe` CodexCompactionRequest
            readCodexTurnIdentifier turnState `shouldReturn` Just first.turnId

        it "mints a new turn identifier after the turn is reset" do
            turnState <- newCodexTurnState
            first <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "session-1")
            recordCodexTurnState turnState "ts-1"
            resetCodexTurnState turnState
            readCodexTurnIdentifier turnState `shouldReturn` Nothing
            readCodexTurnState turnState `shouldReturn` Nothing
            second <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "session-1")
            second.turnId `shouldNotBe` first.turnId
            second.threadId `shouldBe` first.threadId
            second.windowNumber `shouldBe` first.windowNumber
            second.contextWindowId `shouldBe` first.contextWindowId

        it "uses the prompt cache key as the session and thread identity" do
            turnState <- newCodexTurnState
            identity <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "2026-09-14-67ed6793")
            identity.sessionId `shouldBe` "2026-09-14-67ed6793"
            identity.threadId `shouldBe` "2026-09-14-67ed6793"
            identity.windowNumber `shouldBe` 0

        it "falls back to a stable generated thread identity without a usable cache key" do
            turnState <- newCodexTurnState
            missing <- resolveCodexRequestIdentity turnState CodexTurnRequest Nothing
            unsafe <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "not header safe")
            missing.threadId `shouldSatisfy` isHeaderSafeIdentifier
            unsafe.threadId `shouldBe` missing.threadId
            missing.sessionId `shouldBe` missing.threadId

    describe "advanceCodexContextWindow" do
        it "moves later requests to the next window without ending the turn" do
            turnState <- newCodexTurnState
            before <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "session-1")
            recordCodexTurnState turnState "ts-1"
            advanceCodexContextWindow turnState
            after <- resolveCodexRequestIdentity turnState CodexTurnRequest (Just "session-1")
            after.windowNumber `shouldBe` before.windowNumber + 1
            after.contextWindowId `shouldNotBe` before.contextWindowId
            after.turnId `shouldBe` before.turnId
            readCodexTurnState turnState `shouldReturn` Just "ts-1"
            (generation, contextWindowId) <- readCodexContextWindow turnState
            generation `shouldBe` 1
            contextWindowId `shouldBe` after.contextWindowId

        it "survives a turn reset" do
            turnState <- newCodexTurnState
            advanceCodexContextWindow turnState
            resetCodexTurnState turnState
            fmap fst (readCodexContextWindow turnState) `shouldReturn` 1

    describe "copyCodexTurnRecord" do
        it "carries the turn, routing token, and window onto another state" do
            source <- newCodexTurnState
            destination <- newCodexTurnState
            original <- resolveCodexRequestIdentity source CodexTurnRequest Nothing
            recordCodexTurnState source "ts-source"
            advanceCodexContextWindow source
            copyCodexTurnRecord source destination
            copied <- resolveCodexRequestIdentity destination CodexTurnRequest Nothing
            copied.turnId `shouldBe` original.turnId
            copied.threadId `shouldBe` original.threadId
            copied.windowNumber `shouldBe` 1
            readCodexTurnState destination `shouldReturn` Just "ts-source"

    describe "transientCodexRequestIdentity" do
        it "attributes standalone requests as turns of their own" do
            first <- transientCodexRequestIdentity CodexTurnRequest (Just "session-1")
            second <- transientCodexRequestIdentity CodexCompactionRequest Nothing
            first.threadId `shouldBe` "session-1"
            first.windowNumber `shouldBe` 0
            first.turnId `shouldNotBe` second.turnId
            second.threadId `shouldSatisfy` isHeaderSafeIdentifier
            second.requestKind `shouldBe` CodexCompactionRequest
