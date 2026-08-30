module Agent.CLI.SessionStateSpec (spec) where

import Agent.CLI.SessionState
import Agent.CLI.Session.History
    ( modifyLiveAttachments
    , readLiveAttachments
    )
import Agent.Loop (ImageAttachment(..))
import Data.IORef (readIORef, writeIORef)
import Test.Hspec

spec :: Spec
spec =
    describe "SessionState" do
        it "keeps composer state alive independently of backend replacement" do
            state <- newSessionState
            let image = ImageAttachment "image/png" "png-bytes"
            writeIORef state.sessionDraft "unfinished prompt"
            modifyLiveAttachments state.sessionConversation (\_ -> ([image], ()))
            writeIORef state.sessionPreviewId 42

            -- Replacing a backend reuses this same provider-independent state.
            readIORef state.sessionDraft `shouldReturn` "unfinished prompt"
            writeIORef state.sessionInitialPrompt (Just "fork directive")
            readIORef state.sessionInitialPrompt
                `shouldReturn` Just "fork directive"
            readLiveAttachments state.sessionConversation `shouldReturn` [image]
            readIORef state.sessionPreviewId `shouldReturn` 42

        it "removes only the selected pending image" do
            let first = ImageAttachment "image/png" "first"
                second = ImageAttachment "image/jpeg" "second"
                pending = [first, second]
            removeImageAttachmentAt 0 pending
                `shouldBe` ([second], True)
            removeImageAttachmentAt 1 pending
                `shouldBe` ([first], True)
            removeImageAttachmentAt (-1) pending
                `shouldBe` (pending, False)
            removeImageAttachmentAt 2 pending
                `shouldBe` (pending, False)

        it "starts a new user session with empty composer state" do
            state <- newSessionState
            readIORef state.sessionDraft `shouldReturn` ""
            readIORef state.sessionInitialPrompt `shouldReturn` Nothing
            readLiveAttachments state.sessionConversation `shouldReturn` []
            readIORef state.sessionPreviewId `shouldReturn` 1
