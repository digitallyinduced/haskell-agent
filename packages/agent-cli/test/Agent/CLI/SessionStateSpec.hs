module Agent.CLI.SessionStateSpec (spec) where

import Agent.CLI.SessionState
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
            writeIORef state.sessionAttachments [image]
            writeIORef state.sessionPreviewId 42

            -- Replacing a backend reuses this same provider-independent state.
            readIORef state.sessionDraft `shouldReturn` "unfinished prompt"
            readIORef state.sessionAttachments `shouldReturn` [image]
            readIORef state.sessionPreviewId `shouldReturn` 42

        it "starts a new user session with empty composer state" do
            state <- newSessionState
            readIORef state.sessionDraft `shouldReturn` ""
            readIORef state.sessionAttachments `shouldReturn` []
            readIORef state.sessionPreviewId `shouldReturn` 1
