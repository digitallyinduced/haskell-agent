module Agent.CLI.SessionHistorySpec (spec) where

import Agent.CLI.Session.History
    ( LiveConversation(..)
    , resetLiveConversationState
    , resetLiveConversationWith
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Loop (TurnInput(..))
import qualified Data.ByteString.Char8 as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Agent.Tools.PlanMode (newPlanModeEnv)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = describe "LiveConversation" do
    it "resets all coordinated fields together" do
        let state = LiveConversation
                { livePreviousResponseId = Just "resp-1"
                , liveTranscript = turnInputsToItems [UserMessage "hello"]
                , liveAttachments =
                    [ImageAttachment "image/png" (ByteString.pack "bytes")]
                }
        resetLiveConversationState state `shouldBe`
            LiveConversation
                { livePreviousResponseId = Nothing
                , liveTranscript = []
                , liveAttachments = []
                }

    it "is idempotent" do
        let state = LiveConversation
                { livePreviousResponseId = Just "resp-1"
                , liveTranscript = []
                , liveAttachments = []
                }
            reset = resetLiveConversationState state
        resetLiveConversationState reset `shouldBe` reset

    it "clears provider-owned transcript state at the same reset boundary" do
        let transcript = turnInputsToItems [UserMessage "old context"]
        conversationRef <- newIORef LiveConversation
            { livePreviousResponseId = Just "resp-1"
            , liveTranscript = transcript
            , liveAttachments = []
            }
        providerTranscriptRef <- newIORef transcript
        planMode <- newPlanModeEnv (unsafeEncodeUtf "/tmp/session-reset") Nothing
        resetLiveConversationWith
            (writeIORef providerTranscriptRef [])
            conversationRef
            planMode
        readIORef providerTranscriptRef `shouldReturn` []
        readIORef conversationRef `shouldReturn`
            LiveConversation
                { livePreviousResponseId = Nothing
                , liveTranscript = []
                , liveAttachments = []
                }
