module Agent.CLI.SessionTitleSpec (spec) where

import Agent.CLI.SessionTitle
import Agent.Loop (Backend(..), BackendResult(..), emptyTurnOutput)
import Agent.Responses.Types
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.SessionTitle" do
    describe "cleanGeneratedTitle" do
        it "normalizes labels, quotes, whitespace, and extra lines" do
            cleanGeneratedTitle "Title: \"  Fix   auth races  \"\nExplanation"
                `shouldBe` Just "Fix auth races"

        it "rejects empty output and caps runaway titles" do
            cleanGeneratedTitle "  " `shouldBe` Nothing
            fmap Text.length (cleanGeneratedTitle (Text.replicate 100 "a"))
                `shouldBe` Just 80

    it "runs a private tool-free title request and returns a tagged result" do
        seenParams <- newIORef Nothing
        notified <- newEmptyMVar
        let sessionReasoning = ReasoningConfig
                { context = Nothing
                , effort = Just "medium"
                , generateSummary = Nothing
                , reasoningMode = Nothing
                , summary = Nothing
                , extraFields = KeyMap.empty
                }
            baseParams =
                case defaultResponseCreateParams :: ResponseCreateParams of
                    ResponseCreateParams{..} ->
                        ResponseCreateParams
                            { reasoning = Just sessionReasoning
                            , ..
                            }
        paramsRef <- newIORef baseParams
        let backendFactory privateParams =
                Backend \state _ _ _ -> do
                    writeIORef seenParams . Just =<< readIORef privateParams
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Auth race cleanup")
                        , backendState = state
                        }
        withSessionTitleManager backendFactory paramsRef (putMVar notified) \manager -> do
            requestSessionTitle manager "session-1" 3 "conversation"
            results <- waitForResults manager 100
            let expected = SessionTitleResult
                    { resultSessionId = "session-1"
                    , resultMilestone = 3
                    , resultTitle = "Auth race cleanup"
                    , resultGeneration = 0
                    }
            results `shouldBe` [expected]
            takeMVar notified `shouldReturn` SessionTitleGenerated expected
        Just sent <- readIORef seenParams
        sent.tools `shouldBe` Just []
        sent.parallelToolCalls `shouldBe` Just False
        sent.maxOutputTokens `shouldBe` Nothing
        sent.reasoning `shouldBe` Just sessionReasoning

    it "drops a stale result after a manual rename invalidates generation" do
        started <- newEmptyMVar
        release <- newEmptyMVar
        paramsRef <- newIORef (defaultResponseCreateParams :: ResponseCreateParams)
        let backendFactory _ =
                Backend \state _ _ _ -> do
                    putMVar started ()
                    takeMVar release
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Stale title")
                        , backendState = state
                        }
        withSessionTitleManager backendFactory paramsRef (\_ -> pure ()) \manager -> do
            requestSessionTitle manager "session-1" 1 "conversation"
            takeMVar started
            invalidateSessionTitles manager "session-1"
            putMVar release ()
            threadDelay 50000
            takeSessionTitleResults manager `shouldReturn` []

    it "waits for an in-flight title before shutdown" do
        paramsRef <- newIORef (defaultResponseCreateParams :: ResponseCreateParams)
        let backendFactory _ =
                Backend \state _ _ _ -> do
                    threadDelay 20000
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Finished title")
                        , backendState = state
                        }
        withSessionTitleManager backendFactory paramsRef (\_ -> pure ()) \manager -> do
            requestSessionTitle manager "session-1" 6 "conversation"
            waitForSessionTitleResults 1000000 manager
                `shouldReturn`
                    [ SessionTitleResult
                        { resultSessionId = "session-1"
                        , resultMilestone = 6
                        , resultTitle = "Finished title"
                        , resultGeneration = 0
                        }
                    ]

    it "reports provider failures instead of silently dropping them" do
        notified <- newEmptyMVar
        paramsRef <- newIORef (defaultResponseCreateParams :: ResponseCreateParams)
        let backendFactory _ =
                Backend \state _ _ _ ->
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" [] Nothing
                        , backendState = state
                        }
        withSessionTitleManager backendFactory paramsRef (putMVar notified) \manager -> do
            requestSessionTitle manager "session-1" 3 "conversation"
            takeMVar notified
                `shouldReturn`
                    SessionTitleFailed SessionTitleFailure
                        { failureSessionId = "session-1"
                        , failureMilestone = 3
                        , failureMessage = "provider returned no title text"
                        , failureGeneration = 0
                        }

waitForResults :: SessionTitleManager -> Int -> IO [SessionTitleResult]
waitForResults manager attempts
    | attempts <= 0 = pure []
    | otherwise = do
        results <- takeSessionTitleResults manager
        if null results
            then threadDelay 10000 >> waitForResults manager (attempts - 1)
            else pure results
