module Agent.CLI.SessionTitleSpec (spec) where

import Agent.CLI.SessionTitle
import Agent.Runtime.Session.TitleModel (TitleModelResolution(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyTurnOutput
    )
import Agent.Responses.Types
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
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

    describe "shouldRequestSessionTitle" do
        it "requests titles after the complete first, third, and sixth turns" do
            shouldRequestSessionTitle 1 0 `shouldBe` True
            shouldRequestSessionTitle 2 0 `shouldBe` False
            shouldRequestSessionTitle 3 0 `shouldBe` True
            shouldRequestSessionTitle 6 1 `shouldBe` True

        it "does not repeat completed refresh milestones" do
            shouldRequestSessionTitle 1 1 `shouldBe` False
            shouldRequestSessionTitle 3 1 `shouldBe` False
            shouldRequestSessionTitle 6 2 `shouldBe` False

    it "runs a private tool-free title request and returns a tagged result" do
        seenParams <- newIORef Nothing
        seenInputs <- newIORef []
        notified <- newEmptyMVar
        let sessionReasoning = ReasoningConfig
                { context = Nothing
                , effort = Just "medium"
                , generateSummary = Nothing
                , reasoningMode = Nothing
                , summary = Nothing
                }
            baseParams =
                case defaultResponseCreateParams :: ResponseCreateParams of
                    ResponseCreateParams{..} ->
                        ResponseCreateParams
                            { reasoning = Just sessionReasoning
                            , ..
                            }
        let backendFactory privateParams =
                Backend \state _ inputs _ -> do
                    writeIORef seenParams (Just privateParams)
                    writeIORef seenInputs inputs
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Auth race cleanup")
                        , backendState = state
                        }
        withSessionTitleManager backendFactory (pure baseParams) (pure testTitleModel) (putMVar notified) \manager -> do
            requestSessionTitle manager "session-1" 3
                "User:\nFix auth\n\nAssistant:\nI found the race"
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
        sent.model `shouldBe` Just "title-model"
        fmap (.effort) sent.reasoning `shouldBe` Just (Just "low")
        [UserMessage titleInput] <- readIORef seenInputs
        titleInput `shouldSatisfy` Text.isInfixOf "User:\nFix auth"
        titleInput `shouldSatisfy`
            Text.isInfixOf "Assistant:\nI found the race"

    it "forwards high reasoning effort for cheap OpenAI title models" do
        seenParams <- newIORef Nothing
        let backendFactory privateParams =
                Backend \state _ _ _ -> do
                    writeIORef seenParams (Just privateParams)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Auth race cleanup")
                        , backendState = state
                        }
            lunaTitle = testTitleModel
                { titleModelId = "gpt-5.6-luna"
                , titleWireModelId = "gpt-5.6-luna"
                , titleReasoningEffort = "high"
                }
        withSessionTitleManager backendFactory (pure defaultResponseCreateParams)
            (pure lunaTitle) (\_ -> pure ()) \manager -> do
            requestSessionTitle manager "session-1" 1 "conversation"
            _ <- waitForResults manager 100
            pure ()
        Just sent <- readIORef seenParams
        sent.model `shouldBe` Just "gpt-5.6-luna"
        fmap (.effort) sent.reasoning `shouldBe` Just (Just "high")

    it "caps the title prompt for a 4K-token on-device window" do
        seenInputs <- newIORef []
        let backendFactory _ =
                Backend \state _ inputs _ -> do
                    writeIORef seenInputs inputs
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Local title")
                        , backendState = state
                        }
            smallWindow = testTitleModel { titleContextWindow = Just 4096 }
        withSessionTitleManager backendFactory (pure defaultResponseCreateParams)
            (pure smallWindow) (\_ -> pure ()) \manager -> do
            requestSessionTitle manager "session-1" 1
                (Text.replicate 5000 "abcdefghij")
            _ <- waitForResults manager 100
            [UserMessage titleInput] <- readIORef seenInputs
            Text.length titleInput `shouldSatisfy` (< 3000)

    it "drops a stale result after a manual rename invalidates generation" do
        started <- newEmptyMVar
        release <- newEmptyMVar
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
        withSessionTitleManager backendFactory (pure defaultResponseCreateParams) (pure testTitleModel) (\_ -> pure ()) \manager -> do
            requestSessionTitle manager "session-1" 1 "conversation"
            takeMVar started
            invalidateSessionTitles manager "session-1"
            putMVar release ()
            threadDelay 50000
            takeSessionTitleResults manager `shouldReturn` []

    it "waits for an in-flight title before shutdown" do
        let backendFactory _ =
                Backend \state _ _ _ -> do
                    threadDelay 20000
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (Just "Finished title")
                        , backendState = state
                        }
        withSessionTitleManager backendFactory (pure defaultResponseCreateParams) (pure testTitleModel) (\_ -> pure ()) \manager -> do
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

    it "retries a failed title request once before reporting its outcome" do
        attempts <- newIORef (0 :: Int)
        notified <- newEmptyMVar
        let backendFactory _ =
                Backend \state _ _ _ -> do
                    attempt <- atomicModifyIORef' attempts \count ->
                        let next = count + 1
                        in (next, next)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" []
                                (if attempt == 1
                                    then Nothing
                                    else Just "Recovered title")
                        , backendState = state
                        }
        withSessionTitleManager backendFactory (pure defaultResponseCreateParams) (pure testTitleModel) (putMVar notified) \manager -> do
            requestSessionTitle manager "session-1" 3 "conversation"
            takeMVar notified
                `shouldReturn`
                    SessionTitleGenerated SessionTitleResult
                        { resultSessionId = "session-1"
                        , resultMilestone = 3
                        , resultTitle = "Recovered title"
                        , resultGeneration = 0
                        }
        readIORef attempts `shouldReturn` 2

    it "reports provider failures instead of silently dropping them" do
        attempts <- newIORef (0 :: Int)
        notified <- newEmptyMVar
        let backendFactory _ =
                Backend \state _ _ _ -> do
                    modifyIORef' attempts (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "title-response" [] Nothing
                        , backendState = state
                        }
        withSessionTitleManager backendFactory (pure defaultResponseCreateParams) (pure testTitleModel) (putMVar notified) \manager -> do
            requestSessionTitle manager "session-1" 3 "conversation"
            takeMVar notified
                `shouldReturn`
                    SessionTitleFailed SessionTitleFailure
                        { failureSessionId = "session-1"
                        , failureMilestone = 3
                        , failureMessage = "provider returned no title text"
                        , failureGeneration = 0
                        }
        readIORef attempts `shouldReturn` 2

testTitleModel :: TitleModelResolution
testTitleModel = TitleModelResolution
    { titleModelId = "title-model"
    , titleWireModelId = "title-model"
    , titleContextWindow = Nothing
    , titleReasoningEffort = "low"
    , titlePinned = False
    }

waitForResults :: SessionTitleManager -> Int -> IO [SessionTitleResult]
waitForResults manager attempts
    | attempts <= 0 = pure []
    | otherwise = do
        results <- takeSessionTitleResults manager
        if null results
            then threadDelay 10000 >> waitForResults manager (attempts - 1)
            else pure results
