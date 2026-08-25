module Agent.CLI.CompactionSpec (spec) where

import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , autoCompactOpenAiBackendWith
    , autoCompactOpenAiBackendWithApi
    , autoCompactOpenAiBackendWithSender
    , autoCompactOpenAiBackendWithThreshold
    , codexAutoCompactTokenLimit
    , compactOpenAIWith
    , installCompactOutcome
    , runProviderCompact
    , runProviderCompactWith
    )
import Agent.CLI.Connectivity (withConnectionRecoveryUsing)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
import Agent.OpenAI.Compaction
    ( compactionTriggerItem
    , hasCompactionCheckpoint
    , userTextItem
    )
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    )
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Agent.Provider
    ( BillingMode(..)
    , Provider(..)
    , tokenProvider
    )
import Control.Exception
    ( AsyncException(..)
    , MaskingState(..)
    , getMaskingState
    , throwIO
    , try
    )
import Data.IORef
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "runProviderCompact" do
        it "reports a provider-specific error when credentials are unavailable" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            runProviderCompact OpenAIProvider Nothing params transcript Nothing
                `shouldReturn` Left "openai compact requires a token provider"
            runProviderCompact XAIProvider Nothing params transcript Nothing
                `shouldReturn` Left "xai compact requires a token provider"
            runProviderCompact OpenRouterProvider Nothing params transcript Nothing
                `shouldReturn` Left "openrouter compact requires a token provider"

        it "short-circuits an empty transcript before using credentials" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "empty compaction unexpectedly requested credentials"
            runProviderCompact OpenAIProvider (Just provider) params transcript Nothing
                `shouldReturn` Left "nothing to compact"

        it "uses an injected OpenAI sender and records its response usage" do
            params <- newIORef defaultResponseCreateParams
            let history = [userTextItem "old context"]
            transcript <- newIORef history
            requests <- newIORef []
            recordedUsage <- newIORef []
            let sender request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                record usage =
                    modifyIORef' recordedUsage (<> [usage])
            result <-
                runProviderCompactWith
                    (Just sender)
                    record
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` either (const False) (const True)
            map requestItems <$> readIORef requests
                `shouldReturn` [history <> [compactionTriggerItem]]
            readIORef recordedUsage `shouldReturn` [compactionUsage]

        it "records completed-response usage with asynchronous exceptions masked" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [userTextItem "old context"]
            senderMasking <- newIORef MaskedUninterruptible
            recorderMasking <- newIORef Unmasked
            result <-
                runProviderCompactWith
                    (Just \_ -> do
                        getMaskingState >>= writeIORef senderMasking
                        pure (Right remoteCompactionResponse))
                    (\_ -> getMaskingState >>= writeIORef recorderMasking)
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` either (const False) (const True)
            readIORef senderMasking `shouldReturn` Unmasked
            readIORef recorderMasking `shouldReturn` MaskedInterruptible

        it "records local-summary response usage when summary text is missing" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [userTextItem "old context"]
            recordedUsage <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \_ -> pure (Right responseWithoutCompaction))
                    (\usage -> modifyIORef' recordedUsage (<> [usage]))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    (Just "focus")
            result `shouldSatisfy` \case
                Left message ->
                    "compaction produced no summary text"
                        `Text.isInfixOf` message
                Right _ -> False
            readIORef recordedUsage `shouldReturn` [compactionUsage]

        it "does not record usage for a transport failure" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [userTextItem "old context"]
            recordedUsage <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \_ -> pure (Left (ConnectionError "offline")))
                    (\usage -> modifyIORef' recordedUsage (<> [usage]))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` \case
                Left message -> "offline" `Text.isInfixOf` message
                Right _ -> False
            readIORef recordedUsage `shouldReturn` []

        it "records completed-response usage even when the checkpoint is invalid" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [userTextItem "old context"]
            recordedUsage <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \_ -> pure (Right responseWithoutCompaction))
                    (\usage -> modifyIORef' recordedUsage (<> [usage]))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` \case
                Left message ->
                    "expected exactly one compaction" `Text.isInfixOf` message
                Right _ -> False
            readIORef recordedUsage `shouldReturn` [compactionUsage]

    describe "installCompactOutcome" do
        it "clears the previous response id with transcript and token state" do
            previous <- newIORef (Just "resp-old")
            transcript <- newIORef [userTextItem "old context"]
            contextState <- newIORef (Just (100, 1))
            actionMasking <- newIORef MaskedUninterruptible
            let compactedHistory = [userTextItem "compacted context"]
                outcome = CompactOutcome
                    { compactBeforeTokens = 100
                    , compactAfterTokens = 12
                    , compactHistory = compactedHistory
                    , compactSummary = "checkpoint"
                    }
                compactAction _ = do
                    getMaskingState >>= writeIORef actionMasking
                    pure (Right outcome)
            installCompactOutcome
                previous
                transcript
                (Just contextState)
                compactAction
                Nothing
                `shouldReturn` Right outcome
            readIORef actionMasking `shouldReturn` Unmasked
            readIORef previous `shouldReturn` Nothing
            readIORef transcript `shouldReturn` compactedHistory
            readIORef contextState `shouldReturn`
                Just (outcome.compactAfterTokens, length compactedHistory)

        it "leaves live state unchanged when compaction fails" do
            let oldHistory = [userTextItem "old context"]
                oldContextState = Just (100, length oldHistory)
            previous <- newIORef (Just "resp-old")
            transcript <- newIORef oldHistory
            contextState <- newIORef oldContextState
            installCompactOutcome
                previous
                transcript
                (Just contextState)
                (const (pure (Left "failed")))
                Nothing
                `shouldReturn` Left "failed"
            readIORef previous `shouldReturn` Just "resp-old"
            readIORef transcript `shouldReturn` oldHistory
            readIORef contextState `shouldReturn` oldContextState

    describe "compactOpenAIWith" do
        it "uses remote compaction v2 on normal Responses" do
            requests <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "remote compaction unexpectedly requested credentials"
                send _ request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                history = [userTextItem "old context"]
                params = defaultResponseCreateParams
                    { instructions = Just "keep these instructions"
                    , tools = Just []
                    , stream = Just False
                    }
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    params
                    history
                    100
                    Nothing
            case result of
                Left err -> expectationFailure (show err)
                Right outcome -> do
                    outcome.compactSummary
                        `shouldBe` "Context compacted remotely."
                    outcome.compactHistory
                        `shouldSatisfy` hasCompactionCheckpoint
            seen <- readIORef requests
            length seen `shouldBe` 1
            map (.instructions) seen `shouldBe` [Just "keep these instructions"]
            map (.tools) seen `shouldBe` [Just []]
            map (.parallelToolCalls) seen `shouldBe` [Just True]
            map (.previousResponseId) seen `shouldBe` [Nothing]
            map (.store) seen `shouldBe` [Just False]
            map (.stream) seen `shouldBe` [Just True]
            map (.toolChoice) seen
                `shouldBe` [Just (ToolChoiceMode ToolChoiceAuto)]
            map requestItems seen
                `shouldBe` [history <> [compactionTriggerItem]]

        it "keeps focused manual compaction on local summarization" do
            requests <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "local summarization unexpectedly requested credentials"
                send _ request = do
                    modifyIORef' requests (<> [request])
                    pure (Right (summaryResponse "local summary"))
                history = [userTextItem "old context"]
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    defaultResponseCreateParams
                    history
                    100
                    (Just "focus on auth")
            case result of
                Left err -> expectationFailure (show err)
                Right outcome -> do
                    outcome.compactSummary `shouldBe` "local summary"
                    outcome.compactHistory
                        `shouldSatisfy` hasCompactionCheckpoint
            seen <- readIORef requests
            map (.tools) seen `shouldBe` [Nothing]
            map (.parallelToolCalls) seen `shouldBe` [Just False]
            map (.stream) seen `shouldBe` [Just True]

        it "returns friendly provider errors from manual compaction" do
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "compaction unexpectedly requested credentials"
                send _ _ =
                    pure $ Left $
                        ProviderError UsageLimitReached
                            "quota exhausted"
                            (Just 120)
                history = [userTextItem "old context"]
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    defaultResponseCreateParams
                    history
                    100
                    Nothing
            case result of
                Left err -> do
                    err `shouldSatisfy`
                        Text.isInfixOf "Usage limit reached"
                    err `shouldSatisfy`
                        Text.isInfixOf "Try again in 2m"
                    err `shouldNotSatisfy`
                        Text.isInfixOf "ProviderError"
                Right _ ->
                    expectationFailure "expected compaction to fail"

    describe "autoCompactOpenAiBackendWith" do
        it "compacts at a configured threshold below the model default" do
            let history = [userTextItem "old"]
                threshold = 20
                tokens = tokenProvider SubscriptionBilled \_ ->
                    pure (Left (ConnectionError "configured threshold fired"))
                params =
                    withModel
                        (Just "gpt-5.6-luna")
                        defaultResponseCreateParams
                base = Backend \_ _ _ _ ->
                    error "configured compaction threshold should run first"
            contextState <- newIORef
                (Just (threshold, length history))
            let backend =
                    autoCompactOpenAiBackendWithThreshold
                        (Just threshold)
                        tokens
                        (pure params)
                        contextState
                        base
            backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
                `shouldReturn`
                    Left (ConnectionError
                        "automatic compaction failed: configured threshold fired")

        it "compacts before the next request at the Codex token limit" do
            let oldHistory = [userTextItem "old"]
                compactedHistory = [userTextItem "compacted"]
            contextState <- newIORef
                (Just (codexAutoCompactTokenLimit, length oldHistory))
            compactCalls <- newIORef (0 :: Int)
            seenPrevious <- newIORef []
            events <- newIORef []
            let compactAction = do
                    modifyIORef' compactCalls (+ 1)
                    pure $ Right CompactOutcome
                        { compactBeforeTokens = codexAutoCompactTokenLimit
                        , compactAfterTokens = 10
                        , compactHistory = compactedHistory
                        , compactSummary = "checkpoint"
                        }
                base = Backend \state previous _ _ -> do
                    modifyIORef' seenPrevious (<> [previous])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWith
                        compactAction contextState base
            result <- backend.submitTurn oldHistory (Just "resp-old")
                [UserMessage "new"]
                (\event -> modifyIORef' events (<> [event]))
            result `shouldSatisfy` either (const False) (const True)
            fmap (.backendState) result `shouldBe` Right compactedHistory
            readIORef compactCalls `shouldReturn` 1
            readIORef seenPrevious `shouldReturn` [Nothing]
            readIORef contextState `shouldReturn` Nothing
            readIORef events `shouldReturn`
                [ActivityUpdated "Compacting context…"]

        it "records active-session compaction usage before a failed continuation" do
            let history = [userTextItem "old"]
                threshold = 20
                oldContextState = Just (threshold - 2, length history)
            contextState <- newIORef oldContextState
            requests <- newIORef []
            recordedUsage <- newIORef []
            let sender request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                base = Backend \_ _ _ _ ->
                    pure (Left (ConnectionError "continuation failed"))
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (\usage -> modifyIORef' recordedUsage (<> [usage]))
                        (pure defaultResponseCreateParams)
                        contextState
                        base
            backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
                `shouldReturn` Left (ConnectionError "continuation failed")
            map requestItems <$> readIORef requests
                `shouldReturn` [history <> [compactionTriggerItem]]
            readIORef recordedUsage `shouldReturn` [compactionUsage]
            readIORef contextState `shouldReturn` oldContextState

        it "rolls back compacted state when the continuation is cancelled" do
            let history = [userTextItem "old"]
                threshold = 20
                oldContextState = Just (threshold, length history)
            contextState <- newIORef oldContextState
            continuationMasking <- newIORef MaskedUninterruptible
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \_ _ _ _ -> do
                    getMaskingState >>= writeIORef continuationMasking
                    throwIO UserInterrupt
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        contextState
                        base
                submit =
                    backend.submitTurn history Nothing
                        [UserMessage "new"]
                        (const (pure ()))
            result <- try submit
                :: IO
                    (Either AsyncException
                        (Either ApiError BackendResult))
            result `shouldBe` Left UserInterrupt
            readIORef continuationMasking `shouldReturn` Unmasked
            readIORef contextState `shouldReturn` oldContextState

        it "does not rerun compaction while its continuation reconnects" do
            let history = [userTextItem "old"]
                threshold = 20
            contextState <- newIORef
                (Just (threshold, length history))
            compactCalls <- newIORef (0 :: Int)
            continuationCalls <- newIORef (0 :: Int)
            recordedUsage <- newIORef []
            waits <- newIORef []
            seenPrevious <- newIORef []
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                continuation =
                    Backend \state previous _inputs _onEvent -> do
                        modifyIORef' seenPrevious (<> [previous])
                        attempt <- atomicModifyIORef' continuationCalls
                            \n -> (n + 1, n + 1)
                        pure $
                            if attempt == 1
                                then Left (ConnectionError "offline")
                                else successful state TurnOutput
                                    { responseId = "resp-new"
                                    , toolCalls = []
                                    , assistantText = Just "ok"
                                    , tokenUsage = TokenUsage 20 5 0
                                    }
                reconnectingContinuation =
                    withConnectionRecoveryUsing
                        (\delay -> modifyIORef' waits (<> [delay]))
                        continuation
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (\usage -> modifyIORef' recordedUsage (<> [usage]))
                        (pure defaultResponseCreateParams)
                        contextState
                        reconnectingContinuation
            result <- backend.submitTurn history (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef continuationCalls `shouldReturn` 2
            readIORef waits `shouldReturn` [1_000_000]
            readIORef seenPrevious `shouldReturn` [Nothing, Nothing]
            readIORef recordedUsage `shouldReturn` [compactionUsage]

        it "compacts when a pending tool output crosses the limit" do
            let danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = "shell_command"
                    , namespace = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    , extraFields = mempty
                    }
                oldHistory = [userTextItem "run it", danglingCall]
                toolOutputText = Text.replicate 400 "x"
                toolResult = ToolCallResult
                    { callId = "call-1"
                    , output = toolOutputText
                    , callKind = FunctionCallKind
                    }
            contextState <- newIORef
                (Just (codexAutoCompactTokenLimit - 10, length oldHistory))
            compactCalls <- newIORef (0 :: Int)
            recordedUsage <- newIORef []
            historyAtCompact <- newIORef []
            seenPrevious <- newIORef []
            seenInputs <- newIORef []
            let sender request = do
                    modifyIORef' compactCalls (+ 1)
                    writeIORef historyAtCompact
                        (init (requestItems request))
                    pure (Right remoteCompactionResponse)
                base = Backend \state previous inputs _ -> do
                    modifyIORef' seenPrevious (<> [previous])
                    modifyIORef' seenInputs (<> [inputs])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just codexAutoCompactTokenLimit)
                        sender
                        (\usage -> modifyIORef' recordedUsage (<> [usage]))
                        (pure defaultResponseCreateParams)
                        contextState
                        base
                inputs = [CompletedTool toolResult]
            result <- backend.submitTurn oldHistory (Just "resp-tool") inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            compactInput <- readIORef historyAtCompact
            compactInput `shouldSatisfy` \items ->
                case reverse items of
                    FunctionCallOutputItem output : _ ->
                        output.callId == "call-1"
                            && output.output == Aeson.String toolOutputText
                    _ -> False
            readIORef seenPrevious `shouldReturn` [Nothing]
            readIORef seenInputs `shouldReturn` [[]]
            let compacted = either (const []) (.backendState) result
            compacted `shouldSatisfy` hasCompactionCheckpoint
            readIORef contextState `shouldReturn` Nothing
            readIORef recordedUsage `shouldReturn` [compactionUsage]

        it "preserves typed provider failures from automatic compaction" do
            let history = [userTextItem "old"]
                compactError =
                    ProviderError UsageLimitReached "quota exhausted" (Just 120)
            contextState <- newIORef
                (Just (codexAutoCompactTokenLimit, length history))
            let compactAction =
                    pure (Left compactError)
                base = Backend \_ _ _ _ ->
                    error "failed compaction should not submit a model request"
                backend =
                    autoCompactOpenAiBackendWithApi
                        compactAction contextState base
            backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
                `shouldReturn`
                    Left (ProviderError UsageLimitReached
                        "automatic compaction failed: quota exhausted"
                        (Just 120))

        it "estimates resumed history when no provider token snapshot exists" do
            let history =
                    [ userTextItem
                        (Text.replicate
                            (codexAutoCompactTokenLimit * 4)
                            "x")
                    ]
            contextState <- newIORef Nothing
            compactCalls <- newIORef (0 :: Int)
            let compactAction = do
                    modifyIORef' compactCalls (+ 1)
                    pure $ Right CompactOutcome
                        { compactBeforeTokens = codexAutoCompactTokenLimit
                        , compactAfterTokens = 10
                        , compactHistory = [userTextItem "compacted"]
                        , compactSummary = "checkpoint"
                        }
                base = Backend \state _ _ _ ->
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWith
                        compactAction contextState base
            result <- backend.submitTurn history Nothing [UserMessage "new"]
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1

successful
    :: [ResponseItem]
    -> TurnOutput
    -> Either ApiError BackendResult
successful state output =
    Right BackendResult
        { backendOutput = output
        , backendState = state
        }

requestItems :: ResponseCreateParams -> [ResponseItem]
requestItems request = case request.input of
    Just (ResponseInputItems items) -> items
    _ -> []

remoteCompactionResponse :: Response
remoteCompactionResponse =
    responseWithOutput
        [ Aeson.object
            [ "type" .= ("compaction" :: Text)
            , "encrypted_content" .= ("opaque" :: Text)
            ]
        ]

responseWithoutCompaction :: Response
responseWithoutCompaction =
    responseWithOutput []

responseWithOutput :: [Aeson.Value] -> Response
responseWithOutput output =
    case Aeson.fromJSON $ Aeson.object
        [ "id" .= ("resp-compact" :: Text)
        , "created_at" .= (0 :: Int)
        , "status" .= ("completed" :: Text)
        , "model" .= ("gpt-test" :: Text)
        , "output" .= output
        , "usage" .= Aeson.object
            [ "input_tokens" .= compactionUsage.inputTokens
            , "output_tokens" .= compactionUsage.outputTokens
            , "total_tokens" .=
                (compactionUsage.inputTokens + compactionUsage.outputTokens)
            , "input_tokens_details" .= Aeson.object
                [ "cached_tokens" .= compactionUsage.cachedTokens
                ]
            ]
        ] of
        Aeson.Success response -> response
        Aeson.Error err -> error err

compactionUsage :: TokenUsage
compactionUsage = TokenUsage
    { inputTokens = 80
    , outputTokens = 6
    , cachedTokens = 40
    }

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

summaryResponse :: Text -> Response
summaryResponse summary =
    case Aeson.fromJSON $ Aeson.object
        [ "id" .= ("resp-summary" :: Text)
        , "created_at" .= (0 :: Int)
        , "status" .= ("completed" :: Text)
        , "model" .= ("gpt-test" :: Text)
        , "output" .=
            [ Aeson.object
                [ "type" .= ("message" :: Text)
                , "role" .= ("assistant" :: Text)
                , "content" .=
                    [ Aeson.object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= summary
                        ]
                    ]
                ]
            ]
        ] of
        Aeson.Success response -> response
        Aeson.Error err -> error err
