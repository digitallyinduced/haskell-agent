module Agent.CLI.CompactionSpec (spec) where

import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , autoCompactOpenAiBackendWith
    , autoCompactOpenAiBackendWithApi
    , autoCompactOpenAiBackendWithSender
    , autoCompactOpenAiBackendWithSenderAndHook
    , autoCompactOpenAiBackendWithThreshold
    , codexAutoCompactTokenLimit
    , compactOpenAIWith
    , installCompactOutcome
    , runProviderCompact
    , runProviderCompactWith
    , runResponsesCompactWith
    , runResponsesCompactWithContextWindow
    )
import Agent.CLI.Connectivity (withConnectionRecoveryUsing)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
import Agent.OpenAI.Compaction
    ( assistantSummaryItem
    , compactionTriggerItem
    , estimateRequestTokensWithItems
    , estimateResponseCreateParamsTokens
    , hasCompactionCheckpoint
    , summarizationPrompt
    , userTextItem
    )
import Agent.OpenAI.ModelMetadata (codexEffectiveContextWindowFor)
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
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

        it "rejects incomplete local summaries" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [userTextItem "old context"]
            result <-
                runProviderCompactWith
                    (Just \_ ->
                        pure (Right (summaryResponseWithStatus
                            "incomplete"
                            "partial summary")))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    (Just "focus")
            result `shouldSatisfy` \case
                Left message ->
                    "compaction response was not complete"
                        `Text.isInfixOf` message
                Right _ -> False

        it "clears tool and continuation controls for local summaries" do
            let paramsValue =
                    (defaultResponseCreateParams :: ResponseCreateParams)
                        { tools =
                            Just
                                [ FunctionToolValue FunctionTool
                                    { name = "must_call"
                                    , description = Nothing
                                    , parameters = Nothing
                                    , strict = Just True
                                    , extraFields = mempty
                                    }
                                ]
                        , toolChoice =
                            Just (ToolChoiceMode ToolChoiceRequired)
                        , maxToolCalls = Just 1
                        , previousResponseId = Just "resp-old"
                        , conversation = Just (ConversationId "conv-old")
                        }
            params <- newIORef paramsValue
            transcript <- newIORef [userTextItem "old context"]
            requests <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "summary")))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    (Just "focus")
            result `shouldSatisfy` either (const False) (const True)
            readIORef requests `shouldReturn`
                [ paramsValue
                    { tools = Nothing
                    , toolChoice = Nothing
                    , maxToolCalls = Nothing
                    , parallelToolCalls = Just False
                    , previousResponseId = Nothing
                    , conversation = Nothing
                    , input = Just
                        (ResponseInputItems
                            [ userTextItem "old context"
                            , userTextItem (summarizationPrompt (Just "focus"))
                            ])
                    , stream = Just True
                    }
                ]

        it "does not replay OpenAI checkpoints to portable summarizers" do
            let remoteCheckpoint =
                    KnownResponseItem ItemCompaction TaggedObject
                        { tag = "compaction"
                        , fields = mempty
                        }
                remoteTrigger =
                    UnknownResponseItem TaggedObject
                        { tag = "COMPACTION_TRIGGER"
                        , fields = mempty
                        }
                paramsValue = defaultResponseCreateParams
                history =
                    [ userTextItem "old context"
                    , remoteCheckpoint
                    , remoteTrigger
                    ]
            params <- newIORef paramsValue
            transcript <- newIORef history
            requests <- newIORef []
            result <-
                runResponsesCompactWithContextWindow
                    (Just 258_400)
                    (\request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "summary")))
                    (const (pure ()))
                    params
                    transcript
                    (Just "focus")
            result `shouldSatisfy` either (const False) (const True)
            readIORef requests >>= \case
                [request] ->
                    requestItems request `shouldSatisfy`
                        all \case
                            KnownResponseItem ItemCompaction _ -> False
                            KnownResponseItem ItemCompactionTrigger _ -> False
                            UnknownResponseItem tagged ->
                                Text.toLower (Text.strip tagged.tag)
                                    `notElem`
                                        [ "compaction"
                                        , "compaction_summary"
                                        , "compaction_trigger"
                                        ]
                            _ -> True
                seen ->
                    expectationFailure
                        ("expected one portable summary request, got "
                            <> show (length seen))

        it "preserves OpenAI checkpoints for focused OpenAI summaries" do
            let remoteCheckpoint =
                    KnownResponseItem ItemCompaction TaggedObject
                        { tag = "compaction"
                        , fields = mempty
                        }
                history = [userTextItem "old context", remoteCheckpoint]
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef history
            requests <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "summary")))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    (Just "focus")
            result `shouldSatisfy` either (const False) (const True)
            map requestItems <$> readIORef requests
                `shouldReturn`
                    [ history
                        <> [userTextItem (summarizationPrompt (Just "focus"))]
                    ]

        it "rejects portable summaries with only opaque checkpoints" do
            let remoteCheckpoint =
                    KnownResponseItem ItemCompaction TaggedObject
                        { tag = "compaction"
                        , fields = mempty
                        }
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [remoteCheckpoint]
            requests <- newIORef (0 :: Int)
            result <-
                runResponsesCompactWithContextWindow
                    (Just 258_400)
                    (\_ -> do
                        modifyIORef' requests (+ 1)
                        pure (Right (summaryResponse "summary")))
                    (const (pure ()))
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` \case
                Left message ->
                    "nothing compatible to compact" `Text.isInfixOf` message
                Right _ -> False
            readIORef requests `shouldReturn` 0

        it "refuses to guess a portable model context window" do
            let paramsValue =
                    (defaultResponseCreateParams :: ResponseCreateParams)
                        { model = Just "custom-small-model"
                        }
            params <- newIORef paramsValue
            transcript <- newIORef [userTextItem "old context"]
            requests <- newIORef (0 :: Int)
            result <-
                runResponsesCompactWith
                    (\_ -> do
                        modifyIORef' requests (+ 1)
                        pure (Right (summaryResponse "summary")))
                    (const (pure ()))
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` \case
                Left message ->
                    "context_window" `Text.isInfixOf` message
                        && "effective transport model" `Text.isInfixOf` message
                Right _ -> False
            readIORef requests `shouldReturn` 0

        it "uses a portable model context larger than the Codex fallback" do
            let contextWindow = 400_000
                huge = Text.replicate 1_100_000 "x"
                paramsValue =
                    (defaultResponseCreateParams :: ResponseCreateParams)
                        { model = Just "large-portable-model"
                        }
            params <- newIORef paramsValue
            transcript <- newIORef [userTextItem huge]
            requests <- newIORef []
            result <-
                runResponsesCompactWithContextWindow
                    (Just contextWindow)
                    (\request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "summary")))
                    (const (pure ()))
                    params
                    transcript
                    Nothing
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right outcome ->
                    estimateRequestTokensWithItems
                        paramsValue
                        outcome.compactHistory
                        `shouldSatisfy` (<= contextWindow)
            readIORef requests >>= \case
                [request] -> do
                    estimateResponseCreateParamsTokens request
                        `shouldSatisfy`
                            (> codexEffectiveContextWindowFor
                                paramsValue.model)
                    estimateResponseCreateParamsTokens request
                        `shouldSatisfy` (<= contextWindow)
                    requestItems request `shouldSatisfy` \case
                        MessageItem message : _ ->
                            case message.content of
                                MessageContentText text ->
                                    Text.length text == Text.length huge
                                MessageContentParts parts ->
                                    any
                                        (\case
                                            InputTextPart { text } ->
                                                Text.length text
                                                    == Text.length huge
                                            _ -> False)
                                        parts
                        _ -> False
                seen ->
                    expectationFailure
                        ("expected one portable compaction request, got "
                            <> show (length seen))

        it "bounds portable requests and snapshots to a smaller model window" do
            let contextWindow = 8_000
                paramsValue =
                    (defaultResponseCreateParams :: ResponseCreateParams)
                        { model = Just "small-portable-model"
                        }
            params <- newIORef paramsValue
            transcript <- newIORef
                [ userTextItem (Text.replicate 100_000 "old ")
                , userTextItem "recent request"
                ]
            requests <- newIORef []
            result <-
                runResponsesCompactWithContextWindow
                    (Just contextWindow)
                    (\request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "bounded summary")))
                    (const (pure ()))
                    params
                    transcript
                    Nothing
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right outcome ->
                    estimateRequestTokensWithItems
                        paramsValue
                        outcome.compactHistory
                        `shouldSatisfy` (<= contextWindow)
            readIORef requests >>= \case
                [request] ->
                    estimateResponseCreateParamsTokens request
                        `shouldSatisfy` (<= contextWindow)
                seen ->
                    expectationFailure
                        ("expected one portable compaction request, got "
                            <> show (length seen))

        it "bounds oversized local-summary requests before sending them" do
            params <- newIORef defaultResponseCreateParams
            let huge = Text.replicate 1_100_000 "x"
            transcript <- newIORef
                [ userTextItem huge
                , userTextItem "recent request"
                ]
            requests <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "bounded summary")))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    (Just "focus")
            result `shouldSatisfy` either (const False) (const True)
            seen <- readIORef requests
            case seen of
                [request] -> do
                    estimateResponseCreateParamsTokens request
                        `shouldSatisfy`
                            (<= codexEffectiveContextWindowFor
                                defaultResponseCreateParams.model)
                    requestItems request `shouldSatisfy` \items ->
                        all
                            (\case
                                MessageItem message ->
                                    case message.content of
                                        MessageContentText text ->
                                            Text.length text < Text.length huge
                                        MessageContentParts parts ->
                                            all
                                                (\case
                                                    InputTextPart { text } ->
                                                        Text.length text
                                                            < Text.length huge
                                                    _ -> True)
                                                parts
                                _ -> True)
                            items
                _ -> expectationFailure
                    ("expected one local compaction request, got "
                        <> show (length seen))

        it "bounds the installed local snapshot after summarization" do
            let paramsValue = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor paramsValue.model
                history =
                    [ userTextItem
                        (Text.replicate 180_000
                            ("message-" <> Text.pack (show index)))
                    | index <- [1 :: Int .. 6]
                    ]
            params <- newIORef paramsValue
            transcript <- newIORef history
            result <-
                runProviderCompactWith
                    (Just \_ -> pure (Right (summaryResponse "bounded summary")))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    (Just "focus")
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right outcome ->
                    estimateRequestTokensWithItems
                        paramsValue
                        outcome.compactHistory
                        `shouldSatisfy` (<= contextWindow)

        it "rejects blank local summaries" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef [userTextItem "old context"]
            result <-
                runProviderCompactWith
                    (Just \_ -> pure (Right (summaryResponse "   ")))
                    (const (pure ()))
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

        it "includes tool schemas when deciding whether to compact" do
            let history =
                    [userTextItem (Text.replicate 12_000 "old context ")]
                params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { tools = Just
                        [ FunctionToolValue FunctionTool
                            { name = "large_tool"
                            , description =
                                Just (Text.replicate 4_000 "schema")
                            , parameters = Nothing
                            , strict = Just True
                            , extraFields = mempty
                            }
                        ]
                    }
                paramsWithoutTools =
                    defaultResponseCreateParams :: ResponseCreateParams
                projectedItems = history <> [userTextItem "new"]
                projectedWithoutTools =
                    estimateRequestTokensWithItems
                        paramsWithoutTools
                        projectedItems
                projectedWithTools =
                    estimateRequestTokensWithItems params projectedItems
                threshold =
                    projectedWithoutTools
                        + ((projectedWithTools - projectedWithoutTools) `div` 2)
            estimateRequestTokensWithItems params []
                `shouldSatisfy` (< threshold)
            projectedWithoutTools `shouldSatisfy` (< threshold)
            projectedWithTools `shouldSatisfy` (>= threshold)
            contextState <-
                newIORef (Just (projectedWithoutTools, length history))
            compactCalls <- newIORef (0 :: Int)
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ ->
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn history (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1

        it "shrinks retained history below low automatic thresholds" do
            let history =
                    [ userTextItem (Text.replicate 200
                        ("message-" <> Text.pack (show index)))
                    | index <- [1 :: Int .. 100]
                    ]
                threshold = 5_000
                params = defaultResponseCreateParams
            contextState <- newIORef Nothing
            seenHistory <- newIORef []
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    writeIORef seenHistory state
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            compacted <- readIORef seenHistory
            estimateRequestTokensWithItems params compacted
                `shouldSatisfy` (< threshold)

        it "reserves pending input when sizing the compacted continuation" do
            let pendingText = Text.replicate 6_000 "pending "
                pendingItem = userTextItem pendingText
                params = defaultResponseCreateParams
                threshold =
                    estimateRequestTokensWithItems params [pendingItem]
                        + 8_000
                history =
                    [userTextItem (Text.replicate 30_000 "old ")]
            contextState <- newIORef Nothing
            continuationTokens <- newIORef 0
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    writeIORef continuationTokens $
                        estimateRequestTokensWithItems
                            params
                            (state <> [pendingItem])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn history Nothing
                [UserMessage pendingText] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            tokens <- readIORef continuationTokens
            tokens `shouldSatisfy` (< threshold)

        it "rejects a tiny threshold before starting compaction" do
            let history = [userTextItem "old"]
                threshold = 1
                params = defaultResponseCreateParams
            contextState <- newIORef Nothing
            compactCalls <- newIORef (0 :: Int)
            continuationCalls <- newIORef (0 :: Int)
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    modifyIORef' continuationCalls (+ 1)
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` \case
                Left (ProviderError InvalidRequestError message _) ->
                    "increase --compact-threshold" `Text.isInfixOf` message
                _ -> False
            readIORef compactCalls `shouldReturn` 0
            readIORef continuationCalls `shouldReturn` 0
            readIORef contextState `shouldReturn` Nothing

        it "runs the post-compaction hook only after a successful continuation" do
            let history = [userTextItem "old"]
                threshold = 20
            contextState <- newIORef (Just (threshold, length history))
            hookCalls <- newIORef (0 :: Int)
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ ->
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSenderAndHook
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        (modifyIORef' hookCalls (+ 1))
                        contextState
                        base
            result <- backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef hookCalls `shouldReturn` 1

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

        it "disables parallel tool calls for Responses Lite remote compaction" do
            requests <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "remote compaction unexpectedly requested credentials"
                send _ request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                history = [userTextItem "old context"]
                params = defaultResponseCreateParams
                    { model = Just "gpt-5.6-sol"
                    , instructions = Just "keep these instructions"
                    , store = Just True
                    , tools = Just []
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
                Right outcome ->
                    outcome.compactSummary
                        `shouldBe` "Context compacted remotely."
            map (.parallelToolCalls) <$> readIORef requests
                `shouldReturn` [Just False]

        it "rejects remote checkpoints that cannot fit the installed snapshot" do
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "remote compaction unexpectedly requested credentials"
                contextWindow =
                    codexEffectiveContextWindowFor
                        defaultResponseCreateParams.model
                oversizedResponse =
                    responseWithOutput
                        [ Aeson.object
                            [ "type" .= ("compaction" :: Text)
                            , "encrypted_content" .=
                                Text.replicate (contextWindow * 4 + 10_000) "x"
                            ]
                        ]
                send _ _ = pure (Right oversizedResponse)
                history = [userTextItem "old context"]
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    defaultResponseCreateParams
                    history
                    100
                    Nothing
            result `shouldSatisfy` \case
                Left message ->
                    "remote compacted snapshot request cannot fit"
                        `Text.isInfixOf` message
                Right _ -> False

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
                        `shouldBe`
                            [ userTextItem "old context"
                            , assistantSummaryItem "local summary"
                            ]
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

        it "caps configured thresholds at the effective context window" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                history =
                    [ userTextItem
                        (Text.replicate ((contextWindow + 1_000) * 4) "x")
                    ]
                pendingText = "new"
                projectedTokens =
                    estimateRequestTokensWithItems
                        params
                        (history <> [userTextItem pendingText])
                threshold = projectedTokens + 1_000
            projectedTokens `shouldSatisfy` (> contextWindow)
            projectedTokens `shouldSatisfy` (< threshold)
            contextState <- newIORef Nothing
            compactCalls <- newIORef (0 :: Int)
            seenHistory <- newIORef []
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    modifyIORef' seenHistory (<> [state])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn history Nothing
                [UserMessage pendingText] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef seenHistory >>= \case
                [compacted] -> hasCompactionCheckpoint compacted
                    `shouldBe` True
                seen ->
                    expectationFailure
                        ("expected one compacted continuation, got "
                            <> show (length seen))

        it "rejects an oversized first turn before provider submission" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                prompt =
                    Text.replicate ((contextWindow + 1_000) * 4) "x"
                projectedTokens =
                    estimateRequestTokensWithItems
                        params
                        [userTextItem prompt]
            projectedTokens `shouldSatisfy` (> contextWindow)
            contextState <- newIORef Nothing
            compactCalls <- newIORef (0 :: Int)
            submitCalls <- newIORef (0 :: Int)
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    modifyIORef' submitCalls (+ 1)
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just (projectedTokens + 1_000))
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn [] Nothing
                [UserMessage prompt] (const (pure ()))
            result `shouldSatisfy` \case
                Left (ProviderError InvalidRequestError message _) ->
                    "initial request cannot fit" `Text.isInfixOf` message
                _ -> False
            readIORef compactCalls `shouldReturn` 0
            readIORef submitCalls `shouldReturn` 0

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
            hookCalls <- newIORef (0 :: Int)
            let sender request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                base = Backend \_ _ _ _ ->
                    pure (Left (ConnectionError "continuation failed"))
                backend =
                    autoCompactOpenAiBackendWithSenderAndHook
                        (Just threshold)
                        sender
                        (\usage -> modifyIORef' recordedUsage (<> [usage]))
                        (pure defaultResponseCreateParams)
                        (modifyIORef' hookCalls (+ 1))
                        contextState
                        base
            backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
                `shouldReturn` Left (ConnectionError "continuation failed")
            map requestItems <$> readIORef requests
                `shouldReturn` [history <> [compactionTriggerItem]]
            readIORef recordedUsage `shouldReturn` [compactionUsage]
            readIORef contextState `shouldReturn` oldContextState
            readIORef hookCalls `shouldReturn` 0

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

        it "defers compaction while continuing a completed tool call" do
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
            seenPrevious <- newIORef []
            seenInputs <- newIORef []
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
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
            readIORef compactCalls `shouldReturn` 0
            readIORef seenPrevious `shouldReturn` [Just "resp-tool"]
            readIORef seenInputs `shouldReturn` [inputs]
            fmap (.backendState) result `shouldBe` Right oldHistory
            readIORef contextState `shouldReturn` Nothing
            readIORef recordedUsage `shouldReturn` []

        it "truncates oversized tool output before continuing the call" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-oversized"
                    , name = "shell_command"
                    , namespace = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    , extraFields = mempty
                    }
                oldHistory = [userTextItem "run it", danglingCall]
                originalOutput =
                    Text.replicate ((contextWindow + 10_000) * 4) "x"
                toolResult = ToolCallResult
                    { callId = "call-oversized"
                    , output = originalOutput
                    , callKind = FunctionCallKind
                    }
                inputs = [CompletedTool toolResult]
            estimateRequestTokensWithItems
                params
                (oldHistory <> turnInputsToItems inputs)
                `shouldSatisfy` (> contextWindow)
            contextState <- newIORef Nothing
            compactCalls <- newIORef (0 :: Int)
            seenPrevious <- newIORef []
            seenInputs <- newIORef []
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                base = Backend \state previous submitted _ -> do
                    modifyIORef' seenPrevious (<> [previous])
                    modifyIORef' seenInputs (<> [submitted])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn oldHistory (Just "resp-tool") inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 0
            readIORef seenPrevious `shouldReturn` [Just "resp-tool"]
            readIORef seenInputs >>= \case
                [[CompletedTool bounded]] -> do
                    bounded.callId `shouldBe` toolResult.callId
                    bounded.callKind `shouldBe` toolResult.callKind
                    Text.length bounded.output
                        `shouldSatisfy` (< Text.length originalOutput)
                    bounded.output
                        `shouldSatisfy`
                            Text.isSuffixOf
                                "[tool output truncated to fit the model context]"
                    estimateRequestTokensWithItems
                        params
                        ( oldHistory
                            <> turnInputsToItems [CompletedTool bounded]
                        )
                        `shouldSatisfy` (<= contextWindow)
                submitted ->
                    expectationFailure
                        ("expected one bounded tool result, got "
                            <> show submitted)

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

        it "rejects compacted snapshots at or above the trigger" do
            let history = [userTextItem "old"]
                oldContextState =
                    Just (codexAutoCompactTokenLimit, length history)
                compactedHistory = [userTextItem "still too large"]
                compactAction =
                    pure $ Right CompactOutcome
                        { compactBeforeTokens = codexAutoCompactTokenLimit
                        , compactAfterTokens = codexAutoCompactTokenLimit
                        , compactHistory = compactedHistory
                        , compactSummary = "checkpoint"
                        }
            contextState <- newIORef oldContextState
            continuationCalls <- newIORef (0 :: Int)
            let base = Backend \state _ _ _ -> do
                    modifyIORef' continuationCalls (+ 1)
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend =
                    autoCompactOpenAiBackendWith
                        compactAction contextState base
            result <- backend.submitTurn history Nothing
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` \case
                Left (ProviderError InvalidRequestError message _) ->
                    "increase --compact-threshold" `Text.isInfixOf` message
                _ -> False
            readIORef continuationCalls `shouldReturn` 0
            readIORef contextState `shouldReturn` oldContextState

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
    summaryResponseWithStatus "completed" summary

summaryResponseWithStatus :: Text -> Text -> Response
summaryResponseWithStatus responseStatus summary =
    case Aeson.fromJSON $ Aeson.object
        [ "id" .= ("resp-summary" :: Text)
        , "created_at" .= (0 :: Int)
        , "status" .= responseStatus
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
