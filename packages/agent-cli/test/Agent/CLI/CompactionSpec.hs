module Agent.CLI.CompactionSpec (spec) where

import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , CompactionInstall(..)
    , autoCompactBackendWith
    , autoCompactOpenAiBackendWith
    , autoCompactOpenAiBackendWithApi
    , autoCompactOpenAiBackendWithSender
    , autoCompactOpenAiBackendWithSenderAndHook
    , autoCompactOpenAiBackendWithSenderHookAndDecorator
    , autoCompactOpenAiBackendWithThreshold
    , codexAutoCompactTokenLimit
    , compactOpenAIWith
    , decorateCompactOutcomeWithTaskPlan
    , decorateCompactOutcomeWithTaskPlanWithin
    , estimatedOccupancy
    , installCompactOutcome
    , reportedOccupancy
    , runProviderCompact
    , runProviderCompactWith
    , runBackendCompactWithContextWindow
    , runResponsesCompactWith
    , runResponsesCompactWithContextWindow
    )
import Agent.Connectivity (withConnectionRecoveryUsing)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Json.Decode qualified as Hermes
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
import Agent.Tools.TaskPlan
    ( CurrentTaskPlan(..)
    , TaskPlan(..)
    , TaskPlanItem(..)
    , TaskPlanStatus(..)
    , isTaskPlanContextText
    , newTaskPlanEnv
    , replaceTaskPlan
    , taskPlanContextText
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Agent.Provider
    ( BillingMode(..)
    , Provider(..)
    , tokenProvider
    )
import Control.Exception
    ( AsyncException(..)
    , ErrorCall(..)
    , MaskingState(..)
    , getMaskingState
    , throwIO
    , try
    )
import Data.IORef
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
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
            runProviderCompact GeminiProvider Nothing params transcript Nothing
                `shouldReturn` Left "gemini compact requires a token provider"

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
                    KnownResponseItem ItemCompaction (TaggedObject "compaction")
                remoteTrigger =
                    UnknownResponseItem (TaggedObject "COMPACTION_TRIGGER")
                contextCheckpoint =
                    ContextCompactionItemValue ContextCompactionItem
                        { itemId = Just "ctx-compact"
                        , encryptedContent = Just "opaque"
                        }
                knownContextCheckpoint =
                    KnownResponseItem ItemContextCompaction
                        (TaggedObject "context_compaction")
                paramsValue = defaultResponseCreateParams
                history =
                    [ userTextItem "old context"
                    , remoteCheckpoint
                    , contextCheckpoint
                    , knownContextCheckpoint
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
                            CompactionItemValue{} -> False
                            ContextCompactionItemValue{} -> False
                            CompactionTriggerItemValue{} -> False
                            KnownResponseItem ItemCompaction _ -> False
                            KnownResponseItem ItemContextCompaction _ -> False
                            KnownResponseItem ItemCompactionTrigger _ -> False
                            UnknownResponseItem tagged ->
                                Text.toLower (Text.strip tagged.tag)
                                    `notElem`
                                        [ "compaction"
                                        , "compaction_summary"
                                        , "context_compaction"
                                        , "compaction_trigger"
                                        ]
                            _ -> True
                seen ->
                    expectationFailure
                        ("expected one portable summary request, got "
                            <> show (length seen))

        it "preserves OpenAI checkpoints for focused OpenAI summaries" do
            let remoteCheckpoint =
                    KnownResponseItem ItemCompaction (TaggedObject "compaction")
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
                    KnownResponseItem ItemCompaction (TaggedObject "compaction")
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

    describe "runBackendCompactWithContextWindow" do
        it "uses an isolated fresh backend and records summary usage" do
            let history = [userTextItem "old context"]
                paramsValue =
                    (defaultResponseCreateParams :: ResponseCreateParams)
                        { tools =
                            Just
                                [ FunctionToolValue FunctionTool
                                    { name = "must_not_run"
                                    , description = Nothing
                                    , parameters = Nothing
                                    , strict = Just True
                                    }
                                ]
                        , previousResponseId = Just "resp-old"
                        }
            params <- newIORef paramsValue
            transcript <- newIORef history
            requests <- newIORef []
            recordedUsage <- newIORef []
            let makeBackend summaryParams =
                    Backend \snapshot previous inputs _onEvent -> do
                        modifyIORef' requests
                            (<> [(summaryParams, snapshot, previous, inputs)])
                        pure $ successful snapshot TurnOutput
                            { responseId = "claude-summary-session"
                            , toolCalls = []
                            , assistantText = Just "portable summary"
                            , tokenUsage = compactionUsage
                            , providerTelemetry = Nothing
                            , completion = TurnCompleted
                            }
            result <-
                runBackendCompactWithContextWindow
                    200_000
                    makeBackend
                    (\usage -> modifyIORef' recordedUsage (<> [usage]))
                    params
                    transcript
                    (Just "focus")
            outcome <- case result of
                Left err -> expectationFailure (Text.unpack err) >> undefined
                Right value -> pure value
            outcome.compactSummary `shouldBe` "portable summary"
            readIORef recordedUsage `shouldReturn` [compactionUsage]
            readIORef requests >>= \case
                [(summaryParams, snapshot, previous, inputs)] -> do
                    summaryParams.tools `shouldBe` Nothing
                    summaryParams.previousResponseId `shouldBe` Nothing
                    snapshot.backendItems `shouldBe` history
                    snapshot.backendContinuation `shouldBe` Nothing
                    previous `shouldBe` Nothing
                    inputs `shouldBe`
                        [UserMessage (summarizationPrompt (Just "focus"))]
                _ -> expectationFailure "expected one isolated backend request"

        it "clears a provider continuation before automatic continuation" do
            let oldHistory = [userTextItem "old context"]
                compactedHistory = [assistantSummaryItem "summary"]
                outcome = CompactOutcome
                    { compactBeforeTokens = 100
                    , compactAfterTokens = 4
                    , compactHistory = compactedHistory
                    , compactSummary = "summary"
                    }
                oldSnapshot =
                    advanceBackendSnapshot
                        emptyBackendSnapshot
                        oldHistory
                        (Just BackendContinuation
                            { continuationProvider = "test"
                            , continuationToken = "session-old"
                            })
            contextState <- newIORef
                (Just (reportedOccupancy 100 (length oldHistory)))
            submissions <- newIORef []
            let base =
                    Backend \snapshot previous inputs _onEvent -> do
                        modifyIORef' submissions
                            (<> [(snapshot, previous, inputs)])
                        pure $ successful snapshot TurnOutput
                            { responseId = "session-new"
                            , toolCalls = []
                            , assistantText = Just "continued"
                            , tokenUsage = TokenUsage 5 1 0
                            , providerTelemetry = Nothing
                            , completion = TurnCompleted
                            }
                backend =
                    autoCompactBackendWith
                        (pure 20)
                        (\_history _inputs -> pure (Right outcome))
                        (\_outcome _inputs -> pure CompactionNotInstalled)
                        (pure defaultResponseCreateParams)
                        contextState
                        base
            result <-
                backend.submitTurn
                    oldSnapshot
                    (Just "session-old")
                    [UserMessage "continue"]
                    (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef submissions >>= \case
                [(snapshot, previous, inputs)] -> do
                    snapshot.backendItems `shouldBe` compactedHistory
                    snapshot.backendContinuation `shouldBe` Nothing
                    previous `shouldBe` Nothing
                    inputs `shouldBe` [UserMessage "continue"]
                _ -> expectationFailure "expected one compacted continuation"

    describe "installCompactOutcome" do
        it "clears the previous response id with transcript and token state" do
            previous <- newIORef (Just "resp-old")
            transcript <- newIORef [userTextItem "old context"]
            contextState <- newIORef (Just (reportedOccupancy 100 1))
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
                Just
                    ( estimatedOccupancy
                        outcome.compactAfterTokens
                        (length compactedHistory)
                    )

        it "leaves live state unchanged when compaction fails" do
            let oldHistory = [userTextItem "old context"]
                oldContextState =
                    Just (reportedOccupancy 100 (length oldHistory))
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
            contextState <- newIORef Nothing
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1

        it "does not treat estimated compact occupancy as full request usage" do
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
                            }
                        ]
                    }
                pending = [UserMessage "new"]
                projectedItems = history <> [userTextItem "new"]
                projectedWithoutTools =
                    estimateRequestTokensWithItems
                        defaultResponseCreateParams
                        projectedItems
                projectedWithTools =
                    estimateRequestTokensWithItems params projectedItems
                threshold =
                    projectedWithoutTools
                        + ((projectedWithTools - projectedWithoutTools) `div` 2)
                estimatedTokens = 50
            projectedWithoutTools `shouldSatisfy` (< threshold)
            projectedWithTools `shouldSatisfy` (>= threshold)
            estimatedTokens + 20 `shouldSatisfy` (< threshold)
            contextState <- newIORef
                (Just (estimatedOccupancy estimatedTokens (length history)))
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) (Just "resp-old") pending
                (const (pure ()))
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
                    writeIORef seenHistory state.backendItems
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing
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
                            (state.backendItems <> [pendingItem])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` \case
                Left (ProviderError InvalidRequestError message _) ->
                    "increase --compact-threshold" `Text.isInfixOf` message
                _ -> False
            readIORef compactCalls `shouldReturn` 0
            readIORef continuationCalls `shouldReturn` 0
            readIORef contextState `shouldReturn` Nothing

        it "does not continue when the durable compaction hook fails" do
            let history = [userTextItem "old"]
                threshold = 20
            contextState <- newIORef
                (Just (reportedOccupancy threshold (length history)))
            continuationCalls <- newIORef (0 :: Int)
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    modifyIORef' continuationCalls (+ 1)
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSenderAndHook
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        (\_outcome _inputs ->
                            throwIO (ErrorCall "checkpoint failed"))
                        contextState
                        base
            result <- try $
                backend.submitTurn (initialBackendSnapshot history) Nothing
                    [UserMessage "new"] (const (pure ()))
                :: IO
                    (Either ErrorCall (Either ApiError BackendResult))
            result `shouldBe` Left (ErrorCall "checkpoint failed")
            readIORef continuationCalls `shouldReturn` 0
            readIORef contextState `shouldReturn`
                Just (reportedOccupancy threshold (length history))

        it "commits the compaction hook before its continuation" do
            let history = [userTextItem "old"]
                threshold = 20
            contextState <- newIORef
                (Just (reportedOccupancy threshold (length history)))
            hookCalls <- newIORef (0 :: Int)
            hookWasCommitted <- newIORef False
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \state _ _ _ -> do
                    readIORef hookWasCommitted `shouldReturn` True
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSenderAndHook
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        (\_outcome inputs -> do
                            inputs `shouldBe` [UserMessage "new"]
                            writeIORef hookWasCommitted True
                            modifyIORef' hookCalls (+ 1)
                            pure CompactionInstalled)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing
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

    describe "task-plan compaction context" do
        it "replaces stale generated copies from authoritative state" do
            let stale =
                    taskPlanContextText $
                        CurrentTaskPlan 8 $
                            TaskPlan Nothing
                                [TaskPlanItem "stale" TaskPlanPending]
                plan = TaskPlan
                    (Just "continue here")
                    [TaskPlanItem "current" TaskPlanInProgress]
                current = CurrentTaskPlan 1 plan
                outcome = CompactOutcome
                    { compactBeforeTokens = 20
                    , compactAfterTokens = 10
                    , compactHistory =
                        [userTextItem stale, userTextItem "retained"]
                    , compactSummary = "summary"
                    }
            env <- newTaskPlanEnv Nothing Nothing
            replaceTaskPlan env plan `shouldReturn` Right current
            decorated <-
                decorateCompactOutcomeWithTaskPlan (Just env) outcome
            filter responseItemHasTaskPlan decorated.compactHistory
                `shouldSatisfy` \case
                    [MessageItem message] ->
                        message.role == RoleDeveloper
                            && responseMessageHasText
                                (taskPlanContextText current)
                                message
                    _ -> False

        it "does not reconstruct a plan from pre-compaction history" do
            let stale =
                    taskPlanContextText $
                        CurrentTaskPlan 8 $
                            TaskPlan Nothing
                                [TaskPlanItem "stale" TaskPlanInProgress]
                outcome = CompactOutcome
                    { compactBeforeTokens = 20
                    , compactAfterTokens = 10
                    , compactHistory =
                        [userTextItem stale, userTextItem "retained"]
                    , compactSummary = "summary"
                    }
            env <- newTaskPlanEnv Nothing Nothing
            decorated <-
                decorateCompactOutcomeWithTaskPlan (Just env) outcome
            decorated.compactHistory
                `shouldBe` [userTextItem "retained"]

        it "rejects a generated plan that cannot fit the compacted request" do
            let plan = TaskPlan Nothing
                    [TaskPlanItem "current" TaskPlanInProgress]
                outcome = CompactOutcome
                    { compactBeforeTokens = 20
                    , compactAfterTokens = 1
                    , compactHistory = []
                    , compactSummary = "summary"
                    }
                rawLimit =
                    estimateRequestTokensWithItems
                        defaultResponseCreateParams
                        outcome.compactHistory
            env <- newTaskPlanEnv Nothing Nothing
            _ <- replaceTaskPlan env plan
            result <-
                decorateCompactOutcomeWithTaskPlanWithin
                    rawLimit
                    defaultResponseCreateParams
                    (Just env)
                    outcome
            result `shouldSatisfy` \case
                Left message ->
                    "authoritative task plan does not fit"
                        `Text.isInfixOf` message
                Right _ -> False

    describe "autoCompactOpenAiBackendWith" do
        it "decorates before publishing and continuing automatic compaction" do
            let history = [userTextItem "old"]
                threshold = 2_000
                plan = TaskPlan Nothing
                    [TaskPlanItem "continue implementation" TaskPlanInProgress]
            taskPlanEnv <- newTaskPlanEnv Nothing Nothing
            _ <- replaceTaskPlan taskPlanEnv plan
            contextState <- newIORef
                (Just (reportedOccupancy threshold (length history)))
            hookHistory <- newIORef []
            continuationHistory <- newIORef []
            let sender _request =
                    pure (Right remoteCompactionResponse)
                base = Backend \state _previous _inputs _onEvent -> do
                    writeIORef continuationHistory state.backendItems
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSenderHookAndDecorator
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        (decorateCompactOutcomeWithTaskPlan
                            (Just taskPlanEnv))
                        (\outcome _inputs -> do
                            writeIORef hookHistory outcome.compactHistory
                            pure CompactionInstalled)
                        contextState
                        base
            result <-
                backend.submitTurn
                    (initialBackendSnapshot history)
                    Nothing
                    [UserMessage "new"]
                    (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef hookHistory
                >>= (`shouldSatisfy` any responseItemHasTaskPlan)
            readIORef continuationHistory
                >>= (`shouldSatisfy` any responseItemHasTaskPlan)

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
                (Just (reportedOccupancy threshold (length history)))
            let backend =
                    autoCompactOpenAiBackendWithThreshold
                        (Just threshold)
                        tokens
                        (pure params)
                        contextState
                        base
            backend.submitTurn (initialBackendSnapshot history) Nothing
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing
                [UserMessage pendingText] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef seenHistory >>= \case
                [compacted] ->
                    hasCompactionCheckpoint compacted.backendItems
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just (projectedTokens + 1_000))
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn emptyBackendSnapshot Nothing
                [UserMessage prompt] (const (pure ()))
            result `shouldSatisfy` \case
                Left (ProviderError InvalidRequestError message _) ->
                    "initial request cannot fit" `Text.isInfixOf` message
                _ -> False
            readIORef compactCalls `shouldReturn` 0
            readIORef submitCalls `shouldReturn` 0

        it "does not reject a first turn whose size is dominated by an image" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { model = Just "gpt-5.6-sol" }
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                image =
                    ImageAttachment
                        { imageMime = "image/png"
                        , imageBytes = ByteString.replicate (1024 * 1024) 1
                        }
                inputs =
                    [ userMessageWithAttachments
                        "what does this screenshot show?"
                        [ImageAttachmentItem image]
                    ]
                items = turnInputsToItems inputs
                naiveTokens =
                    Text.length
                        (TextEncoding.decodeUtf8
                            (LBS.toStrict (Aeson.encode items)))
                        `div` 4
                estimated =
                    estimateRequestTokensWithItems params items
            naiveTokens `shouldSatisfy` (> contextWindow)
            estimated `shouldSatisfy` (< contextWindow)
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn emptyBackendSnapshot Nothing inputs (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 0
            readIORef submitCalls `shouldReturn` 1

        it "compacts before the next request at the Codex token limit" do
            let oldHistory = [userTextItem "old"]
                compactedHistory = [userTextItem "compacted"]
            contextState <- newIORef
                (Just (reportedOccupancy
                    codexAutoCompactTokenLimit (length oldHistory)))
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWith
                        compactAction contextState base
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) (Just "resp-old")
                [UserMessage "new"]
                (\event -> modifyIORef' events (<> [event]))
            result `shouldSatisfy` either (const False) (const True)
            fmap (.backendState.backendItems) result
                `shouldBe` Right compactedHistory
            readIORef compactCalls `shouldReturn` 1
            readIORef seenPrevious `shouldReturn` [Nothing]
            readIORef contextState `shouldReturn`
                Just (reportedOccupancy 25 (length compactedHistory))
            readIORef events `shouldReturn`
                [ActivityUpdated "Compacting context…"]

        it "records active-session compaction usage before a failed continuation" do
            let history = [userTextItem "old"]
                threshold = 20
                oldContextState =
                    Just (reportedOccupancy (threshold - 2) (length history))
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
                        (\_outcome inputs -> do
                            inputs `shouldBe` [UserMessage "new"]
                            modifyIORef' hookCalls (+ 1)
                            pure CompactionInstalled)
                        contextState
                        base
            backend.submitTurn (initialBackendSnapshot history) Nothing
                [UserMessage "new"] (const (pure ()))
                `shouldReturn` Left (ConnectionError "continuation failed")
            map requestItems <$> readIORef requests
                `shouldReturn` [history <> [compactionTriggerItem]]
            readIORef recordedUsage `shouldReturn` [compactionUsage]
            readIORef contextState >>= (`shouldSatisfy`
                (/= oldContextState))
            readIORef hookCalls `shouldReturn` 1

        it "rolls back deferred compacted state when the continuation is cancelled" do
            let history = [userTextItem "old"]
                threshold = 20
                oldContextState =
                    Just (reportedOccupancy threshold (length history))
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
                    backend.submitTurn (initialBackendSnapshot history) Nothing
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
                (Just (reportedOccupancy threshold (length history)))
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
                                    , providerTelemetry = Nothing
                                    , completion = TurnCompleted
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
            result <- backend.submitTurn (initialBackendSnapshot history) (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef continuationCalls `shouldReturn` 2
            readIORef waits `shouldReturn` [1_000_000]
            readIORef seenPrevious `shouldReturn` [Nothing, Nothing]
            readIORef recordedUsage `shouldReturn` [compactionUsage]

        it "absorbs completed tool output before mid-turn compaction" do
            let danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    }
                oldHistory = [userTextItem "run it", danglingCall]
                toolOutputText = Text.replicate 400 "x"
                toolResult = ToolCallResult
                    { callId = "call-1"
                    , output = toolOutputText
                    , callKind = FunctionCallKind
                    }
            contextState <- newIORef
                (Just (reportedOccupancy
                    (codexAutoCompactTokenLimit - 10) (length oldHistory)))
            compactCalls <- newIORef (0 :: Int)
            recordedUsage <- newIORef []
            seenPrevious <- newIORef []
            seenInputs <- newIORef []
            requests <- newIORef []
            installedHistory <- newIORef []
            let sender request = do
                    modifyIORef' compactCalls (+ 1)
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                base = Backend \state previous inputs _ -> do
                    modifyIORef' seenPrevious (<> [previous])
                    modifyIORef' seenInputs (<> [inputs])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSenderAndHook
                        (Just codexAutoCompactTokenLimit)
                        sender
                        (\usage -> modifyIORef' recordedUsage (<> [usage]))
                        (pure defaultResponseCreateParams)
                        (\outcome pending -> do
                            pending `shouldBe` [UserMessage "steer"]
                            writeIORef installedHistory
                                (outcome.compactHistory <> turnInputsToItems pending)
                            pure CompactionInstalled)
                        contextState
                        base
                inputs = [CompletedTool toolResult, UserMessage "steer"]
                completedInputs = [CompletedTool toolResult]
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) (Just "resp-tool") inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            map requestItems <$> readIORef requests
                `shouldReturn`
                    [ oldHistory
                        <> turnInputsToItems completedInputs
                        <> [compactionTriggerItem]
                    ]
            readIORef seenPrevious `shouldReturn` [Nothing]
            readIORef seenInputs `shouldReturn` [[]]
            compacted <- readIORef installedHistory
            compacted `shouldSatisfy` hasCompactionCheckpoint
            fmap (.backendState.backendItems) result `shouldBe` Right compacted
            readIORef contextState `shouldReturn`
                Just (reportedOccupancy 25 (length compacted))
            readIORef recordedUsage `shouldReturn` [compactionUsage]

        it "truncates oversized tool output before continuing the call" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-oversized"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) (Just "resp-tool") inputs
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

        it "records provider-reported occupancy after a successful turn" do
            let history = [userTextItem "old"]
                usage = TokenUsage 1_200 80 400
            contextState <- newIORef Nothing
            let base = Backend \state _ _ _ ->
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = usage
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        (\_ -> error "occupancy tracking should not compact")
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef contextState `shouldReturn`
                Just (reportedOccupancy
                    (usage.inputTokens + usage.outputTokens)
                    (length history))

        it "uses last reported occupancy instead of JSON length to decide compaction" do
            let history =
                    [userTextItem (Text.replicate 20_000 "old context ")]
                params = defaultResponseCreateParams
                occupancy = 200
                pending = [UserMessage "new"]
                jsonEstimate =
                    estimateRequestTokensWithItems
                        params
                        (history <> turnInputsToItems pending)
                threshold = occupancy + 1_000
            jsonEstimate `shouldSatisfy` (> threshold)
            contextState <- newIORef
                (Just (reportedOccupancy occupancy (length history)))
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        (Just threshold)
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) (Just "resp-old") pending
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 0
            readIORef contextState `shouldReturn`
                Just (reportedOccupancy 25 (length history))

        it "does not truncate tool output when reported occupancy still fits" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-keep"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    }
                oldHistory =
                    [ userTextItem
                        (Text.replicate ((contextWindow + 1_000) * 4) "x")
                    , danglingCall
                    ]
                toolResult = ToolCallResult
                    { callId = "call-keep"
                    , output = "hello from the tool"
                    , callKind = FunctionCallKind
                    }
                inputs = [CompletedTool toolResult]
                occupancy = 500
            estimateRequestTokensWithItems
                params
                (oldHistory <> turnInputsToItems inputs)
                `shouldSatisfy` (> contextWindow)
            contextState <- newIORef
                (Just (reportedOccupancy occupancy (length oldHistory)))
            seenInputs <- newIORef []
            let base = Backend \_state _previous submitted _ -> do
                    modifyIORef' seenInputs (<> [submitted])
                    pure $ successful (initialBackendSnapshot oldHistory) TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        (\_ -> error "fitting tool output should not compact")
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) (Just "resp-tool") inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef seenInputs `shouldReturn` [inputs]

        it "bounds tool output before absorbing it into compaction" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-occupancy"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    }
                oldHistory = [userTextItem "run it", danglingCall]
                originalOutput = Text.replicate 80_000 "x"
                toolResult = ToolCallResult
                    { callId = "call-occupancy"
                    , output = originalOutput
                    , callKind = FunctionCallKind
                    }
                inputs = [CompletedTool toolResult]
                occupancy = contextWindow - 10_000
            estimateRequestTokensWithItems
                params
                (oldHistory <> turnInputsToItems inputs)
                `shouldSatisfy` (<= contextWindow)
            contextState <- newIORef
                (Just (reportedOccupancy occupancy (length oldHistory)))
            seenInputs <- newIORef []
            requests <- newIORef []
            let sender request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                base = Backend \_state _previous submitted _ -> do
                    modifyIORef' seenInputs (<> [submitted])
                    pure $ successful (initialBackendSnapshot oldHistory) TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) (Just "resp-tool") inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef seenInputs `shouldReturn` [[]]
            readIORef requests >>= \case
                [request] -> do
                    let encoded =
                            TextEncoding.decodeUtf8
                                (LBS.toStrict (Aeson.encode request))
                    Text.length encoded
                        `shouldSatisfy` (< Text.length originalOutput)
                    encoded
                        `shouldSatisfy`
                            Text.isInfixOf
                                "[tool output truncated to fit the model context]"
                submitted ->
                    expectationFailure
                        ("expected one compact request, got "
                            <> show (length submitted))

        it "forgets occupancy when the provider omits usage" do
            let history = [userTextItem "old"]
                oldContextState =
                    Just (reportedOccupancy 1_000 (length history))
            contextState <- newIORef oldContextState
            let base = Backend \state _ _ _ ->
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = emptyTokenUsage
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        (\_ -> error "missing usage should not compact")
                        (const (pure ()))
                        (pure defaultResponseCreateParams)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot history) (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef contextState `shouldReturn` Nothing

        it "compacts an oversized live tool chain with its output absorbed" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-live"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    }
                oldHistory =
                    [ userTextItem
                        (Text.replicate ((contextWindow + 1_000) * 4) "x")
                    , danglingCall
                    ]
                toolResult = ToolCallResult
                    { callId = "call-live"
                    , output = "ok"
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) (Just "resp-tool") inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef seenPrevious `shouldReturn` [Nothing]
            readIORef seenInputs `shouldReturn` [[]]

        it "compacts replay history when a tool continuation cannot fit" do
            let params = defaultResponseCreateParams
                contextWindow =
                    codexEffectiveContextWindowFor params.model
                danglingCall = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-replay"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    }
                oldHistory =
                    [ userTextItem
                        (Text.replicate ((contextWindow + 1_000) * 4) "x")
                    , danglingCall
                    ]
                toolResult = ToolCallResult
                    { callId = "call-replay"
                    , output = "ok"
                    , callKind = FunctionCallKind
                    }
                inputs = [CompletedTool toolResult]
            estimateRequestTokensWithItems
                params
                (oldHistory <> turnInputsToItems inputs)
                `shouldSatisfy` (> contextWindow)
            contextState <- newIORef Nothing
            compactCalls <- newIORef (0 :: Int)
            seenHistory <- newIORef []
            let sender _request = do
                    modifyIORef' compactCalls (+ 1)
                    pure (Right remoteCompactionResponse)
                base = Backend \state _previous _submitted _ -> do
                    modifyIORef' seenHistory (<> [state])
                    pure $ successful state TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWithSender
                        Nothing
                        sender
                        (const (pure ()))
                        (pure params)
                        contextState
                        base
            result <- backend.submitTurn (initialBackendSnapshot oldHistory) Nothing inputs
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef seenHistory >>= \case
                [fitted] ->
                    fitted.backendItems
                        `shouldSatisfy` hasCompactionCheckpoint
                seen ->
                    expectationFailure
                        ("expected one compacted continuation, got "
                            <> show (length seen))

        it "preserves typed provider failures from automatic compaction" do
            let history = [userTextItem "old"]
                compactError =
                    ProviderError UsageLimitReached "quota exhausted" (Just 120)
            contextState <- newIORef
                (Just (reportedOccupancy
                    codexAutoCompactTokenLimit (length history)))
            let compactAction =
                    pure (Left compactError)
                base = Backend \_ _ _ _ ->
                    error "failed compaction should not submit a model request"
                backend =
                    autoCompactOpenAiBackendWithApi
                        compactAction contextState base
            backend.submitTurn (initialBackendSnapshot history) Nothing
                [UserMessage "new"] (const (pure ()))
                `shouldReturn`
                    Left (ProviderError UsageLimitReached
                        "automatic compaction failed: quota exhausted"
                        (Just 120))

        it "rejects compacted snapshots at or above the trigger" do
            let history = [userTextItem "old"]
                oldContextState =
                    Just (reportedOccupancy
                        codexAutoCompactTokenLimit (length history))
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWith
                        compactAction contextState base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing
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
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        }
                backend =
                    autoCompactOpenAiBackendWith
                        compactAction contextState base
            result <- backend.submitTurn (initialBackendSnapshot history) Nothing [UserMessage "new"]
                (const (pure ()))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1

responseItemHasTaskPlan :: ResponseItem -> Bool
responseItemHasTaskPlan = \case
    MessageItem message ->
        any isTaskPlanContextText (responseMessageTexts message)
    _ -> False

responseMessageHasText :: Text -> ResponseMessage -> Bool
responseMessageHasText expected =
    elem expected . responseMessageTexts

responseMessageTexts :: ResponseMessage -> [Text]
responseMessageTexts message =
    case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts ->
            [ text
            | part <- parts
            , text <- case part of
                InputTextPart{text} -> [text]
                OutputTextPart{text} -> [text]
                PlainTextPart{text} -> [text]
                _ -> []
            ]

successful
    :: BackendSnapshot
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
    decodeResponseFixture $ Aeson.object
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
        ]

decodeResponseFixture :: Aeson.Value -> Response
decodeResponseFixture fixture =
    case Hermes.decodeEither responseDecoder
            (LBS.toStrict (Aeson.encode fixture)) of
        Right response -> response
        Left err -> error (Text.unpack (Hermes.jsonErrorMessage err))

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
    decodeResponseFixture $ Aeson.object
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
        ]
