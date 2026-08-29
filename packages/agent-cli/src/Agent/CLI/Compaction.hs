-- | Run provider compaction and rewrite the local transcript.
module Agent.CLI.Compaction
    ( AutomaticCompactionBoundary(..)
    , CompactOutcome(..)
    , CompactionInstall(..)
    , OpenAiCompactionSender
    , codexAutoCompactTokenLimit
    , autoCompactOpenAiBackend
    , autoCompactOpenAiBackendWithThreshold
    , autoCompactOpenAiBackendWithSender
    , autoCompactOpenAiBackendWithSenderAndHook
    , autoCompactOpenAiBackendWith
    , autoCompactOpenAiBackendWithApi
    , boundCompletedToolContinuations
    , compactOpenAIWith
    , installCompactOutcome
    , installLiveCompactOutcome
    , runProviderCompact
    , runProviderCompactWith
    , runProviderCompactWithContextWindow
    , runResponsesCompactWith
    , runResponsesCompactWithContextWindow
    , OccupancyKind(..)
    , OccupancySnapshot(..)
    , estimatedOccupancy
    , reportedOccupancy
    ) where

import Agent.CLI.Error (formatApiError)
import Agent.CLI.Compaction.Continuation
    ( boundCompletedToolContinuations
    )
import Agent.CLI.Compaction.Projection
import Agent.CLI.Compaction.Types
import Agent.CLI.Session.History
    ( LiveConversation
    , writeLivePreviousResponseId
    , writeLiveTranscript
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnInput(..)
    , TurnOutput(..)
    , emptyTokenUsage
    )
import qualified Agent.OpenAI.Client as OpenAI
import Agent.OpenAI.Compaction
    ( buildLocalCompactedHistoryToFit
    , buildRemoteCompactedHistory
    , buildRemoteCompactionRequest
    , extractRemoteCompactionItem
    , estimateItemsTokens
    , estimateRequestTokensWithItems
    , estimateResponseCreateParamsTokens
    , remoteCompactionRetainedTokenBudget
    , summarizationPrompt
    , trimRemoteCompactionRequestToFit
    , trimResponseHistoryToFit
    , userTextItem
    )
import Agent.OpenAI.ModelMetadata
    ( codexAutoCompactTokenLimitFor
    , codexEffectiveContextWindowFor
    , defaultCodexAutoCompactTokenLimit
    )
import Agent.Responses.LoopBackend
    ( assistantTextFromResponse
    , responseTokenUsage
    , turnInputsToItems
    , withRequestInput
    )
import Agent.Responses.Types
import Agent.Provider
    ( Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import qualified Agent.OpenRouter.Client as OpenRouter
import qualified Agent.OpenRouter.Options as OpenRouter
import qualified Agent.XAI.Client as XAI
import qualified Agent.XAI.Options as XAI
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT
    , throwE
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe (mask, onException)
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

codexAutoCompactTokenLimit :: Int
codexAutoCompactTokenLimit = defaultCodexAutoCompactTokenLimit

data CompactAttempt error = CompactAttempt
    { compactAttemptUsage :: !TokenUsage
    , compactAttemptResult :: !(Either error CompactOutcome)
    }

runProviderCompact
    :: Provider
    -> Maybe TokenProvider
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
runProviderCompact =
    runProviderCompactWith Nothing (const (pure ()))

-- | Run manual compaction, optionally routing OpenAI through the active model
-- session. Provider-reported compaction usage is recorded as soon as a
-- completed response arrives, including protocol-invalid responses.
runProviderCompactWith
    :: Maybe OpenAiCompactionSender
    -> (TokenUsage -> IO ())
    -> Provider
    -> Maybe TokenProvider
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
runProviderCompactWith =
    runProviderCompactWithContextWindow Nothing

-- | Variant of 'runProviderCompactWith' that receives the selected model's
-- machine-readable context window. Portable providers must not inherit the
-- unrelated Codex fallback when their model metadata is absent.
runProviderCompactWithContextWindow
    :: Maybe Int
    -> Maybe OpenAiCompactionSender
    -> (TokenUsage -> IO ())
    -> Provider
    -> Maybe TokenProvider
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
runProviderCompactWithContextWindow contextWindow openAiSender recordUsage
        provider tokenProvider
        paramsRef transcriptRef focus = do
    params <- readIORef paramsRef
    history <- readIORef transcriptRef
    attempt <- runAttemptAndRecord recordUsage $ case provider of
        OpenAIProvider ->
            case openAiSender of
                Just sender ->
                    compactTextAttempt sender params history focus
                Nothing ->
                    case tokenProvider of
                        Nothing ->
                            pure (compactTextFailure
                                "openai compact requires a token provider")
                        Just tokens ->
                            compactTextAttempt
                                (sendOpenAIRemoteCompaction tokens)
                                params
                                history
                                focus
        XAIProvider ->
            case tokenProvider of
                Nothing ->
                    pure (compactTextFailure
                        "xai compact requires a token provider")
                Just tokens -> do
                    options <- XAI.clientOptionsFromEnv
                    summarizeTextAttempt
                        (\request ->
                            runWithTokenProvider tokens \credential ->
                                XAI.createResponseWith options credential request)
                        params
                        history
                        focus
        OpenRouterProvider ->
            case tokenProvider of
                Nothing ->
                    pure (compactTextFailure
                        "openrouter compact requires a token provider")
                Just tokens -> do
                    options <- OpenRouter.clientOptionsFromEnv
                    summarizeTextAttempt
                        (\request ->
                            runWithTokenProvider tokens \credential ->
                                OpenRouter.createResponseWith
                                    options credential request)
                        params
                        history
                        focus
        ClaudeCodeProvider ->
            pure (compactTextFailure
                "Claude Code manages its own context; /compact is unavailable")
    pure attempt.compactAttemptResult
  where
    compactTextAttempt sender params history focus
        | null history = pure (compactTextFailure "nothing to compact")
        | otherwise =
            mapCompactAttemptError formatApiError
                <$> compactOpenAIAttempt
                    sender
                    params
                    history
                    (estimateItemsTokens history)
                    focus

    summarizeTextAttempt sender params history focus
        | null history = pure (compactTextFailure "nothing to compact")
        | otherwise = case configuredContextWindow params contextWindow of
            Left message -> pure (compactTextFailure message)
            Right limit ->
                mapCompactAttemptError formatApiError
                    <$> summarizePortableLocalAttempt
                        limit
                        sender
                        params
                        history
                        (estimateItemsTokens history)
                        focus

-- | Run local-summary compaction through any stateless Responses-compatible
-- sender, including user-configured local endpoints.
runResponsesCompactWith
    :: (ResponseCreateParams -> IO (Either ApiError Response))
    -> (TokenUsage -> IO ())
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
runResponsesCompactWith =
    runResponsesCompactWithContextWindow Nothing

runResponsesCompactWithContextWindow
    :: Maybe Int
    -> (ResponseCreateParams -> IO (Either ApiError Response))
    -> (TokenUsage -> IO ())
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
runResponsesCompactWithContextWindow contextWindow sender recordUsage
        paramsRef transcriptRef focus = do
    params <- readIORef paramsRef
    history <- readIORef transcriptRef
    attempt <- runAttemptAndRecord recordUsage $
        if null history
            then pure (compactTextFailure "nothing to compact")
            else case configuredContextWindow params contextWindow of
                Left message -> pure (compactTextFailure message)
                Right limit ->
                    mapCompactAttemptError formatApiError
                        <$> summarizePortableLocalAttempt
                            limit
                            sender
                            params
                            history
                            (estimateItemsTokens history)
                            focus
    pure attempt.compactAttemptResult

configuredContextWindow
    :: ResponseCreateParams
    -> Maybe Int
    -> Either Text Int
configuredContextWindow _params = \case
    Just contextWindow
        | contextWindow > 0 -> Right contextWindow
        | otherwise ->
            Left "model context_window must be positive"
    Nothing ->
        Left
            ( "the effective transport model has no context_window metadata; "
                <> "add a positive context_window to its model catalog entry "
                <> "before using /compact"
            )

compactTextFailure :: Text -> CompactAttempt Text
compactTextFailure message =
    CompactAttempt emptyTokenUsage (Left message)

mapCompactAttemptError
    :: (source -> target)
    -> CompactAttempt source
    -> CompactAttempt target
mapCompactAttemptError f attempt =
    CompactAttempt
        { compactAttemptUsage = attempt.compactAttemptUsage
        , compactAttemptResult =
            either (Left . f) Right attempt.compactAttemptResult
        }

recordAttemptUsage
    :: (TokenUsage -> IO ())
    -> CompactAttempt error
    -> IO ()
recordAttemptUsage recordUsage attempt =
    when (attempt.compactAttemptUsage /= emptyTokenUsage) $
        recordUsage attempt.compactAttemptUsage

-- Keep the model request cancellable, but once it returns a completed response
-- close the ordinary asynchronous-exception window before entering the usage
-- recorder.
runAttemptAndRecord
    :: (TokenUsage -> IO ())
    -> IO (CompactAttempt error)
    -> IO (CompactAttempt error)
runAttemptAndRecord recordUsage action =
    mask \restore -> do
        attempt <- restore action
        recordAttemptUsage recordUsage attempt
        pure attempt

-- | Install a successful manual compaction as one masked local state change.
-- Clearing the server continuation id together with replacing the transcript
-- prevents the next request from pairing compacted local history with the
-- pre-compaction response chain.
installCompactOutcome
    :: IORef (Maybe Text)
    -> IORef [ResponseItem]
    -> Maybe (IORef (Maybe OccupancySnapshot))
    -> (Maybe Text -> IO (Either Text CompactOutcome))
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
installCompactOutcome previous transcript =
    installCompactionOutcome \outcome -> do
        writeIORef previous Nothing
        writeIORef transcript outcome.compactHistory

installLiveCompactOutcome
    :: IORef LiveConversation
    -> Maybe (IORef (Maybe OccupancySnapshot))
    -> (Maybe Text -> IO (Either Text CompactOutcome))
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
installLiveCompactOutcome conversationRef =
    installCompactionOutcome \outcome -> do
        writeLivePreviousResponseId conversationRef Nothing
        writeLiveTranscript conversationRef outcome.compactHistory

installCompactionOutcome
    :: (CompactOutcome -> IO ())
    -> Maybe (IORef (Maybe OccupancySnapshot))
    -> (Maybe Text -> IO (Either Text CompactOutcome))
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
installCompactionOutcome installState contextTokens runCompact focus =
    mask \restore -> do
        result <- restore (runCompact focus)
        case result of
            Left _ -> pure ()
            Right outcome -> do
                installState outcome
                case contextTokens of
                    Nothing -> pure ()
                    Just ref ->
                        writeIORef ref $ Just $
                            estimatedOccupancy
                                outcome.compactAfterTokens
                                (length outcome.compactHistory)
        pure result

sendOpenAIRemoteCompaction
    :: TokenProvider
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
sendOpenAIRemoteCompaction =
    OpenAI.createCodexMessageWithProviderWithOptions
        OpenAI.remoteCompactionV2RequestOptions

compactOpenAIWith
    :: (TokenProvider -> ResponseCreateParams -> IO (Either ApiError Response))
    -> Maybe TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> ExceptT Text IO CompactOutcome
compactOpenAIWith send tokenProvider params history before focus = do
    provider <- requireTokenProvider OpenAIProvider tokenProvider
    requireHistory history
    attempt <- lift $
        compactOpenAIAttempt
            (send provider)
            params
            history
            before
            focus
    either (throwE . formatApiError) pure attempt.compactAttemptResult

compactOpenAIAttempt
    :: OpenAiCompactionSender
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> IO (CompactAttempt ApiError)
compactOpenAIAttempt send params history before focus
    | hasFocus focus =
        summarizeLocalAttempt send params history before focus
    | otherwise =
        compactRemoteV2Attempt send params history before

compactRemoteV2Attempt
    :: OpenAiCompactionSender
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> IO (CompactAttempt ApiError)
compactRemoteV2Attempt send params history before =
    compactRemoteV2AttemptWithRetainedBudget
        send
        params
        history
        before
        (const remoteCompactionRetainedTokenBudget)

compactRemoteV2AttemptWithRetainedBudget
    :: OpenAiCompactionSender
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> (ResponseItem -> Int)
    -> IO (CompactAttempt ApiError)
compactRemoteV2AttemptWithRetainedBudget
        send params history before retainedBudgetFor
    | null history =
        pure $ CompactAttempt emptyTokenUsage $
            Left (ProviderError InvalidRequestError "nothing to compact" Nothing)
    | otherwise = do
        let contextWindow =
                codexEffectiveContextWindowFor params.model
            requestHistory =
                trimRemoteCompactionRequestToFit
                    contextWindow
                    params
                    history
            request = buildRemoteCompactionRequest params requestHistory
        if estimateResponseCreateParamsTokens request > contextWindow
            then pure $ CompactAttempt emptyTokenUsage $
                Left (requestTooLargeError "remote compaction")
            else
                send request >>= \case
                    Left err ->
                        pure (CompactAttempt emptyTokenUsage (Left err))
                    Right response ->
                        pure CompactAttempt
                            { compactAttemptUsage = responseTokenUsage response
                            , compactAttemptResult = do
                                checkpoint <-
                                    either
                                        (Left . protocolError)
                                        Right
                                        (extractRemoteCompactionItem response)
                                let items =
                                        buildRemoteCompactedHistory
                                            ( min
                                                (max 0
                                                    (retainedBudgetFor checkpoint))
                                                ( max 0
                                                    ( contextWindow
                                                        - estimateRequestTokensWithItems
                                                            params
                                                            [checkpoint]
                                                    )
                                                )
                                            )
                                            history
                                            checkpoint
                                if
                                    estimateRequestTokensWithItems params items
                                        > contextWindow
                                    then
                                        Left
                                            (requestTooLargeError
                                                "remote compacted snapshot")
                                    else
                                        Right CompactOutcome
                                            { compactBeforeTokens = before
                                            , compactAfterTokens =
                                                estimateItemsTokens items
                                            , compactHistory = items
                                            , compactSummary =
                                                "Context compacted remotely."
                                            }
                            }
  where
    protocolError message =
        ProviderError ApiErrorType message Nothing

summarizeLocalAttempt
    :: OpenAiCompactionSender
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> IO (CompactAttempt ApiError)
summarizeLocalAttempt send params history before focus =
    summarizeLocalAttemptWith
        (codexEffectiveContextWindowFor params.model)
        id
        send
        params
        history
        before
        focus

summarizePortableLocalAttempt
    :: Int
    -> OpenAiCompactionSender
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> IO (CompactAttempt ApiError)
summarizePortableLocalAttempt contextWindow =
    summarizeLocalAttemptWith
        contextWindow
        (filter isPortableLocalSummaryItem)

summarizeLocalAttemptWith
    :: Int
    -> ([ResponseItem] -> [ResponseItem])
    -> OpenAiCompactionSender
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> IO (CompactAttempt ApiError)
summarizeLocalAttemptWith contextWindow prepareHistory send params history
        before focus
    | null history =
        pure $ CompactAttempt emptyTokenUsage $
            Left (ProviderError InvalidRequestError "nothing to compact" Nothing)
    | null summaryHistory =
        pure $ CompactAttempt emptyTokenUsage $
            Left
                (ProviderError InvalidRequestError
                    "nothing compatible to compact"
                    Nothing)
    | otherwise = do
        let summaryPrompt = summarizationPrompt focus
            ResponseCreateParams{..} = params
            summaryParams =
                ResponseCreateParams
                    { tools = Nothing
                    , toolChoice = Nothing
                    , maxToolCalls = Nothing
                    , parallelToolCalls = Just False
                    , previousResponseId = Nothing
                    , conversation = Nothing
                    -- The ChatGPT Codex REST endpoint only accepts streaming
                    -- Responses requests, and this client decodes its SSE result.
                    , stream = Just True
                    , ..
                    }
            promptItem = userTextItem summaryPrompt
            requestHistory =
                trimResponseHistoryToFit
                    contextWindow
                    summaryParams
                    [promptItem]
                    summaryHistory
            request =
                withRequestInput
                    summaryParams
                    (requestHistory <> [promptItem])
        if estimateResponseCreateParamsTokens request > contextWindow
            then pure $ CompactAttempt emptyTokenUsage $
                Left (requestTooLargeError "local compaction")
            else
                send request >>= \case
                    Left err ->
                        pure (CompactAttempt emptyTokenUsage (Left err))
                    Right response ->
                        pure CompactAttempt
                            { compactAttemptUsage = responseTokenUsage response
                            , compactAttemptResult =
                                if response.status /= ResponseCompleted
                                    then Left (ProviderError ApiErrorType
                                        ( "compaction response was not complete: "
                                            <> Text.pack (show response.status)
                                        )
                                        Nothing)
                                    else
                                        case assistantTextFromResponse response of
                                            Nothing ->
                                                Left (ProviderError ApiErrorType
                                                    "compaction produced no summary text"
                                                    Nothing)
                                            Just summary
                                                | Text.null
                                                    (Text.strip summary) ->
                                                    Left
                                                        (ProviderError ApiErrorType
                                                            "compaction produced no summary text"
                                                            Nothing)
                                            Just summary ->
                                                let items =
                                                        buildLocalCompactedHistoryToFit
                                                            contextWindow
                                                            params
                                                            6
                                                            history
                                                            summary
                                                in if
                                                    estimateRequestTokensWithItems
                                                        params
                                                        items
                                                        > contextWindow
                                                    then Left
                                                        (requestTooLargeError
                                                            "local compacted snapshot")
                                                    else Right CompactOutcome
                                                        { compactBeforeTokens = before
                                                        , compactAfterTokens =
                                                            estimateItemsTokens items
                                                        , compactHistory = items
                                                        , compactSummary = summary
                                                        }
                            }
  where
    summaryHistory = prepareHistory history

isPortableLocalSummaryItem :: ResponseItem -> Bool
isPortableLocalSummaryItem = \case
    -- OpenAI checkpoints are opaque provider protocol items. Preserve them
    -- for focused OpenAI summaries, but never replay them through
    -- xAI/OpenRouter or user-configured Responses endpoints.
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

autoCompactOpenAiBackend
    :: TokenProvider
    -> IO ResponseCreateParams
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackend =
    autoCompactOpenAiBackendWithThreshold Nothing

-- | Wrap an OpenAI backend with client-managed automatic compaction. A
-- configured threshold overrides the current model's default.
autoCompactOpenAiBackendWithThreshold
    :: Maybe Int
    -> TokenProvider
    -> IO ResponseCreateParams
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithThreshold configuredThreshold tokenProvider
        getParams contextTokensRef backend =
    autoCompactOpenAiBackendWithSender
        configuredThreshold
        (sendOpenAIRemoteCompaction tokenProvider)
        (const (pure ()))
        getParams
        contextTokensRef
        backend

-- | Automatic compaction with an injected Responses sender and an immediate
-- usage recorder. The root CLI uses this to share its active WebSocket session
-- and persist billable compaction usage even if the following turn fails.
autoCompactOpenAiBackendWithSender
    :: Maybe Int
    -> OpenAiCompactionSender
    -> (TokenUsage -> IO ())
    -> IO ResponseCreateParams
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithSender configuredThreshold send recordUsage
        getParams contextTokensRef backend =
    autoCompactOpenAiBackendWithSenderAndHook
        configuredThreshold
        send
        recordUsage
        getParams
        (\_outcome _inputs -> pure CompactionNotInstalled)
        contextTokensRef
        backend

-- | Variant that commits a successful compaction before submitting its
-- continuation. The root CLI uses the hook as a first-class persistence
-- boundary: once it returns, the compacted transcript must survive a failed
-- or cancelled continuation.
autoCompactOpenAiBackendWithSenderAndHook
    :: Maybe Int
    -> OpenAiCompactionSender
    -> (TokenUsage -> IO ())
    -> IO ResponseCreateParams
    -> (CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithSenderAndHook configuredThreshold send recordUsage
        getParams onCompacted contextTokensRef backend =
    rejectOversizedInitialRequest getParams $
        boundCompletedToolContinuations
            (codexEffectiveContextWindowFor . (.model))
            getParams
            contextTokensRef $
            autoCompactOpenAiBackendWithLimit
                getLimit
                True
                compactAction
                recordUsage
                estimateProjectedRequest
                onCompacted
                contextTokensRef
                backend
  where
    getLimit = do
        params <- getParams
        let configuredLimit =
                fromMaybe
                    (codexAutoCompactTokenLimitFor params.model)
                    configuredThreshold
        pure $
            min
                configuredLimit
                (codexEffectiveContextWindowFor params.model)
    compactAction history inputs = do
        params <- getParams
        tokenLimit <- getLimit
        let fixedRequestTokens =
                estimateRequestTokensWithItems params []
            pendingItems = turnInputsToItems inputs
            contextWindow =
                codexEffectiveContextWindowFor params.model
            checkpointBaseTokens checkpoint =
                estimateRequestTokensWithItems params [checkpoint]
            continuationBaseTokens checkpoint =
                estimateRequestTokensWithItems
                    params
                    (checkpoint : pendingItems)
            retainedBudget checkpoint =
                min remoteCompactionRetainedTokenBudget $
                    max 0
                        ( tokenLimit
                            - continuationBaseTokens checkpoint
                            - automaticCompactionHeadroom tokenLimit
                        )
            thresholdError minimumTokens =
                ProviderError InvalidRequestError
                    ( "automatic compaction threshold "
                        <> Text.pack (show tokenLimit)
                        <> " is below the minimum compacted request size of "
                        <> Text.pack (show minimumTokens)
                        <> " tokens; increase --compact-threshold"
                    )
                    Nothing
        if tokenLimit <= 0 || fixedRequestTokens >= tokenLimit
            then
                pure $ CompactAttempt emptyTokenUsage $
                    Left (thresholdError fixedRequestTokens)
            else do
                attempt <-
                    compactRemoteV2AttemptWithRetainedBudget
                        send
                        params
                        history
                        (estimateItemsTokens history)
                        retainedBudget
                pure $
                    case attempt.compactAttemptResult of
                        Right outcome ->
                            let continuationTokens =
                                    estimateRequestTokensWithItems
                                        params
                                        (outcome.compactHistory <> pendingItems)
                                (baseTokens, baseWithPendingTokens) =
                                    case reverse outcome.compactHistory of
                                        checkpoint : _ ->
                                            ( checkpointBaseTokens checkpoint
                                            , continuationBaseTokens checkpoint
                                            )
                                        [] ->
                                            ( fixedRequestTokens
                                            , estimateRequestTokensWithItems
                                                params
                                                pendingItems
                                            )
                            in if continuationTokens > contextWindow
                                then
                                    attempt
                                        { compactAttemptResult =
                                            Left
                                                (requestTooLargeError
                                                    "automatic compacted continuation")
                                        }
                                else if baseTokens >= tokenLimit
                                    then
                                        attempt
                                            { compactAttemptResult =
                                                Left
                                                    (thresholdError baseTokens)
                                            }
                                    else if
                                        baseWithPendingTokens < tokenLimit
                                            && continuationTokens >= tokenLimit
                                        then
                                            attempt
                                                { compactAttemptResult =
                                                    Left
                                                        (thresholdError
                                                            continuationTokens)
                                                }
                                        else attempt
                        _ -> attempt
    estimateProjectedRequest occupancy history inputs = do
        params <- getParams
        pure (projectRequestTokens (Just params) occupancy history inputs)

rejectOversizedInitialRequest
    :: IO ResponseCreateParams
    -> Backend
    -> Backend
rejectOversizedInitialRequest getParams (Backend submit) =
    Backend \history previous inputs onEvent ->
        if null history
            then do
                params <- getParams
                let requestTokens =
                        estimateRequestTokensWithItems
                            params
                            (turnInputsToItems inputs)
                    contextWindow =
                        codexEffectiveContextWindowFor params.model
                if requestTokens > contextWindow
                    then
                        pure $
                            Left $
                                requestTooLargeError "initial"
                    else submit history previous inputs onEvent
            else submit history previous inputs onEvent

autoCompactOpenAiBackendWith
    :: IO (Either Text CompactOutcome)
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackendWith compactAction =
    autoCompactOpenAiBackendWithLimit
        (pure codexAutoCompactTokenLimit)
        False
        (\_history _inputs ->
            (CompactAttempt emptyTokenUsage
                <$> fmap (either (Left . textCompactionError) Right)
                    compactAction))
        (const (pure ()))
        estimateProjectedFromCache
        (\_outcome _inputs -> pure CompactionNotInstalled)
  where
    textCompactionError message =
        ProviderError ApiErrorType message Nothing

autoCompactOpenAiBackendWithApi
    :: IO (Either ApiError CompactOutcome)
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithApi compactAction =
    autoCompactOpenAiBackendWithLimit
        (pure codexAutoCompactTokenLimit)
        False
        (\_history _inputs ->
            CompactAttempt emptyTokenUsage <$> compactAction)
        (const (pure ()))
        estimateProjectedFromCache
        (\_outcome _inputs -> pure CompactionNotInstalled)

autoCompactOpenAiBackendWithLimit
    :: IO Int
    -> Bool
    -> ([ResponseItem] -> [TurnInput] -> IO (CompactAttempt ApiError))
    -> (TokenUsage -> IO ())
    -> (Maybe OccupancySnapshot -> [ResponseItem] -> [TurnInput] -> IO Int)
    -> (CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithLimit getLimit absorbCompletedTools compactAction
        recordUsage estimateProjected
        onCompacted
        contextTokensRef
        (Backend submit) =
    Backend \history previous inputs onEvent -> do
        contextState <- readIORef contextTokensRef
        tokenLimit <- getLimit
        projectedTokens <- estimateProjected contextState history inputs
        let shouldCompact =
                not (null history)
                    && projectedTokens >= tokenLimit
        if shouldCompact
            && (absorbCompletedTools || not (any isCompletedTool inputs))
            then compactThenSubmit
                tokenLimit
                contextState history inputs onEvent
            else submitAndTrack
                contextState history previous inputs onEvent
  where
    runCompaction history inputs =
        (.compactAttemptResult)
            <$> runAttemptAndRecord recordUsage (compactAction history inputs)

    isCompletedTool = \case
        CompletedTool{} -> True
        _ -> False

    compactThenSubmit tokenLimit oldTokens oldHistory inputs onEvent = do
        onEvent (ActivityUpdated "Compacting context…")
        -- Tool results complete protocol units that are already represented by
        -- calls in oldHistory. Put those results behind their calls before
        -- requesting the checkpoint; replaying them after the checkpoint
        -- would create orphaned or duplicated tool output.
        let (completedTools, continuationInputs) =
                partitionCompletedTools inputs
            compactionHistory =
                oldHistory <> turnInputsToItems completedTools
        runCompaction compactionHistory continuationInputs >>= \case
                Left err ->
                    pure (Left (automaticCompactionError err))
                Right outcome
                    | outcome.compactAfterTokens >= tokenLimit ->
                        pure $
                            Left $
                                automaticCompactionError $
                                    compactedSnapshotThresholdError
                                        tokenLimit
                                        outcome.compactAfterTokens
                Right outcome ->
                    mask \restore -> do
                        let rollback = writeIORef contextTokensRef oldTokens
                        installSubmitAndTrack
                            restore
                            rollback
                            outcome
                            continuationInputs
                            onEvent

    partitionCompletedTools =
        foldr
            (\input (completed, pending) ->
                if isCompletedTool input
                    then (input : completed, pending)
                    else (completed, input : pending))
            ([], [])

    installSubmitAndTrack restore rollback outcome inputs onEvent = do
        let compactedHistory = outcome.compactHistory
            pendingItems = turnInputsToItems inputs
            durableHistory = compactedHistory <> pendingItems
            compactSnapshot =
                Just $
                    estimatedOccupancy
                        ( outcome.compactAfterTokens
                            + estimateItemsTokens pendingItems
                        )
                        (length durableHistory)
        writeIORef contextTokensRef compactSnapshot
        -- Match Codex's compaction lifecycle: install and durably record the
        -- checkpoint before issuing the model continuation. Root sessions put
        -- pending inputs in that checkpoint too, so a crash in this gap cannot
        -- lose the user's request. Lightweight wrappers defer installation and
        -- keep passing the inputs normally.
        installation <-
            onCompacted outcome inputs `onException` rollback
        let (continuationHistory, continuationInputs, rollbackIfDeferred) =
                case installation of
                    CompactionInstalled -> (durableHistory, [], pure ())
                    CompactionNotInstalled -> (compactedHistory, inputs, rollback)
        result <-
            restore
                (submit continuationHistory Nothing continuationInputs onEvent)
                `onException` rollbackIfDeferred
        case result of
            Left _ -> rollbackIfDeferred
            Right backendResult -> do
                writeIORef contextTokensRef $
                    occupancySnapshot backendResult <|> compactSnapshot
        pure result

    submitAndTrack oldTokens history previous inputs onEvent = do
        result <-
            submit history previous inputs onEvent
                `onException` writeIORef contextTokensRef oldTokens
        case result of
            Left _ -> writeIORef contextTokensRef oldTokens
            Right backendResult ->
                writeIORef contextTokensRef (occupancySnapshot backendResult)
        pure result

    automaticCompactionError = \case
        ProviderError errorType message retryAfter ->
            ProviderError errorType
                ("automatic compaction failed: " <> message)
                retryAfter
        ConnectionError message ->
            ConnectionError ("automatic compaction failed: " <> message)
        CredentialError message ->
            CredentialError ("automatic compaction failed: " <> message)
        err -> err

estimateProjectedFromCache
    :: Maybe OccupancySnapshot
    -> [ResponseItem]
    -> [TurnInput]
    -> IO Int
estimateProjectedFromCache occupancy history inputs =
    pure (projectRequestTokens Nothing occupancy history inputs)
