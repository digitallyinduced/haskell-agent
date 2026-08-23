-- | Run provider compaction and rewrite the local transcript.
module Agent.CLI.Compaction
    ( CompactOutcome(..)
    , OpenAiCompactionSender
    , codexAutoCompactTokenLimit
    , autoCompactOpenAiBackend
    , autoCompactOpenAiBackendWithThreshold
    , autoCompactOpenAiBackendWithSender
    , autoCompactOpenAiBackendWith
    , autoCompactOpenAiBackendWithApi
    , compactOpenAIWith
    , installCompactOutcome
    , runProviderCompact
    , runProviderCompactWith
    ) where

import Agent.CLI.Error (formatApiError)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnInput(..)
    , emptyTokenUsage
    )
import qualified Agent.OpenAI.Client as OpenAI
import Agent.OpenAI.Compaction
    ( buildLocalCompactedHistory
    , buildRemoteCompactedHistory
    , buildRemoteCompactionRequest
    , extractRemoteCompactionItem
    , estimateItemsTokens
    , remoteCompactionRetainedTokenBudget
    , summarizationPrompt
    , trimRemoteCompactionHistoryToFit
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
    , toolResultToItem
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
import Control.Exception.Safe (mask, onException)
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

codexAutoCompactTokenLimit :: Int
codexAutoCompactTokenLimit = defaultCodexAutoCompactTokenLimit

data CompactOutcome = CompactOutcome
    { compactBeforeTokens :: !Int
    , compactAfterTokens :: !Int
    , compactHistory :: ![ResponseItem]
    , compactSummary :: !Text
    } deriving (Eq, Show)

type OpenAiCompactionSender =
    ResponseCreateParams -> IO (Either ApiError Response)

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
runProviderCompactWith openAiSender recordUsage provider tokenProvider
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
        | otherwise =
            mapCompactAttemptError formatApiError
                <$> summarizeLocalAttempt
                    sender
                    params
                    history
                    (estimateItemsTokens history)
                    focus

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
    -> Maybe (IORef (Maybe (Int, Int)))
    -> (Maybe Text -> IO (Either Text CompactOutcome))
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
installCompactOutcome previous transcript contextTokens runCompact focus =
    mask \restore -> do
        result <- restore (runCompact focus)
        case result of
            Left _ -> pure ()
            Right outcome -> do
                writeIORef previous Nothing
                writeIORef transcript outcome.compactHistory
                case contextTokens of
                    Nothing -> pure ()
                    Just ref ->
                        writeIORef ref $ Just
                            ( outcome.compactAfterTokens
                            , length outcome.compactHistory
                            )
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
compactRemoteV2Attempt send params history before
    | null history =
        pure $ CompactAttempt emptyTokenUsage $
            Left (ProviderError InvalidRequestError "nothing to compact" Nothing)
    | otherwise = do
        let requestHistory =
                trimRemoteCompactionHistoryToFit
                    (codexEffectiveContextWindowFor params.model)
                    params.instructions
                    history
            request = buildRemoteCompactionRequest params requestHistory
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
                                    remoteCompactionRetainedTokenBudget
                                    history
                                    checkpoint
                        Right CompactOutcome
                            { compactBeforeTokens = before
                            , compactAfterTokens = estimateItemsTokens items
                            , compactHistory = items
                            , compactSummary = "Context compacted remotely."
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
summarizeLocalAttempt send params history before focus
    | null history =
        pure $ CompactAttempt emptyTokenUsage $
            Left (ProviderError InvalidRequestError "nothing to compact" Nothing)
    | otherwise = do
        let summaryPrompt = summarizationPrompt focus
            ResponseCreateParams{..} = params
            summaryParams =
                ResponseCreateParams
                    { tools = Nothing
                    , parallelToolCalls = Just False
                    -- The ChatGPT Codex REST endpoint only accepts streaming
                    -- Responses requests, and this client decodes its SSE result.
                    , stream = Just True
                    , ..
                    }
            request =
                withRequestInput
                    summaryParams
                    (history <> [userTextItem summaryPrompt])
        send request >>= \case
            Left err ->
                pure (CompactAttempt emptyTokenUsage (Left err))
            Right response ->
                pure CompactAttempt
                    { compactAttemptUsage = responseTokenUsage response
                    , compactAttemptResult =
                        case assistantTextFromResponse response of
                            Nothing ->
                                Left (ProviderError ApiErrorType
                                    "compaction produced no summary text"
                                    Nothing)
                            Just summary ->
                                let items =
                                        buildLocalCompactedHistory
                                            6 history summary
                                in Right CompactOutcome
                                    { compactBeforeTokens = before
                                    , compactAfterTokens =
                                        estimateItemsTokens items
                                    , compactHistory = items
                                    , compactSummary = summary
                                    }
                    }

autoCompactOpenAiBackend
    :: TokenProvider
    -> IO ResponseCreateParams
    -> IORef (Maybe (Int, Int))
    -> Backend [ResponseItem]
    -> Backend [ResponseItem]
autoCompactOpenAiBackend =
    autoCompactOpenAiBackendWithThreshold Nothing

-- | Wrap an OpenAI backend with client-managed automatic compaction. A
-- configured threshold overrides the current model's default.
autoCompactOpenAiBackendWithThreshold
    :: Maybe Int
    -> TokenProvider
    -> IO ResponseCreateParams
    -> IORef (Maybe (Int, Int))
    -> Backend [ResponseItem]
    -> Backend [ResponseItem]
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
    -> IORef (Maybe (Int, Int))
    -> Backend [ResponseItem]
    -> Backend [ResponseItem]
autoCompactOpenAiBackendWithSender configuredThreshold send recordUsage
        getParams contextTokensRef backend =
    autoCompactOpenAiBackendWithLimit
        getLimit
        compactAction
        recordUsage
        contextTokensRef
        backend
  where
    getLimit = do
        params <- getParams
        pure $ fromMaybe
            (codexAutoCompactTokenLimitFor params.model)
            configuredThreshold
    compactAction history = do
        params <- getParams
        compactRemoteV2Attempt
            send
            params
            history
            (estimateItemsTokens history)

autoCompactOpenAiBackendWith
    :: IO (Either Text CompactOutcome)
    -> IORef (Maybe (Int, Int))
    -> Backend [ResponseItem]
    -> Backend [ResponseItem]
autoCompactOpenAiBackendWith compactAction =
    autoCompactOpenAiBackendWithLimit
        (pure codexAutoCompactTokenLimit)
        (const
            (CompactAttempt emptyTokenUsage
                <$> fmap (either (Left . textCompactionError) Right)
                    compactAction))
        (const (pure ()))
  where
    textCompactionError message =
        ProviderError ApiErrorType message Nothing

autoCompactOpenAiBackendWithApi
    :: IO (Either ApiError CompactOutcome)
    -> IORef (Maybe (Int, Int))
    -> Backend [ResponseItem]
    -> Backend [ResponseItem]
autoCompactOpenAiBackendWithApi compactAction =
    autoCompactOpenAiBackendWithLimit
        (pure codexAutoCompactTokenLimit)
        (const (CompactAttempt emptyTokenUsage <$> compactAction))
        (const (pure ()))

autoCompactOpenAiBackendWithLimit
    :: IO Int
    -> ([ResponseItem] -> IO (CompactAttempt ApiError))
    -> (TokenUsage -> IO ())
    -> IORef (Maybe (Int, Int))
    -> Backend [ResponseItem]
    -> Backend [ResponseItem]
autoCompactOpenAiBackendWithLimit getLimit compactAction recordUsage
        contextTokensRef
        (Backend submit) =
    Backend \history previous inputs onEvent -> do
        contextState <- readIORef contextTokensRef
        tokenLimit <- getLimit
        let historyLength = length history
            pendingItems = turnInputsToItems inputs
            projectedTokens =
                case contextState of
                    Just (tokens, observedLength)
                        | observedLength == historyLength ->
                            tokens + estimateItemsTokens pendingItems
                    _ ->
                        estimateItemsTokens (history <> pendingItems)
            shouldCompact =
                not (null history)
                    && projectedTokens >= tokenLimit
        if shouldCompact
            then if any isCompletedTool inputs
                then compactToolContinuation
                    contextState history inputs onEvent
                else compactThenSubmit
                    contextState history inputs onEvent
            else submitAndTrack
                contextState history previous inputs onEvent
  where
    runCompaction history =
        (.compactAttemptResult)
            <$> runAttemptAndRecord recordUsage (compactAction history)

    isCompletedTool = \case
        CompletedTool{} -> True
        _ -> False

    -- Tool outputs must be part of the checkpoint, but the wrapped backend has
    -- not committed them yet. Absorb them into history before compaction, then
    -- resume from the new checkpoint without sending them a second time.
    compactToolContinuation oldTokens oldHistory inputs onEvent = do
        mask \restore -> do
            let (toolItems, remainingInputs) = absorbCompletedTools inputs
                compactHistory = oldHistory <> toolItems
                rollback = writeIORef contextTokensRef oldTokens
            (do
                onEvent (ActivityUpdated "Compacting context…")
                restore (runCompaction compactHistory) >>= \case
                    Left err -> do
                        rollback
                        pure (Left (automaticCompactionError err))
                    Right outcome ->
                        installSubmitAndTrack
                            restore
                            rollback
                            outcome
                            remainingInputs
                            onEvent
                ) `onException` rollback

    compactThenSubmit oldTokens oldHistory inputs onEvent = do
        onEvent (ActivityUpdated "Compacting context…")
        runCompaction oldHistory >>= \case
                Left err ->
                    pure (Left (automaticCompactionError err))
                Right outcome ->
                    mask \restore -> do
                        let rollback = writeIORef contextTokensRef oldTokens
                        installSubmitAndTrack
                            restore
                            rollback
                            outcome
                            inputs
                            onEvent
                            `onException` rollback

    installSubmitAndTrack restore rollback outcome inputs onEvent = do
        let compactedHistory = outcome.compactHistory
        writeIORef contextTokensRef $
            Just (outcome.compactAfterTokens, length compactedHistory)
        result <- restore (submit compactedHistory Nothing inputs onEvent)
        case result of
            Left _ -> rollback
            Right _ -> invalidateContextTokens
        pure result

    submitAndTrack oldTokens history previous inputs onEvent = do
        result <-
            submit history previous inputs onEvent
                `onException` writeIORef contextTokensRef oldTokens
        case result of
            Left _ -> writeIORef contextTokensRef oldTokens
            Right _ -> invalidateContextTokens
        pure result

    -- Transcript state is committed by the loop after validating the response.
    -- Keep this separate cache conservative until it becomes part of that
    -- explicit state; otherwise cancellation or an invalid response id could
    -- leave token metadata describing an uncommitted transcript.
    invalidateContextTokens =
        writeIORef contextTokensRef Nothing

    absorbCompletedTools =
        foldr
            (\input (toolItems, otherInputs) ->
                case input of
                    CompletedTool result ->
                        (toolResultToItem result : toolItems, otherInputs)
                    _ ->
                        (toolItems, input : otherInputs))
            ([], [])

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

requireTokenProvider
    :: Provider
    -> Maybe TokenProvider
    -> ExceptT Text IO TokenProvider
requireTokenProvider provider =
    maybe (throwE (providerLabel provider <> " compact requires a token provider")) pure

requireHistory :: [ResponseItem] -> ExceptT Text IO ()
requireHistory history
    | null history = throwE "nothing to compact"
    | otherwise = pure ()

providerLabel :: Provider -> Text
providerLabel = \case
    OpenAIProvider -> "openai"
    XAIProvider -> "xai"
    OpenRouterProvider -> "openrouter"

hasFocus :: Maybe Text -> Bool
hasFocus =
    maybe False (not . Text.null . Text.strip)
