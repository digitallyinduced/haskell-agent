-- | Run provider compaction and rewrite the local transcript.
module Agent.CLI.Compaction
    ( CompactOutcome(..)
    , codexAutoCompactTokenLimit
    , autoCompactOpenAiBackend
    , autoCompactOpenAiBackendWithThreshold
    , autoCompactOpenAiBackendWith
    , autoCompactOpenAiBackendWithApi
    , compactOpenAIWith
    , runProviderCompact
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnInput(..)
    , TurnOutput(..)
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
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import Control.Exception.Safe (onException)
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

runProviderCompact
    :: Provider
    -> Maybe TokenProvider
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
runProviderCompact provider tokenProvider paramsRef transcriptRef focus =
    runExceptT do
        params <- lift (readIORef paramsRef)
        history <- lift (readIORef transcriptRef)
        case provider of
            OpenAIProvider ->
                compactOpenAI tokenProvider params history
                    (estimateItemsTokens history) focus
            XAIProvider ->
                compactLocalXai tokenProvider params history
                    (estimateItemsTokens history) focus
            OpenRouterProvider ->
                compactLocalOpenRouter tokenProvider params history
                    (estimateItemsTokens history) focus

compactOpenAI
    :: Maybe TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> ExceptT Text IO CompactOutcome
compactOpenAI =
    compactOpenAIWith sendOpenAIRemoteCompaction

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
    if hasFocus focus
        then summarizeLocal
            send
            provider
            params
            history
            before
            focus
        else compactRemoteV2
            send
            provider
            params
            history
            before

compactRemoteV2
    :: (TokenProvider -> ResponseCreateParams -> IO (Either ApiError Response))
    -> TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> ExceptT Text IO CompactOutcome
compactRemoteV2 send provider params history before =
    do
        requireHistory history
        withExceptT formatApiError $
            ExceptT (compactRemoteV2Api send provider params history before)

compactRemoteV2Api
    :: (TokenProvider -> ResponseCreateParams -> IO (Either ApiError Response))
    -> TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> IO (Either ApiError CompactOutcome)
compactRemoteV2Api send provider params history before =
    runExceptT do
        requireHistoryApi history
        let requestHistory =
                trimRemoteCompactionHistoryToFit
                    (codexEffectiveContextWindowFor params.model)
                    params.instructions
                    history
            request = buildRemoteCompactionRequest params requestHistory
        response <- ExceptT (send provider request)
        checkpoint <-
            either
                (throwE . protocolError)
                pure
                (extractRemoteCompactionItem response)
        let items =
                buildRemoteCompactedHistory
                    remoteCompactionRetainedTokenBudget
                    history
                    checkpoint
        pure CompactOutcome
            { compactBeforeTokens = before
            , compactAfterTokens = estimateItemsTokens items
            , compactHistory = items
            , compactSummary = "Context compacted remotely."
            }
  where
    protocolError message =
        ProviderError ApiErrorType message Nothing

compactLocalXai
    :: Maybe TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> ExceptT Text IO CompactOutcome
compactLocalXai tokenProvider params history before focus = do
    provider <- requireTokenProvider XAIProvider tokenProvider
    options <- lift XAI.clientOptionsFromEnv
    summarizeLocal
        (\tokens request ->
            runWithTokenProvider tokens \credential ->
                XAI.createResponseWith options credential request)
        provider
        params
        history
        before
        focus

compactLocalOpenRouter
    :: Maybe TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> ExceptT Text IO CompactOutcome
compactLocalOpenRouter tokenProvider params history before focus = do
    provider <- requireTokenProvider OpenRouterProvider tokenProvider
    options <- lift OpenRouter.clientOptionsFromEnv
    summarizeLocal
        (\tokens request ->
            runWithTokenProvider tokens \credential ->
                OpenRouter.createResponseWith options credential request)
        provider
        params
        history
        before
        focus

summarizeLocal
    :: (TokenProvider -> ResponseCreateParams -> IO (Either ApiError Response))
    -> TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> ExceptT Text IO CompactOutcome
summarizeLocal send provider params history before focus = do
    requireHistory history
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
    response <- withExceptT formatApiError $
        ExceptT (send provider request)
    summary <- case assistantTextFromResponse response of
        Nothing -> throwE "compaction produced no summary text"
        Just text -> pure text
    let items = buildLocalCompactedHistory 6 history summary
    pure CompactOutcome
        { compactBeforeTokens = before
        , compactAfterTokens = estimateItemsTokens items
        , compactHistory = items
        , compactSummary = summary
        }

autoCompactOpenAiBackend
    :: TokenProvider
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
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
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithThreshold configuredThreshold tokenProvider
        getParams transcriptRef contextTokensRef backend =
    autoCompactOpenAiBackendWithLimit
        getLimit
        compactAction
        transcriptRef
        contextTokensRef
        backend
  where
    getLimit = do
        params <- getParams
        pure $ fromMaybe
            (codexAutoCompactTokenLimitFor params.model)
            configuredThreshold
    compactAction = do
        params <- getParams
        history <- readIORef transcriptRef
        compactRemoteV2Api
            sendOpenAIRemoteCompaction
            tokenProvider
            params
            history
            (estimateItemsTokens history)

autoCompactOpenAiBackendWith
    :: IO (Either Text CompactOutcome)
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> Backend
    -> Backend
autoCompactOpenAiBackendWith =
    autoCompactOpenAiBackendWithApi
        . fmap (either (Left . textCompactionError) Right)
  where
    textCompactionError message =
        ProviderError ApiErrorType message Nothing

autoCompactOpenAiBackendWithApi
    :: IO (Either ApiError CompactOutcome)
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithApi =
    autoCompactOpenAiBackendWithLimit
        (pure codexAutoCompactTokenLimit)

autoCompactOpenAiBackendWithLimit
    :: IO Int
    -> IO (Either ApiError CompactOutcome)
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> Backend
    -> Backend
autoCompactOpenAiBackendWithLimit getLimit compactAction transcriptRef contextTokensRef
        (Backend submit) =
    Backend \previous inputs onEvent -> do
        contextState <- readIORef contextTokensRef
        history <- readIORef transcriptRef
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
                then compactToolContinuation contextState inputs onEvent
                else compactThenSubmit contextState inputs onEvent
            else submitAndTrack contextState previous inputs onEvent
  where
    isCompletedTool = \case
        CompletedTool{} -> True
        _ -> False

    -- Tool outputs must be part of the checkpoint, but the wrapped backend has
    -- not committed them yet. Absorb them into history before compaction, then
    -- resume from the new checkpoint without sending them a second time.
    compactToolContinuation oldTokens inputs onEvent = do
        oldHistory <- readIORef transcriptRef
        let (toolItems, remainingInputs) = absorbCompletedTools inputs
        writeIORef transcriptRef (oldHistory <> toolItems)
        onEvent (ActivityUpdated "Compacting context…")
        (compactAction `onException` writeIORef transcriptRef oldHistory) >>= \case
            Left err -> do
                writeIORef transcriptRef oldHistory
                pure (Left (automaticCompactionError err))
            Right outcome -> do
                writeIORef transcriptRef outcome.compactHistory
                submitAndTrack oldTokens Nothing remainingInputs onEvent

    compactThenSubmit oldTokens inputs onEvent = do
        onEvent (ActivityUpdated "Compacting context…")
        compactAction >>= \case
                Left err ->
                    pure (Left (automaticCompactionError err))
                Right outcome -> do
                    writeIORef transcriptRef outcome.compactHistory
                    submitAndTrack oldTokens Nothing inputs onEvent

    submitAndTrack oldTokens previous inputs onEvent = do
        result <- submit previous inputs onEvent
        case result of
            Left _ -> writeIORef contextTokensRef oldTokens
            Right output -> do
                historyLength <- length <$> readIORef transcriptRef
                writeIORef contextTokensRef $ Just
                    ( output.tokenUsage.inputTokens + output.tokenUsage.outputTokens
                    , historyLength
                    )
        pure result

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

requireHistoryApi :: [ResponseItem] -> ExceptT ApiError IO ()
requireHistoryApi history
    | null history =
        throwE $ ProviderError InvalidRequestError
            "nothing to compact"
            Nothing
    | otherwise = pure ()

providerLabel :: Provider -> Text
providerLabel = \case
    OpenAIProvider -> "openai"
    XAIProvider -> "xai"
    OpenRouterProvider -> "openrouter"

hasFocus :: Maybe Text -> Bool
hasFocus =
    maybe False (not . Text.null . Text.strip)

formatApiError :: ApiError -> Text
formatApiError err = Text.pack (show err)
