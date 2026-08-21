-- | Run provider compaction and rewrite the local transcript.
module Agent.CLI.Compaction
    ( CompactOutcome(..)
    , codexAutoCompactTokenLimit
    , autoCompactOpenAiBackend
    , autoCompactOpenAiBackendWith
    , runProviderCompact
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    )
import Agent.OpenAI.CompactClient
    ( CompactRequest(..)
    , compactConversation
    )
import Agent.OpenAI.Compaction
    ( buildLocalCompactedHistory
    , compactTranscriptAtLastCheckpoint
    , estimateItemsTokens
    , summarizationPrompt
    , userTextItem
    )
import Agent.OpenAI.LoopBackend
    ( assistantTextFromResponse
    , withRequestInput
    )
import Agent.OpenAI.Responses.Types
import Agent.Provider
    ( Credential
    , Provider(..)
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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

codexAutoCompactTokenLimit :: Int
codexAutoCompactTokenLimit = 244800

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
            OpenAIProvider -> do
                let effectiveHistory =
                        compactTranscriptAtLastCheckpoint history
                compactOpenAI tokenProvider params effectiveHistory
                    (estimateItemsTokens effectiveHistory) focus
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
compactOpenAI tokenProvider params history before focus = do
    provider <- requireTokenProvider OpenAIProvider tokenProvider
    requireHistory history
    let model = fromMaybe "gpt-5.6-luna" params.model
        focusedHistory = case focus of
            Just text | not (Text.null (Text.strip text)) ->
                history
                    <> [ userTextItem
                            ( "Compaction focus from the user: "
                                <> Text.strip text
                            )
                       ]
            _ -> history
        request =
            CompactRequest
                { compactModel = model
                , compactInput = focusedHistory
                , compactInstructions = params.instructions
                , compactTools = params.tools
                , compactParallelToolCalls =
                    fromMaybe True params.parallelToolCalls
                , compactReasoning = params.reasoning
                }
    items <- withExceptT formatApiError $
        ExceptT (compactConversation provider request)
    pure CompactOutcome
        { compactBeforeTokens = before
        , compactAfterTokens = estimateItemsTokens items
        , compactHistory = items
        , compactSummary =
            "remote compact returned "
                <> Text.pack (show (length items))
                <> " items"
        }

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
        (XAI.createResponseWith options)
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
        (OpenRouter.createResponseWith options)
        provider
        params
        history
        before
        focus

summarizeLocal
    :: (Credential -> ResponseCreateParams -> IO (Either ApiError Response))
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
                , ..
                }
        request =
            withRequestInput
                summaryParams
                (history <> [userTextItem summaryPrompt])
    response <- withExceptT formatApiError $
        ExceptT $
            runWithTokenProvider provider \credential ->
                send credential request
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
autoCompactOpenAiBackend tokenProvider getParams transcriptRef contextTokensRef
        backend =
    autoCompactOpenAiBackendWith compactAction transcriptRef contextTokensRef backend
  where
    compactAction = do
        paramsRef <- newIORef =<< getParams
        runProviderCompact OpenAIProvider (Just tokenProvider)
            paramsRef transcriptRef Nothing

autoCompactOpenAiBackendWith
    :: IO (Either Text CompactOutcome)
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> Backend
    -> Backend
autoCompactOpenAiBackendWith compactAction transcriptRef contextTokensRef
        (Backend submit) =
    Backend \previous inputs onEvent -> do
        contextState <- readIORef contextTokensRef
        historyLength <- length <$> readIORef transcriptRef
        case contextState of
            Just (tokens, observedLength)
                | observedLength == historyLength
                , tokens >= codexAutoCompactTokenLimit ->
                    compactThenSubmit contextState inputs onEvent
            _ -> submitAndTrack contextState previous inputs onEvent
  where
    compactThenSubmit oldTokens inputs onEvent = do
        onEvent (ActivityUpdated "Compacting context…")
        compactAction >>= \case
                Left err ->
                    pure $ Left $ ProviderError ApiErrorType
                        ("automatic compaction failed: " <> err) Nothing
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

formatApiError :: ApiError -> Text
formatApiError err = Text.pack (show err)
