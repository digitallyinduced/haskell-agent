-- | Run provider compaction and rewrite the local transcript.
module Agent.CLI.Compaction
    ( CompactOutcome(..)
    , runProviderCompact
    ) where

import Agent.Error (ApiError)
import Agent.OpenAI.CompactClient
    ( CompactRequest(..)
    , compactConversation
    )
import Agent.OpenAI.Compaction
    ( buildLocalCompactedHistory
    , estimateItemsTokens
    , summarizationPrompt
    , userTextItem
    , userTextItem
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
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

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
runProviderCompact provider tokenProvider paramsRef transcriptRef focus = do
    params <- readIORef paramsRef
    history <- readIORef transcriptRef
    let before = estimateItemsTokens history
    case provider of
        OpenAIProvider ->
            compactOpenAI tokenProvider params history before focus
        XAIProvider ->
            compactLocalXai tokenProvider params history before focus
        OpenRouterProvider ->
            compactLocalOpenRouter tokenProvider params history before focus

compactOpenAI
    :: Maybe TokenProvider
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
    -> Maybe Text
    -> IO (Either Text CompactOutcome)
compactOpenAI tokenProvider params history before focus =
    case tokenProvider of
        Nothing -> pure (Left "openai compact requires a token provider")
        Just provider
            | null history -> pure (Left "nothing to compact")
            | otherwise -> do
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
                compactConversation provider request >>= \case
                    Left err -> pure (Left (formatApiError err))
                    Right items ->
                        pure $
                            Right
                                CompactOutcome
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
    -> IO (Either Text CompactOutcome)
compactLocalXai tokenProvider params history before focus =
    case tokenProvider of
        Nothing -> pure (Left "xai compact requires a token provider")
        Just provider -> do
            options <- XAI.clientOptionsFromEnv
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
    -> IO (Either Text CompactOutcome)
compactLocalOpenRouter tokenProvider params history before focus =
    case tokenProvider of
        Nothing -> pure (Left "openrouter compact requires a token provider")
        Just provider -> do
            options <- OpenRouter.clientOptionsFromEnv
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
    -> IO (Either Text CompactOutcome)
summarizeLocal send provider params history before focus
    | null history = pure (Left "nothing to compact")
    | otherwise = do
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
        result <- runWithTokenProvider provider \credential ->
            send credential request
        case result of
            Left err -> pure (Left (formatApiError err))
            Right response ->
                case assistantTextFromResponse response of
                    Nothing ->
                        pure (Left "compaction produced no summary text")
                    Just summary -> do
                        let items =
                                buildLocalCompactedHistory 6 history summary
                        pure $
                            Right
                                CompactOutcome
                                    { compactBeforeTokens = before
                                    , compactAfterTokens = estimateItemsTokens items
                                    , compactHistory = items
                                    , compactSummary = summary
                                    }

formatApiError :: ApiError -> Text
formatApiError err = Text.pack (show err)
