module Agent.Runtime.Compaction.Projection
    ( automaticCompactionHeadroom
    , compactedSnapshotThresholdError
    , hasFocus
    , occupancyOnTurnFinished
    , occupancyForSubmission
    , occupancySnapshot
    , projectRequestTokens
    , providerLabel
    , reportedContextTokens
    , requestTooLargeError
    , requireHistory
    , requireTokenProvider
    , toolContinuationTooLargeError
    ) where

import Agent.Runtime.Compaction.Types
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( BackendContinuation(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , TokenUsage(..)
    , TurnInput
    , TurnOutput(..)
    )
import Agent.OpenAI.Compaction
    ( estimateItemsTokens
    , estimateRequestTokensWithItems
    )
import Agent.Provider (Provider(..), TokenProvider)
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseCreateParams, ResponseItem)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Last provider-reported context occupancy after a committed response.
-- @input_tokens@ already includes instructions, tools, and skills in the
-- request; @output_tokens@ remain in the next turn's context.
reportedContextTokens :: TokenUsage -> Maybe Int
reportedContextTokens usage
    | usage.inputTokens <= 0 && usage.outputTokens <= 0 = Nothing
    | otherwise =
        Just (max 0 usage.inputTokens + max 0 usage.outputTokens)

occupancySnapshot :: BackendResult -> Maybe OccupancySnapshot
occupancySnapshot result
    | Text.null output.responseId = Nothing
    | otherwise =
        output.contextUsage >>= reportedContextTokens >>= \tokens ->
            Just
                ((reportedOccupancy tokens
                    (length state.backendItems))
                    { occupancyCheckpoint =
                        case state.backendContinuation of
                            Nothing -> Nothing
                            Just _ -> Just state
                    })
  where
    state = result.backendState
    output = result.backendOutput

-- | Completion must not rebind a live process's measurement to a revision
-- assigned by the host store: that revision may force a fresh process import.
-- Preserve the middleware's provider checkpoint, but clear missing usage.
occupancyOnTurnFinished
    :: BackendSnapshot
    -> TurnOutput
    -> Maybe OccupancySnapshot
    -> Maybe OccupancySnapshot
occupancyOnTurnFinished state output cached = do
    measured <- occupancySnapshot (BackendResult output state)
    case cached of
        Just existing | Just _ <- existing.occupancyCheckpoint ->
            Just existing
        _ -> Just measured

-- | Discard measurements of a different live provider context before
-- projecting a submission. In particular, a cleared Claude continuation
-- means the host history will be imported into a fresh process, so the old
-- process's potentially compacted context size cannot describe that prompt.
occupancyForSubmission
    :: BackendSnapshot
    -> Maybe Text
    -> Maybe OccupancySnapshot
    -> Maybe OccupancySnapshot
occupancyForSubmission state previous occupancy = do
    snapshot <- occupancy
    case snapshot.occupancyCheckpoint of
        Nothing -> Just snapshot
        Just checkpoint
            | checkpoint /= state -> Nothing
            | otherwise ->
                case checkpoint.backendContinuation of
                    Just continuation
                        | continuation.continuationProvider == "anthropic.claude-code"
                        , previous /= Just continuation.continuationToken ->
                            Nothing
                        | Just requested <- previous
                        , requested /= continuation.continuationToken ->
                            Nothing
                        | otherwise -> Just snapshot
                    Nothing -> Nothing

-- | Project the next request from last occupancy when that snapshot still
-- describes @history@. Provider-reported occupancy already includes
-- instructions, tools, and skills, so only unsent items are estimated.
-- Estimated compaction snapshots are items-only; recompute against the
-- complete request when params are available so those fields are counted.
projectRequestTokens
    :: Maybe ResponseCreateParams
    -> Maybe OccupancySnapshot
    -> [ResponseItem]
    -> [TurnInput]
    -> Int
projectRequestTokens params occupancy history inputs =
    case occupancy of
        Just snapshot
            | occupancyMatchesHistory history snapshot
            , snapshot.occupancyTokens > 0
            , snapshot.occupancyKind == ReportedOccupancy ->
                snapshot.occupancyTokens + estimateItemsTokens pendingItems
        Just snapshot
            | occupancyMatchesHistory history snapshot
            , snapshot.occupancyTokens > 0
            , snapshot.occupancyKind == EstimatedOccupancy
            , Nothing <- params ->
                snapshot.occupancyTokens + estimateItemsTokens pendingItems
        _ ->
            case params of
                Just requestParams ->
                    estimateRequestTokensWithItems
                        requestParams
                        (history <> pendingItems)
                Nothing ->
                    estimateItemsTokens (history <> pendingItems)
  where
    pendingItems = turnInputsToItems inputs

automaticCompactionHeadroom :: Int -> Int
automaticCompactionHeadroom tokenLimit =
    max 1_024 (max 0 tokenLimit `div` 10)

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
    GeminiProvider -> "gemini"
    ClaudeCodeProvider -> "claude-code"

requestTooLargeError :: Text -> ApiError
requestTooLargeError label =
    ProviderError InvalidRequestError
        (label <> " request cannot fit within the model context window")
        Nothing

compactedSnapshotThresholdError :: Int -> Int -> ApiError
compactedSnapshotThresholdError tokenLimit compactedTokens =
    ProviderError InvalidRequestError
        ( "compaction produced a "
            <> Text.pack (show compactedTokens)
            <> "-token snapshot at or above the automatic compaction threshold "
            <> Text.pack (show tokenLimit)
            <> "; increase --compact-threshold"
        )
        Nothing

toolContinuationTooLargeError :: ApiError
toolContinuationTooLargeError =
    ProviderError InvalidRequestError
        "tool continuation request cannot fit within the model context window \
        \even after truncating completed tool output"
        Nothing

hasFocus :: Maybe Text -> Bool
hasFocus =
    maybe False (not . Text.null . Text.strip)
