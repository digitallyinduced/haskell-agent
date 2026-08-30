module Agent.CLI.Compaction.Continuation
    ( boundCompletedToolContinuations
    ) where

import Agent.CLI.Compaction.Projection
    ( automaticCompactionHeadroom
    , projectRequestTokens
    , toolContinuationTooLargeError
    )
import Agent.CLI.Compaction.Types
import Agent.Loop (Backend(..), TurnInput(..))
import Agent.OpenAI.Compaction
    ( estimateItemsTokens
    , estimateRequestTokensWithItems
    , trimResponseHistoryToFit
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , toolCallResultImages
    )
import Control.Applicative ((<|>))
import Data.IORef (IORef, readIORef)
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text

-- A completed tool result must be submitted against its live response chain
-- before that call/output pair can be compacted. If the result itself would
-- overflow the model context, cap only its output text while preserving the
-- protocol identifiers and continuation id.
boundCompletedToolContinuations
    :: (ResponseCreateParams -> Int)
    -> IO ResponseCreateParams
    -> IORef (Maybe OccupancySnapshot)
    -> Backend
    -> Backend
boundCompletedToolContinuations contextWindowFor getParams contextTokensRef (Backend submit) =
    Backend \history previous inputs onEvent ->
        if not (any isCompletedTool inputs)
            then submit history previous inputs onEvent
            else do
                params <- getParams
                occupancy <- readIORef contextTokensRef
                let contextWindow = contextWindowFor params
                let liveChain = isJust previous
                    requestTokens candidate
                        | liveChain =
                            case occupancy of
                                Just snapshot
                                    | snapshot.occupancyLength == length history
                                    , snapshot.occupancyTokens > 0
                                    , snapshot.occupancyKind == ReportedOccupancy ->
                                        snapshot.occupancyTokens
                                            + estimateItemsTokens
                                                (turnInputsToItems candidate)
                                _ ->
                                    estimateRequestTokensWithItems
                                        params
                                        (turnInputsToItems candidate)
                        | otherwise =
                            projectRequestTokens
                                (Just params)
                                occupancy
                                history
                                candidate
                    truncated =
                        fromMaybe inputs $
                            fitCompletedToolOutputsToLimit
                                ( max 0
                                    ( contextWindow
                                        - automaticCompactionHeadroom
                                            contextWindow
                                    )
                                )
                                requestTokens
                                inputs
                            <|> fitCompletedToolOutputsToLimit
                                contextWindow
                                requestTokens
                                inputs
                if requestTokens inputs <= contextWindow
                    then submit history previous inputs onEvent
                    else if requestTokens truncated <= contextWindow
                        then submit history previous truncated onEvent
                        else submitTrimmedHistory
                            params
                            contextWindow
                            history
                            truncated
                            onEvent
  where
    isCompletedTool = \case
        CompletedTool{} -> True
        _ -> False

    submitTrimmedHistory params contextWindow history inputs onEvent = do
        let callIds = pendingToolCallIds inputs
            (danglingCalls, prefix) =
                partition (isPendingToolCall callIds) history
            trailing = danglingCalls <> turnInputsToItems inputs
            fittedPrefix =
                trimResponseHistoryToFit
                    contextWindow
                    params
                    trailing
                    prefix
            fittedHistory = fittedPrefix <> danglingCalls
            fittedTokens =
                estimateRequestTokensWithItems
                    params
                    (fittedHistory <> turnInputsToItems inputs)
        if fittedTokens <= contextWindow
            then submit fittedHistory Nothing inputs onEvent
            else pure (Left toolContinuationTooLargeError)

pendingToolCallIds :: [TurnInput] -> [Text]
pendingToolCallIds inputs =
    [ result.callId
    | CompletedTool result <- inputs
    ]

isPendingToolCall :: [Text] -> ResponseItem -> Bool
isPendingToolCall callIds = \case
    FunctionCallItem call -> call.callId `elem` callIds
    CustomToolCallItem call -> call.callId `elem` callIds
    _ -> False

fitCompletedToolOutputsToLimit
    :: Int
    -> ([TurnInput] -> Int)
    -> [TurnInput]
    -> Maybe [TurnInput]
fitCompletedToolOutputsToLimit limit requestTokens inputs
    | requestTokens inputs <= limit = Just inputs
    | requestTokens minimal > limit = Nothing
    | otherwise = Just (search 0 maximumOutputLength minimal)
  where
    maximumOutputLength =
        maximum
            ( 0 :
                [ Text.length result.output
                | CompletedTool result <- inputs
                ]
            )
    minimal = capCompletedToolOutputs 0 inputs

    search low high best
        | low > high = best
        | otherwise =
            let middle = (low + high) `div` 2
                candidate = capCompletedToolOutputs middle inputs
            in if requestTokens candidate <= limit
                then search (middle + 1) high candidate
                else search low (middle - 1) best

capCompletedToolOutputs :: Int -> [TurnInput] -> [TurnInput]
capCompletedToolOutputs maximumCharacters =
    map \case
        CompletedTool result -> CompletedTool (capToolResult result)
        input -> input
  where
    capToolResult result =
        let cappedOutput =
                capToolOutput maximumCharacters result.output
        in case toolCallResultImages result of
            [] -> ToolCallResult
                { callId = result.callId
                , output = cappedOutput
                , callKind = result.callKind
                }
            images -> ToolCallResultWithImages
                { callId = result.callId
                , output = cappedOutput
                , callKind = result.callKind
                , toolResultImages = images
                }

capToolOutput :: Int -> Text -> Text
capToolOutput maximumCharacters text
    | Text.length text <= maximumCharacters = text
    | maximumCharacters <= 0 = ""
    | maximumCharacters <= noticeLength =
        Text.take maximumCharacters toolOutputTruncationNotice
    | otherwise =
        Text.take
            (maximumCharacters - noticeLength)
            text
            <> toolOutputTruncationNotice
  where
    noticeLength = Text.length toolOutputTruncationNotice

toolOutputTruncationNotice :: Text
toolOutputTruncationNotice =
    "\n[tool output truncated to fit the model context]"
