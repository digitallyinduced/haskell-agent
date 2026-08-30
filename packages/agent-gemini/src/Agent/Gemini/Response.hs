-- | Incremental projection of native Gemini chunks into the harness' common
-- response model.
module Agent.Gemini.Response
    ( GeminiStreamEvent(..)
    , StreamState
    , initialStreamState
    , initialStreamStateWithCustomTools
    , applyChunk
    , buildResponse
    , streamReachedTerminal
    ) where

import Agent.Gemini.Types
import Agent.Responses.Types
import Control.Applicative ((<|>))
import Data.Aeson (Value(..), encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data GeminiStreamEvent
    = GeminiTextDelta !Text
    | GeminiReasoningDelta !Text
    | GeminiFunctionCallReady !FunctionCall
    deriving (Eq, Show)

data StreamState = StreamState
    { fallbackResponseId :: !Text
    , requestedModel :: !Text
    , observedResponseId :: !(Maybe Text)
    , observedModelVersion :: !(Maybe Text)
    , responseOutputRev :: ![ResponseItem]
    , pendingAssistantTextRev :: ![Text]
    , responseUsage :: !(Maybe UsageMetadata)
    , responseFinishReason :: !(Maybe Text)
    , responsePromptBlockReason :: !(Maybe Text)
    , nextFunctionCallIndex :: !Int
    , responseCustomToolNames :: !(Set Text)
    }

initialStreamState :: Text -> Text -> StreamState
initialStreamState model fallbackId =
    initialStreamStateWithCustomTools model fallbackId Set.empty

initialStreamStateWithCustomTools
    :: Text
    -> Text
    -> Set Text
    -> StreamState
initialStreamStateWithCustomTools model fallbackId customToolNames = StreamState
    { fallbackResponseId = fallbackId
    , requestedModel = model
    , observedResponseId = Nothing
    , observedModelVersion = Nothing
    , responseOutputRev = []
    , pendingAssistantTextRev = []
    , responseUsage = Nothing
    , responseFinishReason = Nothing
    , responsePromptBlockReason = Nothing
    , nextFunctionCallIndex = 0
    , responseCustomToolNames = customToolNames
    }

applyChunk
    :: StreamState
    -> GenerateContentResponse
    -> (StreamState, [GeminiStreamEvent])
applyChunk initial chunk =
    foldl' applyPart (withMetadata, []) parts
  where
    candidate = primaryCandidate chunk.nativeCandidates
    parts = maybe [] (.contentParts)
        (candidate >>= (.candidateContent))
    withMetadata = initial
        { observedResponseId =
            chunk.nativeResponseId <|> initial.observedResponseId
        , observedModelVersion =
            chunk.nativeModelVersion <|> initial.observedModelVersion
        , responseUsage =
            chunk.nativeUsageMetadata <|> initial.responseUsage
        , responseFinishReason =
            mergeFinishReason
                initial.responseFinishReason
                (candidate >>= (.candidateFinishReason))
        , responsePromptBlockReason =
            mergePromptBlockReason
                initial.responsePromptBlockReason
                chunk.nativePromptBlockReason
        }

primaryCandidate :: [Candidate] -> Maybe Candidate
primaryCandidate candidates =
    find ((== Just 0) . (.candidateIndex)) candidates
        <|> case candidates of
            first : _ -> Just first
            [] -> Nothing

applyPart
    :: (StreamState, [GeminiStreamEvent])
    -> Part
    -> (StreamState, [GeminiStreamEvent])
applyPart (state, events) part =
    ( withOutput
        { nextFunctionCallIndex =
            withOutput.nextFunctionCallIndex + functionCallCount
        }
    , events <> partEvents
    )
  where
    withOutput = foldl' appendOutputItem state outputItems
    text = part.partText >>= nonEmptyDelta
    functionCall = canonicalFunctionCall state <$> part.partFunctionCall
    functionCallCount = maybe 0 (const 1) functionCall
    signatureItems
        | part.partThought = []
        | otherwise = maybe [] (pure . signatureItem)
            part.partThoughtSignature
    textItems = case text of
        Just value
            | part.partThought ->
                [reasoningItem value part.partThoughtSignature]
            | otherwise ->
                [assistantMessageItem value]
        Nothing
            | part.partThought
            , Just signature <- part.partThoughtSignature ->
                [signatureItem signature]
            | otherwise -> []
    callItems = maybe [] (pure . FunctionCallItem) functionCall
    outputItems = signatureItems <> textItems <> callItems
    partEvents =
        maybe [] (\value ->
            [ if part.partThought
                then GeminiReasoningDelta value
                else GeminiTextDelta value
            ])
            text
        <> maybe [] (pure . GeminiFunctionCallReady) functionCall

canonicalFunctionCall
    :: StreamState
    -> NativeFunctionCall
    -> FunctionCall
canonicalFunctionCall state native = FunctionCall
    { itemId = Just callId
    , callId
    , name = native.functionCallName
    , namespace = Nothing
    , provider = Just "gemini"
    , arguments
    , encryptedFunctionArgs = Nothing
    , status = Just ItemCompleted
    }
  where
    callId = fromMaybe syntheticId
        (native.functionCallId >>= nonEmptyText)
    syntheticId =
        effectiveResponseId state
            <> "-call-"
            <> Text.pack (show state.nextFunctionCallIndex)
    arguments
        | native.functionCallName
            `Set.member` state.responseCustomToolNames =
            customToolInput native.functionCallArgs
        | otherwise = jsonText native.functionCallArgs

customToolInput :: Value -> Text
customToolInput = \case
    Object arguments
        | Just (String input) <- KeyMap.lookup "input" arguments ->
            input
    value -> jsonText value

assistantMessageItem :: Text -> ResponseItem
assistantMessageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        [OutputTextPart text Nothing Nothing]
    , role = RoleAssistant
    , status = Just ItemCompleted
    , phase = Nothing
    , passthrough = Nothing
    }

-- Gemini emits answer text as wire deltas, while a canonical Responses
-- message stores the complete text. Accumulate adjacent chunks in reverse and
-- flatten once, both preserving token-boundary whitespace and avoiding a
-- repeated strict-Text append for every network frame.
appendOutputItem :: StreamState -> ResponseItem -> StreamState
appendOutputItem state = \case
    MessageItem message
        | message.role == RoleAssistant
        , MessageContentParts [OutputTextPart text Nothing Nothing] <-
            message.content ->
            state
                { pendingAssistantTextRev =
                    text : state.pendingAssistantTextRev
                }
    item ->
        let flushed = flushAssistantText state
        in flushed
            { responseOutputRev = item : flushed.responseOutputRev }

flushAssistantText :: StreamState -> StreamState
flushAssistantText state = case state.pendingAssistantTextRev of
    [] -> state
    chunksRev ->
        state
            { responseOutputRev =
                assistantMessageItem (Text.concat (reverse chunksRev))
                    : state.responseOutputRev
            , pendingAssistantTextRev = []
            }

assembledOutput :: StreamState -> [ResponseItem]
assembledOutput state =
    let flushed = flushAssistantText state
    in reverse flushed.responseOutputRev

reasoningItem :: Text -> Maybe Text -> ResponseItem
reasoningItem text signature = ReasoningItemValue ReasoningItem
    { itemId = Nothing
    , summary = [ReasoningSummaryPart "summary_text" (Just text)]
    , content = Nothing
    , encryptedContent = signature
    , status = Just ItemCompleted
    }

signatureItem :: Text -> ResponseItem
signatureItem signature = ReasoningItemValue ReasoningItem
    { itemId = Nothing
    , summary = []
    , content = Nothing
    , encryptedContent = Just signature
    , status = Just ItemCompleted
    }

buildResponse :: ResponseCreateParams -> StreamState -> Response
buildResponse request state = Response
    { responseId = effectiveResponseId state
    , createdAt = 0
    , error = Nothing
    , incompleteDetails =
        IncompleteDetails <$> incompleteReason
    , instructions = Nothing
    , metadata = request.metadata
    , model = fromMaybe state.requestedModel state.observedModelVersion
    , object = "response"
    , output = assembledOutput state
    , parallelToolCalls = request.parallelToolCalls
    , temperature = request.temperature
    , toolChoice = request.toolChoice
    , tools = request.tools
    , topP = request.topP
    , background = Nothing
    , completedAt = Just 0
    , conversation = Nothing
    , maxOutputTokens = request.maxOutputTokens
    , maxToolCalls = request.maxToolCalls
    , moderation = Nothing
    , previousResponseId = Nothing
    , prompt = Nothing
    , promptCacheKey = request.promptCacheKey
    , promptCacheOptions = request.promptCacheOptions
    , promptCacheRetention = request.promptCacheRetention
    , reasoning = request.reasoning
    , safetyIdentifier = request.safetyIdentifier
    , serviceTier = Nothing
    , status = maybe ResponseCompleted
        (const ResponseIncomplete)
        incompleteReason
    , text = request.text
    , topLogprobs = Nothing
    , truncation = request.truncation
    , usage = responseUsage <$> state.responseUsage
    , user = request.user
    }
  where
    incompleteReason =
        (blockedReason <$> state.responsePromptBlockReason)
            <|> (state.responseFinishReason >>= finishReason)

-- A clean HTTP EOF is only a complete Gemini response after the provider has
-- supplied a candidate finish reason or a prompt-block reason. Without one,
-- EOF can mean that the streaming connection was truncated.
streamReachedTerminal :: StreamState -> Bool
streamReachedTerminal state =
    maybe False isTerminalFinishReason state.responseFinishReason
        || maybe False isTerminalPromptBlockReason
            state.responsePromptBlockReason

mergeFinishReason :: Maybe Text -> Maybe Text -> Maybe Text
mergeFinishReason previous current = case current of
    Just reason
        | isTerminalFinishReason reason -> Just reason
    _ -> previous

isTerminalFinishReason :: Text -> Bool
isTerminalFinishReason raw =
    let normalized = Text.toUpper (Text.strip raw)
    in not (Text.null normalized)
        && normalized /= "FINISH_REASON_UNSPECIFIED"

mergePromptBlockReason :: Maybe Text -> Maybe Text -> Maybe Text
mergePromptBlockReason previous current = case current of
    Just reason
        | isTerminalPromptBlockReason reason -> Just reason
    _ -> previous

isTerminalPromptBlockReason :: Text -> Bool
isTerminalPromptBlockReason raw =
    let normalized = Text.toUpper (Text.strip raw)
    in not (Text.null normalized)
        && normalized /= "BLOCK_REASON_UNSPECIFIED"

effectiveResponseId :: StreamState -> Text
effectiveResponseId state =
    fromMaybe state.fallbackResponseId state.observedResponseId

finishReason :: Text -> Maybe Text
finishReason raw = case Text.toUpper (Text.strip raw) of
    "" -> Nothing
    "STOP" -> Nothing
    "FINISH_REASON_UNSPECIFIED" -> Nothing
    "MAX_TOKENS" -> Just "max_output_tokens"
    "MALFORMED_FUNCTION_CALL" -> Just "invalid_tool_call"
    "UNEXPECTED_TOOL_CALL" -> Just "invalid_tool_call"
    reason -> Just (blockedReason reason)

blockedReason :: Text -> Text
blockedReason reason
    | Text.toUpper reason `elem` filterReasons = "content_filter"
    | otherwise = Text.toLower reason
  where
    filterReasons =
        [ "SAFETY"
        , "RECITATION"
        , "BLOCKLIST"
        , "PROHIBITED_CONTENT"
        , "SPII"
        , "IMAGE_SAFETY"
        , "IMAGE_PROHIBITED_CONTENT"
        ]

responseUsage :: UsageMetadata -> ResponseUsage
responseUsage usage = ResponseUsage
    { inputTokens = promptTokens
    , inputTokensDetails = Just TokenDetails
        { cachedTokens = usage.cachedContentTokenCount
        , reasoningTokens = Nothing
        }
    , outputTokens = outputTokens
    , outputTokensDetails = Just TokenDetails
        { cachedTokens = Nothing
        , reasoningTokens = usage.thoughtsTokenCount
        }
    , totalTokens =
        fromMaybe (promptTokens + outputTokens) usage.totalTokenCount
    }
  where
    promptTokens = fromMaybe 0 usage.promptTokenCount
    -- Google reports candidate and thought tokens separately; its documented
    -- total is prompt + thoughts + response candidates.
    outputTokens =
        fromMaybe 0 usage.candidatesTokenCount
            + fromMaybe 0 usage.thoughtsTokenCount

jsonText :: Value -> Text
jsonText =
    TextEncoding.decodeUtf8 . LBS.toStrict . encode

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value

-- Streamed text is a byte-for-byte delta. Trimming individual chunks would
-- collapse token-boundary spaces (for example, @"hello "@ <> @"world"@).
nonEmptyDelta :: Text -> Maybe Text
nonEmptyDelta value
    | Text.null value = Nothing
    | otherwise = Just value
