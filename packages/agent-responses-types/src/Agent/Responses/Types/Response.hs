module Agent.Responses.Types.Response
    ( Response(..), responseEncoder, responseDecoder, responseFragmentDecoder
    , ResponseStatus(..), responseStatusEncoder, responseStatusDecoder
    , ResponseError(..), responseErrorEncoder, responseErrorDecoder
    , IncompleteDetails(..), incompleteDetailsEncoder, incompleteDetailsDecoder
    , ResponseUsage(..), responseUsageEncoder, responseUsageDecoder
    , TokenDetails(..), tokenDetailsEncoder, tokenDetailsDecoder
    ) where

import Agent.Json (Extensions, RawJson, emptyExtensions, insertExtension)
import qualified Agent.Json.Decoder as D
import qualified Agent.Json.Encoder as E
import Agent.Responses.Types.Items
    ( ResponseInput, ResponseItem, responseInputDecoder, responseInputEncoder
    , responseItemDecoder, responseItemEncoder
    )
import Agent.Responses.Types.Request
    ( Conversation, Prompt, PromptCacheOptions, ReasoningConfig
    , ResponseTextConfig, ToolChoice
    , conversationDecoder, conversationEncoder, promptDecoder, promptEncoder
    , promptCacheOptionsDecoder, promptCacheOptionsEncoder
    , reasoningConfigDecoder, reasoningConfigEncoder
    , responseTextConfigDecoder, responseTextConfigEncoder
    , toolChoiceDecoder, toolChoiceEncoder
    )
import Agent.Responses.Types.Tools (ResponseTool, responseToolDecoder, responseToolEncoder)
import Data.Scientific (Scientific)
import Data.Text (Text)

data ResponseStatus
    = ResponseCompleted | ResponseFailed | ResponseInProgress | ResponseCancelled
    | ResponseQueued | ResponseIncomplete | ResponseStatusUnknown !Text
    deriving stock (Eq, Show)

responseStatusText :: ResponseStatus -> Text
responseStatusText = \case
    ResponseCompleted -> "completed"; ResponseFailed -> "failed"
    ResponseInProgress -> "in_progress"; ResponseCancelled -> "cancelled"
    ResponseQueued -> "queued"; ResponseIncomplete -> "incomplete"
    ResponseStatusUnknown value -> value

responseStatusEncoder :: E.Encoder ResponseStatus
responseStatusEncoder = E.contramap responseStatusText E.text

responseStatusDecoder :: D.Decoder ResponseStatus
responseStatusDecoder = D.mapDecoder parse D.text
  where
    parse = \case
        "completed" -> ResponseCompleted; "failed" -> ResponseFailed
        "in_progress" -> ResponseInProgress; "cancelled" -> ResponseCancelled
        "queued" -> ResponseQueued; "incomplete" -> ResponseIncomplete
        value -> ResponseStatusUnknown value

data ResponseError = ResponseError
    { code :: !Text, message :: !Text, extraFields :: !Extensions
    } deriving stock (Eq, Show)

responseErrorEncoder :: E.Encoder ResponseError
responseErrorEncoder = E.objectWithExtensions (.extraFields)
    [ E.field "code" E.text (.code), E.field "message" E.text (.message) ]

responseErrorDecoder :: D.Decoder ResponseError
responseErrorDecoder = D.object emptyState fields unknown finish
  where
    emptyState = (Nothing, Nothing, emptyExtensions)
    fields =
        [ D.field "code" (D.nullable D.text) (\v (c,m,e) -> Right (maybe c Just v,m,e))
        , D.field "message" (D.nullable D.text) (\v (c,m,e) -> Right (c,maybe m Just v,e))
        ]
    unknown = D.unknownField D.rawJson (\k v (c,m,e) -> Right (c,m,insertExtension k v e))
    finish (c,m,e) = Right (ResponseError (maybe "" id c) (maybe "" id m) e)

data IncompleteDetails = IncompleteDetails
    { reason :: !Text, extraFields :: !Extensions
    } deriving stock (Eq, Show)

incompleteDetailsEncoder :: E.Encoder IncompleteDetails
incompleteDetailsEncoder = E.objectWithExtensions (.extraFields)
    [ E.field "reason" E.text (.reason) ]

incompleteDetailsDecoder :: D.Decoder IncompleteDetails
incompleteDetailsDecoder = D.object emptyState
    [ D.field "reason" D.text (\v ( _, e) -> Right (Just v, e))
    ] (D.unknownField D.rawJson (\k v (r,e) -> Right (r, insertExtension k v e)))
    \(r,e) -> maybe (Left "missing required field reason") (\v -> Right (IncompleteDetails v e)) r
  where
    emptyState = (Nothing, emptyExtensions)

data TokenDetails = TokenDetails
    { cachedTokens :: !(Maybe Int), reasoningTokens :: !(Maybe Int)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

tokenDetailsEncoder :: E.Encoder TokenDetails
tokenDetailsEncoder = E.objectWithExtensions (.extraFields)
    [ E.optionalField "cached_tokens" E.int (.cachedTokens)
    , E.optionalField "reasoning_tokens" E.int (.reasoningTokens)
    ]

tokenDetailsDecoder :: D.Decoder TokenDetails
tokenDetailsDecoder = D.object (Nothing, Nothing, emptyExtensions)
    [ D.field "cached_tokens" (D.nullable D.int) (\v (_,r,e) -> Right (v,r,e))
    , D.field "reasoning_tokens" (D.nullable D.int) (\v (c,_,e) -> Right (c,v,e))
    ] (D.unknownField D.rawJson (\k v (c,r,e) -> Right (c,r,insertExtension k v e)))
    \(c,r,e) -> Right (TokenDetails c r e)

data ResponseUsage = ResponseUsage
    { inputTokens :: !Int, inputTokensDetails :: !(Maybe TokenDetails)
    , outputTokens :: !Int, outputTokensDetails :: !(Maybe TokenDetails)
    , totalTokens :: !Int, extraFields :: !Extensions
    } deriving stock (Eq, Show)

responseUsageEncoder :: E.Encoder ResponseUsage
responseUsageEncoder = E.objectWithExtensions (.extraFields)
    [ E.field "input_tokens" E.int (.inputTokens)
    , E.optionalField "input_tokens_details" tokenDetailsEncoder (.inputTokensDetails)
    , E.field "output_tokens" E.int (.outputTokens)
    , E.optionalField "output_tokens_details" tokenDetailsEncoder (.outputTokensDetails)
    , E.field "total_tokens" E.int (.totalTokens)
    ]

responseUsageDecoder :: D.Decoder ResponseUsage
responseUsageDecoder = responseUsageDecoderWith False

responseUsageFragmentDecoder :: D.Decoder ResponseUsage
responseUsageFragmentDecoder = responseUsageDecoderWith True

responseUsageDecoderWith :: Bool -> D.Decoder ResponseUsage
responseUsageDecoderWith permitMissing =
    D.object (Nothing,Nothing,Nothing,Nothing,Nothing,emptyExtensions) fields unknown finish
  where
    fields =
        [ D.field "input_tokens" D.int (\v (_,a,b,c,d,e) -> Right (Just v,a,b,c,d,e))
        , D.field "input_tokens_details" (D.nullable tokenDetailsDecoder) (\v (a,_,b,c,d,e) -> Right (a,v,b,c,d,e))
        , D.field "output_tokens" D.int (\v (a,b,_,c,d,e) -> Right (a,b,Just v,c,d,e))
        , D.field "output_tokens_details" (D.nullable tokenDetailsDecoder) (\v (a,b,c,_,d,e) -> Right (a,b,c,v,d,e))
        , D.field "total_tokens" D.int (\v (a,b,c,d,_,e) -> Right (a,b,c,d,Just v,e))
        ]
    unknown = D.unknownField D.rawJson (\k v (a,b,c,d,e,f) -> Right (a,b,c,d,e,insertExtension k v f))
    finish (a,b,c,d,e,f)
        | permitMissing = Right ResponseUsage
            { inputTokens = maybe 0 id a
            , inputTokensDetails = b
            , outputTokens = maybe 0 id c
            , outputTokensDetails = d
            , totalTokens = maybe 0 id e
            , extraFields = f
            }
        | otherwise = ResponseUsage
            <$> req "input_tokens" a
            <*> Right b
            <*> req "output_tokens" c
            <*> Right d
            <*> req "total_tokens" e
            <*> Right f
    req name = maybe (Left ("missing required field " <> name)) Right

data Response = Response
    { responseId :: !Text, createdAt :: !Scientific, error :: !(Maybe ResponseError)
    , incompleteDetails :: !(Maybe IncompleteDetails), instructions :: !(Maybe ResponseInput)
    , metadata :: !(Maybe Extensions), model :: !Text, object :: !Text
    , output :: ![ResponseItem], parallelToolCalls :: !(Maybe Bool)
    , temperature :: !(Maybe Scientific), toolChoice :: !(Maybe ToolChoice)
    , tools :: !(Maybe [ResponseTool]), topP :: !(Maybe Scientific)
    , background :: !(Maybe Bool), completedAt :: !(Maybe Scientific)
    , conversation :: !(Maybe Conversation), maxOutputTokens :: !(Maybe Int)
    , maxToolCalls :: !(Maybe Int), moderation :: !(Maybe RawJson)
    , previousResponseId :: !(Maybe Text), prompt :: !(Maybe Prompt)
    , promptCacheKey :: !(Maybe Text), promptCacheOptions :: !(Maybe PromptCacheOptions)
    , promptCacheRetention :: !(Maybe Text), reasoning :: !(Maybe ReasoningConfig)
    , safetyIdentifier :: !(Maybe Text), serviceTier :: !(Maybe Text)
    , status :: !ResponseStatus, text :: !(Maybe ResponseTextConfig)
    , topLogprobs :: !(Maybe Int), truncation :: !(Maybe Text)
    , usage :: !(Maybe ResponseUsage), user :: !(Maybe Text)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

responseEncoder :: E.Encoder Response
responseEncoder = E.objectWithExtensions (.extraFields)
    [ E.field "id" E.text (.responseId), E.field "created_at" E.scientific (.createdAt)
    , E.optionalField "error" responseErrorEncoder (.error)
    , E.optionalField "incomplete_details" incompleteDetailsEncoder (.incompleteDetails)
    , E.optionalField "instructions" responseInputEncoder (.instructions)
    , E.optionalField "metadata" extensionsEncoder (.metadata)
    , E.field "model" E.text (.model)
    , E.optionalField "object" E.text
        (\response ->
            if response.object == "response"
                then Nothing
                else Just response.object)
    , E.field "output" (E.list responseItemEncoder) (.output)
    , E.optionalField "parallel_tool_calls" E.bool (.parallelToolCalls)
    , E.optionalField "temperature" E.scientific (.temperature)
    , E.optionalField "tool_choice" toolChoiceEncoder (.toolChoice)
    , E.optionalField "tools" (E.list responseToolEncoder) (.tools)
    , E.optionalField "top_p" E.scientific (.topP), E.optionalField "background" E.bool (.background)
    , E.optionalField "completed_at" E.scientific (.completedAt), E.optionalField "conversation" conversationEncoder (.conversation)
    , E.optionalField "max_output_tokens" E.int (.maxOutputTokens), E.optionalField "max_tool_calls" E.int (.maxToolCalls)
    , E.optionalField "moderation" E.rawJson (.moderation), E.optionalField "previous_response_id" E.text (.previousResponseId)
    , E.optionalField "prompt" promptEncoder (.prompt), E.optionalField "prompt_cache_key" E.text (.promptCacheKey)
    , E.optionalField "prompt_cache_options" promptCacheOptionsEncoder (.promptCacheOptions)
    , E.optionalField "prompt_cache_retention" E.text (.promptCacheRetention), E.optionalField "reasoning" reasoningConfigEncoder (.reasoning)
    , E.optionalField "safety_identifier" E.text (.safetyIdentifier), E.optionalField "service_tier" E.text (.serviceTier)
    , E.field "status" responseStatusEncoder (.status), E.optionalField "text" responseTextConfigEncoder (.text)
    , E.optionalField "top_logprobs" E.int (.topLogprobs), E.optionalField "truncation" E.text (.truncation)
    , E.optionalField "usage" responseUsageEncoder (.usage), E.optionalField "user" E.text (.user)
    ]

-- Decoder state uses Maybe for required/defaulted fields and preserves unknown
-- values as opaque validated JSON extensions.
responseDecoder :: D.Decoder Response
responseDecoder = responseDecoderWith False

-- | Decoder for partial lifecycle snapshots carried by streaming events.
responseFragmentDecoder :: D.Decoder Response
responseFragmentDecoder = responseDecoderWith True

responseDecoderWith :: Bool -> D.Decoder Response
responseDecoderWith permitMissing = D.object empty fields unknown finish
  where
    empty = ResponseState Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing emptyExtensions
    fields =
      [ D.field "id" D.text (\v s -> Right s { rsId = Just v }), D.field "created_at" D.scientific (\v s -> Right s { rsCreatedAt = Just v })
      , D.field "error" (D.nullable responseErrorDecoder) (\v s -> Right s { rsError = v }), D.field "incomplete_details" (D.nullable incompleteDetailsDecoder) (\v s -> Right s { rsIncompleteDetails = v })
      , D.field "instructions" (D.nullable responseInputDecoder) (\v s -> Right s { rsInstructions = v }), D.field "metadata" (D.nullable extensionsDecoder) (\v s -> Right s { rsMetadata = v })
      , D.field "model" D.text (\v s -> Right s { rsModel = Just v }), D.field "object" D.text (\v s -> Right s { rsObject = Just v }), D.field "output" (D.array responseItemDecoder) (\v s -> Right s { rsOutput = Just v })
      , D.field "parallel_tool_calls" (D.nullable D.bool) (\v s -> Right s { rsParallelToolCalls = v }), D.field "temperature" (D.nullable D.scientific) (\v s -> Right s { rsTemperature = v })
      , D.field "tool_choice" (D.nullable toolChoiceDecoder) (\v s -> Right s { rsToolChoice = v }), D.field "tools" (D.nullable (D.array responseToolDecoder)) (\v s -> Right s { rsTools = v })
      , D.field "top_p" (D.nullable D.scientific) (\v s -> Right s { rsTopP = v }), D.field "background" (D.nullable D.bool) (\v s -> Right s { rsBackground = v })
      , D.field "completed_at" (D.nullable D.scientific) (\v s -> Right s { rsCompletedAt = v }), D.field "conversation" (D.nullable conversationDecoder) (\v s -> Right s { rsConversation = v })
      , D.field "max_output_tokens" (D.nullable D.int) (\v s -> Right s { rsMaxOutputTokens = v }), D.field "max_tool_calls" (D.nullable D.int) (\v s -> Right s { rsMaxToolCalls = v })
      , D.field "moderation" (D.nullable D.rawJson) (\v s -> Right s { rsModeration = v }), D.field "previous_response_id" (D.nullable D.text) (\v s -> Right s { rsPreviousResponseId = v })
      , D.field "prompt" (D.nullable promptDecoder) (\v s -> Right s { rsPrompt = v }), D.field "prompt_cache_key" (D.nullable D.text) (\v s -> Right s { rsPromptCacheKey = v })
      , D.field "prompt_cache_options" (D.nullable promptCacheOptionsDecoder) (\v s -> Right s { rsPromptCacheOptions = v }), D.field "prompt_cache_retention" (D.nullable D.text) (\v s -> Right s { rsPromptCacheRetention = v })
      , D.field "reasoning" (D.nullable reasoningConfigDecoder) (\v s -> Right s { rsReasoning = v }), D.field "safety_identifier" (D.nullable D.text) (\v s -> Right s { rsSafetyIdentifier = v })
      , D.field "service_tier" (D.nullable D.text) (\v s -> Right s { rsServiceTier = v }), D.field "status" responseStatusDecoder (\v s -> Right s { rsStatus = Just v })
      , D.field "text" (D.nullable responseTextConfigDecoder) (\v s -> Right s { rsText = v }), D.field "top_logprobs" (D.nullable D.int) (\v s -> Right s { rsTopLogprobs = v })
      , D.field "truncation" (D.nullable D.text) (\v s -> Right s { rsTruncation = v }), D.field "usage" (D.nullable usageDecoder) (\v s -> Right s { rsUsage = v })
      , D.field "user" (D.nullable D.text) (\v s -> Right s { rsUser = v }) ]
    unknown = D.unknownField D.rawJson (\k v s -> Right s { rsExtraFields = insertExtension k v s.rsExtraFields })
    usageDecoder
        | permitMissing = responseUsageFragmentDecoder
        | otherwise = responseUsageDecoder
    finish s
        | permitMissing = Right Response
        { responseId = maybe "" id s.rsId
        , createdAt = maybe 0 id s.rsCreatedAt
        , error = s.rsError
        , incompleteDetails = s.rsIncompleteDetails
        , instructions = s.rsInstructions
        , metadata = s.rsMetadata
        , model = maybe "" id s.rsModel
        , object = maybe "response" id s.rsObject
        , output = maybe [] id s.rsOutput
        , parallelToolCalls = s.rsParallelToolCalls
        , temperature = s.rsTemperature
        , toolChoice = s.rsToolChoice
        , tools = s.rsTools
        , topP = s.rsTopP
        , background = s.rsBackground
        , completedAt = s.rsCompletedAt
        , conversation = s.rsConversation
        , maxOutputTokens = s.rsMaxOutputTokens
        , maxToolCalls = s.rsMaxToolCalls
        , moderation = s.rsModeration
        , previousResponseId = s.rsPreviousResponseId
        , prompt = s.rsPrompt
        , promptCacheKey = s.rsPromptCacheKey
        , promptCacheOptions = s.rsPromptCacheOptions
        , promptCacheRetention = s.rsPromptCacheRetention
        , reasoning = s.rsReasoning
        , safetyIdentifier = s.rsSafetyIdentifier
        , serviceTier = s.rsServiceTier
        , status = maybe ResponseInProgress id s.rsStatus
        , text = s.rsText
        , topLogprobs = s.rsTopLogprobs
        , truncation = s.rsTruncation
        , usage = s.rsUsage
        , user = s.rsUser
        , extraFields = s.rsExtraFields
        }
        | otherwise = Response
            <$> req "id" s.rsId
            <*> req "created_at" s.rsCreatedAt
            <*> pure s.rsError
            <*> pure s.rsIncompleteDetails
            <*> pure s.rsInstructions
            <*> pure s.rsMetadata
            <*> req "model" s.rsModel
            <*> pure (maybe "response" id s.rsObject)
            <*> pure (maybe [] id s.rsOutput)
            <*> pure s.rsParallelToolCalls
            <*> pure s.rsTemperature
            <*> pure s.rsToolChoice
            <*> pure s.rsTools
            <*> pure s.rsTopP
            <*> pure s.rsBackground
            <*> pure s.rsCompletedAt
            <*> pure s.rsConversation
            <*> pure s.rsMaxOutputTokens
            <*> pure s.rsMaxToolCalls
            <*> pure s.rsModeration
            <*> pure s.rsPreviousResponseId
            <*> pure s.rsPrompt
            <*> pure s.rsPromptCacheKey
            <*> pure s.rsPromptCacheOptions
            <*> pure s.rsPromptCacheRetention
            <*> pure s.rsReasoning
            <*> pure s.rsSafetyIdentifier
            <*> pure s.rsServiceTier
            <*> req "status" s.rsStatus
            <*> pure s.rsText
            <*> pure s.rsTopLogprobs
            <*> pure s.rsTruncation
            <*> pure s.rsUsage
            <*> pure s.rsUser
            <*> pure s.rsExtraFields
    req name = maybe (Left ("missing required field " <> name)) Right

data ResponseState = ResponseState
    { rsId :: Maybe Text, rsCreatedAt :: Maybe Scientific, rsError :: Maybe ResponseError, rsIncompleteDetails :: Maybe IncompleteDetails, rsInstructions :: Maybe ResponseInput, rsMetadata :: Maybe Extensions, rsModel :: Maybe Text, rsObject :: Maybe Text, rsOutput :: Maybe [ResponseItem], rsParallelToolCalls :: Maybe Bool, rsTemperature :: Maybe Scientific, rsToolChoice :: Maybe ToolChoice, rsTools :: Maybe [ResponseTool], rsTopP :: Maybe Scientific, rsBackground :: Maybe Bool, rsCompletedAt :: Maybe Scientific, rsConversation :: Maybe Conversation, rsMaxOutputTokens :: Maybe Int, rsMaxToolCalls :: Maybe Int, rsModeration :: Maybe RawJson, rsPreviousResponseId :: Maybe Text, rsPrompt :: Maybe Prompt, rsPromptCacheKey :: Maybe Text, rsPromptCacheOptions :: Maybe PromptCacheOptions, rsPromptCacheRetention :: Maybe Text, rsReasoning :: Maybe ReasoningConfig, rsSafetyIdentifier :: Maybe Text, rsServiceTier :: Maybe Text, rsStatus :: Maybe ResponseStatus, rsText :: Maybe ResponseTextConfig, rsTopLogprobs :: Maybe Int, rsTruncation :: Maybe Text, rsUsage :: Maybe ResponseUsage, rsUser :: Maybe Text, rsExtraFields :: Extensions }

extensionsEncoder :: E.Encoder Extensions
extensionsEncoder = E.objectWithExtensions id []

extensionsDecoder :: D.Decoder Extensions
extensionsDecoder = D.object emptyExtensions []
    (D.unknownField D.rawJson (\k v e -> Right (insertExtension k v e)))
    Right
