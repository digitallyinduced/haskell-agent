module Agent.Responses.Types.Response
    ( Response(..)
    , responseDecoder
    , ResponseStatus(..)
    , ResponseError(..)
    , IncompleteDetails(..)
    , ResponseUsage(..)
    , TokenDetails(..)
    ) where

import Agent.Responses.Types.Common
    ( field
    , objectWith
    , optionalField
    , optionalAtKey
    , RawJson
    , rawJsonDecoder
    )
import Agent.Responses.Types.Items
    ( ResponseInput
    , ResponseItem
    , responseInputDecoder
    , responseItemDecoder
    )
import Agent.Responses.Types.Request
    ( Conversation
    , Prompt
    , PromptCacheOptions
    , ReasoningConfig
    , ResponseTextConfig
    , ToolChoice
    , conversationDecoder
    , promptDecoder
    , promptCacheOptionsDecoder
    , reasoningConfigDecoder
    , responseTextConfigDecoder
    , toolChoiceDecoder
    )
import Agent.Responses.Types.Tools
    ( ResponseTool, responseToolDecoder )
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Hermes as Hermes
import Data.Scientific (Scientific)
import Data.Text (Text)

data ResponseStatus
    = ResponseCompleted
    | ResponseFailed
    | ResponseInProgress
    | ResponseCancelled
    | ResponseQueued
    | ResponseIncomplete
    | ResponseStatusUnknown !Text
    deriving stock (Eq, Show)

responseStatusText :: ResponseStatus -> Text
responseStatusText = \case
    ResponseCompleted -> "completed"
    ResponseFailed -> "failed"
    ResponseInProgress -> "in_progress"
    ResponseCancelled -> "cancelled"
    ResponseQueued -> "queued"
    ResponseIncomplete -> "incomplete"
    ResponseStatusUnknown value -> value

instance ToJSON ResponseStatus where
    toJSON = Aeson.String . responseStatusText


data ResponseError = ResponseError
    { code        :: !Text
    , message     :: !Text

    } deriving stock (Eq, Show)

instance ToJSON ResponseError where
    toJSON ResponseError { code, message } = objectWith
        [Just (field "code" code), Just (field "message" message)]


data IncompleteDetails = IncompleteDetails
    { reason      :: !Text

    } deriving stock (Eq, Show)

instance ToJSON IncompleteDetails where
    toJSON IncompleteDetails { reason } =
        objectWith [Just (field "reason" reason)]


data TokenDetails = TokenDetails
    { cachedTokens    :: !(Maybe Int)
    , reasoningTokens :: !(Maybe Int)

    } deriving stock (Eq, Show)

instance ToJSON TokenDetails where
    toJSON TokenDetails { cachedTokens, reasoningTokens } =
        objectWith
            [ optionalField "cached_tokens" cachedTokens
            , optionalField "reasoning_tokens" reasoningTokens
            ]


data ResponseUsage = ResponseUsage
    { inputTokens         :: !Int
    , inputTokensDetails  :: !(Maybe TokenDetails)
    , outputTokens        :: !Int
    , outputTokensDetails :: !(Maybe TokenDetails)
    , totalTokens         :: !Int

    } deriving stock (Eq, Show)

instance ToJSON ResponseUsage where
    toJSON ResponseUsage
        { inputTokens
        , inputTokensDetails
        , outputTokens
        , outputTokensDetails
        , totalTokens

        } = objectWith
            [ Just (field "input_tokens" inputTokens)
            , optionalField "input_tokens_details" inputTokensDetails
            , Just (field "output_tokens" outputTokens)
            , optionalField "output_tokens_details" outputTokensDetails
            , Just (field "total_tokens" totalTokens)
            ]


data Response = Response
    { responseId           :: !Text
    , createdAt            :: !Scientific
    , error                :: !(Maybe ResponseError)
    , incompleteDetails    :: !(Maybe IncompleteDetails)
    , instructions         :: !(Maybe ResponseInput)
    , metadata             :: !(Maybe RawJson)
    , model                :: !Text
    , object               :: !Text
    , output               :: ![ResponseItem]
    , parallelToolCalls    :: !(Maybe Bool)
    , temperature          :: !(Maybe Scientific)
    , toolChoice           :: !(Maybe ToolChoice)
    , tools                :: !(Maybe [ResponseTool])
    , topP                 :: !(Maybe Scientific)
    , background           :: !(Maybe Bool)
    , completedAt          :: !(Maybe Scientific)
    , conversation         :: !(Maybe Conversation)
    , maxOutputTokens      :: !(Maybe Int)
    , maxToolCalls         :: !(Maybe Int)
    , moderation           :: !(Maybe RawJson)
    , previousResponseId   :: !(Maybe Text)
    , prompt               :: !(Maybe Prompt)
    , promptCacheKey       :: !(Maybe Text)
    , promptCacheOptions   :: !(Maybe PromptCacheOptions)
    , promptCacheRetention :: !(Maybe Text)
    , reasoning            :: !(Maybe ReasoningConfig)
    , safetyIdentifier     :: !(Maybe Text)
    , serviceTier          :: !(Maybe Text)
    , status               :: !ResponseStatus
    , text                 :: !(Maybe ResponseTextConfig)
    , topLogprobs          :: !(Maybe Int)
    , truncation           :: !(Maybe Text)
    , usage                :: !(Maybe ResponseUsage)
    , user                 :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON Response where
    toJSON Response
        { responseId
        , createdAt
        , error
        , incompleteDetails
        , instructions
        , metadata
        , model
        , object
        , output
        , parallelToolCalls
        , temperature
        , toolChoice
        , tools
        , topP
        , background
        , completedAt
        , conversation
        , maxOutputTokens
        , maxToolCalls
        , moderation
        , previousResponseId
        , prompt
        , promptCacheKey
        , promptCacheOptions
        , promptCacheRetention
        , reasoning
        , safetyIdentifier
        , serviceTier
        , status
        , text
        , topLogprobs
        , truncation
        , usage
        , user

        } = objectWith
            [ Just (field "id" responseId)
            , Just (field "created_at" createdAt)
            , Just (field "error" error)
            , Just (field "incomplete_details" incompleteDetails)
            , optionalField "instructions" instructions
            , Just (field "metadata" metadata)
            , Just (field "model" model)
            , Just (field "object" object)
            , Just (field "output" output)
            , optionalField "parallel_tool_calls" parallelToolCalls
            , optionalField "temperature" temperature
            , optionalField "tool_choice" toolChoice
            , optionalField "tools" tools
            , optionalField "top_p" topP
            , optionalField "background" background
            , optionalField "completed_at" completedAt
            , optionalField "conversation" conversation
            , optionalField "max_output_tokens" maxOutputTokens
            , optionalField "max_tool_calls" maxToolCalls
            , optionalField "moderation" moderation
            , optionalField "previous_response_id" previousResponseId
            , optionalField "prompt" prompt
            , optionalField "prompt_cache_key" promptCacheKey
            , optionalField "prompt_cache_options" promptCacheOptions
            , optionalField "prompt_cache_retention" promptCacheRetention
            , optionalField "reasoning" reasoning
            , optionalField "safety_identifier" safetyIdentifier
            , optionalField "service_tier" serviceTier
            , Just (field "status" status)
            , optionalField "text" text
            , optionalField "top_logprobs" topLogprobs
            , optionalField "truncation" truncation
            , Just (field "usage" usage)
            , optionalField "user" user
            ]


responseStatusDecoder :: Hermes.Decoder ResponseStatus
responseStatusDecoder = fmap (\case
    "completed" -> ResponseCompleted
    "failed" -> ResponseFailed
    "in_progress" -> ResponseInProgress
    "cancelled" -> ResponseCancelled
    "queued" -> ResponseQueued
    "incomplete" -> ResponseIncomplete
    value -> ResponseStatusUnknown value) Hermes.text

responseErrorDecoder :: Hermes.Decoder ResponseError
responseErrorDecoder = Hermes.object $
    ResponseError
        <$> (maybe "" id <$> optionalAtKey "code" Hermes.text)
        <*> (maybe "" id <$> optionalAtKey "message" Hermes.text)

incompleteDetailsDecoder :: Hermes.Decoder IncompleteDetails
incompleteDetailsDecoder = Hermes.object $
    IncompleteDetails
        <$> Hermes.atKey "reason" Hermes.text

tokenDetailsDecoder :: Hermes.Decoder TokenDetails
tokenDetailsDecoder = Hermes.object $
    TokenDetails
        <$> optionalAtKey "cached_tokens" Hermes.int
        <*> optionalAtKey "reasoning_tokens" Hermes.int

responseUsageDecoder :: Hermes.Decoder ResponseUsage
responseUsageDecoder = Hermes.object $
    ResponseUsage
        <$> Hermes.atKey "input_tokens" Hermes.int
        <*> optionalAtKey "input_tokens_details" tokenDetailsDecoder
        <*> Hermes.atKey "output_tokens" Hermes.int
        <*> optionalAtKey "output_tokens_details" tokenDetailsDecoder
        <*> Hermes.atKey "total_tokens" Hermes.int

responseDecoder :: Hermes.Decoder Response
responseDecoder = Hermes.object $
    Response
        <$> (maybe "" id <$> optionalAtKey "id" Hermes.text)
        <*> (maybe 0 id <$> optionalAtKey "created_at" Hermes.scientific)
        <*> optionalAtKey "error" responseErrorDecoder
        <*> optionalAtKey "incomplete_details" incompleteDetailsDecoder
        <*> optionalAtKey "instructions" responseInputDecoder
        <*> optionalAtKey "metadata" rawJsonDecoder
        <*> (maybe "" id <$> optionalAtKey "model" Hermes.text)
        <*> (maybe "response" id <$> optionalAtKey "object" Hermes.text)
        <*> (maybe [] id <$> optionalAtKey
            "output"
            (Hermes.list responseItemDecoder))
        <*> optionalAtKey "parallel_tool_calls" Hermes.bool
        <*> optionalAtKey "temperature" Hermes.scientific
        <*> optionalAtKey "tool_choice" toolChoiceDecoder
        <*> optionalAtKey "tools" (Hermes.list responseToolDecoder)
        <*> optionalAtKey "top_p" Hermes.scientific
        <*> optionalAtKey "background" Hermes.bool
        <*> optionalAtKey "completed_at" Hermes.scientific
        <*> optionalAtKey "conversation" conversationDecoder
        <*> optionalAtKey "max_output_tokens" Hermes.int
        <*> optionalAtKey "max_tool_calls" Hermes.int
        <*> optionalAtKey "moderation" rawJsonDecoder
        <*> optionalAtKey "previous_response_id" Hermes.text
        <*> optionalAtKey "prompt" promptDecoder
        <*> optionalAtKey "prompt_cache_key" Hermes.text
        <*> optionalAtKey "prompt_cache_options" promptCacheOptionsDecoder
        <*> optionalAtKey "prompt_cache_retention" Hermes.text
        <*> optionalAtKey "reasoning" reasoningConfigDecoder
        <*> optionalAtKey "safety_identifier" Hermes.text
        <*> optionalAtKey "service_tier" Hermes.text
        <*> (maybe ResponseInProgress id
            <$> optionalAtKey "status" responseStatusDecoder)
        <*> optionalAtKey "text" responseTextConfigDecoder
        <*> optionalAtKey "top_logprobs" Hermes.int
        <*> optionalAtKey "truncation" Hermes.text
        <*> optionalAtKey "usage" responseUsageDecoder
        <*> optionalAtKey "user" Hermes.text
