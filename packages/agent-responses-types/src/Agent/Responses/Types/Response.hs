module Agent.Responses.Types.Response
    ( Response(..)
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
    , without
    )
import Agent.Responses.Types.Items
    ( ResponseInput
    , ResponseItem
    )
import Agent.Responses.Types.Request
    ( Conversation
    , Prompt
    , PromptCacheOptions
    , ReasoningConfig
    , ResponseTextConfig
    , ToolChoice
    )
import Agent.Responses.Types.Tools
    ( ResponseTool )
import Data.Aeson
import qualified Data.Aeson as Aeson
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

instance FromJSON ResponseStatus where
    parseJSON = withText "ResponseStatus" $ pure . \case
        "completed" -> ResponseCompleted
        "failed" -> ResponseFailed
        "in_progress" -> ResponseInProgress
        "cancelled" -> ResponseCancelled
        "queued" -> ResponseQueued
        "incomplete" -> ResponseIncomplete
        value -> ResponseStatusUnknown value

data ResponseError = ResponseError
    { code        :: !Text
    , message     :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseError where
    toJSON ResponseError { code, message, extraFields } = objectWith extraFields
        [Just (field "code" code), Just (field "message" message)]

instance FromJSON ResponseError where
    parseJSON = withObject "ResponseError" $ \o -> ResponseError
        -- Failed responses are occasionally reduced to only one of these
        -- fields. Treat both as optional on input while retaining the stable
        -- non-optional public representation.
        <$> o .:? "code" .!= ""
        <*> o .:? "message" .!= ""
        <*> pure (without ["code", "message"] o)

data IncompleteDetails = IncompleteDetails
    { reason      :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON IncompleteDetails where
    toJSON IncompleteDetails { reason, extraFields } =
        objectWith extraFields [Just (field "reason" reason)]

instance FromJSON IncompleteDetails where
    parseJSON = withObject "IncompleteDetails" $ \o -> IncompleteDetails
        <$> o .: "reason"
        <*> pure (without ["reason"] o)

data TokenDetails = TokenDetails
    { cachedTokens    :: !(Maybe Int)
    , reasoningTokens :: !(Maybe Int)
    , extraFields     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON TokenDetails where
    toJSON TokenDetails { cachedTokens, reasoningTokens, extraFields } =
        objectWith extraFields
            [ optionalField "cached_tokens" cachedTokens
            , optionalField "reasoning_tokens" reasoningTokens
            ]

instance FromJSON TokenDetails where
    parseJSON = withObject "TokenDetails" $ \o -> TokenDetails
        <$> o .:? "cached_tokens"
        <*> o .:? "reasoning_tokens"
        <*> pure (without ["cached_tokens", "reasoning_tokens"] o)

data ResponseUsage = ResponseUsage
    { inputTokens         :: !Int
    , inputTokensDetails  :: !(Maybe TokenDetails)
    , outputTokens        :: !Int
    , outputTokensDetails :: !(Maybe TokenDetails)
    , totalTokens         :: !Int
    , extraFields         :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseUsage where
    toJSON ResponseUsage
        { inputTokens
        , inputTokensDetails
        , outputTokens
        , outputTokensDetails
        , totalTokens
        , extraFields
        } = objectWith extraFields
            [ Just (field "input_tokens" inputTokens)
            , optionalField "input_tokens_details" inputTokensDetails
            , Just (field "output_tokens" outputTokens)
            , optionalField "output_tokens_details" outputTokensDetails
            , Just (field "total_tokens" totalTokens)
            ]

instance FromJSON ResponseUsage where
    parseJSON = withObject "ResponseUsage" $ \o -> ResponseUsage
        <$> o .: "input_tokens"
        <*> o .:? "input_tokens_details"
        <*> o .: "output_tokens"
        <*> o .:? "output_tokens_details"
        <*> o .: "total_tokens"
        <*> pure
            (without
                [ "input_tokens"
                , "input_tokens_details"
                , "output_tokens"
                , "output_tokens_details"
                , "total_tokens"
                ]
                o
            )

data Response = Response
    { responseId           :: !Text
    , createdAt            :: !Scientific
    , error                :: !(Maybe ResponseError)
    , incompleteDetails    :: !(Maybe IncompleteDetails)
    , instructions         :: !(Maybe ResponseInput)
    , metadata             :: !(Maybe Aeson.Object)
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
    , moderation           :: !(Maybe Aeson.Value)
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
    , extraFields          :: !Aeson.Object
    } deriving stock (Eq, Show)

responseFieldNames :: [Text]
responseFieldNames =
    [ "id", "created_at", "error", "incomplete_details", "instructions", "metadata", "model"
    , "object", "output", "parallel_tool_calls", "temperature", "tool_choice", "tools", "top_p"
    , "background", "completed_at", "conversation", "max_output_tokens", "max_tool_calls"
    , "moderation", "previous_response_id", "prompt", "prompt_cache_key", "prompt_cache_options"
    , "prompt_cache_retention", "reasoning", "safety_identifier", "service_tier", "status", "text"
    , "top_logprobs", "truncation", "usage", "user"
    ]

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
        , extraFields
        } = objectWith extraFields
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

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> Response
        <$> o .: "id"
        <*> o .: "created_at"
        <*> o .:? "error"
        <*> o .:? "incomplete_details"
        <*> o .:? "instructions"
        <*> o .:? "metadata"
        <*> o .: "model"
        <*> o .:? "object" .!= "response"
        <*> o .:? "output" .!= []
        <*> o .:? "parallel_tool_calls"
        <*> o .:? "temperature"
        <*> o .:? "tool_choice"
        <*> o .:? "tools"
        <*> o .:? "top_p"
        <*> o .:? "background"
        <*> o .:? "completed_at"
        <*> o .:? "conversation"
        <*> o .:? "max_output_tokens"
        <*> o .:? "max_tool_calls"
        <*> o .:? "moderation"
        <*> o .:? "previous_response_id"
        <*> o .:? "prompt"
        <*> o .:? "prompt_cache_key"
        <*> o .:? "prompt_cache_options"
        <*> o .:? "prompt_cache_retention"
        <*> o .:? "reasoning"
        <*> o .:? "safety_identifier"
        <*> o .:? "service_tier"
        <*> o .: "status"
        <*> o .:? "text"
        <*> o .:? "top_logprobs"
        <*> o .:? "truncation"
        <*> o .:? "usage"
        <*> o .:? "user"
        <*> pure (without responseFieldNames o)
