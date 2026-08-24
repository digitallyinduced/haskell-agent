-- | Parameters and primitives for creating a response.
module Agent.Responses.Types.Request
    ( ResponseCreateParams(..)
    , defaultResponseCreateParams
    , ResponseInclude(..)
    , ContextManagement(..)
    , Conversation(..)
    , Prompt(..)
    , PromptCacheOptions(..)
    , ReasoningConfig(..)
    , ResponseTextConfig(..)
    , ResponseFormat(..)
    , StreamOptions(..)
    , ToolChoice(..)
    , ToolChoiceMode(..)
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Items (ResponseInput)
import Agent.Responses.Types.Tools (ResponseTool)
import Control.Applicative ((<|>))
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (Scientific)
import Data.Text (Text)

newtype ResponseInclude = ResponseInclude { unResponseInclude :: Text }
    deriving stock (Eq, Show)

instance ToJSON ResponseInclude where
    toJSON ResponseInclude { unResponseInclude } =
        Aeson.String unResponseInclude

instance FromJSON ResponseInclude where
    parseJSON = withText "ResponseInclude" (pure . ResponseInclude)

data ContextManagement = ContextManagement
    { contextType      :: !Text
    , compactThreshold :: !(Maybe Int)
    , extraFields      :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ContextManagement where
    toJSON ContextManagement
        { contextType, compactThreshold, extraFields } =
            objectWith extraFields
                [ Just (field "type" contextType)
                , optionalField "compact_threshold" compactThreshold
                ]

instance FromJSON ContextManagement where
    parseJSON = withObject "ContextManagement" $ \o -> ContextManagement
        <$> o .: "type"
        <*> o .:? "compact_threshold"
        <*> pure (without ["type", "compact_threshold"] o)

data Conversation
    = ConversationId !Text
    | ConversationObject
        { conversationId :: !Text
        , extraFields     :: !Aeson.Object
        }
    deriving stock (Eq, Show)

instance ToJSON Conversation where
    toJSON (ConversationId value) = Aeson.String value
    toJSON ConversationObject { conversationId, extraFields } =
        objectWith extraFields [Just (field "id" conversationId)]

instance FromJSON Conversation where
    parseJSON (Aeson.String value) = pure (ConversationId value)
    parseJSON value = withObject "Conversation" (\o -> ConversationObject
        <$> o .: "id"
        <*> pure (without ["id"] o)) value

data Prompt = Prompt
    { promptId        :: !Text
    , promptVariables :: !(Maybe Aeson.Object)
    , promptVersion   :: !(Maybe Text)
    , extraFields     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON Prompt where
    toJSON Prompt
        { promptId, promptVariables, promptVersion, extraFields } =
            objectWith extraFields
                [ Just (field "id" promptId)
                , optionalField "variables" promptVariables
                , optionalField "version" promptVersion
                ]

instance FromJSON Prompt where
    parseJSON = withObject "Prompt" $ \o -> Prompt
        <$> o .: "id"
        <*> o .:? "variables"
        <*> o .:? "version"
        <*> pure (without ["id", "variables", "version"] o)

data PromptCacheOptions = PromptCacheOptions
    { mode        :: !(Maybe Text)
    , ttl         :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON PromptCacheOptions where
    toJSON PromptCacheOptions { mode, ttl, extraFields } =
        objectWith extraFields
            [optionalField "mode" mode, optionalField "ttl" ttl]

instance FromJSON PromptCacheOptions where
    parseJSON = withObject "PromptCacheOptions" $ \o -> PromptCacheOptions
        <$> o .:? "mode"
        <*> o .:? "ttl"
        <*> pure (without ["mode", "ttl"] o)

data ReasoningConfig = ReasoningConfig
    { context         :: !(Maybe Text)
    , effort          :: !(Maybe Text)
    , generateSummary :: !(Maybe Text)
    , reasoningMode   :: !(Maybe Text)
    , summary         :: !(Maybe Text)
    , extraFields     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ReasoningConfig where
    toJSON ReasoningConfig
        { context, effort, generateSummary, reasoningMode, summary
        , extraFields } =
            objectWith extraFields
                [ optionalField "context" context
                , optionalField "effort" effort
                , optionalField "generate_summary" generateSummary
                , optionalField "mode" reasoningMode
                , optionalField "summary" summary
                ]

instance FromJSON ReasoningConfig where
    parseJSON = withObject "ReasoningConfig" $ \o -> ReasoningConfig
        <$> o .:? "context"
        <*> o .:? "effort"
        <*> o .:? "generate_summary"
        <*> o .:? "mode"
        <*> o .:? "summary"
        <*> pure
            (without
                ["context", "effort", "generate_summary", "mode", "summary"] o)

data ResponseFormat
    = ResponseFormatText
        { extraFields :: !Aeson.Object }
    | ResponseFormatJsonObject
        { extraFields :: !Aeson.Object }
    | ResponseFormatJsonSchema
        { formatName        :: !Text
        , formatDescription :: !(Maybe Text)
        , formatSchema      :: !Aeson.Value
        , formatStrict      :: !(Maybe Bool)
        , extraFields       :: !Aeson.Object
        }
    | ResponseFormatUnknown !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseFormat where
    toJSON ResponseFormatText { extraFields } =
        objectWith extraFields [Just (field "type" ("text" :: Text))]
    toJSON ResponseFormatJsonObject { extraFields } =
        objectWith extraFields [Just (field "type" ("json_object" :: Text))]
    toJSON ResponseFormatJsonSchema
        { formatName, formatDescription, formatSchema, formatStrict
        , extraFields } =
            objectWith extraFields
                [ Just (field "type" ("json_schema" :: Text))
                , Just (field "name" formatName)
                , optionalField "description" formatDescription
                , Just (field "schema" formatSchema)
                , optionalField "strict" formatStrict
                ]
    toJSON (ResponseFormatUnknown tagged) = toJSON tagged

instance FromJSON ResponseFormat where
    parseJSON value = withObject "ResponseFormat" (\o -> do
        tag <- o .: "type"
        case (tag :: Text) of
            "text" -> pure ResponseFormatText
                { extraFields = without ["type"] o }
            "json_object" -> pure ResponseFormatJsonObject
                { extraFields = without ["type"] o }
            "json_schema" -> ResponseFormatJsonSchema
                <$> o .: "name"
                <*> o .:? "description"
                <*> o .: "schema"
                <*> o .:? "strict"
                <*> pure
                    (without
                        ["type", "name", "description", "schema", "strict"] o)
            _ -> ResponseFormatUnknown <$> parseJSON value) value

data ResponseTextConfig = ResponseTextConfig
    { format      :: !(Maybe ResponseFormat)
    , verbosity   :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseTextConfig where
    toJSON ResponseTextConfig { format, verbosity, extraFields } =
        objectWith extraFields
            [ optionalField "format" format
            , optionalField "verbosity" verbosity
            ]

instance FromJSON ResponseTextConfig where
    parseJSON = withObject "ResponseTextConfig" $ \o -> ResponseTextConfig
        <$> (o .:? "format" <|> o .:? "format_")
        <*> o .:? "verbosity"
        <*> pure (without ["format", "format_", "verbosity"] o)

data StreamOptions = StreamOptions
    { includeObfuscation :: !(Maybe Bool)
    , extraFields        :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON StreamOptions where
    toJSON StreamOptions { includeObfuscation, extraFields } =
        objectWith extraFields
            [optionalField "include_obfuscation" includeObfuscation]

instance FromJSON StreamOptions where
    parseJSON = withObject "StreamOptions" $ \o -> StreamOptions
        <$> o .:? "include_obfuscation"
        <*> pure (without ["include_obfuscation"] o)

data ToolChoiceMode
    = ToolChoiceNone
    | ToolChoiceAuto
    | ToolChoiceRequired
    | ToolChoiceModeUnknown !Text
    deriving stock (Eq, Show)

toolChoiceModeText :: ToolChoiceMode -> Text
toolChoiceModeText = \case
    ToolChoiceNone -> "none"
    ToolChoiceAuto -> "auto"
    ToolChoiceRequired -> "required"
    ToolChoiceModeUnknown value -> value

parseToolChoiceMode :: Text -> ToolChoiceMode
parseToolChoiceMode = \case
    "none" -> ToolChoiceNone
    "auto" -> ToolChoiceAuto
    "required" -> ToolChoiceRequired
    value -> ToolChoiceModeUnknown value

data ToolChoice
    = ToolChoiceMode !ToolChoiceMode
    | ToolChoiceObject !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ToolChoice where
    toJSON (ToolChoiceMode choice) =
        Aeson.String (toolChoiceModeText choice)
    toJSON (ToolChoiceObject tagged) = toJSON tagged

instance FromJSON ToolChoice where
    parseJSON (Aeson.String value) =
        pure (ToolChoiceMode (parseToolChoiceMode value))
    parseJSON value = ToolChoiceObject <$> parseJSON value

data ResponseCreateParams = ResponseCreateParams
    { background           :: !(Maybe Bool)
    , contextManagement    :: !(Maybe [ContextManagement])
    , conversation         :: !(Maybe Conversation)
    , include              :: !(Maybe [ResponseInclude])
    , input                :: !(Maybe ResponseInput)
    , instructions         :: !(Maybe Text)
    , maxOutputTokens      :: !(Maybe Int)
    , maxToolCalls         :: !(Maybe Int)
    , metadata             :: !(Maybe Aeson.Object)
    , model                :: !(Maybe Text)
    , moderation           :: !(Maybe Aeson.Value)
    , parallelToolCalls    :: !(Maybe Bool)
    , previousResponseId   :: !(Maybe Text)
    , prompt               :: !(Maybe Prompt)
    , promptCacheKey       :: !(Maybe Text)
    , promptCacheOptions   :: !(Maybe PromptCacheOptions)
    , promptCacheRetention :: !(Maybe Text)
    , reasoning            :: !(Maybe ReasoningConfig)
    , safetyIdentifier     :: !(Maybe Text)
    , serviceTier          :: !(Maybe Text)
    , store                :: !(Maybe Bool)
    , stream               :: !(Maybe Bool)
    , streamOptions        :: !(Maybe StreamOptions)
    , temperature          :: !(Maybe Scientific)
    , text                 :: !(Maybe ResponseTextConfig)
    , toolChoice           :: !(Maybe ToolChoice)
    , tools                :: !(Maybe [ResponseTool])
    , topLogprobs          :: !(Maybe Int)
    , topP                 :: !(Maybe Scientific)
    , truncation           :: !(Maybe Text)
    , user                 :: !(Maybe Text)
    , extraFields          :: !Aeson.Object
    } deriving stock (Eq, Show)

defaultResponseCreateParams :: ResponseCreateParams
defaultResponseCreateParams = ResponseCreateParams
    { background = Nothing
    , contextManagement = Nothing
    , conversation = Nothing
    , include = Nothing
    , input = Nothing
    , instructions = Nothing
    , maxOutputTokens = Nothing
    , maxToolCalls = Nothing
    , metadata = Nothing
    , model = Nothing
    , moderation = Nothing
    , parallelToolCalls = Nothing
    , previousResponseId = Nothing
    , prompt = Nothing
    , promptCacheKey = Nothing
    , promptCacheOptions = Nothing
    , promptCacheRetention = Nothing
    , reasoning = Nothing
    , safetyIdentifier = Nothing
    , serviceTier = Nothing
    , store = Nothing
    , stream = Nothing
    , streamOptions = Nothing
    , temperature = Nothing
    , text = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , truncation = Nothing
    , user = Nothing
    , extraFields = KeyMap.empty
    }

responseCreateFieldNames :: [Text]
responseCreateFieldNames =
    [ "background", "context_management", "conversation", "include", "input"
    , "instructions", "max_output_tokens", "max_tool_calls", "metadata", "model"
    , "moderation", "parallel_tool_calls", "previous_response_id", "prompt"
    , "prompt_cache_key", "prompt_cache_options", "prompt_cache_retention"
    , "reasoning", "safety_identifier", "service_tier", "store", "stream"
    , "stream_options", "temperature", "text", "tool_choice", "tools"
    , "top_logprobs", "top_p", "truncation", "user"
    ]

instance ToJSON ResponseCreateParams where
    toJSON ResponseCreateParams
        { background, contextManagement, conversation, include, input
        , instructions, maxOutputTokens, maxToolCalls, metadata, model
        , moderation, parallelToolCalls, previousResponseId, prompt
        , promptCacheKey, promptCacheOptions, promptCacheRetention, reasoning
        , safetyIdentifier, serviceTier, store, stream, streamOptions
        , temperature, text, toolChoice, tools, topLogprobs, topP, truncation
        , user, extraFields } =
            objectWith extraFields
                [ optionalField "background" background
                , optionalField "context_management" contextManagement
                , optionalField "conversation" conversation
                , optionalField "include" include
                , optionalField "input" input
                , optionalField "instructions" instructions
                , optionalField "max_output_tokens" maxOutputTokens
                , optionalField "max_tool_calls" maxToolCalls
                , optionalField "metadata" metadata
                , optionalField "model" model
                , optionalField "moderation" moderation
                , optionalField "parallel_tool_calls" parallelToolCalls
                , optionalField "previous_response_id" previousResponseId
                , optionalField "prompt" prompt
                , optionalField "prompt_cache_key" promptCacheKey
                , optionalField "prompt_cache_options" promptCacheOptions
                , optionalField "prompt_cache_retention" promptCacheRetention
                , optionalField "reasoning" reasoning
                , optionalField "safety_identifier" safetyIdentifier
                , optionalField "service_tier" serviceTier
                , optionalField "store" store
                , optionalField "stream" stream
                , optionalField "stream_options" streamOptions
                , optionalField "temperature" temperature
                , optionalField "text" text
                , optionalField "tool_choice" toolChoice
                , optionalField "tools" tools
                , optionalField "top_logprobs" topLogprobs
                , optionalField "top_p" topP
                , optionalField "truncation" truncation
                , optionalField "user" user
                ]

instance FromJSON ResponseCreateParams where
    parseJSON = withObject "ResponseCreateParams" $ \o -> ResponseCreateParams
        <$> o .:? "background"
        <*> o .:? "context_management"
        <*> o .:? "conversation"
        <*> o .:? "include"
        <*> o .:? "input"
        <*> o .:? "instructions"
        <*> o .:? "max_output_tokens"
        <*> o .:? "max_tool_calls"
        <*> o .:? "metadata"
        <*> o .:? "model"
        <*> o .:? "moderation"
        <*> o .:? "parallel_tool_calls"
        <*> o .:? "previous_response_id"
        <*> o .:? "prompt"
        <*> o .:? "prompt_cache_key"
        <*> o .:? "prompt_cache_options"
        <*> o .:? "prompt_cache_retention"
        <*> o .:? "reasoning"
        <*> o .:? "safety_identifier"
        <*> o .:? "service_tier"
        <*> o .:? "store"
        <*> o .:? "stream"
        <*> o .:? "stream_options"
        <*> o .:? "temperature"
        <*> o .:? "text"
        <*> o .:? "tool_choice"
        <*> o .:? "tools"
        <*> o .:? "top_logprobs"
        <*> o .:? "top_p"
        <*> o .:? "truncation"
        <*> o .:? "user"
        <*> pure (without responseCreateFieldNames o)
