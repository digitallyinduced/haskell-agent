-- | Parameters and primitives for creating a response.
module Agent.Responses.Types.Request
    ( ResponseCreateParams(..)
    , responseCreateParamsDecoder
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
    , conversationDecoder
    , promptDecoder
    , promptCacheOptionsDecoder
    , reasoningConfigDecoder
    , responseTextConfigDecoder
    , toolChoiceDecoder
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Items (ResponseInput, responseInputDecoder)
import Agent.Responses.Types.Tools (ResponseTool, responseToolDecoder)
import Control.Applicative ((<|>))
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Hermes as Hermes
import Data.Scientific (Scientific)
import Data.Text (Text)

newtype ResponseInclude = ResponseInclude { unResponseInclude :: Text }
    deriving stock (Eq, Show)

instance ToJSON ResponseInclude where
    toJSON ResponseInclude { unResponseInclude } =
        Aeson.String unResponseInclude


data ContextManagement = ContextManagement
    { contextType      :: !Text
    , compactThreshold :: !(Maybe Int)

    } deriving stock (Eq, Show)

instance ToJSON ContextManagement where
    toJSON ContextManagement
        { contextType, compactThreshold } =
            objectWith
                [ Just (field "type" contextType)
                , optionalField "compact_threshold" compactThreshold
                ]


data Conversation
    = ConversationId !Text
    | ConversationObject
        { conversationId :: !Text

        }
    deriving stock (Eq, Show)

instance ToJSON Conversation where
    toJSON (ConversationId value) = Aeson.String value
    toJSON ConversationObject { conversationId } =
        objectWith [Just (field "id" conversationId)]


data Prompt = Prompt
    { promptId        :: !Text
    , promptVariables :: !(Maybe RawJson)
    , promptVersion   :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON Prompt where
    toJSON Prompt
        { promptId, promptVariables, promptVersion } =
            objectWith
                [ Just (field "id" promptId)
                , optionalField "variables" promptVariables
                , optionalField "version" promptVersion
                ]


data PromptCacheOptions = PromptCacheOptions
    { mode        :: !(Maybe Text)
    , ttl         :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON PromptCacheOptions where
    toJSON PromptCacheOptions { mode, ttl } =
        objectWith
            [optionalField "mode" mode, optionalField "ttl" ttl]


data ReasoningConfig = ReasoningConfig
    { context         :: !(Maybe Text)
    , effort          :: !(Maybe Text)
    , generateSummary :: !(Maybe Text)
    , reasoningMode   :: !(Maybe Text)
    , summary         :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON ReasoningConfig where
    toJSON ReasoningConfig
        { context, effort, generateSummary, reasoningMode, summary
         } =
            objectWith
                [ optionalField "context" context
                , optionalField "effort" effort
                , optionalField "generate_summary" generateSummary
                , optionalField "mode" reasoningMode
                , optionalField "summary" summary
                ]


data ResponseFormat
    = ResponseFormatText
    | ResponseFormatJsonObject
    | ResponseFormatJsonSchema
        { formatName        :: !Text
        , formatDescription :: !(Maybe Text)
        , formatSchema      :: !RawJson
        , formatStrict      :: !(Maybe Bool)

        }
    | ResponseFormatUnknown !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseFormat where
    toJSON ResponseFormatText {} =
        objectWith [Just (field "type" ("text" :: Text))]
    toJSON ResponseFormatJsonObject {} =
        objectWith [Just (field "type" ("json_object" :: Text))]
    toJSON ResponseFormatJsonSchema
        { formatName, formatDescription, formatSchema, formatStrict
         } =
            objectWith
                [ Just (field "type" ("json_schema" :: Text))
                , Just (field "name" formatName)
                , optionalField "description" formatDescription
                , Just (field "schema" formatSchema)
                , optionalField "strict" formatStrict
                ]
    toJSON (ResponseFormatUnknown tagged) = toJSON tagged


data ResponseTextConfig = ResponseTextConfig
    { format      :: !(Maybe ResponseFormat)
    , verbosity   :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON ResponseTextConfig where
    toJSON ResponseTextConfig { format, verbosity } =
        objectWith
            [ optionalField "format" format
            , optionalField "verbosity" verbosity
            ]


data StreamOptions = StreamOptions
    { includeObfuscation :: !(Maybe Bool)

    } deriving stock (Eq, Show)

instance ToJSON StreamOptions where
    toJSON StreamOptions { includeObfuscation } =
        objectWith
            [optionalField "include_obfuscation" includeObfuscation]


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


data ResponseCreateParams = ResponseCreateParams
    { background           :: !(Maybe Bool)
    , contextManagement    :: !(Maybe [ContextManagement])
    , conversation         :: !(Maybe Conversation)
    , include              :: !(Maybe [ResponseInclude])
    , input                :: !(Maybe ResponseInput)
    , instructions         :: !(Maybe Text)
    , maxOutputTokens      :: !(Maybe Int)
    , maxToolCalls         :: !(Maybe Int)
    , metadata             :: !(Maybe ResponseMetadata)
    , model                :: !(Maybe Text)
    , moderation           :: !(Maybe RawJson)
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

    }

instance ToJSON ResponseCreateParams where
    toJSON ResponseCreateParams
        { background, contextManagement, conversation, include, input
        , instructions, maxOutputTokens, maxToolCalls, metadata, model
        , moderation, parallelToolCalls, previousResponseId, prompt
        , promptCacheKey, promptCacheOptions, promptCacheRetention, reasoning
        , safetyIdentifier, serviceTier, store, stream, streamOptions
        , temperature, text, toolChoice, tools, topLogprobs, topP, truncation
        , user } =
            objectWith
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


responseIncludeDecoder :: Hermes.Decoder ResponseInclude
responseIncludeDecoder = ResponseInclude <$> Hermes.text

contextManagementDecoder :: Hermes.Decoder ContextManagement
contextManagementDecoder = Hermes.object $
    ContextManagement
        <$> Hermes.atKey "type" Hermes.text
        <*> optionalAtKey "compact_threshold" Hermes.int

conversationDecoder :: Hermes.Decoder Conversation
conversationDecoder =
    Hermes.getType >>= \case
        Hermes.VString -> ConversationId <$> Hermes.text
        Hermes.VObject -> Hermes.object $
            ConversationObject
                <$> Hermes.atKey "id" Hermes.text
        _ -> fail "Conversation: expected string or object"

promptDecoder :: Hermes.Decoder Prompt
promptDecoder = Hermes.object $
    Prompt
        <$> Hermes.atKey "id" Hermes.text
        <*> optionalAtKey "variables" rawJsonDecoder
        <*> optionalAtKey "version" Hermes.text

promptCacheOptionsDecoder :: Hermes.Decoder PromptCacheOptions
promptCacheOptionsDecoder = Hermes.object $
    PromptCacheOptions
        <$> optionalAtKey "mode" Hermes.text
        <*> optionalAtKey "ttl" Hermes.text

reasoningConfigDecoder :: Hermes.Decoder ReasoningConfig
reasoningConfigDecoder = Hermes.object $
    ReasoningConfig
        <$> optionalAtKey "context" Hermes.text
        <*> optionalAtKey "effort" Hermes.text
        <*> optionalAtKey "generate_summary" Hermes.text
        <*> optionalAtKey "mode" Hermes.text
        <*> optionalAtKey "summary" Hermes.text

responseFormatDecoder :: Hermes.Decoder ResponseFormat
responseFormatDecoder =
    Hermes.object do
        wireType <- Hermes.atKey "type" Hermes.text
        Hermes.liftObjectDecoder $ Hermes.object $ case wireType of
            "text" -> pure ResponseFormatText
            "json_object" -> pure ResponseFormatJsonObject
            "json_schema" -> ResponseFormatJsonSchema
                <$> Hermes.atKey "name" Hermes.text
                <*> optionalAtKey "description" Hermes.text
                <*> Hermes.atKey "schema" rawJsonDecoder
                <*> optionalAtKey "strict" Hermes.bool
            _ -> pure
                (ResponseFormatUnknown (TaggedObject wireType))

responseTextConfigDecoder :: Hermes.Decoder ResponseTextConfig
responseTextConfigDecoder = Hermes.object do
    format <- optionalAtKey "format" responseFormatDecoder
    legacyFormat <- optionalAtKey "format_" responseFormatDecoder
    verbosity <- optionalAtKey "verbosity" Hermes.text
    pure ResponseTextConfig
        { format = format <|> legacyFormat
        , verbosity

        }

streamOptionsDecoder :: Hermes.Decoder StreamOptions
streamOptionsDecoder = Hermes.object $
    StreamOptions
        <$> optionalAtKey "include_obfuscation" Hermes.bool

toolChoiceDecoder :: Hermes.Decoder ToolChoice
toolChoiceDecoder =
    Hermes.getType >>= \case
        Hermes.VString ->
            ToolChoiceMode . parseToolChoiceMode <$> Hermes.text
        Hermes.VObject ->
            ToolChoiceObject <$> taggedObjectDecoder
        _ -> fail "ToolChoice: expected string or object"

responseCreateParamsDecoder :: Hermes.Decoder ResponseCreateParams
responseCreateParamsDecoder = Hermes.object $
    ResponseCreateParams
        <$> optionalAtKey "background" Hermes.bool
        <*> optionalAtKey
            "context_management"
            (Hermes.list contextManagementDecoder)
        <*> optionalAtKey "conversation" conversationDecoder
        <*> optionalAtKey "include" (Hermes.list responseIncludeDecoder)
        <*> optionalAtKey "input" responseInputDecoder
        <*> optionalAtKey "instructions" Hermes.text
        <*> optionalAtKey "max_output_tokens" Hermes.int
        <*> optionalAtKey "max_tool_calls" Hermes.int
        <*> optionalAtKey "metadata" responseMetadataDecoder
        <*> optionalAtKey "model" Hermes.text
        <*> optionalAtKey "moderation" rawJsonDecoder
        <*> optionalAtKey "parallel_tool_calls" Hermes.bool
        <*> optionalAtKey "previous_response_id" Hermes.text
        <*> optionalAtKey "prompt" promptDecoder
        <*> optionalAtKey "prompt_cache_key" Hermes.text
        <*> optionalAtKey "prompt_cache_options" promptCacheOptionsDecoder
        <*> optionalAtKey "prompt_cache_retention" Hermes.text
        <*> optionalAtKey "reasoning" reasoningConfigDecoder
        <*> optionalAtKey "safety_identifier" Hermes.text
        <*> optionalAtKey "service_tier" Hermes.text
        <*> optionalAtKey "store" Hermes.bool
        <*> optionalAtKey "stream" Hermes.bool
        <*> optionalAtKey "stream_options" streamOptionsDecoder
        <*> optionalAtKey "temperature" Hermes.scientific
        <*> optionalAtKey "text" responseTextConfigDecoder
        <*> optionalAtKey "tool_choice" toolChoiceDecoder
        <*> optionalAtKey "tools" (Hermes.list responseToolDecoder)
        <*> optionalAtKey "top_logprobs" Hermes.int
        <*> optionalAtKey "top_p" Hermes.scientific
        <*> optionalAtKey "truncation" Hermes.text
        <*> optionalAtKey "user" Hermes.text
