-- | Parameters and primitives for creating a response.
module Agent.Responses.Types.Request
    ( ResponseCreateParams(..), responseCreateParamsEncoder, responseCreateParamsDecoder
    , defaultResponseCreateParams
    , ResponseInclude(..), responseIncludeEncoder, responseIncludeDecoder
    , ContextManagement(..), contextManagementEncoder, contextManagementDecoder
    , Conversation(..), conversationEncoder, conversationDecoder
    , Prompt(..), promptEncoder, promptDecoder
    , PromptCacheOptions(..), promptCacheOptionsEncoder, promptCacheOptionsDecoder
    , ReasoningConfig(..), reasoningConfigEncoder, reasoningConfigDecoder
    , ResponseTextConfig(..), responseTextConfigEncoder, responseTextConfigDecoder
    , ResponseFormat(..), responseFormatEncoder, responseFormatDecoder
    , StreamOptions(..), streamOptionsEncoder, streamOptionsDecoder
    , ToolChoice(..), toolChoiceEncoder, toolChoiceDecoder
    , ToolChoiceMode(..), toolChoiceModeEncoder, toolChoiceModeDecoder
    ) where

import Agent.Json
    ( Extensions, RawJson, emptyExtensions, insertExtension )
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import Agent.Responses.Types.Common
    ( TaggedObject(..), taggedObjectEncoder )
import Agent.Responses.Types.Items (ResponseInput, responseInputDecoder, responseInputEncoder)
import Agent.Responses.Types.Tools (ResponseTool, responseToolDecoder, responseToolEncoder)
import Data.Scientific (Scientific)
import Data.Text (Text)

type E a = Encoder.Encoder a
type D a = Decoder.Decoder a

extensionsDecoder :: D Extensions
extensionsDecoder = Decoder.object emptyExtensions []
    (Decoder.unknownField Decoder.rawJson \k v e -> Right (insertExtension k v e))
    Right

responseIncludeEncoder :: E ResponseInclude
responseIncludeEncoder = Encoder.contramap (\(ResponseInclude t) -> t) Encoder.text
responseIncludeDecoder :: D ResponseInclude
responseIncludeDecoder = Decoder.mapDecoder ResponseInclude Decoder.text
newtype ResponseInclude = ResponseInclude { unResponseInclude :: Text }
    deriving stock (Eq, Show)

data ContextManagement = ContextManagement
    { contextType :: !Text, compactThreshold :: !(Maybe Int), extraFields :: !Extensions }
    deriving stock (Eq, Show)
contextManagementEncoder :: E ContextManagement
contextManagementEncoder = Encoder.objectWithExtensions (.extraFields)
    [ Encoder.field "type" Encoder.text (.contextType)
    , Encoder.optionalField "compact_threshold" Encoder.int (.compactThreshold) ]
contextManagementDecoder :: D ContextManagement
contextManagementDecoder = Decoder.object (ContextState Nothing Nothing emptyExtensions)
    [ Decoder.field "type" Decoder.text \v (ContextState _ n e) -> Right (ContextState (Just v) n e)
    , Decoder.field "compact_threshold" (Decoder.nullable Decoder.int) \v (ContextState t _ e) -> Right (ContextState t v e) ]
    (Decoder.unknownField Decoder.rawJson \k v (ContextState t n e) -> Right (ContextState t n (insertExtension k v e)))
    \(ContextState t n e) -> ContextManagement <$> req "type" t <*> Right n <*> Right e
  where
    req n = maybe (Left ("missing " <> n)) Right
data ContextState = ContextState (Maybe Text) (Maybe Int) !Extensions

data Conversation = ConversationId !Text
    | ConversationObject { conversationId :: !Text, extraFields :: !Extensions }
    deriving stock (Eq, Show)
conversationEncoder :: E Conversation
conversationEncoder = Encoder.choose \case
    ConversationId{} ->
        Encoder.contramap conversationIdValue Encoder.text
    ConversationObject{} ->
        Encoder.objectWithExtensions conversationFields
            [ Encoder.field "id" Encoder.text conversationObjectId ]
  where
    conversationIdValue = \case
        ConversationId value -> value
        _ -> impossible
    conversationObjectId = \case
        ConversationObject { conversationId = value } -> value
        _ -> impossible
    conversationFields = \case
        ConversationObject { extraFields = value } -> value
        _ -> impossible
    impossible = error "conversationEncoder: impossible constructor"
conversationDecoder :: D Conversation
conversationDecoder = Decoder.byType \case
    Decoder.JsonString -> ConversationId <$> Decoder.text
    Decoder.JsonObject -> conversationObjectDecoder
    _ -> Decoder.mapEither
        (const (Left "Conversation: expected string or object"))
        Decoder.skip
  where
    conversationObjectDecoder = Decoder.object (ConversationState Nothing emptyExtensions)
        [Decoder.field "id" Decoder.text \v (ConversationState _ e) -> Right (ConversationState (Just v) e)]
        (Decoder.unknownField Decoder.rawJson \k v (ConversationState i e) -> Right (ConversationState i (insertExtension k v e)))
        \(ConversationState i e) -> ConversationObject <$> req "id" i <*> Right e
    req n = maybe (Left ("missing " <> n)) Right
data ConversationState = ConversationState (Maybe Text) !Extensions

data Prompt = Prompt
    { promptId :: !Text, promptVariables :: !(Maybe Extensions)
    , promptVersion :: !(Maybe Text), extraFields :: !Extensions }
    deriving stock (Eq, Show)
promptEncoder :: E Prompt
promptEncoder = Encoder.objectWithExtensions (.extraFields)
    [ Encoder.field "id" Encoder.text (.promptId)
    , Encoder.optionalField "variables" (Encoder.objectWithExtensions id []) (.promptVariables)
    , Encoder.optionalField "version" Encoder.text (.promptVersion) ]
promptDecoder :: D Prompt
promptDecoder = Decoder.object (PromptState Nothing Nothing Nothing emptyExtensions)
    [ Decoder.field "id" Decoder.text \v (PromptState _ vars ver e) -> Right (PromptState (Just v) vars ver e)
    , Decoder.field "variables" (Decoder.nullable extensionsDecoder) \v (PromptState i _ ver e) -> Right (PromptState i v ver e)
    , Decoder.field "version" (Decoder.nullable Decoder.text) \v (PromptState i vars _ e) -> Right (PromptState i vars v e) ]
    (Decoder.unknownField Decoder.rawJson \k v (PromptState i vars ver e) -> Right (PromptState i vars ver (insertExtension k v e)))
    \(PromptState i vars ver e) -> Prompt <$> req "id" i <*> Right vars <*> Right ver <*> Right e
  where req n = maybe (Left ("missing " <> n)) Right
data PromptState = PromptState (Maybe Text) (Maybe Extensions) (Maybe Text) !Extensions

data PromptCacheOptions = PromptCacheOptions
    { mode :: !(Maybe Text), ttl :: !(Maybe Text), extraFields :: !Extensions }
    deriving stock (Eq, Show)
promptCacheOptionsEncoder :: E PromptCacheOptions
promptCacheOptionsEncoder = Encoder.objectWithExtensions (.extraFields)
    [Encoder.optionalField "mode" Encoder.text (.mode), Encoder.optionalField "ttl" Encoder.text (.ttl)]
promptCacheOptionsDecoder :: D PromptCacheOptions
promptCacheOptionsDecoder = Decoder.object (PCS Nothing Nothing emptyExtensions)
    [Decoder.field "mode" (Decoder.nullable Decoder.text) \v (PCS _ t e) -> Right (PCS v t e)
    ,Decoder.field "ttl" (Decoder.nullable Decoder.text) \v (PCS m _ e) -> Right (PCS m v e)]
    (Decoder.unknownField Decoder.rawJson \k v (PCS m t e) -> Right (PCS m t (insertExtension k v e)))
    \(PCS m t e) -> Right (PromptCacheOptions m t e)
data PCS = PCS (Maybe Text) (Maybe Text) !Extensions

data ReasoningConfig = ReasoningConfig
    { context :: !(Maybe Text), effort :: !(Maybe Text), generateSummary :: !(Maybe Text)
    , reasoningMode :: !(Maybe Text), summary :: !(Maybe Text), extraFields :: !Extensions }
    deriving stock (Eq, Show)
reasoningConfigEncoder :: E ReasoningConfig
reasoningConfigEncoder = Encoder.objectWithExtensions (.extraFields)
    [Encoder.optionalField "context" Encoder.text (.context), Encoder.optionalField "effort" Encoder.text (.effort)
    ,Encoder.optionalField "generate_summary" Encoder.text (.generateSummary), Encoder.optionalField "mode" Encoder.text (.reasoningMode)
    ,Encoder.optionalField "summary" Encoder.text (.summary)]
reasoningConfigDecoder :: D ReasoningConfig
reasoningConfigDecoder = Decoder.object (RCS Nothing Nothing Nothing Nothing Nothing emptyExtensions)
    [Decoder.field "context" (Decoder.nullable Decoder.text) \v (RCS _ b c d e x) -> Right (RCS v b c d e x)
    ,Decoder.field "effort" (Decoder.nullable Decoder.text) \v (RCS a _ c d e x) -> Right (RCS a v c d e x)
    ,Decoder.field "generate_summary" (Decoder.nullable Decoder.text) \v (RCS a b _ d e x) -> Right (RCS a b v d e x)
    ,Decoder.field "mode" (Decoder.nullable Decoder.text) \v (RCS a b c _ e x) -> Right (RCS a b c v e x)
    ,Decoder.field "summary" (Decoder.nullable Decoder.text) \v (RCS a b c d _ x) -> Right (RCS a b c d v x)]
    (Decoder.unknownField Decoder.rawJson \k v (RCS a b c d e x) -> Right (RCS a b c d e (insertExtension k v x)))
    \(RCS a b c d e x) -> Right (ReasoningConfig a b c d e x)
data RCS = RCS (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) !Extensions

data ResponseFormat
    = ResponseFormatText { extraFields :: !Extensions }
    | ResponseFormatJsonObject { extraFields :: !Extensions }
    | ResponseFormatJsonSchema { formatName :: !Text, formatDescription :: !(Maybe Text), formatSchema :: !RawJson, formatStrict :: !(Maybe Bool), extraFields :: !Extensions }
    | ResponseFormatUnknown !TaggedObject
    deriving stock (Eq, Show)
responseFormatEncoder :: E ResponseFormat
responseFormatEncoder = Encoder.choose \case
    ResponseFormatText{} -> textEncoder
    ResponseFormatJsonObject{} -> jsonObjectEncoder
    ResponseFormatJsonSchema{} -> jsonSchemaEncoder
    ResponseFormatUnknown{} ->
        Encoder.contramap unknownFormat taggedObjectEncoder
  where
    textEncoder = Encoder.objectWithExtensions formatFields
        [ Encoder.field "type" Encoder.text (const "text") ]
    jsonObjectEncoder = Encoder.objectWithExtensions formatFields
        [ Encoder.field "type" Encoder.text (const "json_object") ]
    jsonSchemaEncoder = Encoder.objectWithExtensions formatFields
        [ Encoder.field "type" Encoder.text (const "json_schema")
        , Encoder.field "name" Encoder.text (.formatName)
        , Encoder.optionalField
            "description"
            Encoder.text
            (.formatDescription)
        , Encoder.field "schema" Encoder.rawJson (.formatSchema)
        , Encoder.optionalField "strict" Encoder.bool (.formatStrict)
        ]
    formatFields = \case
        ResponseFormatText value -> value
        ResponseFormatJsonObject value -> value
        ResponseFormatJsonSchema { extraFields = value } -> value
        _ -> impossible
    unknownFormat = \case
        ResponseFormatUnknown value -> value
        _ -> impossible
    impossible = error "responseFormatEncoder: impossible constructor"
responseFormatDecoder :: D ResponseFormat
responseFormatDecoder =
    Decoder.discriminatedObject "type" \case
        "text" ->
            Decoder.mapDecoder
                ResponseFormatText
                extensionObjectDecoder
        "json_object" ->
            Decoder.mapDecoder
                ResponseFormatJsonObject
                extensionObjectDecoder
        "json_schema" ->
            Decoder.objectFields $
                ResponseFormatJsonSchema
                    <$> Decoder.requiredField "name" Decoder.text
                    <*> Decoder.optionalField
                        "description"
                        Decoder.text
                    <*> Decoder.requiredField
                        "schema"
                        Decoder.rawJson
                    <*> Decoder.optionalField
                        "strict"
                        Decoder.bool
                    <*> Decoder.extensionFields
                    <* Decoder.defaultField
                        ()
                        "type"
                        (() <$ Decoder.text)
        _ ->
            ResponseFormatUnknown <$> taggedDecoder
  where
    extensionObjectDecoder =
        Decoder.objectFields $
            Decoder.extensionFields
                <* Decoder.defaultField
                    ()
                    "type"
                    (() <$ Decoder.text)

data ResponseTextConfig = ResponseTextConfig
    { format :: !(Maybe ResponseFormat), verbosity :: !(Maybe Text), extraFields :: !Extensions }
    deriving stock (Eq, Show)
responseTextConfigEncoder :: E ResponseTextConfig
responseTextConfigEncoder = Encoder.objectWithExtensions (.extraFields)
    [Encoder.optionalField "format" responseFormatEncoder (.format), Encoder.optionalField "verbosity" Encoder.text (.verbosity)]
responseTextConfigDecoder :: D ResponseTextConfig
responseTextConfigDecoder = Decoder.object (RTCS Nothing Nothing emptyExtensions)
    [Decoder.field "format" (Decoder.nullable responseFormatDecoder) \v (RTCS _ b e) -> Right (RTCS v b e)
    ,Decoder.field "format_" (Decoder.nullable responseFormatDecoder) \v (RTCS _ b e) -> Right (RTCS v b e)
    ,Decoder.field "verbosity" (Decoder.nullable Decoder.text) \v (RTCS a _ e) -> Right (RTCS a v e)]
    (Decoder.unknownField Decoder.rawJson \k v (RTCS a b e) -> Right (RTCS a b (insertExtension k v e)))
    \(RTCS a b e) -> Right (ResponseTextConfig a b e)
data RTCS = RTCS (Maybe ResponseFormat) (Maybe Text) !Extensions

data StreamOptions = StreamOptions { includeObfuscation :: !(Maybe Bool), extraFields :: !Extensions }
    deriving stock (Eq, Show)
streamOptionsEncoder :: E StreamOptions
streamOptionsEncoder = Encoder.objectWithExtensions (.extraFields)
    [Encoder.optionalField "include_obfuscation" Encoder.bool (.includeObfuscation)]
streamOptionsDecoder :: D StreamOptions
streamOptionsDecoder = Decoder.object (SOS Nothing emptyExtensions)
    [Decoder.field "include_obfuscation" (Decoder.nullable Decoder.bool) \v (SOS _ e) -> Right (SOS v e)]
    (Decoder.unknownField Decoder.rawJson \k v (SOS a e) -> Right (SOS a (insertExtension k v e)))
    \(SOS a e) -> Right (StreamOptions a e)
data SOS = SOS (Maybe Bool) !Extensions

data ToolChoiceMode = ToolChoiceNone | ToolChoiceAuto | ToolChoiceRequired | ToolChoiceModeUnknown !Text
    deriving stock (Eq, Show)
toolChoiceModeEncoder :: E ToolChoiceMode
toolChoiceModeEncoder = Encoder.contramap modeText Encoder.text
toolChoiceModeDecoder :: D ToolChoiceMode
toolChoiceModeDecoder = Decoder.mapDecoder parseMode Decoder.text
modeText ToolChoiceNone = "none"; modeText ToolChoiceAuto = "auto"; modeText ToolChoiceRequired = "required"; modeText (ToolChoiceModeUnknown t) = t
parseMode "none" = ToolChoiceNone; parseMode "auto" = ToolChoiceAuto; parseMode "required" = ToolChoiceRequired; parseMode t = ToolChoiceModeUnknown t

data ToolChoice = ToolChoiceMode !ToolChoiceMode | ToolChoiceObject !TaggedObject
    deriving stock (Eq, Show)
toolChoiceEncoder :: E ToolChoice
toolChoiceEncoder = Encoder.choose \case
    ToolChoiceMode{} ->
        Encoder.contramap toolChoiceModeValue toolChoiceModeEncoder
    ToolChoiceObject{} ->
        Encoder.contramap toolChoiceObjectValue taggedObjectEncoder
  where
    toolChoiceModeValue = \case
        ToolChoiceMode value -> value
        _ -> impossible
    toolChoiceObjectValue = \case
        ToolChoiceObject value -> value
        _ -> impossible
    impossible = error "toolChoiceEncoder: impossible constructor"
toolChoiceDecoder :: D ToolChoice
toolChoiceDecoder = Decoder.byType \case
    Decoder.JsonString ->
        ToolChoiceMode <$> toolChoiceModeDecoder
    Decoder.JsonObject ->
        ToolChoiceObject <$> taggedDecoder
    _ -> Decoder.mapEither
        (const (Left "ToolChoice: expected string or object"))
        Decoder.skip

data ResponseCreateParams = ResponseCreateParams
    { background :: !(Maybe Bool), contextManagement :: !(Maybe [ContextManagement]), conversation :: !(Maybe Conversation)
    , include :: !(Maybe [ResponseInclude]), input :: !(Maybe ResponseInput), instructions :: !(Maybe Text)
    , maxOutputTokens :: !(Maybe Int), maxToolCalls :: !(Maybe Int), metadata :: !(Maybe Extensions), model :: !(Maybe Text)
    , moderation :: !(Maybe RawJson), parallelToolCalls :: !(Maybe Bool), previousResponseId :: !(Maybe Text)
    , prompt :: !(Maybe Prompt), promptCacheKey :: !(Maybe Text), promptCacheOptions :: !(Maybe PromptCacheOptions)
    , promptCacheRetention :: !(Maybe Text), reasoning :: !(Maybe ReasoningConfig), safetyIdentifier :: !(Maybe Text)
    , serviceTier :: !(Maybe Text), store :: !(Maybe Bool), stream :: !(Maybe Bool), streamOptions :: !(Maybe StreamOptions)
    , temperature :: !(Maybe Scientific), text :: !(Maybe ResponseTextConfig), toolChoice :: !(Maybe ToolChoice)
    , tools :: !(Maybe [ResponseTool]), topLogprobs :: !(Maybe Int), topP :: !(Maybe Scientific), truncation :: !(Maybe Text)
    , user :: !(Maybe Text), extraFields :: !Extensions }
    deriving stock (Eq, Show)
defaultResponseCreateParams = ResponseCreateParams
    { background = Nothing, contextManagement = Nothing, conversation = Nothing
    , include = Nothing, input = Nothing, instructions = Nothing
    , maxOutputTokens = Nothing, maxToolCalls = Nothing, metadata = Nothing
    , model = Nothing, moderation = Nothing, parallelToolCalls = Nothing
    , previousResponseId = Nothing, prompt = Nothing, promptCacheKey = Nothing
    , promptCacheOptions = Nothing, promptCacheRetention = Nothing
    , reasoning = Nothing, safetyIdentifier = Nothing, serviceTier = Nothing
    , store = Nothing, stream = Nothing, streamOptions = Nothing
    , temperature = Nothing, text = Nothing, toolChoice = Nothing, tools = Nothing
    , topLogprobs = Nothing, topP = Nothing, truncation = Nothing, user = Nothing
    , extraFields = emptyExtensions }
responseCreateParamsEncoder :: E ResponseCreateParams
responseCreateParamsEncoder = Encoder.objectWithExtensions (.extraFields)
    [ Encoder.optionalField "background" Encoder.bool (.background), Encoder.optionalField "context_management" (Encoder.list contextManagementEncoder) (.contextManagement)
    , Encoder.optionalField "conversation" conversationEncoder (.conversation), Encoder.optionalField "include" (Encoder.list responseIncludeEncoder) (.include)
    , Encoder.optionalField "input" responseInputEncoder (.input), Encoder.optionalField "instructions" Encoder.text (.instructions)
    , Encoder.optionalField "max_output_tokens" Encoder.int (.maxOutputTokens), Encoder.optionalField "max_tool_calls" Encoder.int (.maxToolCalls)
    , Encoder.optionalField "metadata" (Encoder.objectWithExtensions id []) (.metadata), Encoder.optionalField "model" Encoder.text (.model)
    , Encoder.optionalField "moderation" Encoder.rawJson (.moderation), Encoder.optionalField "parallel_tool_calls" Encoder.bool (.parallelToolCalls)
    , Encoder.optionalField "previous_response_id" Encoder.text (.previousResponseId), Encoder.optionalField "prompt" promptEncoder (.prompt)
    , Encoder.optionalField "prompt_cache_key" Encoder.text (.promptCacheKey), Encoder.optionalField "prompt_cache_options" promptCacheOptionsEncoder (.promptCacheOptions)
    , Encoder.optionalField "prompt_cache_retention" Encoder.text (.promptCacheRetention), Encoder.optionalField "reasoning" reasoningConfigEncoder (.reasoning)
    , Encoder.optionalField "safety_identifier" Encoder.text (.safetyIdentifier), Encoder.optionalField "service_tier" Encoder.text (.serviceTier)
    , Encoder.optionalField "store" Encoder.bool (.store), Encoder.optionalField "stream" Encoder.bool (.stream), Encoder.optionalField "stream_options" streamOptionsEncoder (.streamOptions)
    , Encoder.optionalField "temperature" Encoder.scientific (.temperature), Encoder.optionalField "text" responseTextConfigEncoder (.text)
    , Encoder.optionalField "tool_choice" toolChoiceEncoder (.toolChoice), Encoder.optionalField "tools" (Encoder.list responseToolEncoder) (.tools)
    , Encoder.optionalField "top_logprobs" Encoder.int (.topLogprobs), Encoder.optionalField "top_p" Encoder.scientific (.topP)
    , Encoder.optionalField "truncation" Encoder.text (.truncation), Encoder.optionalField "user" Encoder.text (.user) ]
responseCreateParamsDecoder :: D ResponseCreateParams
responseCreateParamsDecoder = Decoder.object defaultResponseCreateParams
    [ Decoder.field "background" (Decoder.nullable Decoder.bool) \v s -> Right s { background = v }
    , Decoder.field "context_management" (Decoder.nullable (Decoder.list contextManagementDecoder)) \v s -> Right s { contextManagement = v }
    , Decoder.field "conversation" (Decoder.nullable conversationDecoder) \v s -> Right s { conversation = v }
    , Decoder.field "include" (Decoder.nullable (Decoder.list responseIncludeDecoder)) \v s -> Right s { include = v }
    , Decoder.field "input" (Decoder.nullable responseInputDecoder) \v s -> Right s { input = v }
    , Decoder.field "instructions" (Decoder.nullable Decoder.text) \v s -> Right s { instructions = v }
    , Decoder.field "max_output_tokens" (Decoder.nullable Decoder.int) \v s -> Right s { maxOutputTokens = v }
    , Decoder.field "max_tool_calls" (Decoder.nullable Decoder.int) \v s -> Right s { maxToolCalls = v }
    , Decoder.field "metadata" (Decoder.nullable extensionsDecoder) \v s -> Right s { metadata = v }
    , Decoder.field "model" (Decoder.nullable Decoder.text) \v s -> Right s { model = v }
    , Decoder.field "moderation" (Decoder.nullable Decoder.rawJson) \v s -> Right s { moderation = v }
    , Decoder.field "parallel_tool_calls" (Decoder.nullable Decoder.bool) \v s -> Right s { parallelToolCalls = v }
    , Decoder.field "previous_response_id" (Decoder.nullable Decoder.text) \v s -> Right s { previousResponseId = v }
    , Decoder.field "prompt" (Decoder.nullable promptDecoder) \v s -> Right s { prompt = v }
    , Decoder.field "prompt_cache_key" (Decoder.nullable Decoder.text) \v s -> Right s { promptCacheKey = v }
    , Decoder.field "prompt_cache_options" (Decoder.nullable promptCacheOptionsDecoder) \v s -> Right s { promptCacheOptions = v }
    , Decoder.field "prompt_cache_retention" (Decoder.nullable Decoder.text) \v s -> Right s { promptCacheRetention = v }
    , Decoder.field "reasoning" (Decoder.nullable reasoningConfigDecoder) \v s -> Right s { reasoning = v }
    , Decoder.field "safety_identifier" (Decoder.nullable Decoder.text) \v s -> Right s { safetyIdentifier = v }
    , Decoder.field "service_tier" (Decoder.nullable Decoder.text) \v s -> Right s { serviceTier = v }
    , Decoder.field "store" (Decoder.nullable Decoder.bool) \v s -> Right s { store = v }
    , Decoder.field "stream" (Decoder.nullable Decoder.bool) \v s -> Right s { stream = v }
    , Decoder.field "stream_options" (Decoder.nullable streamOptionsDecoder) \v s -> Right s { streamOptions = v }
    , Decoder.field "temperature" (Decoder.nullable Decoder.scientific) \v s -> Right s { temperature = v }
    , Decoder.field "text" (Decoder.nullable responseTextConfigDecoder) \v s -> Right s { text = v }
    , Decoder.field "tool_choice" (Decoder.nullable toolChoiceDecoder) \v s -> Right s { toolChoice = v }
    , Decoder.field "tools" (Decoder.nullable (Decoder.list responseToolDecoder)) \v s -> Right s { tools = v }
    , Decoder.field "top_logprobs" (Decoder.nullable Decoder.int) \v s -> Right s { topLogprobs = v }
    , Decoder.field "top_p" (Decoder.nullable Decoder.scientific) \v s -> Right s { topP = v }
    , Decoder.field "truncation" (Decoder.nullable Decoder.text) \v s -> Right s { truncation = v }
    , Decoder.field "user" (Decoder.nullable Decoder.text) \v s -> Right s { user = v } ]
    (Decoder.unknownField Decoder.rawJson addResponseExtension)
    Right

addResponseExtension :: Text -> RawJson -> ResponseCreateParams -> Either Text ResponseCreateParams
addResponseExtension k v ResponseCreateParams{..} =
    Right ResponseCreateParams
        { extraFields = insertExtension k v extraFields
        , ..
        }

taggedDecoder :: D TaggedObject
taggedDecoder = Decoder.objectFields $
    TaggedObject
        <$> Decoder.requiredField "type" Decoder.text
        <*> Decoder.extensionFields
