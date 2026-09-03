-- | Projection from the harness' canonical Responses-shaped transcript to
-- Gemini's native GenerateContent request.
module Agent.Gemini.Request
    ( GeminiRequest(..)
    , buildRequest
    , normalizeModelId
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Responses.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data GeminiRequest = GeminiRequest
    { requestModel :: !Text
    , requestBody :: !Value
    , requestCustomToolNames :: !(Set Text)
    } deriving (Eq, Show)

-- | Build one native request. The caller supplies the fallback model so this
-- pure projection can also be used by side requests such as compaction.
buildRequest
    :: Text
    -> ResponseCreateParams
    -> Either ApiError GeminiRequest
buildRequest fallbackModel params = do
    (tools, customToolNames) <-
        projectTools (enabledTools params.toolChoice params.tools)
    projection <- projectInput customToolNames params.input
    let model = normalizeModelId
            (fromMaybe fallbackModel params.model)
        systemTexts =
            maybe projection.systemTexts
                (: projection.systemTexts)
                (params.instructions >>= nonEmptyText)
        bodyFields =
            [ Just ("contents" .= projection.contents)
            , ("systemInstruction" .=)
                <$> systemInstruction systemTexts
            , ("generationConfig" .=)
                <$> generationConfig model params
            , ("tools" .=) <$> tools
            , ("toolConfig" .=) <$> (tools >> toolConfig params.toolChoice)
            ]
    if Text.null model
        then Left $ ProviderError InvalidRequestError
            "Gemini model id must not be empty"
            Nothing
        else pure GeminiRequest
            { requestModel = model
            , requestBody = object (catMaybes bodyFields)
            , requestCustomToolNames = customToolNames
            }

normalizeModelId :: Text -> Text
normalizeModelId raw =
    fromMaybe stripped (Text.stripPrefix "models/" stripped)
  where
    stripped = Text.strip raw

enabledTools
    :: Maybe ToolChoice
    -> Maybe [ResponseTool]
    -> Maybe [ResponseTool]
enabledTools (Just (ToolChoiceMode ToolChoiceNone)) _ = Nothing
enabledTools _ tools = tools

data Projection = Projection
    { systemTexts :: ![Text]
    , contents :: ![Value]
    , callNames :: !(Map Text Text)
    , pendingThoughtSignature :: !(Maybe Text)
    , customToolNames :: !(Set Text)
    }

emptyProjection :: Set Text -> Projection
emptyProjection customToolNames = Projection
    { systemTexts = []
    , contents = []
    , callNames = Map.empty
    , pendingThoughtSignature = Nothing
    , customToolNames
    }

projectInput :: Set Text -> Maybe ResponseInput -> Either ApiError Projection
projectInput customToolNames = \case
    Nothing -> pure (emptyProjection customToolNames)
    Just (ResponseInputText text) ->
        pure $ appendContent "user" [textPart text]
            (emptyProjection customToolNames)
    Just (ResponseInputItems items) ->
        foldM projectItem (emptyProjection customToolNames) items

projectItem :: Projection -> ResponseItem -> Either ApiError Projection
projectItem projection = \case
    MessageItem message ->
        projectMessage projection message.role message.content
    AgentMessageItem message ->
        projectMessage projection RoleUser
            (MessageContentParts message.content)
    ReasoningItemValue reasoning ->
        pure (projectReasoning projection reasoning)
    FunctionCallItem call ->
        projectFunctionCall projection call
    FunctionCallOutputItem callOutput ->
        pure (projectFunctionOutput projection callOutput)
    CustomToolCallItem call ->
        projectCustomToolCall projection call
    CustomToolCallOutputItem callOutput ->
        pure (projectCustomToolOutput projection callOutput)
    -- Local compaction snapshots are represented by ordinary messages. The
    -- remaining Responses-only control/hosted-tool items have no native
    -- GenerateContent equivalent and are intentionally not replayed.
    ComputerCallItem{} -> unsupportedItem "computer calls"
    ComputerCallOutputItem{} -> unsupportedItem "computer outputs"
    AdditionalToolsItemValue{} -> pure projection
    ItemReferenceValue{} -> pure projection
    LocalShellCallItem{} -> pure projection
    ToolSearchCallItem{} -> pure projection
    ToolSearchOutputItem{} -> pure projection
    WebSearchCallItem{} -> pure projection
    ImageGenerationCallItem{} -> pure projection
    CompactionItemValue{} -> pure projection
    CompactionTriggerItemValue{} -> pure projection
    ContextCompactionItemValue{} -> pure projection
    KnownResponseItem{} -> pure projection
    UnknownResponseItem{} -> pure projection
  where
    unsupportedItem label = Left $ ProviderError InvalidRequestError
        ("Gemini does not support replaying " <> label)
        Nothing

projectMessage
    :: Projection
    -> ResponseRole
    -> MessageContent
    -> Either ApiError Projection
projectMessage projection role content =
    let parts = messageParts content
    in case role of
        RoleSystem -> pure projection
            { systemTexts =
                projection.systemTexts <> textFromMessage content
            }
        RoleDeveloper -> pure projection
            { systemTexts =
                projection.systemTexts <> textFromMessage content
            }
        RoleAssistant ->
            pure (appendModelParts parts projection)
        RoleUnknown "model" ->
            pure (appendModelParts parts projection)
        RoleUnknown _ ->
            pure (appendContent "user" parts projection)
        RoleUser ->
            pure (appendContent "user" parts projection)

appendModelParts :: [Value] -> Projection -> Projection
appendModelParts [] projection = projection
appendModelParts parts projection =
    appendContent "model" signedParts projection
        { pendingThoughtSignature = Nothing }
  where
    signedParts = attachSignature
        projection.pendingThoughtSignature
        parts

projectReasoning :: Projection -> ReasoningItem -> Projection
projectReasoning projection reasoning
    | Just text <- nonEmptyText (reasoningItemText reasoning) =
        appendContent "model"
            [ object $
                [ "text" .= text
                , "thought" .= True
                ]
                <> maybe []
                    (\signature -> ["thoughtSignature" .= signature])
                    signature
            ]
            projection
                { pendingThoughtSignature = Nothing }
    | Just encrypted <- reasoning.encryptedContent =
        projection { pendingThoughtSignature = Just encrypted }
    | otherwise = projection
  where
    signature =
        reasoning.encryptedContent
            <|> projection.pendingThoughtSignature

projectFunctionCall
    :: Projection
    -> FunctionCall
    -> Either ApiError Projection
projectFunctionCall projection call = do
    args <-
        if call.name `Set.member` projection.customToolNames
            then pure (object ["input" .= call.arguments])
            else case Aeson.eitherDecodeStrict' (encodeUtf8 call.arguments) of
                Right value -> pure value
                Left _ -> Left $ ProviderError InvalidRequestError
                    ( "Gemini function call `" <> call.name
                        <> "` arguments must be valid JSON"
                    )
                    Nothing
    pure $ appendContent "model" [part args] projection
        { callNames = Map.insert call.callId call.name projection.callNames
        , pendingThoughtSignature = Nothing
        }
  where
    functionCall args = object
        [ "id" .= call.callId
        , "name" .= call.name
        , "args" .= args
        ]
    part args = object $
        ["functionCall" .= functionCall args]
            <> maybe []
                (\signature -> ["thoughtSignature" .= signature])
                projection.pendingThoughtSignature

projectCustomToolCall
    :: Projection
    -> CustomToolCall
    -> Either ApiError Projection
projectCustomToolCall projection call =
    projectFunctionCall projection FunctionCall
        { itemId = call.itemId
        , callId = call.callId
        , name = call.name
        , namespace = call.namespace
        , provider = Just "gemini"
        , arguments = call.input
        , encryptedFunctionArgs = Nothing
        , status = call.status
        }

projectCustomToolOutput
    :: Projection
    -> CustomToolCallOutput
    -> Projection
projectCustomToolOutput projection callOutput =
    projectFunctionOutput projection FunctionCallOutput
        { itemId = callOutput.itemId
        , callId = callOutput.callId
        , name = callOutput.name
        , namespace = Nothing
        , provider = Just "gemini"
        , output = callOutput.output
        , status = callOutput.status
        }

projectFunctionOutput :: Projection -> FunctionCallOutput -> Projection
projectFunctionOutput projection callOutput =
    appendContent "user" [part] projection
  where
    callName = fromMaybe "tool"
        (callOutput.name
            <|> Map.lookup callOutput.callId projection.callNames)
    result = toJSON callOutput.output
    responseObject = case result of
        Object objectValue -> Object objectValue
        value -> object ["result" .= value]
    part = object
        [ "functionResponse" .= object
            [ "id" .= callOutput.callId
            , "name" .= callName
            , "response" .= responseObject
            ]
        ]

appendContent :: Text -> [Value] -> Projection -> Projection
appendContent _ [] projection = projection
appendContent role parts projection =
    projection { contents = appendOrMerge projection.contents }
  where
    next = object ["role" .= role, "parts" .= parts]
    appendOrMerge [] = [next]
    appendOrMerge values =
        case unsnoc values of
            Just (prefix, Object lastContent)
                | Just (String lastRole) <- KeyMap.lookup "role" lastContent
                , lastRole == role
                , Just (Array lastParts) <- KeyMap.lookup "parts" lastContent ->
                    prefix
                        <> [Object (KeyMap.insert "parts"
                                (toJSON (toList lastParts <> parts))
                                lastContent)]
            _ -> values <> [next]

attachSignature :: Maybe Text -> [Value] -> [Value]
attachSignature Nothing parts = parts
attachSignature _ [] = []
attachSignature (Just signature) (Object first : rest) =
    Object (KeyMap.insert "thoughtSignature" (String signature) first) : rest
attachSignature _ parts = parts

messageParts :: MessageContent -> [Value]
messageParts = \case
    MessageContentText text -> [textPart text]
    MessageContentParts parts -> concatMap contentPart parts

textFromMessage :: MessageContent -> [Text]
textFromMessage = \case
    MessageContentText text -> maybe [] pure (nonEmptyText text)
    MessageContentParts parts ->
        mapMaybe contentPartText parts

contentPart :: ResponseContentPart -> [Value]
contentPart = \case
    InputTextPart{text} -> [textPart text]
    OutputTextPart{text} -> [textPart text]
    PlainTextPart{text} -> [textPart text]
    ReasoningTextPart{text} -> [textPart text]
    SummaryTextPart{text} -> [textPart text]
    RefusalPart{refusal} -> [textPart refusal]
    InputImagePart{imageUrl = Just url} ->
        [dataOrFilePart Nothing url]
    InputFilePart{fileData = Just fileData} ->
        [dataOrFilePart Nothing fileData]
    InputFilePart{fileUrl = Just fileUrl} ->
        [dataOrFilePart Nothing fileUrl]
    -- File ids belong to Responses' upload API and cannot be dereferenced by
    -- Google's API. Encrypted/unknown parts are provider-private.
    InputImagePart{} -> []
    InputFilePart{} -> []
    InputAudioPart{} -> []
    EncryptedContentPart{} -> []
    UnknownContentPart{} -> []

contentPartText :: ResponseContentPart -> Maybe Text
contentPartText = \case
    InputTextPart{text} -> nonEmptyText text
    OutputTextPart{text} -> nonEmptyText text
    PlainTextPart{text} -> nonEmptyText text
    ReasoningTextPart{text} -> nonEmptyText text
    SummaryTextPart{text} -> nonEmptyText text
    RefusalPart{refusal} -> nonEmptyText refusal
    InputImagePart{} -> Nothing
    InputFilePart{} -> Nothing
    InputAudioPart{} -> Nothing
    EncryptedContentPart{} -> Nothing
    UnknownContentPart{} -> Nothing

dataOrFilePart :: Maybe Text -> Text -> Value
dataOrFilePart fallbackMime source =
    case parseDataUrl source of
        Just (mime, payload) -> object
            [ "inlineData" .= object
                [ "mimeType" .= mime
                , "data" .= payload
                ]
            ]
        Nothing -> object
            [ "fileData" .= object
                [ "mimeType" .= fromMaybe "application/octet-stream" fallbackMime
                , "fileUri" .= source
                ]
            ]

parseDataUrl :: Text -> Maybe (Text, Text)
parseDataUrl source = do
    remainder <- Text.stripPrefix "data:" source
    let (header, withComma) = Text.breakOn "," remainder
    payload <- Text.stripPrefix "," withComma
    mime <- Text.stripSuffix ";base64" header
    if Text.null mime || Text.null payload
        then Nothing
        else Just (mime, payload)

reasoningItemText :: ReasoningItem -> Text
reasoningItemText reasoning =
    Text.concat selected
  where
    summaries =
        mapMaybe (\part -> part.text) reasoning.summary
    contentTexts =
        maybe [] (mapMaybe contentPartText) reasoning.content
    selected
        | null summaries = contentTexts
        | otherwise = summaries

textPart :: Text -> Value
textPart text = object ["text" .= text]

systemInstruction :: [Text] -> Maybe Value
systemInstruction values = do
    text <- nonEmptyText (Text.intercalate "\n\n" values)
    pure $ object ["parts" .= [textPart text]]

generationConfig :: Text -> ResponseCreateParams -> Maybe Value
generationConfig model params
    | null fields = Nothing
    | otherwise = Just (object fields)
  where
    fields = catMaybes
        [ ("maxOutputTokens" .=) <$> params.maxOutputTokens
        , ("temperature" .=) <$> params.temperature
        , ("topP" .=) <$> params.topP
        , ("thinkingConfig" .=) <$> thinkingConfig model params.reasoning
        , ("responseMimeType" .=) <$> responseMimeType params.text
        , ("responseJsonSchema" .=) <$> responseJsonSchema params.text
        ]

thinkingConfig :: Text -> Maybe ReasoningConfig -> Maybe Value
thinkingConfig model reasoning = do
    effort <- reasoning >>= (.effort) >>= nonEmptyText
    let normalized = Text.toLower (Text.strip effort)
    pure $ object
        [ "thinkingLevel" .= thinkingLevel model effort
        , "includeThoughts" .= (normalized /= "none")
        ]

thinkingLevel :: Text -> Text -> Text
thinkingLevel model effort = case Text.toLower (Text.strip effort) of
    "none" -> minimumThinkingLevel model
    "minimal" -> minimumThinkingLevel model
    "low" -> "LOW"
    "medium" -> "MEDIUM"
    "high" -> "HIGH"
    "xhigh" -> "HIGH"
    _ -> "MEDIUM"

-- Gemini's lowest accepted thinking level is model-specific. In particular,
-- Gemini 3.7 Flash and 3.1 Pro reject MINIMAL, while 3.5 Flash-Lite accepts it.
-- LOW is the safest minimum for unknown text models.
minimumThinkingLevel :: Text -> Text
minimumThinkingLevel model
    | normalizeModelId model `Set.member` minimalThinkingModels = "MINIMAL"
    | otherwise = "LOW"
  where
    minimalThinkingModels = Set.fromList
        [ "gemini-3.5-flash-lite"
        , "gemini-3.5-flash"
        , "gemini-3.6-flash"
        , "gemini-3-flash-preview"
        ]

responseMimeType :: Maybe ResponseTextConfig -> Maybe Text
responseMimeType config = config >>= (.format) >>= \case
    ResponseFormatText -> Nothing
    ResponseFormatJsonObject -> Just "application/json"
    ResponseFormatJsonSchema{} -> Just "application/json"
    ResponseFormatUnknown{} -> Nothing

responseJsonSchema :: Maybe ResponseTextConfig -> Maybe Value
responseJsonSchema config = config >>= (.format) >>= \case
    ResponseFormatJsonSchema{formatSchema} -> Just (toJSON formatSchema)
    ResponseFormatText -> Nothing
    ResponseFormatJsonObject -> Nothing
    ResponseFormatUnknown{} -> Nothing

projectTools
    :: Maybe [ResponseTool]
    -> Either ApiError (Maybe [Value], Set Text)
projectTools Nothing = pure (Nothing, Set.empty)
projectTools (Just []) = pure (Nothing, Set.empty)
projectTools (Just tools) = do
    projected <- traverse projectTool tools
    let declarations = concatMap fst projected
        nativeTools = concatMap (fst . snd) projected
        customToolNames =
            Set.unions (map (snd . snd) projected)
        functionTools = case declarations of
            [] -> []
            values ->
                [object ["functionDeclarations" .= values]]
        allTools = functionTools <> nativeTools
    pure $ case allTools of
        [] -> (Nothing, customToolNames)
        values -> (Just values, customToolNames)

projectTool
    :: ResponseTool
    -> Either ApiError ([Value], ([Value], Set Text))
projectTool = \case
    FunctionToolValue tool ->
        pure
            ( [object (catMaybes
                [ Just ("name" .= tool.name)
                , ("description" .=) <$> tool.description
                , ("parametersJsonSchema" .=)
                    . toJSON <$> tool.parameters
                ])]
            , ([], Set.empty)
            )
    NamespaceToolValue namespace ->
        combineProjected <$> traverse projectTool namespace.tools
    CustomToolValue tool ->
        pure
            ( [object
                [ "name" .= tool.name
                , "description" .= customToolDescription tool
                , "parametersJsonSchema" .= customToolSchema
                ]]
            , ([], Set.singleton tool.name)
            )
    KnownResponseTool ToolWebSearch ->
        pure
            ( []
            , ([object ["googleSearch" .= object []]], Set.empty)
            )
    KnownResponseTool ToolWebSearchPreview ->
        pure
            ( []
            , ([object ["googleSearch" .= object []]], Set.empty)
            )
    KnownResponseTool toolType ->
        unsupported (responseToolTypeText toolType)
    UnknownResponseTool{} -> unsupported "unknown"
  where
    unsupported kind = Left $ ProviderError InvalidRequestError
        ("Gemini does not support tool type " <> kind)
        Nothing

    combineProjected values =
        ( concatMap fst values
        , ( concatMap (fst . snd) values
          , Set.unions (map (snd . snd) values)
          )
        )

customToolDescription :: CustomTool -> Text
customToolDescription tool =
    fromMaybe ("Run " <> tool.name) tool.description
        <> "\n\nPass the tool's complete raw input in the `input` field."

customToolSchema :: Value
customToolSchema = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "input" .= object
            [ "type" .= ("string" :: Text)
            , "description" .=
                ("Complete raw input for the custom tool." :: Text)
            ]
        ]
    , "required" .= ["input" :: Text]
    , "additionalProperties" .= False
    ]

toolConfig :: Maybe ToolChoice -> Maybe Value
toolConfig choice = choice >>= \case
    ToolChoiceMode mode -> Just $ object
        [ "functionCallingConfig" .= object
            ["mode" .= modeText mode]
        ]
    ToolChoiceObject{} -> Nothing
  where
    modeText = \case
        ToolChoiceNone -> ("NONE" :: Text)
        ToolChoiceAuto -> "AUTO"
        ToolChoiceRequired -> "ANY"
        ToolChoiceModeUnknown value -> Text.toUpper value

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value

encodeUtf8 :: Text -> BS.ByteString
encodeUtf8 = TextEncoding.encodeUtf8

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc values = Just (init values, last values)

toList = Foldable.toList
