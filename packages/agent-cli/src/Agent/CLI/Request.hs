-- | Construction of provider-neutral Responses requests.
module Agent.CLI.Request
    ( requestParams
    , requestPromptParts
    , requestToolIdentities
    , setRequestInstructions
    , setRequestInstructionsAndTools
    , setRequestModel
    , setRequestPromptCacheKey
    ) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.Responses.Types
import Agent.Responses.Types.Tools (responseToolDecoder)
import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.Json.Decode qualified as Hermes
import Agent.Provider (Provider(..))
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Codex requires @store = false@. Continuation still uses
-- @previous_response_id@, with the local transcript available for recovery.
requestParams
    :: Provider
    -> Text
    -> Text
    -> [ResponseTool]
    -> Text
    -> ResponseCreateParams
requestParams provider modelName instructionText toolSchemas effort =
    case defaultResponseCreateParams of
        ResponseCreateParams{..} ->
            let responsesLite =
                    provider == OpenAIProvider
                        && isCodexResponsesLiteModel modelName
            in ResponseCreateParams
                { model = Just modelName
                , instructions =
                    if responsesLite then Nothing else Just instructionText
                , input =
                    if responsesLite
                        then Just
                            (ResponseInputItems
                                (responsesLitePrefix instructionText toolSchemas))
                        else Nothing
                , include =
                    Just [ResponseInclude "reasoning.encrypted_content"]
                , parallelToolCalls = Just (not responsesLite)
                , reasoning = Just ReasoningConfig
                    { context =
                        if responsesLite then Just "all_turns" else Nothing
                    , effort = Just effort
                    , generateSummary = Nothing
                    , reasoningMode = Nothing
                    , summary =
                        if provider == OpenAIProvider
                            then Just "auto"
                            else Nothing
                    }
                , store = Just False
                , stream = Just True
                , text =
                    if responsesLite
                        then Just liteTextConfig
                        else Nothing
                , toolChoice = Just (ToolChoiceMode ToolChoiceAuto)
                , tools =
                    if responsesLite then Nothing else Just toolSchemas
                , ..
                }

-- | Route repeated requests for one durable session to the same provider-side
-- prompt-cache bucket. This is only a routing hint, not a forced cache hit:
-- the provider still requires an unchanged request prefix. Changed schemas or
-- instructions naturally miss rather than reusing stale content.
setRequestPromptCacheKey
    :: Text
    -> ResponseCreateParams
    -> ResponseCreateParams
setRequestPromptCacheKey cacheKey ResponseCreateParams{..} =
    ResponseCreateParams
        { promptCacheKey = Just cacheKey
        , ..
        }

-- | Extract the provider-visible instruction/tool prefix regardless of
-- whether the request uses conventional Responses fields or Responses Lite's
-- ordered input template.
requestPromptParts :: ResponseCreateParams -> (Text, [ResponseTool])
requestPromptParts params
    | requestUsesResponsesLite params =
        let (instructionText, toolSchemas, _) =
                responsesLiteTemplateParts params.input
        in (instructionText, toolSchemas)
    | otherwise =
        ( fromMaybe "" params.instructions
        , fromMaybe [] params.tools
        )

-- | Stable ordered identities used to decide whether persisted tool schemas
-- are safe to reuse. Descriptions and JSON schemas may evolve between binary
-- versions without invalidating a resume, while adding, removing, reordering,
-- or renaming a tool starts a new prompt epoch.
requestToolIdentities :: [ResponseTool] -> [Text]
requestToolIdentities = concatMap identity
  where
    identity = \case
        FunctionToolValue tool -> ["function:" <> tool.name]
        CustomToolValue tool -> ["custom:" <> tool.name]
        NamespaceToolValue namespace ->
            ["namespace:" <> namespace.name <> ":begin"]
                <> concatMap identity namespace.tools
                <> ["namespace:" <> namespace.name <> ":end"]
        KnownResponseTool toolType ->
            ["known:" <> responseToolTypeText toolType]
        UnknownResponseTool tagged -> ["unknown:" <> tagged.tag]

-- | Change the wire model while keeping the request dialect coherent.
--
-- A plain record update is not sufficient when crossing the Responses Lite
-- boundary: Lite moves instructions and tools into an ordered input prefix,
-- disables parallel tool calls, and changes reasoning context.
setRequestModel
    :: Provider
    -> Text
    -> ResponseCreateParams
    -> ResponseCreateParams
setRequestModel provider modelName params
    | oldLite == newLite =
        setModelField modelName
            (if params.model == Just modelName
                then params
                else clearServiceTier params)
    | newLite =
        let instructionText = fromMaybe "" params.instructions
            toolSchemas = fromMaybe [] params.tools
            suffix = responseInputItems params.input
        in case params of
            ResponseCreateParams{..} ->
                ResponseCreateParams
                    { model = Just modelName
                    , instructions = Nothing
                    , input = Just (ResponseInputItems
                        (responsesLitePrefix instructionText toolSchemas <> suffix))
                    , parallelToolCalls = Just False
                    , reasoning = setReasoningContext
                        (Just "all_turns")
                        reasoning
                    , serviceTier =
                        if params.model == Just modelName
                            then serviceTier
                            else Nothing
                    , text = Just (toResponsesLiteTextConfig text)
                    , tools = Nothing
                    , ..
                    }
    | otherwise =
        let (instructionText, toolSchemas, suffix) =
                responsesLiteTemplateParts params.input
        in case params of
            ResponseCreateParams{..} ->
                ResponseCreateParams
                    { model = Just modelName
                    , instructions = Just instructionText
                    , input =
                        if null suffix
                            then Nothing
                            else Just (ResponseInputItems suffix)
                    , parallelToolCalls = Just True
                    , reasoning = setReasoningContext Nothing reasoning
                    , serviceTier =
                        if params.model == Just modelName
                            then serviceTier
                            else Nothing
                    , text = fromResponsesLiteTextConfig text
                    , tools = Just toolSchemas
                    , ..
                    }
  where
    oldLite = requestUsesResponsesLite params
    newLite =
        provider == OpenAIProvider
            && isCodexResponsesLiteModel modelName

setRequestInstructions :: Text -> ResponseCreateParams -> ResponseCreateParams
setRequestInstructions instructionText =
    setRequestInstructionsAndTools instructionText Nothing

-- | Refresh instructions and optionally tools without corrupting the active
-- request dialect. This is used when shell/tool settings change at runtime.
setRequestInstructionsAndTools
    :: Text
    -> Maybe [ResponseTool]
    -> ResponseCreateParams
    -> ResponseCreateParams
setRequestInstructionsAndTools instructionText maybeTools params
    | requestUsesResponsesLite params =
        case params of
            ResponseCreateParams{..} ->
                ResponseCreateParams
                    { instructions = Nothing
                    , input = Just (ResponseInputItems
                        (updateResponsesLitePrefix
                            instructionText
                            maybeTools
                            input))
                    , tools = Nothing
                    , ..
                    }
    | otherwise =
        case params of
            ResponseCreateParams{..} ->
                ResponseCreateParams
                    { instructions = Just instructionText
                    , tools = maybe tools Just maybeTools
                    , ..
                    }

requestUsesResponsesLite :: ResponseCreateParams -> Bool
requestUsesResponsesLite params =
    case responseInputItems params.input of
        first : _ | isAdditionalToolsItem first -> True
        _ ->
            maybe False isCodexResponsesLiteModel params.model
                && params.instructions == Nothing
                && params.tools == Nothing

responsesLitePrefix :: Text -> [ResponseTool] -> [ResponseItem]
responsesLitePrefix instructionText toolSchemas =
    additionalToolsItem (responsesLiteToolValues toolSchemas)
        : baseInstructionItems instructionText

updateResponsesLitePrefix
    :: Text
    -> Maybe [ResponseTool]
    -> Maybe ResponseInput
    -> [ResponseItem]
updateResponsesLitePrefix instructionText maybeTools existingInput =
    nextAdditional : baseInstructionItems instructionText <> suffix
  where
    existingItems = responseInputItems existingInput
    (existingAdditional, afterAdditional) =
        case existingItems of
            item : rest | isAdditionalToolsItem item -> (Just item, rest)
            _ -> (Nothing, existingItems)
    suffix = dropWhile isBaseInstructionsItem afterAdditional
    nextAdditional = case maybeTools of
        Just tools -> additionalToolsItem (responsesLiteToolValues tools)
        Nothing -> fromMaybe (additionalToolsItem []) existingAdditional

-- | Convert conventional Responses tools to the Lite @additional_tools@
-- representation. Function and custom tools share the default @functions@
-- namespace. Responses Lite does not accept the native @computer@ tool, so
-- expose the same local harness as a reserved screenshot-returning function.
responsesLiteToolValues :: [ResponseTool] -> [RawJson]
responsesLiteToolValues tools =
    case groupedValues of
        [] -> ungroupedValues
        _ ->
            let namespaceValue = rawJsonFromEncoding . Aeson.toEncoding $
                    Aeson.object
                    [ "type" Aeson..= ("namespace" :: Text)
                    , "name" Aeson..= ("functions" :: Text)
                    , "description" Aeson..= namespaceDescription
                    , "tools" Aeson..= groupedValues
                    ]
                insertionIndex = fromMaybe 0 firstGroupedPosition
            in take insertionIndex ungroupedValues
                <> [namespaceValue]
                <> drop insertionIndex ungroupedValues
  where
    (firstGroupedPosition, groupedValues, namespaceDescription, ungroupedValues) =
        foldl collect (Nothing, [], "", []) tools

    collect (firstPosition, grouped, description, ungrouped) tool =
        case groupedToolValues tool of
            Just (values, nextDescription) ->
                ( firstPosition <|> Just (length ungrouped)
                , grouped <> values
                , fromMaybe description nextDescription
                , ungrouped
                )
            Nothing ->
                ( firstPosition
                , grouped
                , description
                , ungrouped <> [responsesLiteToolValue tool]
                )

responsesLiteToolValue :: ResponseTool -> RawJson
responsesLiteToolValue = \case
    KnownResponseTool ToolComputer ->
        rawJsonFromEncoding (Aeson.toEncoding computerFunctionNamespaceValue)
    tool -> encodeTool tool

computerFunctionNamespaceValue :: Aeson.Value
computerFunctionNamespaceValue = Aeson.object
    [ "type" Aeson..= ("namespace" :: Text)
    , "name" Aeson..= computerFunctionNamespace
    , "description" Aeson..=
        ("Inspect and control the local macOS desktop. Every call returns a fresh screenshot." :: Text)
    , "tools" Aeson..=
        [ Aeson.object
            [ "type" Aeson..= ("function" :: Text)
            , "name" Aeson..= computerFunctionName
            , "description" Aeson..=
                ("Run one or more approved desktop actions and return a fresh screenshot. Start with screenshot when the UI state is unknown." :: Text)
            , "parameters" Aeson..= computerFunctionParameters
            , "strict" Aeson..= True
            ]
        ]
    ]

computerFunctionParameters :: Aeson.Value
computerFunctionParameters = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "properties" Aeson..= Aeson.object
        [ "actions" Aeson..= Aeson.object
            [ "type" Aeson..= ("array" :: Text)
            , "description" Aeson..=
                ("Ordered desktop actions. Coordinates are logical pixels on the main display." :: Text)
            , "items" Aeson..= Aeson.object
                ["anyOf" Aeson..= computerActionSchemas]
            , "minItems" Aeson..= (1 :: Int)
            , "maxItems" Aeson..= (128 :: Int)
            ]
        ]
    , "required" Aeson..= ["actions" :: Text]
    , "additionalProperties" Aeson..= False
    ]

computerActionSchemas :: [Aeson.Value]
computerActionSchemas =
    [ actionSchema "screenshot" []
    , actionSchema "click"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("button", enumProperty
            ["left", "right", "middle", "back", "forward"])
        , ("keys", keysProperty)
        ]
    , actionSchema "double_click"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("keys", keysProperty)
        ]
    , actionSchema "scroll"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("scroll_x", integerProperty)
        , ("scroll_y", integerProperty)
        , ("keys", keysProperty)
        ]
    , actionSchema "move"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("keys", keysProperty)
        ]
    , actionSchema "drag"
        [ ("path", Aeson.object
            [ "type" Aeson..= ("array" :: Text)
            , "items" Aeson..= Aeson.object
                [ "type" Aeson..= ("object" :: Text)
                , "properties" Aeson..= Aeson.object
                    [ "x" Aeson..= integerProperty
                    , "y" Aeson..= integerProperty
                    ]
                , "required" Aeson..= ["x" :: Text, "y"]
                , "additionalProperties" Aeson..= False
                ]
            , "minItems" Aeson..= (2 :: Int)
            , "maxItems" Aeson..= (1024 :: Int)
            ])
        , ("keys", keysProperty)
        ]
    , actionSchema "type"
        [ ("text", Aeson.object
            [ "type" Aeson..= ("string" :: Text)
            , "maxLength" Aeson..= (8192 :: Int)
            ])
        ]
    , actionSchema "keypress" [("keys", keysProperty)]
    , actionSchema "wait" []
    ]
  where
    integerProperty :: Aeson.Value
    integerProperty = Aeson.object ["type" Aeson..= ("integer" :: Text)]
    keysProperty :: Aeson.Value
    keysProperty = Aeson.object
        [ "type" Aeson..= ("array" :: Text)
        , "items" Aeson..= Aeson.object
            [ "type" Aeson..= ("string" :: Text)
            , "maxLength" Aeson..= (64 :: Int)
            ]
        , "maxItems" Aeson..= (16 :: Int)
        ]
    enumProperty :: [Text] -> Aeson.Value
    enumProperty values = Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..= values
        ]

actionSchema :: Text -> [(Text, Aeson.Value)] -> Aeson.Value
actionSchema actionType properties = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "properties" Aeson..= Aeson.Object
        (KeyMap.fromList
            ((Key.fromText "type", enumProperty [actionType])
                : [(Key.fromText name, value) | (name, value) <- properties]))
    , "required" Aeson..= ("type" : map fst properties)
    , "additionalProperties" Aeson..= False
    ]
  where
    enumProperty :: [Text] -> Aeson.Value
    enumProperty values = Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..= values
        ]

groupedToolValues :: ResponseTool -> Maybe ([RawJson], Maybe Text)
groupedToolValues tool = case tool of
    FunctionToolValue{} -> Just ([encodeTool tool], Nothing)
    CustomToolValue{} -> Just ([encodeTool tool], Nothing)
    UnknownResponseTool tagged
        | tagged.tag == "custom" -> Just ([encodeTool tool], Nothing)
    NamespaceToolValue namespace
        | namespace.name == "functions" ->
            Just
                ( map encodeTool namespace.tools
                , nonBlank =<< namespace.description
                )
    _ -> Nothing

encodeTool :: ResponseTool -> RawJson
encodeTool = rawJsonFromEncoding . Aeson.toEncoding

additionalToolsItem :: [RawJson] -> ResponseItem
additionalToolsItem tools =
    AdditionalToolsItemValue AdditionalToolsItem
        { itemId = Nothing
        , role = "developer"
        , tools
        }

baseInstructionItems :: Text -> [ResponseItem]
baseInstructionItems instructionText
    | Text.null (Text.strip instructionText) = []
    | otherwise =
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts
                [InputTextPart instructionText Nothing]
            , role = RoleDeveloper
            , status = Nothing
            , phase = Nothing
            , passthrough = Just InternalChatMetadata
                { turnId = Nothing
                , createTime = Nothing
                , contentItemKinds = Just ["model.base_instructions"]
                , executedToolCalls = Nothing
                }
            }
        ]

responsesLiteTemplateParts
    :: Maybe ResponseInput
    -> (Text, [ResponseTool], [ResponseItem])
responsesLiteTemplateParts requestInput =
    ( instructionText
    , liteToolValuesToConventional (additionalToolsValues additional)
    , suffix
    )
  where
    items = responseInputItems requestInput
    (additional, afterAdditional) =
        case items of
            item : rest | isAdditionalToolsItem item -> (Just item, rest)
            _ -> (Nothing, items)
    (baseItems, suffix) = span isBaseInstructionsItem afterAdditional
    instructionText = fromMaybe "" (firstBaseInstructionText baseItems)

liteToolValuesToConventional :: [RawJson] -> [ResponseTool]
liteToolValuesToConventional = concatMap flatten
  where
    flatten value = case decodeTool value of
        Just (NamespaceToolValue namespace)
            | namespace.name == computerFunctionNamespace
            , any isComputerFunction namespace.tools ->
                [knownResponseTool ToolComputer]
        Just (NamespaceToolValue namespace)
            | namespace.name == "functions" ->
                namespace.tools
        decoded -> maybe [] pure decoded

    isComputerFunction = \case
        FunctionToolValue tool -> tool.name == computerFunctionName
        _ -> False

decodeTool :: RawJson -> Maybe ResponseTool
decodeTool value =
    case Hermes.decodeEither responseToolDecoder (rawJsonBytes value) of
        Right tool -> Just tool
        Left _ -> Nothing

additionalToolsValues :: Maybe ResponseItem -> [RawJson]
additionalToolsValues = \case
    Just (AdditionalToolsItemValue item) -> item.tools
    _ -> []

firstBaseInstructionText :: [ResponseItem] -> Maybe Text
firstBaseInstructionText = \case
    MessageItem message : rest ->
        messageText message <|> firstBaseInstructionText rest
    _ : rest -> firstBaseInstructionText rest
    [] -> Nothing
  where
    messageText message = case message.content of
        MessageContentText text -> Just text
        MessageContentParts parts ->
            case [text | InputTextPart{text} <- parts] of
                text : _ -> Just text
                [] -> Nothing

isAdditionalToolsItem :: ResponseItem -> Bool
isAdditionalToolsItem = \case
    AdditionalToolsItemValue{} -> True
    UnknownResponseItem TaggedObject{tag = "additional_tools"} -> True
    _ -> False

isBaseInstructionsItem :: ResponseItem -> Bool
isBaseInstructionsItem = \case
    MessageItem ResponseMessage{role = RoleDeveloper, passthrough} ->
        maybe False
            ("model.base_instructions" `elem`)
            (passthrough >>= (.contentItemKinds))
    _ -> False

responseInputItems :: Maybe ResponseInput -> [ResponseItem]
responseInputItems = \case
    Just (ResponseInputItems items) -> items
    Just (ResponseInputText text) ->
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts
                [InputTextPart text Nothing]
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            }
        ]
    Nothing -> []

setModelField :: Text -> ResponseCreateParams -> ResponseCreateParams
setModelField modelName ResponseCreateParams{..} =
    ResponseCreateParams { model = Just modelName, .. }

clearServiceTier :: ResponseCreateParams -> ResponseCreateParams
clearServiceTier ResponseCreateParams{..} =
    ResponseCreateParams { serviceTier = Nothing, .. }

setReasoningContext
    :: Maybe Text
    -> Maybe ReasoningConfig
    -> Maybe ReasoningConfig
setReasoningContext nextContext = \case
    Just ReasoningConfig{..} ->
        Just ReasoningConfig { context = nextContext, .. }
    Nothing -> Nothing

liteTextConfig :: ResponseTextConfig
liteTextConfig = ResponseTextConfig
    { format = Nothing
    , verbosity = Just "low"
    }

toResponsesLiteTextConfig
    :: Maybe ResponseTextConfig
    -> ResponseTextConfig
toResponsesLiteTextConfig = \case
    Nothing -> liteTextConfig
    Just ResponseTextConfig{..} ->
        ResponseTextConfig { verbosity = Just "low", .. }

fromResponsesLiteTextConfig
    :: Maybe ResponseTextConfig
    -> Maybe ResponseTextConfig
fromResponsesLiteTextConfig = \case
    Just ResponseTextConfig
        { format = Nothing
        , verbosity = Just "low"
        } -> Nothing
    Just ResponseTextConfig{..} ->
        Just ResponseTextConfig { verbosity = Nothing, .. }
    Nothing -> Nothing

nonBlank :: Text -> Maybe Text
nonBlank value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value
