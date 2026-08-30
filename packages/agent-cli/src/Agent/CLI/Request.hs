-- | Construction of provider-neutral Responses requests.
module Agent.CLI.Request
    ( requestParams
    , setRequestInstructions
    , setRequestInstructionsAndTools
    , setRequestModel
    ) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.Responses.Types
import Agent.Provider (Provider(..))
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

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
                                (responsesLitePrefix
                                    instructionText
                                    toolSchemas))
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
                    , extraFields = KeyMap.empty
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
        setModelField modelName params
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
                        (responsesLitePrefix
                            instructionText
                            toolSchemas
                            <> suffix))
                    , parallelToolCalls = Just False
                    , reasoning = setReasoningContext
                        (Just "all_turns")
                        reasoning
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
-- namespace. The ChatGPT/Codex Responses Lite transport does not accept the
-- native @computer@ tool, so expose its local harness in a reserved namespace
-- whose function output can carry a screenshot back to the model.
responsesLiteToolValues :: [ResponseTool] -> [Aeson.Value]
responsesLiteToolValues tools =
    case groupedValues of
        [] -> ungroupedValues
        _ ->
            let namespaceValue = Aeson.object
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

responsesLiteToolValue :: ResponseTool -> Aeson.Value
responsesLiteToolValue = \case
    KnownResponseTool ToolComputer _ -> computerFunctionNamespaceValue
    tool -> Aeson.toJSON tool

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
                [ "anyOf" Aeson..= computerActionSchemas ]
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

groupedToolValues :: ResponseTool -> Maybe ([Aeson.Value], Maybe Text)
groupedToolValues tool = case tool of
    FunctionToolValue{} -> Just ([Aeson.toJSON tool], Nothing)
    KnownResponseTool ToolCustom _ -> Just ([Aeson.toJSON tool], Nothing)
    UnknownResponseTool tagged
        | tagged.tag == "custom" -> Just ([Aeson.toJSON tool], Nothing)
    KnownResponseTool ToolNamespace tagged
        | textField "name" tagged.fields == Just "functions" ->
            Just
                ( arrayField "tools" tagged.fields
                , nonBlank =<< textField "description" tagged.fields
                )
    _ -> Nothing

additionalToolsItem :: [Aeson.Value] -> ResponseItem
additionalToolsItem tools =
    AdditionalToolsItemValue AdditionalToolsItem
        { itemId = Nothing
        , role = "developer"
        , tools
        , extraFields = KeyMap.empty
        }

baseInstructionItems :: Text -> [ResponseItem]
baseInstructionItems instructionText
    | Text.null (Text.strip instructionText) = []
    | otherwise =
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts
                [InputTextPart instructionText Nothing KeyMap.empty]
            , role = RoleDeveloper
            , status = Nothing
            , phase = Nothing
            , passthrough = Just InternalChatMetadata
                { turnId = Nothing
                , createTime = Nothing
                , contentItemKinds = Just ["model.base_instructions"]
                , executedToolCalls = Nothing
                , extraFields = KeyMap.empty
                }
            , extraFields = KeyMap.empty
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

liteToolValuesToConventional :: [Aeson.Value] -> [ResponseTool]
liteToolValuesToConventional = concatMap flatten
  where
    flatten value@(Aeson.Object object)
        | textField "type" object == Just "namespace"
        , textField "name" object == Just computerFunctionNamespace
        , any isComputerFunction (arrayField "tools" object) =
            [knownResponseTool ToolComputer KeyMap.empty]
        | textField "type" object == Just "namespace"
        , textField "name" object == Just "functions" =
            mapMaybe decodeTool (arrayField "tools" object)
        | otherwise = maybeToListDecoded value
    flatten value = maybeToListDecoded value

    maybeToListDecoded value = maybe [] pure (decodeTool value)

    isComputerFunction = \case
        Aeson.Object object ->
            textField "type" object == Just "function"
                && textField "name" object == Just computerFunctionName
        _ -> False

decodeTool :: Aeson.Value -> Maybe ResponseTool
decodeTool value = case Aeson.fromJSON value of
    Aeson.Success tool -> Just tool
    Aeson.Error _ -> Nothing

additionalToolsValues :: Maybe ResponseItem -> [Aeson.Value]
additionalToolsValues = \case
    Just (AdditionalToolsItemValue item) -> item.tools
    Just (UnknownResponseItem TaggedObject{tag = "additional_tools", fields}) ->
        arrayField "tools" fields
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
                [InputTextPart text Nothing KeyMap.empty]
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            , extraFields = KeyMap.empty
            }
        ]
    Nothing -> []

setModelField :: Text -> ResponseCreateParams -> ResponseCreateParams
setModelField modelName ResponseCreateParams{..} =
    ResponseCreateParams { model = Just modelName, .. }

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
    , extraFields = KeyMap.empty
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
        , extraFields
        }
        | KeyMap.null extraFields -> Nothing
    Just ResponseTextConfig{..} ->
        Just ResponseTextConfig { verbosity = Nothing, .. }
    Nothing -> Nothing

textField :: Text -> Aeson.Object -> Maybe Text
textField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.String value) -> Just value
        _ -> Nothing

arrayField :: Text -> Aeson.Object -> [Aeson.Value]
arrayField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.Array values) -> Vector.toList values
        _ -> []

arrayTextField :: Text -> Aeson.Object -> [Text]
arrayTextField name object =
    [value | Aeson.String value <- arrayField name object]

nonBlank :: Text -> Maybe Text
nonBlank value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value
