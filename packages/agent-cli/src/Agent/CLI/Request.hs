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
import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , extensionsToList
    , lookupExtension
    , rawJsonBytes
    )
import qualified Agent.Json.Decoder as JsonDecoder
import qualified Agent.Json.Encoder as JsonEncoder
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, mapMaybe)
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
                    , extraFields = emptyExtensions
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
                        (responsesLitePrefix instructionText toolSchemas <> suffix))
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
-- namespace; hosted tools and non-default namespaces remain top-level.
responsesLiteToolValues :: [ResponseTool] -> [RawJson]
responsesLiteToolValues tools =
    case groupedValues of
        [] -> ungroupedValues
        _ ->
            let namespaceValue = encodeRaw liteNamespaceEncoder LiteNamespace
                    { namespaceType = "namespace"
                    , namespaceName = "functions"
                    , namespaceDescription
                    , namespaceTools = groupedValues
                    }
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
                , ungrouped <> [encodeRaw responseToolEncoder tool]
                )

groupedToolValues :: ResponseTool -> Maybe ([RawJson], Maybe Text)
groupedToolValues tool = case tool of
    FunctionToolValue{} -> Just ([encoded], Nothing)
    KnownResponseTool ToolCustom _ -> Just ([encoded], Nothing)
    UnknownResponseTool tagged
        | tagged.tag == "custom" -> Just ([encoded], Nothing)
    KnownResponseTool ToolNamespace tagged
        | textField "name" tagged.fields == Just "functions" ->
            Just
                ( arrayField "tools" tagged.fields
                , nonBlank =<< textField "description" tagged.fields
                )
    _ -> Nothing
  where
    encoded = encodeRaw responseToolEncoder tool

additionalToolsItem :: [RawJson] -> ResponseItem
additionalToolsItem tools =
    AdditionalToolsItemValue AdditionalToolsItem
        { itemId = Nothing
        , role = "developer"
        , tools
        , extraFields = emptyExtensions
        }

baseInstructionItems :: Text -> [ResponseItem]
baseInstructionItems instructionText
    | Text.null (Text.strip instructionText) = []
    | otherwise =
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts
                [InputTextPart instructionText Nothing emptyExtensions]
            , role = RoleDeveloper
            , status = Nothing
            , phase = Nothing
            , passthrough = Just InternalChatMetadata
                { turnId = Nothing
                , createTime = Nothing
                , contentItemKinds = Just ["model.base_instructions"]
                , executedToolCalls = Nothing
                , extraFields = emptyExtensions
                }
            , extraFields = emptyExtensions
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
    flatten value
        | Just namespace <- decodeRaw liteNamespaceDecoder value
        , namespace.namespaceType == "namespace"
        , namespace.namespaceName == "functions" =
            mapMaybe decodeTool namespace.namespaceTools
        | otherwise = maybeToListDecoded value

    maybeToListDecoded value = maybe [] pure (decodeTool value)

decodeTool :: RawJson -> Maybe ResponseTool
decodeTool = decodeRaw responseToolDecoder

additionalToolsValues :: Maybe ResponseItem -> [RawJson]
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
                [InputTextPart text Nothing emptyExtensions]
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            , extraFields = emptyExtensions
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
    , extraFields = emptyExtensions
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
        | null (extensionsToList extraFields) -> Nothing
    Just ResponseTextConfig{..} ->
        Just ResponseTextConfig { verbosity = Nothing, .. }
    Nothing -> Nothing

textField :: Text -> Extensions -> Maybe Text
textField name fields =
    lookupExtension name fields >>= decodeRaw JsonDecoder.text

arrayField :: Text -> Extensions -> [RawJson]
arrayField name fields =
    fromMaybe [] $
        lookupExtension name fields
            >>= decodeRaw (JsonDecoder.list JsonDecoder.rawJson)

data LiteNamespace = LiteNamespace
    { namespaceType :: !Text
    , namespaceName :: !Text
    , namespaceDescription :: !Text
    , namespaceTools :: ![RawJson]
    }

nonBlank :: Text -> Maybe Text
nonBlank value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value

liteNamespaceEncoder :: JsonEncoder.Encoder LiteNamespace
liteNamespaceEncoder = JsonEncoder.object
    [ JsonEncoder.field "type" JsonEncoder.text (const "namespace")
    , JsonEncoder.field "name" JsonEncoder.text (const "functions")
    , JsonEncoder.field "description" JsonEncoder.text (.namespaceDescription)
    , JsonEncoder.field
        "tools"
        (JsonEncoder.list JsonEncoder.rawJson)
        (.namespaceTools)
    ]

liteNamespaceDecoder :: JsonDecoder.Decoder LiteNamespace
liteNamespaceDecoder =
    JsonDecoder.object
        LiteNamespace
            { namespaceType = ""
            , namespaceName = ""
            , namespaceDescription = ""
            , namespaceTools = []
            }
        [ JsonDecoder.field "type" JsonDecoder.text \value state ->
            Right state { namespaceType = value }
        , JsonDecoder.field "name" JsonDecoder.text \value state ->
            Right state { namespaceName = value }
        , JsonDecoder.field "description" JsonDecoder.text \value state ->
            Right state { namespaceDescription = value }
        , JsonDecoder.field
            "tools"
            (JsonDecoder.list JsonDecoder.rawJson)
            \value state -> Right state { namespaceTools = value }
        ]
        (JsonDecoder.unknownField JsonDecoder.skip
            \_ _ state -> Right state)
        Right

encodeRaw :: JsonEncoder.Encoder value -> value -> RawJson
encodeRaw encoder =
    either (error . show) id
        . JsonDecoder.validateRawJson
        . JsonEncoder.encode encoder

decodeRaw :: JsonDecoder.Decoder value -> RawJson -> Maybe value
decodeRaw decoder =
    either (const Nothing) Just
        . JsonDecoder.decode decoder
        . rawJsonBytes
