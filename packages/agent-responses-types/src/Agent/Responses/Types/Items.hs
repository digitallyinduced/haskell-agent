-- | Input and output items carried by Responses requests and responses.
module Agent.Responses.Types.Items
    ( ResponseInput(..)
    , responseInputDecoder
    , ResponseItem(..)
    , responseItemDecoder
    , ResponseItemType(..)
    , parseResponseItemType
    , responseItemTypeText
    , ResponseMessage(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , ComputerAction(..)
    , ComputerPoint(..)
    , SafetyCheck(..)
    , ComputerCall(..)
    , computerCallDecoder
    , ComputerCallOutput(..)
    , computerCallOutputDecoder
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , reasoningSummaryPartDecoder
    , ItemReference(..)
    , ResponseAgentMessage(..)
    , InternalChatMetadata(..)
    , AdditionalToolsItem(..)
    , LocalShellCall(..)
    , LocalShellAction(..)
    , ToolSearchCall(..)
    , ToolSearchOutput(..)
    , WebSearchCall(..)
    , WebSearchAction(..)
    , ImageGenerationCall(..)
    , CompactionItem(..)
    , CompactionTriggerItem(..)
    , ContextCompactionItem(..)
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Content
import Agent.Responses.Types.Items.Known
import qualified Agent.Json.Decode as Json
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Hermes as Hermes
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text


data ResponseMessage = ResponseMessage
    { messageId   :: !(Maybe Text)
    , content     :: !MessageContent
    , role        :: !ResponseRole
    , status      :: !(Maybe ItemStatus)
    , phase       :: !(Maybe Text)
    , passthrough :: !(Maybe InternalChatMetadata)

    } deriving stock (Eq, Show)

instance ToJSON ResponseMessage where
    toJSON ResponseMessage
        { messageId, content, role, status, phase, passthrough } =
            objectWith
                [ Just (field "type" ("message" :: Text))
                , optionalField "id" messageId
                , Just (field "content" content)
                , Just (field "role" role)
                , optionalField "status" status
                , optionalField "phase" phase
                , optionalField
                    "internal_chat_message_metadata_passthrough"
                    passthrough
                ]


data FunctionCall = FunctionCall
    { itemId                 :: !(Maybe Text)
    , callId                 :: !Text
    , name                   :: !Text
    , namespace              :: !(Maybe Text)
    , provider               :: !(Maybe Text)
    , arguments              :: !Text
    , encryptedFunctionArgs  :: !(Maybe [Text])
    , status                 :: !(Maybe ItemStatus)

    } deriving stock (Eq, Show)

instance ToJSON FunctionCall where
    toJSON FunctionCall
        { itemId, callId, name, namespace, provider, arguments, encryptedFunctionArgs
        , status } =
            objectWith
                [ Just (field "type" ("function_call" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , Just (field "name" name)
                , optionalField "namespace" namespace
                , optionalField "provider" provider
                , Just (field "arguments" arguments)
                , optionalField "encrypted_function_args" encryptedFunctionArgs
                , optionalField "status" status
                ]

data ComputerPoint = ComputerPoint { pointX :: !Int, pointY :: !Int }
    deriving stock (Eq, Show)

instance ToJSON ComputerPoint where
    toJSON ComputerPoint{pointX, pointY} = object ["x" .= pointX, "y" .= pointY]

instance FromJSON ComputerPoint where
    parseJSON = withObject "ComputerPoint" \object ->
        ComputerPoint <$> object .: "x" <*> object .: "y"

data ComputerAction
    = ScreenshotAction
    | ClickAction
        { clickX :: !Int
        , clickY :: !Int
        , clickButton :: !Text
        , clickKeys :: ![Text]
        }
    | DoubleClickAction
        { doubleClickX :: !Int
        , doubleClickY :: !Int
        , doubleClickKeys :: ![Text]
        }
    | TypeAction !Text
    | KeypressAction ![Text]
    | ScrollAction
        { scrollX :: !Int
        , scrollY :: !Int
        , scrollDx :: !Int
        , scrollDy :: !Int
        , scrollKeys :: ![Text]
        }
    | MoveAction
        { moveX :: !Int
        , moveY :: !Int
        , moveKeys :: ![Text]
        }
    | WaitAction
    | DragAction
        { dragPath :: ![ComputerPoint]
        , dragKeys :: ![Text]
        }
    | UnknownComputerAction !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ComputerAction where
    toJSON = \case
        ScreenshotAction -> object ["type" .= ("screenshot" :: Text)]
        ClickAction{clickX, clickY, clickButton, clickKeys} -> object
            [ "type" .= ("click" :: Text)
            , "x" .= clickX
            , "y" .= clickY
            , "button" .= clickButton
            , "keys" .= clickKeys
            ]
        DoubleClickAction{doubleClickX, doubleClickY, doubleClickKeys} -> object
            [ "type" .= ("double_click" :: Text)
            , "x" .= doubleClickX
            , "y" .= doubleClickY
            , "keys" .= doubleClickKeys
            ]
        TypeAction text -> object ["type" .= ("type" :: Text), "text" .= text]
        KeypressAction keys -> object ["type" .= ("keypress" :: Text), "keys" .= keys]
        ScrollAction{scrollX, scrollY, scrollDx, scrollDy, scrollKeys} -> object
            [ "type" .= ("scroll" :: Text)
            , "x" .= scrollX
            , "y" .= scrollY
            , "scroll_x" .= scrollDx
            , "scroll_y" .= scrollDy
            , "keys" .= scrollKeys
            ]
        MoveAction{moveX, moveY, moveKeys} -> object
            [ "type" .= ("move" :: Text)
            , "x" .= moveX
            , "y" .= moveY
            , "keys" .= moveKeys
            ]
        WaitAction -> object ["type" .= ("wait" :: Text)]
        DragAction{dragPath, dragKeys} -> object
            [ "type" .= ("drag" :: Text)
            , "path" .= dragPath
            , "keys" .= dragKeys
            ]
        UnknownComputerAction tagged -> toJSON tagged

instance FromJSON ComputerAction where
    parseJSON value = withObject "ComputerAction" (\object -> do
        wireType <- object .: "type"
        case wireType of
            "screenshot" -> pure ScreenshotAction
            "click" ->
                ClickAction
                    <$> object .: "x"
                    <*> object .: "y"
                    <*> object .:? "button" .!= "left"
                    <*> object .:? "keys" .!= []
            "double_click" ->
                DoubleClickAction
                    <$> object .: "x"
                    <*> object .: "y"
                    <*> object .:? "keys" .!= []
            "type" -> TypeAction <$> object .: "text"
            "keypress" -> KeypressAction <$> object .: "keys"
            "scroll" ->
                ScrollAction
                    <$> object .: "x"
                    <*> object .: "y"
                    <*> object .:? "scroll_x" .!= 0
                    <*> object .:? "scroll_y" .!= 0
                    <*> object .:? "keys" .!= []
            "move" ->
                MoveAction
                    <$> object .: "x"
                    <*> object .: "y"
                    <*> object .:? "keys" .!= []
            "wait" -> pure WaitAction
            "drag" ->
                DragAction
                    <$> object .:? "path" .!= []
                    <*> object .:? "keys" .!= []
            _ -> pure (UnknownComputerAction (TaggedObject wireType))) value

data SafetyCheck = SafetyCheck
    { safetyCheckId :: !Text, safetyCheckCode :: !(Maybe Text)
    , safetyCheckMessage :: !(Maybe Text), safetyCheckExtra :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON SafetyCheck where
    toJSON SafetyCheck
        { safetyCheckId, safetyCheckCode, safetyCheckMessage, safetyCheckExtra } =
            objectWithExtra safetyCheckExtra
                [ Just (field "id" safetyCheckId)
                , optionalField "code" safetyCheckCode
                , optionalField "message" safetyCheckMessage
                ]

instance FromJSON SafetyCheck where
    parseJSON = withObject "SafetyCheck" \object ->
        SafetyCheck
            <$> object .: "id"
            <*> object .:? "code"
            <*> object .:? "message"
            <*> pure KeyMap.empty

data ComputerCall = ComputerCall
    { computerCallItemId :: !(Maybe Text), computerCallId :: !Text
    , computerActions :: ![ComputerAction], pendingSafetyChecks :: ![SafetyCheck]
    , computerCallStatus :: !(Maybe ItemStatus), computerCallExtra :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ComputerCall where
    toJSON ComputerCall{computerCallItemId, computerCallId, computerActions,
            pendingSafetyChecks, computerCallStatus, computerCallExtra} =
        objectWithExtra computerCallExtra
            [ Just (field "type" ("computer_call" :: Text))
            , optionalField "id" computerCallItemId
            , Just (field "call_id" computerCallId)
            , Just (field "actions" computerActions)
            , optionalField "pending_safety_checks" (nonEmpty pendingSafetyChecks)
            , optionalField "status" computerCallStatus
            ]
      where nonEmpty [] = Nothing; nonEmpty xs = Just xs

data ComputerCallOutput = ComputerCallOutput
    { computerOutputItemId :: !(Maybe Text), computerOutputCallId :: !Text
    , screenshotDataUrl :: !Text, acknowledgedChecks :: ![SafetyCheck]
    , computerOutputStatus :: !(Maybe ItemStatus), computerOutputExtra :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ComputerCallOutput where
    toJSON ComputerCallOutput{computerOutputItemId, computerOutputCallId,
            screenshotDataUrl, acknowledgedChecks, computerOutputStatus,
            computerOutputExtra} =
        objectWithExtra computerOutputExtra
            [ Just (field "type" ("computer_call_output" :: Text))
            , optionalField "id" computerOutputItemId
            , Just (field "call_id" computerOutputCallId)
            , Just (field "output" (object
                [ "type" .= ("computer_screenshot" :: Text)
                , "image_url" .= screenshotDataUrl
                , "detail" .= ("original" :: Text)
                ]))
            , optionalField "acknowledged_safety_checks"
                (nonEmpty acknowledgedChecks)
            , optionalField "status" computerOutputStatus
            ]
      where nonEmpty [] = Nothing; nonEmpty xs = Just xs


data FunctionCallOutput = FunctionCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , namespace   :: !(Maybe Text)
    , provider    :: !(Maybe Text)
    , output      :: !RawJson
    , status      :: !(Maybe ItemStatus)

    } deriving stock (Eq, Show)

instance ToJSON FunctionCallOutput where
    toJSON FunctionCallOutput
        { itemId, callId, name, namespace, provider, output, status } =
            objectWith
                [ Just (field "type" ("function_call_output" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , optionalField "name" name
                , optionalField "namespace" namespace
                , optionalField "provider" provider
                , Just (field "output" output)
                , optionalField "status" status
                ]


data CustomToolCall = CustomToolCall
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !Text
    , namespace   :: !(Maybe Text)
    , input       :: !Text
    , status      :: !(Maybe ItemStatus)

    } deriving stock (Eq, Show)

instance ToJSON CustomToolCall where
    toJSON CustomToolCall
        { itemId, callId, name, namespace, input, status } =
            objectWith
                [ Just (field "type" ("custom_tool_call" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , Just (field "name" name)
                , optionalField "namespace" namespace
                , Just (field "input" input)
                , optionalField "status" status
                ]


data CustomToolCallOutput = CustomToolCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , output      :: !RawJson
    , status      :: !(Maybe ItemStatus)

    } deriving stock (Eq, Show)

instance ToJSON CustomToolCallOutput where
    toJSON CustomToolCallOutput
        { itemId, callId, name, output, status } =
            objectWith
                [ Just (field "type" ("custom_tool_call_output" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , optionalField "name" name
                , Just (field "output" output)
                , optionalField "status" status
                ]


data ReasoningSummaryPart = ReasoningSummaryPart
    { partType    :: !Text
    , text        :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON ReasoningSummaryPart where
    toJSON ReasoningSummaryPart { partType, text } =
        objectWith
            [Just (field "type" partType), optionalField "text" text]


data ReasoningItem = ReasoningItem
    { itemId           :: !(Maybe Text)
    , summary          :: ![ReasoningSummaryPart]
    , content          :: !(Maybe [ResponseContentPart])
    , encryptedContent :: !(Maybe Text)
    , status           :: !(Maybe ItemStatus)

    } deriving stock (Eq)

-- Keep opaque encrypted reasoning payloads out of logs while retaining enough
-- structure to diagnose response-shape and lifecycle issues.
instance Show ReasoningItem where
    show item =
        "ReasoningItem { itemId = " <> show item.itemId
            <> ", summary = " <> show item.summary
            <> ", content = " <> show item.content
            <> ", encryptedContent = "
            <> maybe "Nothing" (const "Just <redacted>") item.encryptedContent
            <> ", status = " <> show item.status
            <> " }"

instance ToJSON ReasoningItem where
    toJSON ReasoningItem
        { itemId, summary, content, encryptedContent, status } =
            objectWith
                [ Just (field "type" ("reasoning" :: Text))
                , optionalField "id" itemId
                , Just (field "summary" summary)
                , optionalField "content" content
                , optionalField "encrypted_content" encryptedContent
                , optionalField "status" status
                ]


data ItemReference = ItemReference
    { itemId      :: !Text

    } deriving stock (Eq, Show)

instance ToJSON ItemReference where
    toJSON ItemReference { itemId } =
        objectWith
            [ Just (field "type" ("item_reference" :: Text))
            , Just (field "id" itemId)
            ]


data ResponseAgentMessage = ResponseAgentMessage
    { messageId   :: !(Maybe Text)
    , author      :: !(Maybe Text)
    , recipient   :: !(Maybe Text)
    , content     :: ![ResponseContentPart]
    , passthrough :: !(Maybe InternalChatMetadata)

    } deriving stock (Eq, Show)

instance ToJSON ResponseAgentMessage where
    toJSON ResponseAgentMessage
        { messageId, author, recipient, content, passthrough } =
            objectWith
                [ Just (field "type" ("agent_message" :: Text))
                , optionalField "id" messageId
                , optionalField "author" author
                , optionalField "recipient" recipient
                , Just (field "content" content)
                , optionalField
                    "internal_chat_message_metadata_passthrough"
                    passthrough
                ]


data AdditionalToolsItem = AdditionalToolsItem
    { itemId      :: !(Maybe Text)
    , role        :: !Text
    , tools       :: ![RawJson]

    } deriving stock (Eq, Show)

instance ToJSON AdditionalToolsItem where
    toJSON AdditionalToolsItem { itemId, role, tools } =
        objectWith
            [ Just (field "type" ("additional_tools" :: Text))
            , optionalField "id" itemId
            , Just (field "role" role)
            , Just (field "tools" tools)
            ]


data LocalShellAction
    = LocalShellExec
        { command           :: ![Text]
        , timeoutMs         :: !(Maybe Integer)
        , workingDirectory  :: !(Maybe Text)
        , env               :: !(Maybe EnvironmentVariables)
        , user              :: !(Maybe Text)

        }
    | LocalShellActionOther !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON LocalShellAction where
    toJSON LocalShellExec
        { command, timeoutMs, workingDirectory, env, user } =
            objectWith
                [ Just (field "type" ("exec" :: Text))
                , Just (field "command" command)
                , optionalField "timeout_ms" timeoutMs
                , optionalField "working_directory" workingDirectory
                , optionalField "env" env
                , optionalField "user" user
                ]
    toJSON (LocalShellActionOther tagged) = toJSON tagged


data LocalShellCall = LocalShellCall
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe ItemStatus)
    , action      :: !(Maybe LocalShellAction)

    } deriving stock (Eq, Show)

instance ToJSON LocalShellCall where
    toJSON LocalShellCall { itemId, callId, status, action } =
        objectWith
            [ Just (field "type" ("local_shell_call" :: Text))
            , optionalField "id" itemId
            , optionalField "call_id" callId
            , optionalField "status" status
            , optionalField "action" action
            ]


data ToolSearchCall = ToolSearchCall
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , execution   :: !(Maybe Text)
    , arguments   :: !(Maybe RawJson)

    } deriving stock (Eq, Show)

instance ToJSON ToolSearchCall where
    toJSON ToolSearchCall
        { itemId, callId, status, execution, arguments } =
            objectWith
                [ Just (field "type" ("tool_search_call" :: Text))
                , optionalField "id" itemId
                , optionalField "call_id" callId
                , optionalField "status" status
                , optionalField "execution" execution
                , optionalField "arguments" arguments
                ]


data ToolSearchOutput = ToolSearchOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , execution   :: !(Maybe Text)
    , tools       :: ![RawJson]

    } deriving stock (Eq, Show)

instance ToJSON ToolSearchOutput where
    toJSON ToolSearchOutput
        { itemId, callId, status, execution, tools } =
            objectWith
                [ Just (field "type" ("tool_search_output" :: Text))
                , optionalField "id" itemId
                , optionalField "call_id" callId
                , optionalField "status" status
                , optionalField "execution" execution
                , Just (field "tools" tools)
                ]


data WebSearchAction
    = WebSearchQuery
        { query       :: !(Maybe Text)
        , queries     :: !(Maybe [Text])

        }
    | WebSearchOpenPage
        { url         :: !(Maybe Text)

        }
    | WebSearchFindInPage
        { url         :: !(Maybe Text)
        , pattern     :: !(Maybe Text)

        }
    | WebSearchActionOther !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON WebSearchAction where
    toJSON WebSearchQuery { query, queries } =
        objectWith
            [ Just (field "type" ("search" :: Text))
            , optionalField "query" query
            , optionalField "queries" queries
            ]
    toJSON WebSearchOpenPage { url } =
        objectWith
            [ Just (field "type" ("open_page" :: Text))
            , optionalField "url" url
            ]
    toJSON WebSearchFindInPage { url, pattern } =
        objectWith
            [ Just (field "type" ("find_in_page" :: Text))
            , optionalField "url" url
            , optionalField "pattern" pattern
            ]
    toJSON (WebSearchActionOther tagged) = toJSON tagged


data WebSearchCall = WebSearchCall
    { itemId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , action      :: !(Maybe WebSearchAction)

    } deriving stock (Eq, Show)

instance ToJSON WebSearchCall where
    toJSON WebSearchCall { itemId, status, action } =
        objectWith
            [ Just (field "type" ("web_search_call" :: Text))
            , optionalField "id" itemId
            , optionalField "status" status
            , optionalField "action" action
            ]


data ImageGenerationCall = ImageGenerationCall
    { itemId         :: !(Maybe Text)
    , status         :: !(Maybe Text)
    , revisedPrompt  :: !(Maybe Text)
    , result         :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON ImageGenerationCall where
    toJSON ImageGenerationCall
        { itemId, status, revisedPrompt, result } =
            objectWith
                [ Just (field "type" ("image_generation_call" :: Text))
                , optionalField "id" itemId
                , optionalField "status" status
                , optionalField "revised_prompt" revisedPrompt
                , optionalField "result" result
                ]


data CompactionItem = CompactionItem
    { itemId            :: !(Maybe Text)
    , encryptedContent  :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON CompactionItem where
    toJSON CompactionItem { itemId, encryptedContent } =
        objectWith
            [ Just (field "type" ("compaction" :: Text))
            , optionalField "id" itemId
            , optionalField "encrypted_content" encryptedContent
            ]


data CompactionTriggerItem = CompactionTriggerItem
    deriving stock (Eq, Show)

instance ToJSON CompactionTriggerItem where
    toJSON CompactionTriggerItem {} =
        objectWith
            [Just (field "type" ("compaction_trigger" :: Text))]


data ContextCompactionItem = ContextCompactionItem
    { itemId           :: !(Maybe Text)
    , encryptedContent :: !(Maybe Text)

    } deriving stock (Eq, Show)

instance ToJSON ContextCompactionItem where
    toJSON ContextCompactionItem { itemId, encryptedContent } =
        objectWith
            [ Just (field "type" ("context_compaction" :: Text))
            , optionalField "id" itemId
            , optionalField "encrypted_content" encryptedContent
            ]


data ResponseItem
    = MessageItem !ResponseMessage
    | FunctionCallItem !FunctionCall
    | FunctionCallOutputItem !FunctionCallOutput
    | CustomToolCallItem !CustomToolCall
    | CustomToolCallOutputItem !CustomToolCallOutput
    | ComputerCallItem !ComputerCall
    | ComputerCallOutputItem !ComputerCallOutput
    | ReasoningItemValue !ReasoningItem
    | ItemReferenceValue !ItemReference
    | AgentMessageItem !ResponseAgentMessage
    | AdditionalToolsItemValue !AdditionalToolsItem
    | LocalShellCallItem !LocalShellCall
    | ToolSearchCallItem !ToolSearchCall
    | ToolSearchOutputItem !ToolSearchOutput
    | WebSearchCallItem !WebSearchCall
    | ImageGenerationCallItem !ImageGenerationCall
    | CompactionItemValue !CompactionItem
    | CompactionTriggerItemValue !CompactionTriggerItem
    | ContextCompactionItemValue !ContextCompactionItem
    | KnownResponseItem !ResponseItemType !TaggedObject
    | UnknownResponseItem !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseItem where
    toJSON = \case
        MessageItem value -> toJSON value
        FunctionCallItem value -> toJSON value
        FunctionCallOutputItem value -> toJSON value
        CustomToolCallItem value -> toJSON value
        CustomToolCallOutputItem value -> toJSON value
        ComputerCallItem value -> toJSON value
        ComputerCallOutputItem value -> toJSON value
        ReasoningItemValue value -> toJSON value
        ItemReferenceValue value -> toJSON value
        AgentMessageItem value -> toJSON value
        AdditionalToolsItemValue value -> toJSON value
        LocalShellCallItem value -> toJSON value
        ToolSearchCallItem value -> toJSON value
        ToolSearchOutputItem value -> toJSON value
        WebSearchCallItem value -> toJSON value
        ImageGenerationCallItem value -> toJSON value
        CompactionItemValue value -> toJSON value
        CompactionTriggerItemValue value -> toJSON value
        ContextCompactionItemValue value -> toJSON value
        KnownResponseItem _ value -> toJSON value
        UnknownResponseItem value -> toJSON value


data ResponseInput
    = ResponseInputText !Text
    | ResponseInputItems ![ResponseItem]
    deriving stock (Eq, Show)

instance ToJSON ResponseInput where
    toJSON (ResponseInputText value) = Aeson.String value
    toJSON (ResponseInputItems values) = toJSON values


responseInputDecoder :: Hermes.Decoder ResponseInput
responseInputDecoder =
    Hermes.getType >>= \case
        Hermes.VString -> ResponseInputText <$> Hermes.text
        Hermes.VArray -> ResponseInputItems <$> Hermes.list responseItemDecoder
        _ -> fail "ResponseInput: expected string or array"

responseItemDecoder :: Hermes.Decoder ResponseItem
responseItemDecoder =
    Hermes.object do
        wireType <- Hermes.atKey "type" Hermes.text
        Hermes.liftObjectDecoder $ case parseResponseItemType wireType of
            ItemMessage -> MessageItem <$> responseMessageDecoder
            ItemFunctionCall -> FunctionCallItem <$> functionCallDecoder
            ItemFunctionCallOutput ->
                FunctionCallOutputItem <$> functionCallOutputDecoder
            ItemCustomToolCall -> CustomToolCallItem <$> customToolCallDecoder
            ItemCustomToolCallOutput ->
                CustomToolCallOutputItem <$> customToolCallOutputDecoder
            ItemComputerCall -> ComputerCallItem <$> computerCallDecoder
            ItemComputerCallOutput ->
                ComputerCallOutputItem <$> computerCallOutputDecoder
            ItemReasoning -> ReasoningItemValue <$> reasoningItemDecoder
            ItemReferenceType -> ItemReferenceValue <$> itemReferenceDecoder
            ItemAgentMessage ->
                AgentMessageItem <$> responseAgentMessageDecoder
            ItemAdditionalTools ->
                AdditionalToolsItemValue <$> additionalToolsItemDecoder
            ItemLocalShellCall -> LocalShellCallItem <$> localShellCallDecoder
            ItemToolSearchCall -> ToolSearchCallItem <$> toolSearchCallDecoder
            ItemToolSearchOutput ->
                ToolSearchOutputItem <$> toolSearchOutputDecoder
            ItemWebSearchCall -> WebSearchCallItem <$> webSearchCallDecoder
            ItemImageGenerationCall ->
                ImageGenerationCallItem <$> imageGenerationCallDecoder
            ItemCompaction -> CompactionItemValue <$> compactionItemDecoder
            ItemCompactionTrigger ->
                CompactionTriggerItemValue <$> compactionTriggerItemDecoder
            ItemContextCompaction ->
                ContextCompactionItemValue <$> contextCompactionItemDecoder
            ItemUnknownType{} ->
                pure (UnknownResponseItem (TaggedObject wireType))
            itemType ->
                pure (KnownResponseItem itemType (TaggedObject wireType))

responseMessageDecoder :: Hermes.Decoder ResponseMessage
responseMessageDecoder = Hermes.object $
    ResponseMessage
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "content" messageContentDecoder
        <*> Hermes.atKey "role" responseRoleDecoder
        <*> optionalAtKey "status" itemStatusDecoder
        <*> optionalAtKey "phase" Hermes.text
        <*> optionalAtKey
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder

computerPointDecoder :: Hermes.Decoder ComputerPoint
computerPointDecoder = Hermes.object $
    ComputerPoint <$> intAtKey "x" <*> intAtKey "y"

computerActionDecoder :: Hermes.Decoder ComputerAction
computerActionDecoder = Hermes.object do
    wireType <- Hermes.atKey "type" Hermes.text
    case wireType of
        "screenshot" -> pure ScreenshotAction
        "click" -> ClickAction <$> intAtKey "x" <*> intAtKey "y"
            <*> (maybe "left" id <$> optionalAtKey "button" Hermes.text)
            <*> (maybe [] id <$> optionalAtKey "keys" (Hermes.list Hermes.text))
        "double_click" -> DoubleClickAction <$> intAtKey "x" <*> intAtKey "y"
            <*> (maybe [] id <$> optionalAtKey "keys" (Hermes.list Hermes.text))
        "type" -> TypeAction <$> Hermes.atKey "text" Hermes.text
        "keypress" -> KeypressAction <$> Hermes.atKey "keys" (Hermes.list Hermes.text)
        "scroll" -> ScrollAction <$> intAtKey "x" <*> intAtKey "y"
            <*> optionalIntAtKeyWithDefault "scroll_x" 0
            <*> optionalIntAtKeyWithDefault "scroll_y" 0
            <*> (maybe [] id <$> optionalAtKey "keys" (Hermes.list Hermes.text))
        "move" -> MoveAction <$> intAtKey "x" <*> intAtKey "y"
            <*> (maybe [] id <$> optionalAtKey "keys" (Hermes.list Hermes.text))
        "wait" -> pure WaitAction
        "drag" -> DragAction
            <$> (maybe [] id
                <$> optionalAtKey "path" (Hermes.list computerPointDecoder))
            <*> (maybe [] id <$> optionalAtKey "keys" (Hermes.list Hermes.text))
        _ -> pure (UnknownComputerAction (TaggedObject wireType))

safetyCheckDecoder :: Hermes.Decoder SafetyCheck
safetyCheckDecoder = Json.withOwnedRawJson \raw ->
    Hermes.object $
        SafetyCheck <$> Hermes.atKey "id" Hermes.text
            <*> optionalAtKey "code" Hermes.text
            <*> optionalAtKey "message" Hermes.text
            <*> pure (extraFieldsFromRaw ["id", "code", "message"] raw)

computerCallDecoder :: Hermes.Decoder ComputerCall
computerCallDecoder = Json.withOwnedRawJson \raw ->
    Hermes.object $
        ComputerCall <$> optionalAtKey "id" Hermes.text
            <*> Hermes.atKey "call_id" Hermes.text
            <*> (maybe [] id
                <$> optionalAtKey "actions" (Hermes.list computerActionDecoder))
            <*> (maybe [] id
                <$> optionalAtKey
                    "pending_safety_checks"
                    (Hermes.list safetyCheckDecoder))
            <*> optionalAtKey "status" itemStatusDecoder
            <*> pure (extraFieldsFromRaw
                [ "type", "id", "call_id", "actions"
                , "pending_safety_checks", "status"
                ]
                raw)

computerScreenshotDataUrlDecoder :: Hermes.Decoder Text
computerScreenshotDataUrlDecoder = Hermes.object $
    maybe "" id <$> optionalAtKey "image_url" Hermes.text

computerCallOutputDecoder :: Hermes.Decoder ComputerCallOutput
computerCallOutputDecoder = Json.withOwnedRawJson \raw ->
    Hermes.object $
        ComputerCallOutput <$> optionalAtKey "id" Hermes.text
            <*> Hermes.atKey "call_id" Hermes.text
            <*> (maybe "" id
                <$> optionalAtKey "output" computerScreenshotDataUrlDecoder)
            <*> (maybe [] id
                <$> optionalAtKey
                    "acknowledged_safety_checks"
                    (Hermes.list safetyCheckDecoder))
            <*> optionalAtKey "status" itemStatusDecoder
            <*> pure (extraFieldsFromRaw
                [ "type", "id", "call_id", "output"
                , "acknowledged_safety_checks", "status"
                ]
                raw)

objectWithExtra :: Aeson.Object -> [Maybe Field] -> Aeson.Value
objectWithExtra extra members =
    case objectWith members of
        Aeson.Object known -> Aeson.Object (KeyMap.union known extra)
        value -> value

extraFieldsFromRaw reserved raw =
    case Aeson.decodeStrict' raw of
        Just (Aeson.Object object) ->
            foldr (KeyMap.delete . Key.fromText) object reserved
        _ -> KeyMap.empty

intAtKey :: Text -> Hermes.FieldsDecoder Int
intAtKey key =
    Hermes.atKey key integerDecoder >>= boundedComputerInt key

optionalIntAtKeyWithDefault :: Text -> Int -> Hermes.FieldsDecoder Int
optionalIntAtKeyWithDefault key fallback =
    optionalAtKey key integerDecoder >>= \case
        Nothing -> pure fallback
        Just value -> boundedComputerInt key value

boundedComputerInt :: MonadFail parser => Text -> Integer -> parser Int
boundedComputerInt key value
    | value < toInteger (minBound :: Int)
        || value > toInteger (maxBound :: Int) =
            fail $
                "integer at key "
                    <> Text.unpack key
                    <> " is outside the platform Int range"
    | otherwise = pure (fromInteger value)

functionCallDecoder :: Hermes.Decoder FunctionCall
functionCallDecoder = Hermes.object $
    FunctionCall
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> Hermes.atKey "name" Hermes.text
        <*> optionalAtKey "namespace" Hermes.text
        <*> optionalAtKey "provider" Hermes.text
        <*> Hermes.atKey "arguments" Hermes.text
        <*> optionalAtKey "encrypted_function_args" (Hermes.list Hermes.text)
        <*> optionalAtKey "status" itemStatusDecoder

functionCallOutputDecoder :: Hermes.Decoder FunctionCallOutput
functionCallOutputDecoder = Hermes.object $
    FunctionCallOutput
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> optionalAtKey "name" Hermes.text
        <*> optionalAtKey "namespace" Hermes.text
        <*> optionalAtKey "provider" Hermes.text
        <*> Hermes.atKey "output" rawJsonDecoder
        <*> optionalAtKey "status" itemStatusDecoder

customToolCallDecoder :: Hermes.Decoder CustomToolCall
customToolCallDecoder = Hermes.object $
    CustomToolCall
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> Hermes.atKey "name" Hermes.text
        <*> optionalAtKey "namespace" Hermes.text
        <*> Hermes.atKey "input" Hermes.text
        <*> optionalAtKey "status" itemStatusDecoder

customToolCallOutputDecoder :: Hermes.Decoder CustomToolCallOutput
customToolCallOutputDecoder = Hermes.object $
    CustomToolCallOutput
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> optionalAtKey "name" Hermes.text
        <*> Hermes.atKey "output" rawJsonDecoder
        <*> optionalAtKey "status" itemStatusDecoder

reasoningSummaryPartDecoder :: Hermes.Decoder ReasoningSummaryPart
reasoningSummaryPartDecoder = Hermes.object $
    ReasoningSummaryPart
        <$> Hermes.atKey "type" Hermes.text
        <*> optionalAtKey "text" Hermes.text

reasoningItemDecoder :: Hermes.Decoder ReasoningItem
reasoningItemDecoder = Hermes.object $
    ReasoningItem
        <$> optionalAtKey "id" Hermes.text
        <*> (maybe [] id
            <$> optionalAtKey
                "summary"
                (Hermes.list reasoningSummaryPartDecoder))
        <*> optionalAtKey
            "content"
            (Hermes.list responseContentPartDecoder)
        <*> optionalAtKey "encrypted_content" Hermes.text
        <*> optionalAtKey "status" itemStatusDecoder

itemReferenceDecoder :: Hermes.Decoder ItemReference
itemReferenceDecoder = Hermes.object $
    ItemReference
        <$> Hermes.atKey "id" Hermes.text

responseAgentMessageDecoder :: Hermes.Decoder ResponseAgentMessage
responseAgentMessageDecoder = Hermes.object $
    ResponseAgentMessage
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "author" Hermes.text
        <*> optionalAtKey "recipient" Hermes.text
        <*> (maybe [] id <$> optionalAtKey
            "content"
            (Hermes.list responseContentPartDecoder))
        <*> optionalAtKey
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder

additionalToolsItemDecoder :: Hermes.Decoder AdditionalToolsItem
additionalToolsItemDecoder = Hermes.object $
    AdditionalToolsItem
        <$> optionalAtKey "id" Hermes.text
        <*> (maybe "developer" id <$> optionalAtKey "role" Hermes.text)
        <*> (maybe [] id <$> optionalAtKey
            "tools"
            (Hermes.list rawJsonDecoder))

localShellActionDecoder :: Hermes.Decoder LocalShellAction
localShellActionDecoder =
    Hermes.object do
        wireType <- optionalAtKey "type" Hermes.text
        case wireType of
            Just "exec" -> LocalShellExec
                <$> (maybe [] id <$> optionalAtKey
                    "command"
                    (Hermes.list Hermes.text))
                <*> optionalAtKey "timeout_ms" integerDecoder
                <*> optionalAtKey "working_directory" Hermes.text
                <*> optionalAtKey "env" environmentVariablesDecoder
                <*> optionalAtKey "user" Hermes.text
            _ -> pure $
                LocalShellActionOther
                    (TaggedObject (maybe "" id wireType))

localShellCallDecoder :: Hermes.Decoder LocalShellCall
localShellCallDecoder = Hermes.object $
    LocalShellCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "call_id" Hermes.text
        <*> optionalAtKey "status" itemStatusDecoder
        <*> optionalAtKey "action" localShellActionDecoder

toolSearchCallDecoder :: Hermes.Decoder ToolSearchCall
toolSearchCallDecoder = Hermes.object $
    ToolSearchCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "call_id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "execution" Hermes.text
        <*> optionalAtKey "arguments" rawJsonDecoder

toolSearchOutputDecoder :: Hermes.Decoder ToolSearchOutput
toolSearchOutputDecoder = Hermes.object $
    ToolSearchOutput
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "call_id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "execution" Hermes.text
        <*> (maybe [] id <$> optionalAtKey
            "tools"
            (Hermes.list rawJsonDecoder))

webSearchActionDecoder :: Hermes.Decoder WebSearchAction
webSearchActionDecoder =
    Hermes.object do
        wireType <- optionalAtKey "type" Hermes.text
        Hermes.liftObjectDecoder $ Hermes.object $ case wireType of
            Just "search" -> WebSearchQuery
                <$> optionalAtKey "query" Hermes.text
                <*> optionalAtKey "queries" (Hermes.list Hermes.text)
            Just "open_page" -> WebSearchOpenPage
                <$> optionalAtKey "url" Hermes.text
            Just "find_in_page" -> WebSearchFindInPage
                <$> optionalAtKey "url" Hermes.text
                <*> optionalAtKey "pattern" Hermes.text
            _ -> pure $
                WebSearchActionOther
                    (TaggedObject (maybe "" id wireType))

webSearchCallDecoder :: Hermes.Decoder WebSearchCall
webSearchCallDecoder = Hermes.object $
    WebSearchCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "action" webSearchActionDecoder

imageGenerationCallDecoder :: Hermes.Decoder ImageGenerationCall
imageGenerationCallDecoder = Hermes.object $
    ImageGenerationCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "revised_prompt" Hermes.text
        <*> optionalAtKey "result" Hermes.text

compactionItemDecoder :: Hermes.Decoder CompactionItem
compactionItemDecoder = Hermes.object $
    CompactionItem
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "encrypted_content" Hermes.text

compactionTriggerItemDecoder :: Hermes.Decoder CompactionTriggerItem
compactionTriggerItemDecoder =
    CompactionTriggerItem <$ Hermes.object (pure ())

contextCompactionItemDecoder :: Hermes.Decoder ContextCompactionItem
contextCompactionItemDecoder = Hermes.object $
    ContextCompactionItem
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "encrypted_content" Hermes.text

integerDecoder :: Hermes.Decoder Integer
integerDecoder = do
    value <- Hermes.scientific
    case floatingOrInteger value of
        Right integer -> pure integer
        Left (_ :: Double) -> fail "expected an integer"
