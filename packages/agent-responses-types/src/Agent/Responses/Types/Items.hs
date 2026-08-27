-- | Input and output items carried by Responses requests and responses.
module Agent.Responses.Types.Items
    ( ResponseInput(..)
    , ResponseItem(..)
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
    , ComputerCallOutput(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
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
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import Data.Text (Text)


data ResponseMessage = ResponseMessage
    { messageId   :: !(Maybe Text)
    , content     :: !MessageContent
    , role        :: !ResponseRole
    , status      :: !(Maybe ItemStatus)
    , phase       :: !(Maybe Text)
    , passthrough :: !(Maybe InternalChatMetadata)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseMessage where
    toJSON ResponseMessage
        { messageId, content, role, status, phase, passthrough, extraFields } =
            objectWith extraFields
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

instance FromJSON ResponseMessage where
    parseJSON = withObject "ResponseMessage" $ \o -> ResponseMessage
        <$> o .:? "id"
        <*> o .: "content"
        <*> o .: "role"
        <*> o .:? "status"
        <*> o .:? "phase"
        <*> o .:? "internal_chat_message_metadata_passthrough"
        <*> pure
            (without
                [ "type", "id", "content", "role", "status", "phase"
                , "internal_chat_message_metadata_passthrough"
                ]
                o)

data FunctionCall = FunctionCall
    { itemId                 :: !(Maybe Text)
    , callId                 :: !Text
    , name                   :: !Text
    , namespace              :: !(Maybe Text)
    , arguments              :: !Text
    , encryptedFunctionArgs  :: !(Maybe [Text])
    , status                 :: !(Maybe ItemStatus)
    , extraFields            :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCall where
    toJSON FunctionCall
        { itemId, callId, name, namespace, arguments, encryptedFunctionArgs
        , status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("function_call" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , Just (field "name" name)
                , optionalField "namespace" namespace
                , Just (field "arguments" arguments)
                , optionalField "encrypted_function_args" encryptedFunctionArgs
                , optionalField "status" status
                ]

instance FromJSON FunctionCall where
    parseJSON = withObject "FunctionCall" $ \o -> FunctionCall
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .: "name"
        <*> o .:? "namespace"
        <*> o .: "arguments"
        <*> o .:? "encrypted_function_args"
        <*> o .:? "status"
        <*> pure
            (without
                [ "type", "id", "call_id", "name", "namespace", "arguments"
                , "encrypted_function_args", "status"
                ]
                o)

data FunctionCallOutput = FunctionCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , namespace   :: !(Maybe Text)
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCallOutput where
    toJSON FunctionCallOutput
        { itemId, callId, name, namespace, output, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("function_call_output" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , optionalField "name" name
                , optionalField "namespace" namespace
                , Just (field "output" output)
                , optionalField "status" status
                ]

instance FromJSON FunctionCallOutput where
    parseJSON = withObject "FunctionCallOutput" $ \o -> FunctionCallOutput
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .:? "name"
        <*> o .:? "namespace"
        <*> o .: "output"
        <*> o .:? "status"
        <*> pure
            (without
                [ "type", "id", "call_id", "name", "namespace", "output"
                , "status"
                ]
                o)

data CustomToolCall = CustomToolCall
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !Text
    , namespace   :: !(Maybe Text)
    , input       :: !Text
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CustomToolCall where
    toJSON CustomToolCall
        { itemId, callId, name, namespace, input, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("custom_tool_call" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , Just (field "name" name)
                , optionalField "namespace" namespace
                , Just (field "input" input)
                , optionalField "status" status
                ]

instance FromJSON CustomToolCall where
    parseJSON = withObject "CustomToolCall" $ \o -> CustomToolCall
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .: "name"
        <*> o .:? "namespace"
        <*> o .: "input"
        <*> o .:? "status"
        <*> pure
            (without
                [ "type", "id", "call_id", "name", "namespace", "input"
                , "status"
                ]
                o)

data CustomToolCallOutput = CustomToolCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CustomToolCallOutput where
    toJSON CustomToolCallOutput
        { itemId, callId, name, output, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("custom_tool_call_output" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , optionalField "name" name
                , Just (field "output" output)
                , optionalField "status" status
                ]

instance FromJSON CustomToolCallOutput where
    parseJSON = withObject "CustomToolCallOutput" $ \o ->
        CustomToolCallOutput
            <$> o .:? "id"
            <*> o .: "call_id"
            <*> o .:? "name"
            <*> o .: "output"
            <*> o .:? "status"
            <*> pure
                (without
                    ["type", "id", "call_id", "name", "output", "status"] o)

-- | A point in the screen coordinate space used by the Responses computer
-- tool. Coordinates are intentionally integers: providers reject fractional
-- input and native executors can pass these values without rounding.
data ComputerPoint = ComputerPoint
    { pointX :: !Int
    , pointY :: !Int
    } deriving stock (Eq, Show)

instance ToJSON ComputerPoint where
    toJSON ComputerPoint { pointX, pointY } =
        object ["x" .= pointX, "y" .= pointY]

instance FromJSON ComputerPoint where
    parseJSON = withObject "ComputerPoint" $ \o ->
        ComputerPoint <$> o .: "x" <*> o .: "y"

-- | Actions emitted by a provider-native computer-use tool.
--
-- The unknown constructor is deliberately lossless. Providers occasionally
-- add actions before the API client is updated; retaining those items allows
-- a session to be resumed instead of making its transcript undecodable.
data ComputerAction
    = ScreenshotAction
    | ClickAction
        { clickX      :: !Int
        , clickY      :: !Int
        , clickButton :: !Text
        }
    | DoubleClickAction
        { doubleClickX :: !Int
        , doubleClickY :: !Int
        }
    | TypeAction !Text
    | KeypressAction ![Text]
    | ScrollAction
        { scrollX  :: !Int
        , scrollY  :: !Int
        , scrollDx :: !Int
        , scrollDy :: !Int
        }
    | MoveAction
        { moveX :: !Int
        , moveY :: !Int
        }
    | WaitAction !Int
    | DragAction ![ComputerPoint]
    | UnknownComputerAction !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ComputerAction where
    toJSON = \case
        ScreenshotAction -> object ["type" .= ("screenshot" :: Text)]
        ClickAction { clickX, clickY, clickButton } ->
            object
                [ "type" .= ("click" :: Text)
                , "x" .= clickX
                , "y" .= clickY
                , "button" .= clickButton
                ]
        DoubleClickAction { doubleClickX, doubleClickY } ->
            object
                [ "type" .= ("double_click" :: Text)
                , "x" .= doubleClickX
                , "y" .= doubleClickY
                ]
        TypeAction value -> object
            [ "type" .= ("type" :: Text), "text" .= value ]
        KeypressAction keys -> object
            [ "type" .= ("keypress" :: Text), "keys" .= keys ]
        ScrollAction { scrollX, scrollY, scrollDx, scrollDy } ->
            object
                [ "type" .= ("scroll" :: Text)
                , "x" .= scrollX
                , "y" .= scrollY
                , "scroll_x" .= scrollDx
                , "scroll_y" .= scrollDy
                ]
        MoveAction { moveX, moveY } ->
            object
                [ "type" .= ("move" :: Text)
                , "x" .= moveX
                , "y" .= moveY
                ]
        WaitAction milliseconds -> object
            [ "type" .= ("wait" :: Text), "ms" .= milliseconds ]
        DragAction path -> object
            [ "type" .= ("drag" :: Text), "path" .= path ]
        UnknownComputerAction value -> toJSON value

instance FromJSON ComputerAction where
    parseJSON value = withObject "ComputerAction" (\o -> do
        tag <- o .: "type"
        case tag of
            "screenshot" -> pure ScreenshotAction
            "click" -> ClickAction
                <$> o .: "x" <*> o .: "y" <*> o .:? "button" .!= "left"
            "double_click" -> DoubleClickAction <$> o .: "x" <*> o .: "y"
            "type" -> TypeAction <$> o .: "text"
            "keypress" -> KeypressAction <$> o .: "keys"
            "scroll" -> ScrollAction
                <$> o .: "x" <*> o .: "y"
                <*> o .:? "scroll_x" .!= 0
                <*> o .:? "scroll_y" .!= 0
            "move" -> MoveAction <$> o .: "x" <*> o .: "y"
            "wait" -> WaitAction <$> o .:? "ms" .!= 1000
            "drag" -> DragAction <$> o .:? "path" .!= []
            _ -> pure (UnknownComputerAction TaggedObject
                { tag
                , fields = without ["type"] o
                })) value

data SafetyCheck = SafetyCheck
    { safetyCheckId      :: !Text
    , safetyCheckCode    :: !(Maybe Text)
    , safetyCheckMessage :: !(Maybe Text)
    , safetyCheckExtra   :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON SafetyCheck where
    toJSON SafetyCheck
        { safetyCheckId, safetyCheckCode, safetyCheckMessage, safetyCheckExtra } =
            objectWith safetyCheckExtra
                [ Just (field "id" safetyCheckId)
                , optionalField "code" safetyCheckCode
                , optionalField "message" safetyCheckMessage
                ]

instance FromJSON SafetyCheck where
    parseJSON = withObject "SafetyCheck" $ \o -> SafetyCheck
        <$> o .: "id"
        <*> o .:? "code"
        <*> o .:? "message"
        <*> pure (without ["id", "code", "message"] o)

data ComputerCall = ComputerCall
    { computerCallItemId    :: !(Maybe Text)
    , computerCallId        :: !Text
    , computerActions       :: ![ComputerAction]
    , pendingSafetyChecks   :: ![SafetyCheck]
    , computerCallStatus    :: !(Maybe ItemStatus)
    , computerCallExtra     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ComputerCall where
    toJSON ComputerCall
        { computerCallItemId, computerCallId, computerActions
        , pendingSafetyChecks, computerCallStatus, computerCallExtra } =
            objectWith computerCallExtra
                [ Just (field "type" ("computer_call" :: Text))
                , optionalField "id" computerCallItemId
                , Just (field "call_id" computerCallId)
                , Just (field "actions" computerActions)
                , optionalField "pending_safety_checks"
                    (nonEmpty pendingSafetyChecks)
                , optionalField "status" computerCallStatus
                ]
      where
        nonEmpty [] = Nothing
        nonEmpty values = Just values

instance FromJSON ComputerCall where
    parseJSON = withObject "ComputerCall" $ \o -> ComputerCall
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .:? "actions" .!= []
        <*> o .:? "pending_safety_checks" .!= []
        <*> o .:? "status"
        <*> pure (without
            [ "type", "id", "call_id", "actions", "pending_safety_checks"
            , "status"
            ] o)

data ComputerCallOutput = ComputerCallOutput
    { computerOutputItemId   :: !(Maybe Text)
    , computerOutputCallId   :: !Text
    , screenshotDataUrl      :: !Text
    , acknowledgedChecks     :: ![SafetyCheck]
    , computerOutputStatus   :: !(Maybe ItemStatus)
    , computerOutputExtra    :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ComputerCallOutput where
    toJSON ComputerCallOutput
        { computerOutputItemId, computerOutputCallId, screenshotDataUrl
        , acknowledgedChecks, computerOutputStatus, computerOutputExtra } =
            objectWith computerOutputExtra
                [ Just (field "type" ("computer_call_output" :: Text))
                , optionalField "id" computerOutputItemId
                , Just (field "call_id" computerOutputCallId)
                , Just (field "output" (object
                    [ "type" .= ("computer_screenshot" :: Text)
                    , "image_url" .= screenshotDataUrl
                    ]))
                , optionalField "acknowledged_safety_checks"
                    (nonEmpty acknowledgedChecks)
                , optionalField "status" computerOutputStatus
                ]
      where
        nonEmpty [] = Nothing
        nonEmpty values = Just values

instance FromJSON ComputerCallOutput where
    parseJSON = withObject "ComputerCallOutput" $ \o -> do
        outputValue <- o .:? "output" .!= Aeson.Null
        image <- case outputValue of
            Aeson.Object outputObject ->
                outputObject .:? "image_url" .!= ""
            _ -> pure ""
        ComputerCallOutput
            <$> o .:? "id"
            <*> o .: "call_id"
            <*> pure image
            <*> o .:? "acknowledged_safety_checks" .!= []
            <*> o .:? "status"
            <*> pure (without
                [ "type", "id", "call_id", "output"
                , "acknowledged_safety_checks", "status"
                ] o)

data ReasoningSummaryPart = ReasoningSummaryPart
    { partType    :: !Text
    , text        :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ReasoningSummaryPart where
    toJSON ReasoningSummaryPart { partType, text, extraFields } =
        objectWith extraFields
            [Just (field "type" partType), optionalField "text" text]

instance FromJSON ReasoningSummaryPart where
    parseJSON = withObject "ReasoningSummaryPart" $ \o ->
        ReasoningSummaryPart
            <$> o .: "type"
            <*> o .:? "text"
            <*> pure (without ["type", "text"] o)

data ReasoningItem = ReasoningItem
    { itemId           :: !(Maybe Text)
    , summary          :: ![ReasoningSummaryPart]
    , content          :: !(Maybe [ResponseContentPart])
    , encryptedContent :: !(Maybe Text)
    , status           :: !(Maybe ItemStatus)
    , extraFields      :: !Aeson.Object
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
            <> ", extraFields = " <> show item.extraFields
            <> " }"

instance ToJSON ReasoningItem where
    toJSON ReasoningItem
        { itemId, summary, content, encryptedContent, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("reasoning" :: Text))
                , optionalField "id" itemId
                , Just (field "summary" summary)
                , optionalField "content" content
                , optionalField "encrypted_content" encryptedContent
                , optionalField "status" status
                ]

instance FromJSON ReasoningItem where
    parseJSON = withObject "ReasoningItem" $ \o -> ReasoningItem
        <$> o .:? "id"
        <*> o .:? "summary" .!= []
        <*> o .:? "content"
        <*> o .:? "encrypted_content"
        <*> o .:? "status"
        <*> pure
            (without
                [ "type", "id", "summary", "content", "encrypted_content"
                , "status"
                ]
                o)

data ItemReference = ItemReference
    { itemId      :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ItemReference where
    toJSON ItemReference { itemId, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("item_reference" :: Text))
            , Just (field "id" itemId)
            ]

instance FromJSON ItemReference where
    parseJSON = withObject "ItemReference" $ \o -> ItemReference
        <$> o .: "id"
        <*> pure (without ["type", "id"] o)

data ResponseAgentMessage = ResponseAgentMessage
    { messageId   :: !(Maybe Text)
    , author      :: !(Maybe Text)
    , recipient   :: !(Maybe Text)
    , content     :: ![ResponseContentPart]
    , passthrough :: !(Maybe InternalChatMetadata)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseAgentMessage where
    toJSON ResponseAgentMessage
        { messageId, author, recipient, content, passthrough, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("agent_message" :: Text))
                , optionalField "id" messageId
                , optionalField "author" author
                , optionalField "recipient" recipient
                , Just (field "content" content)
                , optionalField
                    "internal_chat_message_metadata_passthrough"
                    passthrough
                ]

instance FromJSON ResponseAgentMessage where
    parseJSON = withObject "ResponseAgentMessage" $ \o ->
        ResponseAgentMessage
            <$> o .:? "id"
            <*> o .:? "author"
            <*> o .:? "recipient"
            <*> o .:? "content" .!= []
            <*> o .:? "internal_chat_message_metadata_passthrough"
            <*> pure
                (without
                    [ "type", "id", "author", "recipient", "content"
                    , "internal_chat_message_metadata_passthrough"
                    ]
                    o)

data AdditionalToolsItem = AdditionalToolsItem
    { itemId      :: !(Maybe Text)
    , role        :: !Text
    , tools       :: ![Aeson.Value]
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON AdditionalToolsItem where
    toJSON AdditionalToolsItem { itemId, role, tools, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("additional_tools" :: Text))
            , optionalField "id" itemId
            , Just (field "role" role)
            , Just (field "tools" tools)
            ]

instance FromJSON AdditionalToolsItem where
    parseJSON = withObject "AdditionalToolsItem" $ \o ->
        AdditionalToolsItem
            <$> o .:? "id"
            <*> o .:? "role" .!= "developer"
            <*> o .:? "tools" .!= []
            <*> pure (without ["type", "id", "role", "tools"] o)

data LocalShellAction
    = LocalShellExec
        { command           :: ![Text]
        , timeoutMs         :: !(Maybe Integer)
        , workingDirectory  :: !(Maybe Text)
        , env               :: !(Maybe Aeson.Object)
        , user              :: !(Maybe Text)
        , extraFields       :: !Aeson.Object
        }
    | LocalShellActionOther !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON LocalShellAction where
    toJSON LocalShellExec
        { command, timeoutMs, workingDirectory, env, user, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("exec" :: Text))
                , Just (field "command" command)
                , optionalField "timeout_ms" timeoutMs
                , optionalField "working_directory" workingDirectory
                , optionalField "env" env
                , optionalField "user" user
                ]
    toJSON (LocalShellActionOther tagged) = toJSON tagged

instance FromJSON LocalShellAction where
    parseJSON value = withObject "LocalShellAction" (\o -> do
        tag <- o .:? "type"
        case tag of
            Just ("exec" :: Text) -> LocalShellExec
                <$> o .:? "command" .!= []
                <*> o .:? "timeout_ms"
                <*> o .:? "working_directory"
                <*> o .:? "env"
                <*> o .:? "user"
                <*> pure
                    (without
                        [ "type", "command", "timeout_ms", "working_directory"
                        , "env", "user"
                        ]
                        o)
            _ -> LocalShellActionOther <$> parseJSON value) value

data LocalShellCall = LocalShellCall
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe ItemStatus)
    , action      :: !(Maybe LocalShellAction)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON LocalShellCall where
    toJSON LocalShellCall { itemId, callId, status, action, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("local_shell_call" :: Text))
            , optionalField "id" itemId
            , optionalField "call_id" callId
            , optionalField "status" status
            , optionalField "action" action
            ]

instance FromJSON LocalShellCall where
    parseJSON = withObject "LocalShellCall" $ \o -> LocalShellCall
        <$> o .:? "id"
        <*> o .:? "call_id"
        <*> o .:? "status"
        <*> o .:? "action"
        <*> pure (without ["type", "id", "call_id", "status", "action"] o)

data ToolSearchCall = ToolSearchCall
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , execution   :: !(Maybe Text)
    , arguments   :: !(Maybe Aeson.Value)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ToolSearchCall where
    toJSON ToolSearchCall
        { itemId, callId, status, execution, arguments, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("tool_search_call" :: Text))
                , optionalField "id" itemId
                , optionalField "call_id" callId
                , optionalField "status" status
                , optionalField "execution" execution
                , optionalField "arguments" arguments
                ]

instance FromJSON ToolSearchCall where
    parseJSON = withObject "ToolSearchCall" $ \o -> ToolSearchCall
        <$> o .:? "id"
        <*> o .:? "call_id"
        <*> o .:? "status"
        <*> o .:? "execution"
        <*> o .:? "arguments"
        <*> pure
            (without
                ["type", "id", "call_id", "status", "execution", "arguments"] o)

data ToolSearchOutput = ToolSearchOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , execution   :: !(Maybe Text)
    , tools       :: ![Aeson.Value]
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ToolSearchOutput where
    toJSON ToolSearchOutput
        { itemId, callId, status, execution, tools, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("tool_search_output" :: Text))
                , optionalField "id" itemId
                , optionalField "call_id" callId
                , optionalField "status" status
                , optionalField "execution" execution
                , Just (field "tools" tools)
                ]

instance FromJSON ToolSearchOutput where
    parseJSON = withObject "ToolSearchOutput" $ \o -> ToolSearchOutput
        <$> o .:? "id"
        <*> o .:? "call_id"
        <*> o .:? "status"
        <*> o .:? "execution"
        <*> o .:? "tools" .!= []
        <*> pure
            (without
                ["type", "id", "call_id", "status", "execution", "tools"] o)

data WebSearchAction
    = WebSearchQuery
        { query       :: !(Maybe Text)
        , queries     :: !(Maybe [Text])
        , extraFields :: !Aeson.Object
        }
    | WebSearchOpenPage
        { url         :: !(Maybe Text)
        , extraFields :: !Aeson.Object
        }
    | WebSearchFindInPage
        { url         :: !(Maybe Text)
        , pattern     :: !(Maybe Text)
        , extraFields :: !Aeson.Object
        }
    | WebSearchActionOther !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON WebSearchAction where
    toJSON WebSearchQuery { query, queries, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("search" :: Text))
            , optionalField "query" query
            , optionalField "queries" queries
            ]
    toJSON WebSearchOpenPage { url, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("open_page" :: Text))
            , optionalField "url" url
            ]
    toJSON WebSearchFindInPage { url, pattern, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("find_in_page" :: Text))
            , optionalField "url" url
            , optionalField "pattern" pattern
            ]
    toJSON (WebSearchActionOther tagged) = toJSON tagged

instance FromJSON WebSearchAction where
    parseJSON value = withObject "WebSearchAction" (\o -> do
        tag <- o .:? "type"
        case tag of
            Just ("search" :: Text) -> WebSearchQuery
                <$> o .:? "query"
                <*> o .:? "queries"
                <*> pure (without ["type", "query", "queries"] o)
            Just "open_page" -> WebSearchOpenPage
                <$> o .:? "url"
                <*> pure (without ["type", "url"] o)
            Just "find_in_page" -> WebSearchFindInPage
                <$> o .:? "url"
                <*> o .:? "pattern"
                <*> pure (without ["type", "url", "pattern"] o)
            _ -> WebSearchActionOther <$> parseJSON value) value

data WebSearchCall = WebSearchCall
    { itemId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , action      :: !(Maybe WebSearchAction)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON WebSearchCall where
    toJSON WebSearchCall { itemId, status, action, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("web_search_call" :: Text))
            , optionalField "id" itemId
            , optionalField "status" status
            , optionalField "action" action
            ]

instance FromJSON WebSearchCall where
    parseJSON = withObject "WebSearchCall" $ \o -> WebSearchCall
        <$> o .:? "id"
        <*> o .:? "status"
        <*> o .:? "action"
        <*> pure (without ["type", "id", "status", "action"] o)

data ImageGenerationCall = ImageGenerationCall
    { itemId         :: !(Maybe Text)
    , status         :: !(Maybe Text)
    , revisedPrompt  :: !(Maybe Text)
    , result         :: !(Maybe Text)
    , extraFields    :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ImageGenerationCall where
    toJSON ImageGenerationCall
        { itemId, status, revisedPrompt, result, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("image_generation_call" :: Text))
                , optionalField "id" itemId
                , optionalField "status" status
                , optionalField "revised_prompt" revisedPrompt
                , optionalField "result" result
                ]

instance FromJSON ImageGenerationCall where
    parseJSON = withObject "ImageGenerationCall" $ \o ->
        ImageGenerationCall
            <$> o .:? "id"
            <*> o .:? "status"
            <*> o .:? "revised_prompt"
            <*> o .:? "result"
            <*> pure
                (without
                    ["type", "id", "status", "revised_prompt", "result"] o)

data CompactionItem = CompactionItem
    { itemId            :: !(Maybe Text)
    , encryptedContent  :: !(Maybe Text)
    , extraFields       :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CompactionItem where
    toJSON CompactionItem { itemId, encryptedContent, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("compaction" :: Text))
            , optionalField "id" itemId
            , optionalField "encrypted_content" encryptedContent
            ]

instance FromJSON CompactionItem where
    parseJSON = withObject "CompactionItem" $ \o -> CompactionItem
        <$> o .:? "id"
        <*> o .:? "encrypted_content"
        <*> pure (without ["type", "id", "encrypted_content"] o)

data CompactionTriggerItem = CompactionTriggerItem
    { extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CompactionTriggerItem where
    toJSON CompactionTriggerItem { extraFields } =
        objectWith extraFields
            [Just (field "type" ("compaction_trigger" :: Text))]

instance FromJSON CompactionTriggerItem where
    parseJSON = withObject "CompactionTriggerItem" $ \o ->
        CompactionTriggerItem <$> pure (without ["type"] o)

data ContextCompactionItem = ContextCompactionItem
    { itemId           :: !(Maybe Text)
    , encryptedContent :: !(Maybe Text)
    , extraFields      :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ContextCompactionItem where
    toJSON ContextCompactionItem { itemId, encryptedContent, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("context_compaction" :: Text))
            , optionalField "id" itemId
            , optionalField "encrypted_content" encryptedContent
            ]

instance FromJSON ContextCompactionItem where
    parseJSON = withObject "ContextCompactionItem" $ \o ->
        ContextCompactionItem
            <$> o .:? "id"
            <*> o .:? "encrypted_content"
            <*> pure (without ["type", "id", "encrypted_content"] o)

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

instance FromJSON ResponseItem where
    parseJSON value = withObject "ResponseItem" (\o -> do
        tag <- o .: "type"
        case parseResponseItemType tag of
            ItemMessage -> MessageItem <$> parseJSON value
            ItemFunctionCall -> FunctionCallItem <$> parseJSON value
            ItemFunctionCallOutput ->
                FunctionCallOutputItem <$> parseJSON value
            ItemCustomToolCall -> CustomToolCallItem <$> parseJSON value
            ItemCustomToolCallOutput ->
                CustomToolCallOutputItem <$> parseJSON value
            ItemComputerCall -> ComputerCallItem <$> parseJSON value
            ItemComputerCallOutput -> ComputerCallOutputItem <$> parseJSON value
            ItemReasoning -> ReasoningItemValue <$> parseJSON value
            ItemReferenceType -> ItemReferenceValue <$> parseJSON value
            ItemAgentMessage -> AgentMessageItem <$> parseJSON value
            ItemAdditionalTools -> AdditionalToolsItemValue <$> parseJSON value
            ItemLocalShellCall -> LocalShellCallItem <$> parseJSON value
            ItemToolSearchCall -> ToolSearchCallItem <$> parseJSON value
            ItemToolSearchOutput -> ToolSearchOutputItem <$> parseJSON value
            ItemWebSearchCall -> WebSearchCallItem <$> parseJSON value
            ItemImageGenerationCall ->
                ImageGenerationCallItem <$> parseJSON value
            ItemCompaction -> CompactionItemValue <$> parseJSON value
            ItemCompactionTrigger ->
                CompactionTriggerItemValue <$> parseJSON value
            ItemContextCompaction ->
                ContextCompactionItemValue <$> parseJSON value
            ItemUnknownType{} -> UnknownResponseItem <$> parseJSON value
            itemType -> KnownResponseItem itemType <$> parseJSON value) value

data ResponseInput
    = ResponseInputText !Text
    | ResponseInputItems ![ResponseItem]
    deriving stock (Eq, Show)

instance ToJSON ResponseInput where
    toJSON (ResponseInputText value) = Aeson.String value
    toJSON (ResponseInputItems values) = toJSON values

instance FromJSON ResponseInput where
    parseJSON (Aeson.String value) = pure (ResponseInputText value)
    parseJSON value@(Aeson.Array _) = ResponseInputItems <$> parseJSON value
    parseJSON value =
        fail ("ResponseInput: expected string or array, got " <> show value)
