module Agent.Subagents.TestItems
    ( computerCallItem
    , computerOutputItem
    , functionCallItem
    , functionOutputItem
    , messageItem
    , reasoningItem
    , userItem
    ) where

import Agent.Json (rawJsonFromEncoding)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import Data.Text (Text)

userItem :: Text -> ResponseItem
userItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

messageItem :: ResponseRole -> Text -> ResponseItem
messageItem role text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

functionCallItem :: Text -> ResponseItem
functionCallItem callId = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name = "shell_command"
    , namespace = Nothing
    , provider = Nothing
    , arguments = "{}"
    , encryptedFunctionArgs = Nothing
    , status = Nothing
    , async = Nothing
    }

functionOutputItem :: Text -> ResponseItem
functionOutputItem callId = FunctionCallOutputItem FunctionCallOutput
    { localOutcome = Nothing
    , itemId = Nothing
    , callId
    , name = Nothing
    , namespace = Nothing
    , provider = Nothing
    , output = rawJsonFromEncoding (Aeson.toEncoding ("ok" :: Text))
    , status = Nothing
    , async = Nothing
    }

computerCallItem :: Text -> ResponseItem
computerCallItem computerCallId = ComputerCallItem ComputerCall
    { computerCallItemId = Nothing
    , computerCallId
    , computerActions = []
    , pendingSafetyChecks = []
    , computerCallStatus = Nothing
    , computerCallExtra = mempty
    }

computerOutputItem :: Text -> ResponseItem
computerOutputItem computerOutputCallId =
    ComputerCallOutputItem ComputerCallOutput
        { computerOutputItemId = Nothing
        , computerOutputCallId
        , screenshotDataUrl = ""
        , acknowledgedChecks = []
        , computerOutputStatus = Nothing
        , computerOutputExtra = mempty
        }

reasoningItem :: ResponseItem
reasoningItem = ReasoningItemValue ReasoningItem
    { itemId = Nothing
    , summary = []
    , content = Nothing
    , encryptedContent = Nothing
    , status = Nothing
    }
