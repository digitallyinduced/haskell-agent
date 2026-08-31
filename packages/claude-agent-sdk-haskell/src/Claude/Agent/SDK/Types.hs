-- | Public options, messages, and content types. Names intentionally follow
-- the official Python and TypeScript Claude Agent SDKs where Haskell permits.
module Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , defaultClaudeAgentOptions
    , SystemPrompt(..)
    , PermissionMode(..)
    , permissionModeName
    , Usage(..)
    , emptyUsage
    , addUsage
    , ModelUsage(..)
    , modelUsageToUsage
    , MessageOrigin(..)
    , UserContentBlock(..)
    , ContentBlock(..)
    , UserMessage(..)
    , AssistantMessage(..)
    , SystemMessage(..)
    , ResultMessage(..)
    , StreamEvent(..)
    , ConversationResetMessage(..)
    , Message(..)
    , messageUuid
    , messageParentToolUseId
    , messageHasParentToolUseId
    ) where

import Data.Aeson (Object, Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

data PermissionMode
    = PermissionDefault
    | PermissionAcceptEdits
    | PermissionPlan
    | PermissionBypassPermissions
    | PermissionDontAsk
    | PermissionAuto
    deriving (Eq, Ord, Show)

permissionModeName :: PermissionMode -> Text
permissionModeName = \case
    PermissionDefault -> "default"
    PermissionAcceptEdits -> "acceptEdits"
    PermissionPlan -> "plan"
    PermissionBypassPermissions -> "bypassPermissions"
    PermissionDontAsk -> "dontAsk"
    PermissionAuto -> "auto"

data SystemPrompt
    -- | Use the minimal Agent SDK prompt rather than Claude Code's built-in
    -- coding-agent prompt.
    = SystemPromptNone
    -- | Replace the system prompt with the supplied text.
    | SystemPromptText !Text
    -- | Use Claude Code's built-in coding-agent system prompt.
    | SystemPromptClaudeCode
    deriving (Eq, Show)

-- | Options accepted by the subprocess transport. This is intentionally a
-- useful, typed subset of the official SDK option surface; 'extraArgs' keeps
-- new CLI flags usable without waiting for a package release.
data ClaudeAgentOptions = ClaudeAgentOptions
    { executable :: !FilePath
    , cwd :: !FilePath
    , systemPrompt :: !SystemPrompt
    -- | @Nothing@ keeps the CLI default tools; @Just []@ disables all tools.
    , tools :: !(Maybe [Text])
    , allowedTools :: ![Text]
    , disallowedTools :: ![Text]
    , permissionMode :: !(Maybe PermissionMode)
    , allowDangerouslySkipPermissions :: !Bool
    , model :: !(Maybe Text)
    , effort :: !(Maybe Text)
    , resume :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , continueConversation :: !Bool
    -- | @Nothing@ loads the CLI defaults; @Just []@ disables filesystem
    -- setting sources.
    , settingSources :: !(Maybe [Text])
    , mcpServers :: !(Maybe Value)
    , strictMcpConfig :: !Bool
    , includePartialMessages :: !Bool
    , safeMode :: !Bool
    , disableSlashCommands :: !Bool
    , noChrome :: !Bool
    , extraArgs :: !(Map Text (Maybe Text))
    -- | Exact base environment for the child. 'Nothing' inherits the current
    -- environment. SDK classification variables are installed afterward.
    , environment :: !(Maybe [(String, String)])
    , clientApplication :: !(Maybe Text)
    , promptWriteTimeoutMicros :: !Int
    , streamStartupTimeoutMicros :: !Int
    , streamInactivityTimeoutMicros :: !Int
    , turnTimeoutMicros :: !Int
    -- | Maximum size of one newline-delimited structured-output record.
    , maxBufferSizeBytes :: !Int
    } deriving (Eq)

-- The exact child environment can contain provider credentials. Keep the
-- useful process identity in diagnostics without rendering environment
-- values (or the many prompt/session options that may contain user data).
instance Show ClaudeAgentOptions where
    show options =
        "ClaudeAgentOptions { executable = "
            <> show options.executable
            <> ", cwd = "
            <> show options.cwd
            <> ", environment = <redacted>, ... }"

defaultClaudeAgentOptions :: FilePath -> FilePath -> ClaudeAgentOptions
defaultClaudeAgentOptions executable cwd = ClaudeAgentOptions
    { executable
    , cwd
    , systemPrompt = SystemPromptNone
    , tools = Nothing
    , allowedTools = []
    , disallowedTools = []
    , permissionMode = Nothing
    , allowDangerouslySkipPermissions = False
    , model = Nothing
    , effort = Nothing
    , resume = Nothing
    , sessionId = Nothing
    , continueConversation = False
    , settingSources = Nothing
    , mcpServers = Nothing
    , strictMcpConfig = False
    , includePartialMessages = False
    , safeMode = False
    , disableSlashCommands = False
    , noChrome = False
    , extraArgs = Map.empty
    , environment = Nothing
    , clientApplication = Nothing
    , promptWriteTimeoutMicros = 60 * 1_000_000
    , streamStartupTimeoutMicros = 60 * 1_000_000
    , streamInactivityTimeoutMicros = 15 * 60 * 1_000_000
    , turnTimeoutMicros = 2 * 60 * 60 * 1_000_000
    , maxBufferSizeBytes = 1_048_576
    }

data Usage = Usage
    { inputTokens :: !Int
    , outputTokens :: !Int
    , cachedTokens :: !Int
    } deriving (Eq, Show)

emptyUsage :: Usage
emptyUsage = Usage
    { inputTokens = 0
    , outputTokens = 0
    , cachedTokens = 0
    }

addUsage :: Usage -> Usage -> Usage
addUsage left right = Usage
    { inputTokens = left.inputTokens + right.inputTokens
    , outputTokens = left.outputTokens + right.outputTokens
    , cachedTokens = left.cachedTokens + right.cachedTokens
    }

data ModelUsage = ModelUsage
    { inputTokens :: !Int
    , outputTokens :: !Int
    , cacheReadInputTokens :: !Int
    , cacheCreationInputTokens :: !Int
    , costUSD :: !(Maybe Double)
    , raw :: !Object
    } deriving (Eq, Show)

modelUsageToUsage :: ModelUsage -> Usage
modelUsageToUsage modelUsage =
    Usage
        { inputTokens =
            modelUsage.inputTokens
                + modelUsage.cacheCreationInputTokens
                + modelUsage.cacheReadInputTokens
        , outputTokens = modelUsage.outputTokens
        , cachedTokens = modelUsage.cacheReadInputTokens
        }

-- | Provenance attached to user messages and terminal results. Only @kind@
-- has a stable meaning; the raw object is retained for forward-compatible
-- origin-specific metadata.
data MessageOrigin = MessageOrigin
    { kind :: !Text
    , raw :: !Object
    } deriving (Eq, Show)

-- | Content accepted in one streaming-input user message.
data UserContentBlock
    = UserTextBlock
        { text :: !Text
        }
    | UserImageBlock
        { mediaType :: !Text
        , imageBytes :: !ByteString
        }
    deriving (Eq)

instance Show UserContentBlock where
    show UserTextBlock{text} =
        "UserTextBlock { text = " <> show text <> " }"
    show UserImageBlock{mediaType, imageBytes} =
        "UserImageBlock { mediaType = " <> show mediaType
            <> ", imageBytes = <redacted>"
            <> ", imageByteLength = " <> show (ByteString.length imageBytes)
            <> " }"

data ContentBlock
    = TextBlock
        { text :: !Text
        }
    | ThinkingBlock
        { thinking :: !Text
        , signature :: !(Maybe Text)
        }
    | ToolUseBlock
        { toolUseId :: !Text
        , name :: !Text
        , input :: !Value
        }
    | ToolResultBlock
        { toolUseId :: !Text
        , content :: !(Maybe Value)
        , isError :: !(Maybe Bool)
        }
    | ServerToolUseBlock
        { toolUseId :: !Text
        , name :: !Text
        , input :: !Value
        }
    | ServerToolResultBlock
        { toolUseId :: !Text
        , content :: !(Maybe Value)
        }
    | UnknownContentBlock
        { raw :: !Value
        }
    deriving (Eq, Show)

data UserMessage = UserMessage
    { content :: ![ContentBlock]
    , uuid :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , origin :: !(Maybe MessageOrigin)
    , raw :: !Object
    } deriving (Eq, Show)

data AssistantMessage = AssistantMessage
    { content :: ![ContentBlock]
    , model :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , error :: !(Maybe Text)
    , usage :: !(Maybe Usage)
    , messageId :: !(Maybe Text)
    , stopReason :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , uuid :: !(Maybe Text)
    , supersedes :: ![Text]
    , raw :: !Object
    } deriving (Eq, Show)

data SystemMessage = SystemMessage
    { subtype :: !Text
    , sessionId :: !(Maybe Text)
    , uuid :: !(Maybe Text)
    , apiKeySource :: !(Maybe Text)
    , retractedMessageUuids :: ![Text]
    , raw :: !Object
    } deriving (Eq, Show)

data ResultMessage = ResultMessage
    { subtype :: !Text
    , durationMs :: !(Maybe Int)
    , durationApiMs :: !(Maybe Int)
    , isError :: !Bool
    , numTurns :: !(Maybe Int)
    , sessionId :: !Text
    , stopReason :: !(Maybe Text)
    , totalCostUsd :: !(Maybe Double)
    , usage :: !Usage
    , result :: !(Maybe Text)
    , structuredOutput :: !(Maybe Value)
    , modelUsage :: !(Map Text ModelUsage)
    , errors :: ![Text]
    , apiErrorStatus :: !(Maybe Int)
    , origin :: !(Maybe MessageOrigin)
    , uuid :: !(Maybe Text)
    , raw :: !Object
    } deriving (Eq, Show)

data StreamEvent = StreamEvent
    { uuid :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , event :: !Value
    , parentToolUseId :: !(Maybe Text)
    , raw :: !Object
    } deriving (Eq, Show)

data ConversationResetMessage = ConversationResetMessage
    { newConversationId :: !(Maybe Text)
    , uuid :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , raw :: !Object
    } deriving (Eq, Show)

data Message
    = MessageUser !UserMessage
    | MessageAssistant !AssistantMessage
    | MessageSystem !SystemMessage
    | MessageResult !ResultMessage
    | MessageStreamEvent !StreamEvent
    | MessageConversationReset !ConversationResetMessage
    | MessageControlRequest !Object
    | MessageUnknown !Value
    deriving (Eq, Show)

messageUuid :: Message -> Maybe Text
messageUuid = \case
    MessageUser message -> message.uuid
    MessageAssistant message -> message.uuid
    MessageSystem message -> message.uuid
    MessageResult message -> message.uuid
    MessageStreamEvent message -> message.uuid
    MessageConversationReset message -> message.uuid
    MessageControlRequest _ -> Nothing
    MessageUnknown (Aeson.Object object) ->
        nonEmptyRawText "uuid" object
    MessageUnknown _ -> Nothing

messageParentToolUseId :: Message -> Maybe Text
messageParentToolUseId = \case
    MessageUser message -> message.parentToolUseId
    MessageAssistant message -> message.parentToolUseId
    MessageStreamEvent message -> message.parentToolUseId
    MessageUnknown (Aeson.Object object) ->
        nonEmptyRawText "parent_tool_use_id" object
    _ -> Nothing

-- | Whether the wire record had a non-null @parent_tool_use_id@. This remains
-- true for malformed or empty values so nested records can never be mistaken
-- for top-level assistant output.
messageHasParentToolUseId :: Message -> Bool
messageHasParentToolUseId = \case
    MessageUser message -> rawHasParent message.raw
    MessageAssistant message -> rawHasParent message.raw
    MessageSystem message -> rawHasParent message.raw
    MessageResult message -> rawHasParent message.raw
    MessageStreamEvent message -> rawHasParent message.raw
    MessageConversationReset message -> rawHasParent message.raw
    MessageControlRequest object -> rawHasParent object
    MessageUnknown (Aeson.Object object) -> rawHasParent object
    MessageUnknown _ -> False
  where
    rawHasParent object =
        case KeyMap.lookup "parent_tool_use_id" object of
            Nothing -> False
            Just Aeson.Null -> False
            Just _ -> True

nonEmptyRawText :: Aeson.Key -> Object -> Maybe Text
nonEmptyRawText key object =
    case KeyMap.lookup key object of
        Just (Aeson.String rawText) ->
            let stripped = Text.strip rawText
            in if Text.null stripped
                then Nothing
                else Just stripped
        _ ->
            Nothing
