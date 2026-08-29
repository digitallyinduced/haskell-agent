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
    , ToolResultContent(..)
    , StreamToolUse(..)
    , OpaqueMessage(..)
    , UserMessage(..)
    , AssistantMessage(..)
    , SystemMessage(..)
    , ResultMessage(..)
    , StreamEvent(..)
    , ConversationResetMessage(..)
    , Message(..)
    , QueryMessageScope(..)
    , QueryProgress(..)
    , messageUuid
    , messageSessionId
    , messageParentToolUseId
    , messageHasParentToolUseId
    ) where

import Agent.Json (RawJson)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data PermissionMode
    = PermissionDefault
    | PermissionAcceptEdits
    | PermissionPlan
    | PermissionBypassPermissions
    | PermissionDontAsk
    -- | Compatibility spelling accepted by newer Claude Code releases.
    | PermissionManual
    | PermissionAuto
    deriving (Eq, Ord, Show)

permissionModeName :: PermissionMode -> Text
permissionModeName = \case
    PermissionDefault -> "default"
    PermissionAcceptEdits -> "acceptEdits"
    PermissionPlan -> "plan"
    PermissionBypassPermissions -> "bypassPermissions"
    PermissionDontAsk -> "dontAsk"
    PermissionManual -> "manual"
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
    --
    -- Claude Code runs its own tools and echoes every tool result on stdout
    -- as a single NDJSON record. Reading an image or PDF therefore produces
    -- a record that embeds the whole file as base64, and multi-megabyte
    -- records are routine. The limit only guards against a runaway process,
    -- so it should stay far above any legitimate record.
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
    , maxBufferSizeBytes = 1_073_741_824
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

-- | Provenance attached to user messages and terminal results.
data MessageOrigin = MessageOrigin
    { kind :: !Text
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
        , input :: !RawJson
        }
    | ToolResultBlock
        { toolUseId :: !Text
        , content :: !(Maybe ToolResultContent)
        , isError :: !(Maybe Bool)
        }
    | ServerToolUseBlock
        { toolUseId :: !Text
        , name :: !Text
        , input :: !RawJson
        }
    | ServerToolResultBlock
        { toolUseId :: !Text
        , content :: !(Maybe ToolResultContent)
        }
    | UnknownContentBlock
        { contentType :: !(Maybe Text)
        , raw :: !RawJson
        }
    deriving (Eq, Show)

-- | Opaque tool-result JSON plus the protocol-specific text projection used
-- for loop events and persisted tool output.
data ToolResultContent = ToolResultContent
    { raw :: !RawJson
    , renderedText :: !Text
    } deriving (Eq, Show)

data StreamToolUse = StreamToolUse
    { toolUseId :: !Text
    , name :: !Text
    , input :: !RawJson
    } deriving (Eq, Show)

-- | Envelope metadata retained for message kinds whose payload is otherwise
-- deliberately opaque.
data OpaqueMessage = OpaqueMessage
    { uuid :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    , raw :: !RawJson
    } deriving (Eq, Show)

data UserMessage = UserMessage
    { content :: ![ContentBlock]
    , uuid :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    , sessionId :: !(Maybe Text)
    , origin :: !(Maybe MessageOrigin)
    } deriving (Eq, Show)

data AssistantMessage = AssistantMessage
    { content :: ![ContentBlock]
    , model :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    , error :: !(Maybe Text)
    , usage :: !(Maybe Usage)
    , messageId :: !(Maybe Text)
    , stopReason :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , uuid :: !(Maybe Text)
    , supersedes :: ![Text]
    } deriving (Eq, Show)

data SystemMessage = SystemMessage
    { subtype :: !Text
    , sessionId :: !(Maybe Text)
    , uuid :: !(Maybe Text)
    , apiKeySource :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    , retractedMessageUuids :: ![Text]
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
    , structuredOutput :: !(Maybe RawJson)
    , modelUsage :: !(Map Text ModelUsage)
    , errors :: ![Text]
    , apiErrorStatus :: !(Maybe Int)
    , origin :: !(Maybe MessageOrigin)
    , uuid :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    } deriving (Eq, Show)

data StreamEvent = StreamEvent
    { uuid :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , event :: !RawJson
    , streamToolUse :: !(Maybe StreamToolUse)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    } deriving (Eq, Show)

data ConversationResetMessage = ConversationResetMessage
    { newConversationId :: !(Maybe Text)
    , uuid :: !(Maybe Text)
    , sessionId :: !(Maybe Text)
    , parentToolUseId :: !(Maybe Text)
    , hasParentToolUseId :: !Bool
    } deriving (Eq, Show)

data Message
    = MessageUser !UserMessage
    | MessageAssistant !AssistantMessage
    | MessageSystem !SystemMessage
    | MessageResult !ResultMessage
    | MessageStreamEvent !StreamEvent
    | MessageConversationReset !ConversationResetMessage
    | MessageControlRequest !OpaqueMessage
    | MessageUnknown !OpaqueMessage
    deriving (Eq, Show)

data QueryMessageScope
    = QueryTopLevel
    | QueryNested !(Maybe Text)
    deriving (Eq, Ord, Show)

data QueryProgress
    = QueryMessageObserved !QueryMessageScope !Message
    | QueryMessagesRetracted !(Maybe QueryMessageScope) ![Text]
    deriving (Eq, Show)

messageUuid :: Message -> Maybe Text
messageUuid = \case
    MessageUser message -> message.uuid
    MessageAssistant message -> message.uuid
    MessageSystem message -> message.uuid
    MessageResult message -> message.uuid
    MessageStreamEvent message -> message.uuid
    MessageConversationReset message -> message.uuid
    MessageControlRequest message -> message.uuid
    MessageUnknown message -> message.uuid

messageSessionId :: Message -> Maybe Text
messageSessionId = \case
    MessageUser message -> message.sessionId
    MessageAssistant message -> message.sessionId
    MessageSystem message -> message.sessionId
    MessageResult message -> Just message.sessionId
    MessageStreamEvent message -> message.sessionId
    MessageConversationReset message -> message.sessionId
    MessageControlRequest message -> message.sessionId
    MessageUnknown message -> message.sessionId

messageParentToolUseId :: Message -> Maybe Text
messageParentToolUseId = \case
    MessageUser message -> message.parentToolUseId
    MessageAssistant message -> message.parentToolUseId
    MessageSystem message -> message.parentToolUseId
    MessageResult message -> message.parentToolUseId
    MessageStreamEvent message -> message.parentToolUseId
    MessageConversationReset message -> message.parentToolUseId
    MessageControlRequest message -> message.parentToolUseId
    MessageUnknown message -> message.parentToolUseId

-- | Whether the wire record had a non-null @parent_tool_use_id@. This remains
-- true for malformed or empty values so nested records can never be mistaken
-- for top-level assistant output.
messageHasParentToolUseId :: Message -> Bool
messageHasParentToolUseId = \case
    MessageUser message -> message.hasParentToolUseId
    MessageAssistant message -> message.hasParentToolUseId
    MessageSystem message -> message.hasParentToolUseId
    MessageResult message -> message.hasParentToolUseId
    MessageStreamEvent message -> message.hasParentToolUseId
    MessageConversationReset message -> message.hasParentToolUseId
    MessageControlRequest message -> message.hasParentToolUseId
    MessageUnknown message -> message.hasParentToolUseId
