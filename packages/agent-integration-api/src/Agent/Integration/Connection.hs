-- | Native connection management. No credentials or authorization challenges
-- enter the conversational tool protocol. A distribution owns the state
-- machine; the host renders these typed, non-executable descriptions.
module Agent.Integration.Connection where

import Data.ByteString (ByteString)
import Data.Text (Text)

data ConnectionPhase
    = ConnectionCatalog
    | ConnectionSearch
    | ConnectionCredentials
    | ConnectionChallenge
    | ConnectionRedirect
    | ConnectionSelection
    | ConnectionConnected
    | ConnectionWaiting
    deriving (Eq, Show, Enum, Bounded)

data ConnectionFieldKind = ConnectionTextField | ConnectionSecretField
    deriving (Eq, Show, Enum, Bounded)

data ConnectionField = ConnectionField
    { connectionFieldId :: !Text
    , connectionFieldLabel :: !Text
    , connectionFieldKind :: !ConnectionFieldKind
    , connectionFieldRequired :: !Bool
    } deriving (Eq, Show)

data ConnectionItem = ConnectionItem
    { connectionItemId :: !Text
    , connectionItemTitle :: !Text
    , connectionItemDetail :: !Text
    , connectionItemSelected :: !Bool
    } deriving (Eq, Show)

data ConnectionSnapshot = ConnectionSnapshot
    { connectionPhase :: !ConnectionPhase
    , connectionSessionId :: !Text
    , connectionTitle :: !Text
    , connectionMessage :: !Text
    , connectionRedirectUrl :: !Text
    , connectionPollAfterMilliseconds :: !Int
    , connectionFields :: ![ConnectionField]
    , connectionItems :: ![ConnectionItem]
    } deriving (Eq, Show)

-- Deliberately no Show instance: answers may contain PINs or TANs.
data ConnectionAnswer = ConnectionAnswer
    { connectionAnswerId :: !Text
    , connectionAnswerValue :: !Text
    }

-- Deliberately no Show instance: commands can contain secret answers.
data ConnectionCommand
    = ListConnections
    | SearchConnections !Text !Text
    | BeginConnection !Text !Text
    | SubmitConnection !Text ![ConnectionAnswer]
    | PollConnection !Text
    | CancelConnection !Text
    | DisconnectConnection !Text

-- | Key bytes are supplied by the native secure store, not the chat or a
-- configuration file. The provider must validate its required key length.
type ConnectionSecretStore = Text -> IO (Either Text ByteString)

-- | Every operation is bound to the runtime's captured authority. Providers
-- must reject stale sessions and cancel/join outstanding work when closed.
-- The supplied secure-store function is borrowed for this invocation only.
-- Cache validated key bytes if needed by subsequent tool calls, never the
-- function itself: its native callback context expires at completion.
-- Cancellation must be callable concurrently with an in-flight operation.
newtype IntegrationConnections = IntegrationConnections
    { runConnectionCommand
        :: ConnectionSecretStore
        -> ConnectionCommand
        -> IO (Either Text ConnectionSnapshot)
    }
