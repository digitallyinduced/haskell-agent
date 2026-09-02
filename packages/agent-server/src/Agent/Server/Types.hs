-- | Public protocol and supervisor types for the HTTP agent server.
module Agent.Server.Types
    ( GatewayBoundary(..)
    , TurnId(..)
    , RequestId(..)
    , TurnStatus(..)
    , turnStatusText
    , TurnSpec(..)
    , TurnRecord(..)
    , HumanRequestKind(..)
    , humanRequestKindText
    , HumanRequestSpec(..)
    , HumanRequest(..)
    , ServerEvent(..)
    , EventSubscription(..)
    , ApiError(..)
    , CreateSessionRequest(..)
    , PatchSessionRequest(..)
    , ForkSessionRequest(..)
    , CreateTurnRequest(..)
    , ResolveRequest(..)
    , SessionArchiveFilter(..)
    , archiveFilterText
    ) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , Value
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | The exact organization-routing context captured at request admission.
--
-- 'NoGatewayBoundary' intentionally covers only sessions with no gateway
-- identity. 'OrganizationGatewayBoundary' is keyed by the credential identity,
-- not merely by the shared gateway connection id.
data GatewayBoundary
    = NoGatewayBoundary
    | OrganizationGatewayBoundary !Text
    deriving (Eq, Ord, Show)

newtype TurnId = TurnId { unTurnId :: Text }
    deriving (Eq, Ord, Show)

newtype RequestId = RequestId { unRequestId :: Text }
    deriving (Eq, Ord, Show)

data TurnStatus
    = TurnQueued
    | TurnRunning
    | TurnWaitingForInput
    | TurnCompleted
    | TurnFailed
    | TurnCancelled
    deriving (Eq, Ord, Show)

turnStatusText :: TurnStatus -> Text
turnStatusText = \case
    TurnQueued -> "queued"
    TurnRunning -> "running"
    TurnWaitingForInput -> "waiting_for_input"
    TurnCompleted -> "completed"
    TurnFailed -> "failed"
    TurnCancelled -> "cancelled"

data TurnSpec = TurnSpec
    { turnSpecSessionId :: !Text
    , turnSpecPrompt :: !Text
    , turnSpecBoundary :: !GatewayBoundary
    }
    deriving (Eq, Show)

data TurnRecord = TurnRecord
    { turnRecordId :: !TurnId
    , turnRecordSessionId :: !Text
    , turnRecordBoundary :: !GatewayBoundary
    , turnRecordStatus :: !TurnStatus
    , turnRecordCreatedAt :: !UTCTime
    , turnRecordStartedAt :: !(Maybe UTCTime)
    , turnRecordFinishedAt :: !(Maybe UTCTime)
    , turnRecordError :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON TurnRecord where
    toJSON turn = object
        [ "id" .= turn.turnRecordId.unTurnId
        , "sessionId" .= turn.turnRecordSessionId
        , "status" .= turnStatusText turn.turnRecordStatus
        , "createdAt" .= turn.turnRecordCreatedAt
        , "startedAt" .= turn.turnRecordStartedAt
        , "finishedAt" .= turn.turnRecordFinishedAt
        , "error" .= turn.turnRecordError
        ]

data HumanRequestKind
    = ToolApprovalRequest
    | RootAccessRequest
    | PlanEnterRequest
    | PlanExitRequest
    | PlanQuestionRequest
    deriving (Eq, Ord, Show)

humanRequestKindText :: HumanRequestKind -> Text
humanRequestKindText = \case
    ToolApprovalRequest -> "tool_approval"
    RootAccessRequest -> "root_access"
    PlanEnterRequest -> "plan_enter"
    PlanExitRequest -> "plan_exit"
    PlanQuestionRequest -> "plan_question"

data HumanRequestSpec = HumanRequestSpec
    { humanRequestSpecKind :: !HumanRequestKind
    , humanRequestSpecPrompt :: !Text
    , humanRequestSpecOptions :: ![Text]
    }
    deriving (Eq, Show)

data HumanRequest = HumanRequest
    { humanRequestId :: !RequestId
    , humanRequestTurnId :: !TurnId
    , humanRequestSessionId :: !Text
    , humanRequestBoundary :: !GatewayBoundary
    , humanRequestKind :: !HumanRequestKind
    , humanRequestPrompt :: !Text
    , humanRequestOptions :: ![Text]
    , humanRequestCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

instance ToJSON HumanRequest where
    toJSON request = object
        [ "id" .= request.humanRequestId.unRequestId
        , "turnId" .= request.humanRequestTurnId.unTurnId
        , "sessionId" .= request.humanRequestSessionId
        , "kind" .= humanRequestKindText request.humanRequestKind
        , "prompt" .= request.humanRequestPrompt
        , "options" .= request.humanRequestOptions
        , "createdAt" .= request.humanRequestCreatedAt
        ]

data ServerEvent = ServerEvent
    { serverEventId :: !Integer
    , serverEventBoundary :: !GatewayBoundary
    , serverEventType :: !Text
    , serverEventTurnId :: !(Maybe TurnId)
    , serverEventSessionId :: !(Maybe Text)
    , serverEventData :: !Value
    , serverEventAt :: !UTCTime
    }
    deriving (Eq, Show)

instance ToJSON ServerEvent where
    toJSON event = object
        [ "id" .= event.serverEventId
        , "type" .= event.serverEventType
        , "turnId" .= fmap (.unTurnId) event.serverEventTurnId
        , "sessionId" .= event.serverEventSessionId
        , "data" .= event.serverEventData
        , "at" .= event.serverEventAt
        ]

data EventSubscription channel = EventSubscription
    { subscriptionReplay :: ![ServerEvent]
    , subscriptionResetRequired :: !Bool
    , subscriptionChannel :: !channel
    }

data ApiError = ApiError
    { apiErrorStatus :: !Int
    , apiErrorCode :: !Text
    , apiErrorMessage :: !Text
    , apiErrorDetails :: !(Maybe Value)
    }
    deriving (Eq, Show)

data CreateSessionRequest = CreateSessionRequest
    { createSessionModel :: !(Maybe Text)
    , createSessionCwd :: !(Maybe FilePath)
    , createSessionEffort :: !(Maybe Text)
    , createSessionTitle :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON CreateSessionRequest where
    parseJSON = withObject "CreateSessionRequest" \value ->
        CreateSessionRequest
            <$> value .:? "model"
            <*> value .:? "cwd"
            <*> value .:? "effort"
            <*> value .:? "title"

data PatchSessionRequest = PatchSessionRequest
    { patchSessionTitle :: !(Maybe Text)
    , patchSessionArchived :: !(Maybe Bool)
    }
    deriving (Eq, Show)

instance FromJSON PatchSessionRequest where
    parseJSON = withObject "PatchSessionRequest" \value ->
        PatchSessionRequest
            <$> value .:? "title"
            <*> value .:? "archived"

data ForkSessionRequest = ForkSessionRequest
    { forkSessionThroughTurn :: !(Maybe Integer)
    , forkSessionTitle :: !(Maybe Text)
    , forkSessionCwd :: !(Maybe FilePath)
    }
    deriving (Eq, Show)

instance FromJSON ForkSessionRequest where
    parseJSON = withObject "ForkSessionRequest" \value ->
        ForkSessionRequest
            <$> value .:? "throughTurn"
            <*> value .:? "title"
            <*> value .:? "cwd"

newtype CreateTurnRequest = CreateTurnRequest
    { createTurnInput :: Text
    }
    deriving (Eq, Show)

instance FromJSON CreateTurnRequest where
    parseJSON = withObject "CreateTurnRequest" \value ->
        CreateTurnRequest <$> value .: "input"

data ResolveRequest = ResolveRequest
    { resolveRequestDecision :: !Text
    , resolveRequestValue :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON ResolveRequest where
    parseJSON = withObject "ResolveRequest" \value ->
        ResolveRequest
            <$> value .: "decision"
            <*> value .:? "value"

data SessionArchiveFilter
    = ActiveSessions
    | ArchivedSessions
    | AllSessions
    deriving (Eq, Ord, Show)

archiveFilterText :: SessionArchiveFilter -> Text
archiveFilterText = \case
    ActiveSessions -> "active"
    ArchivedSessions -> "archived"
    AllSessions -> "all"
