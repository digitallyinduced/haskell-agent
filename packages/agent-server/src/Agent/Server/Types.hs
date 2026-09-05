-- | Public protocol and supervisor types for the HTTP agent server.
module Agent.Server.Types
    ( GatewayBoundary(..)
    , TenantId
    , parseTenantId
    , renderTenantId
    , localTenantId
    , CredentialId
    , parseCredentialId
    , renderCredentialId
    , Principal(..)
    , localPrincipal
    , AccessBoundary(..)
    , accessBoundary
    , TurnId(..)
    , ClientRequestId(..)
    , RequestId(..)
    , TurnStatus(..)
    , turnStatusText
    , TurnSpec(..)
    , TurnRecord(..)
    , TurnReservation(..)
    , TurnCompletionResult(..)
    , TurnExecutionOutput(..)
    , TurnTerminalOutcome(..)
    , TurnResult(..)
    , HumanRequestKind(..)
    , humanRequestKindText
    , HumanRequestSpec(..)
    , HumanResponse(..)
    , HumanRequest(..)
    , ServerEvent(..)
    , EventSubscription(..)
    , ApiError(..)
    , CreateSessionRequest(..)
    , PatchSessionRequest(..)
    , ForkSessionRequest(..)
    , CreateTurnRequest(..)
    , FileAttachment(..)
    , ResolveRequest(..)
    , SessionArchiveFilter(..)
    , archiveFilterText
    ) where

import Agent.CLI.GatewayBoundary (GatewayBoundary(..))
import Agent.Loop (ImageAttachment(..))
import Agent.Server.Identifier (isUUIDText)
import Control.Monad (unless, when)
import Data.Aeson
    ( FromJSON(..)
    , Object
    , ToJSON(..)
    , Value
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Char (isAlphaNum, isAscii, isControl)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock (UTCTime)

newtype TenantId = TenantId Text
    deriving (Eq, Ord, Show)

parseTenantId :: Text -> Either Text TenantId
parseTenantId raw
    | isUUIDText normalized = Right (TenantId normalized)
    | otherwise = Left "tenant id must be a UUID"
  where
    normalized = Text.toLower (Text.strip raw)

renderTenantId :: TenantId -> Text
renderTenantId (TenantId value) = value

-- A fixed, internal identity keeps local single-user mode on the same typed
-- authorization path without accepting a tenant selector from the client.
localTenantId :: TenantId
localTenantId = TenantId "00000000-0000-0000-0000-000000000000"

newtype CredentialId = CredentialId Text
    deriving (Eq, Ord, Show)

parseCredentialId :: Text -> Either Text CredentialId
parseCredentialId raw
    | isUUIDText normalized = Right (CredentialId normalized)
    | otherwise = Left "credential id must be a UUID"
  where
    normalized = Text.toLower (Text.strip raw)

renderCredentialId :: CredentialId -> Text
renderCredentialId (CredentialId value) = value

data Principal = Principal
    { principalTenantId :: !TenantId
    , principalCredentialId :: !(Maybe CredentialId)
    }
    deriving (Eq, Ord, Show)

localPrincipal :: Principal
localPrincipal = Principal
    { principalTenantId = localTenantId
    , principalCredentialId = Nothing
    }

-- | Complete server authorization boundary. Organization gateway identity is
-- deliberately nested below, and never substituted for, tenant identity.
data AccessBoundary = AccessBoundary
    { accessTenantId :: !TenantId
    , accessGatewayBoundary :: !GatewayBoundary
    }
    deriving (Eq, Ord, Show)

accessBoundary :: Principal -> GatewayBoundary -> AccessBoundary
accessBoundary principal gateway = AccessBoundary
    { accessTenantId = principal.principalTenantId
    , accessGatewayBoundary = gateway
    }

newtype TurnId = TurnId { unTurnId :: Text }
    deriving (Eq, Ord, Show)

newtype ClientRequestId = ClientRequestId { unClientRequestId :: Text }
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
    , turnSpecClientRequestId :: !ClientRequestId
    , turnSpecPrompt :: !Text
    , turnSpecImages :: ![ImageAttachment]
    , turnSpecFiles :: ![FileAttachment]
    , turnSpecBoundary :: !AccessBoundary
    }
    deriving (Eq, Show)

instance ToJSON TurnExecutionOutput where
    toJSON output =
        object
            [ "responseId" .= output.turnExecutionResponseId
            , "assistantText" .= output.turnExecutionAssistantText
            , "assistantTextTruncated"
                .= output.turnExecutionAssistantTextTruncated
            , "completion" .= output.turnExecutionCompletion
            ]

data TurnRecord = TurnRecord
    { turnRecordId :: !TurnId
    , turnRecordSessionId :: !Text
    , turnRecordClientRequestId :: !ClientRequestId
    , turnRecordBoundary :: !AccessBoundary
    , turnRecordStatus :: !TurnStatus
    , turnRecordCreatedAt :: !UTCTime
    , turnRecordStartedAt :: !(Maybe UTCTime)
    , turnRecordFinishedAt :: !(Maybe UTCTime)
    , turnRecordError :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON TurnRecord where
    toJSON turn =
        object
            [ "id" .= turn.turnRecordId.unTurnId
            , "sessionId" .= turn.turnRecordSessionId
            , "clientRequestId"
                .= turn.turnRecordClientRequestId.unClientRequestId
            , "status" .= turnStatusText turn.turnRecordStatus
            , "createdAt" .= turn.turnRecordCreatedAt
            , "startedAt" .= turn.turnRecordStartedAt
            , "finishedAt" .= turn.turnRecordFinishedAt
            , "error" .= turn.turnRecordError
            ]

data TurnReservation
    = TurnReservationCreated !TurnRecord
    | TurnReservationExistingOwned !TurnRecord
    | TurnReservationExisting !TurnRecord
    deriving (Eq, Show)

data TurnCompletionResult
    = TurnCompletionComplete
    | TurnCompletionIncomplete !Text !(Maybe Int)
    deriving (Eq, Show)

instance ToJSON TurnCompletionResult where
    toJSON = \case
        TurnCompletionComplete ->
            object ["status" .= ("completed" :: Text)]
        TurnCompletionIncomplete reason reasoningTokens ->
            object
                [ "status" .= ("incomplete" :: Text)
                , "reason" .= reason
                , "reasoningTokens" .= reasoningTokens
                ]

data TurnExecutionOutput = TurnExecutionOutput
    { turnExecutionResponseId :: !Text
    , turnExecutionAssistantText :: !(Maybe Text)
    , turnExecutionAssistantTextTruncated :: !Bool
    , turnExecutionCompletion :: !TurnCompletionResult
    }
    deriving (Eq, Show)

data TurnTerminalOutcome
    = TurnSucceeded !TurnExecutionOutput
    | TurnErrored !Text
    | TurnWasCancelled
    deriving (Eq, Show)

data TurnResult = TurnResult
    { turnResultTurn :: !TurnRecord
    , turnResultOutput :: !(Maybe TurnExecutionOutput)
    }
    deriving (Eq, Show)

instance ToJSON TurnResult where
    toJSON result =
        object
            [ "turn" .= result.turnResultTurn
            , "output" .= result.turnResultOutput
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

data HumanResponse = HumanResponse
    { humanResponseDecision :: !Text
    , humanResponseValue :: !(Maybe Text)
    }
    deriving (Eq, Show)

data HumanRequest = HumanRequest
    { humanRequestId :: !RequestId
    , humanRequestTurnId :: !TurnId
    , humanRequestSessionId :: !Text
    , humanRequestBoundary :: !AccessBoundary
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
    , serverEventBoundary :: !AccessBoundary
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
    , subscriptionLatestEventId :: !(Maybe Integer)
    , subscriptionChannel :: !channel
    , subscriptionClose :: !(IO ())
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
    parseJSON = withObject "CreateSessionRequest" \value -> do
        rejectUnknownFields
            "CreateSessionRequest"
            ["model", "cwd", "effort", "title"]
            value
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
    parseJSON = withObject "PatchSessionRequest" \value -> do
        rejectUnknownFields
            "PatchSessionRequest"
            ["title", "archived"]
            value
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
    parseJSON = withObject "ForkSessionRequest" \value -> do
        rejectUnknownFields
            "ForkSessionRequest"
            ["throughTurn", "title", "cwd"]
            value
        ForkSessionRequest
            <$> value .:? "throughTurn"
            <*> value .:? "title"
            <*> value .:? "cwd"

data CreateTurnRequest = CreateTurnRequest
    { createTurnClientRequestId :: !(Maybe ClientRequestId)
    , createTurnInput :: !Text
    , createTurnImages :: ![ImageAttachment]
    , createTurnFiles :: ![FileAttachment]
    }
    deriving (Eq, Show)

instance FromJSON CreateTurnRequest where
    parseJSON = withObject "CreateTurnRequest" \value -> do
        rejectUnknownFields
            "CreateTurnRequest"
            ["clientRequestId", "input", "images", "files"]
            value
        rawRequestId <- value .:? "clientRequestId"
        requestId <- case rawRequestId of
            Nothing -> pure Nothing
            Just candidate
                | isUUIDText candidate ->
                    pure
                        (Just
                            (ClientRequestId
                                (Text.toLower candidate)))
                | otherwise -> fail "clientRequestId must be a UUID"
        input <- value .:? "input" .!= ""
        imageValues <- value .:? "images" .!= []
        when (length imageValues > maxTurnImageCount) $
            fail "images must contain at most one item"
        images <- traverse parseTurnImage imageValues
        fileValues <- value .:? "files" .!= []
        when (length imageValues + length fileValues > maxTurnAttachmentCount) $
            fail "images and files must contain at most five items in total"
        files <- traverse parseTurnFile fileValues
        when
            ( sum (map (ByteString.length . imageBytes) images)
                + sum (map (ByteString.length . fileBytes) files)
                > maxTurnAttachmentBytesTotal
            )
            (fail "images and files are limited to 20 MiB decoded in total")
        pure (CreateTurnRequest requestId input images files)

data FileAttachment = FileAttachment
    { fileName :: !Text
    , fileMime :: !Text
    , fileBytes :: !ByteString.ByteString
    }
    deriving (Eq, Show)

maxTurnAttachmentCount, maxTurnFileBytesEach, maxTurnAttachmentBytesTotal :: Int
maxTurnAttachmentCount = 5
maxTurnFileBytesEach = 20 * 1024 * 1024
maxTurnAttachmentBytesTotal = 20 * 1024 * 1024

parseTurnFile :: Value -> Parser FileAttachment
parseTurnFile = withObject "TurnFile" \value -> do
    rejectUnknownFields "TurnFile" ["name", "mimeType", "data"] value
    name <- value .: "name"
    when
        ( Text.null name
            || Text.length name > 255
            || Text.any (`elem` ['/', '\\', '\NUL']) name
            || Text.any isControl name
            || name == "."
            || name == ".."
        )
        (fail "file name must be a non-empty basename of at most 255 characters")
    mime <- value .: "mimeType"
    when
        (not (validFileMime mime))
        (fail "file mimeType must be a valid media type of at most 255 characters")
    encoded <- value .: "data"
    bytes <-
        either
            (const (fail "file data must be valid base64"))
            pure
            (Base64.decode (TextEncoding.encodeUtf8 encoded))
    when (ByteString.null bytes) $
        fail "file data must not be empty"
    when (ByteString.length bytes > maxTurnFileBytesEach) $
        fail "each file is limited to 20 MiB decoded"
    pure FileAttachment
        { fileName = name
        , fileMime = mime
        , fileBytes = bytes
        }

validFileMime :: Text -> Bool
validFileMime mime =
    not (Text.null mime)
        && Text.length mime <= 255
        && Text.count "/" mime == 1
        && Text.all
            ( \character ->
                isAscii character
                    && ( isAlphaNum character
                            || character
                                `elem` ("!#$%&'*+-.^_`|~/" :: String)
                       )
            )
            mime

maxTurnImageCount, maxTurnImageBytesEach :: Int
maxTurnImageCount = 1
maxTurnImageBytesEach = 20 * 1024 * 1024

parseTurnImage :: Value -> Parser ImageAttachment
parseTurnImage = withObject "TurnImage" \value -> do
    rejectUnknownFields "TurnImage" ["mimeType", "data"] value
    mime <- value .: "mimeType"
    unless (mime `elem` supportedTurnImageTypes) $
        fail "image mimeType must be image/jpeg, image/png, image/gif, image/webp, or image/bmp"
    encoded <- value .: "data"
    bytes <-
        either
            (const (fail "image data must be valid base64"))
            pure
            (Base64.decode (TextEncoding.encodeUtf8 encoded))
    when (ByteString.null bytes) $
        fail "image data must not be empty"
    when (ByteString.length bytes > maxTurnImageBytesEach) $
        fail "each image is limited to 20 MiB decoded"
    unless (matchesTurnImageSignature mime bytes) $
        fail "image data does not match its mimeType"
    pure ImageAttachment{imageMime = mime, imageBytes = bytes}

supportedTurnImageTypes :: [Text]
supportedTurnImageTypes =
    ["image/jpeg", "image/png", "image/gif", "image/webp", "image/bmp"]

matchesTurnImageSignature :: Text -> ByteString.ByteString -> Bool
matchesTurnImageSignature "image/jpeg" = ByteString.isPrefixOf "\xff\xd8\xff"
matchesTurnImageSignature "image/png" =
    ByteString.isPrefixOf "\x89PNG\r\n\x1a\n"
matchesTurnImageSignature "image/gif" = \bytes ->
    "GIF87a" `ByteString.isPrefixOf` bytes
        || "GIF89a" `ByteString.isPrefixOf` bytes
matchesTurnImageSignature "image/webp" = \bytes ->
    "RIFF" `ByteString.isPrefixOf` bytes
        && ByteString.take 4 (ByteString.drop 8 bytes) == "WEBP"
matchesTurnImageSignature "image/bmp" = ByteString.isPrefixOf "BM"
matchesTurnImageSignature _ = const False

data ResolveRequest = ResolveRequest
    { resolveRequestDecision :: !Text
    , resolveRequestValue :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON ResolveRequest where
    parseJSON = withObject "ResolveRequest" \value -> do
        rejectUnknownFields
            "ResolveRequest"
            ["decision", "value"]
            value
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

rejectUnknownFields :: String -> [Key] -> Object -> Parser ()
rejectUnknownFields typeName allowed value =
    case filter (`notElem` allowed) (KeyMap.keys value) of
        [] -> pure ()
        unknown ->
            fail
                ("unknown field(s) in "
                    <> typeName
                    <> ": "
                    <> unwords (map Key.toString unknown))
