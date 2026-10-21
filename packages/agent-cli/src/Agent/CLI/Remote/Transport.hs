-- | Typed local-service operations used by terminal presentation.
module Agent.CLI.Remote.Transport
    ( RemoteSession(..), RemoteModel(..), RemoteFailure(..)
    , requireRemote, remoteJson, remoteSession, remoteSessions, remoteModels
    , createRemoteSession, submitRemoteTurn, remoteDisplayEvent, remoteLocalPayload, remoteEnvelope, remoteErrorText
    , sessionSettingsForOptions, segment
    ) where

import Agent.CLI.Options
import Agent.Dialect (DialectId, parseDialect)
import Agent.Loop (LoopEvent, TokenUsage(..), ImageAttachment(..), FileAttachment(..))
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider)
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Runtime.Request (NativeInteractionMode(..), NativeShellMode(..))
import Agent.Runtime.Session.Events (decodeDisplayEvent)
import Agent.Runtime.Session.Settings
import Agent.Runtime.Project
    (ProjectSettings(..), ProjectModel(..), resolveProjectRoot, loadProjectSettings, inheritProjectLastModel)
import Agent.Runtime.Models (ModelTarget(..))
import Agent.Runtime.Timestamp (MessageClock(..), HourCycle(..))
import Data.Time.LocalTime (timeZoneName, timeZoneMinutes)
import Data.Time.Clock (UTCTime)
import Agent.Server.Client
import Control.Exception.Safe (Exception, throwIO)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types.URI (urlEncode)
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.Directory.OsPath (getHomeDirectory)

data RemoteSession = RemoteSession
    { remoteId :: !Text, remoteModel :: !Text, remoteProvider :: !Provider
    , remoteDialect :: !DialectId, remoteCwd :: !OsPath, remoteEffort :: !Text
    , remoteTitle :: !Text, remoteUsage :: !TokenUsage
    , remoteGatewayIdentity :: !(Maybe Text)
    , remoteUpdatedAt :: !UTCTime
    } deriving (Eq, Show)

instance FromJSON RemoteSession where
    parseJSON = withObject "session" \fields -> do
        remoteId <- fields .: "id"
        remoteModel <- fields .: "model"
        remoteProvider <- fields .: "provider" >>= maybe (fail "unknown provider") pure . parseProvider
        remoteDialect <- fields .: "dialect" >>= maybe (fail "unknown dialect") pure . parseDialect
        remoteCwd <- unsafeEncodeUtf <$> fields .: "cwd"
        remoteEffort <- fields .: "effort"
        remoteTitle <- fields .: "title"
        remoteGatewayIdentity <- fields .:? "gatewayIdentity"
        remoteUpdatedAt <- fields .: "updatedAt"
        remoteUsage <- fields .: "usage" >>= withObject "usage" \usage ->
            TokenUsage <$> usage .: "input" <*> usage .: "output" <*> usage .: "cached"
        pure RemoteSession{..}

data RemoteModel = RemoteModel
    { remoteModelId :: !Text, remoteModelProvider :: !Provider
    , remoteModelDialect :: !DialectId, remoteModelLabel :: !(Maybe Text)
    , remoteModelConnection :: !Text, remoteWireModel :: !Text
    } deriving (Eq, Show)

instance FromJSON RemoteModel where
    parseJSON = withObject "model" \fields ->
        RemoteModel <$> fields .: "id"
            <*> (fields .: "provider" >>= maybe (fail "unknown provider") pure . parseProvider)
            <*> (fields .: "dialect" >>= maybe (fail "unknown dialect") pure . parseDialect)
            <*> fields .:? "label"
            <*> fields .: "connection" <*> fields .: "transportModel"

newtype RemoteList a = RemoteList [a]
instance FromJSON a => FromJSON (RemoteList a) where
    parseJSON = withObject "list" \fields -> RemoteList <$> fields .: "data"

newtype RemoteFailure = RemoteFailure Text deriving (Show)
instance Exception RemoteFailure

requireRemote :: Either AgentServerClientError a -> IO a
requireRemote = either (throwIO . RemoteFailure . remoteErrorText) pure

remoteErrorText :: AgentServerClientError -> Text
remoteErrorText = \case
    AgentServerHttpError _ _ message -> message
    AgentServerTransportError message -> message
    AgentServerProtocolError message -> message
    AgentServerCredentialError message -> message
    AgentServerDecodeError message -> message

remoteJson :: FromJSON a => AgentServerClient -> ByteString -> Text -> [Int] -> Maybe Value -> IO a
remoteJson client method path statuses body =
    performAgentServerJsonRequest client method path statuses body >>= requireRemote

segment :: Text -> Text
segment = Text.decodeUtf8 . urlEncode True . Text.encodeUtf8

remoteSession :: AgentServerClient -> Text -> IO RemoteSession
remoteSession client identifier = remoteJson client "GET" ("/v1/sessions/" <> segment identifier) [200] Nothing

remoteSessions :: AgentServerClient -> IO [RemoteSession]
remoteSessions client = do
    collect Nothing
  where
    collect cursor = do
        value <- remoteJson client "GET" ("/v1/sessions?limit=100"
            <> maybe "" (("&cursor=" <>) . segment) cursor) [200] Nothing
        (sessions, next) <- either (throwIO . RemoteFailure . Text.pack) pure $
            parseEither (withObject "session list" (\fields -> (,) <$> fields .: "data" <*> fields .:? "nextCursor")) value
        rest <- if next == Nothing || next == cursor then pure [] else collect next
        pure (sessions <> rest)

remoteModels :: AgentServerClient -> IO [RemoteModel]
remoteModels client = do
    RemoteList models <- remoteJson client "GET" "/v1/models" [200] Nothing
    pure models

createRemoteSession :: AgentServerClient -> CliOptions -> OsPath -> IO RemoteSession
createRemoteSession client options cwd = do
    home <- getHomeDirectory
    root <- resolveProjectRoot cwd
    inherited <- loadProjectSettings root >>= inheritProjectLastModel home root
    let remembered = inherited.settingsLastModel >>= \choice ->
            if maybe True (== choice.projectModelTarget.targetProvider) options.optProvider
                then Just choice.projectModelTarget.targetModelId else Nothing
        requested = options.optModel <|> remembered
    selected <- case (options.optProvider, requested) of
        (Just provider, Nothing) -> remoteModels client >>= \models ->
            maybe (throwIO (RemoteFailure "No model is available for the requested provider."))
                (pure . Just . (.remoteModelId)) (find ((== provider) . (.remoteModelProvider)) models)
        (Just provider, Just name) -> remoteModels client >>= \models ->
            case find ((== name) . (.remoteModelId)) models of
                Just model | model.remoteModelProvider /= provider ->
                    throwIO (RemoteFailure "The selected model belongs to a different provider.")
                _ -> pure requested
        _ -> pure requested
    remoteJson client "POST" "/v1/sessions" [201] (Just (object
        [ "cwd" .= showPath cwd, "model" .= selected
        , "effort" .= (reasoningEffortText <$> options.optEffort) ]))
  where
    showPath = unsafeToFilePath

submitRemoteTurn
    :: AgentServerClient -> RemoteSession -> SessionSettings -> SessionOperation -> Text -> [ImageAttachment]
    -> [FileAttachment] -> Maybe MessageClock
    -> IO AgentServerTurn
submitRemoteTurn client session settings operation prompt images files clock = do
    identifier <- newAgentServerClientRequestId
    remoteJson client "POST" ("/v1/sessions/" <> segment session.remoteId <> "/turns") [202]
        (Just (object (
            [ "clientRequestId" .= identifier, "input" .= prompt
            , "images" .= [AgentServerTurnImage image.imageMime image.imageBytes | image <- images]
            , "files" .= [AgentServerTurnFile (maybe "attachment" id file.fileName)
                file.fileMime file.fileBytes | file <- files]
            , "localOptions" .= settings{sessionOperation = operation}
            ] <> maybe [] (\value ->
                [ "hourCycle" .= (case value.messageHourCycle of Hour12 -> ("h12" :: Text); Hour24 -> "h24")
                , "timeZoneName" .= timeZoneName value.messageTimeZone
                , "timeZoneOffsetMinutes" .= timeZoneMinutes value.messageTimeZone ]) clock)))

sessionSettingsForOptions :: CliOptions -> SessionSettings
sessionSettingsForOptions options = defaultSessionSettings
    { sessionMaxTurns = options.optMaxTurns
    , sessionMaxConcurrentAgents = options.optMaxConcurrentAgents
    , sessionCompactThreshold = options.optCompactThreshold
    , sessionShowRawReasoning = options.optShowRawReasoning
    , sessionAgentsMd = options.optAgentsMd, sessionSkills = options.optSkills
    , sessionComputerUse = options.optComputerUse /= Explicit False && not (isOneShot options)
    , sessionCodeMode = case options.optCodeMode of
        CodeModeCatalog -> "catalog"; CodeModeEnabled -> "enabled"; CodeModeDisabled -> "disabled"
    , sessionCodeModeBackend = options.optCodeModeBackend, sessionCodeModeRepair = options.optCodeModeRepair
    , sessionOneShot = isOneShot options
    , sessionInteractionMode = if options.optYolo == Explicit True || (isOneShot options && options.optYolo /= Explicit False)
        then NativeYolo else NativeAsk
    , sessionShellMode = case (options.optGhci, options.optBash) of
        (True, True) -> NativeShellBoth; (True, False) -> NativeShellGhci
        (False, True) -> NativeShellBash; (False, False) -> NativeShellNone
    , sessionReadOnly = options.optManagedDenyMutations
    }

remoteDisplayEvent :: AgentServerClient -> AgentServerEvent -> IO (Either AgentServerClientError (Maybe LoopEvent))
remoteDisplayEvent client event = remoteLocalPayload client event >>= pure . (>>= traverse decode)
  where
    decode value = either (const (Left (AgentServerProtocolError "invalid local display event")))
        Right (decodeDisplayEvent value)

remoteLocalPayload :: AgentServerClient -> AgentServerEvent -> IO (Either AgentServerClientError (Maybe Value))
remoteLocalPayload client event = case event.agentServerEventData of
    Object fields -> case KeyMap.lookup "localEvent" fields of
        Nothing -> pure (Right Nothing)
        Just envelope -> fmap (fmap Just) (remoteEnvelope client envelope)
    _ -> pure (Right Nothing)

remoteEnvelope :: AgentServerClient -> Value -> IO (Either AgentServerClientError Value)
remoteEnvelope client envelope = case parseEither parseEnvelope envelope of
            Left _ -> pure (Left (AgentServerProtocolError "invalid local event envelope"))
            Right (Left value) -> pure (Right value)
            Right (Right (turnId, identifier)) ->
                getAgentServerLocalEventPayload client turnId identifier
  where
    parseEnvelope = withObject "local event envelope" \fields ->
        fields .:? "event" >>= \case
            Just value -> pure (Left value)
            Nothing -> Right <$> ((,) <$> fields .: "turnId" <*> fields .: "payloadId")
