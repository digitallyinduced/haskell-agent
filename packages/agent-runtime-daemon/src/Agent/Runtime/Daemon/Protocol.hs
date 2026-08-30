module Agent.Runtime.Daemon.Protocol
    ( ProtocolVersion (..)
    , currentProtocolVersion
    , supportedProtocolVersions
    , Sequence (..)
    , ClientId (..)
    , CommandId (..)
    , Hello (..)
    , Welcome (..)
    , EventEnvelope (..)
    , ClientMessage (..)
    , ServerMessage (..)
    , negotiateVersion
    ) where

import Data.Aeson
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

newtype ProtocolVersion = ProtocolVersion { unProtocolVersion :: Word16 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = ProtocolVersion 2

supportedProtocolVersions :: [ProtocolVersion]
supportedProtocolVersions = [currentProtocolVersion]

newtype Sequence = Sequence { unSequence :: Word64 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Enum, FromJSON, Num, Real, Integral, ToJSON)

newtype ClientId = ClientId { unClientId :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

newtype CommandId = CommandId { unCommandId :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

data Hello = Hello
    { clientId :: ClientId
    , versions :: [ProtocolVersion]
    , resumeAfter :: Maybe Sequence
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Hello
instance FromJSON Hello

data Welcome = Welcome
    { version :: ProtocolVersion
    , currentSequence :: Sequence
    , heartbeatSeconds :: Int
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Welcome
instance FromJSON Welcome

data EventEnvelope = EventEnvelope
    { sequenceNumber :: Sequence
    , eventType :: Text
    , payload :: Value
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON EventEnvelope
instance FromJSON EventEnvelope

data ClientMessage
    = ClientHello Hello
    | ClientAck Sequence
    | ClientPong Sequence
    | ClientCommand CommandId Value
    deriving stock (Eq, Show)

instance ToJSON ClientMessage where
    toJSON = \case
        ClientHello hello -> object ["type" .= String "hello", "hello" .= hello]
        ClientAck sequenceNumber -> object ["type" .= String "ack", "sequence" .= sequenceNumber]
        ClientPong sequenceNumber -> object ["type" .= String "pong", "sequence" .= sequenceNumber]
        ClientCommand commandId command ->
            object ["type" .= String "command", "id" .= commandId, "command" .= command]

instance FromJSON ClientMessage where
    parseJSON = withObject "ClientMessage" $ \objectValue ->
        objectValue .: "type" >>= \case
            ("hello" :: Text) -> ClientHello <$> objectValue .: "hello"
            "ack" -> ClientAck <$> objectValue .: "sequence"
            "pong" -> ClientPong <$> objectValue .: "sequence"
            "command" -> ClientCommand <$> objectValue .: "id" <*> objectValue .: "command"
            unknown -> fail ("unknown client message type: " <> show unknown)

data ServerMessage
    = ServerWelcome Welcome
    | ServerVersionRejected [ProtocolVersion]
    | ServerSnapshotChunk Sequence Int Int Text
    | ServerEvent EventEnvelope
    | ServerCommandResult CommandId (Either Text Value)
    | ServerHeartbeat Sequence
    deriving stock (Eq, Show)

instance ToJSON ServerMessage where
    toJSON = \case
        ServerWelcome welcome -> object ["type" .= String "welcome", "welcome" .= welcome]
        ServerVersionRejected versions ->
            object ["type" .= String "version_rejected", "supported" .= versions]
        ServerSnapshotChunk sequenceNumber chunkIndex chunkCount snapshotData ->
            object
                [ "type" .= String "snapshot_chunk"
                , "sequence" .= sequenceNumber
                , "chunk_index" .= chunkIndex
                , "chunk_count" .= chunkCount
                , "snapshot_data" .= snapshotData
                ]
        ServerEvent event -> object ["type" .= String "event", "event" .= event]
        ServerCommandResult commandId result ->
            object
                [ "type" .= String "command_result"
                , "id" .= commandId
                , "ok" .= either (const False) (const True) result
                , "error" .= either Just (const Nothing) result
                , "result" .= either (const Nothing) Just result
                ]
        ServerHeartbeat sequenceNumber ->
            object ["type" .= String "heartbeat", "sequence" .= sequenceNumber]

instance FromJSON ServerMessage where
    parseJSON = withObject "ServerMessage" $ \objectValue ->
        objectValue .: "type" >>= \case
            ("welcome" :: Text) -> ServerWelcome <$> objectValue .: "welcome"
            "version_rejected" -> ServerVersionRejected <$> objectValue .: "supported"
            "snapshot_chunk" ->
                ServerSnapshotChunk
                    <$> objectValue .: "sequence"
                    <*> objectValue .: "chunk_index"
                    <*> objectValue .: "chunk_count"
                    <*> objectValue .: "snapshot_data"
            "event" -> ServerEvent <$> objectValue .: "event"
            "command_result" -> do
                commandId <- objectValue .: "id"
                succeeded <- objectValue .: "ok"
                if succeeded
                    then ServerCommandResult commandId . Right <$> objectValue .: "result"
                    else ServerCommandResult commandId . Left <$> objectValue .: "error"
            "heartbeat" -> ServerHeartbeat <$> objectValue .: "sequence"
            unknown -> fail ("unknown server message type: " <> show unknown)

negotiateVersion :: [ProtocolVersion] -> Maybe ProtocolVersion
negotiateVersion offered =
    case filter (`elem` supportedProtocolVersions) offered of
        [] -> Nothing
        compatible -> Just (maximum compatible)
