-- | Versioned prompt-file payloads for managed background turns.
module Agent.CLI.ManagedTurn
    ( ManagedTurnMedia(..)
    , ManagedTurnContext(..)
    , ManagedTurnRequest(..)
    , managedTurnRequestFromText
    , managedTurnRequestWithImages
    , managedTurnRequestWithFiles
    , managedTurnRequestWithGateway
    , renderManagedTurnPrompt
    , loadManagedTurnRequest
    , managedTurnInputs
    ) where

import Agent.Loop
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnInput(..)
    )
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.CLI.Json (decodeLazy, integer)
import Agent.Json.Decode (defaultKey, optionalKey)
import Agent.FileRetry (retryOnFileBusy)
import Agent.Json.Decode qualified as Hermes
import Agent.OsPath (unsafeToFilePath)
import Data.Aeson
    ( ToJSON(..)
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory.OsPath (doesFileExist)
import System.OsPath (OsPath)

data ManagedTurnMedia = ManagedTurnMedia
    { managedTurnMediaPath :: !FilePath
    , managedTurnMediaMime :: !Text
    , managedTurnMediaName :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON ManagedTurnMedia where
    toJSON media = object
        [ "path" .= media.managedTurnMediaPath
        , "mime" .= media.managedTurnMediaMime
        , "name" .= media.managedTurnMediaName
        ]

managedTurnMediaDecoder :: Hermes.Decoder ManagedTurnMedia
managedTurnMediaDecoder =
    Hermes.object $
        ManagedTurnMedia
            <$> Hermes.atKey "path" Hermes.string
            <*> Hermes.atKey "mime" Hermes.text
            <*> optionalKey "name" Hermes.text

data ManagedTurnContext = ManagedTurnContext
    { managedGateway :: !Text
    , managedChatId :: !Integer
    , managedMessageThreadId :: !(Maybe Integer)
    , managedReplyToMessageId :: !(Maybe Integer)
    , managedUserId :: !Integer
    } deriving (Eq, Show)

instance ToJSON ManagedTurnContext where
    toJSON context = object
        [ "gateway" .= context.managedGateway
        , "chat_id" .= context.managedChatId
        , "message_thread_id" .= context.managedMessageThreadId
        , "reply_to_message_id" .= context.managedReplyToMessageId
        , "user_id" .= context.managedUserId
        ]

managedTurnContextDecoder :: Hermes.Decoder ManagedTurnContext
managedTurnContextDecoder =
    Hermes.object $
        ManagedTurnContext
            <$> Hermes.atKey "gateway" Hermes.text
            <*> Hermes.atKey "chat_id" integer
            <*> optionalKey "message_thread_id" integer
            <*> optionalKey "reply_to_message_id" integer
            <*> Hermes.atKey "user_id" integer

data ManagedTurnRequest = ManagedTurnRequest
    { managedTurnVersion :: !Int
    , managedTurnText :: !Text
    , managedTurnImages :: ![ManagedTurnMedia]
    , managedTurnFiles :: ![ManagedTurnMedia]
    , managedTurnBridgeDirectory :: !(Maybe FilePath)
    , managedTurnContext :: !(Maybe ManagedTurnContext)
    } deriving (Eq, Show)

instance ToJSON ManagedTurnRequest where
    toJSON request = object
        [ "version" .= request.managedTurnVersion
        , "text" .= request.managedTurnText
        , "images" .= request.managedTurnImages
        , "files" .= request.managedTurnFiles
        , "bridge_directory" .= request.managedTurnBridgeDirectory
        , "context" .= request.managedTurnContext
        ]

managedTurnRequestDecoder :: Hermes.Decoder ManagedTurnRequest
managedTurnRequestDecoder =
    Hermes.object $
        ManagedTurnRequest
            <$> defaultKey 1 "version" Hermes.int
            <*> Hermes.atKey "text" Hermes.text
            <*> defaultKey [] "images" (Hermes.list managedTurnMediaDecoder)
            <*> defaultKey [] "files" (Hermes.list managedTurnMediaDecoder)
            <*> optionalKey "bridge_directory" Hermes.string
            <*> optionalKey "context" managedTurnContextDecoder

managedTurnRequestFromText :: Text -> ManagedTurnRequest
managedTurnRequestFromText text =
    ManagedTurnRequest 1 text [] [] Nothing Nothing

managedTurnRequestWithImages :: Text -> [ManagedTurnMedia] -> ManagedTurnRequest
managedTurnRequestWithImages text images =
    ManagedTurnRequest 1 text images [] Nothing Nothing

managedTurnRequestWithFiles :: Text -> [ManagedTurnMedia] -> ManagedTurnRequest
managedTurnRequestWithFiles text files =
    ManagedTurnRequest 1 text [] files Nothing Nothing

managedTurnRequestWithGateway
    :: FilePath
    -> ManagedTurnContext
    -> ManagedTurnRequest
    -> ManagedTurnRequest
managedTurnRequestWithGateway bridgeDirectory context request =
    request
        { managedTurnBridgeDirectory = Just bridgeDirectory
        , managedTurnContext = Just context
        }

loadManagedTurnRequest :: OsPath -> IO (Either Text ManagedTurnRequest)
loadManagedTurnRequest path = do
    exists <- doesFileExist path
    if not exists
        then pure (Left "managed turn request file does not exist")
        else do
            bytes <- retryOnFileBusy (LBS.readFile (unsafeToFilePath path))
            case decodeLazy managedTurnRequestDecoder bytes of
                Right request@ManagedTurnRequest{managedTurnVersion = version}
                    | version == 1 ->
                        pure (Right request)
                Right request ->
                    pure (Left
                        ("unsupported managed turn version: "
                            <> Text.pack (show request.managedTurnVersion)))
                Left err ->
                    pure (Left
                        ("could not decode managed turn request: "
                            <> err))

managedTurnInputs :: OsPath -> ManagedTurnRequest -> IO [TurnInput]
managedTurnInputs _ request
    | null request.managedTurnImages && null request.managedTurnFiles =
        pure [UserMessage request.managedTurnText]
    | otherwise = do
        loaded <- mapConcurrentlyBounded 4 loadMedia
            (map Left request.managedTurnImages
                <> map Right request.managedTurnFiles)
        let images = [image | Left image <- loaded]
            files = [file | Right file <- loaded]
        pure
            [ UserMultimodalFiles
                { userText = request.managedTurnText
                , userImages = images
                , userFiles = files
                }
            ]
  where
    loadMedia = \case
        Left media -> Left <$> loadImage media
        Right media -> Right <$> loadFile media

    loadImage ManagedTurnMedia{managedTurnMediaPath = path, managedTurnMediaMime = mime} = do
        bytes <- BS.readFile path
        pure ImageAttachment
            { imageMime = mime
            , imageBytes = bytes
            }

    loadFile ManagedTurnMedia
        { managedTurnMediaPath = path
        , managedTurnMediaName = name
        , managedTurnMediaMime = mime
        } = do
        bytes <- BS.readFile path
        pure FileAttachment
            { fileName = name
            , fileMime = mime
            , fileBytes = bytes
            }

renderManagedTurnPrompt :: ManagedTurnRequest -> Text
renderManagedTurnPrompt =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode
