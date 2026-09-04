-- | HTTPS device authorization and restricted gateway credential storage.
module Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayModel(..)
    , GatewayModelCatalogResponse(..)
    , GatewayModelProtocol(..)
    , GatewayModelAccess
    , GatewayAuthorization(..)
    , GatewayAuthorizationCodeResponse(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , gatewayCredentialIdentity
    , connectGatewayBrowser
    , connectGatewayBrowserWithCancel
    , defaultGatewayBaseUrl
    , gatewayAuthorizationCodeDecoder
    , gatewayAuthorizationUrl
    , gatewayBrowserClientId
    , gatewayBrowserRedirectPath
    , gatewayPkceChallenge
    , validateGatewayAuthorizationCallback
    , validateGatewayAuthorizationCodeResponse
    , validateGatewayDeviceAuthorization
    , startGatewayAuthorization
    , startNativeGatewayAuthorization
    , pollGatewayAuthorization
    , pollNativeGatewayAuthorizationAndSave
    , pollNativeGatewayAuthorizationAndSaveWith
    , exchangeNativeGatewayAuthorizationCode
    , exchangeNativeGatewayAuthorizationCodeWith
    , saveGatewayCredential
    , removeGatewayCredential
    , removeGatewayCredentialWith
    , openGatewayAuthorizationPage
    , connectGateway
    , disconnectGateway
    , gatewayCredentialPath
    , withGatewayCredentialLock
    , withGatewayCredentialLockAt
    , withGatewayCredentialLease
    , withGatewayCredentialLeaseAt
    , withGatewayCredentialTurnLease
    , withGatewayCredentialTurnLeaseAt
    , gatewayDeviceDecoder
    , gatewayPollDecoder
    , loadGatewayCredential
    , loadGatewayCredentialAt
    , fetchGatewayModels
    , fetchGatewayUsage
    , newGatewayModelAccess
    , newGatewayModelAccessWith
    , newGatewayModelAccessWithDictation
    , newGatewayModelAccessWithUsage
    , refreshGatewayModels
    , cachedGatewayModels
    , gatewayModelIds
    , transcribeGatewayPcm
    , runGatewayCommand
    , saveGatewayCredentialAt
    , showGatewayStatus
    , validateBaseUrl
    , validateGatewayCredential
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OpenAI.Transcription
    ( ChatGPTDictationStreamFailure(..)
    , encodePcm16Wav
    , openAITranscriptionSampleRate
    , transcribePcmWithChatGPTStreamAt
    )
import Agent.CLI.Runtime.Options (GatewayCommand (..))
import Agent.CLI.PrivateFileLock
    ( withPrivateFileLock
    , withPrivateSharedFileLock
    , withPrivateSharedFileLocksAfterGate
    )
import Agent.Json.Decode qualified as Hermes
import Agent.OpenAI.Usage (UsageSnapshot, decodeUsageResponse)
import Agent.OpenAI.WebSocketClient (validateGatewayWebSocketUrl)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , retry
    , writeTVar
    )
import Control.Exception.Safe
    ( bracket
    , bracketOnError
    , finally
    , mask
    , onException
    , throwString
    , tryAny
    )
import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson ((.=), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit, isPrint, isSpace)
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hAccept
    , hAuthorization
    , hContentType
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (parseQueryText, renderQueryText)
import Network.URI qualified as URI
import Network.Socket
    ( Family(AF_INET)
    , SockAddr(SockAddrInet)
    , Socket
    , SocketOption(ReuseAddr)
    , SocketType(Stream)
    , accept
    , bind
    , close
    , defaultProtocol
    , getSocketName
    , listen
    , setSocketOption
    , socket
    , tupleToHostAddress
    )
import Network.Socket.ByteString qualified as Socket
import System.Directory.OsPath qualified as Directory
import System.Entropy (getEntropy)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Process (rawSystem)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data GatewayCredential = GatewayCredential
    { gatewayBaseUrl :: !Text
    , gatewayWebSocketUrl :: !Text
    , gatewayAccessToken :: !Text
    }
    deriving (Eq)

data GatewayModelProtocol
    = GatewayResponsesProtocol
    | GatewayAnthropicProtocol
    deriving (Eq, Show)

data GatewayModel = GatewayModel
    { gatewayModelId :: !Text
    , gatewayModelProtocol :: !GatewayModelProtocol
    }
    deriving (Eq, Show)

-- | Stable, non-secret binding for sessions created with one exact gateway
-- credential. Reauthorization deliberately produces a different identity:
-- without a gateway-issued organization identifier, treating a replacement
-- bearer as the same routing boundary could send prior context to another
-- organization.
gatewayCredentialIdentity :: GatewayCredential -> Text
gatewayCredentialIdentity credential =
    "gateway-sha256:"
        <> TextEncoding.decodeUtf8
            (Base64Url.encodeUnpadded
                (ByteArray.convert
                    (hash material :: Digest SHA256)))
  where
    material =
        TextEncoding.encodeUtf8 $
            Text.intercalate
                "\NUL"
                [ "haskell-agent gateway session binding v1"
                , canonicalGatewayIdentityUrl
                    ""
                    credential.gatewayBaseUrl
                , canonicalGatewayIdentityUrl
                    "/v1/responses"
                    credential.gatewayWebSocketUrl
                , credential.gatewayAccessToken
                ]

canonicalGatewayIdentityUrl :: Text -> Text -> Text
canonicalGatewayIdentityUrl defaultPath raw =
    case URI.parseURI (Text.unpack (Text.strip raw)) of
        Just uri
            | Just authority <- URI.uriAuthority uri
            , let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
            , Right port <-
                gatewayOriginPort
                    "invalid gateway identity URL"
                    scheme
                    (URI.uriPort authority) ->
                let host =
                        Text.toLower (Text.pack (URI.uriRegName authority))
                    rawPath = Text.pack (URI.uriPath uri)
                    path
                        | Text.null rawPath = defaultPath
                        | Text.null defaultPath =
                            Text.dropWhileEnd (== '/') rawPath
                        | otherwise = rawPath
                in scheme
                    <> "//"
                    <> host
                    <> ":"
                    <> Text.pack (show port)
                    <> path
        _ -> Text.strip raw

newtype GatewayModelCatalogResponse = GatewayModelCatalogResponse
    { gatewayModelCatalogData :: [GatewayModel]
    }
    deriving (Eq, Show)

instance Aeson.FromJSON GatewayModelCatalogResponse where
    parseJSON =
        Aeson.withObject "GatewayModelCatalogResponse" \object ->
            GatewayModelCatalogResponse
                . normalizeGatewayModels
                <$> object .: "data"

instance Aeson.FromJSON GatewayModel where
    parseJSON =
        Aeson.withObject "GatewayModel" \object ->
            GatewayModel
                <$> object .: "id"
                <*> object .: "protocol"

instance Aeson.FromJSON GatewayModelProtocol where
    parseJSON =
        Aeson.withText "GatewayModelProtocol" \case
            "responses" -> pure GatewayResponsesProtocol
            "anthropic" -> pure GatewayAnthropicProtocol
            _ -> fail "Gateway model protocol is invalid."

-- | A gateway-scoped model catalog and its most recently successful refresh.
--
-- The constructor is deliberately hidden: callers can list models but cannot
-- accidentally inspect or log the credential captured by its fetch action.
data GatewayModelAccess = GatewayModelAccess
    { gatewayModelFetch :: !(IO (Either Text [GatewayModel]))
    , gatewayUsageFetch :: !(Text -> IO (Either Text UsageSnapshot))
    , gatewayModelCache :: !(IORef (Maybe [GatewayModel]))
    , gatewayModelRefreshLock :: !(MVar ())
    , gatewayDictation
        :: !(((BS.ByteString -> IO ()) -> IO ())
            -> (Text -> IO ())
            -> IO (Either Text Text))
    }

-- | The hosted gateway selected by the interactive @/login@ flow.
defaultGatewayBaseUrl :: Text
defaultGatewayBaseUrl = "https://platform.digitallyinduced.com"

-- | Public OAuth client registered for the terminal application's loopback
-- Authorization Code + PKCE flow.
gatewayBrowserClientId :: Text
gatewayBrowserClientId = "haskell-agent-cli"

gatewayBrowserRedirectPath :: Text
gatewayBrowserRedirectPath = "/oauth2callback"

-- | A validated gateway base URL paired with its short-lived device flow.
--
-- Keeping the normalized base URL in the value prevents UI callers from
-- accidentally saving a different origin from the one that issued the code.
data GatewayAuthorization = GatewayAuthorization
    { authorizationBaseUrl :: !Text
    , authorizationDevice :: !GatewayDeviceAuthorization
    }
    deriving (Eq)

instance Show GatewayAuthorization where
    show authorization =
        "GatewayAuthorization { authorizationBaseUrl = "
            <> show authorization.authorizationBaseUrl
            <> ", authorizationDevice = <redacted> }"

instance Show GatewayCredential where
    show credential =
        "GatewayCredential { gatewayBaseUrl = "
            <> show credential.gatewayBaseUrl
            <> ", gatewayWebSocketUrl = "
            <> show credential.gatewayWebSocketUrl
            <> ", gatewayAccessToken = <redacted> }"

instance Aeson.ToJSON GatewayCredential where
    toJSON credential =
        Aeson.object
            [ "version" .= (1 :: Int)
            , "base_url" .= credential.gatewayBaseUrl
            , "websocket_url" .= credential.gatewayWebSocketUrl
            , "access_token" .= credential.gatewayAccessToken
            ]

data GatewayAuthorizationCodeResponse = GatewayAuthorizationCodeResponse
    { authorizationAccessToken :: !Text
    , authorizationTokenType :: !Text
    , authorizationResponseBaseUrl :: !Text
    , authorizationWebSocketUrl :: !Text
    }
    deriving (Eq)

instance Show GatewayAuthorizationCodeResponse where
    show response =
        "GatewayAuthorizationCodeResponse"
            <> " { authorizationAccessToken = <redacted>"
            <> ", authorizationTokenType = "
            <> show response.authorizationTokenType
            <> ", authorizationResponseBaseUrl = "
            <> show response.authorizationResponseBaseUrl
            <> ", authorizationWebSocketUrl = <redacted> }"

gatewayAuthorizationCodeDecoder
    :: Hermes.Decoder GatewayAuthorizationCodeResponse
gatewayAuthorizationCodeDecoder =
    Hermes.object $
        GatewayAuthorizationCodeResponse
            <$> Hermes.atKey "access_token" Hermes.text
            <*> Hermes.atKey "token_type" Hermes.text
            <*> Hermes.atKey "base_url" Hermes.text
            <*> Hermes.atKey "websocket_url" Hermes.text

gatewayCredentialDecoder :: Hermes.Decoder GatewayCredential
gatewayCredentialDecoder =
    Hermes.object $
        GatewayCredential
            <$> Hermes.atKey "base_url" Hermes.text
            <*> Hermes.atKey "websocket_url" Hermes.text
            <*> Hermes.atKey "access_token" Hermes.text

-- | Construct a cached model-list handle for a validated gateway credential.
newGatewayModelAccess :: GatewayCredential -> IO GatewayModelAccess
newGatewayModelAccess credential =
    newGatewayModelAccessWithActions
        (fetchGatewayModels credential)
        (fetchGatewayUsageWithCredential credential)
        (transcribeGatewayPcmWith credential)

-- | Injectable constructor used by tests and alternative trusted transports.
-- The resulting value remains opaque, so the fetch action cannot be read back
-- or accidentally included in diagnostics.
newGatewayModelAccessWith
    :: IO (Either Text [GatewayModel])
    -> IO GatewayModelAccess
newGatewayModelAccessWith fetch =
    newGatewayModelAccessWithUsage
        fetch
        unavailableGatewayUsage

-- | Injectable usage transport used by tests and trusted alternative
-- gateways. Model aliases are passed through exactly and the decoded snapshot
-- uses the same type as a direct OpenAI connection.
newGatewayModelAccessWithUsage
    :: IO (Either Text [GatewayModel])
    -> (Text -> IO (Either Text UsageSnapshot))
    -> IO GatewayModelAccess
newGatewayModelAccessWithUsage fetch usage =
    newGatewayModelAccessWithActions
        fetch
        usage
        unavailableGatewayDictation

-- | Injectable constructor for tests and trusted alternative gateway
-- transports. The action stays opaque with the credential-bearing model
-- access handle.
newGatewayModelAccessWithDictation
    :: IO (Either Text [GatewayModel])
    -> (((BS.ByteString -> IO ()) -> IO ())
        -> (Text -> IO ())
        -> IO (Either Text Text))
    -> IO GatewayModelAccess
newGatewayModelAccessWithDictation fetch dictation =
    newGatewayModelAccessWithActions
        fetch
        unavailableGatewayUsage
        dictation

newGatewayModelAccessWithActions
    :: IO (Either Text [GatewayModel])
    -> (Text -> IO (Either Text UsageSnapshot))
    -> (((BS.ByteString -> IO ()) -> IO ())
        -> (Text -> IO ())
        -> IO (Either Text Text))
    -> IO GatewayModelAccess
newGatewayModelAccessWithActions fetch usage dictation = do
    cache <- newIORef Nothing
    refreshLock <- newMVar ()
    pure GatewayModelAccess
        { gatewayModelFetch = fetch
        , gatewayUsageFetch = usage
        , gatewayModelCache = cache
        , gatewayModelRefreshLock = refreshLock
        , gatewayDictation = dictation
        }

unavailableGatewayUsage
    :: Text
    -> IO (Either Text UsageSnapshot)
unavailableGatewayUsage _ =
    pure $
        Left "Usage is not available through this gateway connection."

unavailableGatewayDictation
    :: ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
unavailableGatewayDictation _ _ =
    pure $
        Left "Dictation is not available through this gateway connection."

-- | Fetch usage through the transport captured by this gateway connection.
fetchGatewayUsage
    :: GatewayModelAccess
    -> Text
    -> IO (Either Text UsageSnapshot)
fetchGatewayUsage access model
    | Text.null model =
        pure (Left "Gateway usage requires a model alias.")
    | otherwise =
        tryAny (access.gatewayUsageFetch model) >>= \case
            Left _ ->
                pure (Left "Could not refresh organization gateway usage.")
            Right result -> pure result

fetchGatewayUsageWithCredential
    :: GatewayCredential
    -> Text
    -> IO (Either Text UsageSnapshot)
fetchGatewayUsageWithCredential admitted model =
    withGatewayCredentialLease do
        loadGatewayCredential >>= \case
            Right (Just current)
                | current == admitted ->
                    requestGatewayUsage current model
            _ ->
                pure $
                    Left
                        "The organization gateway changed before usage was refreshed."

requestGatewayUsage
    :: GatewayCredential
    -> Text
    -> IO (Either Text UsageSnapshot)
requestGatewayUsage credential model =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "Gateway credential is invalid.")
        Right () -> do
            let query =
                    TextEncoding.decodeUtf8
                        . LBS.toStrict
                        . Builder.toLazyByteString
                        $ renderQueryText True [("model", Just model)]
                endpoint =
                    Text.dropWhileEnd (== '/')
                        (Text.strip credential.gatewayBaseUrl)
                        <> "/backend-api/wham/usage"
                        <> query
            outcome <- tryAny do
                manager <- newTlsManager
                initial <- HTTP.parseRequest (Text.unpack endpoint)
                let request =
                        initial
                            { HTTP.method = "GET"
                            , HTTP.requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            credential.gatewayAccessToken
                                  )
                                , (hAccept, "application/json")
                                ]
                            , HTTP.checkResponse = \_ _ -> pure ()
                            -- Never forward the gateway bearer to a redirect.
                            , HTTP.redirectCount = 0
                            , HTTP.responseTimeout =
                                HTTP.responseTimeoutMicro (5 * 1_000_000)
                            }
                HTTP.withResponse request manager \response -> do
                    body <-
                        readBoundedBody
                            gatewayMaxResponseBytes
                            (HTTP.responseBody response)
                    pure (HTTP.responseStatus response, body)
            pure case outcome of
                Left _ ->
                    Left "Could not reach the gateway usage endpoint."
                Right (_status, Nothing) ->
                    Left "Gateway usage returned an oversized response."
                Right (status, Just body)
                    | statusIsSuccessful status ->
                        case decodeUsageResponse (LBS.fromStrict body) of
                            Left _ ->
                                Left
                                    "Gateway returned an unreadable usage response."
                            Right snapshot -> Right snapshot
                    | statusCode status == 404 ->
                        Left
                            "Usage is not supported by this organization gateway."
                    | otherwise ->
                        Left $
                            "Gateway usage returned HTTP "
                                <> Text.pack (show (statusCode status))

-- | Record PCM through the opaque, gateway-bound dictation action.
transcribeGatewayPcm
    :: GatewayModelAccess
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
transcribeGatewayPcm access = access.gatewayDictation

transcribeGatewayPcmWith
    :: GatewayCredential
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
transcribeGatewayPcmWith admitted produceAudio onTranscript =
    withGatewayCredentialTurnLease do
        loadGatewayCredential >>= \case
            Right (Just current)
                | current == admitted ->
                    case gatewayDictationWebSocketUrl current of
                        Left err -> pure (Left err)
                        Right websocketUrl -> do
                            chunks <- newIORef []
                            capturedBytes <- newIORef 0
                            let captureAndBuffer sendAudio =
                                    produceAudio \chunk ->
                                        unless (BS.null chunk) do
                                            total <-
                                                (+ BS.length chunk)
                                                    <$> readIORef capturedBytes
                                            when
                                                (total > gatewayMaxPcmBytes)
                                                (throwString
                                                    "gateway dictation audio exceeded the client limit")
                                            writeIORef capturedBytes total
                                            modifyIORef' chunks (chunk :)
                                            sendAudio chunk
                            transcribePcmWithChatGPTStreamAt
                                websocketUrl
                                [ ( "Authorization"
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            current.gatewayAccessToken
                                  )
                                ]
                                captureAndBuffer
                                onTranscript >>= \case
                                    Left ChatGPTDictationCaptureFailed{} ->
                                        pure $
                                            Left
                                                "Gateway dictation audio capture failed."
                                    Left ChatGPTDictationStreamUnavailable{} -> do
                                        pcm <-
                                            BS.concat . reverse
                                                <$> readIORef chunks
                                        wavResult <-
                                            if BS.null pcm
                                                then
                                                    captureGatewayWav
                                                        produceAudio
                                                else
                                                    pure $
                                                        case encodePcm16Wav
                                                            openAITranscriptionSampleRate
                                                            pcm of
                                                                Left _ ->
                                                                    Left
                                                                        "Gateway dictation streaming failed."
                                                                Right wav ->
                                                                    Right wav
                                        case wavResult of
                                            Left err -> pure (Left err)
                                            Right wav ->
                                                loadGatewayCredential >>= \case
                                                    Right (Just latest)
                                                        | latest == current ->
                                                            postGatewayTranscription
                                                                latest
                                                                wav >>= \case
                                                                    Left err ->
                                                                        pure
                                                                            (Left
                                                                                err)
                                                                    Right transcript -> do
                                                                        onTranscript
                                                                            transcript
                                                                        pure
                                                                            (Right
                                                                                transcript)
                                                    _ ->
                                                        pure $
                                                            Left
                                                                "The organization gateway changed during dictation."
                                    Right transcript ->
                                        pure (Right transcript)
            _ ->
                pure $
                    Left
                        "The organization gateway changed before dictation started."

gatewayDictationWebSocketUrl
    :: GatewayCredential
    -> Either Text Text
gatewayDictationWebSocketUrl credential = do
    uri <-
        maybe
            (Left "Gateway WebSocket URL is invalid.")
            Right
            (URI.parseURI (Text.unpack credential.gatewayWebSocketUrl))
    pure $
        Text.pack $
            show
                uri
                    { URI.uriPath = "/v1/audio/transcriptions"
                    , URI.uriQuery = ""
                    , URI.uriFragment = ""
                    }

captureGatewayWav
    :: ((BS.ByteString -> IO ()) -> IO ())
    -> IO (Either Text LBS.ByteString)
captureGatewayWav produceAudio =
    tryAny capture >>= \case
        Left _ ->
            pure (Left "Gateway dictation audio capture failed.")
        Right result -> pure result
  where
    capture = do
        chunks <- newIORef []
        capturedBytes <- newIORef 0
        produceAudio \chunk ->
            unless (BS.null chunk) do
                total <- (+ BS.length chunk) <$> readIORef capturedBytes
                when (total > gatewayMaxPcmBytes) $
                    throwString
                        "gateway dictation audio exceeded the client limit"
                writeIORef capturedBytes total
                modifyIORef' chunks (chunk :)
        pcm <- BS.concat . reverse <$> readIORef chunks
        pure $
            case encodePcm16Wav openAITranscriptionSampleRate pcm of
                Left _ ->
                    Left "Gateway dictation captured invalid audio."
                Right wav -> Right wav

gatewayMaxPcmBytes :: Int
gatewayMaxPcmBytes = 4 * 1024 * 1024 - 4096

postGatewayTranscription
    :: GatewayCredential
    -> LBS.ByteString
    -> IO (Either Text Text)
postGatewayTranscription credential wav =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "Gateway credential is invalid.")
        Right () -> do
            boundary <- gatewayTranscriptionBoundary
            let body = gatewayTranscriptionBody boundary wav
                endpoint =
                    Text.dropWhileEnd (== '/')
                        (Text.strip credential.gatewayBaseUrl)
                        <> "/v1/audio/transcriptions"
            outcome <- tryAny do
                manager <- newTlsManager
                initial <- HTTP.parseRequest (Text.unpack endpoint)
                let request =
                        initial
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            credential.gatewayAccessToken
                                  )
                                , ( hContentType
                                  , "multipart/form-data; boundary=" <> boundary
                                  )
                                , (hAccept, "application/json")
                                ]
                            , HTTP.requestBody = HTTP.RequestBodyLBS body
                            , HTTP.checkResponse = \_ _ -> pure ()
                            -- Never forward the gateway bearer to a redirect.
                            , HTTP.redirectCount = 0
                            , HTTP.responseTimeout =
                                HTTP.responseTimeoutMicro
                                    (2 * 60 * 1_000_000)
                            }
                HTTP.withResponse request manager \response -> do
                    responseBody <-
                        readBoundedBody
                            gatewayMaxResponseBytes
                            (HTTP.responseBody response)
                    pure
                        ( HTTP.responseStatus response
                        , responseBody
                        )
            pure case outcome of
                Left _ ->
                    Left "Could not reach the organization gateway for dictation."
                Right (_status, Nothing) ->
                    Left "Gateway dictation returned an oversized response."
                Right (status, Just responseBody)
                    | statusIsSuccessful status ->
                        decodeGatewayTranscript responseBody
                    | statusCode status == 404 ->
                        Left
                            "Dictation is not supported by this organization gateway."
                    | otherwise ->
                        Left $
                            "Gateway dictation returned HTTP "
                                <> Text.pack (show (statusCode status))

gatewayMaxResponseBytes :: Int
gatewayMaxResponseBytes = 64 * 1024

readBoundedBody
    :: Int
    -> HTTP.BodyReader
    -> IO (Maybe BS.ByteString)
readBoundedBody limit = go 0 []
  where
    go total chunks reader = do
        chunk <- HTTP.brRead reader
        if BS.null chunk
            then pure (Just (BS.concat (reverse chunks)))
            else
                let next = total + BS.length chunk
                in if next > limit
                    then pure Nothing
                    else go next (chunk : chunks) reader

gatewayTranscriptionBoundary :: IO BS.ByteString
gatewayTranscriptionBoundary =
    ("----haskell-agent-gateway-" <>)
        . Base64Url.encodeUnpadded
        <$> getEntropy 18

gatewayTranscriptionBody
    :: BS.ByteString
    -> LBS.ByteString
    -> LBS.ByteString
gatewayTranscriptionBody boundary wav =
    LBS.fromStrict
        ( "--" <> boundary <> "\r\n"
        <> "Content-Disposition: form-data; name=\"model\"\r\n\r\n"
        <> "dictation\r\n"
        <> "--" <> boundary <> "\r\n"
        <> "Content-Disposition: form-data; name=\"file\"; filename=\"audio.wav\"\r\n"
        <> "Content-Type: audio/wav\r\n\r\n"
        )
        <> wav
        <> LBS.fromStrict
            ("\r\n--" <> boundary <> "--\r\n")

newtype GatewayTranscript =
    GatewayTranscript { gatewayTranscriptText :: Text }

instance Aeson.FromJSON GatewayTranscript where
    parseJSON =
        Aeson.withObject "GatewayTranscript" \object ->
            GatewayTranscript <$> object .: "text"

decodeGatewayTranscript :: BS.ByteString -> Either Text Text
decodeGatewayTranscript body =
    case
        Aeson.eitherDecodeStrict' body
            :: Either String GatewayTranscript
        of
        Left _ ->
            Left "Gateway dictation returned an unreadable response."
        Right response
            | Text.null transcript ->
                Left "Gateway dictation returned an empty transcript."
            | otherwise -> Right transcript
          where
            transcript =
                Text.strip response.gatewayTranscriptText

-- | Refresh the gateway's authorized model aliases.
--
-- A failed refresh deliberately clears the previous value.  Continuing to
-- show a stale authorization list would let an organization revocation look
-- like an available model.
refreshGatewayModels
    :: GatewayModelAccess
    -> IO (Either Text [GatewayModel])
refreshGatewayModels
        GatewayModelAccess
            { gatewayModelFetch
            , gatewayModelCache
            , gatewayModelRefreshLock
            } =
    withMVar gatewayModelRefreshLock \_ ->
        tryAny gatewayModelFetch >>= \case
            Left _ -> do
                writeIORef gatewayModelCache Nothing
                pure (Left "Could not refresh organization gateway models.")
            Right result ->
                case result of
                    Left err -> do
                        writeIORef gatewayModelCache Nothing
                        pure (Left err)
                    Right models -> do
                        let normalized = normalizeGatewayModels models
                        writeIORef gatewayModelCache (Just normalized)
                        pure (Right normalized)

-- | Read the most recent successful gateway refresh without issuing I/O.
cachedGatewayModels :: GatewayModelAccess -> IO (Maybe [GatewayModel])
cachedGatewayModels GatewayModelAccess { gatewayModelCache } =
    readIORef gatewayModelCache

-- | Fetch the aliases currently offered to this gateway credential.
--
-- Errors deliberately omit exception and response-body detail: both may
-- contain external content, while request headers contain the bearer token.
fetchGatewayModels :: GatewayCredential -> IO (Either Text [GatewayModel])
fetchGatewayModels credential =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "Gateway credential is invalid.")
        Right () -> do
            response <- tryAny do
                manager <- newTlsManager
                initial <-
                    HTTP.parseRequest
                        (Text.unpack
                            (Text.dropWhileEnd (== '/')
                                (Text.strip credential.gatewayBaseUrl)
                                <> "/v1/models"))
                HTTP.httpLbs
                    initial
                        { HTTP.method = "GET"
                        , HTTP.requestHeaders =
                            [ ( hAuthorization
                              , "Bearer "
                                    <> TextEncoding.encodeUtf8
                                        credential.gatewayAccessToken
                              )
                            , (hAccept, "application/json")
                            ]
                        , HTTP.checkResponse = \_ _ -> pure ()
                        -- Never follow a redirect with the gateway bearer.
                        , HTTP.redirectCount = 0
                        , HTTP.responseTimeout =
                            HTTP.responseTimeoutMicro (5 * 1_000_000)
                        }
                    manager
            pure case response of
                Left _ ->
                    Left "Could not reach the gateway models endpoint."
                Right value
                    | statusIsSuccessful (HTTP.responseStatus value) ->
                        case
                            Aeson.eitherDecodeStrict'
                                (LBS.toStrict (HTTP.responseBody value))
                                :: Either String GatewayModelCatalogResponse
                            of
                            Left _ ->
                                Left
                                    "Gateway returned an unreadable models response."
                            Right catalog
                                | null catalog.gatewayModelCatalogData ->
                                    Left "Gateway returned an empty model catalog."
                                | otherwise ->
                                    Right catalog.gatewayModelCatalogData
                    | otherwise ->
                        Left $
                            "Gateway models returned HTTP "
                                <> Text.pack
                                    (show
                                        (statusCode
                                            (HTTP.responseStatus value)))

gatewayModelIds :: [GatewayModel] -> [Text]
gatewayModelIds = fmap (.gatewayModelId)

normalizeGatewayModels :: [GatewayModel] -> [GatewayModel]
normalizeGatewayModels = go Set.empty
  where
    go _ [] = []
    go seen (model : rest)
        | Text.null modelId = go seen rest
        | Text.any (\char -> isSpace char || not (isPrint char)) modelId =
            go seen rest
        | modelId `Set.member` seen = go seen rest
        | otherwise =
            model { gatewayModelId = modelId }
                : go (Set.insert modelId seen) rest
      where
        modelId = Text.strip model.gatewayModelId

data GatewayDeviceAuthorization = GatewayDeviceAuthorization
    { deviceCode :: !Text
    , userCode :: !Text
    , verificationUri :: !Text
    , verificationUriComplete :: !Text
    , expiresInSeconds :: !Int
    , pollIntervalSeconds :: !Int
    }
    deriving (Eq)

instance Show GatewayDeviceAuthorization where
    show device =
        "GatewayDeviceAuthorization { deviceCode = <redacted>, userCode = "
            <> show device.userCode
            <> ", verificationUri = "
            <> show device.verificationUri
            <> ", verificationUriComplete = "
            <> show device.verificationUriComplete
            <> ", expiresInSeconds = "
            <> show device.expiresInSeconds
            <> ", pollIntervalSeconds = "
            <> show device.pollIntervalSeconds
            <> " }"

gatewayDeviceDecoder :: Hermes.Decoder GatewayDeviceAuthorization
gatewayDeviceDecoder =
    Hermes.object $
        GatewayDeviceAuthorization
            <$> Hermes.atKey "device_code" Hermes.text
            <*> Hermes.atKey "user_code" Hermes.text
            <*> Hermes.atKey "verification_uri" Hermes.text
            <*> Hermes.atKey "verification_uri_complete" Hermes.text
            <*> Hermes.atKey "expires_in" Hermes.int
            <*> Hermes.atKey "interval" Hermes.int

-- | Ensure a device response cannot make a native or terminal client open an
-- unrelated origin.
validateGatewayDeviceAuthorization
    :: Text
    -> GatewayDeviceAuthorization
    -> Either Text GatewayDeviceAuthorization
validateGatewayDeviceAuthorization rawBaseUrl authorization = do
    baseUrl <- validateBaseUrl rawBaseUrl
    whenEither
        (Text.null (Text.strip authorization.deviceCode))
        "The gateway returned an empty device code."
    whenEither
        (Text.null (Text.strip authorization.userCode))
        "The gateway returned an empty user code."
    whenEither
        (authorization.expiresInSeconds <= 0)
        "The gateway returned an invalid authorization expiry."
    whenEither
        (authorization.pollIntervalSeconds <= 0)
        "The gateway returned an invalid polling interval."
    gatewayOrigin <-
        parseGatewayOrigin
            "The gateway URL is invalid."
            baseUrl
    verificationOrigin <-
        parseGatewayResourceOrigin
            "The gateway returned an invalid verification URL."
            authorization.verificationUri
    completeOrigin <-
        parseGatewayResourceOrigin
            "The gateway returned an invalid complete verification URL."
            authorization.verificationUriComplete
    whenEither
        (verificationOrigin /= gatewayOrigin
            || completeOrigin /= gatewayOrigin)
        "The gateway returned a verification URL for a different origin."
    pure authorization

data GatewayPollResult
    = GatewayAuthorized !Text !Text
    | GatewayAuthorizationPending !(Maybe Int)
    | GatewaySlowDown !(Maybe Int)
    | GatewayAccessDenied
    | GatewayExpired
    | GatewayPollFailed !Text
    deriving (Eq)

instance Show GatewayPollResult where
    show result = case result of
        GatewayAuthorized _ websocketUrl ->
            "GatewayAuthorized <redacted> " <> show websocketUrl
        GatewayAuthorizationPending interval ->
            "GatewayAuthorizationPending " <> show interval
        GatewaySlowDown interval ->
            "GatewaySlowDown " <> show interval
        GatewayAccessDenied -> "GatewayAccessDenied"
        GatewayExpired -> "GatewayExpired"
        GatewayPollFailed code ->
            "GatewayPollFailed " <> show code

gatewayPollDecoder :: Hermes.Decoder GatewayPollResult
gatewayPollDecoder =
    Hermes.withOwnedRawJson \raw ->
        case Hermes.decodeEither successDecoder raw of
            Right (token, websocketUrl) ->
                pure (GatewayAuthorized token websocketUrl)
            Left _ -> case Hermes.decodeEither errorDecoder raw of
                Right (code, interval) -> pure case code of
                    "authorization_pending" -> GatewayAuthorizationPending interval
                    "slow_down" -> GatewaySlowDown interval
                    "access_denied" -> GatewayAccessDenied
                    "expired_token" -> GatewayExpired
                    other -> GatewayPollFailed other
                Left err -> fail (Text.unpack (Hermes.jsonErrorMessage err))
  where
    successDecoder =
        Hermes.object $
            (,)
                <$> Hermes.atKey "access_token" Hermes.text
                <*> Hermes.atKey "websocket_url" Hermes.text
    errorDecoder =
        Hermes.object $
            (,)
                <$> Hermes.atKey "error" Hermes.text
                <*> Hermes.optionalKey "interval" Hermes.int

gatewayCredentialPath :: OsPath -> OsPath
gatewayCredentialPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "credentials"
        </> unsafeEncodeUtf "gateway.json"

gatewayCredentialLockPath :: OsPath -> OsPath
gatewayCredentialLockPath home =
    takeDirectory (gatewayCredentialPath home)
        </> unsafeEncodeUtf "gateway.lock"

gatewayCredentialTurnAdmissionPath :: OsPath -> OsPath
gatewayCredentialTurnAdmissionPath home =
    takeDirectory (gatewayCredentialPath home)
        </> unsafeEncodeUtf "gateway-turn-admission.lock"

gatewayCredentialTurnLeasePath :: OsPath -> OsPath
gatewayCredentialTurnLeasePath home =
    takeDirectory (gatewayCredentialPath home)
        </> unsafeEncodeUtf "gateway-turn.lock"

-- | Serialize gateway credential changes with operations whose authorization
-- depends on one exact credential snapshot. The process lock is required
-- because advisory file-lock behavior between threads in one process is
-- platform-dependent; the private file lock extends the boundary to CLI and
-- native application processes.
withGatewayCredentialLock :: IO value -> IO value
withGatewayCredentialLock action = do
    home <- Directory.getHomeDirectory
    withGatewayCredentialLockAt home action

withGatewayCredentialLockAt :: OsPath -> IO value -> IO value
withGatewayCredentialLockAt home action =
    withGatewayCredentialProcessWriteLock $
        withPrivateFileLock (gatewayCredentialTurnAdmissionPath home) $
            withPrivateFileLock (gatewayCredentialTurnLeasePath home) $
                withPrivateFileLock
                    (gatewayCredentialLockPath home)
                    action

-- | Hold a shared credential lease. Credential changes wait for every lease,
-- while independent session reads and native turns remain concurrent.
withGatewayCredentialLease :: IO value -> IO value
withGatewayCredentialLease action = do
    home <- Directory.getHomeDirectory
    withGatewayCredentialLeaseAt home action

withGatewayCredentialLeaseAt :: OsPath -> IO value -> IO value
withGatewayCredentialLeaseAt home action =
    withGatewayCredentialProcessReadLock True \needsFileLock ->
        if needsFileLock
            then
                withPrivateSharedFileLock
                    (gatewayCredentialLockPath home)
                    action
            else action

-- | Start a long-running native turn only if no credential writer is already
-- waiting. Unlike short callback leases, a new turn must not prolong an
-- organization transition by joining an existing reader phase.
withGatewayCredentialTurnLease :: IO value -> IO value
withGatewayCredentialTurnLease action = do
    home <- Directory.getHomeDirectory
    withGatewayCredentialTurnLeaseAt home action

withGatewayCredentialTurnLeaseAt :: OsPath -> IO value -> IO value
withGatewayCredentialTurnLeaseAt home action =
    withGatewayCredentialProcessReadLock False \_ ->
        withPrivateSharedFileLocksAfterGate
            (gatewayCredentialTurnAdmissionPath home)
            [ gatewayCredentialTurnLeasePath home
            -- Keep the original credential lock for rolling compatibility
            -- with an older process that does not know the admission protocol.
            , gatewayCredentialLockPath home
            ]
            action

data GatewayCredentialProcessLockState =
    GatewayCredentialProcessLockState
        { processLockReaders :: !Int
        , processLockWriterOwner :: !(Maybe ThreadId)
        , processLockWaitingWriters :: !Int
        }

gatewayCredentialProcessLock :: TVar GatewayCredentialProcessLockState
gatewayCredentialProcessLock =
    unsafePerformIO $
        newTVarIO
            GatewayCredentialProcessLockState
                { processLockReaders = 0
                , processLockWriterOwner = Nothing
                , processLockWaitingWriters = 0
                }
{-# NOINLINE gatewayCredentialProcessLock #-}

withGatewayCredentialProcessReadLock
    :: Bool
    -> (Bool -> IO value)
    -> IO value
withGatewayCredentialProcessReadLock mayJoinReaderPhase action =
    mask \restore -> do
        thread <- myThreadId
        needsFileLock <- atomically do
            state <- readTVar gatewayCredentialProcessLock
            case state.processLockWriterOwner of
                Just owner
                    | mayJoinReaderPhase && owner == thread ->
                        -- A terminal credential callback may synchronously
                        -- query the new state. The writer already owns the
                        -- process and file boundaries on this same thread.
                        pure False
                    | otherwise -> retry
                Nothing
                    -- Short readers may join an active reader phase. This
                    -- matters for native supervisors: a long-running turn can
                    -- need a boundary-checked approval or snapshot callback
                    -- before it can finish and release its lifetime lease.
                    -- Long turn leases pass False and wait behind the writer;
                    -- once the last reader leaves, every reader must wait.
                    | state.processLockWaitingWriters > 0
                            && ( not mayJoinReaderPhase
                                    || state.processLockReaders == 0
                               )
                        -> retry
                    | otherwise -> do
                        writeTVar
                            gatewayCredentialProcessLock
                            state
                                { processLockReaders =
                                    state.processLockReaders + 1
                                }
                        pure True
        if not needsFileLock
            then restore (action False)
            else restore (action True)
                `finally`
                    atomically do
                        state <- readTVar gatewayCredentialProcessLock
                        writeTVar
                            gatewayCredentialProcessLock
                            state
                                { processLockReaders =
                                    max 0 (state.processLockReaders - 1)
                                }

withGatewayCredentialProcessWriteLock :: IO value -> IO value
withGatewayCredentialProcessWriteLock action =
    mask \restore -> do
        thread <- myThreadId
        reentrant <- atomically do
            state <- readTVar gatewayCredentialProcessLock
            case state.processLockWriterOwner of
                Just owner
                    | owner == thread -> pure True
                _ -> do
                    writeTVar
                        gatewayCredentialProcessLock
                        state
                            { processLockWaitingWriters =
                                state.processLockWaitingWriters + 1
                            }
                    pure False
        if reentrant
            then
                throwString
                    "A gateway credential transition is already in progress."
            else do
                let unregisterWaitingWriter =
                        atomically do
                            state <- readTVar gatewayCredentialProcessLock
                            writeTVar
                                gatewayCredentialProcessLock
                                state
                                    { processLockWaitingWriters =
                                        max 0
                                            (state.processLockWaitingWriters - 1)
                                    }
                -- Keep the successful handoff masked. A blocked STM
                -- transaction is still interruptible, so cancellation can
                -- unregister the waiter; once it commits, no async exception
                -- can land before the finalizer is installed below.
                (atomically do
                        state <- readTVar gatewayCredentialProcessLock
                        if
                            state.processLockWriterOwner /= Nothing
                                || state.processLockReaders > 0
                        then retry
                        else
                            writeTVar
                                gatewayCredentialProcessLock
                                state
                                    { processLockWriterOwner = Just thread
                                    , processLockWaitingWriters =
                                        max 0
                                            (state.processLockWaitingWriters - 1)
                                    })
                    `onException` unregisterWaitingWriter
                restore action
                    `finally`
                        atomically do
                            state <- readTVar gatewayCredentialProcessLock
                            writeTVar
                                gatewayCredentialProcessLock
                                state { processLockWriterOwner = Nothing }

loadGatewayCredential :: IO (Either Text (Maybe GatewayCredential))
loadGatewayCredential = do
    home <- Directory.getHomeDirectory
    loadGatewayCredentialAt home

loadGatewayCredentialAt
    :: OsPath
    -> IO (Either Text (Maybe GatewayCredential))
loadGatewayCredentialAt home = do
    let path = gatewayCredentialPath home
    exists <- Directory.doesFileExist path
    if not exists
        then pure (Right Nothing)
        else do
            result <-
                retryOnFileBusy $
                    tryAny (LBS.readFile (unsafeToFilePath path))
            pure case result of
                Left exception -> Left (Text.pack (show exception))
                Right bytes ->
                    case Hermes.decodeEither gatewayCredentialDecoder (LBS.toStrict bytes) of
                        Left err -> Left (Hermes.jsonErrorMessage err)
                        Right credential ->
                            case validateGatewayCredential credential of
                                Left err -> Left err
                                Right () -> Right (Just credential)

saveGatewayCredentialAt :: OsPath -> GatewayCredential -> IO (Either Text ())
saveGatewayCredentialAt home credential =
    fmap (fmap (const ())) $
        saveGatewayCredentialAtWith home credential (pure ())

saveGatewayCredentialAtWith
    :: OsPath
    -> GatewayCredential
    -> IO value
    -> IO (Either Text value)
saveGatewayCredentialAtWith home credential afterSave =
    case validateGatewayCredential credential of
        Left err -> pure (Left err)
        Right () -> do
            result <- tryAny $
                withGatewayCredentialLockAt home do
                    let path = gatewayCredentialPath home
                        directory = takeDirectory path
                    Directory.createDirectoryIfMissing True directory
                    setFileMode (unsafeToFilePath directory) 0o700
                    writeLazyFileAtomically
                        path 0o600 (Aeson.encode credential)
                    afterSave
            pure case result of
                Left exception -> Left (Text.pack (show exception))
                Right value -> Right value

validateGatewayCredential :: GatewayCredential -> Either Text ()
validateGatewayCredential credential = do
    baseUrl <- validateBaseUrl credential.gatewayBaseUrl
    validateGatewayWebSocketUrl credential.gatewayWebSocketUrl
    baseOrigin <-
        parseGatewayOrigin
            "Gateway URL is invalid."
            baseUrl
    websocketOrigin <-
        parseGatewayOrigin
            "Gateway WebSocket URL is invalid."
            credential.gatewayWebSocketUrl
    let expectedWebSocketOrigin =
            case baseOrigin of
                ("https:", host, port) -> ("wss:", host, port)
                ("http:", host, port) -> ("ws:", host, port)
                origin -> origin
    whenEither
        (websocketOrigin /= expectedWebSocketOrigin)
        "Gateway base and WebSocket URLs must use the same origin."
    whenEither
        (Text.null (Text.strip credential.gatewayAccessToken))
        "Gateway access token cannot be empty."
    baseOrigin <-
        parseGatewayOrigin
            "Gateway credential contains an invalid base URL."
            baseUrl
    websocketOrigin <-
        parseGatewayOrigin
            "Gateway credential contains an invalid WebSocket URL."
            credential.gatewayWebSocketUrl
    let expectedWebSocketOrigin =
            case baseOrigin of
                ("https:", host, port) -> ("wss:", host, port)
                ("http:", host, port) -> ("ws:", host, port)
                origin -> origin
    whenEither
        (websocketOrigin /= expectedWebSocketOrigin)
        "Gateway credential WebSocket URL uses a different origin."

saveGatewayCredential :: GatewayCredential -> IO (Either Text ())
saveGatewayCredential credential =
    fmap (fmap (const ())) $
        saveGatewayCredentialWith credential (pure ())

saveGatewayCredentialWith
    :: GatewayCredential
    -> IO value
    -> IO (Either Text value)
saveGatewayCredentialWith credential afterSave =
    tryAny Directory.getHomeDirectory >>= \case
        Left exception -> pure (Left (Text.pack (show exception)))
        Right home ->
            saveGatewayCredentialAtWith home credential afterSave

startGatewayAuthorization
    :: Text
    -> IO (Either Text GatewayAuthorization)
startGatewayAuthorization rawBaseUrl =
    startGatewayAuthorizationWithClient rawBaseUrl "haskell-agent"

-- | Start the device flow used by native clients without opening a browser or
-- beginning a poll loop.
startNativeGatewayAuthorization
    :: Text
    -> Text
    -> IO (Either Text GatewayDeviceAuthorization)
startNativeGatewayAuthorization rawBaseUrl rawClientName =
    fmap (fmap (.authorizationDevice)) $
        startGatewayAuthorizationWithClient rawBaseUrl rawClientName

startGatewayAuthorizationWithClient
    :: Text
    -> Text
    -> IO (Either Text GatewayAuthorization)
startGatewayAuthorizationWithClient rawBaseUrl rawClientName =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl
            | Text.null clientName || Text.length clientName > 160 ->
                pure
                    (Left
                        "Gateway client name must contain between 1 and 160 characters.")
            | otherwise -> do
                tryAny newTlsManager >>= \case
                    Left exception ->
                        pure (Left (Text.pack (show exception)))
                    Right manager -> do
                        result <-
                            postJson manager
                                (baseUrl
                                    <> "/api/v1/agent-connections/device")
                                (Aeson.object
                                    [ "client_name"
                                        .= clientName
                                    ])
                                gatewayDeviceDecoder
                        pure do
                            device <- result
                            GatewayAuthorization baseUrl
                                <$> validateGatewayDeviceAuthorization
                                    baseUrl
                                    device
  where
    clientName = Text.strip rawClientName

pollGatewayAuthorization
    :: GatewayAuthorization
    -> IO (Either Text GatewayPollResult)
pollGatewayAuthorization authorization = do
    tryAny newTlsManager >>= \case
        Left exception ->
            pure (Left (Text.pack (show exception)))
        Right manager ->
            pollGatewayAuthorizationWith manager authorization

openGatewayAuthorizationPage :: GatewayAuthorization -> IO Bool
openGatewayAuthorizationPage =
    openBrowser
        . (.verificationUriComplete)
        . (.authorizationDevice)

-- | Complete the hosted gateway's browser OAuth flow through a loopback
-- callback. The injected presenter normally opens the URL in the user's
-- browser; keeping it injectable makes the network-independent contract
-- testable and lets interactive callers choose their own presentation.
connectGatewayBrowser
    :: Text
    -> Text
    -> (Text -> IO Bool)
    -> IO (Either Text ())
connectGatewayBrowser rawBaseUrl rawClientName present =
    connectGatewayBrowserWithCancel
        rawBaseUrl
        rawClientName
        present
        (atomically retry)

-- | Browser authorization with an explicit cancellation signal. Callers that
-- own an interactive prompt can complete the supplied action to stop waiting
-- for the loopback callback without waiting for the five-minute timeout.
connectGatewayBrowserWithCancel
    :: Text
    -> Text
    -> (Text -> IO Bool)
    -> IO ()
    -> IO (Either Text ())
connectGatewayBrowserWithCancel
    rawBaseUrl rawClientName present waitForCancellation =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl -> do
            attempted <-
                tryAny $
                    bracket openGatewayCallbackSocket close $
                        runBrowserFlow baseUrl
            pure case attempted of
                Left _ ->
                    Left
                        "Gateway browser authorization failed unexpectedly."
                Right result -> result
  where
    runBrowserFlow baseUrl listener = do
        redirectUri <- gatewayLoopbackRedirectUri listener
        verifier <- randomUrlText 32
        state <- randomUrlText 32
        case
            gatewayAuthorizationUrl
                baseUrl
                redirectUri
                state
                (gatewayPkceChallenge verifier)
                rawClientName of
            Left err -> pure (Left err)
            Right authorizationUrl -> do
                presented <- present authorizationUrl
                if not presented
                    then
                        pure
                            (Left
                                "Could not open the gateway authorization page.")
                    else do
                        callback <-
                            timeout
                                gatewayBrowserTimeoutMicroseconds
                                (race
                                    waitForCancellation
                                    (receiveGatewayAuthorizationCallback
                                        listener state))
                        case callback of
                            Nothing ->
                                pure
                                    (Left
                                        "Gateway browser authorization timed out.")
                            Just (Left ()) ->
                                pure
                                    (Left
                                        "Gateway browser authorization was cancelled.")
                            Just (Right (Left err)) -> pure (Left err)
                            Just (Right (Right authorizationCode)) ->
                                exchangeGatewayAuthorizationCode
                                    baseUrl
                                    redirectUri
                                    verifier
                                    authorizationCode
                                    >>= \case
                                        Left err -> pure (Left err)
                                        Right response ->
                                            case
                                                validateGatewayAuthorizationCodeResponse
                                                    baseUrl response of
                                                Left err -> pure (Left err)
                                                Right credential ->
                                                    saveGatewayCredential
                                                        credential

gatewayBrowserTimeoutMicroseconds :: Int
gatewayBrowserTimeoutMicroseconds = 5 * 60 * 1_000_000

-- | Construct the registered authorization request. The redirect is accepted
-- only when it is an IPv4 loopback URI with the exact callback path.
gatewayAuthorizationUrl
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> Either Text Text
gatewayAuthorizationUrl
    rawBaseUrl redirectUri state challenge rawClientName = do
        baseUrl <- validateBaseUrl rawBaseUrl
        validateGatewayLoopbackRedirectUri redirectUri
        whenEither
            ( Text.length state < 32
                || Text.length state > 200
                || not (Text.all isPkceCharacter state)
            )
            "Gateway OAuth state is invalid."
        whenEither
            ( Text.length challenge /= 43
                || not (Text.all isBase64UrlCharacter challenge)
            )
            "Gateway PKCE challenge is invalid."
        whenEither
            (Text.null clientName || Text.length clientName > 160)
            "Gateway client name must contain between 1 and 160 characters."
        pure $
            baseUrl
                <> "/connect/agent/authorize"
                <> query
  where
    clientName = Text.strip rawClientName
    query =
        TextEncoding.decodeUtf8 $
            LBS.toStrict $
                Builder.toLazyByteString $
                    renderQueryText
                        True
                        [ ("response_type", Just "code")
                        , ("client_id", Just gatewayBrowserClientId)
                        , ("redirect_uri", Just redirectUri)
                        , ("state", Just state)
                        , ("code_challenge", Just challenge)
                        , ("code_challenge_method", Just "S256")
                        , ("client_name", Just clientName)
                        ]

gatewayPkceChallenge :: Text -> Text
gatewayPkceChallenge =
    TextEncoding.decodeUtf8
        . Base64Url.encodeUnpadded
        . ByteArray.convert
        . (hash :: BS.ByteString -> Digest SHA256)
        . TextEncoding.encodeUtf8

-- | Validate a complete HTTP callback request and return only the one-time
-- authorization code. Method, path, singleton state, and state value are
-- checked before an OAuth error or code is accepted.
validateGatewayAuthorizationCallback
    :: Text
    -> BS.ByteString
    -> Either Text Text
validateGatewayAuthorizationCallback expectedState request = do
    target <- case BS8.words requestLine of
        method : rawTarget : _
            | method == "GET" -> Right rawTarget
            | otherwise -> Left "Gateway OAuth callback must use GET."
        _ -> Left "Gateway OAuth callback request is malformed."
    let (path, rawQuery) = BS.break (== 63) target
    whenEither
        (path /= TextEncoding.encodeUtf8 gatewayBrowserRedirectPath)
        "Gateway OAuth callback path is invalid."
    whenEither
        (BS.null rawQuery)
        "Gateway OAuth callback query is missing."
    state <- singletonParameter "state" parameters
        >>= maybe
            (Left "Gateway OAuth callback state is missing.")
            Right
    whenEither
        (state /= expectedState)
        "Gateway OAuth callback state mismatch."
    case singletonParameter "error" parameters of
        Left err -> Left err
        Right (Just oauthError) ->
            Left
                ("Gateway authorization was not granted"
                    <> safeOAuthErrorSuffix oauthError)
        Right Nothing -> do
            code <- singletonParameter "code" parameters
                >>= maybe
                    (Left
                        "Gateway OAuth callback authorization code is missing.")
                    Right
            whenEither
                (Text.null code || Text.length code > 4096)
                "Gateway OAuth callback authorization code is invalid."
            pure code
  where
    requestLine = BS8.takeWhile (/= '\r') request
    parameters = parseQueryText (BS.drop 1 rawQueryBytes)
    (_, rawQueryBytes) =
        case BS8.words requestLine of
            _ : target : _ -> BS.break (== 63) target
            _ -> ("", "")

validateGatewayAuthorizationCodeResponse
    :: Text
    -> GatewayAuthorizationCodeResponse
    -> Either Text GatewayCredential
validateGatewayAuthorizationCodeResponse rawRequestedBaseUrl response = do
    requestedBaseUrl <- validateBaseUrl rawRequestedBaseUrl
    responseBaseUrl <-
        validateBaseUrl response.authorizationResponseBaseUrl
    requestedOrigin <-
        parseGatewayOrigin
            "Gateway URL is invalid."
            requestedBaseUrl
    responseOrigin <-
        parseGatewayOrigin
            "The gateway returned an invalid base URL."
            responseBaseUrl
    whenEither
        (response.authorizationTokenType /= "Bearer")
        "The gateway returned an unsupported token type."
    whenEither
        (requestedOrigin /= responseOrigin)
        "The gateway returned a credential for a different origin."
    websocketOrigin <-
        parseGatewayOrigin
            "The gateway returned an invalid WebSocket URL."
            response.authorizationWebSocketUrl
    let expectedWebSocketOrigin =
            case responseOrigin of
                ("https:", host, port) -> ("wss:", host, port)
                ("http:", host, port) -> ("ws:", host, port)
                origin -> origin
    whenEither
        (websocketOrigin /= expectedWebSocketOrigin)
        "The gateway returned a WebSocket URL for a different origin."
    let credential =
            GatewayCredential
                { gatewayBaseUrl = responseBaseUrl
                , gatewayWebSocketUrl =
                    response.authorizationWebSocketUrl
                , gatewayAccessToken =
                    response.authorizationAccessToken
                }
    validateGatewayCredential credential
    pure credential

connectGateway :: Text -> IO ()
connectGateway rawBaseUrl = do
    authorization <-
        startGatewayAuthorization rawBaseUrl >>= either failText pure
    manager <- newTlsManager
    let device = authorization.authorizationDevice
    putStrLn ("Enter code " <> Text.unpack device.userCode <> " at:")
    putStrLn (Text.unpack device.verificationUri)
    opened <- openGatewayAuthorizationPage authorization
    when (not opened) $
        putStrLn "Could not open a browser automatically."
    credential <- pollUntilAuthorized manager authorization
    saveGatewayCredential credential >>= either failText pure
    putStrLn "Gateway connection saved."

showGatewayStatus :: IO ()
showGatewayStatus =
    loadGatewayCredential >>= \case
        Left err -> failText err
        Right Nothing -> putStrLn "Not connected to a gateway."
        Right (Just credential) -> do
            putStrLn ("Connected to " <> Text.unpack credential.gatewayBaseUrl)
            putStrLn ("Responses WebSocket: " <> Text.unpack credential.gatewayWebSocketUrl)

disconnectGateway :: IO ()
disconnectGateway = do
    removeGatewayCredential >>= either failText pure
    putStrLn "Gateway connection removed."

removeGatewayCredential :: IO (Either Text ())
removeGatewayCredential =
    fmap (fmap (const ())) $
        removeGatewayCredentialWith (pure ())

removeGatewayCredentialWith :: IO value -> IO (Either Text value)
removeGatewayCredentialWith afterRemove = do
    result <- tryAny do
        home <- Directory.getHomeDirectory
        withGatewayCredentialLockAt home do
            let path = gatewayCredentialPath home
            exists <- Directory.doesFileExist path
            when exists (Directory.removeFile path)
            afterRemove
    pure case result of
        Left exception -> Left (Text.pack (show exception))
        Right value -> Right value

runGatewayCommand :: GatewayCommand -> IO ()
runGatewayCommand = \case
    GatewayConnect url -> connectGateway url
    GatewayStatus -> showGatewayStatus
    GatewayDisconnect -> disconnectGateway

pollUntilAuthorized
    :: HTTP.Manager
    -> GatewayAuthorization
    -> IO GatewayCredential
pollUntilAuthorized manager authorization =
    go device.expiresInSeconds (max 1 device.pollIntervalSeconds)
  where
    baseUrl = authorization.authorizationBaseUrl
    device = authorization.authorizationDevice
    go remaining interval
        | remaining <= 0 = failText "Gateway authorization expired."
        | otherwise = do
            threadDelay (interval * 1_000_000)
            result <-
                pollGatewayAuthorizationWith manager authorization
                    >>= either failText pure
            case result of
                GatewayAuthorized accessToken websocketUrl ->
                    pure
                        GatewayCredential
                            { gatewayBaseUrl = baseUrl
                            , gatewayWebSocketUrl = websocketUrl
                            , gatewayAccessToken = accessToken
                            }
                GatewayAuthorizationPending serverInterval ->
                    let next = maybe interval (max 1) serverInterval
                     in go (remaining - next) next
                GatewaySlowDown serverInterval ->
                    let next =
                            maybe
                                (interval + 5)
                                (max (interval + 5))
                                serverInterval
                     in go (remaining - next) next
                GatewayAccessDenied -> failText "Gateway authorization was denied."
                GatewayExpired -> failText "Gateway authorization expired."
                GatewayPollFailed code ->
                    failText ("Gateway authorization failed: " <> code)

pollGatewayAuthorizationWith
    :: HTTP.Manager
    -> GatewayAuthorization
    -> IO (Either Text GatewayPollResult)
pollGatewayAuthorizationWith manager authorization =
    pollGatewayDeviceCodeWith
        manager
        authorization.authorizationBaseUrl
        authorization.authorizationDevice.deviceCode

pollGatewayDeviceCodeWith
    :: HTTP.Manager
    -> Text
    -> Text
    -> IO (Either Text GatewayPollResult)
pollGatewayDeviceCodeWith manager baseUrl deviceCode =
    postJson
        manager
        (baseUrl <> "/api/v1/agent-connections/token")
        (Aeson.object
            [ "device_code" .= deviceCode ])
        gatewayPollDecoder

-- | Perform one native device-flow poll and persist an authorized credential.
-- Secret token fields remain inside the trusted Haskell runtime.
pollNativeGatewayAuthorizationAndSave
    :: Text
    -> Text
    -> IO (Either Text GatewayPollResult)
pollNativeGatewayAuthorizationAndSave rawBaseUrl rawDeviceCode =
    pollNativeGatewayAuthorizationAndSaveWith
        rawBaseUrl rawDeviceCode (const (pure ()))

pollNativeGatewayAuthorizationAndSaveWith
    :: Text
    -> Text
    -> (GatewayPollResult -> IO ())
    -> IO (Either Text GatewayPollResult)
pollNativeGatewayAuthorizationAndSaveWith
        rawBaseUrl rawDeviceCode afterAuthorized =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl
            | Text.null deviceCode || Text.length deviceCode > 4096 ->
                pure (Left "Gateway device code is invalid.")
            | otherwise ->
                tryAny newTlsManager >>= \case
                    Left exception ->
                        pure (Left (Text.pack (show exception)))
                    Right manager ->
                        pollGatewayDeviceCodeWith manager baseUrl deviceCode
                            >>= \case
                                Left err -> pure (Left err)
                                Right
                                    authorized@(GatewayAuthorized
                                        accessToken
                                        websocketUrl) -> do
                                        saveGatewayCredentialWith
                                            GatewayCredential
                                                { gatewayBaseUrl = baseUrl
                                                , gatewayWebSocketUrl =
                                                    websocketUrl
                                                , gatewayAccessToken =
                                                    accessToken
                                                }
                                            (afterAuthorized authorized)
                                            >>= \case
                                                Left err -> pure (Left err)
                                                Right () ->
                                                    pure (Right authorized)
                                Right result -> pure (Right result)
  where
    deviceCode = Text.strip rawDeviceCode

postJson
    :: HTTP.Manager
    -> Text
    -> Aeson.Value
    -> Hermes.Decoder value
    -> IO (Either Text value)
postJson manager url payload decoder = do
    parsed <- tryAny (HTTP.parseRequest (Text.unpack url))
    case parsed of
        Left exception -> pure (Left (Text.pack (show exception)))
        Right initial -> do
            response <-
                tryAny $
                    HTTP.httpLbs
                        initial
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders = [(hContentType, "application/json")]
                            , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode payload)
                            , HTTP.checkResponse = \_ _ -> pure ()
                            }
                        manager
            pure case response of
                Left _ ->
                    Left "Could not reach the gateway authorization endpoint."
                Right value ->
                    case Hermes.decodeEither decoder (LBS.toStrict (HTTP.responseBody value)) of
                        Left err -> Left (Hermes.jsonErrorMessage err)
                        Right decoded -> Right decoded

exchangeGatewayAuthorizationCode
    :: Text
    -> Text
    -> Text
    -> Text
    -> IO (Either Text GatewayAuthorizationCodeResponse)
exchangeGatewayAuthorizationCode
    baseUrl redirectUri verifier authorizationCode = do
        manager <- newTlsManager
        postGatewayOAuthForm
            manager
            (baseUrl <> "/api/v1/agent-connections/oauth/token")
            [ ("grant_type", "authorization_code")
            , ("client_id", gatewayBrowserClientId)
            , ("code", authorizationCode)
            , ("code_verifier", verifier)
            , ("redirect_uri", redirectUri)
            ]

-- | Exchange the registered macOS custom-scheme callback and persist the
-- resulting credential without exposing its secret fields through the ABI.
exchangeNativeGatewayAuthorizationCode
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either Text ())
exchangeNativeGatewayAuthorizationCode
    rawBaseUrl clientId authorizationCode verifier redirectUri =
        exchangeNativeGatewayAuthorizationCodeWith
            rawBaseUrl
            clientId
            authorizationCode
            verifier
            redirectUri
            (pure ())

exchangeNativeGatewayAuthorizationCodeWith
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO ()
    -> IO (Either Text ())
exchangeNativeGatewayAuthorizationCodeWith
    rawBaseUrl clientId authorizationCode verifier redirectUri afterSave =
        case validateNativeGatewayAuthorizationExchange
            rawBaseUrl
            clientId
            authorizationCode
            verifier
            redirectUri of
            Left err -> pure (Left err)
            Right baseUrl ->
                tryAny newTlsManager >>= \case
                    Left _ ->
                        pure
                            (Left
                                "Could not prepare the gateway token request.")
                    Right manager ->
                        postGatewayOAuthForm
                            manager
                            (baseUrl
                                <> "/api/v1/agent-connections/oauth/token")
                            [ ("grant_type", "authorization_code")
                            , ("client_id", clientId)
                            , ("code", authorizationCode)
                            , ("code_verifier", verifier)
                            , ("redirect_uri", redirectUri)
                            ]
                            >>= \case
                                Left err -> pure (Left err)
                                Right response ->
                                    case
                                        validateGatewayAuthorizationCodeResponse
                                            baseUrl
                                            response of
                                        Left err -> pure (Left err)
                                        Right credential ->
                                            saveGatewayCredentialWith
                                                credential
                                                afterSave

validateNativeGatewayAuthorizationExchange
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> Either Text Text
validateNativeGatewayAuthorizationExchange
    rawBaseUrl clientId authorizationCode verifier redirectUri = do
        baseUrl <- validateBaseUrl rawBaseUrl
        whenEither
            (clientId /= "haskell-agent-macos")
            "Gateway OAuth client ID is invalid."
        whenEither
            (Text.null authorizationCode
                || Text.length authorizationCode > 4096)
            "Gateway authorization code is invalid."
        whenEither
            ( Text.length verifier < 43
                || Text.length verifier > 128
                || not (Text.all isPkceCharacter verifier)
            )
            "Gateway PKCE code verifier is invalid."
        whenEither
            (redirectUri /= "haskell-agent-auth://gateway/callback")
            "Gateway OAuth redirect URI is invalid."
        pure baseUrl

postGatewayOAuthForm
    :: HTTP.Manager
    -> Text
    -> [(Text, Text)]
    -> IO (Either Text GatewayAuthorizationCodeResponse)
postGatewayOAuthForm manager url fields = do
    parsed <- tryAny (HTTP.parseRequest (Text.unpack url))
    case parsed of
        Left _ ->
            pure (Left "Could not prepare the gateway token request.")
        Right initial -> do
            response <-
                tryAny $
                    HTTP.httpLbs
                        initial
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders =
                                [ ( hContentType
                                  , "application/x-www-form-urlencoded"
                                  )
                                ]
                            , HTTP.requestBody =
                                HTTP.RequestBodyLBS $
                                    Builder.toLazyByteString $
                                        renderQueryText
                                            False
                                            (fmap
                                                (\(name, value) ->
                                                    (name, Just value))
                                                fields)
                            , HTTP.checkResponse = \_ _ -> pure ()
                            , HTTP.redirectCount = 0
                            , HTTP.responseTimeout =
                                HTTP.responseTimeoutMicro (30 * 1_000_000)
                            }
                        manager
            pure case response of
                Left _ ->
                    Left "Could not reach the gateway OAuth token endpoint."
                Right value
                    | statusIsSuccessful (HTTP.responseStatus value) ->
                        case Hermes.decodeEither
                            gatewayAuthorizationCodeDecoder
                            (LBS.toStrict (HTTP.responseBody value)) of
                            Left _ ->
                                Left
                                    "The gateway returned an invalid authorization response."
                            Right decoded -> Right decoded
                    | otherwise ->
                        Left $
                            gatewayOAuthResponseError
                                (statusCode (HTTP.responseStatus value))
                                (HTTP.responseBody value)

gatewayOAuthResponseError :: Int -> LBS.ByteString -> Text
gatewayOAuthResponseError responseStatus body =
    "Gateway authorization failed"
        <> maybe
            (" (HTTP " <> Text.pack (show responseStatus) <> ").")
            (<> ".")
            (safeOAuthErrorCode body)

safeOAuthErrorCode :: LBS.ByteString -> Maybe Text
safeOAuthErrorCode body = do
    value <- Aeson.decode body
    rawCode <- case value of
        Aeson.Object object ->
            AesonTypes.parseMaybe
                (\fields -> fields Aeson..: "error")
                object
        _ -> Nothing
    let code = Text.take 64 (Text.strip rawCode)
    if Text.null code
        || not
            (Text.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` ("_-" :: String))
                code)
        then Nothing
        else Just (": " <> code)

openGatewayCallbackSocket :: IO Socket
openGatewayCallbackSocket =
    bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close
        \listener -> do
            setSocketOption listener ReuseAddr 1
            bind listener
                (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
            listen listener 1
            pure listener

gatewayLoopbackRedirectUri :: Socket -> IO Text
gatewayLoopbackRedirectUri listener =
    getSocketName listener >>= \case
        SockAddrInet port _ ->
            pure
                ("http://127.0.0.1:"
                    <> Text.pack (show port)
                    <> gatewayBrowserRedirectPath)
        _ -> failText "Gateway OAuth callback did not bind IPv4 loopback."

receiveGatewayAuthorizationCallback
    :: Socket
    -> Text
    -> IO (Either Text Text)
receiveGatewayAuthorizationCallback listener expectedState =
    bracket (fst <$> accept listener) close \connection -> do
        request <- receiveGatewayCallbackHeaders connection
        let result =
                request >>= validateGatewayAuthorizationCallback expectedState
            page = gatewayCallbackPage result
            response =
                "HTTP/1.1 200 OK\r\n\
                \Content-Type: text/html; charset=utf-8\r\n\
                \Cache-Control: no-store\r\n\
                \Content-Security-Policy: default-src 'none'; style-src 'unsafe-inline'\r\n\
                \X-Content-Type-Options: nosniff\r\n\
                \Connection: close\r\n\
                \Content-Length: "
                    <> BS8.pack (show (BS.length page))
                    <> "\r\n\r\n"
                    <> page
        Socket.sendAll connection response
        pure result

receiveGatewayCallbackHeaders
    :: Socket
    -> IO (Either Text BS.ByteString)
receiveGatewayCallbackHeaders connection = go BS.empty
  where
    maximumHeaderBytes = 16_384
    delimiter = "\r\n\r\n"

    go accumulated
        | delimiter `BS.isInfixOf` accumulated =
            pure (Right accumulated)
        | BS.length accumulated >= maximumHeaderBytes =
            pure (Left "Gateway OAuth callback headers were too large.")
        | otherwise = do
            chunk <-
                Socket.recv
                    connection
                    (maximumHeaderBytes - BS.length accumulated)
            if BS.null chunk
                then
                    pure
                        (Left
                            "Gateway OAuth callback closed before sending headers.")
                else go (accumulated <> chunk)

gatewayCallbackPage :: Either Text Text -> BS.ByteString
gatewayCallbackPage result =
    TextEncoding.encodeUtf8 $
        "<!doctype html><html><head><meta charset=\"utf-8\">\
        \<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\
        \<title>Haskell Agent</title></head>\
        \<body style=\"font-family:system-ui;margin:3rem;max-width:40rem\">\
        \<h1>"
            <> title
            <> "</h1><p>"
            <> message
            <> "</p></body></html>"
  where
    (title, message) = case result of
        Right _ ->
            ( "Authorization received"
            , "Return to Haskell Agent while it finishes connecting."
            )
        Left _ ->
            ( "Haskell Agent could not connect"
            , "Return to Haskell Agent to see the error and try again."
            )

validateGatewayLoopbackRedirectUri :: Text -> Either Text ()
validateGatewayLoopbackRedirectUri raw =
    case URI.parseURI (Text.unpack raw) of
        Just uri
            | URI.uriScheme uri == "http:"
            , Just authority <- URI.uriAuthority uri
            , null (URI.uriUserInfo authority)
            , URI.uriRegName authority == "127.0.0.1"
            , validPort (URI.uriPort authority)
            , Text.pack (URI.uriPath uri) == gatewayBrowserRedirectPath
            , null (URI.uriQuery uri)
            , null (URI.uriFragment uri) ->
                Right ()
        _ -> Left "Gateway OAuth redirect URI is invalid."
  where
    validPort (':' : digits) =
        not (null digits)
            && all isDigit digits
            && case readMaybe digits of
                Just port -> port > (0 :: Int) && port <= 65535
                Nothing -> False
    validPort _ = False

singletonParameter
    :: Text
    -> [(Text, Maybe Text)]
    -> Either Text (Maybe Text)
singletonParameter name parameters =
    case [value | (key, value) <- parameters, key == name] of
        [] -> Right Nothing
        [Just value] -> Right (Just value)
        _ ->
            Left
                ("Gateway OAuth callback contains duplicate or invalid "
                    <> name <> " parameters.")

safeOAuthErrorSuffix :: Text -> Text
safeOAuthErrorSuffix raw =
    let value = Text.take 64 (Text.strip raw)
    in if Text.null value
        || not
            (Text.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` ("_-" :: String))
                value)
        then "."
        else ": " <> value <> "."

randomUrlText :: Int -> IO Text
randomUrlText byteCount =
    TextEncoding.decodeUtf8
        . Base64Url.encodeUnpadded
        <$> getEntropy byteCount

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric character =
    character >= 'a' && character <= 'z'
        || character >= 'A' && character <= 'Z'
        || character >= '0' && character <= '9'

isBase64UrlCharacter :: Char -> Bool
isBase64UrlCharacter character =
    isAsciiAlphaNumeric character || character `elem` ("-_" :: String)

isPkceCharacter :: Char -> Bool
isPkceCharacter character =
    isBase64UrlCharacter character
        || character `elem` (".~" :: String)

parseGatewayOrigin
    :: Text
    -> Text
    -> Either Text (Text, Text, Int)
parseGatewayOrigin errorMessage raw = do
    uri <-
        maybe (Left errorMessage) Right $
            URI.parseURI (Text.unpack (Text.strip raw))
    authority <-
        maybe (Left errorMessage) Right (URI.uriAuthority uri)
    let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
        host = Text.toLower (Text.pack (URI.uriRegName authority))
    whenEither
        ( not (null (URI.uriUserInfo authority))
            || Text.null host
            || not (null (URI.uriQuery uri))
            || not (null (URI.uriFragment uri))
        )
        errorMessage
    port <- gatewayOriginPort errorMessage scheme (URI.uriPort authority)
    pure (scheme, host, port)

parseGatewayResourceOrigin
    :: Text
    -> Text
    -> Either Text (Text, Text, Int)
parseGatewayResourceOrigin errorMessage raw = do
    uri <-
        maybe (Left errorMessage) Right $
            URI.parseURI (Text.unpack (Text.strip raw))
    authority <-
        maybe (Left errorMessage) Right (URI.uriAuthority uri)
    let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
        host = Text.toLower (Text.pack (URI.uriRegName authority))
    whenEither
        ( not (null (URI.uriUserInfo authority))
            || Text.null host
            || not (null (URI.uriFragment uri))
        )
        errorMessage
    port <- gatewayOriginPort errorMessage scheme (URI.uriPort authority)
    pure (scheme, host, port)

gatewayOriginPort :: Text -> Text -> String -> Either Text Int
gatewayOriginPort _ "https:" "" = Right 443
gatewayOriginPort _ "http:" "" = Right 80
gatewayOriginPort _ "wss:" "" = Right 443
gatewayOriginPort _ "ws:" "" = Right 80
gatewayOriginPort errorMessage scheme (':' : digits)
    | scheme `elem` ["https:", "http:", "wss:", "ws:"]
    , not (null digits)
    , all isDigit digits
    , Just port <- readMaybe digits
    , port > 0
    , port <= (65535 :: Int) =
        Right port
    | otherwise = Left errorMessage
gatewayOriginPort errorMessage _ _ = Left errorMessage

whenEither :: Bool -> Text -> Either Text ()
whenEither condition message
    | condition = Left message
    | otherwise = Right ()

validateBaseUrl :: Text -> Either Text Text
validateBaseUrl raw
    | Text.null base = Left "Gateway URL cannot be empty."
    | otherwise = do
        uri <- maybe
            (Left "Gateway URL is invalid.")
            Right
            (URI.parseURI (Text.unpack base))
        authority <- maybe
            (Left "Gateway URL must include a host.")
            Right
            (URI.uriAuthority uri)
        whenEither
            (null (URI.uriRegName authority))
            "Gateway URL must include a host."
        whenEither
            (not (null (URI.uriUserInfo authority))
                || not (null (URI.uriQuery uri))
                || not (null (URI.uriFragment uri)))
            "Gateway URL cannot contain user info, a query, or a fragment."
        case Text.toLower (Text.pack (URI.uriScheme uri)) of
            "https:" -> Right base
            "http:"
                | localHost (URI.uriRegName authority) -> Right base
            _ ->
                Left
                    "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
  where
    base = Text.dropWhileEnd (== '/') (Text.strip raw)
    localHost rawHost =
        Text.toLower (Text.pack rawHost)
            `elem` ["localhost", "127.0.0.1", "::1", "[::1]"]
    whenEither condition message
        | condition = Left message
        | otherwise = Right ()

openBrowser :: Text -> IO Bool
openBrowser url = do
    result <- tryAny (rawSystem "open" [Text.unpack url])
    case result of
        Right ExitSuccess -> pure True
        _ -> do
            fallback <- tryAny (rawSystem "xdg-open" [Text.unpack url])
            pure case fallback of
                Right ExitSuccess -> True
                _ -> False

failText :: Text -> IO a
failText = ioError . userError . Text.unpack
