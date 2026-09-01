{-# LANGUAGE ScopedTypeVariables #-}

-- | Browser OAuth for connected, read-only Gmail and Microsoft accounts.
--
-- The native host receives an authorization URL and opaque flow id only. PKCE
-- verifiers, callback codes, and tokens remain in the Haskell runtime.  The
-- callback listener is bound exclusively to IPv4 loopback and each flow is
-- tracked until it is polled or cancelled.
module Agent.CLI.Mail.OAuth
    ( MailOAuthChallenge(..)
    , MailOAuthPoll(..)
    , startMailOAuth
    , pollMailOAuth
    , cancelMailOAuth
    , refreshMailOAuthCredential
    ) where

import Agent.CLI.Mail.Store
import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , readMVar
    , tryPutMVar
    , tryReadMVar
    , withMVar
    )
import Control.Exception.Safe (SomeException, finally, onException, tryAny)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Char8 as BS8
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Client
    ( BodyReader
    , RequestBody(RequestBodyBS)
    , brRead
    , checkResponse
    , method
    , parseRequest
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    , responseTimeout
    , responseTimeoutMicro
    , withResponse
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( Status
    , hAuthorization
    , hContentType
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.Socket
    ( AddrInfo(..)
    , SockAddr(SockAddrInet)
    , Socket
    , SocketOption(ReuseAddr)
    , SocketType(Stream)
    , accept
    , bind
    , close
    , defaultHints
    , defaultProtocol
    , getAddrInfo
    , getSocketName
    , listen
    , setSocketOption
    , socket
    )
import qualified Network.Socket.ByteString as Socket
import System.Entropy (getEntropy)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

data MailOAuthChallenge = MailOAuthChallenge
    { mailOAuthProvider :: !MailProvider
    , mailOAuthAuthorizationUrl :: !Text
    , mailOAuthFlowId :: !Text
    , mailOAuthExpiresInSeconds :: !Int
    }
    deriving (Eq, Show)

data MailOAuthPoll
    = MailOAuthPending
    | MailOAuthConnected !Text
    | MailOAuthFailed !Text
    | MailOAuthCancelled
    deriving (Eq, Show)

data OAuthFlow = OAuthFlow
    { flowListener :: !Socket
    , flowResult :: !(MVar MailOAuthPoll)
    , flowExpiresAt :: !UTCTime
    , flowWorker :: !ThreadId
    }

{-# NOINLINE activeFlows #-}
activeFlows :: MVar (Map.Map Text OAuthFlow)
activeFlows = unsafePerformIO (newMVar Map.empty)

{-# NOINLINE oauthStartLock #-}
oauthStartLock :: MVar ()
oauthStartLock = unsafePerformIO (newMVar ())

-- | Starts a public-client authorization-code flow. OAuth client secrets are
-- deliberately not accepted anywhere in this API.
startMailOAuth
    :: MailProvider
    -> Text
    -> IO (Either Text MailOAuthChallenge)
startMailOAuth provider rawClientId
    | provider == ImapProvider =
        pure (Left "Custom IMAP accounts use password authentication, not OAuth.")
    | Left err <- validateMailOAuthClientId provider clientId =
        pure (Left err)
    | otherwise = withMVar oauthStartLock \_ -> do
        pruneExpiredFlows
        activeCount <- Map.size <$> readMVar activeFlows
        if activeCount >= maximumActiveOAuthFlows
            then pure (Left
                "Too many email authorization windows are already open.")
            else tryAny begin >>= \case
                Left exception -> pure (Left (sanitizeException exception))
                Right challenge -> pure (Right challenge)
  where
    clientId = Text.strip rawClientId
    begin = do
        listener <- openLoopbackListener
        (do
            port <- loopbackPort listener
            verifier <- randomUrlText 48
            state <- randomUrlText 24
            flowId <- randomUrlText 24
            now <- getCurrentTime
            result <- newEmptyMVar
            let redirectUri =
                    "http://" <> redirectHost provider <> ":"
                        <> Text.pack (show port) <> "/mail/callback"
                challenge = MailOAuthChallenge
                    { mailOAuthProvider = provider
                    , mailOAuthAuthorizationUrl =
                        authorizationUrl provider clientId redirectUri state verifier
                    , mailOAuthFlowId = flowId
                    , mailOAuthExpiresInSeconds = oauthFlowLifetimeSeconds
                    }
            worker <- forkFinally
                (runOAuthFlow provider clientId verifier state redirectUri listener)
                (\settled ->
                    void (tryPutMVar result
                        (either (MailOAuthFailed . sanitizeException) id settled)))
            modifyMVar_ activeFlows (pure . Map.insert flowId OAuthFlow
                { flowListener = listener
                , flowResult = result
                , flowExpiresAt =
                    addUTCTime (fromIntegral oauthFlowLifetimeSeconds) now
                , flowWorker = worker
                })
            pure challenge)
            `onException` closeQuietly listener

pollMailOAuth :: Text -> IO (Either Text MailOAuthPoll)
pollMailOAuth rawFlowId
    | Text.null flowId = pure (Left "Mail OAuth flow id is required.")
    | otherwise = do
        now <- getCurrentTime
        maybeFlow <- Map.lookup flowId <$> readMVar activeFlows
        case maybeFlow of
            Nothing -> pure (Left "Mail OAuth flow was not found or has expired.")
            Just flow
                | now > flow.flowExpiresAt -> do
                    cancelFlow flow
                    removeFlow flowId
                    pure (Right MailOAuthCancelled)
                | otherwise -> do
                    result <- fromMaybe MailOAuthPending <$> tryReadMVar flow.flowResult
                    case result of
                        MailOAuthPending -> pure (Right result)
                        _ -> removeFlow flowId >> pure (Right result)
  where
    flowId = Text.strip rawFlowId

cancelMailOAuth :: Text -> IO (Either Text ())
cancelMailOAuth rawFlowId
    | Text.null flowId = pure (Left "Mail OAuth flow id is required.")
    | otherwise =
        modifyMVar activeFlows \flows ->
            case Map.lookup flowId flows of
                Nothing -> pure (flows, Left "Mail OAuth flow was not found.")
                Just flow -> do
                    cancelFlow flow
                    pure (Map.delete flowId flows, Right ())
  where
    flowId = Text.strip rawFlowId

runOAuthFlow
    :: MailProvider
    -> Text
    -> Text
    -> Text
    -> Text
    -> Socket
    -> IO MailOAuthPoll
runOAuthFlow provider clientId verifier expectedState redirectUri listener =
    (do
        callback <- timeout oauthFlowTimeoutMicros
            (receiveMatchingCallback expectedState listener)
        case callback of
            Nothing -> pure (MailOAuthFailed "Mail authorization timed out.")
            Just parameters -> do
                case singleQueryValue "error" parameters of
                    Just failure ->
                        pure (MailOAuthFailed
                            ("Mail authorization was not granted: " <> bounded failure))
                    Nothing -> do
                        authorizationCode <- maybe
                            (failText "Mail authorization callback had no code.")
                            pure
                            (singleQueryValue "code" parameters)
                        exchangeAndPersist
                            provider clientId authorizationCode verifier redirectUri
                            >>= pure . either MailOAuthFailed MailOAuthConnected
    ) `finally` closeQuietly listener

-- | Refresh immediately before a provider request. Reload and secret-first
-- persistence are protected by the dedicated cross-process refresh lock.
refreshMailOAuthCredential :: MailCredential -> IO (Either Text MailCredential)
refreshMailOAuthCredential MailCredential
        { mailCredentialAccount = suppliedAccount
        , mailCredentialSecret = MailOAuthSecret {}
        }
    | not suppliedAccount.mailAccountEnabled = pure (Left "Mail account is disabled.")
    | otherwise = withMailRefreshLock do
        loadMailCredentials >>= \case
            Left err -> pure (Left err)
            Right credentials ->
                case find ((== suppliedAccount.mailAccountId)
                        . (.mailCredentialAccount.mailAccountId)) credentials of
                    Nothing -> pure (Left "Mail account no longer exists.")
                    Just current -> refreshCurrent current
  where
    refreshCurrent current@MailCredential
            { mailCredentialAccount = account
            , mailCredentialSecret = MailOAuthSecret
                { mailSecretAccountId
                , mailOAuthAccessToken
                , mailOAuthRefreshToken
                , mailOAuthExpiresAt
                , mailOAuthScopes
                }
            }
        | not account.mailAccountEnabled = pure (Left "Mail account is disabled.")
        | otherwise = do
            now <- getCurrentTime
            case mailOAuthExpiresAt of
                Just expiry | expiry > addUTCTime
                    (fromIntegral oauthRefreshSkewSeconds) now -> pure (Right current)
                _ -> case account.mailAccountOAuthClientId of
                    Nothing -> failed current "oauth_client_missing"
                        "Mail account must be reconnected."
                    Just clientId ->
                        refreshToken account.mailAccountProvider clientId
                            mailSecretAccountId mailOAuthAccessToken
                            mailOAuthRefreshToken mailOAuthScopes >>= \case
                                Left err
                                    | oauthRefreshRequiresReconnect err ->
                                        failed current "oauth_refresh_failed"
                                            "Mail account must be reconnected."
                                    | otherwise -> pure (Left err)
                                Right refreshed -> do
                                    let account' = account
                                            { mailAccountState = MailConnected
                                            , mailAccountLastErrorCode = Nothing }
                                    upsertMailAccountAfterRefresh account' refreshed >>= \case
                                        Left err -> pure (Left err)
                                        Right () ->
                                            lookupMailCredential
                                                account.mailAccountId >>= \case
                                                    Left err -> pure (Left err)
                                                    Right (Just stored)
                                                        | stored.mailCredentialAccount.mailAccountEnabled
                                                        , stored.mailCredentialAccount.mailAccountState
                                                            == MailConnected ->
                                                                pure (Right stored)
                                                    _ -> pure (Left
                                                        "Mail account changed while credentials were refreshing.")
    refreshCurrent MailCredential {} = pure (Left "Mail account does not use OAuth.")
    failed current code message =
        setMailAccountStateIfUnchanged
            current.mailCredentialAccount
            MailNeedsReauthorization
            (Just code) >>= \case
                Left err -> pure (Left err)
                Right () -> pure (Left message)
refreshMailOAuthCredential MailCredential {} =
    pure (Left "Mail account does not use OAuth.")

exchangeAndPersist
    :: MailProvider -> Text -> Text -> Text -> Text -> IO (Either Text Text)
exchangeAndPersist provider clientId authorizationCode verifier redirectUri =
    exchangeToken provider
        [ ("grant_type", "authorization_code"), ("client_id", clientId)
        , ("code", authorizationCode), ("code_verifier", verifier)
        , ("redirect_uri", redirectUri) ] >>= \case
        Left err -> pure (Left err)
        Right token -> resolveMailbox provider token.tokenAccessToken >>= \case
            Left err -> pure (Left err)
            Right (rawEmail, rawLabel) -> case normalizeMailEmail rawEmail of
                Left _ -> pure (Left "Mail provider returned an invalid mailbox identity.")
                Right email -> loadMailCredentials >>= \case
                    Left err -> pure (Left err)
                    Right credentials -> do
                        now <- getCurrentTime
                        let prior = find (\credential ->
                                credential.mailCredentialAccount.mailAccountProvider == provider
                                && normalizedEmail credential.mailCredentialAccount.mailAccountEmail
                                    == normalizedEmail email) credentials
                            preserved = prior >>= \credential -> case credential.mailCredentialSecret of
                                MailOAuthSecret { mailOAuthRefreshToken } -> mailOAuthRefreshToken
                                MailImapSecret {} -> Nothing
                        case nonEmpty token.tokenRefreshToken <|> preserved of
                            Nothing -> pure (Left "Mail provider did not return offline refresh access. Authorize again and approve offline access.")
                            Just refresh -> do
                                accountId <- maybe (newMailAccountId provider)
                                    (pure . (.mailCredentialAccount.mailAccountId)) prior
                                let account = MailAccount
                                        { mailAccountId = accountId
                                        , mailAccountProvider = provider
                                        , mailAccountEmail = email
                                        , mailAccountLabel = fromMaybe email (nonEmpty rawLabel)
                                        , mailAccountEnabled = True
                                        , mailAccountState = MailConnected
                                        , mailAccountImapSettings = Nothing
                                        , mailAccountOAuthClientId = Just clientId
                                        , mailAccountCreatedAt = maybe now
                                            (.mailCredentialAccount.mailAccountCreatedAt) prior
                                        , mailAccountUpdatedAt = now
                                        , mailAccountLastVerifiedAt = Just now
                                        , mailAccountLastErrorCode = Nothing
                                        }
                                    secret = MailOAuthSecret
                                        { mailSecretAccountId = accountId
                                        , mailOAuthAccessToken = token.tokenAccessToken
                                        , mailOAuthRefreshToken = Just refresh
                                        , mailOAuthExpiresAt = Just (addUTCTime
                                            (fromIntegral token.tokenExpiresIn) now)
                                        , mailOAuthScopes = Text.words token.tokenScope
                                        }
                                upsertMailAccount account secret >>= \case
                                    Left err -> pure (Left err)
                                    Right () -> pure (Right accountId)

refreshToken :: MailProvider -> Text -> Text -> Text -> Maybe Text -> [Text]
    -> IO (Either Text MailSecret)
refreshToken provider clientId accountId oldAccess oldRefresh oldScopes
    | provider == ImapProvider = pure (Left "Custom IMAP accounts do not use OAuth.")
    | otherwise = case oldRefresh >>= nonEmpty of
        Nothing -> pure (Left "Mail account must be reconnected.")
        Just refresh -> exchangeToken provider
            [("grant_type", "refresh_token"), ("client_id", clientId),
             ("refresh_token", refresh)] >>= \case
                Left err -> pure (Left err)
                Right token -> do
                    now <- getCurrentTime
                    pure (Right MailOAuthSecret
                        { mailSecretAccountId = accountId
                        , mailOAuthAccessToken = fromMaybe oldAccess
                            (nonEmpty token.tokenAccessToken)
                        , mailOAuthRefreshToken = nonEmpty token.tokenRefreshToken <|> oldRefresh
                        , mailOAuthExpiresAt = Just (addUTCTime
                            (fromIntegral token.tokenExpiresIn) now)
                        , mailOAuthScopes = case Text.words token.tokenScope of
                            [] -> oldScopes
                            scopes -> scopes })

data OAuthToken = OAuthToken
    { tokenAccessToken :: !Text
    , tokenRefreshToken :: !Text
    , tokenExpiresIn :: !Int
    , tokenScope :: !Text
    }

instance Aeson.FromJSON OAuthToken where
    parseJSON = Aeson.withObject "Mail OAuth token" \object ->
        OAuthToken
            <$> object .: "access_token"
            <*> object .:? "refresh_token" Aeson..!= ""
            <*> object .:? "expires_in" Aeson..!= 3600
            <*> object .:? "scope" Aeson..!= ""

exchangeToken :: MailProvider -> [(Text, Text)] -> IO (Either Text OAuthToken)
exchangeToken provider fields
    | provider == ImapProvider =
        pure (Left "Custom IMAP accounts do not use OAuth.")
    | otherwise = do
        manager <- newTlsManager
        parsed <- tryAny (parseRequest (Text.unpack (tokenEndpoint provider)))
        case parsed of
            Left exception -> pure (Left (sanitizeException exception))
            Right initial -> do
                response <- tryAny $ withResponse
                    initial
                        { method = "POST"
                        , requestHeaders =
                            [(hContentType, "application/x-www-form-urlencoded")]
                        , requestBody = RequestBodyBS
                            (renderSimpleQuery False
                                [ (TextEncoding.encodeUtf8 key, TextEncoding.encodeUtf8 value)
                                | (key, value) <- fields
                                ])
                        , responseTimeout =
                            responseTimeoutMicro oauthHttpTimeoutMicros
                        , checkResponse = \_ _ -> pure ()
                        }
                    manager
                    \value -> do
                        body <- readOAuthBody value.responseBody
                        pure (value.responseStatus, body)
                pure case response of
                    Left exception -> Left (sanitizeException exception)
                    Right (status, body)
                        | not (statusIsSuccessful status) ->
                            Left (oauthTokenStatusError status)
                        | otherwise -> body >>= \bytes ->
                            case Aeson.eitherDecodeStrict' bytes of
                                Left _ ->
                                    Left "Mail OAuth provider returned an invalid token response."
                                Right token
                                    | Text.null (Text.strip token.tokenAccessToken) ->
                                        Left "Mail OAuth provider returned an empty access token."
                                    | otherwise -> Right token

resolveMailbox :: MailProvider -> Text -> IO (Either Text (Text, Text))
resolveMailbox provider accessToken =
    case provider of
        GmailProvider ->
            getJson accessToken
                "https://gmail.googleapis.com/gmail/v1/users/me/profile"
                >>= \case
                    Left err -> pure (Left err)
                    Right value ->
                        pure $ maybe
                            (Left "Gmail did not return a mailbox identity. Verify gmail.readonly was granted.")
                            Right
                            (parseMaybe
                                (Aeson.withObject "Gmail profile" \object ->
                                    (,) <$> object .: "emailAddress" <*> pure "")
                                value)
        MicrosoftProvider -> do
            identity <- getJson accessToken
                "https://graph.microsoft.com/v1.0/me?$select=mail,userPrincipalName,displayName"
            mailboxProbe <- getJson accessToken
                "https://graph.microsoft.com/v1.0/me/messages?$top=1&$select=id"
            pure do
                _ <- mailboxProbe
                value <- identity
                maybe
                    (Left "Microsoft did not return a mailbox identity. Verify Mail.Read was granted.")
                    Right
                    (parseMaybe
                        (Aeson.withObject "Microsoft profile" \object -> do
                            mail <- object .:? "mail"
                            principal <- object .:? "userPrincipalName"
                            displayName <- object .:? "displayName" Aeson..!= ""
                            address <- maybe
                                (fail "missing mailbox identity")
                                pure
                                ((mail >>= nonEmpty) <|> (principal >>= nonEmpty))
                            pure (address, displayName))
                        value)
        ImapProvider -> pure (Left "Custom IMAP accounts do not use OAuth.")

getJson :: Text -> Text -> IO (Either Text Aeson.Value)
getJson accessToken rawUrl = do
    manager <- newTlsManager
    parsed <- tryAny (parseRequest (Text.unpack rawUrl))
    case parsed of
        Left exception -> pure (Left (sanitizeException exception))
        Right initial -> do
            response <- tryAny $ withResponse
                initial
                    { requestHeaders =
                        [(hAuthorization, "Bearer " <> TextEncoding.encodeUtf8 accessToken)]
                    , responseTimeout = responseTimeoutMicro oauthHttpTimeoutMicros
                    , checkResponse = \_ _ -> pure ()
                    }
                manager
                \value -> do
                    body <- readOAuthBody value.responseBody
                    pure (value.responseStatus, body)
            pure case response of
                Left exception -> Left (sanitizeException exception)
                Right (status, body)
                    | not (statusIsSuccessful status) ->
                        Left "Mail mailbox access validation failed. Reconnect the account and grant read access."
                    | otherwise -> body >>= \bytes ->
                        firstText "Mail provider returned invalid validation JSON."
                            (Aeson.eitherDecodeStrict' bytes)

authorizationUrl :: MailProvider -> Text -> Text -> Text -> Text -> Text
authorizationUrl provider clientId redirectUri state verifier =
    authorizationEndpoint provider <> "?"
        <> TextEncoding.decodeUtf8
            (renderSimpleQuery False
                [ ("response_type", "code")
                , ("client_id", TextEncoding.encodeUtf8 clientId)
                , ("redirect_uri", TextEncoding.encodeUtf8 redirectUri)
                , ("scope", TextEncoding.encodeUtf8 (oauthScopes provider))
                , ("state", TextEncoding.encodeUtf8 state)
                , ( "code_challenge"
                  , Base64URL.encodeUnpadded
                        (digestBytes
                            (hash (TextEncoding.encodeUtf8 verifier)
                                :: Digest SHA256))
                  )
                , ("code_challenge_method", "S256")
                ]
                <> providerExtras provider)

providerExtras :: MailProvider -> BS.ByteString
providerExtras GmailProvider = "&access_type=offline&prompt=consent"
providerExtras MicrosoftProvider = BS.empty
providerExtras ImapProvider = BS.empty

authorizationEndpoint :: MailProvider -> Text
authorizationEndpoint = \case
    GmailProvider -> "https://accounts.google.com/o/oauth2/v2/auth"
    MicrosoftProvider -> "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
    ImapProvider -> ""

tokenEndpoint :: MailProvider -> Text
tokenEndpoint = \case
    GmailProvider -> "https://oauth2.googleapis.com/token"
    MicrosoftProvider -> "https://login.microsoftonline.com/common/oauth2/v2.0/token"
    ImapProvider -> ""

oauthScopes :: MailProvider -> Text
oauthScopes = \case
    GmailProvider -> "openid email https://www.googleapis.com/auth/gmail.readonly"
    MicrosoftProvider -> "openid profile offline_access User.Read Mail.Read"
    ImapProvider -> ""

-- Microsoft desktop-app registrations use the special localhost redirect,
-- whose port is intentionally dynamic. Google installed-app clients support
-- the numeric loopback redirect. Both resolve to the IPv4-only listener below.
redirectHost :: MailProvider -> Text
redirectHost = \case
    MicrosoftProvider -> "localhost"
    GmailProvider -> "127.0.0.1"
    ImapProvider -> "127.0.0.1"

openLoopbackListener :: IO Socket
openLoopbackListener = do
    addresses <- getAddrInfo
        (Just defaultHints { addrSocketType = Stream })
        (Just "127.0.0.1")
        (Just "0")
    address <- maybe
        (failText "Could not allocate an IPv4 loopback OAuth listener.")
        pure
        (safeHead addresses)
    listener <- socket (addrFamily address) Stream defaultProtocol
    setSocketOption listener ReuseAddr 1
    bind listener (addrAddress address)
    listen listener 1
    pure listener

loopbackPort :: Socket -> IO Int
loopbackPort listener =
    getSocketName listener >>= \case
        SockAddrInet port _ -> pure (fromIntegral port)
        _ -> failText "Could not allocate an IPv4 loopback OAuth port."

receiveMatchingCallback :: Text -> Socket -> IO [(Text, Text)]
receiveMatchingCallback expectedState listener =
    go maximumCallbackConnections
  where
    go remaining
        | remaining <= 0 =
            failText "Mail authorization received too many invalid callbacks."
        | otherwise = do
            (client, _) <- accept listener
            attempted <- tryAny $
                timeout oauthCallbackClientTimeoutMicros
                    (receiveCallbackAttempt expectedState client)
                    `finally` closeQuietly client
            case attempted of
                Right (Just (Just parameters)) -> pure parameters
                _ -> go (remaining - 1)

receiveCallbackAttempt :: Text -> Socket -> IO (Maybe [(Text, Text)])
receiveCallbackAttempt expectedState client = do
    request <- readCallbackRequest client
    let parsed = request >>= callbackParameters
        matching = parsed >>= \parameters ->
            case
                ( singleQueryValue "state" parameters
                , singleQueryValue "code" parameters
                , singleQueryValue "error" parameters
                )
            of
                (Just state, Just code, Nothing)
                    | state == expectedState
                    , isJust (nonEmpty code) ->
                        Just parameters
                (Just state, Nothing, Just failure)
                    | state == expectedState
                    , isJust (nonEmpty failure) ->
                        Just parameters
                _ -> Nothing
    void
        (tryAny (sendCallbackResponse client (isJust matching))
            :: IO (Either SomeException ()))
    pure matching

readCallbackRequest :: Socket -> IO (Maybe BS.ByteString)
readCallbackRequest client =
    go BS.empty
  where
    go accumulated
        | callbackHeadersComplete accumulated = pure (Just accumulated)
        | BS.length accumulated >= maximumCallbackRequestBytes = pure Nothing
        | otherwise = do
            let remaining = maximumCallbackRequestBytes - BS.length accumulated
            chunk <- Socket.recv client (min callbackReadChunkBytes remaining)
            if BS.null chunk
                then pure Nothing
                else go (accumulated <> chunk)

callbackHeadersComplete :: BS.ByteString -> Bool
callbackHeadersComplete request =
    "\r\n\r\n" `BS.isInfixOf` request || "\n\n" `BS.isInfixOf` request

callbackParameters :: BS.ByteString -> Maybe [(Text, Text)]
callbackParameters request = do
    requestLine <- safeHead (BS8.lines request)
    case BS8.words requestLine of
        method : target : _
            | method == "GET"
            , let (path, rawQuery) = BS.break (== 63) target
            , path == "/mail/callback"
            , not (BS.null rawQuery) ->
                Just (parseQuery (BS.drop 1 rawQuery))
        _ -> Nothing

sendCallbackResponse :: Socket -> Bool -> IO ()
sendCallbackResponse client accepted = do
    let body
            | accepted =
                "<!doctype html><title>Haskell Agent</title>"
                    <> "<p>You can return to Haskell Agent.</p>"
            | otherwise =
                "<!doctype html><title>Haskell Agent</title>"
                    <> "<p>This authorization callback was not recognized.</p>"
        response = BS.concat
            [ "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n"
            , "Content-Length: "
            , BS8.pack (show (BS.length body))
            , "\r\nConnection: close\r\n\r\n"
            , body
            ]
    Socket.sendAll client response

parseQuery :: BS.ByteString -> [(Text, Text)]
parseQuery =
    map parseParameter . take maximumCallbackParameters
        . filter (not . BS.null) . BS.split 38
  where
    parseParameter pair =
        let (key, rawValue) = BS.break (== 61) pair
        in (decodeQueryPart key, decodeQueryPart (BS.drop 1 rawValue))

singleQueryValue :: Text -> [(Text, Text)] -> Maybe Text
singleQueryValue key parameters =
    case [value | (candidate, value) <- parameters, candidate == key] of
        [value] -> Just value
        _ -> Nothing

decodeQueryPart :: BS.ByteString -> Text
decodeQueryPart =
    TextEncoding.decodeUtf8With TextEncodingError.lenientDecode . percentDecode

percentDecode :: BS.ByteString -> BS.ByteString
percentDecode = BS.concat . go
  where
    go bytes =
        case BS.uncons bytes of
            Nothing -> []
            Just (37, remaining) ->
                case BS.uncons remaining of
                    Just (first, afterFirst) ->
                        case BS.uncons afterFirst of
                            Just (second, afterSecond) ->
                                case hex first second of
                                    Just value -> BS.singleton value : go afterSecond
                                    Nothing -> BS.singleton 37 : go remaining
                            Nothing -> BS.singleton 37 : go remaining
                    Nothing -> [BS.singleton 37]
            Just (43, remaining) -> BS.singleton 32 : go remaining
            Just (value, remaining) -> BS.singleton value : go remaining
    hex first second = do
        high <- hexDigit first
        low <- hexDigit second
        pure (high * 16 + low)
    hexDigit value
        | value >= 48 && value <= 57 = Just (value - 48)
        | value >= 65 && value <= 70 = Just (value - 55)
        | value >= 97 && value <= 102 = Just (value - 87)
        | otherwise = Nothing

randomUrlText :: Int -> IO Text
randomUrlText bytes =
    TextEncoding.decodeUtf8 . Base64URL.encodeUnpadded <$> getEntropy bytes

-- crypton renders a digest as exactly two lowercase hexadecimal characters
-- per byte. Decoding that representation avoids coupling this module to the
-- byte-array implementation package used internally by a particular crypton
-- release.
digestBytes :: Digest SHA256 -> BS.ByteString
digestBytes = BS.pack . go . show
  where
    go (high : low : rest) =
        case (hexNibble high, hexNibble low) of
            (Just highValue, Just lowValue) ->
                (highValue * 16 + lowValue) : go rest
            _ -> []
    go _ = []

    hexNibble character
        | character >= '0' && character <= '9' =
            Just (fromIntegral (fromEnum character - fromEnum '0'))
        | character >= 'a' && character <= 'f' =
            Just (fromIntegral (fromEnum character - fromEnum 'a' + 10))
        | character >= 'A' && character <= 'F' =
            Just (fromIntegral (fromEnum character - fromEnum 'A' + 10))
        | otherwise = Nothing

pruneExpiredFlows :: IO ()
pruneExpiredFlows = do
    now <- getCurrentTime
    expired <- modifyMVar activeFlows \flows -> do
        let (stale, current) =
                Map.partition (\flow -> flow.flowExpiresAt < now) flows
        pure (current, Map.elems stale)
    mapM_ cancelFlow expired

cancelFlow :: OAuthFlow -> IO ()
cancelFlow flow = do
    void (tryPutMVar flow.flowResult MailOAuthCancelled)
    closeQuietly flow.flowListener
    void (tryAny (killThread flow.flowWorker) :: IO (Either SomeException ()))

removeFlow :: Text -> IO ()
removeFlow flowId =
    modifyMVar_ activeFlows (pure . Map.delete flowId)

closeQuietly :: Socket -> IO ()
closeQuietly socketToClose =
    void (tryAny (close socketToClose) :: IO (Either SomeException ()))

firstText :: Text -> Either a value -> Either Text value
firstText message = either (const (Left message)) Right

normalizedEmail :: Text -> Text
normalizedEmail = Text.toCaseFold . Text.strip

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value

bounded :: Text -> Text
bounded = Text.take 512 . Text.unwords . Text.words

sanitizeException :: SomeException -> Text
sanitizeException _ =
    "Could not complete secure mail authorization. Please try again."

oauthTokenStatusError :: Status -> Text
oauthTokenStatusError status
    | statusCode status == 400
        || statusCode status == 401
        || statusCode status == 403 =
            "Mail OAuth credentials were rejected and the account must be reconnected."
    | statusCode status == 429 || statusCode status >= 500 =
            "The mail OAuth provider is temporarily unavailable. Please try again."
    | otherwise =
            "Mail OAuth authorization failed. Please try again."

oauthRefreshRequiresReconnect :: Text -> Bool
oauthRefreshRequiresReconnect =
    Text.isInfixOf "must be reconnected" . Text.toCaseFold

readOAuthBody :: BodyReader -> IO (Either Text BS.ByteString)
readOAuthBody = go [] 0
  where
    go chunks size reader = brRead reader >>= \chunk ->
        if BS.null chunk
            then pure (Right (BS.concat (reverse chunks)))
            else
                let size' = size + BS.length chunk
                in if size' > oauthMaximumResponseBytes
                    then pure (Left
                        "Mail OAuth provider returned an oversized response.")
                    else go (chunk : chunks) size' reader

safeHead :: [value] -> Maybe value
safeHead = \case
    [] -> Nothing
    value : _ -> Just value

failText :: Text -> IO value
failText = ioError . userError . Text.unpack

oauthFlowLifetimeSeconds, oauthRefreshSkewSeconds, oauthHttpTimeoutMicros :: Int
oauthFlowLifetimeSeconds = 5 * 60
oauthRefreshSkewSeconds = 60
oauthHttpTimeoutMicros = 30 * 1000 * 1000

oauthFlowTimeoutMicros, oauthCallbackClientTimeoutMicros
    , maximumCallbackRequestBytes, maximumCallbackParameters
    , maximumCallbackConnections, callbackReadChunkBytes :: Int
oauthFlowTimeoutMicros = oauthFlowLifetimeSeconds * 1000 * 1000
oauthCallbackClientTimeoutMicros = 5 * 1000 * 1000
maximumCallbackRequestBytes = 16 * 1024
maximumCallbackParameters = 32
maximumCallbackConnections = 32
callbackReadChunkBytes = 4096
maximumActiveOAuthFlows :: Int
maximumActiveOAuthFlows = 8

oauthMaximumResponseBytes :: Int
oauthMaximumResponseBytes = 1024 * 1024
