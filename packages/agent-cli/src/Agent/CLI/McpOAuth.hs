{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Interactive OAuth 2.1 login for remote MCP servers following the MCP
-- authorization specification (revision 2026-07-28): unauthenticated probe
-- for the @WWW-Authenticate@ challenge, protected resource and authorization
-- server metadata discovery with issuer validation, PKCE gate, client
-- registration selection, scope selection, RFC 8707 resource indicators, and
-- RFC 9207 issuer validation of the authorization response.
module Agent.CLI.McpOAuth
    ( LoginOptions(..)
    , defaultLoginOptions
    , loginMcp
    , loginMcpWith
    , logoutMcp
    , lookupServerOAuthConfig
    ) where

import Agent.CLI.Config (HarnessConfig(..), McpOAuthConfig(..), McpServerConfig(..), loadHarnessConfig)
import Agent.CLI.McpOAuthStore (loadMcpOAuthRecord, mcpOAuthStorePath, saveMcpOAuthRecord)
import qualified Agent.MCP.OAuth as OAuth
import Control.Exception.Safe (bracket, bracketOnError, tryAny)
import Control.Monad (forM_, when)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.URI (urlEncode)
import Network.Socket
    ( AddrInfo(..), Socket, SockAddr(..), SocketType(Stream), accept, bind, close
    , defaultHints, defaultProtocol, getAddrInfo, getSocketName, listen
    , setSocketOption, socket, SocketOption(ReuseAddr)
    )
import qualified Network.Socket.ByteString as Socket
import qualified System.Directory.OsPath as Dir
import qualified System.Entropy
import System.Exit (ExitCode(..))
import System.Process (rawSystem)
import System.Timeout (timeout)

data LoginOptions = LoginOptions
    { loginAdditionalScopes :: [Text]
    -- ^ Extra scopes to request on top of the selected and previously
    -- granted ones (step-up authorization).
    }
    deriving (Eq, Show)

defaultLoginOptions :: LoginOptions
defaultLoginOptions = LoginOptions { loginAdditionalScopes = [] }

loginMcp :: Text -> IO ()
loginMcp = loginMcpWith defaultLoginOptions

-- | The @client_id@ chosen for this login and how it was obtained.
data ResolvedClient = ResolvedClient
    { resolvedClientId :: Text
    , resolvedClientSecret :: Maybe Text
    , resolvedSource :: OAuth.ClientIdSource
    , resolvedMetadataUrl :: Maybe Text
    }

loginMcpWith :: LoginOptions -> Text -> IO ()
loginMcpWith options serverUrl = do
    home <- Dir.getHomeDirectory
    harness <- loadHarnessConfig home >>= either failText pure
    let oauthConfig = lookupServerOAuthConfig serverUrl harness
        resourceUri = OAuth.canonicalResourceUri serverUrl
    manager <- newTlsManager
    challenge <- OAuth.probeAuthorizationChallenge manager serverUrl >>= \case
        Left err -> do
            putStrLn ("Warning: " <> Text.unpack err <> "; falling back to well-known discovery.")
            pure Nothing
        Right probe -> do
            when (probe.probeStatus /= 401 && probe.probeStatus /= 403) $
                putStrLn ("Note: the MCP server answered the unauthenticated probe with HTTP "
                    <> show probe.probeStatus <> "; continuing with discovery.")
            pure probe.probeChallenge
    resource <- OAuth.discoverProtectedResourceMetadata manager serverUrl
        (challenge >>= (.challengeResourceMetadata)) >>= either failText pure
    stored <- loadMcpOAuthRecord serverUrl >>= \case
        Left err -> do
            putStrLn ("Warning: ignoring unreadable MCP OAuth record: " <> Text.unpack err)
            pure Nothing
        Right record -> pure record
    let storedIssuer = stored >>= (.extraIssuer) . snd
    issuer <- case resource.authorizationServers of
        [] -> failText "MCP protected resource metadata did not advertise an authorization server"
        first : _ -> pure case storedIssuer of
            Just previous | previous `elem` resource.authorizationServers -> previous
            _ -> first
    metadata <- OAuth.discoverAuthorizationServerMetadata manager issuer >>= either failText pure
    either failText pure (OAuth.checkPkceSupport metadata)
    let recordedIssuer = fromMaybe issuer metadata.issuer
        sameIssuer = storedIssuer == Just recordedIssuer
        previous = if sameIssuer then stored else Nothing
    when (isJust stored && not sameIssuer) $
        putStrLn ("Note: the authorization server changed to " <> Text.unpack recordedIssuer
            <> "; the previous client registration and granted scopes will not be reused.")
    let preferredPort = previous >>= (.extraRedirectUri) . snd >>= OAuth.loopbackRedirectPort
    bracket (openCallbackSocket preferredPort) close $ \listener -> do
        port <- callbackPort listener
        let redirect = "http://127.0.0.1:" <> Text.pack (show port) <> "/callback"
            scopes = OAuth.planScopes OAuth.ScopePlan
                { scopeSources = OAuth.ScopeSources
                    { scopeChallenge = maybe [] OAuth.challengeScopes challenge
                    , scopeResourceMetadata = resource.scopesSupported
                    , scopeConfigured = maybe [] (.mcpOAuthScopes) oauthConfig
                    }
                , scopePreviouslyGranted = maybe [] Text.words (previous >>= (.extraScope) . snd)
                , scopeAdditional = options.loginAdditionalScopes
                , scopeAuthorizationServerSupported = metadata.scopesSupportedByServer
                }
            registrationOptions = OAuth.RegistrationOptions
                { registrationPreRegistered = oauthConfig >>= \config ->
                    (\clientId -> OAuth.PreRegisteredClient clientId config.mcpOAuthClientSecret)
                        <$> config.mcpOAuthClientId
                , registrationClientIdMetadataUrl = oauthConfig >>= (.mcpOAuthClientIdMetadataUrl)
                , registrationStored = previous >>= \(file, extra) ->
                    OAuth.StoredClient
                        <$> extra.extraIssuer
                        <*> pure file.tokenClientId
                        <*> extra.extraClientIdSource
                        <*> pure extra.extraRedirectUri
                , registrationRedirectUri = redirect
                }
        plan <- either failText pure (OAuth.selectClientRegistration registrationOptions metadata)
        client <- case plan of
            OAuth.UsePreRegisteredClient pre -> do
                putStrLn "Using the pre-registered OAuth client from ~/.haskell-agent/config.json."
                pure ResolvedClient
                    { resolvedClientId = pre.preRegisteredClientId
                    , resolvedClientSecret = pre.preRegisteredClientSecret
                    , resolvedSource = OAuth.ClientIdPreRegistered
                    , resolvedMetadataUrl = Nothing
                    }
            OAuth.UseClientIdMetadataDocument url -> do
                putStrLn ("Using the Client ID Metadata Document " <> Text.unpack url <> " as client_id.")
                pure ResolvedClient
                    { resolvedClientId = url
                    , resolvedClientSecret = Nothing
                    , resolvedSource = OAuth.ClientIdMetadataDocument
                    , resolvedMetadataUrl = Just url
                    }
            OAuth.ReuseDynamicRegistration clientId -> do
                putStrLn "Reusing the dynamic client registration from the previous login."
                pure ResolvedClient
                    { resolvedClientId = clientId
                    , resolvedClientSecret = Nothing
                    , resolvedSource = OAuth.ClientIdDynamicRegistration
                    , resolvedMetadataUrl = Nothing
                    }
            OAuth.UseDynamicRegistration endpoint -> do
                registration <- OAuth.registerClientWith manager endpoint OAuth.ClientRegistrationRequest
                    { registrationClientName = "Haskell Agent"
                    , registrationRedirectUris = [redirect]
                    , registrationScopes = scopes
                    } >>= either failText pure
                pure ResolvedClient
                    { resolvedClientId = registration.clientId
                    , resolvedClientSecret = registration.clientSecret
                    , resolvedSource = OAuth.ClientIdDynamicRegistration
                    , resolvedMetadataUrl = Nothing
                    }
        verifier <- randomUrlBytes 32
        state <- randomUrlBytes 24
        let codeChallenge = Base64.encodeUnpadded (BA.convert (hash (Encoding.encodeUtf8 verifier) :: Digest SHA256))
            scopeText = Text.unwords scopes
            separator = if "?" `Text.isInfixOf` metadata.authorizationEndpoint then "&" else "?"
            authUrl = metadata.authorizationEndpoint <> separator
                <> "response_type=code&client_id=" <> encode client.resolvedClientId
                <> "&redirect_uri=" <> encode redirect
                <> "&code_challenge=" <> Encoding.decodeUtf8 codeChallenge
                <> "&code_challenge_method=S256&state=" <> encode state
                <> (if Text.null scopeText then "" else "&scope=" <> encode scopeText)
                <> "&resource=" <> encode resourceUri
        putStrLn ("Opening browser for MCP authorization: " <> Text.unpack authUrl)
        _ <- openBrowser authUrl
        callback <- timeout (5 * 60 * 1000000) (receiveCallback listener)
            >>= maybe (failText "Timed out waiting for MCP OAuth callback") pure
        -- RFC 9207: validate the issuer before acting on any other parameter,
        -- including error responses.
        either failText pure $ OAuth.validateAuthorizationResponseIssuer
            metadata.authorizationResponseIssParameterSupported recordedIssuer callback.callbackIss
        when (callback.callbackState /= Just state) (failText "MCP OAuth callback state mismatch")
        forM_ callback.callbackError \err ->
            failText ("MCP authorization was not granted: " <> err
                <> maybe "" (\description -> " (" <> description <> ")") callback.callbackErrorDescription)
        code <- maybe (failText "MCP OAuth callback did not contain an authorization code") pure callback.callbackCode
        OAuth.exchangeAuthorizationCodeWith manager OAuth.TokenExchange
            { exchangeEndpoint = metadata.tokenEndpoint
            , exchangeClientId = client.resolvedClientId
            , exchangeClientSecret = client.resolvedClientSecret
            , exchangeCode = code
            , exchangeRedirectUri = redirect
            , exchangeCodeVerifier = verifier
            , exchangeResource = Just resourceUri
            } >>= \case
                OAuth.OAuthTokenFailure err -> failText err
                OAuth.OAuthTokenSuccess tokens -> do
                    now :: Int <- round <$> getPOSIXTime
                    let tokenFile = OAuth.OAuthTokenFile
                            client.resolvedClientId metadata.tokenEndpoint tokens.accessToken
                            (fromMaybe "" tokens.refreshToken)
                            (fmap (now +) tokens.expiresIn)
                        granted = fromMaybe scopeText tokens.scope
                        extra = OAuth.OAuthTokenFileExtra
                            { extraIssuer = Just recordedIssuer
                            , extraScope = if Text.null (Text.strip granted) then Nothing else Just granted
                            , extraResource = Just resourceUri
                            , extraClientIdSource = Just client.resolvedSource
                            , extraClientIdMetadataUrl = client.resolvedMetadataUrl
                            , extraClientSecret = client.resolvedClientSecret
                            , extraRedirectUri = Just redirect
                            }
                    when (Text.null tokenFile.tokenRefreshToken) $
                        putStrLn "Warning: MCP provider returned no refresh token; reauthorization may be required."
                    saveMcpOAuthRecord serverUrl tokenFile extra
                        >>= either failText (const (putStrLn "MCP authorization saved."))

logoutMcp :: Text -> IO ()
logoutMcp server = do
    home <- Dir.getHomeDirectory
    let path = mcpOAuthStorePath home server
    exists <- Dir.doesFileExist path
    when exists (Dir.removeFile path)

-- | The @oauth@ block of the configured remote server whose URL identifies the
-- same MCP server as the login URL.
lookupServerOAuthConfig :: Text -> HarnessConfig -> Maybe McpOAuthConfig
lookupServerOAuthConfig serverUrl harness =
    case [server | server <- Map.elems harness.configMcpServers, matches server] of
        server : _ -> server.mcpOAuth
        [] -> Nothing
  where
    canonical = OAuth.canonicalResourceUri serverUrl
    matches server = maybe False ((== canonical) . OAuth.canonicalResourceUri) server.mcpUrl

data Callback = Callback
    { callbackCode :: Maybe Text
    , callbackState :: Maybe Text
    , callbackIss :: Maybe Text
    , callbackError :: Maybe Text
    , callbackErrorDescription :: Maybe Text
    }

-- | Listen on the loopback interface, preferring the port used by the
-- previous login so a stored dynamic registration's redirect URI can be
-- reproduced exactly.
openCallbackSocket :: Maybe Int -> IO Socket
openCallbackSocket preferred = do
    preferredSocket <- case preferred of
        Nothing -> pure Nothing
        Just port -> either (const Nothing) Just <$> tryAny (bindLoopback (show port))
    maybe (bindLoopback "0") pure preferredSocket

bindLoopback :: String -> IO Socket
bindLoopback service = do
    addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just service)
    bracketOnError (socket (addrFamily addr) Stream defaultProtocol) close \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 1
        pure sock

callbackPort :: Socket -> IO Int
callbackPort sock = do
    SockAddrInet port _ <- getSocketName sock
    pure (fromIntegral port)

receiveCallback :: Socket -> IO Callback
receiveCallback listener = bracket (fst <$> accept listener) close $ \sock -> do
    bytes <- Socket.recv sock 8192
    let requestLine = case BS8.lines bytes of line : _ -> line; _ -> ""
        target = case BS8.words requestLine of _method : path : _ -> path; _ -> "/"
        query = snd (BS.break (== 63) target)
        params = parseQuery (BS.drop 1 query)
        response = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: "
            <> Encoding.encodeUtf8 (Text.pack (show (LBS.length OAuth.oauthCallbackSuccessPage)))
            <> "\r\nConnection: close\r\n\r\n" <> LBS.toStrict OAuth.oauthCallbackSuccessPage
    Socket.sendAll sock response
    pure Callback
        { callbackCode = lookup "code" params
        , callbackState = lookup "state" params
        , callbackIss = lookup "iss" params
        , callbackError = lookup "error" params
        , callbackErrorDescription = lookup "error_description" params
        }

parseQuery :: BS.ByteString -> [(Text, Text)]
parseQuery raw = map parsePair (filter (not . BS.null) (BS.split 38 raw))
  where
    parsePair item =
        let (key, value) = BS.break (== 61) item
        in (decode key, decode (BS.drop 1 value))
    decode = Encoding.decodeUtf8 . urlDecode
    urlDecode = BS.concat . go
    go input = case BS.uncons input of
        Nothing -> []
        Just (37, a) -> case BS.uncons a of
            Just (x, b) -> case BS.uncons b of
                Just (y, rest) -> maybe [BS.singleton 37] (\v -> [BS.singleton v]) (hex x y) <> go rest
                _ -> [BS.singleton 37] <> go a
            _ -> [BS.singleton 37] <> go a
        Just (43, rest) -> BS.singleton 32 : go rest
        Just (x, rest) -> BS.singleton x : go rest
    hex x y = do
        a <- digit x
        b <- digit y
        pure (a * 16 + b)
    digit c
        | c >= 48 && c <= 57 = Just (c - 48)
        | c >= 65 && c <= 70 = Just (c - 55)
        | c >= 97 && c <= 102 = Just (c - 87)
        | otherwise = Nothing

randomUrlBytes :: Int -> IO Text
randomUrlBytes n = do
    bytes <- System.Entropy.getEntropy n
    pure (Encoding.decodeUtf8 (Base64.encodeUnpadded bytes))

encode :: Text -> Text
encode = Encoding.decodeUtf8 . urlEncode True . Encoding.encodeUtf8

openBrowser :: Text -> IO Bool
openBrowser url = do
    result <- tryAny (rawSystem "open" [Text.unpack url])
    case result of
        Right ExitSuccess -> pure True
        _ -> do
            result' <- tryAny (rawSystem "xdg-open" [Text.unpack url])
            pure (case result' of Right ExitSuccess -> True; _ -> False)

failText :: Text -> IO a
failText = ioError . userError . Text.unpack
