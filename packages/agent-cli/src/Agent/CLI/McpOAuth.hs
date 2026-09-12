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
    , McpLoginHost(..)
    , defaultLoginOptions
    , defaultMcpLoginHost
    , loginMcp
    , loginMcpWith
    , loginMcpWithHost
    , loginMcpWithResult
    , McpOAuthHost(..)
    , authorizeMcpWith
    , validateMcpOAuthCallback
    , logoutMcp
    , lookupServerOAuthConfig
    , mcpOAuthCallbackTimeoutMicros
    , registerAuthorizedMcpServer
    ) where

import Agent.CLI.Config (HarnessConfig(..), McpOAuthConfig(..), McpServerConfig(..), loadHarnessConfig, modifyHarnessConfig)
import Agent.CLI.Error (formatException)
import Agent.CLI.Login.Internal.Browser (openBrowser)
import Agent.CLI.McpOAuthStore (loadMcpOAuthRecord, mcpOAuthStorePath, saveMcpOAuthRecord)
import Agent.MCP (McpProtocolPreference(..))
import qualified Agent.MCP.OAuth as OAuth
import Control.Concurrent.Async (race, wait, waitCatch, withAsync)
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , readMVar
    , tryPutMVar
    )
import Network.URI (parseURI, uriAuthority, uriRegName, uriScheme, uriUserInfo, uriQuery)
import Control.Exception.Safe (bracket, bracketOnError, finally, fromException, throwIO, tryAny)
import Control.Monad (forM_, unless, void, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Char (isAsciiLower, isDigit)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hCacheControl
    , hConnection
    , hContentType
    , methodGet
    , status200
    , status400
    , status404
    , status405
    )
import Network.HTTP.Types.URI (Query, urlEncode)
import Network.Socket
    ( AddrInfo(..), Socket, SockAddr(..), SocketType(Stream), bind, close
    , defaultHints, defaultProtocol, getAddrInfo, getSocketName, listen
    , setSocketOption, socket, SocketOption(ReuseAddr)
    )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory.OsPath as Dir
import qualified System.Entropy
import System.OsPath (OsPath)
import System.IO.Error (ioeGetErrorString, isUserError)
import System.Timeout (timeout)

data LoginOptions = LoginOptions
    { loginAdditionalScopes :: [Text]
    -- ^ Extra scopes to request on top of the selected and previously
    -- granted ones (step-up authorization).
    }
    deriving (Eq, Show)

defaultLoginOptions :: LoginOptions
defaultLoginOptions = LoginOptions { loginAdditionalScopes = [] }

data Callback = Callback
    { callbackCode :: Maybe Text
    , callbackState :: Maybe Text
    , callbackIss :: Maybe Text
    , callbackError :: Maybe Text
    , callbackErrorDescription :: Maybe Text
    }

-- | Host-owned presentation for MCP OAuth. The CLI prints to stdout; the
-- fullscreen manager shows notices and the authorization URL in overlays.
--
-- 'mcpLoginAuthorize' is called only after the loopback callback server is
-- already accepting connections, so the browser redirect can complete without
-- a later confirmation step.
data McpLoginHost = McpLoginHost
    { mcpLoginSay :: !(Text -> IO ())
    -- | Open the authorization URL and wait for the loopback callback. 'Left'
    -- cancels the login; 'Right Nothing' is a timeout.
    , mcpLoginAuthorize :: !(Text -> IO Callback -> IO (Either Text (Maybe Callback)))
    }

mcpOAuthCallbackTimeoutMicros :: Int
mcpOAuthCallbackTimeoutMicros = 5 * 60 * 1_000_000

defaultMcpLoginHost :: McpLoginHost
defaultMcpLoginHost =
    McpLoginHost
        { mcpLoginSay = putStrLn . Text.unpack
        , mcpLoginAuthorize = \url wait -> do
            opened <- openBrowser url
            unless opened $
                putStrLn
                    "Could not launch a browser automatically; open the URL above."
            Right <$> timeout mcpOAuthCallbackTimeoutMicros wait
        }

-- | Host-owned presentation and credential loading. The host must resolve the
-- previous record by immutable connection identity, never by endpoint alone.
-- Authorization returns the new record without persisting it: the owner must
-- verify that the connection and authorization generation are still current
-- before committing credentials to protected storage.
data McpOAuthHost = McpOAuthHost
    { oauthLoadPrevious :: IO (Either Text (Maybe (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)))
    , oauthOpenBrowser :: Text -> IO (Either Text ())
    -- ^ Schedule browser presentation and return promptly. Do not wait for
    -- completion: the runtime receives and validates the loopback response.
    }

-- | Runs synchronously on the caller's worker. Cancellation propagates and
-- closes the loopback listener. No credentials or authorization URLs are
-- logged or written to disk. The callback URL is passed only to presentation.
authorizeMcpWith
    :: McpOAuthHost
    -> LoginOptions
    -> Maybe McpOAuthConfig
    -> Text
    -> IO (Either Text (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra))
authorizeMcpWith host options oauthConfig serverUrl = do
    result <- tryAny $
        timeout mcpOAuthCallbackTimeoutMicros (authorizeMcp host options oauthConfig serverUrl)
    pure case result of
        Right (Just record) -> Right record
        Right Nothing -> Left "Timed out waiting for MCP authorization."
        Left exception -> Left case fromException exception of
            Just ioErr | isUserError ioErr -> Text.pack (ioeGetErrorString ioErr)
            _ -> "MCP authorization could not be completed. Check the connection and try again."

authorizeMcp
    :: McpOAuthHost
    -> LoginOptions
    -> Maybe McpOAuthConfig
    -> Text
    -> IO (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)
authorizeMcp host = runMcpOAuth OAuthWorkflowHost
    { workflowSay = const (pure ())
    , workflowError = const
    , workflowLoadPrevious = host.oauthLoadPrevious >>= \case
        Left _ -> failText "MCP credentials could not be read from protected storage."
        Right record -> pure record
    , workflowAuthorize = \url awaitCallback -> do
        host.oauthOpenBrowser url
            >>= either (const (failText "The authorization browser could not be opened.")) pure
        awaitCallback
    }

-- | Only presentation, error disclosure and credential loading vary by owner.
-- The protocol engine returns credentials; it cannot persist them or configure
-- a server. In particular the runtime owner must still check its generation
-- before committing the returned record.
data OAuthWorkflowHost = OAuthWorkflowHost
    { workflowSay :: Text -> IO ()
    , workflowError :: Text -> Text -> Text
    -- ^ Safe public summary, detailed CLI diagnostic.
    , workflowLoadPrevious :: IO (Maybe (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra))
    , workflowAuthorize :: Text -> IO Callback -> IO Callback
    }

runMcpOAuth
    :: OAuthWorkflowHost
    -> LoginOptions
    -> Maybe McpOAuthConfig
    -> Text
    -> IO (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)
runMcpOAuth host options oauthConfig serverUrl = do
    let reject :: Text -> Text -> IO a
        reject summary = failText . host.workflowError summary
    either (reject "MCP authorization requires an HTTPS endpoint without embedded credentials or a fragment.")
        pure (OAuth.validateOAuthEndpoint serverUrl)
    let resourceUri = OAuth.canonicalResourceUri serverUrl
    manager <- newTlsManager
    challenge <- OAuth.probeAuthorizationChallenge manager serverUrl >>= \case
        Left err -> do
            host.workflowSay ("Warning: " <> err <> "; falling back to well-known discovery.")
            pure Nothing
        Right probe -> do
            when (probe.probeStatus /= 401 && probe.probeStatus /= 403) $
                host.workflowSay
                    ("Note: the MCP server answered the unauthenticated probe with HTTP "
                        <> Text.pack (show probe.probeStatus) <> "; continuing with discovery.")
            pure probe.probeChallenge
    resource <- OAuth.discoverProtectedResourceMetadata manager serverUrl
        (challenge >>= (.challengeResourceMetadata))
            >>= either (reject "MCP authorization metadata could not be discovered or validated.") pure
    stored <- host.workflowLoadPrevious
    let storedIssuer = stored >>= (.extraIssuer) . snd
    issuer <- case resource.authorizationServers of
        [] -> failText "MCP protected resource metadata did not advertise an authorization server"
        first : _ -> pure case storedIssuer of
            Just previous | previous `elem` resource.authorizationServers -> previous
            _ -> first
    metadata <- OAuth.discoverAuthorizationServerMetadata manager issuer
        >>= either (reject "OAuth authorization server metadata could not be discovered or validated.") pure
    forM_ [metadata.authorizationEndpoint, metadata.tokenEndpoint] \endpoint ->
        either (reject "OAuth metadata contains an unsafe authorization or token endpoint.")
            pure (OAuth.validateOAuthEndpoint endpoint)
    either (reject "The authorization server must support S256 PKCE.") pure (OAuth.checkPkceSupport metadata)
    let recordedIssuer = fromMaybe issuer metadata.issuer
        sameIssuer = storedIssuer == Just recordedIssuer
        -- A missing legacy resource is not evidence that this record belongs
        -- to the current resource. Do not carry its client secret or scopes.
        sameResource = (stored >>= (.extraResource) . snd) == Just resourceUri
        previous = if sameIssuer && sameResource then stored else Nothing
    when (isJust stored && not sameIssuer) $
        host.workflowSay
            ("Note: the authorization server changed to " <> recordedIssuer
                <> "; the previous client registration and granted scopes will not be reused.")
    when (isJust stored && sameIssuer && not sameResource) $
        host.workflowSay
            "Note: the stored OAuth resource does not match; the previous client registration and granted scopes will not be reused."
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
        plan <- either
            (reject "The authorization server requires a registered OAuth client. Configure a client ID or client metadata URL.")
            pure (OAuth.selectClientRegistration registrationOptions metadata)
        client <- case plan of
            OAuth.UsePreRegisteredClient pre -> do
                host.workflowSay
                    "Using the pre-registered OAuth client from ~/.haskell-agent/config.json."
                pure ResolvedClient
                    { resolvedClientId = pre.preRegisteredClientId
                    , resolvedClientSecret = pre.preRegisteredClientSecret
                    , resolvedSource = OAuth.ClientIdPreRegistered
                    , resolvedMetadataUrl = Nothing
                    }
            OAuth.UseClientIdMetadataDocument url -> do
                host.workflowSay
                    ("Using the Client ID Metadata Document " <> url <> " as client_id.")
                pure ResolvedClient
                    { resolvedClientId = url
                    , resolvedClientSecret = Nothing
                    , resolvedSource = OAuth.ClientIdMetadataDocument
                    , resolvedMetadataUrl = Just url
                    }
            OAuth.ReuseDynamicRegistration clientId -> do
                host.workflowSay
                    "Reusing the dynamic client registration from the previous login."
                pure ResolvedClient
                    { resolvedClientId = clientId
                    , resolvedClientSecret = previous >>= (.extraClientSecret) . snd
                    , resolvedSource = OAuth.ClientIdDynamicRegistration
                    , resolvedMetadataUrl = Nothing
                    }
            OAuth.UseDynamicRegistration endpoint -> do
                registration <- OAuth.registerClientWith manager endpoint OAuth.ClientRegistrationRequest
                    { registrationClientName = "Haskell Agent"
                    , registrationRedirectUris = [redirect]
                    , registrationScopes = scopes
                    } >>= either (reject "OAuth client registration failed.") pure
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
        callback <-
            withListeningCallback listener
                metadata.authorizationResponseIssParameterSupported recordedIssuer state
                (host.workflowAuthorize authUrl)
        -- Validate issuer before interpreting even an error response.
        either (reject "MCP OAuth callback issuer mismatch.") pure $ OAuth.validateAuthorizationResponseIssuer
            metadata.authorizationResponseIssParameterSupported recordedIssuer callback.callbackIss
        when (callback.callbackState /= Just state) (failText "MCP OAuth callback state mismatch")
        forM_ callback.callbackError \err ->
            reject "MCP authorization was not granted."
                ("MCP authorization was not granted: " <> err
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
                OAuth.OAuthTokenFailure err -> reject "OAuth token exchange failed." err
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
                    pure (tokenFile, extra)

loginMcp :: Text -> IO ()
loginMcp = loginMcpWith defaultLoginOptions

loginMcpWith :: LoginOptions -> Text -> IO ()
loginMcpWith options serverUrl =
    loginMcpWithHost defaultMcpLoginHost options serverUrl
        >>= either failText (const (pure ()))

-- | Same flow as 'loginMcpWith', returning the success message instead of
-- throwing. Used by the TUI so authorization can stay inside the overlay.
loginMcpWithResult :: LoginOptions -> Text -> IO (Either Text Text)
loginMcpWithResult = loginMcpWithHost defaultMcpLoginHost

loginMcpWithHost
    :: McpLoginHost
    -> LoginOptions
    -> Text
    -> IO (Either Text Text)
loginMcpWithHost host options serverUrl =
    tryAny (loginMcpWithHostThrow host options serverUrl) >>= \case
        Left err -> pure (Left (formatException err))
        Right message -> pure (Right message)

-- | The @client_id@ chosen for this login and how it was obtained.
data ResolvedClient = ResolvedClient
    { resolvedClientId :: Text
    , resolvedClientSecret :: Maybe Text
    , resolvedSource :: OAuth.ClientIdSource
    , resolvedMetadataUrl :: Maybe Text
    }

loginMcpWithHostThrow
    :: McpLoginHost
    -> LoginOptions
    -> Text
    -> IO Text
loginMcpWithHostThrow host options serverUrl = do
    home <- Dir.getHomeDirectory
    harness <- loadHarnessConfig home >>= either failText pure
    let oauthConfig = lookupServerOAuthConfig serverUrl harness
        workflow = OAuthWorkflowHost
            { workflowSay = host.mcpLoginSay
            , workflowError = \_ detail -> detail
            , workflowLoadPrevious = loadMcpOAuthRecord serverUrl >>= \case
                Left err -> do
                    host.mcpLoginSay
                        ("Warning: ignoring unreadable MCP OAuth record: " <> err)
                    pure Nothing
                Right record -> pure record
            , workflowAuthorize = \url awaitCallback -> do
                host.mcpLoginSay ("Opening browser for MCP authorization: " <> url)
                host.mcpLoginAuthorize url awaitCallback >>= \case
                    Left err -> failText err
                    Right Nothing -> failText "Timed out waiting for MCP OAuth callback"
                    Right (Just received) -> pure received
            }
    (tokenFile, extra) <- runMcpOAuth workflow options oauthConfig serverUrl
    when (Text.null tokenFile.tokenRefreshToken) $
        host.mcpLoginSay
            "Warning: MCP provider returned no refresh token; reauthorization may be required."
    saveMcpOAuthRecord serverUrl tokenFile extra
        >>= either failText pure
    registerAuthorizedMcpServer home serverUrl >>= \case
        Left err -> failText
            ("MCP authorization was saved, but server registration failed: " <> err)
        Right (name, enabled) -> do
            let followUp =
                    if enabled
                        then "Start a new session to connect this server."
                        else
                            "This server remains disabled. Enable it with: agent-cli mcp enable "
                                <> shellQuote name
                message =
                    "MCP authorization saved. MCP server configured: "
                        <> name
                        <> ". "
                        <> followUp
            host.mcpLoginSay message
            pure message

-- | Register only after the token record has been persisted successfully.
-- Re-read under the configuration lock so browser-time edits are preserved.
registerAuthorizedMcpServer :: OsPath -> Text -> IO (Either Text (Text, Bool))
registerAuthorizedMcpServer home serverUrl =
    modifyHarnessConfig home (\_ -> authorizedMcpServerRegistration serverUrl)
        >>= pure . fmap (\(_, _, result) -> result)

-- | Preserve the configured server when the URL spelling changes. Its URL
-- must be updated to the exact key under which login saved the credential.
authorizedMcpServerRegistration
    :: Text -> HarnessConfig -> Either Text (HarnessConfig, (Text, Bool))
authorizedMcpServerRegistration serverUrl config = do
    uri <- maybe (Left "MCP server URL is invalid") Right (parseURI (Text.unpack serverUrl))
    authority <- maybe (Left "MCP server URL requires a host") Right (uriAuthority uri)
    if uriScheme uri `notElem` ["https:", "http:"] || null (uriRegName authority)
        || not (null (uriUserInfo authority))
        then Left "MCP server URL must be an HTTP URL without user information"
        else case [(name, server) | (name, server) <- Map.toAscList config.configMcpServers,
                    maybe False (sameMcpEndpoint serverUrl) server.mcpUrl] of
            (name, server) : _ -> Right
                ( config { configMcpServers = Map.insert name
                    (server { mcpUrl = Just serverUrl }) config.configMcpServers }
                , (name, server.mcpEnabled)
                )
            [] ->
                let hostName = Text.toLower (Text.pack (uriRegName authority))
                    baseName = Text.map sanitize hostName
                    name = availableName baseName 1
                    server = McpServerConfig
                        { mcpEnabled = True
                        , mcpUrl = Just serverUrl
                        , mcpConnectionId = Nothing
                        , mcpConnectionCredentials = Nothing
                        , mcpConnectionGeneration = Nothing
                        , mcpDisplayName = Nothing
                        , mcpCommand = ""
                        , mcpArgs = []
                        , mcpCwd = Nothing
                        , mcpEnv = Map.empty
                        , mcpStartupTimeoutSeconds = 30
                        , mcpRequestTimeoutSeconds = 60
                        , mcpOAuth = Nothing
                        , mcpProtocol = McpProtocolAuto
                        , mcpRoots = False
                        , mcpSampling = False
                        , mcpLogLevel = Nothing
                        }
                in Right
                    ( config { configMcpServers = Map.insert name server config.configMcpServers }
                    , (name, True)
                    )
  where
    sanitize character
        | isAsciiLower character || isDigit character || character == '-' = character
        | otherwise = '-'
    availableName baseName (suffix :: Int) =
        let candidate = if suffix == 1 then baseName else baseName <> "-" <> Text.pack (show suffix)
        in if Map.member candidate config.configMcpServers
            then availableName baseName (suffix + 1)
            else candidate

-- Server names can be user-selected, so quote the suggested shell command.
shellQuote :: Text -> Text
shellQuote value = "'" <> Text.replace "'" "'\\''" value <> "'"

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
    matches server = maybe False (sameMcpEndpoint serverUrl) server.mcpUrl

-- | Use the resource URI's spelling normalization, but retain the complete
-- query: different queries can identify different accounts or MCP endpoints.
-- Invalid URLs only match exactly, never through the resource URI fallback.
sameMcpEndpoint :: Text -> Text -> Bool
sameMcpEndpoint left right =
    left == right || case (parseURI (Text.unpack left), parseURI (Text.unpack right)) of
        (Just leftUri, Just rightUri) ->
            OAuth.canonicalResourceUri left == OAuth.canonicalResourceUri right
                && uriQuery leftUri == uriQuery rightUri
        _ -> False

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

-- | Run the loopback HTTP server and invoke the action only after Warp is
-- accepting connections, so the browser redirect cannot lose the race.
-- Invalid callbacks are rejected without completing the pending authorization.
withListeningCallback
    :: Socket -> Bool -> Text -> Text -> (IO Callback -> IO a) -> IO a
withListeningCallback listener issuerRequired issuer expectedState action = do
    readyVar <- newEmptyMVar
    withAsync (receiveCallback listener readyVar issuerRequired issuer expectedState)
        \callbackAsync -> do
            race (waitCatch callbackAsync) (readMVar readyVar) >>= \case
                Left (Left err) -> throwIO err
                Left (Right _) ->
                    failText
                        "MCP OAuth callback server exited before accepting connections"
                Right () -> action (wait callbackAsync)

receiveCallback :: Socket -> MVar () -> Bool -> Text -> Text -> IO Callback
receiveCallback listener readyVar issuerRequired issuer expectedState = do
    resultVar <- newEmptyMVar
    shutdownVar <- newEmptyMVar
    let settings =
            Warp.setHost "127.0.0.1"
                $ Warp.setMaxTotalHeaderLength 8_192
                $ Warp.setInstallShutdownHandler (putMVar shutdownVar)
                $ Warp.setBeforeMainLoop (void $ tryPutMVar readyVar ())
                    Warp.defaultSettings
        application request respond
            | Wai.requestMethod request /= methodGet =
                respond (plainResponse status405 "Method Not Allowed")
            | Wai.rawPathInfo request /= "/callback" =
                respond (plainResponse status404 "Not Found")
            | Left _ <- validateMcpOAuthCallback issuerRequired issuer expectedState
                (Wai.queryString request) =
                respond (plainResponse status400 "Invalid authorization response")
            | otherwise = do
                let callback = callbackFromQuery (Wai.queryString request)
                    finish = do
                        void (tryPutMVar resultVar callback)
                        readMVar shutdownVar >>= id
                respond
                    (Wai.responseLBS
                        status200
                        [ (hContentType, "text/html; charset=utf-8")
                        , (hCacheControl, "no-store")
                        , (hConnection, "close")
                        , ("Content-Security-Policy", "default-src 'none'; style-src 'unsafe-inline'")
                        ]
                        OAuth.oauthCallbackSuccessPage)
                    `finally` finish
    Warp.runSettingsSocket settings listener application
    readMVar resultVar
  where
    plainResponse status body =
        Wai.responseLBS
            status
            [ (hContentType, "text/plain; charset=utf-8")
            , (hConnection, "close")
            ]
            body

-- | Validate untrusted callbacks before consuming the pending authorization.
-- Duplicate security parameters are rejected rather than interpreted using
-- first-value semantics. Error responses still require issuer and state.
validateMcpOAuthCallback :: Bool -> Text -> Text -> Query -> Either Text ()
validateMcpOAuthCallback issuerRequired issuer expectedState query = do
    forM_ ["code", "state", "iss", "error", "error_description"] \name -> do
        when (length (filter ((== name) . fst) query) > 1)
            (Left "Duplicate OAuth callback parameter")
        forM_ (filter ((== name) . fst) query) \(_, value) ->
            case value of
                Nothing -> Left "Missing OAuth callback parameter value"
                Just bytes -> either (const (Left "Invalid OAuth callback encoding")) (const (Right ()))
                    (Encoding.decodeUtf8' bytes)
    let callback = callbackFromQuery query
    either (const (Left "MCP OAuth callback issuer mismatch")) Right $
        OAuth.validateAuthorizationResponseIssuer issuerRequired issuer callback.callbackIss
    when (callback.callbackState /= Just expectedState)
        (Left "MCP OAuth callback state mismatch")
    when (isJust callback.callbackCode == isJust callback.callbackError)
        (Left "OAuth callback must contain exactly one code or error")
    when (callback.callbackCode == Just "" || callback.callbackError == Just "")
        (Left "OAuth callback contains an empty result")

callbackFromQuery :: Query -> Callback
callbackFromQuery query = Callback
    { callbackCode = parameter "code"
    , callbackState = parameter "state"
    , callbackIss = parameter "iss"
    , callbackError = parameter "error"
    , callbackErrorDescription = parameter "error_description"
    }
  where
    parameter name =
        lookup name query >>= id >>= either (const Nothing) Just . Encoding.decodeUtf8'

randomUrlBytes :: Int -> IO Text
randomUrlBytes n = do
    bytes <- System.Entropy.getEntropy n
    pure (Encoding.decodeUtf8 (Base64.encodeUnpadded bytes))

encode :: Text -> Text
encode = Encoding.decodeUtf8 . urlEncode True . Encoding.encodeUtf8

failText :: Text -> IO a
failText = ioError . userError . Text.unpack
