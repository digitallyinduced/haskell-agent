-- | OAuth 2.1 client support for remote MCP servers, following the MCP
-- authorization specification (revision 2026-07-28):
--
-- * @WWW-Authenticate@ challenge parsing (RFC 6750)
-- * Protected Resource Metadata discovery (RFC 9728)
-- * Authorization Server Metadata discovery (RFC 8414 / OpenID Connect
--   Discovery) with issuer validation
-- * PKCE capability checks, client registration selection (pre-registered,
--   Client ID Metadata Documents, Dynamic Client Registration)
-- * Scope selection and step-up unions
-- * Authorization response issuer validation (RFC 9207)
-- * Resource indicators (RFC 8707) on authorization, token, and refresh
--   requests
--
-- Secrets never appear in 'Show' output or error text produced here.
module Agent.MCP.OAuth
    ( -- * Tokens and metadata
      OAuthTokens(..), OAuthTokenResponse(..), ProtectedResourceMetadata(..)
    , AuthorizationServerMetadata(..), ClientRegistration(..)
      -- * WWW-Authenticate challenges
    , WwwAuthenticateChallenge(..), parseWwwAuthenticate, challengeScopes
    , AuthorizationProbe(..), probeAuthorizationChallenge
      -- * Canonical resource URIs
    , canonicalResourceUri, resourceOrigin, loopbackRedirectPort
      -- * Protected resource metadata discovery
    , protectedResourceMetadataUrls, discoverProtectedResourceMetadata
    , validateResourceMetadata, discoverProtectedResource
      -- * Authorization server metadata discovery
    , authorizationServerMetadataUrls, discoverAuthorizationServerMetadata
    , validateIssuer, discoverAuthorizationServer
      -- * PKCE
    , checkPkceSupport
      -- * Client registration
    , ClientIdSource(..), clientIdSourceText, parseClientIdSource
    , PreRegisteredClient(..), StoredClient(..), RegistrationOptions(..)
    , RegistrationPlan(..), selectClientRegistration, validateClientIdMetadataUrl
    , ClientRegistrationRequest(..), clientRegistrationPayload
    , registerClientWith, registerClient
      -- * Scopes
    , ScopeSources(..), selectScopes, unionScopes, ScopePlan(..), planScopes
    , offlineAccessScope
      -- * Authorization response validation
    , validateAuthorizationResponseIssuer
      -- * Token endpoint requests
    , TokenExchange(..), exchangeAuthorizationCodeWith, exchangeAuthorizationCode
    , RefreshRequest(..), refreshAccessTokenWith, refreshAccessToken
    , oauthCallbackSuccessPage
      -- * Persisted token records
    , OAuthTokenFile(..), OAuthTokenFileExtra(..), emptyOAuthTokenFileExtra
    , loadOAuthTokenFile, loadOAuthTokenFileExtra, loadOAuthTokenRecord
    , decodeOAuthTokenRecord, encodeOAuthTokenRecord, refreshOAuthTokenFile
    ) where

import Agent.FileRetry (writeLazyFileAtomically)
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (guard)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as EncodingError
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager, RequestBody(..), httpLbs, parseRequest, responseBody, responseStatus, urlEncodedBody)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (statusCode)
import qualified System.FileLock as FileLock
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (ownerReadMode, ownerWriteMode)
import System.Posix.IO (OpenFileFlags(..), OpenMode(ReadWrite), closeFd, defaultFileFlags, openFd)

-- ---------------------------------------------------------------------------
-- Tokens

data OAuthTokens = OAuthTokens
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , expiresIn :: !(Maybe Int)
    , scope :: !(Maybe Text)
    -- ^ Granted scope reported by the token endpoint, when it differs from
    -- (or simply restates) the requested scope.
    } deriving (Eq)

instance Show OAuthTokens where
    show tokens = "OAuthTokens { accessToken = <redacted>, refreshToken = "
        <> show (isJust tokens.refreshToken) <> ", scope = " <> show tokens.scope <> " }"

instance Aeson.FromJSON OAuthTokens where
    parseJSON = Aeson.withObject "OAuthTokens" $ \object ->
        OAuthTokens
            <$> object Aeson..: "access_token"
            <*> object Aeson..:? "refresh_token"
            <*> object Aeson..:? "expires_in"
            <*> object Aeson..:? "scope"

data OAuthTokenResponse = OAuthTokenSuccess !OAuthTokens | OAuthTokenFailure !Text deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Persisted token records

-- | Fields the wire client depends on positionally. The constructor arity is
-- part of the compatibility contract; new persisted fields live in
-- 'OAuthTokenFileExtra' and share the same JSON document.
data OAuthTokenFile = OAuthTokenFile
    { tokenClientId :: !Text
    , tokenEndpoint :: !Text
    , tokenAccessToken :: !Text
    , tokenRefreshToken :: !Text
    , tokenExpiresAt :: !(Maybe Int)
    } deriving (Eq)

instance Show OAuthTokenFile where
    show _ = "OAuthTokenFile { tokens = <redacted> }"

instance Aeson.FromJSON OAuthTokenFile where
    parseJSON = Aeson.withObject "OAuthTokenFile" $ \o -> OAuthTokenFile
        <$> o Aeson..: "client_id" <*> o Aeson..: "token_endpoint"
        <*> o Aeson..: "access_token" <*> o Aeson..: "refresh_token"
        <*> o Aeson..:? "expires_at"

instance Aeson.ToJSON OAuthTokenFile where
    toJSON = Aeson.object . tokenFilePairs

tokenFilePairs :: OAuthTokenFile -> [AesonTypes.Pair]
tokenFilePairs t =
    [ "client_id" Aeson..= t.tokenClientId
    , "token_endpoint" Aeson..= t.tokenEndpoint
    , "access_token" Aeson..= t.tokenAccessToken
    , "refresh_token" Aeson..= t.tokenRefreshToken
    , "expires_at" Aeson..= t.tokenExpiresAt
    ]

-- | Where a @client_id@ came from. Credentials are bound to the issuer that
-- produced them; the source decides whether they may be reused.
data ClientIdSource
    = ClientIdPreRegistered
    | ClientIdMetadataDocument
    | ClientIdDynamicRegistration
    deriving (Eq, Show)

clientIdSourceText :: ClientIdSource -> Text
clientIdSourceText = \case
    ClientIdPreRegistered -> "pre_registered"
    ClientIdMetadataDocument -> "client_id_metadata_document"
    ClientIdDynamicRegistration -> "dynamic_registration"

parseClientIdSource :: Text -> Maybe ClientIdSource
parseClientIdSource = \case
    "pre_registered" -> Just ClientIdPreRegistered
    "client_id_metadata_document" -> Just ClientIdMetadataDocument
    "dynamic_registration" -> Just ClientIdDynamicRegistration
    _ -> Nothing

-- | Additional persisted state stored in the same JSON document as
-- 'OAuthTokenFile'. Every key is optional so records written before these
-- fields existed still load.
data OAuthTokenFileExtra = OAuthTokenFileExtra
    { extraIssuer :: !(Maybe Text)
    -- ^ Validated @issuer@ of the authorization server that issued the tokens.
    , extraScope :: !(Maybe Text)
    -- ^ Space-separated scopes granted for the current tokens.
    , extraResource :: !(Maybe Text)
    -- ^ Canonical MCP server URI sent as the RFC 8707 @resource@.
    , extraClientIdSource :: !(Maybe ClientIdSource)
    , extraClientIdMetadataUrl :: !(Maybe Text)
    , extraClientSecret :: !(Maybe Text)
    -- ^ Pre-registered confidential client secret, sent as @client_secret@.
    , extraRedirectUri :: !(Maybe Text)
    -- ^ Redirect URI the dynamic registration was created with.
    } deriving (Eq)

instance Show OAuthTokenFileExtra where
    show extra = "OAuthTokenFileExtra { extraIssuer = " <> show extra.extraIssuer
        <> ", extraScope = " <> show extra.extraScope
        <> ", extraResource = " <> show extra.extraResource
        <> ", extraClientIdSource = " <> show extra.extraClientIdSource
        <> ", extraClientIdMetadataUrl = " <> show extra.extraClientIdMetadataUrl
        <> ", extraClientSecret = " <> (if isJust extra.extraClientSecret then "<redacted>" else "Nothing")
        <> ", extraRedirectUri = " <> show extra.extraRedirectUri <> " }"

emptyOAuthTokenFileExtra :: OAuthTokenFileExtra
emptyOAuthTokenFileExtra = OAuthTokenFileExtra Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Aeson.FromJSON OAuthTokenFileExtra where
    parseJSON = Aeson.withObject "OAuthTokenFileExtra" $ \o -> do
        source <- o Aeson..:? "client_id_source"
        OAuthTokenFileExtra
            <$> o Aeson..:? "issuer"
            <*> o Aeson..:? "scope"
            <*> o Aeson..:? "resource"
            <*> pure (source >>= parseClientIdSource)
            <*> o Aeson..:? "client_id_metadata_url"
            <*> o Aeson..:? "client_secret"
            <*> o Aeson..:? "redirect_uri"

tokenExtraPairs :: OAuthTokenFileExtra -> [AesonTypes.Pair]
tokenExtraPairs extra = mapMaybe id
    [ ("issuer" Aeson..=) <$> extra.extraIssuer
    , ("scope" Aeson..=) <$> extra.extraScope
    , ("resource" Aeson..=) <$> extra.extraResource
    , ("client_id_source" Aeson..=) . clientIdSourceText <$> extra.extraClientIdSource
    , ("client_id_metadata_url" Aeson..=) <$> extra.extraClientIdMetadataUrl
    , ("client_secret" Aeson..=) <$> extra.extraClientSecret
    , ("redirect_uri" Aeson..=) <$> extra.extraRedirectUri
    ]

-- | Serialise the compatibility fields and the extra fields into one document.
encodeOAuthTokenRecord :: OAuthTokenFile -> OAuthTokenFileExtra -> LBS.ByteString
encodeOAuthTokenRecord file extra = Aeson.encode (Aeson.object (tokenFilePairs file <> tokenExtraPairs extra))

decodeOAuthTokenRecord :: LBS.ByteString -> Either Text (OAuthTokenFile, OAuthTokenFileExtra)
decodeOAuthTokenRecord bytes = do
    file <- either (Left . Text.pack) Right (Aeson.eitherDecode bytes)
    extra <- either (Left . Text.pack) Right (Aeson.eitherDecode bytes)
    pure (file, extra)

loadOAuthTokenFile :: FilePath -> IO (Either Text OAuthTokenFile)
loadOAuthTokenFile path = fmap fst <$> loadOAuthTokenRecord path

loadOAuthTokenFileExtra :: FilePath -> IO (Either Text OAuthTokenFileExtra)
loadOAuthTokenFileExtra path = fmap snd <$> loadOAuthTokenRecord path

loadOAuthTokenRecord :: FilePath -> IO (Either Text (OAuthTokenFile, OAuthTokenFileExtra))
loadOAuthTokenRecord path = do
    result <- tryAny (LBS.readFile path)
    pure $ case result of
        Left exception -> Left ("MCP OAuth token file unavailable: " <> Text.pack (show exception))
        Right bytes -> decodeOAuthTokenRecord bytes

-- | Exchange the stored refresh token and persist the rotated record. The
-- RFC 8707 @resource@ and any pre-registered @client_secret@ recorded in the
-- extra fields are included in the refresh request.
refreshOAuthTokenFile :: Manager -> FilePath -> IO (Either Text OAuthTokenFile)
refreshOAuthTokenFile manager path = withMVar oauthRefreshLock $ \_ ->
    withOAuthFileLock path $
    loadOAuthTokenRecord path >>= \case
        Left err -> pure (Left err)
        Right (current, extra) -> refreshAccessTokenWith manager RefreshRequest
            { refreshEndpoint = current.tokenEndpoint
            , refreshClientId = current.tokenClientId
            , refreshClientSecret = extra.extraClientSecret
            , refreshRefreshToken = current.tokenRefreshToken
            , refreshResource = extra.extraResource
            } >>= \case
            OAuthTokenFailure err -> pure (Left err)
            OAuthTokenSuccess tokens -> do
                now :: Int <- round <$> getPOSIXTime
                let updated = current
                        { tokenAccessToken = tokens.accessToken
                        , tokenRefreshToken = fromMaybe current.tokenRefreshToken tokens.refreshToken
                        , tokenExpiresAt = fmap (now +) tokens.expiresIn
                        }
                    updatedExtra = extra { extraScope = tokens.scope <|> extra.extraScope }
                writeLazyFileAtomically (unsafeEncodeUtf path) (ownerReadMode .|. ownerWriteMode)
                    (encodeOAuthTokenRecord updated updatedExtra)
                pure (Right updated)

withOAuthFileLock :: FilePath -> IO a -> IO a
withOAuthFileLock tokenPath action = do
    let lockPath = tokenPath <> ".refresh.lock"
    bracket
        (openFd lockPath ReadWrite defaultFileFlags { creat = Just 0o600, cloexec = True })
        closeFd
        (const (FileLock.withFileLock lockPath FileLock.Exclusive (const action)))

oauthRefreshLock :: MVar ()
oauthRefreshLock = unsafePerformIO (newMVar ())
{-# NOINLINE oauthRefreshLock #-}

-- ---------------------------------------------------------------------------
-- WWW-Authenticate challenges (RFC 6750 section 3)

data WwwAuthenticateChallenge = WwwAuthenticateChallenge
    { challengeResourceMetadata :: !(Maybe Text)
    , challengeScope :: !(Maybe Text)
    , challengeError :: !(Maybe Text)
    , challengeErrorDescription :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Parse the first @Bearer@ challenge of a @WWW-Authenticate@ header value.
-- Parameter names are case-insensitive; values may be quoted strings (with
-- backslash escapes) or bare tokens. Other schemes in the same header are
-- skipped.
parseWwwAuthenticate :: BS.ByteString -> Maybe WwwAuthenticateChallenge
parseWwwAuthenticate header =
    case [params | (scheme, params) <- challenges, BS8.map toLower scheme == "bearer"] of
        params : _ -> Just WwwAuthenticateChallenge
            { challengeResourceMetadata = param "resource_metadata" params
            , challengeScope = param "scope" params
            , challengeError = param "error" params
            , challengeErrorDescription = param "error_description" params
            }
        [] -> Nothing
  where
    challenges = groupChallenges (lexChallenge header)
    param name params = decodeLenient <$> lookup name [(BS8.map toLower key, value) | (key, value) <- params]

challengeScopes :: WwwAuthenticateChallenge -> [Text]
challengeScopes challenge = maybe [] Text.words challenge.challengeScope

data ChallengeToken = SchemeToken !BS.ByteString | ParamToken !BS.ByteString !BS.ByteString

lexChallenge :: BS.ByteString -> [ChallengeToken]
lexChallenge input =
    let rest = BS8.dropWhile (\c -> isSpace c || c == ',') input
    in if BS.null rest then [] else
        let (word, afterWord) = BS8.span isTokenChar rest
            afterSpaces = BS8.dropWhile (\c -> c == ' ' || c == '\t') afterWord
        in if BS.null word
            then lexChallenge (BS.drop 1 rest)
            else case BS8.uncons afterSpaces of
                Just ('=', afterEquals) ->
                    let (value, remaining) = lexValue (BS8.dropWhile (\c -> c == ' ' || c == '\t') afterEquals)
                    in ParamToken word value : lexChallenge remaining
                _ -> SchemeToken word : lexChallenge afterWord

-- | Quoted strings honour backslash escapes; bare values extend to the next
-- comma or whitespace so servers emitting unquoted URLs still parse.
lexValue :: BS.ByteString -> (BS.ByteString, BS.ByteString)
lexValue input = case BS8.uncons input of
    Just ('"', quoted) -> quotedString quoted []
    _ -> BS8.span (\c -> c /= ',' && not (isSpace c)) input
  where
    quotedString remaining acc = case BS8.uncons remaining of
        Nothing -> (BS8.pack (reverse acc), BS.empty)
        Just ('"', after) -> (BS8.pack (reverse acc), after)
        Just ('\\', after) -> case BS8.uncons after of
            Just (escaped, after') -> quotedString after' (escaped : acc)
            Nothing -> (BS8.pack (reverse acc), BS.empty)
        Just (c, after) -> quotedString after (c : acc)

isTokenChar :: Char -> Bool
isTokenChar c = isAlphaNum c || c `elem` ("!#$%&'*+-.^_`|~" :: String)

groupChallenges :: [ChallengeToken] -> [(BS.ByteString, [(BS.ByteString, BS.ByteString)])]
groupChallenges = reverse . map (fmap reverse) . foldl' step []
  where
    step acc (SchemeToken scheme) = (scheme, []) : acc
    step ((scheme, params) : acc) (ParamToken key value) = (scheme, (key, value) : params) : acc
    step [] (ParamToken _ _) = []

decodeLenient :: BS.ByteString -> Text
decodeLenient = Encoding.decodeUtf8With EncodingError.lenientDecode

-- | Outcome of an unauthenticated request to the MCP endpoint.
data AuthorizationProbe = AuthorizationProbe
    { probeStatus :: !Int
    , probeChallenge :: !(Maybe WwwAuthenticateChallenge)
    } deriving (Eq, Show)

-- | Send a minimal unauthenticated JSON-RPC request to the MCP endpoint and
-- read the @WWW-Authenticate@ challenge from the response, if any.
probeAuthorizationChallenge :: Manager -> Text -> IO (Either Text AuthorizationProbe)
probeAuthorizationChallenge manager endpoint = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack endpoint)
        let payload = Aeson.encode $ Aeson.object
                [ "jsonrpc" Aeson..= ("2.0" :: Text)
                , "id" Aeson..= (0 :: Int)
                , "method" Aeson..= ("server/discover" :: Text)
                , "params" Aeson..= Aeson.object []
                ]
            request' = request
                { HC.method = "POST"
                , HC.requestBody = RequestBodyLBS payload
                , HC.requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Accept", "application/json, text/event-stream")
                    , ("MCP-Protocol-Version", "2026-07-28")
                    ]
                }
        httpLbs request' manager
    pure $ case result of
        Left exception -> Left ("MCP authorization probe failed: " <> Text.pack (show exception))
        Right response ->
            let challenges = [value | (name, value) <- HC.responseHeaders response, name == "WWW-Authenticate"]
            in Right AuthorizationProbe
                { probeStatus = statusCode (responseStatus response)
                , probeChallenge = case mapMaybe parseWwwAuthenticate challenges of
                    challenge : _ -> Just challenge
                    [] -> Nothing
                }

-- ---------------------------------------------------------------------------
-- URL helpers

data UrlParts = UrlParts
    { urlScheme :: !Text
    , urlAuthority :: !Text
    , urlPath :: !Text
    }

parseUrlParts :: Text -> Maybe UrlParts
parseUrlParts url = do
    let (scheme, rest) = Text.breakOn "://" url
    guard (not (Text.null scheme) && Text.all isSchemeChar scheme && not (Text.null rest))
    let afterScheme = Text.drop 3 rest
        (authority, pathQuery) = Text.break (\c -> c == '/' || c == '?' || c == '#') afterScheme
    guard (not (Text.null authority))
    let path = Text.takeWhile (\c -> c /= '?' && c /= '#') pathQuery
    pure UrlParts
        { urlScheme = Text.toLower scheme
        , urlAuthority = Text.toLower authority
        , urlPath = Text.dropWhileEnd (== '/') path
        }
  where
    isSchemeChar c = isAlphaNum c || c == '+' || c == '-' || c == '.'

urlPartsOrigin :: UrlParts -> Text
urlPartsOrigin parts = parts.urlScheme <> "://" <> parts.urlAuthority

-- | Canonical MCP server URI for RFC 8707 resource indicators: lowercase
-- scheme and host, explicit port kept, path kept, no query, fragment, or
-- trailing slash.
canonicalResourceUri :: Text -> Text
canonicalResourceUri url = case parseUrlParts url of
    Just parts -> urlPartsOrigin parts <> parts.urlPath
    Nothing -> Text.dropWhileEnd (== '/') (Text.takeWhile (\c -> c /= '#' && c /= '?') url)

-- | @scheme://host[:port]@ of a URL.
resourceOrigin :: Text -> Maybe Text
resourceOrigin url = urlPartsOrigin <$> parseUrlParts url

-- | Port of a loopback redirect URI such as @http://127.0.0.1:43127/callback@.
loopbackRedirectPort :: Text -> Maybe Int
loopbackRedirectPort url = do
    parts <- parseUrlParts url
    guard (parts.urlScheme == "http")
    let (host, portText) = Text.breakOn ":" parts.urlAuthority
    guard (host `elem` ["127.0.0.1", "localhost", "[::1]"])
    digits <- Text.stripPrefix ":" portText
    guard (not (Text.null digits) && Text.all (`elem` ['0' .. '9']) digits)
    let port = read (Text.unpack digits)
    guard (port > 0 && port < 65536)
    pure port

-- | Authorization server endpoints and metadata must be HTTPS; plain HTTP is
-- tolerated only for loopback development servers.
checkSecureUrl :: Text -> Text -> Either Text ()
checkSecureUrl label url = case parseUrlParts url of
    Nothing -> Left (label <> " is not an absolute URL: " <> url)
    Just parts
        | parts.urlScheme == "https" -> Right ()
        | parts.urlScheme == "http" && isLoopback parts.urlAuthority -> Right ()
        | otherwise -> Left (label <> " must use https: " <> url)
  where
    isLoopback authority =
        let host = Text.takeWhile (/= ':') (Text.takeWhileEnd (/= '@') authority)
        in host `elem` ["localhost", "127.0.0.1", "[::1]"]

-- ---------------------------------------------------------------------------
-- Protected resource metadata (RFC 9728)

data ProtectedResourceMetadata = ProtectedResourceMetadata
    { resource :: !(Maybe Text)
    , authorizationServers :: ![Text]
    , scopesSupported :: ![Text]
    } deriving (Eq, Show)

instance Aeson.FromJSON ProtectedResourceMetadata where
    parseJSON = Aeson.withObject "ProtectedResourceMetadata" $ \o -> ProtectedResourceMetadata
        <$> o Aeson..:? "resource"
        <*> o Aeson..:? "authorization_servers" Aeson..!= []
        <*> o Aeson..:? "scopes_supported" Aeson..!= []

-- | Well-known candidate URLs for an MCP endpoint, path-aware first, then the
-- host root.
protectedResourceMetadataUrls :: Text -> [Text]
protectedResourceMetadataUrls endpoint = case parseUrlParts endpoint of
    Nothing -> []
    Just parts ->
        let root = urlPartsOrigin parts <> "/.well-known/oauth-protected-resource"
        in if Text.null parts.urlPath then [root] else [root <> parts.urlPath, root]

-- | Reject a metadata document whose @resource@ names a different server than
-- the one being authorized. A document served for the host root may describe
-- the origin of the endpoint.
validateResourceMetadata :: Text -> ProtectedResourceMetadata -> Either Text ProtectedResourceMetadata
validateResourceMetadata endpoint metadata = case metadata.resource of
    Nothing -> Right metadata
    Just declared
        | canonicalResourceUri declared == canonical -> Right metadata
        | Just (canonicalResourceUri declared) == resourceOrigin canonical -> Right metadata
        | otherwise -> Left
            ("Protected resource metadata resource " <> declared
                <> " does not match the MCP server " <> canonical)
  where
    canonical = canonicalResourceUri endpoint

-- | Discover protected resource metadata for an MCP endpoint, using the
-- @resource_metadata@ URL from the challenge when present and probing the
-- well-known URIs otherwise.
discoverProtectedResourceMetadata :: Manager -> Text -> Maybe Text -> IO (Either Text ProtectedResourceMetadata)
discoverProtectedResourceMetadata manager endpoint challengeUrl =
    case challengeUrl of
        Just url -> case checkSecureUrl "WWW-Authenticate resource_metadata" url of
            Left err -> pure (Left err)
            Right () -> attempt [url]
        Nothing -> case protectedResourceMetadataUrls endpoint of
            [] -> pure (Left ("MCP server URL is not an absolute URL: " <> endpoint))
            urls -> attempt urls
  where
    attempt = tryCandidates "Protected resource metadata discovery failed"
        (\url -> fmap (>>= validateResourceMetadata endpoint) (getJson manager url))

-- | Compatibility wrapper: well-known discovery without a challenge URL.
discoverProtectedResource :: Manager -> Text -> IO (Either Text ProtectedResourceMetadata)
discoverProtectedResource manager resourceUrl = discoverProtectedResourceMetadata manager resourceUrl Nothing

-- ---------------------------------------------------------------------------
-- Authorization server metadata (RFC 8414 / OpenID Connect Discovery)

data AuthorizationServerMetadata = AuthorizationServerMetadata
    { issuer :: !(Maybe Text)
    , authorizationEndpoint :: !Text
    , tokenEndpoint :: !Text
    , registrationEndpoint :: !(Maybe Text)
    , codeChallengeMethodsSupported :: !(Maybe [Text])
    -- ^ 'Nothing' when the field is absent, which means PKCE is unsupported.
    , scopesSupportedByServer :: ![Text]
    , clientIdMetadataDocumentSupported :: !Bool
    , authorizationResponseIssParameterSupported :: !Bool
    , grantTypesSupported :: ![Text]
    } deriving (Eq, Show)

instance Aeson.FromJSON AuthorizationServerMetadata where
    parseJSON = Aeson.withObject "AuthorizationServerMetadata" $ \o -> AuthorizationServerMetadata
        <$> o Aeson..:? "issuer"
        <*> o Aeson..: "authorization_endpoint"
        <*> o Aeson..: "token_endpoint"
        <*> o Aeson..:? "registration_endpoint"
        <*> o Aeson..:? "code_challenge_methods_supported"
        <*> o Aeson..:? "scopes_supported" Aeson..!= []
        <*> o Aeson..:? "client_id_metadata_document_supported" Aeson..!= False
        <*> o Aeson..:? "authorization_response_iss_parameter_supported" Aeson..!= False
        <*> o Aeson..:? "grant_types_supported" Aeson..!= []

-- | Candidate metadata URLs for an issuer in the order the MCP specification
-- requires: RFC 8414 path insertion, OpenID path insertion, then OpenID path
-- appending (the latter only for issuers with a path component).
authorizationServerMetadataUrls :: Text -> [Text]
authorizationServerMetadataUrls issuerUrl = case parseUrlParts issuerUrl of
    Nothing -> []
    Just parts ->
        let origin = urlPartsOrigin parts
            path = parts.urlPath
        in if Text.null path
            then
                [ origin <> "/.well-known/oauth-authorization-server"
                , origin <> "/.well-known/openid-configuration"
                ]
            else
                [ origin <> "/.well-known/oauth-authorization-server" <> path
                , origin <> "/.well-known/openid-configuration" <> path
                , origin <> path <> "/.well-known/openid-configuration"
                ]

-- | RFC 8414 section 3.3: the document's @issuer@ must be identical to the
-- issuer identifier used to build the well-known URL.
validateIssuer :: Text -> AuthorizationServerMetadata -> Either Text AuthorizationServerMetadata
validateIssuer expected metadata = case metadata.issuer of
    Nothing -> Left ("Authorization server metadata for " <> expected <> " omits issuer")
    Just actual
        | actual == expected -> Right metadata
        | otherwise -> Left
            ("Authorization server metadata issuer " <> actual
                <> " does not match the expected issuer " <> expected)

discoverAuthorizationServerMetadata :: Manager -> Text -> IO (Either Text AuthorizationServerMetadata)
discoverAuthorizationServerMetadata manager issuerUrl =
    case checkSecureUrl "Authorization server issuer" issuerUrl of
        Left err -> pure (Left err)
        Right () -> case authorizationServerMetadataUrls issuerUrl of
            [] -> pure (Left ("Authorization server issuer is not an absolute URL: " <> issuerUrl))
            urls -> tryCandidates "Authorization server metadata discovery failed"
                (\url -> fmap (>>= validateIssuer issuerUrl) (getJson manager url)) urls

-- | Compatibility wrapper around 'discoverAuthorizationServerMetadata'.
discoverAuthorizationServer :: Manager -> Text -> IO (Either Text AuthorizationServerMetadata)
discoverAuthorizationServer = discoverAuthorizationServerMetadata

-- ---------------------------------------------------------------------------
-- PKCE

-- | The authorization flow must not proceed unless the server advertises the
-- @S256@ code challenge method.
checkPkceSupport :: AuthorizationServerMetadata -> Either Text ()
checkPkceSupport metadata = case metadata.codeChallengeMethodsSupported of
    Nothing -> Left
        "Authorization server metadata omits code_challenge_methods_supported; PKCE is required for MCP authorization"
    Just methods
        | "S256" `elem` methods -> Right ()
        | otherwise -> Left
            ("Authorization server does not support the S256 PKCE method (advertised: "
                <> Text.intercalate ", " methods <> ")")

-- ---------------------------------------------------------------------------
-- Client registration

data ClientRegistration = ClientRegistration { clientId :: !Text, clientSecret :: !(Maybe Text) } deriving (Eq)
instance Show ClientRegistration where
    show registration = "ClientRegistration { clientId = <redacted>, clientSecret = " <> show (isJust registration.clientSecret) <> " }"
instance Aeson.FromJSON ClientRegistration where
    parseJSON = Aeson.withObject "ClientRegistration" $ \o -> ClientRegistration <$> o Aeson..: "client_id" <*> o Aeson..:? "client_secret"

-- | Statically configured client credentials.
data PreRegisteredClient = PreRegisteredClient
    { preRegisteredClientId :: !Text
    , preRegisteredClientSecret :: !(Maybe Text)
    } deriving (Eq)

instance Show PreRegisteredClient where
    show client = "PreRegisteredClient { preRegisteredClientId = " <> show client.preRegisteredClientId
        <> ", preRegisteredClientSecret = " <> (if isJust client.preRegisteredClientSecret then "<redacted>" else "Nothing") <> " }"

-- | Client credentials persisted from an earlier login.
data StoredClient = StoredClient
    { storedIssuer :: !Text
    , storedClientId :: !Text
    , storedSource :: !ClientIdSource
    , storedRedirectUri :: !(Maybe Text)
    } deriving (Eq, Show)

data RegistrationOptions = RegistrationOptions
    { registrationPreRegistered :: !(Maybe PreRegisteredClient)
    , registrationClientIdMetadataUrl :: !(Maybe Text)
    , registrationStored :: !(Maybe StoredClient)
    , registrationRedirectUri :: !Text
    } deriving (Eq, Show)

data RegistrationPlan
    = UsePreRegisteredClient !PreRegisteredClient
    | UseClientIdMetadataDocument !Text
    -- ^ The metadata document URL is the @client_id@.
    | ReuseDynamicRegistration !Text
    -- ^ Reuse a @client_id@ obtained earlier from the same issuer.
    | UseDynamicRegistration !Text
    -- ^ Register at the given registration endpoint.
    deriving (Eq, Show)

-- | Choose how to obtain a @client_id@, in the priority order the MCP
-- specification defines: pre-registered credentials, Client ID Metadata
-- Documents, then Dynamic Client Registration. Stored dynamic registrations
-- are reused only when they were issued by the same issuer for the same
-- redirect URI.
selectClientRegistration :: RegistrationOptions -> AuthorizationServerMetadata -> Either Text RegistrationPlan
selectClientRegistration options metadata
    | Just pre <- options.registrationPreRegistered
    , not (Text.null (Text.strip pre.preRegisteredClientId)) =
        Right (UsePreRegisteredClient pre)
    | Just url <- options.registrationClientIdMetadataUrl
    , Left err <- validateClientIdMetadataUrl url =
        Left err
    | Just url <- options.registrationClientIdMetadataUrl
    , metadata.clientIdMetadataDocumentSupported =
        Right (UseClientIdMetadataDocument url)
    | Just stored <- options.registrationStored
    , stored.storedSource == ClientIdDynamicRegistration
    , Just stored.storedIssuer == metadata.issuer
    , stored.storedRedirectUri == Just options.registrationRedirectUri =
        Right (ReuseDynamicRegistration stored.storedClientId)
    | Just endpoint <- metadata.registrationEndpoint =
        Right (UseDynamicRegistration endpoint)
    | otherwise = Left $
        "The authorization server offers neither Client ID Metadata Documents"
            <> (if isJust options.registrationClientIdMetadataUrl then "" else " (no oauth.clientIdMetadataUrl is configured)")
            <> " nor Dynamic Client Registration. Configure oauth.clientId (and oauth.clientSecret if the"
            <> " authorization server issued one) or oauth.clientIdMetadataUrl for this server in"
            <> " ~/.haskell-agent/config.json."

-- | A Client ID Metadata Document URL must use https and carry a path.
validateClientIdMetadataUrl :: Text -> Either Text ()
validateClientIdMetadataUrl url = case parseUrlParts url of
    Just parts | parts.urlScheme == "https", not (Text.null parts.urlPath) -> Right ()
    _ -> Left ("oauth.clientIdMetadataUrl must be an https URL with a path component: " <> url)

data ClientRegistrationRequest = ClientRegistrationRequest
    { registrationClientName :: !Text
    , registrationRedirectUris :: ![Text]
    , registrationScopes :: ![Text]
    } deriving (Eq, Show)

-- | RFC 7591 registration request body for a native public client.
clientRegistrationPayload :: ClientRegistrationRequest -> Aeson.Value
clientRegistrationPayload request = Aeson.object $
    [ "application_type" Aeson..= ("native" :: Text)
    , "client_name" Aeson..= request.registrationClientName
    , "redirect_uris" Aeson..= request.registrationRedirectUris
    , "grant_types" Aeson..= ["authorization_code", "refresh_token" :: Text]
    , "response_types" Aeson..= ["code" :: Text]
    , "token_endpoint_auth_method" Aeson..= ("none" :: Text)
    ] <> ["scope" Aeson..= Text.unwords request.registrationScopes | not (null request.registrationScopes)]

registerClientWith :: Manager -> Text -> ClientRegistrationRequest -> IO (Either Text ClientRegistration)
registerClientWith manager registrationUrl registration = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack registrationUrl)
        let request' = request
                { HC.method = "POST"
                , HC.requestBody = RequestBodyLBS (Aeson.encode (clientRegistrationPayload registration))
                , HC.requestHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]
                }
        httpLbs request' manager
    case result of
        Left exception -> pure (Left ("OAuth client registration failed: " <> Text.pack (show exception)))
        Right response
            | not (isSuccess response) -> pure (Left
                ("OAuth client registration failed with HTTP "
                    <> Text.pack (show (statusCode (responseStatus response)))
                    <> ": " <> responseBodyText response))
            | otherwise -> decodeBody response

-- | Compatibility wrapper using the default client name.
registerClient :: Manager -> Text -> [Text] -> [Text] -> IO (Either Text ClientRegistration)
registerClient manager registrationUrl redirectUris scopes =
    registerClientWith manager registrationUrl ClientRegistrationRequest
        { registrationClientName = "Haskell Agent"
        , registrationRedirectUris = redirectUris
        , registrationScopes = scopes
        }

-- ---------------------------------------------------------------------------
-- Scopes

data ScopeSources = ScopeSources
    { scopeChallenge :: ![Text]
    -- ^ @scope@ from the @WWW-Authenticate@ challenge.
    , scopeResourceMetadata :: ![Text]
    -- ^ @scopes_supported@ from the protected resource metadata.
    , scopeConfigured :: ![Text]
    -- ^ @oauth.scopes@ from the harness configuration.
    } deriving (Eq, Show)

-- | Initial scope selection: the challenge is authoritative, then the
-- protected resource metadata, then the configured scopes; otherwise none.
selectScopes :: ScopeSources -> [Text]
selectScopes sources = case filter (not . null) [sources.scopeChallenge, sources.scopeResourceMetadata, sources.scopeConfigured] of
    chosen : _ -> unionScopes chosen []
    [] -> []

-- | Order-preserving union without duplicates or blank entries.
unionScopes :: [Text] -> [Text] -> [Text]
unionScopes left right = foldl' add [] (left <> right)
  where
    add acc scope
        | Text.null scope || scope `elem` acc = acc
        | otherwise = acc <> [scope]

offlineAccessScope :: Text
offlineAccessScope = "offline_access"

data ScopePlan = ScopePlan
    { scopeSources :: !ScopeSources
    , scopePreviouslyGranted :: ![Text]
    -- ^ Scopes recorded with an earlier token for the same issuer.
    , scopeAdditional :: ![Text]
    -- ^ Extra scopes requested for step-up authorization.
    , scopeAuthorizationServerSupported :: ![Text]
    -- ^ @scopes_supported@ from the authorization server metadata.
    } deriving (Eq, Show)

-- | Scopes to request: the selected set unioned with previously granted and
-- additional scopes. @offline_access@ is included only when the
-- authorization server advertises it.
planScopes :: ScopePlan -> [Text]
planScopes plan =
    let base = filter (/= offlineAccessScope) $
            unionScopes (selectScopes plan.scopeSources) (unionScopes plan.scopePreviouslyGranted plan.scopeAdditional)
        offline = [offlineAccessScope | offlineAccessScope `elem` plan.scopeAuthorizationServerSupported]
    in base <> offline

-- ---------------------------------------------------------------------------
-- Authorization response validation (RFC 9207)

-- | Apply the RFC 9207 section 2.4 table: when the server advertises
-- @authorization_response_iss_parameter_supported@ the @iss@ parameter is
-- mandatory; whenever it is present it must equal the recorded issuer by
-- simple string comparison.
validateAuthorizationResponseIssuer :: Bool -> Text -> Maybe Text -> Either Text ()
validateAuthorizationResponseIssuer advertised recorded received = case received of
    Just actual
        | actual == recorded -> Right ()
        | otherwise -> Left
            ("Authorization response iss " <> actual
                <> " does not match the recorded issuer " <> recorded <> "; discarding the response")
    Nothing
        | advertised -> Left
            ("Authorization server " <> recorded
                <> " advertises the iss response parameter but the authorization response omitted it; discarding the response")
        | otherwise -> Right ()

-- ---------------------------------------------------------------------------
-- Token endpoint

data TokenExchange = TokenExchange
    { exchangeEndpoint :: !Text
    , exchangeClientId :: !Text
    , exchangeClientSecret :: !(Maybe Text)
    , exchangeCode :: !Text
    , exchangeRedirectUri :: !Text
    , exchangeCodeVerifier :: !Text
    , exchangeResource :: !(Maybe Text)
    }

exchangeAuthorizationCodeWith :: Manager -> TokenExchange -> IO OAuthTokenResponse
exchangeAuthorizationCodeWith manager exchange =
    tokenRequest manager "OAuth authorization-code exchange failed" exchange.exchangeEndpoint $
        [ ("grant_type", "authorization_code")
        , ("code", Encoding.encodeUtf8 exchange.exchangeCode)
        , ("redirect_uri", Encoding.encodeUtf8 exchange.exchangeRedirectUri)
        , ("client_id", Encoding.encodeUtf8 exchange.exchangeClientId)
        , ("code_verifier", Encoding.encodeUtf8 exchange.exchangeCodeVerifier)
        ]
        <> optionalParam "client_secret" exchange.exchangeClientSecret
        <> optionalParam "resource" exchange.exchangeResource

-- | Compatibility wrapper for public clients.
exchangeAuthorizationCode :: Manager -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> IO OAuthTokenResponse
exchangeAuthorizationCode manager endpoint clientId code redirectUri verifier resource =
    exchangeAuthorizationCodeWith manager TokenExchange
        { exchangeEndpoint = endpoint
        , exchangeClientId = clientId
        , exchangeClientSecret = Nothing
        , exchangeCode = code
        , exchangeRedirectUri = redirectUri
        , exchangeCodeVerifier = verifier
        , exchangeResource = resource
        }

data RefreshRequest = RefreshRequest
    { refreshEndpoint :: !Text
    , refreshClientId :: !Text
    , refreshClientSecret :: !(Maybe Text)
    , refreshRefreshToken :: !Text
    , refreshResource :: !(Maybe Text)
    }

refreshAccessTokenWith :: Manager -> RefreshRequest -> IO OAuthTokenResponse
refreshAccessTokenWith manager refresh =
    tokenRequest manager "OAuth token refresh failed" refresh.refreshEndpoint $
        [ ("grant_type", "refresh_token")
        , ("refresh_token", Encoding.encodeUtf8 refresh.refreshRefreshToken)
        , ("client_id", Encoding.encodeUtf8 refresh.refreshClientId)
        ]
        <> optionalParam "client_secret" refresh.refreshClientSecret
        <> optionalParam "resource" refresh.refreshResource

-- | Compatibility wrapper: refresh for a public client without a resource
-- indicator.
refreshAccessToken :: Manager -> Text -> Text -> Text -> IO OAuthTokenResponse
refreshAccessToken manager endpoint clientId oldRefresh =
    refreshAccessTokenWith manager RefreshRequest
        { refreshEndpoint = endpoint
        , refreshClientId = clientId
        , refreshClientSecret = Nothing
        , refreshRefreshToken = oldRefresh
        , refreshResource = Nothing
        }

optionalParam :: BS.ByteString -> Maybe Text -> [(BS.ByteString, BS.ByteString)]
optionalParam name = maybe [] (\value -> [(name, Encoding.encodeUtf8 value)])

tokenRequest :: Manager -> Text -> Text -> [(BS.ByteString, BS.ByteString)] -> IO OAuthTokenResponse
tokenRequest manager failure endpoint parameters = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack endpoint)
        let request' = urlEncodedBody parameters request { HC.method = "POST" }
        httpLbs request' { HC.requestHeaders = ("Accept", "application/json") : HC.requestHeaders request' } manager
    case result of
        Left e -> pure (OAuthTokenFailure (failure <> ": " <> Text.pack (show e)))
        Right response
            | not (isSuccess response) -> pure $ OAuthTokenFailure
                (failure <> " with HTTP " <> Text.pack (show (statusCode (responseStatus response)))
                    <> ": " <> responseBodyText response)
            | otherwise -> pure $ either (OAuthTokenFailure . Text.pack) OAuthTokenSuccess
                (Aeson.eitherDecode (responseBody response))

-- ---------------------------------------------------------------------------
-- HTTP helpers

tryCandidates :: Text -> (Text -> IO (Either Text a)) -> [Text] -> IO (Either Text a)
tryCandidates failure fetch = go []
  where
    go errors [] = pure (Left (failure <> ": " <> Text.intercalate "; " (reverse errors)))
    go errors (url : rest) = fetch url >>= \case
        Right value -> pure (Right value)
        Left err -> go ((url <> ": " <> err) : errors) rest

getJson :: Aeson.FromJSON value => Manager -> Text -> IO (Either Text value)
getJson manager url = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack url)
        httpLbs request { HC.requestHeaders = [("Accept", "application/json")] } manager
    case result of
        Left exception -> pure (Left (Text.pack (show exception)))
        Right response
            | not (isSuccess response) -> pure (Left ("HTTP " <> Text.pack (show (statusCode (responseStatus response)))))
            | otherwise -> decodeBody response

decodeBody :: Aeson.FromJSON value => HC.Response LBS.ByteString -> IO (Either Text value)
decodeBody response = pure $ either (Left . Text.pack) Right (Aeson.eitherDecode (responseBody response))

isSuccess :: HC.Response body -> Bool
isSuccess response =
    let code = statusCode (responseStatus response)
    in code >= 200 && code < 300

-- | Error bodies from OAuth endpoints describe the failure and carry no
-- credentials; keep them short for diagnostics.
responseBodyText :: HC.Response LBS.ByteString -> Text
responseBodyText response =
    let body = Text.strip (decodeLenient (LBS.toStrict (LBS.take 2048 (responseBody response))))
    in if Text.null body then "(empty response body)" else body

-- | UTF-8 success page served by the interactive OAuth callback listener.
oauthCallbackSuccessPage :: LBS.ByteString
oauthCallbackSuccessPage = "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width\"><title>Connected</title><style>body{margin:0;min-height:100vh;display:grid;place-items:center;background:#0b1020;color:#e8ecf5;font:16px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif}.card{max-width:430px;margin:24px;padding:36px;border:1px solid #28324a;border-radius:20px;background:#131a2d;box-shadow:0 24px 70px #0008;text-align:center}.check{display:grid;place-items:center;width:56px;height:56px;margin:0 auto 20px;border-radius:50%;background:#173d31;color:#6ee7b7;font-size:30px}h1{margin:0 0 10px;font-size:24px}p{margin:0;color:#aab4ca;line-height:1.55}</style></head><body><main class=\"card\"><div class=\"check\">&#10003;</div><h1>MCP connected</h1><p>Authorization completed successfully. You can close this tab and return to Haskell Agent.</p></main></body></html>"
