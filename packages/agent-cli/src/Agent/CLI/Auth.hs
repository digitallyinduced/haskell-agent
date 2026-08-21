-- | Load ChatGPT, Grok, OpenRouter, or broker credentials for the CLI process.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , GrokAuthState(..)
    , grokCredentialFromAuthJson
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , loadAuth
    , managedGrokTokenProvider
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , probeLoadedAuth
    , reloadableFileCredentialProvider
    , staticCredentialProvider
    , xaiOAuthClientId
    ) where

import Agent.Broker (BrokerOptions(..), newBrokerTokenProviderFor)
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , upsertManagedCredentialAfterRefresh
    , withCredentialRefreshFileLock
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider(..)
    , getNextToken
    , seedTokenProvider
    )
import Agent.OpenRouter.Credential (credentialFromApiKey)
import qualified Agent.XAI.Auth as XAIAuth
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath ((</>))
import qualified System.OsPath as OsPath
import qualified System.Process.Environment.OsString as Environment

openAIOAuthClientId :: Maybe Text -> Text
openAIOAuthClientId =
    fromMaybe "app_EMoamEEZ73f0CkXaXp7hrann"

xaiOAuthClientId :: Maybe Text -> Text
xaiOAuthClientId =
    fromMaybe "b1a00492-073a-47ea-816f-4c329264a828"

data LoadedAuth = LoadedAuth
    { loadedProvider :: !Provider
    , loadedTokenProvider :: !TokenProvider
    -- | Live OpenAI OAuth pool, when authentication uses one.
    , loadedOpenAiPool :: !(Maybe OpenAI.Pool)
    }

data GrokAuthState = GrokAuthState
    { grokAccessToken :: !Text, grokRefreshToken :: !(Maybe Text)
    , grokIdToken :: !(Maybe Text), grokExpiresAt :: !(Maybe UTCTime)
    }
    deriving (Eq)

instance Show GrokAuthState where
    show state =
        "GrokAuthState { grokAccessToken = <redacted>, grokRefreshToken = "
            <> maybe "Nothing" (const "Just <redacted>") state.grokRefreshToken
            <> ", grokIdToken = "
            <> maybe "Nothing" (const "Just <redacted>") state.grokIdToken
            <> ", grokExpiresAt = " <> show state.grokExpiresAt <> " }"

loadAuth :: Maybe Provider -> IO (Either Text LoadedAuth)
loadAuth requested = runExceptT do
    brokerUrl <- lift (lookupNonEmpty "AGENT_BROKER_URL")
    case brokerUrl of
        Just url -> loadBroker url requested
        Nothing -> do
            provider <- detectProvider requested
            case provider of
                XAIProvider -> loadXai
                OpenAIProvider -> loadOpenAi
                OpenRouterProvider -> loadOpenRouter

-- | Ask the token source whether it has a usable credential now without
-- making a model request, preserving a successful checkout for later use.
probeLoadedAuth :: LoadedAuth -> IO (Either ApiError LoadedAuth)
probeLoadedAuth loaded = do
    result <- getNextToken loaded.loadedTokenProvider Nothing
    case result of
        Left err -> pure (Left err)
        Right credential
            | credential.provider /= loaded.loadedProvider ->
                pure $ Left $ ProviderError AuthenticationError
                    "credential provider does not match loaded auth"
                    Nothing
            | otherwise -> do
                tokenProvider <-
                    seedTokenProvider loaded.loadedTokenProvider credential
                pure $ Right loaded { loadedTokenProvider = tokenProvider }

detectProvider :: Maybe Provider -> ExceptT Text IO Provider
detectProvider (Just provider) = pure provider
detectProvider Nothing = do
    grok <- lift hasGrokAuth
    openai <- lift hasOpenAiAuth
    openrouter <- lift hasOpenRouterAuth
    if grok
        then pure XAIProvider
        else if openai
            then pure OpenAIProvider
            else if openrouter
                then pure OpenRouterProvider
                else throwE noAuthHint

loadBroker :: Text -> Maybe Provider -> ExceptT Text IO LoadedAuth
loadBroker url requested = do
    serviceToken <- lift (lookupNonEmpty "AGENT_BROKER_TOKEN")
        >>= maybe
            (throwE "AGENT_BROKER_URL is set; also set AGENT_BROKER_TOKEN")
            pure
    let supportedProviders = case requested of
            Just selected -> [selected]
            Nothing -> [OpenAIProvider, XAIProvider, OpenRouterProvider]
    provider <- lift $ newBrokerTokenProviderFor
        BrokerOptions
            { baseUrl = Text.unpack url
            , serviceToken
            }
        supportedProviders
    credential <- withExceptT
        (\err -> "broker: " <> Text.pack (show err))
        (ExceptT (getNextToken provider Nothing))
    let actual = credential.provider
    when (maybe False (/= actual) requested) $
        throwE $
            "broker returned " <> Text.pack (show actual)
                <> " but --provider asked for a different vendor"
    seeded <- lift (seedTokenProvider provider credential)
    pure LoadedAuth
        { loadedProvider = actual
        , loadedTokenProvider = seeded
        , loadedOpenAiPool = Nothing
        }

loadXai :: ExceptT Text IO LoadedAuth
loadXai = do
    managed <- lift (loadManagedCredential XAIProvider)
    case managed of
        Just (metadata, secret)
            | metadata.managedAuthKind == ManagedGrokAuthJson -> do
                now <- lift getCurrentTime
                state <- maybe
                    (throwE "managed Grok OAuth credential contains invalid auth JSON")
                    pure
                    (grokAuthStateFromJson now secret.secretPayload)
                clientId <-
                    lift $
                        xaiOAuthClientId
                            <$> lookupNonEmpty "XAI_OAUTH_CLIENT_ID"
                provider <- lift $ managedGrokTokenProvider
                    metadata
                    secret
                    state
                    (XAIAuth.refreshAccessToken
                        (XAIAuth.defaultOAuthOptions clientId))
                pure LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider = provider
                    , loadedOpenAiPool = Nothing
                    }
        Just (metadata, secret) ->
            pure LoadedAuth
                { loadedProvider = XAIProvider
                , loadedTokenProvider =
                    staticCredentialProvider Credential
                        { accessToken = secret.secretPayload
                        , accountId = metadata.managedAccountId
                        , leaseId = Nothing
                        , provider = XAIProvider
                        }
                , loadedOpenAiPool = Nothing
                }
        Nothing -> do
            credential <- lift loadExternalGrokCredential
            case credential of
                Nothing -> throwE noAuthHint
                Just loaded -> do
                    provider <- lift $ reloadableFileCredentialProvider
                        XAIProvider loaded loadExternalGrokCredential
                    pure LoadedAuth
                        { loadedProvider = XAIProvider
                        , loadedTokenProvider = provider
                        , loadedOpenAiPool = Nothing
                        }

loadOpenRouterKey :: IO (Maybe Text)
loadOpenRouterKey = do
    managed <- loadManagedCredential OpenRouterProvider
    case managed of
        Just (_, secret) -> pure (Just secret.secretPayload)
        Nothing -> lookupNonEmpty "OPENROUTER_API_KEY"

loadOpenRouter :: ExceptT Text IO LoadedAuth
loadOpenRouter = do
    key <- lift loadOpenRouterKey
    case key of
        Nothing -> throwE noAuthHint
        Just apiKey -> do
            let initial = credentialFromApiKey apiKey
            provider <- lift $ reloadableFileCredentialProvider OpenRouterProvider initial
                (fmap (fmap credentialFromApiKey) loadOpenRouterKey)
            pure LoadedAuth
                { loadedProvider = OpenRouterProvider
                , loadedTokenProvider = provider
                , loadedOpenAiPool = Nothing
                }

loadOpenAi :: ExceptT Text IO LoadedAuth
loadOpenAi = do
    managed <- lift (loadManagedCredential OpenAIProvider)
    fromEnvToken <- lift (lookupNonEmpty "CODEX_ACCESS_TOKEN")
    fromEnvJson <- lift (lookupNonEmpty "CODEX_AUTH_JSON")
    home <- lift getHomeDirectory
    let filePath =
            home </> fromFilePath ".codex" </> fromFilePath "auth.json"
    fileExists <- lift (doesFileExist filePath)
    fileBytes <- if fileExists
        then lift (Just <$> retryOnFileBusy (LBS.readFile (toFilePath filePath)))
        else pure Nothing
    now <- lift getCurrentTime
    case managed of
        Just (metadata, secret) ->
            loadManagedOpenAI now metadata secret
        Nothing ->
            case fromEnvToken of
                Just token -> do
                    accountId <- lift (openaiAccountIdForToken token)
                    pure LoadedAuth
                        { loadedProvider = OpenAIProvider
                        , loadedTokenProvider = staticCredentialProvider Credential
                            { accessToken = token
                            , accountId
                            , leaseId = Nothing
                            , provider = OpenAIProvider
                            }
                        , loadedOpenAiPool = Nothing
                        }
                Nothing -> case envOrFileState now fromEnvJson fileBytes of
                    Nothing -> throwE noAuthHint
                    Just state ->
                        openAiPoolAuth fileExists filePath state

loadManagedOpenAI
    :: UTCTime
    -> ManagedCredential
    -> ManagedSecret
    -> ExceptT Text IO LoadedAuth
loadManagedOpenAI now metadata secret =
    case metadata.managedAuthKind of
        ManagedOpenAIAuthJson ->
            case openaiAuthStateFromJson now
                (LBS.fromStrict (TextEncoding.encodeUtf8 secret.secretPayload)) of
                Nothing ->
                    throwE "managed OpenAI OAuth credential contains invalid auth JSON"
                Just state -> do
                    clientId <-
                        lift $
                            openAIOAuthClientId
                                <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
                    refreshLock <- lift (newMVar ())
                    let refresh stale =
                            withMVar refreshLock \_ ->
                                withCredentialRefreshFileLock
                                    (refreshManagedOpenAI
                                        clientId metadata stale)
                    pool <- lift (OpenAI.newPool [state] refresh)
                    tokenProvider <- lift (OpenAICredential.poolTokenProvider pool)
                    pure LoadedAuth
                        { loadedProvider = OpenAIProvider
                        , loadedTokenProvider = tokenProvider
                        , loadedOpenAiPool = Just pool
                        }
        _ ->
            pure LoadedAuth
                { loadedProvider = OpenAIProvider
                , loadedTokenProvider =
                    staticCredentialProvider Credential
                        { accessToken = secret.secretPayload
                        , accountId = metadata.managedAccountId
                        , leaseId = Nothing
                        , provider = OpenAIProvider
                        }
                , loadedOpenAiPool = Nothing
                }

refreshManagedOpenAI
    :: Text
    -> ManagedCredential
    -> OpenAI.AuthState
    -> IO (Either ApiError OpenAI.AuthState)
refreshManagedOpenAI clientId metadata stale =
    loadManagedCredentialById metadata.managedId >>= \case
        Left err -> pure (Left (ConnectionError err))
        Right (latestMetadata, latestSecret) -> do
            now <- getCurrentTime
            case openaiAuthStateFromJson now
                (LBS.fromStrict
                    (TextEncoding.encodeUtf8 latestSecret.secretPayload)) of
                Nothing ->
                    pure $ Left $ ProviderError AuthenticationError
                        "managed OpenAI OAuth credential became invalid during refresh"
                        Nothing
                Just current
                    | oauthStateChanged stale current ->
                        pure (Right current)
                    | otherwise ->
                        OpenAI.refreshAccessTokenHTTP clientId current >>= \case
                            Left err -> pure (Left err)
                            Right newState -> do
                                stamped <-
                                    authStateToJson newState
                                        <$> getCurrentTime
                                let payload =
                                        TextEncoding.decodeUtf8
                                            (LBS.toStrict
                                                (Aeson.encode stamped))
                                upsertManagedCredentialAfterRefresh
                                    latestMetadata
                                    latestSecret { secretPayload = payload }
                                    >>= \case
                                        Left err ->
                                            pure
                                                (Left
                                                    (ConnectionError err))
                                        Right () -> pure (Right newState)

oauthStateChanged :: OpenAI.AuthState -> OpenAI.AuthState -> Bool
oauthStateChanged stale current =
    stale.accessToken /= current.accessToken
        || stale.refreshToken /= current.refreshToken

openAiPoolAuth
    :: Bool
    -> OsPath
    -> OpenAI.AuthState
    -> ExceptT Text IO LoadedAuth
openAiPoolAuth persistRefresh filePath state = do
    clientId <-
        lift $
            openAIOAuthClientId <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
    let refresh stale =
            OpenAI.refreshAccessTokenHTTP clientId stale >>= \case
                Left err -> pure (Left err)
                Right newState
                    | persistRefresh -> do
                        stamped <- authStateToJson newState <$> getCurrentTime
                        OpenAILogin.writeAuthFile filePath stamped
                        pure (Right newState)
                    | otherwise -> pure (Right newState)
    pool <- lift (OpenAI.newPool [state] refresh)
    tokenProvider <- lift (OpenAICredential.poolTokenProvider pool)
    pure LoadedAuth
        { loadedProvider = OpenAIProvider
        , loadedTokenProvider = tokenProvider
        , loadedOpenAiPool = Just pool
        }

loadManagedCredential
    :: Provider
    -> IO (Maybe (ManagedCredential, ManagedSecret))
loadManagedCredential provider =
    loadManagedCredentials >>= \case
        Left _ -> pure Nothing
        Right credentials ->
            pure $ listToMaybe
                [ (metadata, secret)
                | (metadata, secret) <- credentials
                , metadata.managedEnabled
                , metadata.managedProvider == provider
                ]

loadManagedCredentialById
    :: Text
    -> IO (Either Text (ManagedCredential, ManagedSecret))
loadManagedCredentialById credentialId =
    loadManagedCredentials >>= \case
        Left err -> pure (Left err)
        Right credentials ->
            pure $ maybe
                (Left
                    ("managed credential disappeared during refresh: "
                        <> credentialId))
                Right
                (listToMaybe
                    (filter
                        ((== credentialId) . (.managedId) . fst)
                        credentials))

envOrFileState
    :: UTCTime
    -> Maybe Text
    -> Maybe LBS.ByteString
    -> Maybe OpenAI.AuthState
envOrFileState now fromEnvJson fileBytes =
    (fromEnvJson >>= openaiAuthStateFromJson now . LBS.fromStrict . TextEncoding.encodeUtf8)
        <|> (fileBytes >>= openaiAuthStateFromJson now)

openaiAuthStateFromJson :: UTCTime -> LBS.ByteString -> Maybe OpenAI.AuthState
openaiAuthStateFromJson now bytes = do
    value <- Aeson.decode bytes
    tokens <- tokensObject value
    accessToken <- textField "access_token" tokens
    refreshToken <- textField "refresh_token" tokens <|> Just ""
    accountId <- textField "account_id" tokens
    let idToken = textField "id_token" tokens
    Just OpenAI.AuthState
        { accessToken
        , refreshToken
        , accountId
        , idToken
        , lastRefresh = now
        }

tokensObject :: Aeson.Value -> Maybe Aeson.Object
tokensObject value = case value of
    Aeson.Object object ->
        case KeyMap.lookup "tokens" object of
            Just (Aeson.Object tokens) -> Just tokens
            _ | isJust (textField "access_token" object) -> Just object
            _ -> Nothing
    Aeson.Array values ->
        case [item | item <- foldr (:) [] values] of
            (first : _) -> tokensObject first
            [] -> Nothing
    _ -> Nothing

authStateToJson :: OpenAI.AuthState -> UTCTime -> Aeson.Value
authStateToJson state now = Aeson.object
    [ "auth_mode" .= ("chatgpt" :: Text)
    , "last_refresh" .= now
    , "tokens" .= Aeson.object
        [ "access_token" .= state.accessToken
        , "refresh_token" .= state.refreshToken
        , "account_id" .= state.accountId
        , "id_token" .= state.idToken
        ]
    ]

loadExternalGrokCredential :: IO (Maybe Credential)
loadExternalGrokCredential = do
    fromJson <- lookupNonEmpty "GROK_AUTH_JSON"
    fromToken <- lookupNonEmpty "GROK_ACCESS_TOKEN"
    home <- getHomeDirectory
    let filePath =
            home </> fromFilePath ".grok" </> fromFilePath "auth.json"
    fileExists <- doesFileExist filePath
    fileJson <- if fileExists
        then Just . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> retryOnFileBusy (LBS.readFile (toFilePath filePath))
        else pure Nothing
    let token =
            (fromJson >>= grokCredentialFromAuthJson)
                <|> fromToken
                <|> (fileJson >>= grokCredentialFromAuthJson)
    pure (fmap grokCredential token)

grokCredentialFromAuthJson :: Text -> Maybe Text
grokCredentialFromAuthJson raw =
    (.grokAccessToken) <$> grokAuthStateFromJson epoch raw
  where
    epoch = posixSecondsToUTCTime 0

grokAuthStateFromJson :: UTCTime -> Text -> Maybe GrokAuthState
grokAuthStateFromJson now raw = do
    value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    object <- authObject value
    grokAccessToken <-
        textField "key" object <|> textField "access_token" object
    let grokRefreshToken = textField "refresh_token" object
        grokIdToken = textField "id_token" object
        grokExpiresAt =
            utcTimeField "expires_at" object
                <|> ((`addUTCTime` now) . fromIntegral
                    <$> intField "expires_in" object)
                <|> OpenAI.parseJwtExp grokAccessToken
    pure GrokAuthState{..}

grokAuthStateToJson :: GrokAuthState -> Aeson.Value
grokAuthStateToJson state = Aeson.object
    [ "access_token" .= state.grokAccessToken
    , "refresh_token" .= state.grokRefreshToken
    , "id_token" .= state.grokIdToken
    , "expires_at" .= state.grokExpiresAt
    ]

authObject :: Aeson.Value -> Maybe Aeson.Object
authObject = \case
    Aeson.Object object
        | hasAccessToken object -> Just object
        | otherwise ->
            listToMaybe
                [ nestedObject
                | Aeson.Object nestedObject <- KeyMap.elems object
                , hasAccessToken nestedObject
                ]
    _ -> Nothing
  where
    hasAccessToken object =
        isJust (textField "key" object <|> textField "access_token" object)

utcTimeField :: Text -> Aeson.Object -> Maybe UTCTime
utcTimeField name object =
    KeyMap.lookup (Key.fromText name) object >>= \value ->
        case Aeson.fromJSON value of
            Aeson.Success time -> Just time
            Aeson.Error _ -> case value of
                Aeson.Number seconds ->
                    Just
                        (posixSecondsToUTCTime (realToFrac seconds))
                _ -> Nothing

intField :: Text -> Aeson.Object -> Maybe Int
intField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Number value) -> Just (floor value)
    _ -> Nothing

grokNeedsRefresh :: UTCTime -> GrokAuthState -> Bool
grokNeedsRefresh now state =
    maybe False (<= addUTCTime 600 now) state.grokExpiresAt

managedGrokTokenProvider
    :: ManagedCredential
    -> ManagedSecret
    -> GrokAuthState
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> IO TokenProvider
managedGrokTokenProvider metadata secret initial refresh = do
    stateRef <- newIORef initial
    refreshLock <- newMVar ()
    pure $ TokenProvider \failed ->
        withMVar refreshLock \_ ->
            managedGrokCredential metadata secret stateRef refresh failed

managedGrokCredential
    :: ManagedCredential
    -> ManagedSecret
    -> IORef GrokAuthState
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> Maybe FailedCredential
    -> IO (Either ApiError Credential)
managedGrokCredential metadata secret stateRef refresh failed = do
    current <- readIORef stateRef
    case failed of
        Just FailedCredential
            { failure = AccountRateLimited { retryAfterSeconds }
            } -> do
                now <- getCurrentTime
                let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
                pure $ Left $ CredentialsExhausted
                    (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential
            { credential = rejected
            , failure = AccountAuthenticationRejected
            }
            | rejected.accessToken /= current.grokAccessToken ->
                pure (Right (grokCredentialFromState metadata current))
            | otherwise ->
                refreshManagedGrok metadata secret stateRef refresh current
        Nothing -> do
            now <- getCurrentTime
            if grokNeedsRefresh now current
                then refreshManagedGrok metadata secret stateRef refresh current
                else pure (Right (grokCredentialFromState metadata current))

refreshManagedGrok
    :: ManagedCredential
    -> ManagedSecret
    -> IORef GrokAuthState
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> GrokAuthState
    -> IO (Either ApiError Credential)
refreshManagedGrok metadata _secret stateRef refresh state =
    withCredentialRefreshFileLock $
        loadManagedCredentialById metadata.managedId >>= \case
            Left err -> pure (Left (ConnectionError err))
            Right (latestMetadata, latestSecret) -> do
                now <- getCurrentTime
                case grokAuthStateFromJson now latestSecret.secretPayload of
                    Nothing ->
                        pure $ Left $ ProviderError AuthenticationError
                            "managed Grok OAuth credential became invalid during refresh"
                            Nothing
                    Just current
                        | grokStateChanged state current -> do
                            writeIORef stateRef current
                            pure
                                (Right
                                    (grokCredentialFromState
                                        latestMetadata current))
                        | otherwise ->
                            refreshCurrentGrok
                                latestMetadata latestSecret
                                stateRef refresh current

grokStateChanged :: GrokAuthState -> GrokAuthState -> Bool
grokStateChanged stale current =
    stale.grokAccessToken /= current.grokAccessToken
        || stale.grokRefreshToken /= current.grokRefreshToken

refreshCurrentGrok
    :: ManagedCredential
    -> ManagedSecret
    -> IORef GrokAuthState
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> GrokAuthState
    -> IO (Either ApiError Credential)
refreshCurrentGrok metadata secret stateRef refresh state =
    case state.grokRefreshToken of
        Nothing ->
            pure $ Left $ ProviderError AuthenticationError
                "managed Grok OAuth credential has no refresh token; reconnect the account"
                Nothing
        Just refreshToken ->
            refresh refreshToken >>= \case
                Left err -> pure (Left err)
                Right tokens ->
                    persistRefreshedGrok
                        metadata secret stateRef state tokens

persistRefreshedGrok
    :: ManagedCredential
    -> ManagedSecret
    -> IORef GrokAuthState
    -> GrokAuthState
    -> XAIAuth.OAuthTokens
    -> IO (Either ApiError Credential)
persistRefreshedGrok metadata secret stateRef state tokens = do
    now <- getCurrentTime
    let newState = GrokAuthState
            { grokAccessToken = tokens.accessToken
            , grokRefreshToken =
                tokens.refreshToken <|> state.grokRefreshToken
            , grokIdToken = tokens.idToken <|> state.grokIdToken
            , grokExpiresAt =
                ((`addUTCTime` now) . fromIntegral
                    <$> tokens.expiresInSeconds)
                    <|> OpenAI.parseJwtExp tokens.accessToken
            }
        newAccountId =
            fromMaybe metadata.managedAccountId
                (XAIAuth.accountIdFromAccessToken tokens.accessToken)
        newMetadata = metadata { managedAccountId = newAccountId }
        newSecret = secret
            { secretPayload =
                TextEncoding.decodeUtf8
                    (LBS.toStrict
                        (Aeson.encode (grokAuthStateToJson newState)))
            }
    upsertManagedCredentialAfterRefresh newMetadata newSecret >>= \case
        Left err -> pure (Left (ConnectionError err))
        Right () -> do
            writeIORef stateRef newState
            pure (Right (grokCredentialFromState newMetadata newState))

grokCredentialFromState
    :: ManagedCredential
    -> GrokAuthState
    -> Credential
grokCredentialFromState metadata state = Credential
    { accessToken = state.grokAccessToken
    , accountId =
        fromMaybe metadata.managedAccountId
            (XAIAuth.accountIdFromAccessToken state.grokAccessToken)
    , leaseId = Nothing
    , provider = XAIProvider
    }

grokCredential :: Text -> Credential
grokCredential token = Credential
    { accessToken = token
    , accountId =
        fromMaybe "grok" (XAIAuth.accountIdFromAccessToken token)
    , leaseId = Nothing
    , provider = XAIProvider
    }

openaiAccountIdForToken :: Text -> IO Text
openaiAccountIdForToken token = do
    fromAccount <- lookupNonEmpty "CODEX_ACCOUNT_ID"
    fromIdToken <- lookupNonEmpty "CODEX_ID_TOKEN"
    pure $ fromMaybe "" $
        fromAccount
            <|> (fromIdToken >>= OpenAI.deriveAccountId)
            <|> OpenAI.deriveAccountId token

hasGrokAuth :: IO Bool
hasGrokAuth = do
    envJson <- lookupNonEmpty "GROK_AUTH_JSON"
    envToken <- lookupNonEmpty "GROK_ACCESS_TOKEN"
    home <- getHomeDirectory
    file <- doesFileExist
        (home </> fromFilePath ".grok" </> fromFilePath "auth.json")
    managed <- hasManagedProvider XAIProvider
    pure (isJust envJson || isJust envToken || file || managed)

hasOpenAiAuth :: IO Bool
hasOpenAiAuth = do
    envJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    envToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    home <- getHomeDirectory
    file <- doesFileExist
        (home </> fromFilePath ".codex" </> fromFilePath "auth.json")
    managed <- hasManagedProvider OpenAIProvider
    pure (isJust envJson || isJust envToken || file || managed)

hasOpenRouterAuth :: IO Bool
hasOpenRouterAuth = do
    environment <- isJust <$> lookupNonEmpty "OPENROUTER_API_KEY"
    managed <- hasManagedProvider OpenRouterProvider
    pure (environment || managed)

hasManagedProvider :: Provider -> IO Bool
hasManagedProvider provider =
    isJust <$> loadManagedCredential provider

-- | Cache one credential and only re-read disk/env after the provider rejects
-- it for authentication. Rate-limit failures stay exhausted rather than
-- spinning on the same key.
reloadableFileCredentialProvider
    :: Provider
    -> Credential
    -> IO (Maybe Credential)
    -> IO TokenProvider
reloadableFileCredentialProvider expectedProvider initial reload = do
    cache <- newIORef (Just initial)
    let loadFresh rejectedToken =
            reload >>= \case
                Nothing ->
                    pure $ Left $ ProviderError AuthenticationError
                        "no credentials found while reloading auth"
                        Nothing
                Just credential
                    | credential.provider /= expectedProvider ->
                        pure $ Left $ ProviderError AuthenticationError
                            ("reloaded auth resolved "
                                <> Text.pack (show credential.provider)
                                <> " but this session expects "
                                <> Text.pack (show expectedProvider))
                            Nothing
                    | rejectedToken == Just credential.accessToken ->
                        pure $ Left $ ProviderError AuthenticationError
                            "reloaded credential is unchanged; refresh ~/.grok/auth.json or OPENROUTER_API_KEY and retry"
                            Nothing
                    | otherwise -> do
                        writeIORef cache (Just credential)
                        pure (Right credential)
    pure $ TokenProvider \failed -> case failed of
        Just FailedCredential { failure = AccountRateLimited { retryAfterSeconds } } -> do
            now <- getCurrentTime
            let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
            pure $ Left $ CredentialsExhausted
                (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential
            { credential = rejected
            , failure = AccountAuthenticationRejected
            } -> do
            writeIORef cache Nothing
            loadFresh (Just rejected.accessToken)
        Nothing ->
            readIORef cache >>= \case
                Just credential -> pure (Right credential)
                Nothing -> loadFresh Nothing

staticCredentialProvider :: Credential -> TokenProvider
staticCredentialProvider credential = TokenProvider \failed ->
    case failed of
        Nothing -> pure (Right credential)
        Just FailedCredential
            { failure = AccountRateLimited { retryAfterSeconds }
            } -> do
                now <- getCurrentTime
                let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
                pure $ Left $ CredentialsExhausted
                    (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential
            { failure = AccountAuthenticationRejected
            } ->
                pure $ Left $ ProviderError AuthenticationError
                    "static credential was rejected"
                    Nothing

lookupNonEmpty :: String -> IO (Maybe Text)
lookupNonEmpty name = do
    value <- Environment.getEnv (OsPath.unsafeEncodeUtf name)
    pure $ case value of
        Just raw
            | Right text <- OsPath.decodeUtf raw
            , not (null text) ->
                Just (Text.pack text)
        _ -> Nothing

textField :: Text -> Aeson.Object -> Maybe Text
textField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) | not (Text.null value) -> Just value
    _ -> Nothing

noAuthHint :: Text
noAuthHint =
    "no credentials found. Set GROK_ACCESS_TOKEN, CODEX_ACCESS_TOKEN, \
    \or OPENROUTER_API_KEY, or place auth at ~/.grok/auth.json / ~/.codex/auth.json."
