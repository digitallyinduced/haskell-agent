module Agent.CLI.Auth.Grok
    ( ExternalGrokLoaded(..)
    , ExternalGrokSource(..)
    , externalGrokTokenProvider
    , grokNeedsRefresh
    , loadExternalGrokCredentials
    , managedGrokTokenProvider
    , refreshGrokLoginPayload
    ) where

import Agent.CLI.Auth.Types
    ( GrokAuthState(..)
    , externalAuthSelectionId
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokAuthStateToJsonWithKnownFields
    , grokOAuthOptionsFromAuthJson
    , xaiOAuthClientId
    )
import Agent.CLI.CredentialStore
    ( ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , upsertManagedCredentialAfterRefresh
    , withCredentialRefreshFileLock
    )
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.Error (ApiError(..))
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider
    ( AccountFailure(..)
    , BillingMode(SubscriptionBilled)
    , Credential(..)
    , FailedCredential(..)
    , Provider(XAIProvider)
    , TokenProvider
    , credentialsExhaustedForRateLimit
    , tokenProvider
    )
import qualified Agent.XAI.Auth as XAIAuth
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar, withMVar)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data ExternalGrokSource
    = GrokSourceEnvironment
    | GrokSourceFile !OsPath
    deriving (Eq, Show)

data ExternalGrokLoaded = ExternalGrokLoaded
    { grokSelectionId :: !Text
    , grokState :: !GrokAuthState
    , grokSource :: !ExternalGrokSource
    , grokRawJson :: !(Maybe Text)
    }
    deriving (Eq)

instance Show ExternalGrokLoaded where
    show loaded =
        "ExternalGrokLoaded { grokSelectionId = "
            <> show loaded.grokSelectionId
            <> ", grokState = "
            <> show loaded.grokState
            <> ", grokSource = "
            <> show loaded.grokSource
            <> ", grokRawJson = "
            <> maybe "Nothing" (const "Just <redacted>") loaded.grokRawJson
            <> " }"

loadExternalGrokCredentials :: IO [ExternalGrokLoaded]
loadExternalGrokCredentials = do
    fromJson <- lookupNonEmpty "GROK_AUTH_JSON"
    fromToken <- lookupNonEmpty "GROK_ACCESS_TOKEN"
    home <- getHomeDirectory
    now <- getCurrentTime
    let filePath =
            home </> unsafeEncodeUtf ".grok" </> unsafeEncodeUtf "auth.json"
    fileExists <- doesFileExist filePath
    fileJson <- if fileExists
        then Just . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> retryOnFileBusy (LBS.readFile (unsafeToFilePath filePath))
        else pure Nothing
    let environment = case fromJson >>= \raw ->
            (raw,) <$> grokAuthStateFromJson now raw of
            Just (raw, state) ->
                Just ExternalGrokLoaded
                    { grokSelectionId =
                        externalAuthSelectionId XAIProvider "environment"
                    , grokState = state
                    , grokSource = GrokSourceEnvironment
                    , grokRawJson = Just raw
                    }
            Nothing ->
                (\token -> ExternalGrokLoaded
                    { grokSelectionId =
                        externalAuthSelectionId XAIProvider "environment"
                    , grokState =
                        GrokAuthState token Nothing Nothing Nothing
                    , grokSource = GrokSourceEnvironment
                    , grokRawJson = Nothing
                    }) <$> fromToken
        file = do
            raw <- fileJson
            state <- grokAuthStateFromJson now raw
            Just ExternalGrokLoaded
                { grokSelectionId =
                    externalAuthSelectionId XAIProvider (toText filePath)
                , grokState = state
                , grokSource = GrokSourceFile filePath
                , grokRawJson = Just raw
                }
    pure $ catMaybes [environment, file]

grokNeedsRefresh :: UTCTime -> GrokAuthState -> Bool
grokNeedsRefresh now state =
    maybe False (<= addUTCTime 600 now) state.grokExpiresAt

externalGrokTokenProvider
    :: ExternalGrokLoaded
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> IO TokenProvider
externalGrokTokenProvider loaded refresh = do
    stateRef <- newIORef loaded.grokState
    payloadRef <- newIORef loaded.grokRawJson
    refreshLock <- newMVar ()
    pure $ tokenProvider SubscriptionBilled \failed ->
        withMVar refreshLock \_ ->
            withGrokSourceLock loaded.grokSource $
                externalGrokCredential
                    loaded.grokSource stateRef payloadRef refresh failed

-- | Refresh an expired Grok OAuth payload used by login/account selection.
-- File and managed sources persist rotated tokens; environment JSON stays
-- in-memory for this process.
refreshGrokLoginPayload
    :: Maybe Text
    -> Maybe OsPath
    -> Text
    -> IO (Either ApiError (GrokAuthState, Text))
refreshGrokLoginPayload managedId filePath payload = do
    clientId <- xaiOAuthClientId <$> lookupNonEmpty "XAI_OAUTH_CLIENT_ID"
    let refresh =
            XAIAuth.refreshAccessToken
                (grokOAuthOptionsFromAuthJson clientId payload)
        source = maybe GrokSourceEnvironment GrokSourceFile filePath
    withGrokSourceLock source do
        now <- getCurrentTime
        latestPayload <- case filePath of
            Just path -> readGrokAuthFile path
            Nothing -> pure (Just payload)
        case latestPayload >>= grokAuthStateFromJson now of
            Nothing ->
                pure $ Left $ CredentialError
                    "Grok OAuth credential became invalid during refresh"
            Just current
                | not (grokNeedsRefresh now current) ->
                    pure (Right (current, fromMaybe payload latestPayload))
                | otherwise ->
                    refreshGrokState refresh current >>= \case
                        Left err -> pure (Left err)
                        Right newState -> do
                            encoded <- persistGrokPayload
                                managedId filePath
                                (fromMaybe payload latestPayload)
                                newState
                            case encoded of
                                Left err -> pure (Left err)
                                Right newPayload ->
                                    pure (Right (newState, newPayload))

externalGrokCredential
    :: ExternalGrokSource
    -> IORef GrokAuthState
    -> IORef (Maybe Text)
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> Maybe FailedCredential
    -> IO (Either ApiError Credential)
externalGrokCredential source stateRef payloadRef refresh failed = do
    current <- reloadExternalGrok source stateRef payloadRef
    case failed of
        Just reported -> credentialsExhaustedForRateLimit reported >>= \case
            Just err -> pure (Left err)
            Nothing -> case reported of
                FailedCredential
                    { credential = rejected
                    , failure = AccountAuthenticationRejected
                    }
                    | rejected.accessToken /= current.grokAccessToken ->
                        pure (Right (grokCredential current.grokAccessToken))
                    | otherwise ->
                        refreshExternalGrok
                            source stateRef payloadRef refresh current
                _ -> pure $ Left $ CredentialError
                    "unsupported credential failure"
        Nothing -> do
            now <- getCurrentTime
            if grokNeedsRefresh now current
                then refreshExternalGrok
                    source stateRef payloadRef refresh current
                else pure (Right (grokCredential current.grokAccessToken))

reloadExternalGrok
    :: ExternalGrokSource
    -> IORef GrokAuthState
    -> IORef (Maybe Text)
    -> IO GrokAuthState
reloadExternalGrok source stateRef payloadRef =
    case source of
        GrokSourceEnvironment -> readIORef stateRef
        GrokSourceFile path -> do
            now <- getCurrentTime
            readGrokAuthFile path >>= \case
                Just raw | Just state <- grokAuthStateFromJson now raw -> do
                    writeIORef stateRef state
                    writeIORef payloadRef (Just raw)
                    pure state
                _ -> readIORef stateRef

refreshExternalGrok
    :: ExternalGrokSource
    -> IORef GrokAuthState
    -> IORef (Maybe Text)
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> GrokAuthState
    -> IO (Either ApiError Credential)
refreshExternalGrok source stateRef payloadRef refresh state =
    refreshGrokState refresh state >>= \case
        Left err -> pure (Left err)
        Right newState -> do
            original <- readIORef payloadRef
            persistResult <- persistGrokPayload
                Nothing
                (case source of
                    GrokSourceFile path -> Just path
                    GrokSourceEnvironment -> Nothing)
                (fromMaybe
                    (TextEncoding.decodeUtf8
                        (LBS.toStrict (Aeson.encode (grokAuthStateToJson state))))
                    original)
                newState
            case persistResult of
                Left err -> pure (Left err)
                Right newPayload -> do
                    writeIORef stateRef newState
                    writeIORef payloadRef (Just newPayload)
                    pure (Right (grokCredential newState.grokAccessToken))

refreshGrokState
    :: (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> GrokAuthState
    -> IO (Either ApiError GrokAuthState)
refreshGrokState refresh state =
    case state.grokRefreshToken of
        Nothing ->
            pure $ Left $ CredentialError
                "Grok OAuth credential has no refresh token; reconnect the account"
        Just refreshToken ->
            refresh refreshToken >>= \case
                Left err -> pure (Left err)
                Right tokens -> do
                    now <- getCurrentTime
                    pure (Right (grokStateFromTokens now state tokens))

persistGrokPayload
    :: Maybe Text
    -> Maybe OsPath
    -> Text
    -> GrokAuthState
    -> IO (Either ApiError Text)
persistGrokPayload managedId filePath original newState = do
    let encoded = encodeGrokPayload original newState
    case managedId of
        Just credentialId ->
            loadManagedCredentialById credentialId >>= \case
                Left err -> pure (Left (ConnectionError err))
                Right (metadata, secret) -> do
                    let newAccountId =
                            fromMaybe metadata.managedAccountId
                                (XAIAuth.accountIdFromAccessToken
                                    newState.grokAccessToken)
                        newMetadata = metadata { managedAccountId = newAccountId }
                        newSecret = secret { secretPayload = encoded }
                    upsertManagedCredentialAfterRefresh newMetadata newSecret
                        >>= \case
                            Left err -> pure (Left (ConnectionError err))
                            Right () -> pure (Right encoded)
        Nothing -> case filePath of
            Just path -> do
                writeLazyFileAtomically path 0o600
                    (LBS.fromStrict (TextEncoding.encodeUtf8 encoded))
                pure (Right encoded)
            Nothing ->
                pure (Right encoded)

encodeGrokPayload :: Text -> GrokAuthState -> Text
encodeGrokPayload original state =
    TextEncoding.decodeUtf8
        (LBS.toStrict
            (Aeson.encode (grokAuthStateToJsonWithKnownFields original state)))

readGrokAuthFile :: OsPath -> IO (Maybe Text)
readGrokAuthFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else Just . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> retryOnFileBusy (LBS.readFile (unsafeToFilePath path))

withGrokSourceLock :: ExternalGrokSource -> IO a -> IO a
withGrokSourceLock source action =
    case source of
        GrokSourceFile filePath ->
            withPrivateFileLock
                (unsafeEncodeUtf
                    (unsafeToFilePath filePath <> ".refresh.lock"))
                action
        GrokSourceEnvironment ->
            action

managedGrokTokenProvider
    :: ManagedCredential
    -> ManagedSecret
    -> GrokAuthState
    -> (Text -> IO (Either ApiError XAIAuth.OAuthTokens))
    -> IO TokenProvider
managedGrokTokenProvider metadata secret initial refresh = do
    stateRef <- newIORef initial
    refreshLock <- newMVar ()
    pure $ tokenProvider metadata.managedBilling \failed ->
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
        Just reported -> credentialsExhaustedForRateLimit reported >>= \case
            Just err -> pure (Left err)
            Nothing -> case reported of
                FailedCredential
                    { credential = rejected
                    , failure = AccountAuthenticationRejected
                    }
                    | rejected.accessToken /= current.grokAccessToken ->
                        pure (Right (grokCredentialFromState metadata current))
                    | otherwise ->
                        refreshManagedGrok metadata secret stateRef refresh current
                _ -> pure $ Left $ CredentialError
                    "unsupported credential failure"
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
                        pure $ Left $ CredentialError
                            "managed Grok OAuth credential became invalid during refresh"
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
            pure $ Left $ CredentialError
                "managed Grok OAuth credential has no refresh token; reconnect the account"
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
    let newState = grokStateFromTokens now state tokens
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

grokStateFromTokens
    :: UTCTime
    -> GrokAuthState
    -> XAIAuth.OAuthTokens
    -> GrokAuthState
grokStateFromTokens now state tokens = GrokAuthState
    { grokAccessToken = tokens.accessToken
    , grokRefreshToken =
        tokens.refreshToken <|> state.grokRefreshToken
    , grokIdToken = tokens.idToken <|> state.grokIdToken
    , grokExpiresAt =
        ((`addUTCTime` now) . fromIntegral
            <$> tokens.expiresInSeconds)
            <|> OpenAI.parseJwtExp tokens.accessToken
    }

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
