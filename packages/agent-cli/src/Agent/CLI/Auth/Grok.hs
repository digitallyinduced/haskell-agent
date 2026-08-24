module Agent.CLI.Auth.Grok
    ( loadExternalGrokCredentials
    , managedGrokTokenProvider
    ) where

import Agent.CLI.Auth.Types
    ( GrokAuthState(..)
    , externalAuthSelectionId
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokCredentialFromAuthJson
    )
import Agent.CLI.CredentialStore
    ( ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , upsertManagedCredentialAfterRefresh
    , withCredentialRefreshFileLock
    )
import Agent.Error (ApiError(..))
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(XAIProvider)
    , TokenProvider
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath (unsafeEncodeUtf, (</>))
import qualified System.OsPath as OsPath
import qualified System.Process.Environment.OsString as Environment

loadExternalGrokCredentials :: IO [(Text, Credential)]
loadExternalGrokCredentials = do
    fromJson <- lookupNonEmpty "GROK_AUTH_JSON"
    fromToken <- lookupNonEmpty "GROK_ACCESS_TOKEN"
    home <- getHomeDirectory
    let filePath =
            home </> unsafeEncodeUtf ".grok" </> unsafeEncodeUtf "auth.json"
    fileExists <- doesFileExist filePath
    fileJson <- if fileExists
        then Just . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> retryOnFileBusy (LBS.readFile (unsafeToFilePath filePath))
        else pure Nothing
    let environmentToken =
            (fromJson >>= grokCredentialFromAuthJson) <|> fromToken
        sourceCredential source token =
            (externalAuthSelectionId XAIProvider source, grokCredential token)
    pure $ catMaybes
        [ sourceCredential "environment" <$> environmentToken
        , sourceCredential (toText filePath)
            <$> (fileJson >>= grokCredentialFromAuthJson)
        ]

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

lookupNonEmpty :: String -> IO (Maybe Text)
lookupNonEmpty name = do
    value <- Environment.getEnv (OsPath.unsafeEncodeUtf name)
    pure $ case value of
        Just raw
            | Right text <- OsPath.decodeUtf raw
            , not (null text) ->
                Just (Text.pack text)
        _ -> Nothing
