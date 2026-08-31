module Agent.CLI.Auth.Gemini
    ( geminiAuthStateFromJson
    , geminiAuthStateToJson
    , geminiNeedsRefresh
    , classifyGeminiRefreshFailure
    , managedGeminiTokenProvider
    ) where

import Agent.CLI.CredentialStore
    ( ManagedAuthKind(ManagedGeminiAuthJson)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , upsertManagedCredentialAfterRefresh
    , withCredentialRefreshFileLock
    )
import Agent.Error (ApiError(..))
import qualified Agent.Gemini.Auth as Gemini
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(GeminiProvider)
    , TokenProvider
    , credentialsExhaustedForRateLimit
    , tokenProvider
    )
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar, withMVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy as LazyText
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

geminiAuthStateFromJson :: Text -> Maybe Gemini.GeminiAuthState
geminiAuthStateFromJson =
    Aeson.decode . LBS.fromStrict . TextEncoding.encodeUtf8

geminiAuthStateToJson :: Gemini.GeminiAuthState -> Text
geminiAuthStateToJson =
    LazyText.toStrict . Aeson.encodeToLazyText

-- | Refresh before expiry so a long streaming request does not begin with a
-- token that is about to become invalid.
geminiNeedsRefresh :: UTCTime -> Gemini.GeminiAuthState -> Bool
geminiNeedsRefresh now state =
    maybe False (<= addUTCTime 600 now) state.expiresAt

managedGeminiTokenProvider
    :: ManagedCredential
    -> ManagedSecret
    -> Gemini.GeminiAuthState
    -> (Text -> IO (Either Text Gemini.OAuthTokens))
    -> IO TokenProvider
managedGeminiTokenProvider metadata secret initial refresh = do
    stateRef <- newIORef initial
    refreshLock <- newMVar ()
    pure $ tokenProvider metadata.managedBilling \failed ->
        withMVar refreshLock \_ ->
            managedGeminiCredential
                metadata secret stateRef refresh failed

managedGeminiCredential
    :: ManagedCredential
    -> ManagedSecret
    -> IORef Gemini.GeminiAuthState
    -> (Text -> IO (Either Text Gemini.OAuthTokens))
    -> Maybe FailedCredential
    -> IO (Either ApiError Credential)
managedGeminiCredential metadata secret stateRef refresh failed = do
    current <- readIORef stateRef
    case failed of
        Just reported -> credentialsExhaustedForRateLimit reported >>= \case
            Just err -> pure (Left err)
            Nothing -> case reported of
                FailedCredential
                    { credential = rejected
                    , failure = AccountAuthenticationRejected
                    }
                    | rejected.accessToken /= current.accessToken ->
                        pure (Right (credentialFromState current))
                    | otherwise ->
                        refreshManagedGemini
                            metadata secret stateRef refresh current
                _ -> pure $ Left $ CredentialError
                    "unsupported credential failure"
        Nothing -> do
            now <- getCurrentTime
            if geminiNeedsRefresh now current
                then refreshManagedGemini
                    metadata secret stateRef refresh current
                else pure (Right (credentialFromState current))

refreshManagedGemini
    :: ManagedCredential
    -> ManagedSecret
    -> IORef Gemini.GeminiAuthState
    -> (Text -> IO (Either Text Gemini.OAuthTokens))
    -> Gemini.GeminiAuthState
    -> IO (Either ApiError Credential)
refreshManagedGemini metadata _secret stateRef refresh stale =
    withCredentialRefreshFileLock $
        loadManagedCredentialById metadata.managedId >>= \case
            Left err -> pure (Left err)
            Right (latestMetadata, latestSecret) ->
                case geminiAuthStateFromJson latestSecret.secretPayload of
                    Nothing ->
                        pure $ Left $ CredentialError
                            "managed Gemini OAuth credential contains invalid auth JSON; reconnect the account"
                    Just current
                        | geminiStateChanged stale current -> do
                            writeIORef stateRef current
                            pure (Right (credentialFromState current))
                        | otherwise ->
                            refreshCurrentGemini
                                latestMetadata latestSecret stateRef
                                refresh current

geminiStateChanged
    :: Gemini.GeminiAuthState
    -> Gemini.GeminiAuthState
    -> Bool
geminiStateChanged stale current =
    stale.accessToken /= current.accessToken
        || stale.refreshToken /= current.refreshToken
        || stale.projectId /= current.projectId

refreshCurrentGemini
    :: ManagedCredential
    -> ManagedSecret
    -> IORef Gemini.GeminiAuthState
    -> (Text -> IO (Either Text Gemini.OAuthTokens))
    -> Gemini.GeminiAuthState
    -> IO (Either ApiError Credential)
refreshCurrentGemini metadata secret stateRef refresh state =
    case state.refreshToken of
        Nothing ->
            pure $ Left $ CredentialError
                "managed Gemini OAuth credential has no refresh token; reconnect the Google account"
        Just refreshToken ->
            refresh refreshToken >>= \case
                Left err -> pure (Left (classifyGeminiRefreshFailure err))
                Right tokens -> do
                    now <- getCurrentTime
                    let newState = Gemini.GeminiAuthState
                            tokens.accessToken
                            (tokens.refreshToken <|> state.refreshToken)
                            ( ((`addUTCTime` now)
                                . fromIntegral
                                <$> tokens.expiresInSeconds)
                                <|> state.expiresAt
                            )
                            state.email
                            state.projectId
                            state.userTier
                        newMetadata = metadata
                            { managedAccountId = newState.email
                            , managedLabel = newState.email
                            }
                        newSecret = secret
                            { secretPayload =
                                geminiAuthStateToJson newState
                            }
                    upsertManagedCredentialAfterRefresh
                        newMetadata newSecret >>= \case
                            Left err ->
                                pure (Left (ConnectionError err))
                            Right () -> do
                                writeIORef stateRef newState
                                pure
                                    (Right
                                        (credentialFromState newState))

-- The low-level OAuth helper deliberately omits token-endpoint bodies so
-- refresh tokens cannot be echoed through errors. Its status-only failure
-- text still lets us distinguish invalid grants from transient transport or
-- provider failures for user-facing recovery.
classifyGeminiRefreshFailure :: Text -> ApiError
classifyGeminiRefreshFailure message
    | any (`Text.isInfixOf` message)
        [ "Google OAuth token request failed with HTTP 400"
        , "Google OAuth token request failed with HTTP 401"
        ] =
            CredentialError message
    | otherwise = ConnectionError message

credentialFromState :: Gemini.GeminiAuthState -> Credential
credentialFromState state = Credential
    { accessToken = state.accessToken
    , accountId = state.email
    , leaseId = Just ("code-assist:" <> state.projectId)
    , provider = GeminiProvider
    }

loadManagedCredentialById
    :: Text
    -> IO (Either ApiError (ManagedCredential, ManagedSecret))
loadManagedCredentialById credentialId =
    loadManagedCredentials >>= \case
        Left err -> pure (Left (ConnectionError err))
        Right credentials ->
            pure $ case listToMaybe
                (filter
                    ((== credentialId) . (.managedId) . fst)
                    credentials) of
                Nothing ->
                    Left $ CredentialError
                        ("managed credential disappeared during refresh: "
                            <> credentialId)
                Just (metadata, _)
                    | not metadata.managedEnabled ->
                        Left $ CredentialError
                            ("managed Gemini credential is disabled: "
                                <> credentialId)
                    | metadata.managedProvider /= GeminiProvider
                        || metadata.managedAuthKind
                            /= ManagedGeminiAuthJson ->
                        Left $ CredentialError
                            ("managed Gemini credential changed auth type: "
                                <> credentialId)
                Just credential -> Right credential
