module Agent.CLI.Auth.OpenAI
    ( loadOpenAi
    , openAiAuthStateChanged
    , preferredOpenAiTokenProvider
    ) where

import Agent.CLI.Auth.Types
    ( LoadedAuth(..)
    , authStateToJson
    , credentialAccountLabel
    , nonEmptyText
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    )
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , updateManagedCredentialSecret
    )
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.Error (ApiError(..))
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(OpenAIProvider)
    , TokenProvider
    , getNextToken
    , tokenProviderWithNextToken
    )
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (find, nubBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory.OsPath
    ( doesFileExist
    , getHomeDirectory
    )
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

-- | Pin normal checkouts to one OpenAI pool account until that credential is
-- reported as failed. A failure for the selected credential clears the pin
-- and delegates to the pool's normal cooldown and failover behavior; failures
-- from older in-flight credentials leave the newer selection intact.
preferredOpenAiTokenProvider
    :: IORef (Maybe Text)
    -> OpenAI.Pool
    -> TokenProvider
    -> TokenProvider
preferredOpenAiTokenProvider preferredAccount pool fallback =
    tokenProviderWithNextToken fallback \failed ->
        case failed of
            Just reportedFailure -> do
                fallbackResult <-
                    getNextToken fallback (Just reportedFailure)
                let failedAccountId =
                        reportedFailure.credential.accountId
                cleared <- atomicModifyIORef' preferredAccount \current ->
                    if current == Just failedAccountId
                        then (Nothing, True)
                        else (current, False)
                if cleared
                    then pure fallbackResult
                    else readIORef preferredAccount >>= \case
                        Just accountId ->
                            selectedCredential accountId
                        Nothing ->
                            pure fallbackResult
            Nothing ->
                readIORef preferredAccount >>= \case
                    Nothing ->
                        getNextToken fallback Nothing
                    Just accountId ->
                        selectedCredential accountId
  where
    selectedCredential accountId =
        OpenAI.getAccessTokenForAccount pool accountId >>= \case
            Right (accessToken, selectedAccountId) ->
                pure $ Right Credential
                    { accessToken
                    , accountId = selectedAccountId
                    , leaseId = Nothing
                    , provider = OpenAIProvider
                    }
            Left err ->
                pure (Left err)

loadOpenAi :: ExceptT Text IO LoadedAuth
loadOpenAi = do
    (errors, accounts) <- lift loadOpenAiAccounts
    when (null accounts) $
        throwE $ case errors of
            [] -> noAuthHint
            accountErrors ->
                "no valid OpenAI credentials found: "
                    <> Text.intercalate "; " accountErrors
    clientId <-
        lift $
            openAIOAuthClientId <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
    let billing =
            if any ((== SubscriptionBilled) . (.openAiBilling)) accounts
                then SubscriptionBilled
                else ApiBilled
        activeAccounts =
            filter ((== billing) . (.openAiBilling)) accounts
    refreshLock <- lift (newMVar ())
    accountSources <- lift (newIORef activeAccounts)
    pool <- lift $ OpenAI.newDiscoveringPool
        (map (.openAiState) activeAccounts)
        (refreshOpenAiAccount refreshLock clientId accountSources)
        (discoverOpenAiAccounts billing accountSources)
    tokenProvider <- lift
        (OpenAICredential.poolTokenProviderWithBilling billing pool)
    pure LoadedAuth
        { loadedProvider = OpenAIProvider
        , loadedTokenProvider = tokenProvider
        , loadedAccountLabel = \credential -> do
            currentAccounts <- readIORef accountSources
            pure $ maybe
                (credentialAccountLabel credential)
                (.openAiLabel)
                (find
                    ((== credential.accountId)
                        . (.accountId)
                        . (.openAiState))
                    currentAccounts)
        , loadedSelectionId = Nothing
        , loadedOpenAiPool = Just pool
        }

loadOpenAiAccounts :: IO ([Text], [OpenAiAccount])
loadOpenAiAccounts = do
    managedResult <- loadManagedCredentials
    fromEnvToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    fromEnvJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    home <- getHomeDirectory
    let filePath =
            home </> unsafeEncodeUtf ".codex" </> unsafeEncodeUtf "auth.json"
    fileExists <- doesFileExist filePath
    fileBytes <- if fileExists
        then Just <$> retryOnFileBusy (LBS.readFile (unsafeToFilePath filePath))
        else pure Nothing
    now <- getCurrentTime
    let (storeErrors, managed) = case managedResult of
            Left err -> ([err], [])
            Right credentials ->
                ( []
                , [ credential
                  | credential@(metadata, _) <- credentials
                  , metadata.managedProvider == OpenAIProvider
                  ]
                )
    envTokenAccount <- traverse (openAiStaticAccount now) fromEnvToken
    let enabledManaged =
            [ credential
            | credential@(metadata, _) <- managed
            , metadata.managedEnabled
            ]
        (managedErrors, managedAccounts) =
            partitionEithers (map (managedOpenAiAccount now) enabledManaged)
        managedAccountIds =
            map ((.accountId) . (.openAiState)) managedAccounts
        envJsonAccount = do
            state <- fromEnvJson >>= openaiAuthStateFromJson now
                . LBS.fromStrict . TextEncoding.encodeUtf8
            pure OpenAiAccount
                { openAiState = state
                , openAiSource = OpenAiEnvironmentOAuth
                , openAiLabel = openAiStateLabel "ChatGPT" state
                , openAiBilling = SubscriptionBilled
                }
        fileAccount = do
            state <- fileBytes >>= openaiAuthStateFromJson now
            pure OpenAiAccount
                { openAiState = state
                , openAiSource = OpenAiAuthFile filePath
                , openAiLabel = openAiStateLabel "ChatGPT" state
                , openAiBilling = SubscriptionBilled
                }
        externalAccounts =
            filter
                (\account ->
                    account.openAiState.accountId `notElem` managedAccountIds)
                (catMaybes [envTokenAccount, envJsonAccount, fileAccount])
        accounts = deduplicateOpenAiAccounts
            (managedAccounts <> externalAccounts)
    pure (storeErrors <> managedErrors, accounts)

discoverOpenAiAccounts
    :: BillingMode
    -> IORef [OpenAiAccount]
    -> [Text]
    -> IO (Either ApiError [OpenAI.AuthState])
discoverOpenAiAccounts billing accountSources knownAccountIds = do
    (errors, accounts) <- loadOpenAiAccounts
    let additional =
            filter
                (\account ->
                    account.openAiBilling == billing
                        && account.openAiState.accountId `notElem` knownAccountIds)
                accounts
    if null additional
        then pure $ case errors of
            [] -> Right []
            _ -> Left (ConnectionError (Text.intercalate "; " errors))
        else do
            atomicModifyIORef' accountSources \knownSources ->
                (deduplicateOpenAiAccounts (knownSources <> additional), ())
            pure (Right (map (.openAiState) additional))

data OpenAiCredentialSource
    = OpenAiManagedOAuth !Text
    | OpenAiManagedBearer
    | OpenAiEnvironmentOAuth
    | OpenAiEnvironmentBearer
    | OpenAiAuthFile !OsPath

data OpenAiAccount = OpenAiAccount
    { openAiState :: !OpenAI.AuthState
    , openAiSource :: !OpenAiCredentialSource
    , openAiLabel :: !Text
    , openAiBilling :: !BillingMode
    }

managedOpenAiAccount
    :: UTCTime
    -> (ManagedCredential, ManagedSecret)
    -> Either Text OpenAiAccount
managedOpenAiAccount now (metadata, secret) =
    case metadata.managedAuthKind of
        ManagedOpenAIAuthJson ->
            case openaiAuthStateFromJson now
                (LBS.fromStrict (TextEncoding.encodeUtf8 secret.secretPayload)) of
                Nothing ->
                    Left $
                        "managed OpenAI OAuth credential "
                            <> metadata.managedId
                            <> " contains invalid auth JSON"
                Just state
                    | state.accountId /= metadata.managedAccountId ->
                        Left $
                            "managed OpenAI credential "
                                <> metadata.managedId
                                <> " account id does not match its auth payload"
                    | otherwise ->
                        Right OpenAiAccount
                            { openAiState = state
                            , openAiSource =
                                OpenAiManagedOAuth metadata.managedId
                            , openAiLabel =
                                openAiStateLabel metadata.managedLabel state
                            , openAiBilling = metadata.managedBilling
                            }
        _ ->
            Right OpenAiAccount
                { openAiState = staticOpenAiState
                    now metadata.managedAccountId secret.secretPayload
                , openAiSource = OpenAiManagedBearer
                , openAiLabel = fromMaybe
                    (credentialAccountLabel Credential
                        { accessToken = secret.secretPayload
                        , accountId = metadata.managedAccountId
                        , leaseId = Nothing
                        , provider = OpenAIProvider
                        })
                    (nonEmptyText metadata.managedLabel)
                , openAiBilling = metadata.managedBilling
                }

openAiStaticAccount :: UTCTime -> Text -> IO OpenAiAccount
openAiStaticAccount now token = do
    accountId <- openaiAccountIdForToken token
    pure OpenAiAccount
        { openAiState = staticOpenAiState now accountId token
        , openAiSource = OpenAiEnvironmentBearer
        , openAiLabel =
            credentialAccountLabel Credential
                { accessToken = token
                , accountId
                , leaseId = Nothing
                , provider = OpenAIProvider
                }
        , openAiBilling = SubscriptionBilled
        }

openAiStateLabel :: Text -> OpenAI.AuthState -> Text
openAiStateLabel preferred state =
    fromMaybe fallback $
        (state.idToken >>= OpenAI.deriveEmail)
            <|> OpenAI.deriveEmail state.accessToken
            <|> nonEmptyText preferred
  where
    fallback =
        credentialAccountLabel Credential
            { accessToken = state.accessToken
            , accountId = state.accountId
            , leaseId = Nothing
            , provider = OpenAIProvider
            }

staticOpenAiState :: UTCTime -> Text -> Text -> OpenAI.AuthState
staticOpenAiState now accountId accessToken =
    OpenAI.AuthState
        { accessToken
        , refreshToken = ""
        , accountId
        , idToken = Nothing
        , lastRefresh = now
        }

deduplicateOpenAiAccounts :: [OpenAiAccount] -> [OpenAiAccount]
deduplicateOpenAiAccounts =
    nubBy ((==) `on` ((.accountId) . (.openAiState)))

refreshOpenAiAccount
    :: MVar ()
    -> Text
    -> IORef [OpenAiAccount]
    -> OpenAI.AuthState
    -> IO (Either ApiError OpenAI.AuthState)
refreshOpenAiAccount lock clientId accountSources stale =
    withMVar lock \_ -> do
        accounts <- readIORef accountSources
        case find
            ((== stale.accountId) . (.accountId) . (.openAiState))
            accounts of
            Nothing ->
                pure $ Left $ CredentialError
                    ("OpenAI refresh source is unavailable for account "
                        <> stale.accountId)
            Just account ->
                withOpenAiSourceLock account.openAiSource do
                    reloadOpenAiAccount account.openAiSource stale >>= \case
                        Left err -> pure (Left err)
                        Right current
                            | current.accountId /= stale.accountId ->
                                pure $ Left $ CredentialError
                                    "OpenAI auth source changed account identity"
                            | openAiAuthStateChanged stale current ->
                                pure (Right current)
                            | otherwise ->
                                OpenAI.refreshAccessTokenHTTP clientId current >>= \case
                                    Left err -> pure (Left err)
                                    Right newState
                                        | newState.accountId /= stale.accountId ->
                                            pure $ Left $ CredentialError
                                                "OpenAI refresh changed account identity"
                                        | otherwise ->
                                            persistRefreshedOpenAiAccount
                                                account.openAiSource newState

openAiAuthStateChanged :: OpenAI.AuthState -> OpenAI.AuthState -> Bool
openAiAuthStateChanged stale current =
    current.accessToken /= stale.accessToken
        || current.refreshToken /= stale.refreshToken

withOpenAiSourceLock :: OpenAiCredentialSource -> IO a -> IO a
withOpenAiSourceLock source action =
    openAiSourceLockPath source >>= \case
        Nothing -> action
        Just path -> withPrivateFileLock path action

openAiSourceLockPath :: OpenAiCredentialSource -> IO (Maybe OsPath)
openAiSourceLockPath source =
    case source of
        OpenAiManagedOAuth managedId ->
            Just <$> managedRefreshLockPath managedId
        OpenAiAuthFile filePath ->
            pure (Just (unsafeEncodeUtf (unsafeToFilePath filePath <> ".refresh.lock")))
        _ ->
            pure Nothing

managedRefreshLockPath :: Text -> IO OsPath
managedRefreshLockPath managedId = do
    home <- getHomeDirectory
    let fileName =
            "refresh-" <> Text.unpack (safeLockName managedId) <> ".lock"
    pure $
        home
            </> unsafeEncodeUtf ".haskell-agent"
            </> unsafeEncodeUtf "credentials"
            </> unsafeEncodeUtf fileName

safeLockName :: Text -> Text
safeLockName = Text.map replace
  where
    replace character
        | Text.any (== character) allowed = character
        | otherwise = '-'
    allowed =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

reloadOpenAiAccount
    :: OpenAiCredentialSource
    -> OpenAI.AuthState
    -> IO (Either ApiError OpenAI.AuthState)
reloadOpenAiAccount source stale =
    case source of
        OpenAiManagedOAuth managedId ->
            loadManagedCredentials >>= \case
                Left err -> pure (Left (ConnectionError err))
                Right credentials ->
                    case find
                        ((== managedId) . (.managedId) . fst)
                        credentials of
                        Nothing ->
                            pure $ Left $ CredentialError
                                ("managed OpenAI credential " <> managedId
                                    <> " no longer exists")
                        Just (metadata, secret)
                            | not metadata.managedEnabled ->
                                pure $ Left $ CredentialError
                                    ("managed OpenAI credential " <> managedId
                                        <> " is disabled")
                            | otherwise -> do
                                now <- getCurrentTime
                                pure $ case openaiAuthStateFromJson now
                                    (LBS.fromStrict
                                        (TextEncoding.encodeUtf8
                                            secret.secretPayload)) of
                                    Nothing ->
                                        Left $ CredentialError
                                            ("managed OpenAI credential "
                                                <> managedId
                                                <> " contains invalid auth JSON")
                                    Just current -> Right current
        OpenAiAuthFile filePath -> do
            exists <- doesFileExist filePath
            if not exists
                then pure $ Left $ CredentialError
                    "OpenAI auth file no longer exists"
                else do
                    now <- getCurrentTime
                    bytes <- retryOnFileBusy
                        (LBS.readFile (unsafeToFilePath filePath))
                    pure $ case openaiAuthStateFromJson now bytes of
                        Nothing ->
                            Left $ CredentialError
                                "OpenAI auth file contains invalid auth JSON"
                        Just current -> Right current
        OpenAiEnvironmentOAuth ->
            pure (Right stale)
        OpenAiManagedBearer ->
            staticRefreshError stale.accountId
        OpenAiEnvironmentBearer ->
            staticRefreshError stale.accountId

staticRefreshError :: Text -> IO (Either ApiError OpenAI.AuthState)
staticRefreshError accountId =
    pure $ Left $ CredentialError
        ("OpenAI account " <> accountId
            <> " uses a static bearer token that cannot be refreshed")

persistRefreshedOpenAiAccount
    :: OpenAiCredentialSource
    -> OpenAI.AuthState
    -> IO (Either ApiError OpenAI.AuthState)
persistRefreshedOpenAiAccount source newState = do
    stamped <- authStateToJson newState <$> getCurrentTime
    case source of
        OpenAiManagedOAuth managedId -> do
            let payload =
                    TextEncoding.decodeUtf8
                        (LBS.toStrict (Aeson.encode stamped))
            updateManagedCredentialSecret managedId payload
                >>= \case
                    Left err -> pure $ Left $ ConnectionError err
                    Right () -> pure (Right newState)
        OpenAiAuthFile filePath ->
            OpenAILogin.writeAuthFile filePath stamped
                >> pure (Right newState)
        OpenAiEnvironmentOAuth ->
            pure (Right newState)
        OpenAiManagedBearer ->
            staticRefreshError newState.accountId
        OpenAiEnvironmentBearer ->
            staticRefreshError newState.accountId

openaiAccountIdForToken :: Text -> IO Text
openaiAccountIdForToken token = do
    fromAccount <- lookupNonEmpty "CODEX_ACCOUNT_ID"
    fromIdToken <- lookupNonEmpty "CODEX_ID_TOKEN"
    pure $ fromMaybe "" $
        fromAccount
            <|> (fromIdToken >>= OpenAI.deriveAccountId)
            <|> OpenAI.deriveAccountId token


noAuthHint :: Text
noAuthHint =
    "no credentials found. Set GROK_ACCESS_TOKEN, CODEX_ACCESS_TOKEN, \
    \or OPENROUTER_API_KEY, place auth at ~/.grok/auth.json / ~/.codex/auth.json, \
    \or use --provider claude-code after `claude auth login`."
