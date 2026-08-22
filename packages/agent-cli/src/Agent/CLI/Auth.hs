-- | Load ChatGPT, Grok, or OpenRouter credentials for the CLI process.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , GrokAuthState(..)
    , credentialAccountLabel
    , grokCredentialFromAuthJson
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokEmailFromAuthJson
    , loadAuth
    , managedGrokTokenProvider
    , openAIOAuthClientId
    , openAiAuthStateChanged
    , openaiAuthStateFromJson
    , preferredOpenAiTokenProvider
    , probeLoadedAuth
    , reloadableFileCredentialProvider
    , staticCredentialProvider
    , xaiOAuthClientId
    ) where

import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , upsertManagedCredentialAfterRefresh
    , withCredentialRefreshFileLock
    , updateManagedCredentialSecret
    )
import Agent.Error (ApiError(..))
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.Provider
    ( AccountFailure(..)
    , BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    , providerSlug
    , seedTokenProvider
    , tokenProvider
    , tokenProviderBillingMode
    )
import Agent.OpenRouter.Credential (credentialFromApiKey)
import Agent.OsPath (unsafeToFilePath)
import qualified Agent.XAI.Auth as XAIAuth
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (bracket, bracket_)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT
    , runExceptT
    , throwE
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (find, nubBy)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    )
import System.IO (SeekMode(AbsoluteSeek))
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import qualified System.OsPath as OsPath
import System.Posix.Files (setFileMode)
import System.Posix.IO
    ( LockRequest(Unlock, WriteLock)
    , OpenFileFlags(..)
    , OpenMode(ReadWrite)
    , closeFd
    , defaultFileFlags
    , openFd
    , setLock
    , waitToSetLock
    )
import System.Posix.Types (Fd)
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
    , loadedAccountLabel :: !(Credential -> IO Text)
    -- | Live OpenAI OAuth pool, when authentication uses one.
    , loadedOpenAiPool :: !(Maybe OpenAI.Pool)
    }

-- | Human-readable identity for the credential most recently selected by a
-- provider. Prefer an email claim, then fall back to a compact account id.
credentialAccountLabel :: Credential -> Text
credentialAccountLabel credential = case credential.provider of
    OpenAIProvider ->
        fromMaybe (fallback "ChatGPT") $
            OpenAI.deriveEmail credential.accessToken
    XAIProvider ->
        fromMaybe (fallback "Grok") $
            XAIAuth.emailFromToken credential.accessToken
    OpenRouterProvider ->
        fallback "OpenRouter"
  where
    accountId = Text.strip credential.accountId
    fallback providerName
        | Text.null accountId = providerName
        | Text.length accountId <= 12 = accountId
        | otherwise = Text.take 8 accountId <> "…"

credentialAccountLabelWith :: Text -> Credential -> Text
credentialAccountLabelWith preferred credential =
    fromMaybe (credentialAccountLabel credential) $
        credentialEmail credential <|> nonEmptyText preferred

credentialEmail :: Credential -> Maybe Text
credentialEmail credential = case credential.provider of
    OpenAIProvider -> OpenAI.deriveEmail credential.accessToken
    XAIProvider -> XAIAuth.emailFromToken credential.accessToken
    OpenRouterProvider -> Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null trimmed = Nothing
    | otherwise = Just trimmed
  where
    trimmed = Text.strip value

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
                pure $ Left $ CredentialError
                    "credential provider does not match loaded auth"
            | otherwise -> do
                tokenProvider <-
                    seedTokenProvider loaded.loadedTokenProvider credential
                pure $ Right loaded { loadedTokenProvider = tokenProvider }

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
    tokenProvider (tokenProviderBillingMode fallback) \failed ->
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
                    , loadedAccountLabel =
                        pure . credentialAccountLabelWith metadata.managedLabel
                    , loadedOpenAiPool = Nothing
                    }
        Just (metadata, secret) ->
            pure LoadedAuth
                { loadedProvider = XAIProvider
                , loadedTokenProvider =
                    staticCredentialProvider metadata.managedBilling Credential
                        { accessToken = secret.secretPayload
                        , accountId = metadata.managedAccountId
                        , leaseId = Nothing
                        , provider = XAIProvider
                        }
                , loadedAccountLabel =
                    pure . credentialAccountLabelWith metadata.managedLabel
                , loadedOpenAiPool = Nothing
                }
        Nothing -> do
            credential <- lift loadExternalGrokCredential
            case credential of
                Nothing -> throwE noAuthHint
                Just loaded -> do
                    provider <- lift $ reloadableFileCredentialProvider
                        XAIProvider
                        SubscriptionBilled
                        loaded
                        loadExternalGrokCredential
                    pure LoadedAuth
                        { loadedProvider = XAIProvider
                        , loadedTokenProvider = provider
                        , loadedAccountLabel = pure . credentialAccountLabel
                        , loadedOpenAiPool = Nothing
                        }

loadOpenRouterCredential :: IO (Maybe (Credential, Text))
loadOpenRouterCredential = do
    managed <- loadManagedCredential OpenRouterProvider
    case managed of
        Just (metadata, secret) ->
            pure $ Just
                ( (credentialFromApiKey secret.secretPayload)
                    { accountId = metadata.managedAccountId }
                , metadata.managedLabel
                )
        Nothing ->
            fmap (\key -> (credentialFromApiKey key, "")) <$>
                lookupNonEmpty "OPENROUTER_API_KEY"

loadOpenRouter :: ExceptT Text IO LoadedAuth
loadOpenRouter = do
    loadedCredential <- lift loadOpenRouterCredential
    case loadedCredential of
        Nothing -> throwE noAuthHint
        Just (initial, initialLabel) -> do
            provider <- lift $ reloadableFileCredentialProvider
                OpenRouterProvider
                ApiBilled
                initial
                (fmap (fmap fst) loadOpenRouterCredential)
            pure LoadedAuth
                { loadedProvider = OpenRouterProvider
                , loadedTokenProvider = provider
                , loadedAccountLabel = \credential -> do
                    current <- loadOpenRouterCredential
                    let label = case current of
                            Just (currentCredential, currentLabel)
                                | currentCredential.accountId
                                    == credential.accountId ->
                                        currentLabel
                            _ -> initialLabel
                    pure (credentialAccountLabelWith label credential)
                , loadedOpenAiPool = Nothing
                }

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
        Just path -> withAdvisoryFileLock path action

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

withAdvisoryFileLock :: OsPath -> IO a -> IO a
withAdvisoryFileLock path action = do
    createDirectoryIfMissing True (takeDirectory path)
    setFileMode (unsafeToFilePath (takeDirectory path)) 0o700
    bracket
        (openRefreshLock path)
        closeFd
        (\fd -> bracket_ (lockRefreshFd fd) (unlockRefreshFd fd) action)

openRefreshLock :: OsPath -> IO Fd
openRefreshLock path =
    openFd
        (unsafeToFilePath path)
        ReadWrite
        defaultFileFlags { creat = Just 0o600, cloexec = True }

lockRefreshFd :: Fd -> IO ()
lockRefreshFd fd =
    waitToSetLock fd (WriteLock, AbsoluteSeek, 0, 0)

unlockRefreshFd :: Fd -> IO ()
unlockRefreshFd fd =
    setLock fd (Unlock, AbsoluteSeek, 0, 0)

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
            home </> unsafeEncodeUtf ".grok" </> unsafeEncodeUtf "auth.json"
    fileExists <- doesFileExist filePath
    fileJson <- if fileExists
        then Just . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> retryOnFileBusy (LBS.readFile (unsafeToFilePath filePath))
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

grokEmailFromAuthJson :: Text -> Maybe Text
grokEmailFromAuthJson raw = do
    value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    entryEmail value <|> firstNestedEmail value
  where
    entryEmail (Aeson.Object object) =
        textField "email" object
            <|> (textField "id_token" object >>= XAIAuth.emailFromToken)
            <|> (textField "access_token" object >>= XAIAuth.emailFromToken)
            <|> (textField "key" object >>= XAIAuth.emailFromToken)
    entryEmail _ = Nothing

    firstNestedEmail (Aeson.Object object) =
        listToMaybe
            [ email
            | nested <- KeyMap.elems object
            , Just email <- [entryEmail nested]
            ]
    firstNestedEmail _ = Nothing

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
        (home </> unsafeEncodeUtf ".grok" </> unsafeEncodeUtf "auth.json")
    managed <- hasManagedProvider XAIProvider
    pure (isJust envJson || isJust envToken || file || managed)

hasOpenAiAuth :: IO Bool
hasOpenAiAuth = do
    envJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    envToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    home <- getHomeDirectory
    file <- doesFileExist
        (home </> unsafeEncodeUtf ".codex" </> unsafeEncodeUtf "auth.json")
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
    -> BillingMode
    -> Credential
    -> IO (Maybe Credential)
    -> IO TokenProvider
reloadableFileCredentialProvider expectedProvider billing initial reload = do
    cache <- newIORef (Just initial)
    cacheLock <- newMVar ()
    let loadFresh rejectedToken =
            reload >>= \case
                Nothing ->
                    pure $ Left $ CredentialError
                        "no credentials found while reloading auth"
                Just credential
                    | credential.provider /= expectedProvider ->
                        pure $ Left $ CredentialError
                            ("reloaded auth resolved "
                                <> providerSlug credential.provider
                                <> " but this session expects "
                                <> providerSlug expectedProvider)
                    | rejectedToken == Just credential.accessToken ->
                        pure $ Left $ CredentialError
                            "reloaded credential is unchanged; refresh ~/.grok/auth.json or OPENROUTER_API_KEY and retry"
                    | otherwise -> do
                        writeIORef cache (Just credential)
                        pure (Right credential)
        isExplicitReloadRequest rejected =
            -- /reload-auth uses this otherwise-invalid credential as an
            -- explicit cache invalidation request.
            rejected.provider == expectedProvider
                && Text.null rejected.accessToken
                && Text.null rejected.accountId
                && rejected.leaseId == Nothing
        rejectsCachedCredential rejected credential =
            rejected.provider == credential.provider
                && rejected.accessToken == credential.accessToken
    pure $ tokenProvider billing \failed -> case failed of
        Just FailedCredential { failure = AccountRateLimited { retryAfterSeconds } } -> do
            now <- getCurrentTime
            let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
            pure $ Left $ CredentialsExhausted
                (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential
            { credential = rejected
            , failure = AccountAuthenticationRejected
            } ->
            withMVar cacheLock \_ -> do
                current <- readIORef cache
                let forceReload = isExplicitReloadRequest rejected
                case current of
                    Just credential
                        | not forceReload
                        , not (rejectsCachedCredential rejected credential) ->
                            pure (Right credential)
                    _ -> do
                        writeIORef cache Nothing
                        loadFresh
                            (if forceReload
                                then Nothing
                                else Just rejected.accessToken)
        Nothing ->
            withMVar cacheLock \_ ->
                readIORef cache >>= \case
                    Just credential -> pure (Right credential)
                    Nothing -> loadFresh Nothing

staticCredentialProvider :: BillingMode -> Credential -> TokenProvider
staticCredentialProvider billing credential =
    tokenProvider billing \failed -> case failed of
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
                pure $ Left $ CredentialError
                    "static credential was rejected"

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
