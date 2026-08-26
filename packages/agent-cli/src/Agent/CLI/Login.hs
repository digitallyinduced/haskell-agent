-- | Interactive credential dashboard for @/login@.
module Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , LoginAction(..)
    , LoginState(..)
    , UsageState(..)
    , UsageWindow(..)
    , applyLoginKey
    , connectProviderAccount
    , discoverLoginAccounts
    , discoverSelectableLoginAccounts
    , formatLoginAccounts
    , initialLoginState
    , loginAccountSelectionId
    , refreshLoginAccount
    , renderLoginFrame
    , runLoginManager
    ) where

import Agent.CLI.Auth
    ( GrokAuthState(..)
    , externalAuthSelectionId
    , grokAuthStateToJson
    , grokCredentialFromAuthJson
    , grokEmailFromAuthJson
    , managedAuthSelectionId
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , xaiOAuthClientId
    )
import Agent.CLI.Auth.Grok (refreshGrokLoginPayload)
import Agent.Error (ApiError)
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , deleteManagedCredential
    , loadManagedCredentials
    , newManagedCredentialId
    , setManagedCredentialEnabled
    , upsertManagedCredential
    )
import Agent.CLI.Error
    ( formatApiErrorInlineAt
    , formatException
    )
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Picker
    ( PickerKey(..)
    , runOverlay
    , runOverlayWithUpdates
    )
import Agent.CLI.Style
    ( glyphErr
    , glyphOk
    , roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.OsPath (toText, unsafeToFilePath)
import qualified Agent.OpenAI.Usage as OpenAI
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , providerSlug
    )
import qualified Agent.XAI.Auth as XAIAuth
import qualified Agent.XAI.Usage as XAI
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( mapConcurrently
    , mapConcurrently_
    , withAsync
    )
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (join, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.IO
    ( hFlush
    , hGetEcho
    , hIsTerminalDevice
    , hSetEcho
    , stderr
    , stdin
    )

data AccountBilling
    = SubscriptionBilling !(Maybe Text)
    | ApiCreditsBilling
    deriving (Eq, Show)

data UsageWindow = UsageWindow
    { windowName :: !Text
    , usedPercent :: !Int
    , windowSeconds :: !Int
    , resetsAt :: !UTCTime
    }
    deriving (Eq, Show)

data AccountUsage = AccountUsage
    { usagePlan :: !(Maybe Text)
    , usageWindows :: ![UsageWindow]
    , creditsRemaining :: !(Maybe Text)
    , creditsUsed :: !(Maybe Text)
    }
    deriving (Eq, Show)

data UsageState
    = UsageNotChecked
    | UsageAvailable !AccountUsage
    | UsageUnavailable !Text
    deriving (Eq, Show)

data LoginAccount = LoginAccount
    { loginManagedId :: !(Maybe Text)
    , loginProvider :: !Provider
    , loginAccountId :: !Text
    , loginLabel :: !Text
    , loginBilling :: !AccountBilling
    , loginSource :: !Text
    , loginUsage :: !UsageState
    , loginAccessToken :: !Text
    , loginAuthKind :: !ManagedAuthKind
    , loginSecretPayload :: !Text
    , loginEnabled :: !Bool
    }
    deriving (Eq)

instance Show LoginAccount where
    show account =
        "LoginAccount { loginProvider = "
            <> show account.loginProvider
            <> ", loginAccountId = "
            <> show account.loginAccountId
            <> ", loginLabel = "
            <> show account.loginLabel
            <> ", loginBilling = "
            <> show account.loginBilling
            <> ", loginSource = "
            <> show account.loginSource
            <> ", loginUsage = "
            <> show account.loginUsage
            <> ", loginAccessToken = <redacted> }"

data LoginState = LoginState
    { loginAccounts :: ![LoginAccount]
    , loginIndex :: !Int
    }
    deriving (Eq, Show)

data LoginAction
    = LoginClose
    | LoginRefresh !Int
    | LoginAdd
    | LoginToggle !Int
    | LoginDelete !Int
    | LoginImport !Int
    deriving (Eq, Show)

initialLoginState :: [LoginAccount] -> LoginState
initialLoginState accounts = LoginState
    { loginAccounts = accounts
    , loginIndex = 0
    }

applyLoginKey :: PickerKey -> LoginState -> Either LoginAction LoginState
applyLoginKey key state = case key of
    PickerKeyCancel -> Left LoginClose
    PickerKeyConfirm -> refresh
    PickerKeyChar 'r' -> refresh
    PickerKeyChar 'R' -> refresh
    PickerKeyChar 'a' -> Left LoginAdd
    PickerKeyChar 'A' -> Left LoginAdd
    PickerKeyChar 'e' -> selected LoginToggle
    PickerKeyChar 'E' -> selected LoginToggle
    PickerKeyChar 'd' -> selected LoginDelete
    PickerKeyChar 'D' -> selected LoginDelete
    PickerKeyChar 'i' -> selected LoginImport
    PickerKeyChar 'I' -> selected LoginImport
    PickerKeyUp -> Right (move (-1))
    PickerKeyDown -> Right (move 1)
    _ -> Right state
  where
    count = length state.loginAccounts
    move delta
        | count == 0 = state { loginIndex = 0 }
        | otherwise =
            state
                { loginIndex = (state.loginIndex + delta) `mod` count
                }
    refresh
        | count == 0 = Left LoginClose
        | otherwise = Left (LoginRefresh (min (count - 1) state.loginIndex))
    selected constructor
        | count == 0 = Right state
        | otherwise = Left (constructor (min (count - 1) state.loginIndex))

runLoginManager :: Bool -> IO ()
runLoginManager color = do
    accounts <- discoverLoginAccounts
    tty <- hIsTerminalDevice stdin
    if not tty
        then do
            refreshed <- mapConcurrently refreshLoginAccount accounts
            Text.hPutStrLn stderr (formatLoginAccounts color refreshed)
            hFlush stderr
        else loop [0 .. length accounts - 1] (initialLoginState accounts)
  where
    loop refreshIndices state = do
        updates <- newChan
        result <- withAsync
            (refreshSelectedAccounts updates refreshIndices state.loginAccounts)
            \_ ->
                runOverlayWithUpdates
                    (renderLoginFrame color)
                    applyLoginKey
                    (readChan updates)
                    applyRefreshedAccount
                    state
        case result of
            Nothing -> pure ()
            Just (LoginClose, _) -> pure ()
            Just (LoginRefresh index, state') ->
                loop [index] state'
            Just (LoginAdd, _) -> do
                void (connectAccount color)
                rediscover
            Just (LoginToggle index, state') -> do
                toggleAt color index state'.loginAccounts
                rediscover
            Just (LoginDelete index, state') -> do
                deleteAt color index state'.loginAccounts
                rediscover
            Just (LoginImport index, state') -> do
                importAt color index state'.loginAccounts
                rediscover
      where
        rediscover = do
            accounts <- discoverLoginAccounts
            loop [0 .. length accounts - 1] (initialLoginState accounts)

refreshSelectedAccounts
    :: Chan (Int, LoginAccount)
    -> [Int]
    -> [LoginAccount]
    -> IO ()
refreshSelectedAccounts updates indices accounts =
    mapConcurrently_ refreshOne indices
  where
    refreshOne index = case accountAt index accounts of
        Nothing -> pure ()
        Just account -> do
            refreshed <- tryAny (refreshLoginAccount account) >>= \case
                Left err ->
                    pure account
                        { loginUsage =
                            UsageUnavailable
                                ("usage check failed: " <> formatException err)
                        }
                Right result -> pure result
            writeChan updates (index, refreshed)

applyRefreshedAccount
    :: (Int, LoginAccount)
    -> LoginState
    -> LoginState
applyRefreshedAccount (index, refreshed) state =
    state { loginAccounts = replaceAt index refreshed state.loginAccounts }

replaceAt :: Int -> account -> [account] -> [account]
replaceAt index replacement accounts =
    case splitAt index accounts of
        (before, _ : after) -> before <> (replacement : after)
        _ -> accounts

discoverLoginAccounts :: IO [LoginAccount]
discoverLoginAccounts = do
    accounts <- discoverLoginAccountSources
    pure (nubBy sameAccount accounts)
  where
    sameAccount left right =
        left.loginProvider == right.loginProvider
            && left.loginAccountId == right.loginAccountId

-- | Accounts that can be selected in a live session. Unlike the login
-- dashboard, disabled managed entries do not shadow usable external sources,
-- and distinct managed credentials remain separately addressable.
discoverSelectableLoginAccounts :: IO [LoginAccount]
discoverSelectableLoginAccounts = do
    accounts <- filter (.loginEnabled) <$> discoverLoginAccountSources
    pure (nubBy sameSelection accounts)
  where
    sameSelection left right =
        loginAccountSelectionId left == loginAccountSelectionId right

loginAccountSelectionId :: LoginAccount -> Text
loginAccountSelectionId account =
    case account.loginManagedId of
        Just managedId -> managedAuthSelectionId managedId
        Nothing ->
            externalAuthSelectionId
                account.loginProvider
                account.loginSource

discoverLoginAccountSources :: IO [LoginAccount]
discoverLoginAccountSources = do
    home <- getHomeDirectory
    now <- getCurrentTime
    openaiEnv <- discoverOpenAIEnv
    openaiFile <- discoverOpenAIFile now
        (home </> unsafeEncodeUtf ".codex" </> unsafeEncodeUtf "auth.json")
    grokEnv <- discoverGrokEnv
    grokFile <- discoverGrokFile
        (home </> unsafeEncodeUtf ".grok" </> unsafeEncodeUtf "auth.json")
    openRouter <- discoverOpenRouter
    managed <- loadManagedCredentials
    let managedAccounts = case managed of
            Left _ -> []
            Right entries -> map (managedLoginAccount now) entries
    pure $
        managedAccounts
            <> catMaybes
                [ openaiEnv
                , openaiFile
                , grokEnv
                , grokFile
                , openRouter
                ]

managedLoginAccount
    :: UTCTime
    -> (ManagedCredential, ManagedSecret)
    -> LoginAccount
managedLoginAccount now (metadata, secret) =
    LoginAccount
        { loginManagedId = Just metadata.managedId
        , loginProvider = metadata.managedProvider
        , loginAccountId = metadata.managedAccountId
        , loginLabel = fromMaybe metadata.managedLabel managedAccountEmail
        , loginBilling = case metadata.managedBilling of
            SubscriptionBilled -> SubscriptionBilling Nothing
            ApiBilled -> ApiCreditsBilling
        , loginSource = "managed"
        , loginUsage = UsageNotChecked
        , loginAccessToken = accessToken
        , loginAuthKind = metadata.managedAuthKind
        , loginSecretPayload = secret.secretPayload
        , loginEnabled = metadata.managedEnabled
        }
  where
    managedAccountEmail = case metadata.managedProvider of
        OpenAIProvider -> openAIAccountEmail =<< openAIAuth
        XAIProvider -> case metadata.managedAuthKind of
            ManagedGrokAuthJson ->
                grokEmailFromAuthJson secret.secretPayload
            ManagedBearerToken ->
                XAIAuth.emailFromToken secret.secretPayload
            ManagedOpenAIAuthJson -> Nothing
        OpenRouterProvider -> Nothing
        ClaudeCodeProvider -> Nothing
    openAIAuth = case metadata.managedAuthKind of
        ManagedOpenAIAuthJson ->
            openaiAuthStateFromJson now
                (LBS.fromStrict (Text.encodeUtf8 secret.secretPayload))
        _ -> Nothing
    accessToken = fromMaybe "" case metadata.managedAuthKind of
        ManagedBearerToken -> Just secret.secretPayload
        ManagedOpenAIAuthJson -> (.accessToken) <$> openAIAuth
        ManagedGrokAuthJson ->
            grokCredentialFromAuthJson secret.secretPayload

toggleAt :: Bool -> Int -> [LoginAccount] -> IO ()
toggleAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account -> case account.loginManagedId of
            Nothing ->
                printLoginMessage color False
                    "external credentials are read-only; import them before changing state"
            Just credentialId ->
                setManagedCredentialEnabled
                    credentialId
                    (not account.loginEnabled)
                    >>= printStoreResult color
                        (if account.loginEnabled
                            then "credential disabled"
                            else "credential enabled")

deleteAt :: Bool -> Int -> [LoginAccount] -> IO ()
deleteAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account -> case account.loginManagedId of
            Nothing ->
                printLoginMessage color False
                    "external credentials are read-only; remove them from their source"
            Just credentialId ->
                readApprovalLine
                    ("Disconnect " <> account.loginLabel <> "? [y/N] ")
                    >>= \case
                        Just answer
                            | Text.toLower (Text.strip answer) `elem` ["y", "yes"] ->
                                deleteManagedCredential credentialId
                                    >>= printStoreResult color
                                        "credential disconnected"
                        _ -> pure ()

importAt :: Bool -> Int -> [LoginAccount] -> IO ()
importAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account -> case account.loginManagedId of
            Just _ ->
                printLoginMessage color False "credential is already managed"
            Nothing
                | Text.null account.loginSecretPayload ->
                    printLoginMessage color False
                        "this credential source cannot be imported"
                | otherwise -> do
                    credentialId <-
                        newManagedCredentialId
                            account.loginProvider account.loginAccountId
                    upsertManagedCredential
                        ManagedCredential
                            { managedId = credentialId
                            , managedProvider = account.loginProvider
                            , managedAccountId = account.loginAccountId
                            , managedLabel = account.loginLabel
                            , managedBilling = case account.loginBilling of
                                SubscriptionBilling _ -> SubscriptionBilled
                                ApiCreditsBilling -> ApiBilled
                            , managedAuthKind = account.loginAuthKind
                            , managedEnabled = True
                            }
                        ManagedSecret
                            { secretManagedId = credentialId
                            , secretPayload = account.loginSecretPayload
                            }
                        >>= printStoreResult color
                            "credential imported into the managed store"

accountAt :: Int -> [account] -> Maybe account
accountAt index accounts =
    case drop index accounts of
        account : _ -> Just account
        [] -> Nothing

connectAccount :: Bool -> IO (Maybe (Provider, Text))
connectAccount color =
    pickConnectProvider color >>= \case
        Nothing -> pure Nothing
        Just provider ->
            fmap
                (fmap (\accountId -> (provider, accountId)))
                (connectProviderAccount color provider)

-- | Connect one account for the requested provider and return its provider
-- account id after it has been stored successfully.
connectProviderAccount :: Bool -> Provider -> IO (Maybe Text)
connectProviderAccount color = \case
    OpenAIProvider -> connectOpenAI color
    XAIProvider -> connectXAI color
    OpenRouterProvider -> connectOpenRouter color
    ClaudeCodeProvider -> do
        printLoginMessage color False
            "Claude Code subscriptions are managed by `claude auth login`"
        pure Nothing

pickConnectProvider :: Bool -> IO (Maybe Provider)
pickConnectProvider color =
    join <$> runOverlay render step (0 :: Int)
  where
    providers =
        [ OpenAIProvider
        , XAIProvider
        , OpenRouterProvider
        , ClaudeCodeProvider
        ]
    render index =
        Text.intercalate "\n" $
            [rolePrompt color "connect account"]
                <> zipWith
                    (\i provider ->
                        (if i == index then roleWarn color "› " else "  ")
                            <> roleMuted color (providerSlug provider))
                    [0 ..]
                    providers
                <> [roleMuted color "↑↓/jk or scroll · click/enter · esc/q"]
    step key index = case key of
        PickerKeyCancel -> Left Nothing
        PickerKeyConfirm -> Left (accountAt index providers)
        PickerKeyUp ->
            Right ((index - 1) `mod` length providers)
        PickerKeyDown ->
            Right ((index + 1) `mod` length providers)
        _ -> Right index

connectOpenAI :: Bool -> IO (Maybe Text)
connectOpenAI color = do
    clientId <-
        openAIOAuthClientId <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
    let options = OpenAILogin.defaultLoginOptions clientId
    OpenAILogin.requestDeviceCode options >>= \case
        Left err -> printLoginMessage color False err >> pure Nothing
        Right device -> do
            Text.hPutStrLn stderr $
                roleMuted color "Open "
                    <> rolePrompt color (Text.pack device.verificationUrl)
            Text.hPutStrLn stderr $
                roleMuted color "Enter code "
                    <> rolePrompt color device.userCode
            hFlush stderr
            OpenAILogin.completeDeviceCodeLogin options device >>= \case
                Left err -> printLoginMessage color False err >> pure Nothing
                Right authJson -> do
                    now <- getCurrentTime
                    case openaiAuthStateFromJson now (Aeson.encode authJson) of
                        Nothing ->
                            printLoginMessage color False
                                "OpenAI login returned invalid account data"
                                >> pure Nothing
                        Just auth ->
                            storeConnectedCredential color
                                OpenAIProvider
                                auth.accountId
                                (fromMaybe "ChatGPT"
                                    (openAIAccountEmail auth))
                                SubscriptionBilled
                                ManagedOpenAIAuthJson
                                (Text.decodeUtf8
                                    (LBS.toStrict (Aeson.encode authJson)))
                                >>= \stored ->
                                    pure $
                                        if stored
                                            then Just auth.accountId
                                            else Nothing

connectXAI :: Bool -> IO (Maybe Text)
connectXAI color = do
    clientId <-
        xaiOAuthClientId <$> lookupNonEmpty "XAI_OAUTH_CLIENT_ID"
    let options = XAIAuth.defaultOAuthOptions clientId
    XAIAuth.requestDeviceAuthorization options >>= \case
        Left err -> printLoginMessage color False err >> pure Nothing
        Right device -> do
            Text.hPutStrLn stderr $
                roleMuted color "Open "
                    <> rolePrompt color device.verificationUrl
            Text.hPutStrLn stderr $
                roleMuted color "Enter code "
                    <> rolePrompt color device.userCode
            hFlush stderr
            XAIAuth.completeDeviceAuthorization options device >>= \case
                Left err -> printLoginMessage color False err >> pure Nothing
                Right tokens
                    | Nothing <- tokens.refreshToken ->
                        printLoginMessage color False
                            "Grok login did not return a refresh token; reconnect with offline access"
                            >> pure Nothing
                    | otherwise -> do
                        now <- getCurrentTime
                        let accountId =
                                fromMaybe "grok"
                                    (XAIAuth.accountIdFromAccessToken
                                        tokens.accessToken)
                            label = fromMaybe "Grok" $
                                (tokens.idToken >>= XAIAuth.emailFromToken)
                                    <|> XAIAuth.emailFromToken
                                        tokens.accessToken
                            authJson = grokAuthStateToJson GrokAuthState
                                { grokAccessToken = tokens.accessToken
                                , grokRefreshToken = tokens.refreshToken
                                , grokIdToken = tokens.idToken
                                , grokExpiresAt =
                                    ((`addUTCTime` now) . fromIntegral
                                        <$> tokens.expiresInSeconds)
                                        <|> OpenAI.parseJwtExp
                                            tokens.accessToken
                                }
                        storeConnectedCredential color
                            XAIProvider
                            accountId
                            label
                            SubscriptionBilled
                            ManagedGrokAuthJson
                            (Text.decodeUtf8
                                (LBS.toStrict (Aeson.encode authJson)))
                            >>= \stored ->
                                pure $
                                    if stored
                                        then Just accountId
                                        else Nothing

connectOpenRouter :: Bool -> IO (Maybe Text)
connectOpenRouter color =
    readSecretLine "OpenRouter API key: " >>= \case
        Nothing -> pure Nothing
        Just apiKey ->
            OpenRouter.fetchOpenRouterUsage apiKey >>= \case
                Left err ->
                    printLoginMessage color False
                        ("OpenRouter rejected the key: " <> err)
                        >> pure Nothing
                Right usage -> do
                    let accountId =
                            fromMaybe "openrouter" usage.keyLabel
                        label =
                            fromMaybe "OpenRouter" usage.keyLabel
                    storeConnectedCredential color
                        OpenRouterProvider
                        accountId
                        label
                        ApiBilled
                        ManagedBearerToken
                        apiKey
                        >>= \stored ->
                            pure $
                                if stored
                                    then Just accountId
                                    else Nothing

storeConnectedCredential
    :: Bool
    -> Provider
    -> Text
    -> Text
    -> BillingMode
    -> ManagedAuthKind
    -> Text
    -> IO Bool
storeConnectedCredential color provider accountId label billing authKind payload = do
    credentialId <- newManagedCredentialId provider accountId
    result <- upsertManagedCredential
        ManagedCredential
            { managedId = credentialId
            , managedProvider = provider
            , managedAccountId = accountId
            , managedLabel = label
            , managedBilling = billing
            , managedAuthKind = authKind
            , managedEnabled = True
            }
        ManagedSecret
            { secretManagedId = credentialId
            , secretPayload = payload
            }
    printStoreResult color
        "credential connected"
        result
    pure $ case result of
        Left _ -> False
        Right () -> True

readSecretLine :: Text -> IO (Maybe Text)
readSecretLine prompt = do
    Text.hPutStr stderr prompt
    hFlush stderr
    tty <- hIsTerminalDevice stdin
    if not tty
        then nonEmpty . Text.strip <$> Text.getLine
        else do
            oldEcho <- hGetEcho stdin
            value <- bracket
                (hSetEcho stdin False)
                (const (hSetEcho stdin oldEcho))
                (\() -> Text.getLine)
            Text.hPutStrLn stderr ""
            pure (nonEmpty (Text.strip value))
  where
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value

printStoreResult :: Bool -> Text -> Either Text () -> IO ()
printStoreResult color success = \case
    Left err -> printLoginMessage color False err
    Right () -> printLoginMessage color True success

printLoginMessage :: Bool -> Bool -> Text -> IO ()
printLoginMessage color success message = do
    Text.hPutStrLn stderr $
        if success
            then roleSuccess color (glyphOk <> message)
            else roleError color (glyphErr <> message)
    hFlush stderr

discoverOpenAIEnv :: IO (Maybe LoginAccount)
discoverOpenAIEnv = do
    token <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    explicitAccount <- lookupNonEmpty "CODEX_ACCOUNT_ID"
    idToken <- lookupNonEmpty "CODEX_ID_TOKEN"
    pure $ do
        accessToken <- token
        let accountId =
                fromMaybe "openai-env" $
                    explicitAccount
                        <|> (idToken >>= OpenAI.deriveAccountId)
                        <|> OpenAI.deriveAccountId accessToken
            label = fromMaybe "ChatGPT" $
                (idToken >>= OpenAI.deriveEmail)
                    <|> OpenAI.deriveEmail accessToken
        pure $ subscriptionAccount
            OpenAIProvider accountId label "environment"
            accessToken ManagedBearerToken accessToken

discoverOpenAIFile :: UTCTime -> OsPath -> IO (Maybe LoginAccount)
discoverOpenAIFile now path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            bytes <- retryOnFileBusy (LBS.readFile (unsafeToFilePath path))
            pure $ do
                auth <- openaiAuthStateFromJson now bytes
                pure $ subscriptionAccount
                    OpenAIProvider
                    auth.accountId
                    (fromMaybe "ChatGPT" (openAIAccountEmail auth))
                    (toText path)
                    auth.accessToken
                    ManagedOpenAIAuthJson
                    (Text.decodeUtf8 (LBS.toStrict bytes))

discoverGrokEnv :: IO (Maybe LoginAccount)
discoverGrokEnv = do
    rawJson <- lookupNonEmpty "GROK_AUTH_JSON"
    rawToken <- lookupNonEmpty "GROK_ACCESS_TOKEN"
    pure $ case rawJson >>= grokCredentialFromAuthJson of
        Just token ->
            Just (grokAccount token "environment"
                ManagedGrokAuthJson (fromMaybe "" rawJson))
        Nothing ->
            (\token -> grokAccount token "environment"
                ManagedBearerToken token) <$> rawToken

discoverGrokFile :: OsPath -> IO (Maybe LoginAccount)
discoverGrokFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            raw <- Text.decodeUtf8 . LBS.toStrict
                <$> retryOnFileBusy (LBS.readFile (unsafeToFilePath path))
            pure $ do
                token <- grokCredentialFromAuthJson raw
                pure (grokAccount token (toText path)
                    ManagedGrokAuthJson raw)

discoverOpenRouter :: IO (Maybe LoginAccount)
discoverOpenRouter = do
    token <- lookupNonEmpty "OPENROUTER_API_KEY"
    pure $ do
        accessToken <- token
        pure LoginAccount
            { loginManagedId = Nothing
            , loginProvider = OpenRouterProvider
            , loginAccountId = "openrouter"
            , loginLabel = "OpenRouter"
            , loginBilling = ApiCreditsBilling
            , loginSource = "environment"
            , loginUsage = UsageNotChecked
            , loginAccessToken = accessToken
            , loginAuthKind = ManagedBearerToken
            , loginSecretPayload = accessToken
            , loginEnabled = True
            }

subscriptionAccount
    :: Provider
    -> Text
    -> Text
    -> Text
    -> Text
    -> ManagedAuthKind
    -> Text
    -> LoginAccount
subscriptionAccount provider accountId label source token authKind payload =
    LoginAccount
        { loginManagedId = Nothing
        , loginProvider = provider
        , loginAccountId = accountId
        , loginLabel = label
        , loginBilling = SubscriptionBilling Nothing
        , loginSource = source
        , loginUsage = UsageNotChecked
        , loginAccessToken = token
        , loginAuthKind = authKind
        , loginSecretPayload = payload
        , loginEnabled = True
        }

grokAccount :: Text -> Text -> ManagedAuthKind -> Text -> LoginAccount
grokAccount token source authKind payload =
    subscriptionAccount
        XAIProvider
        (fromMaybe "grok" (XAIAuth.accountIdFromAccessToken token))
        (fromMaybe "Grok"
            (grokEmailFromAuthJson payload
                <|> XAIAuth.emailFromToken token))
        source
        token
        authKind
        payload

openAIAccountEmail :: OpenAI.AuthState -> Maybe Text
openAIAccountEmail auth =
    (auth.idToken >>= OpenAI.deriveEmail)
        <|> OpenAI.deriveEmail auth.accessToken

prepareGrokLoginAccount :: LoginAccount -> IO (Either ApiError LoginAccount)
prepareGrokLoginAccount account
    | account.loginAuthKind /= ManagedGrokAuthJson =
        pure (Right account)
    | Text.null account.loginSecretPayload =
        pure (Right account)
    | otherwise =
        refreshGrokLoginPayload
            account.loginManagedId
            grokFilePath
            account.loginSecretPayload
            >>= \case
                Left err -> pure (Left err)
                Right (state, payload) ->
                    pure $ Right account
                        { loginAccessToken = state.grokAccessToken
                        , loginSecretPayload = payload
                        , loginAccountId =
                            fromMaybe account.loginAccountId
                                (XAIAuth.accountIdFromAccessToken
                                    state.grokAccessToken)
                        }
  where
    grokFilePath
        | isJust account.loginManagedId = Nothing
        | account.loginSource == "environment" = Nothing
        | otherwise =
            Just (unsafeEncodeUtf (Text.unpack account.loginSource))

refreshLoginAccount :: LoginAccount -> IO LoginAccount
refreshLoginAccount account
    | Text.null account.loginAccessToken = pure case account.loginUsage of
        UsageNotChecked ->
            account
                { loginUsage =
                    UsageUnavailable "access token is unavailable"
                }
        _ -> account
    | otherwise = case account.loginProvider of
        OpenAIProvider ->
            if account.loginAccountId == "openai-env"
                then pure account
                    { loginUsage =
                        UsageUnavailable
                            "ChatGPT account id is unavailable"
                    }
                else OpenAI.fetchUsage
                    account.loginAccessToken account.loginAccountId >>= \case
                        Left err -> do
                            now <- getCurrentTime
                            pure account
                                { loginUsage =
                                    UsageUnavailable
                                        (formatApiErrorInlineAt now err)
                                }
                        Right snapshot ->
                            pure account
                                { loginBilling =
                                    SubscriptionBilling
                                        (Just snapshot.planType)
                                , loginUsage =
                                    UsageAvailable
                                        (openAIUsage snapshot)
                                }
        XAIProvider ->
            prepareGrokLoginAccount account >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure account
                        { loginUsage =
                            UsageUnavailable (formatApiErrorInlineAt now err)
                        }
                Right prepared ->
                    XAI.fetchGrokUsage Credential
                        { accessToken = prepared.loginAccessToken
                        , accountId = prepared.loginAccountId
                        , leaseId = Nothing
                        , provider = XAIProvider
                        } >>= \case
                        Left err ->
                            pure prepared
                                { loginUsage = UsageUnavailable err }
                        Right snapshot ->
                            pure prepared
                                { loginUsage =
                                    UsageAvailable AccountUsage
                                        { usagePlan = Nothing
                                        , usageWindows =
                                            [ UsageWindow
                                                { windowName = "current period"
                                                , usedPercent = snapshot.usedPercent
                                                , windowSeconds =
                                                    snapshot.windowSeconds
                                                , resetsAt = snapshot.resetsAt
                                                }
                                            ]
                                        , creditsRemaining = Nothing
                                        , creditsUsed = Nothing
                                        }
                                }
        OpenRouterProvider ->
            OpenRouter.fetchOpenRouterUsage account.loginAccessToken >>= \case
                Left err ->
                    pure account
                        { loginUsage = UsageUnavailable err }
                Right snapshot ->
                    pure account
                        { loginLabel =
                            fromMaybe account.loginLabel snapshot.keyLabel
                        , loginUsage =
                            UsageAvailable AccountUsage
                                { usagePlan =
                                    if snapshot.isFreeTier == Just True
                                        then Just "free tier"
                                        else Nothing
                                , usageWindows = []
                                , creditsRemaining =
                                    formatAmount
                                        (snapshot.keyLimitRemaining
                                            <|> ((-) <$> snapshot.totalCredits
                                                <*> snapshot.totalUsage))
                                , creditsUsed =
                                    formatAmount
                                        (snapshot.totalUsage
                                            <|> snapshot.keyUsage)
                                }
                        }
        ClaudeCodeProvider ->
            pure account
                { loginUsage =
                    UsageUnavailable
                        "Use `claude auth status` for Claude Code subscription auth."
                }
  where
    formatAmount = fmap (("$" <>) . Text.pack . show)

openAIUsage :: OpenAI.UsageSnapshot -> AccountUsage
openAIUsage snapshot = AccountUsage
    { usagePlan = Just snapshot.planType
    , usageWindows =
        catMaybes
            [ toWindow "primary" <$> (snapshot.rateLimit >>= (.primaryWindow))
            , toWindow "secondary" <$> (snapshot.rateLimit >>= (.secondaryWindow))
            ]
    , creditsRemaining = Nothing
    , creditsUsed = Nothing
    }
  where
    toWindow name window = UsageWindow
        { windowName = name
        , usedPercent = window.usedPercent
        , windowSeconds = window.limitWindowSeconds
        , resetsAt =
            posixSecondsToUTCTime (fromIntegral window.resetAt)
        }

formatLoginAccounts :: Bool -> [LoginAccount] -> Text
formatLoginAccounts color accounts =
    renderLoginFrame color (initialLoginState accounts)

renderLoginFrame :: Bool -> LoginState -> Text
renderLoginFrame color state =
    Text.intercalate "\n" $
        [ rolePrompt color "credentials"
            <> roleMuted color
                (" · " <> Text.pack (show (length state.loginAccounts))
                    <> " connected")
        ]
            <> body
            <> [ roleMuted color
                    "↑↓/jk or scroll · click/enter refresh · a add · i import · e enable/disable · d disconnect · esc/q"
               ]
  where
    body
        | null state.loginAccounts =
            [ roleWarn color "No credentials found."
            , roleMuted color
                "Set provider credentials or import an existing Codex/Grok login."
            ]
        | otherwise =
            concat $
                zipWith
                    (renderAccount color state.loginIndex)
                    [0 ..]
                    state.loginAccounts

renderAccount :: Bool -> Int -> Int -> LoginAccount -> [Text]
renderAccount color selected index account =
    [ cursor
        <> health
        <> provider
        <> " · "
        <> label
        <> " · "
        <> billing
        <> enabledSuffix
    , "    "
        <> roleMuted color
            (accountId <> " · " <> account.loginSource)
    ]
        <> usageLines
  where
    cursor = if selected == index then roleWarn color "› " else "  "
    health
        | not account.loginEnabled = roleWarn color "○ "
        | otherwise = case account.loginUsage of
            UsageNotChecked -> roleMuted color "◌ "
            UsageUnavailable _ -> roleError color glyphErr
            UsageAvailable _ -> roleSuccess color glyphOk
    provider = rolePrompt color (providerSlug account.loginProvider)
    label = roleMuted color account.loginLabel
    billing = roleMuted color case account.loginBilling of
        SubscriptionBilling plan ->
            "subscription" <> maybe "" (" · " <>) plan
        ApiCreditsBilling -> "API credits"
    enabledSuffix
        | account.loginEnabled = ""
        | otherwise = roleWarn color " · disabled"
    accountId
        | Text.null account.loginAccountId = "(unknown account)"
        | otherwise = Text.take 24 account.loginAccountId
    usageLines = case account.loginUsage of
        UsageNotChecked ->
            ["    " <> roleMuted color "checking usage…"]
        UsageUnavailable err ->
            ["    " <> roleError color ("usage unavailable · " <> Text.take 100 err)]
        UsageAvailable usage ->
            map ("    " <>) $
                map (roleMuted color . formatWindow) usage.usageWindows
                    <> creditLines usage

formatWindow :: UsageWindow -> Text
formatWindow window =
    window.windowName
        <> " "
        <> Text.pack (show window.usedPercent)
        <> "% used · resets "
        <> Text.pack (show window.resetsAt)

creditLines :: AccountUsage -> [Text]
creditLines usage =
    catMaybes
        [ ("credits remaining " <>) <$> usage.creditsRemaining
        , ("used " <>) <$> usage.creditsUsed
        ]
