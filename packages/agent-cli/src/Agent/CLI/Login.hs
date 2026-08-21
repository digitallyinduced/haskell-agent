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
    , discoverLoginAccounts
    , formatLoginAccounts
    , initialLoginState
    , renderLoginFrame
    , runLoginManager
    ) where

import Agent.CLI.Auth
    ( grokCredentialFromAuthJson
    , openaiAuthStateFromJson
    )
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedBilling(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , deleteManagedCredential
    , loadManagedCredentials
    , newManagedCredentialId
    , setManagedCredentialEnabled
    , upsertManagedCredential
    )
import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Style
    ( glyphErr
    , glyphOk
    , roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.OsPath (OsPath, fromFilePath, toFilePath, toText)
import qualified Agent.OpenAI.Usage as OpenAI
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.Provider (Provider(..), providerSlug)
import qualified Agent.XAI.Auth as XAIAuth
import qualified Agent.XAI.Usage as XAI
import Control.Applicative ((<|>))
import Control.Exception.Safe (bracket)
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.OsPath ((</>))
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
            Text.hPutStrLn stderr (formatLoginAccounts color accounts)
            hFlush stderr
        else loop (initialLoginState accounts)
  where
    loop state =
        runOverlay (renderLoginFrame color) applyLoginKey state >>= \case
            Nothing -> pure ()
            Just LoginClose -> pure ()
            Just (LoginRefresh index) -> do
                accounts <- refreshAt index state.loginAccounts
                loop state
                    { loginAccounts = accounts
                    , loginIndex = index
                    }
            Just LoginAdd -> do
                connectAccount color
                discoverLoginAccounts >>= loop . initialLoginState
            Just (LoginToggle index) -> do
                toggleAt color index state.loginAccounts
                discoverLoginAccounts >>= loop . initialLoginState
            Just (LoginDelete index) -> do
                deleteAt color index state.loginAccounts
                discoverLoginAccounts >>= loop . initialLoginState
            Just (LoginImport index) -> do
                importAt color index state.loginAccounts
                discoverLoginAccounts >>= loop . initialLoginState

refreshAt :: Int -> [LoginAccount] -> IO [LoginAccount]
refreshAt index accounts =
    case splitAt index accounts of
        (before, account : after) -> do
            refreshed <- refreshLoginAccount account
            pure (before <> (refreshed : after))
        _ -> pure accounts

discoverLoginAccounts :: IO [LoginAccount]
discoverLoginAccounts = do
    home <- getHomeDirectory
    now <- getCurrentTime
    openaiEnv <- discoverOpenAIEnv
    openaiFile <- discoverOpenAIFile now
        (home </> fromFilePath ".codex" </> fromFilePath "auth.json")
    grokEnv <- discoverGrokEnv
    grokFile <- discoverGrokFile
        (home </> fromFilePath ".grok" </> fromFilePath "auth.json")
    openRouter <- discoverOpenRouter
    broker <- discoverBroker
    managed <- loadManagedCredentials
    let managedAccounts = case managed of
            Left _ -> []
            Right entries -> map (managedLoginAccount now) entries
    pure $ nubBy sameAccount $
        managedAccounts
            <> catMaybes
                [ broker
                , openaiEnv
                , openaiFile
                , grokEnv
                , grokFile
                , openRouter
                ]
  where
    sameAccount left right =
        left.loginProvider == right.loginProvider
            && left.loginAccountId == right.loginAccountId

managedLoginAccount
    :: UTCTime
    -> (ManagedCredential, ManagedSecret)
    -> LoginAccount
managedLoginAccount now (metadata, secret) =
    LoginAccount
        { loginManagedId = Just metadata.managedId
        , loginProvider = metadata.managedProvider
        , loginAccountId = metadata.managedAccountId
        , loginLabel = metadata.managedLabel
        , loginBilling = case metadata.managedBilling of
            ManagedSubscription -> SubscriptionBilling Nothing
            ManagedApiCredits -> ApiCreditsBilling
        , loginSource = "managed"
        , loginUsage = UsageNotChecked
        , loginAccessToken = accessToken
        , loginAuthKind = metadata.managedAuthKind
        , loginSecretPayload = secret.secretPayload
        , loginEnabled = metadata.managedEnabled
        }
  where
    accessToken = fromMaybe "" case metadata.managedAuthKind of
        ManagedBearerToken -> Just secret.secretPayload
        ManagedOpenAIAuthJson ->
            (.accessToken) <$> openaiAuthStateFromJson now
                (LBS.fromStrict (Text.encodeUtf8 secret.secretPayload))
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
                                SubscriptionBilling _ -> ManagedSubscription
                                ApiCreditsBilling -> ManagedApiCredits
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

connectAccount :: Bool -> IO ()
connectAccount color =
    pickConnectProvider color >>= \case
        Nothing -> pure ()
        Just OpenAIProvider -> connectOpenAI color
        Just XAIProvider -> connectXAI color
        Just OpenRouterProvider -> connectOpenRouter color

pickConnectProvider :: Bool -> IO (Maybe Provider)
pickConnectProvider color =
    join <$> runOverlay render step (0 :: Int)
  where
    providers = [OpenAIProvider, XAIProvider, OpenRouterProvider]
    render index =
        Text.intercalate "\n" $
            [rolePrompt color "connect account"]
                <> zipWith
                    (\i provider ->
                        (if i == index then roleWarn color "› " else "  ")
                            <> roleMuted color (providerSlug provider))
                    [0 ..]
                    providers
                <> [roleMuted color "↑↓/jk · enter · esc/q"]
    step key index = case key of
        PickerKeyCancel -> Left Nothing
        PickerKeyConfirm -> Left (accountAt index providers)
        PickerKeyUp ->
            Right ((index - 1) `mod` length providers)
        PickerKeyDown ->
            Right ((index + 1) `mod` length providers)
        _ -> Right index

connectOpenAI :: Bool -> IO ()
connectOpenAI color =
    lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID" >>= \case
        Nothing ->
            printLoginMessage color False
                "set OPENAI_OAUTH_CLIENT_ID before connecting a ChatGPT account"
        Just clientId -> do
            let options = OpenAILogin.defaultLoginOptions clientId
            OpenAILogin.requestDeviceCode options >>= \case
                Left err -> printLoginMessage color False err
                Right device -> do
                    Text.hPutStrLn stderr $
                        roleMuted color "Open "
                            <> rolePrompt color (Text.pack device.verificationUrl)
                    Text.hPutStrLn stderr $
                        roleMuted color "Enter code "
                            <> rolePrompt color device.userCode
                    hFlush stderr
                    OpenAILogin.completeDeviceCodeLogin options device >>= \case
                        Left err -> printLoginMessage color False err
                        Right authJson -> do
                            now <- getCurrentTime
                            case openaiAuthStateFromJson now (Aeson.encode authJson) of
                                Nothing ->
                                    printLoginMessage color False
                                        "OpenAI login returned invalid account data"
                                Just auth ->
                                    storeConnectedCredential color
                                        OpenAIProvider
                                        auth.accountId
                                        "ChatGPT"
                                        ManagedSubscription
                                        ManagedOpenAIAuthJson
                                        (Text.decodeUtf8
                                            (LBS.toStrict (Aeson.encode authJson)))

connectXAI :: Bool -> IO ()
connectXAI color =
    lookupNonEmpty "XAI_OAUTH_CLIENT_ID" >>= \case
        Nothing ->
            printLoginMessage color False
                "set XAI_OAUTH_CLIENT_ID before connecting a Grok account"
        Just clientId -> do
            let options = XAIAuth.defaultOAuthOptions clientId
            XAIAuth.requestDeviceAuthorization options >>= \case
                Left err -> printLoginMessage color False err
                Right device -> do
                    Text.hPutStrLn stderr $
                        roleMuted color "Open "
                            <> rolePrompt color device.verificationUrl
                    Text.hPutStrLn stderr $
                        roleMuted color "Enter code "
                            <> rolePrompt color device.userCode
                    hFlush stderr
                    XAIAuth.completeDeviceAuthorization options device >>= \case
                        Left err -> printLoginMessage color False err
                        Right tokens -> do
                            let accountId =
                                    fromMaybe "grok"
                                        (XAIAuth.accountIdFromAccessToken
                                            tokens.accessToken)
                                authJson = Aeson.object
                                    [ "access_token" .= tokens.accessToken
                                    , "refresh_token" .= tokens.refreshToken
                                    , "id_token" .= tokens.idToken
                                    ]
                            storeConnectedCredential color
                                XAIProvider
                                accountId
                                "Grok"
                                ManagedSubscription
                                ManagedGrokAuthJson
                                (Text.decodeUtf8
                                    (LBS.toStrict (Aeson.encode authJson)))

connectOpenRouter :: Bool -> IO ()
connectOpenRouter color =
    readSecretLine "OpenRouter API key: " >>= \case
        Nothing -> pure ()
        Just apiKey ->
            OpenRouter.fetchOpenRouterUsage apiKey >>= \case
                Left err ->
                    printLoginMessage color False
                        ("OpenRouter rejected the key: " <> err)
                Right usage -> do
                    let accountId =
                            fromMaybe "openrouter" usage.keyLabel
                        label =
                            fromMaybe "OpenRouter" usage.keyLabel
                    storeConnectedCredential color
                        OpenRouterProvider
                        accountId
                        label
                        ManagedApiCredits
                        ManagedBearerToken
                        apiKey

storeConnectedCredential
    :: Bool
    -> Provider
    -> Text
    -> Text
    -> ManagedBilling
    -> ManagedAuthKind
    -> Text
    -> IO ()
storeConnectedCredential color provider accountId label billing authKind payload = do
    credentialId <- newManagedCredentialId provider accountId
    upsertManagedCredential
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
        >>= printStoreResult color
            "credential connected; restart or reload auth to use it"

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
        pure $ subscriptionAccount
            OpenAIProvider accountId "ChatGPT" "environment"
            accessToken ManagedBearerToken accessToken

discoverOpenAIFile :: UTCTime -> OsPath -> IO (Maybe LoginAccount)
discoverOpenAIFile now path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            bytes <- LBS.readFile (toFilePath path)
            pure $ do
                auth <- openaiAuthStateFromJson now bytes
                pure $ subscriptionAccount
                    OpenAIProvider
                    auth.accountId
                    "ChatGPT"
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
                <$> LBS.readFile (toFilePath path)
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

discoverBroker :: IO (Maybe LoginAccount)
discoverBroker = do
    url <- lookupNonEmpty "AGENT_BROKER_URL"
    token <- lookupNonEmpty "AGENT_BROKER_TOKEN"
    pure case (url, token) of
        (Just brokerUrl, Just _) ->
            Just LoginAccount
                { loginManagedId = Nothing
                , loginProvider = OpenAIProvider
                , loginAccountId = "broker"
                , loginLabel = "Credential broker"
                , loginBilling = SubscriptionBilling Nothing
                , loginSource = brokerUrl
                , loginUsage =
                    UsageUnavailable
                        "account listing is not exposed by the broker client yet"
                , loginAccessToken = ""
                , loginAuthKind = ManagedBearerToken
                , loginSecretPayload = ""
                , loginEnabled = True
                }
        _ -> Nothing

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
        "Grok"
        source
        token
        authKind
        payload

refreshLoginAccount :: LoginAccount -> IO LoginAccount
refreshLoginAccount account
    | Text.null account.loginAccessToken = pure account
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
                        Left err ->
                            pure account
                                { loginUsage =
                                    UsageUnavailable (Text.pack (show err))
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
            XAI.fetchGrokUsage account.loginAccessToken >>= \case
                Left err ->
                    pure account { loginUsage = UsageUnavailable err }
                Right snapshot ->
                    pure account
                        { loginUsage =
                            UsageAvailable AccountUsage
                                { usagePlan = Nothing
                                , usageWindows =
                                    [ UsageWindow
                                        { windowName = "current period"
                                        , usedPercent = snapshot.usedPercent
                                        , windowSeconds = snapshot.windowSeconds
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
                    pure account { loginUsage = UsageUnavailable err }
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
                    "↑↓/jk · r refresh · a add · i import · e enable/disable · d disconnect · esc/q"
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
            UsageUnavailable _ -> roleError color glyphErr
            _ -> roleSuccess color glyphOk
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
            ["    " <> roleMuted color "usage not checked"]
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

lookupNonEmpty :: String -> IO (Maybe Text)
lookupNonEmpty name = do
    value <- lookupEnv name
    pure case value of
        Just text | not (null text) -> Just (Text.pack text)
        _ -> Nothing
