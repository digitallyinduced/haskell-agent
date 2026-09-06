module Agent.CLI.Login.Internal.Accounts
    ( discoverLoginAccounts
    , discoverSelectableLoginAccounts
    , disconnectLoginAccount
    , formatCurrencyAmount
    , importLoginAccount
    , isGatewayLoginAccount
    , loginAccountSelectionId
    , openAIAccountEmail
    , refreshLoginAccount
    , toggleLoginAccount
    ) where

import Agent.CLI.Auth
    ( GrokAuthState(..)
    , externalAuthSelectionId
    , grokCredentialFromAuthJson
    , grokEmailFromAuthJson
    , managedAuthSelectionId
    , openaiAuthStateFromJson
    )
import Agent.CLI.Auth.Gemini (geminiAuthStateFromJson)
import Agent.CLI.Auth.Grok (refreshGrokLoginPayload)
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
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.Error (formatApiErrorInlineAt)
import Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , loadGatewayCredential
    , removeGatewayCredential
    )
import Agent.CLI.Login.Types
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    )
import Agent.Error (ApiError)
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.Gemini.Auth as GeminiAuth
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Usage as OpenAI
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    )
import qualified Agent.XAI.Auth as XAIAuth
import qualified Agent.XAI.Usage as XAI
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as LBS
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Scientific (Scientific, FPFormat(Fixed), formatScientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

isGatewayLoginAccount :: LoginAccount -> Bool
isGatewayLoginAccount account =
    account.loginSource == "gateway"

discoverLoginAccounts :: IO [LoginAccount]
discoverLoginAccounts = do
    accounts <- discoverLoginAccountSources
    pure (nubOrdOn loginAccountKey accounts)
  where
    loginAccountKey account =
        ( account.loginProvider
        , case (account.loginProvider, account.loginManagedId) of
            (GeminiProvider, Just managedId) ->
                managedAuthSelectionId managedId
            _ -> account.loginAccountId
        )

-- | Accounts that can be selected in a live session. Unlike the login
-- dashboard, disabled managed entries do not shadow usable external sources,
-- and distinct managed credentials remain separately addressable.
discoverSelectableLoginAccounts :: IO [LoginAccount]
discoverSelectableLoginAccounts = do
    accounts <-
        filter
            (\account ->
                account.loginEnabled
                    && not (isGatewayLoginAccount account))
            <$> discoverLoginAccountSources
    pure (nubOrdOn loginAccountSelectionId accounts)

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
    gemini <- discoverGemini
    managed <- loadManagedCredentials
    gateway <- loadGatewayLoginAccount
    let managedAccounts = case managed of
            Left _ -> []
            Right entries -> map (managedLoginAccount now) entries
    pure $
        maybe managedAccounts (:managedAccounts) gateway
            <> catMaybes
                [ openaiEnv
                , openaiFile
                , grokEnv
                , grokFile
                , openRouter
                , gemini
                ]

loadGatewayLoginAccount :: IO (Maybe LoginAccount)
loadGatewayLoginAccount =
    loadGatewayCredential >>= \case
        Right (Just credential) ->
            pure $ Just LoginAccount
                { loginManagedId = Nothing
                , loginProvider = OpenAIProvider
                , loginAccountId = credential.gatewayBaseUrl
                , loginLabel = "Gateway · " <> credential.gatewayBaseUrl
                , loginBilling = SubscriptionBilling Nothing
                , loginSource = "gateway"
                , loginUsage = UsageNotChecked
                , loginAccessToken = ""
                , loginAuthKind = ManagedOpenAIAuthJson
                , loginSecretPayload = ""
                , loginEnabled = True
                }
        Left err ->
            pure $ Just LoginAccount
                { loginManagedId = Nothing
                , loginProvider = OpenAIProvider
                , loginAccountId = "(invalid saved credential)"
                , loginLabel = "Gateway connection needs repair"
                , loginBilling = SubscriptionBilling Nothing
                , loginSource = "gateway"
                , loginUsage = UsageUnavailable err
                , loginAccessToken = ""
                , loginAuthKind = ManagedOpenAIAuthJson
                , loginSecretPayload = ""
                , loginEnabled = True
                }
        Right Nothing -> pure Nothing

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
            ManagedGeminiAuthJson -> Nothing
        OpenRouterProvider -> Nothing
        GeminiProvider -> case metadata.managedAuthKind of
            ManagedGeminiAuthJson ->
                (.email) <$> geminiAuth
            _ -> Nothing
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
        ManagedGeminiAuthJson ->
            (.accessToken) <$> geminiAuth
    geminiAuth =
        geminiAuthStateFromJson secret.secretPayload

toggleLoginAccount :: LoginAccount -> IO (Either Text Text)
toggleLoginAccount account = case account.loginManagedId of
    Nothing ->
        pure (Left
            "external credentials are read-only; import them before changing state")
    Just credentialId ->
        fmap (fmap (const successMessage))
            (setManagedCredentialEnabled
                credentialId
                (not account.loginEnabled))
  where
    successMessage
        | account.loginEnabled = "Credential disabled."
        | otherwise = "Credential enabled."

disconnectLoginAccount :: LoginAccount -> IO (Either Text Text)
disconnectLoginAccount account = case account.loginManagedId of
    Nothing
        | isGatewayLoginAccount account ->
            fmap
                (fmap
                    (const
                        "Gateway disconnected. Restart the agent to apply the route change immediately."))
                removeGatewayCredential
    Nothing ->
        pure (Left
            "external credentials are read-only; remove them from their source")
    Just credentialId ->
        fmap (fmap (const "Credential disconnected."))
            (deleteManagedCredential credentialId)

importLoginAccount :: LoginAccount -> IO (Either Text Text)
importLoginAccount account = case account.loginManagedId of
    Just _ ->
        pure (Left "credential is already managed")
    Nothing
        | Text.null account.loginSecretPayload ->
            pure (Left "this credential source cannot be imported")
        | otherwise -> do
            credentialId <-
                newManagedCredentialId
                    account.loginProvider account.loginAccountId
            fmap (fmap (const "Credential imported into the managed store.")) $
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

discoverGemini :: IO (Maybe LoginAccount)
discoverGemini = do
    googleKey <- lookupNonEmpty "GOOGLE_API_KEY"
    geminiKey <- lookupNonEmpty "GEMINI_API_KEY"
    pure $ do
        accessToken <- googleKey <|> geminiKey
        pure LoginAccount
            { loginManagedId = Nothing
            , loginProvider = GeminiProvider
            , loginAccountId = "gemini"
            , loginLabel = "Google Gemini"
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
    | isGatewayLoginAccount account = pure account
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
        GeminiProvider ->
            pure account
                { loginUsage =
                    UsageUnavailable
                        (case account.loginAuthKind of
                            ManagedGeminiAuthJson ->
                                "Gemini Code Assist usage is unavailable."
                            _ ->
                                "Google AI Studio does not expose account usage for API keys.")
                }
        ClaudeCodeProvider ->
            pure account
                { loginUsage =
                    UsageUnavailable
                        "Use `claude auth status` for Claude Code subscription auth."
                }
  where
    formatAmount = fmap formatCurrencyAmount

formatCurrencyAmount :: Scientific -> Text
formatCurrencyAmount =
    ("$" <>) . Text.pack . formatScientific Fixed (Just 2)

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
