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
    , formatCurrencyAmount
    , initialLoginState
    , loginAccountActionRows
    , loginAccountDetail
    , loginAccountSelectionId
    , loginDashboardRows
    , refreshLoginAccount
    , renderLoginFrame
    , runFullscreenLoginManager
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
import Agent.CLI.Login.Types
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , LoginAction(..)
    , LoginState(..)
    , UsageState(..)
    , UsageWindow(..)
    , applyLoginKey
    , initialLoginState
    )
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
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , requestFullscreenChoiceWithBody
    , requestFullscreenSecret
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
import Agent.TUI.Model
    ( UiEvent(UiSetNotice)
    , progressNotice
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
import Control.Exception.Safe (bracket, bracket_, tryAny)
import Control.Monad (join, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Scientific (Scientific, FPFormat(Fixed), formatScientific)
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

-- | Native fullscreen credential manager used by @/login@. Every prompt is a
-- Brick overlay, so the application stays in the alternate screen and
-- provider tokens never pass through the ordinary line editor.
runFullscreenLoginManager :: FullscreenRuntime -> IO ()
runFullscreenLoginManager runtime = do
    accounts <- discoverLoginAccounts
    dashboardLoop Nothing accounts
  where
    dashboardLoop notice accounts = do
        let entries = loginDashboardEntries accounts
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Provider accounts"
                (loginDashboardBody notice accounts)
                0
                (map snd entries)
        case choice >>= (`accountAt` entries) of
            Nothing -> pure ()
            Just (LoginDashboardConnect, _) -> do
                result <- connectFullscreenAccount runtime
                rediscovered <- discoverLoginAccounts
                dashboardLoop (result <|> notice) rediscovered
            Just (LoginDashboardRefreshAll, _) -> do
                refreshed <-
                    withLoginProgress runtime "Refreshing provider usage…" $
                        mapConcurrently refreshLoginAccountSafely accounts
                dashboardLoop
                    (Just (refreshAllNotice refreshed))
                    refreshed
            Just (LoginDashboardOpen index, _) ->
                accountLoop notice index accounts

    accountLoop notice index accounts =
        case accountAt index accounts of
            Nothing -> dashboardLoop notice accounts
            Just account -> do
                let entries = loginAccountMenuEntries account
                choice <-
                    requestFullscreenChoiceWithBody
                        runtime
                        (accountMenuTitle account)
                        (loginAccountBody notice account)
                        0
                        (map snd entries)
                case choice >>= (`accountAt` entries) of
                    Nothing ->
                        dashboardLoop notice accounts
                    Just (LoginAccountRefresh, _) -> do
                        refreshed <-
                            withLoginProgress runtime "Refreshing account usage…" $
                                refreshLoginAccountSafely account
                        accountLoop
                            (Just (refreshOneNotice refreshed))
                            index
                            (replaceAt index refreshed accounts)
                    Just (LoginAccountToggle, _) -> do
                        result <- toggleLoginAccount account
                        rediscoverAndLoop (Just (noticeFromResult result))
                    Just (LoginAccountImport, _) -> do
                        result <- importLoginAccount account
                        rediscoverAndLoop (Just (noticeFromResult result))
                    Just (LoginAccountDisconnect, _) -> do
                        confirmed <- confirmFullscreenDisconnect runtime account
                        if not confirmed
                            then accountLoop notice index accounts
                            else do
                                result <- disconnectLoginAccount account
                                rediscoverAndLoop
                                    (Just (noticeFromResult result))
                    Just (LoginAccountBack, _) ->
                        dashboardLoop notice accounts
      where
        rediscoverAndLoop nextNotice = do
            rediscovered <- discoverLoginAccounts
            dashboardLoop nextNotice rediscovered

data LoginNotice = LoginNotice !Bool !Text

data LoginDashboardAction
    = LoginDashboardConnect
    | LoginDashboardRefreshAll
    | LoginDashboardOpen !Int

data LoginAccountMenuAction
    = LoginAccountRefresh
    | LoginAccountToggle
    | LoginAccountImport
    | LoginAccountDisconnect
    | LoginAccountBack

loginDashboardRows :: [LoginAccount] -> [(Text, Text)]
loginDashboardRows = map snd . loginDashboardEntries

loginDashboardEntries
    :: [LoginAccount]
    -> [(LoginDashboardAction, (Text, Text))]
loginDashboardEntries accounts =
    [ ( LoginDashboardConnect
      , ("＋ Connect account", "OpenAI, xAI / Grok, OpenRouter, or Claude Code")
      )
    ]
        <> refreshEntry
        <> zipWith
            (\index account ->
                (LoginDashboardOpen index, loginDashboardAccountRow account))
            [0 ..]
            accounts
  where
    refreshEntry
        | null accounts = []
        | otherwise =
            [ ( LoginDashboardRefreshAll
              , ( "↻ Refresh all usage"
                , Text.pack (show (length accounts)) <> " accounts"
                )
              )
            ]

loginDashboardAccountRow :: LoginAccount -> (Text, Text)
loginDashboardAccountRow account =
    ( accountHealthGlyph account
        <> providerSlug account.loginProvider
        <> "  "
        <> displayText 48 account.loginLabel
    , Text.intercalate " · " $
        [ accountIdentifier account
        , if isJust account.loginManagedId then "managed" else "external"
        , if account.loginEnabled then "enabled" else "disabled"
        , billingSummary account.loginBilling
        , usageSummary account.loginUsage
        ]
    )

accountIdentifier :: LoginAccount -> Text
accountIdentifier account
    | Text.null (Text.strip account.loginAccountId) = "unknown account"
    | otherwise = displayText 48 account.loginAccountId

loginDashboardBody :: Maybe LoginNotice -> [LoginAccount] -> Text
loginDashboardBody notice accounts =
    Text.intercalate "\n\n" $
        [ "Manage credentials and inspect provider usage without leaving the fullscreen UI."
        , "**" <> Text.pack (show (length accounts)) <> " accounts**"
            <> " · " <> Text.pack (show managedCount) <> " managed"
            <> " · " <> Text.pack (show enabledCount) <> " enabled"
        ]
            <> maybe [] (pure . formatLoginNotice) notice
            <> [ if null accounts
                    then "No credentials were found. Connect an account to get started."
                    else "Select an account for usage details, importing, enable/disable, or disconnect."
               ]
  where
    managedCount = length (filter (isJust . (.loginManagedId)) accounts)
    enabledCount = length (filter (.loginEnabled) accounts)

loginAccountActionRows :: LoginAccount -> [(Text, Text)]
loginAccountActionRows = map snd . loginAccountMenuEntries

loginAccountMenuEntries
    :: LoginAccount
    -> [(LoginAccountMenuAction, (Text, Text))]
loginAccountMenuEntries account =
    [ (LoginAccountRefresh, ("↻ Refresh usage", "Fetch the latest provider limits"))
    ]
        <> managementEntries
        <> [(LoginAccountBack, ("← Back to accounts", "Return to the dashboard"))]
  where
    managementEntries = case account.loginManagedId of
        Just _ ->
            [ ( LoginAccountToggle
              , ( if account.loginEnabled
                    then "○ Disable credential"
                    else "● Enable credential"
                , if account.loginEnabled
                    then "Keep it stored but exclude it from selection"
                    else "Make it available for provider selection"
                )
              )
            , ( LoginAccountDisconnect
              , ("− Disconnect credential", "Delete it from the managed store")
              )
            ]
        Nothing
            | Text.null account.loginSecretPayload -> []
            | otherwise ->
                [ ( LoginAccountImport
                  , ("⇥ Import credential", "Copy it into the managed store")
                  )
                ]

accountMenuTitle :: LoginAccount -> Text
accountMenuTitle account =
    providerSlug account.loginProvider
        <> " · "
        <> displayText 48 account.loginLabel

loginAccountBody :: Maybe LoginNotice -> LoginAccount -> Text
loginAccountBody notice account =
    Text.intercalate "\n\n" $
        maybe [] (pure . formatLoginNotice) notice
            <> [loginAccountDetail account]

loginAccountDetail :: LoginAccount -> Text
loginAccountDetail account =
    Text.intercalate "\n" $
        [ "**" <> markdownText 100 account.loginLabel <> "**"
        , ""
        , "Provider: " <> providerSlug account.loginProvider
        , "Account: " <> markdownText 120 accountId
        , "Source: " <> markdownText 160 account.loginSource
        , "Storage: " <>
            if isJust account.loginManagedId then "managed" else "external (read-only)"
        , "Status: " <>
            if account.loginEnabled then "enabled" else "disabled"
        , "Billing: " <> billingSummary account.loginBilling
        , ""
        , "**Usage**"
        ]
            <> usageDetail account.loginUsage
  where
    accountId
        | Text.null account.loginAccountId = "(unknown)"
        | otherwise = account.loginAccountId

usageDetail :: UsageState -> [Text]
usageDetail = \case
    UsageNotChecked ->
        ["Not checked yet. Choose **Refresh usage** to fetch current limits."]
    UsageUnavailable err ->
        ["⚠ " <> markdownText 500 ("Usage unavailable · " <> err)]
    UsageAvailable usage ->
        planLines <> windowLines <> creditDetail <> fallback
      where
        planLines = maybe [] (\plan -> ["Plan: " <> markdownText 80 plan])
            usage.usagePlan
        windowLines = concatMap formatUsageWindow usage.usageWindows
        creditDetail =
            catMaybes
                [ ("Credits remaining: " <>) . markdownText 80
                    <$> usage.creditsRemaining
                , ("Credits used: " <>) . markdownText 80
                    <$> usage.creditsUsed
                ]
        fallback
            | null planLines && null windowLines && null creditDetail =
                ["Usage is available; this provider reported no active limits."]
            | otherwise = []

formatUsageWindow :: UsageWindow -> [Text]
formatUsageWindow window =
    [ "- " <> markdownText 80 window.windowName
        <> "  `" <> usageBar window.usedPercent <> "`  "
        <> Text.pack (show window.usedPercent) <> "% used"
    , "  Resets " <> Text.pack (show window.resetsAt)
    ]

usageBar :: Int -> Text
usageBar usedPercent =
    Text.replicate filled "█" <> Text.replicate (10 - filled) "░"
  where
    clamped = max 0 (min 100 usedPercent)
    filled = (clamped + 5) `div` 10

accountHealthGlyph :: LoginAccount -> Text
accountHealthGlyph account
    | not account.loginEnabled = "○ "
    | otherwise = case account.loginUsage of
        UsageNotChecked -> "◌ "
        UsageUnavailable _ -> "✗ "
        UsageAvailable _ -> "✓ "

billingSummary :: AccountBilling -> Text
billingSummary = \case
    SubscriptionBilling plan ->
        "subscription" <> maybe "" (" / " <>) plan
    ApiCreditsBilling -> "API credits"

usageSummary :: UsageState -> Text
usageSummary = \case
    UsageNotChecked -> "usage not checked"
    UsageUnavailable _ -> "usage unavailable"
    UsageAvailable usage ->
        case usage.usageWindows of
            window : _ ->
                Text.pack (show window.usedPercent)
                    <> "% " <> window.windowName
            [] -> case usage.creditsRemaining of
                Just remaining -> remaining <> " remaining"
                Nothing -> maybe "usage available" id usage.usagePlan

displayText :: Int -> Text -> Text
displayText limit =
    Text.take limit
        . Text.unwords
        . Text.words
        . Text.map (\character -> if isControl character then ' ' else character)

markdownText :: Int -> Text -> Text
markdownText limit =
    Text.concatMap escape . displayText limit
  where
    escape character
        | character `elem` ("\\`*_[]<>" :: String) =
            "\\" <> Text.singleton character
        | otherwise = Text.singleton character

formatLoginNotice :: LoginNotice -> Text
formatLoginNotice (LoginNotice successful message) =
    (if successful then "✅ " else "⚠️ ")
        <> markdownText 500 message

noticeFromResult :: Either Text Text -> LoginNotice
noticeFromResult = \case
    Left err -> LoginNotice False err
    Right message -> LoginNotice True message

refreshOneNotice :: LoginAccount -> LoginNotice
refreshOneNotice account = case account.loginUsage of
    UsageUnavailable err -> LoginNotice False err
    _ -> LoginNotice True "Usage refreshed."

refreshAllNotice :: [LoginAccount] -> LoginNotice
refreshAllNotice accounts
    | unavailable == 0 =
        LoginNotice True
            ("Usage refreshed for " <> count <> " accounts.")
    | otherwise =
        LoginNotice False
            ("Usage refreshed; " <> Text.pack (show unavailable)
                <> " of " <> count <> " accounts were unavailable.")
  where
    count = Text.pack (show (length accounts))
    unavailable = length
        [()
        | account <- accounts
        , UsageUnavailable _ <- [account.loginUsage]
        ]

withLoginProgress :: FullscreenRuntime -> Text -> IO a -> IO a
withLoginProgress runtime message =
    bracket_
        (emitUiEvent runtime
            (UiSetNotice (Just (progressNotice message))))
        (emitUiEvent runtime (UiSetNotice Nothing))

refreshLoginAccountSafely :: LoginAccount -> IO LoginAccount
refreshLoginAccountSafely account =
    tryAny (refreshLoginAccount account) >>= \case
        Left err ->
            pure account
                { loginUsage =
                    UsageUnavailable
                        ("usage check failed: " <> formatException err)
                }
        Right refreshed -> pure refreshed

confirmFullscreenDisconnect
    :: FullscreenRuntime
    -> LoginAccount
    -> IO Bool
confirmFullscreenDisconnect runtime account = do
    choice <-
        requestFullscreenChoiceWithBody
            runtime
            "Disconnect credential?"
            ( "Delete **" <> markdownText 100 account.loginLabel
                <> "** from the managed credential store?\n\n"
                <> "This cannot be undone."
            )
            1
            [ ("Disconnect", "Delete the stored credential")
            , ("Keep credential", "Return without changing anything")
            ]
    pure (choice == Just 0)

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
            refreshed <- refreshLoginAccountSafely account
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
    pure (nubOrdOn loginAccountKey accounts)
  where
    loginAccountKey account =
        (account.loginProvider, account.loginAccountId)

-- | Accounts that can be selected in a live session. Unlike the login
-- dashboard, disabled managed entries do not shadow usable external sources,
-- and distinct managed credentials remain separately addressable.
discoverSelectableLoginAccounts :: IO [LoginAccount]
discoverSelectableLoginAccounts = do
    accounts <- filter (.loginEnabled) <$> discoverLoginAccountSources
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
        Just account ->
            toggleLoginAccount account >>= printLoginResult color

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

deleteAt :: Bool -> Int -> [LoginAccount] -> IO ()
deleteAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account -> case account.loginManagedId of
            Nothing ->
                disconnectLoginAccount account >>= printLoginResult color
            Just _ ->
                readApprovalLine
                    ("Disconnect " <> account.loginLabel <> "? [y/N] ")
                    >>= \case
                        Just answer
                            | Text.toLower (Text.strip answer) `elem` ["y", "yes"] ->
                                disconnectLoginAccount account
                                    >>= printLoginResult color
                        _ -> pure ()

disconnectLoginAccount :: LoginAccount -> IO (Either Text Text)
disconnectLoginAccount account = case account.loginManagedId of
    Nothing ->
        pure (Left
            "external credentials are read-only; remove them from their source")
    Just credentialId ->
        fmap (fmap (const "Credential disconnected."))
            (deleteManagedCredential credentialId)

importAt :: Bool -> Int -> [LoginAccount] -> IO ()
importAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account ->
            importLoginAccount account >>= printLoginResult color

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

connectFullscreenAccount :: FullscreenRuntime -> IO (Maybe LoginNotice)
connectFullscreenAccount runtime = do
    choice <-
        requestFullscreenChoiceWithBody
            runtime
            "Connect provider account"
            ( "Choose a provider. OAuth links, one-time codes, and API-key "
                <> "entry will remain inside the fullscreen UI."
            )
            0
            (map snd providers)
    case choice >>= (`accountAt` providers) of
        Nothing -> pure Nothing
        Just (provider, _) ->
            fmap noticeFromResult
                <$> connectFullscreenProvider runtime provider
  where
    providers =
        [ ( OpenAIProvider
          , ("OpenAI / ChatGPT", "Connect a ChatGPT subscription with OAuth")
          )
        , ( XAIProvider
          , ("xAI / Grok", "Connect a Grok subscription with OAuth")
          )
        , ( OpenRouterProvider
          , ("OpenRouter", "Add a masked API key and inspect credits")
          )
        , ( ClaudeCodeProvider
          , ("Claude Code", "Managed by the Claude Code CLI")
          )
        ]

connectFullscreenProvider
    :: FullscreenRuntime
    -> Provider
    -> IO (Maybe (Either Text Text))
connectFullscreenProvider runtime = \case
    OpenAIProvider -> connectOpenAIFullscreen runtime
    XAIProvider -> connectXAIFullscreen runtime
    OpenRouterProvider -> connectOpenRouterFullscreen runtime
    ClaudeCodeProvider ->
        pure $
            Just $
                Left
                    "Claude Code subscriptions are managed by `claude auth login`."

connectOpenAIFullscreen
    :: FullscreenRuntime
    -> IO (Maybe (Either Text Text))
connectOpenAIFullscreen runtime = do
    clientId <-
        openAIOAuthClientId <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
    let options = OpenAILogin.defaultLoginOptions clientId
    requested <-
        withLoginProgress runtime "Starting OpenAI device authorization…" $
            OpenAILogin.requestDeviceCode options
    case requested of
        Left err -> pure (Just (Left err))
        Right device -> awaitAuthorization options device False
  where
    awaitAuthorization options device pending = do
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Connect OpenAI / ChatGPT"
                (deviceAuthorizationBody
                    "OpenAI"
                    (Text.pack device.verificationUrl)
                    device.userCode
                    pending)
                0
                [ ( "Check authorization"
                  , "Return after approving the one-time code in your browser"
                  )
                , ("Cancel", "Stop without storing a credential")
                ]
        case choice of
            Just 0 -> do
                polled <-
                    withLoginProgress runtime "Checking OpenAI authorization…" $
                        OpenAILogin.pollDeviceCode options device
                case polled of
                    Left err -> pure (Just (Left err))
                    Right Nothing ->
                        awaitAuthorization options device True
                    Right (Just authJson) ->
                        finishOpenAIFullscreen authJson
            _ -> pure Nothing

    finishOpenAIFullscreen authJson = do
        now <- getCurrentTime
        case openaiAuthStateFromJson now (Aeson.encode authJson) of
            Nothing ->
                pure $
                    Just $
                        Left "OpenAI login returned invalid account data"
            Just auth ->
                Just <$> storeConnectedCredentialResult
                    OpenAIProvider
                    auth.accountId
                    (fromMaybe "ChatGPT" (openAIAccountEmail auth))
                    SubscriptionBilled
                    ManagedOpenAIAuthJson
                    (Text.decodeUtf8
                        (LBS.toStrict (Aeson.encode authJson)))

connectXAIFullscreen
    :: FullscreenRuntime
    -> IO (Maybe (Either Text Text))
connectXAIFullscreen runtime = do
    clientId <-
        xaiOAuthClientId <$> lookupNonEmpty "XAI_OAUTH_CLIENT_ID"
    let options = XAIAuth.defaultOAuthOptions clientId
    requested <-
        withLoginProgress runtime "Starting xAI device authorization…" $
            XAIAuth.requestDeviceAuthorization options
    case requested of
        Left err -> pure (Just (Left err))
        Right device -> awaitAuthorization options device False
  where
    awaitAuthorization options device pending = do
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Connect xAI / Grok"
                (deviceAuthorizationBody
                    "xAI"
                    device.verificationUrl
                    device.userCode
                    pending)
                0
                [ ( "Check authorization"
                  , "Return after approving the one-time code in your browser"
                  )
                , ("Cancel", "Stop without storing a credential")
                ]
        case choice of
            Just 0 -> do
                polled <-
                    withLoginProgress runtime "Checking xAI authorization…" $
                        XAIAuth.pollDeviceAuthorization options device
                case polled of
                    Left err -> pure (Just (Left err))
                    Right Nothing ->
                        awaitAuthorization options device True
                    Right (Just tokens) ->
                        finishXAIFullscreen tokens
            _ -> pure Nothing

    finishXAIFullscreen tokens
        | Nothing <- tokens.refreshToken =
            pure $
                Just $
                    Left
                        "Grok login did not return a refresh token; reconnect with offline access"
        | otherwise = do
            now <- getCurrentTime
            let accountId =
                    fromMaybe "grok"
                        (XAIAuth.accountIdFromAccessToken tokens.accessToken)
                label = fromMaybe "Grok" $
                    (tokens.idToken >>= XAIAuth.emailFromToken)
                        <|> XAIAuth.emailFromToken tokens.accessToken
                authJson = grokAuthStateToJson GrokAuthState
                    { grokAccessToken = tokens.accessToken
                    , grokRefreshToken = tokens.refreshToken
                    , grokIdToken = tokens.idToken
                    , grokExpiresAt =
                        ((`addUTCTime` now) . fromIntegral
                            <$> tokens.expiresInSeconds)
                            <|> OpenAI.parseJwtExp tokens.accessToken
                    }
            Just <$> storeConnectedCredentialResult
                XAIProvider
                accountId
                label
                SubscriptionBilled
                ManagedGrokAuthJson
                (Text.decodeUtf8
                    (LBS.toStrict (Aeson.encode authJson)))

connectOpenRouterFullscreen
    :: FullscreenRuntime
    -> IO (Maybe (Either Text Text))
connectOpenRouterFullscreen runtime =
    requestFullscreenSecret
        runtime
        "Connect OpenRouter"
        ( "Paste an OpenRouter API key. Input is masked and is never added "
            <> "to the conversation transcript."
        )
        >>= \case
            Nothing -> pure Nothing
            Just rawKey
                | Text.null apiKey -> pure Nothing
                | otherwise -> do
                    fetched <-
                        withLoginProgress runtime "Validating OpenRouter key…" $
                            OpenRouter.fetchOpenRouterUsage apiKey
                    case fetched of
                        Left err ->
                            pure $
                                Just $
                                    Left ("OpenRouter rejected the key: " <> err)
                        Right usage -> do
                            let accountId =
                                    fromMaybe "openrouter" usage.keyLabel
                                label =
                                    fromMaybe "OpenRouter" usage.keyLabel
                            Just <$> storeConnectedCredentialResult
                                OpenRouterProvider
                                accountId
                                label
                                ApiBilled
                                ManagedBearerToken
                                apiKey
              where
                apiKey = Text.strip rawKey

deviceAuthorizationBody
    :: Text
    -> Text
    -> Text
    -> Bool
    -> Text
deviceAuthorizationBody provider url userCode pending =
    Text.intercalate "\n\n" $
        [ "1. [Open the " <> provider <> " sign-in page](" <> url <> ")."
        , "2. Enter this one-time code:"
        , "`" <> markdownText 100 userCode <> "`"
        , "3. Return here and choose **Check authorization**."
        ]
            <> [ "Authorization is still pending. Finish the browser step, "
                    <> "then check again."
               | pending
               ]

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
    result <-
        storeConnectedCredentialResult
            provider
            accountId
            label
            billing
            authKind
            payload
    printLoginResult color result
    pure (either (const False) (const True) result)

storeConnectedCredentialResult
    :: Provider
    -> Text
    -> Text
    -> BillingMode
    -> ManagedAuthKind
    -> Text
    -> IO (Either Text Text)
storeConnectedCredentialResult provider accountId label billing authKind payload = do
    credentialId <- newManagedCredentialId provider accountId
    fmap (fmap (const "Credential connected.")) $
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

printLoginResult :: Bool -> Either Text Text -> IO ()
printLoginResult color = \case
    Left err -> printLoginMessage color False err
    Right success -> printLoginMessage color True success

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
