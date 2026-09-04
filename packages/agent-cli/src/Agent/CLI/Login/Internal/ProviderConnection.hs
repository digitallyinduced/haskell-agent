module Agent.CLI.Login.Internal.ProviderConnection
    ( connectAccount
    , connectFullscreenAccount
    , connectProviderAccount
    , printLoginMessage
    , printLoginResult
    , storeConnectedCredential
    ) where

import Agent.CLI.Auth
    ( GrokAuthState(..)
    , grokAuthStateToJson
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , xaiOAuthClientId
    )
import Agent.CLI.Auth.Gemini (geminiAuthStateToJson)
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , newManagedCredentialId
    , upsertManagedCredential
    )
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.Login.Internal.Accounts (openAIAccountEmail)
import Agent.CLI.Login.Internal.Browser (openBrowser)
import Agent.CLI.Login.Internal.Dashboard
    ( LoginNotice
    , noticeFromResult
    , withLoginProgress
    )
import Agent.CLI.Login.Internal.Device
    ( DevicePollReadiness(..)
    , advanceDevicePollSchedule
    , authorizationPendingNotice
    , authorizationSlowDownNotice
    , deviceAuthorizationBody
    , deviceAuthorizationDefaultTimeoutSeconds
    , devicePollReadiness
    , initialDevicePollSchedule
    , pollWaitNotice
    )
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
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoiceWithBody
    , requestFullscreenSecret
    )
import qualified Agent.Gemini.Auth as GeminiAuth
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Login as OpenAILogin
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.Provider (BillingMode(..), Provider(..), providerSlug)
import qualified Agent.XAI.Auth as XAIAuth
import Control.Applicative ((<|>))
import Control.Concurrent.Async (race)
import Control.Exception.Safe (bracket)
import Control.Monad (join, unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.IO
    ( hFlush
    , hGetEcho
    , hIsTerminalDevice
    , hSetEcho
    , stderr
    , stdin
    )

connectAccount :: Bool -> IO (Maybe (Provider, Text))
connectAccount color =
    pickConnectProvider color >>= \case
        Nothing -> pure Nothing
        Just provider ->
            fmap
                (fmap (\accountId -> (provider, accountId)))
                (connectProviderAccount color provider)

-- | Connect one account for the requested provider and return an identifier
-- that selects the newly stored credential.
connectProviderAccount :: Bool -> Provider -> IO (Maybe Text)
connectProviderAccount color = \case
    OpenAIProvider -> connectOpenAI color
    XAIProvider -> connectXAI color
    OpenRouterProvider -> connectOpenRouter color
    GeminiProvider -> connectGemini color
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
        , GeminiProvider
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
        , ( GeminiProvider
          , ("Google Gemini", "Connect a Google account with OAuth")
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
    GeminiProvider -> connectGeminiFullscreen runtime
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
    requestedAt <- getCurrentTime
    requested <-
        withLoginProgress runtime "Starting OpenAI device authorization…" $
            OpenAILogin.requestDeviceCode options
    case requested of
        Left err -> pure (Just (Left err))
        Right device ->
            awaitAuthorization
                options
                device
                (initialDevicePollSchedule
                    requestedAt
                    device.pollIntervalSeconds
                    deviceAuthorizationDefaultTimeoutSeconds)
                Nothing
  where
    awaitAuthorization options device schedule notice = do
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Connect OpenAI / ChatGPT"
                (deviceAuthorizationBody
                    "OpenAI"
                    (Text.pack device.verificationUrl)
                    device.userCode
                    notice)
                0
                [ ( "Check authorization"
                  , "Return after approving the one-time code in your browser"
                  )
                , ("Cancel", "Stop without storing a credential")
                ]
        case choice of
            Just 0 -> do
                now <- getCurrentTime
                case devicePollReadiness now schedule of
                    DevicePollExpired ->
                        openAITimedOut
                    DevicePollWait seconds ->
                        awaitAuthorization
                            options
                            device
                            schedule
                            (Just (pollWaitNotice seconds))
                    DevicePollReady -> do
                        polled <-
                            withLoginProgress runtime
                                "Checking OpenAI authorization…" $
                                    OpenAILogin.pollDeviceCode options device
                        polledAt <- getCurrentTime
                        case polled of
                            Left err -> pure (Just (Left err))
                            Right Nothing ->
                                if devicePollReadiness polledAt schedule
                                        == DevicePollExpired
                                    then openAITimedOut
                                    else
                                        awaitAuthorization
                                            options
                                            device
                                            (advanceDevicePollSchedule
                                                False polledAt schedule)
                                            (Just authorizationPendingNotice)
                            Right (Just authJson)
                                | devicePollReadiness polledAt schedule
                                    == DevicePollExpired ->
                                        openAITimedOut
                                | otherwise ->
                                    finishOpenAIFullscreen authJson
            _ -> pure Nothing

    openAITimedOut =
        pure $
            Just $
                Left "OpenAI device authorization timed out after 15 minutes"

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
    requestedAt <- getCurrentTime
    requested <-
        withLoginProgress runtime "Starting xAI device authorization…" $
            XAIAuth.requestDeviceAuthorization options
    case requested of
        Left err -> pure (Just (Left err))
        Right device ->
            awaitAuthorization
                options
                device
                (initialDevicePollSchedule
                    requestedAt
                    device.pollIntervalSeconds
                    (fromMaybe
                        deviceAuthorizationDefaultTimeoutSeconds
                        device.expiresInSeconds))
                Nothing
  where
    awaitAuthorization options device schedule notice = do
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Connect xAI / Grok"
                (deviceAuthorizationBody
                    "xAI"
                    device.verificationUrl
                    device.userCode
                    notice)
                0
                [ ( "Check authorization"
                  , "Return after approving the one-time code in your browser"
                  )
                , ("Cancel", "Stop without storing a credential")
                ]
        case choice of
            Just 0 -> do
                now <- getCurrentTime
                case devicePollReadiness now schedule of
                    DevicePollExpired ->
                        xaiTimedOut
                    DevicePollWait seconds ->
                        awaitAuthorization
                            options
                            device
                            schedule
                            (Just (pollWaitNotice seconds))
                    DevicePollReady -> do
                        polled <-
                            withLoginProgress runtime
                                "Checking xAI authorization…" $
                                    XAIAuth.pollDeviceAuthorizationStatus
                                        options device
                        polledAt <- getCurrentTime
                        case polled of
                            Left err -> pure (Just (Left err))
                            Right XAIAuth.DeviceAuthorizationPending ->
                                continuePending False polledAt
                                    authorizationPendingNotice
                            Right XAIAuth.DeviceAuthorizationSlowDown ->
                                let nextSchedule =
                                        advanceDevicePollSchedule
                                            True polledAt schedule
                                    seconds = case
                                        devicePollReadiness
                                            polledAt nextSchedule of
                                        DevicePollWait waitSeconds ->
                                            waitSeconds
                                        _ -> 0
                                in continueWithSchedule
                                    polledAt
                                    nextSchedule
                                    (authorizationSlowDownNotice seconds)
                            Right
                                (XAIAuth.DeviceAuthorizationComplete tokens)
                                    | devicePollReadiness polledAt schedule
                                        == DevicePollExpired ->
                                            xaiTimedOut
                                    | otherwise ->
                                        finishXAIFullscreen tokens
            _ -> pure Nothing
      where
        continuePending slowedDown polledAt nextNotice =
            continueWithSchedule
                polledAt
                (advanceDevicePollSchedule
                    slowedDown polledAt schedule)
                nextNotice

        continueWithSchedule polledAt nextSchedule nextNotice
            | devicePollReadiness polledAt schedule
                == DevicePollExpired =
                    xaiTimedOut
            | otherwise =
                awaitAuthorization
                    options
                    device
                    nextSchedule
                    (Just nextNotice)

    xaiTimedOut =
        pure $
            Just $
                Left "xAI device authorization timed out"

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

connectGeminiFullscreen
    :: FullscreenRuntime
    -> IO (Maybe (Either Text Text))
connectGeminiFullscreen runtime = do
    oauthOptions <- GeminiAuth.oauthOptionsFromEnv
    codeAssistOptions <- GeminiAuth.codeAssistOptionsFromEnv
    authenticated <-
        withLoginProgress runtime "Waiting for Google sign-in…" $
            GeminiAuth.authenticateGoogleAccount
                oauthOptions
                codeAssistOptions
                (presentGoogleLoginFullscreen runtime)
    case authenticated of
        Left err ->
            pure (Just (Left ("Google sign-in failed: " <> err)))
        Right auth
            | Nothing <- auth.refreshToken ->
                pure $
                    Just $
                        Left
                            "Google sign-in did not return a refresh token; disconnect access for Gemini CLI in your Google account and try again"
            | otherwise ->
                Just <$> storeConnectedCredentialResult
                    GeminiProvider
                    auth.email
                    auth.email
                    SubscriptionBilled
                    ManagedGeminiAuthJson
                    (geminiAuthStateToJson auth)

presentGoogleLoginFullscreen :: FullscreenRuntime -> Text -> IO ()
presentGoogleLoginFullscreen runtime url = do
    opened <- openBrowser url
    choice <-
        requestFullscreenChoiceWithBody
            runtime
            "Connect Google / Gemini"
            ( Text.intercalate "\n\n" $
                [ "[Open the Google page](" <> url <> ")."
                , if opened
                    then
                        "A browser window was opened automatically. Complete the Google step, then return here."
                    else
                        "The browser could not be opened automatically. Use the link above, then return here."
                ]
            )
            0
            [ ("Continue", "Finish this Google step and continue")
            , ("Cancel", "Stop without storing a credential")
            ]
    case choice of
        Just 0 -> pure ()
        _ -> fail "Google sign-in was cancelled"

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
            race
                (OpenAILogin.completeDeviceCodeLogin options device)
                (runOverlay
                    (const
                        (roleMuted color
                            "Waiting for OpenAI authorization · Esc/q cancel"))
                    (\key state -> case key of
                        PickerKeyCancel -> Left ()
                        _ -> Right state)
                    ())
                >>= \case
                Right _ -> do
                    printLoginMessage color True
                        "OpenAI login cancelled"
                    pure Nothing
                Left completed -> case completed of
                    Left err ->
                        printLoginMessage color False err >> pure Nothing
                    Right authJson -> do
                        now <- getCurrentTime
                        case openaiAuthStateFromJson now
                                (Aeson.encode authJson) of
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

connectGemini :: Bool -> IO (Maybe Text)
connectGemini color = do
    oauthOptions <- GeminiAuth.oauthOptionsFromEnv
    codeAssistOptions <- GeminiAuth.codeAssistOptionsFromEnv
    printLoginMessage color True
        "Opening Google sign-in for Gemini…"
    GeminiAuth.authenticateGoogleAccount
        oauthOptions
        codeAssistOptions
        (presentGoogleLogin color) >>= \case
            Left err -> do
                printLoginMessage color False
                    ("Google sign-in failed: " <> err)
                pure Nothing
            Right auth
                | Nothing <- auth.refreshToken -> do
                    printLoginMessage color False
                        "Google sign-in did not return a refresh token; disconnect access for Gemini CLI in your Google account and try again"
                    pure Nothing
                | otherwise ->
                    storeConnectedCredential color
                        GeminiProvider
                        auth.email
                        auth.email
                        SubscriptionBilled
                        ManagedGeminiAuthJson
                        (geminiAuthStateToJson auth)
                        >>= \stored -> pure (if stored then Just auth.email else Nothing)

presentGoogleLogin :: Bool -> Text -> IO ()
presentGoogleLogin color url = do
    Text.hPutStrLn stderr $
        roleMuted color "Open "
            <> rolePrompt color url
    opened <- openBrowser url
    unless opened $
        Text.hPutStrLn stderr $
            roleMuted color
                "Could not launch a browser automatically; open the URL above."
    Text.hPutStrLn stderr $
        roleMuted color "Waiting for Google…"
    hFlush stderr
accountAt :: Int -> [account] -> Maybe account
accountAt index accounts =
    case drop index accounts of
        account : _ -> Just account
        [] -> Nothing

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
