-- | Load provider credentials or validate a local subscription-backed CLI.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , authErrorNeedsOnboarding
    , geminiAuthErrorNeedsReconnect
    , geminiStartupAuthNeedsReconnect
    , GrokAuthState(..)
    , credentialAccountLabel
    , applyGrokAuthTokens
    , grokCredentialFromAuthJson
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokEmailFromAuthJson
    , grokNeedsRefresh
    , grokOAuthOptionsFromAuthJson
    , gatewayAuthSelectionId
    , gatewayRouterTokenProvider
    , isGatewayLoadedAuth
    , geminiAuthStateFromJson
    , geminiAuthStateToJson
    , geminiNeedsRefresh
    , classifyGeminiRefreshFailure
    , externalAuthSelectionId
    , externalGrokTokenProvider
    , hasOpenAiAuth
    , loadAuth
    , loadAuthForAccount
    , loadDirectOpenAiAuth
    , loadOpenAiDictationAuth
    , managedAuthSelectionId
    , managedGrokTokenProvider
    , managedGeminiTokenProvider
    , ExternalGrokLoaded(..)
    , ExternalGrokSource(..)
    , refreshGrokLoginPayload
    , openAIOAuthClientId
    , openAiAuthStateChanged
    , openaiAuthStateFromJson
    , preferredOpenAiTokenProvider
    , probeLoadedAuth
    , probeLoadedAuthCredential
    , reloadableFileCredentialProvider
    , staticCredentialProvider
    , xaiOAuthClientId
    ) where

import Agent.CLI.Auth.Grok
    ( ExternalGrokLoaded(..)
    , ExternalGrokSource(..)
    , externalGrokTokenProvider
    , grokNeedsRefresh
    , loadExternalGrokCredentials
    , managedGrokTokenProvider
    , refreshGrokLoginPayload
    )
import Agent.CLI.Auth.Gemini
    ( geminiAuthStateFromJson
    , geminiAuthStateToJson
    , geminiNeedsRefresh
    , classifyGeminiRefreshFailure
    , managedGeminiTokenProvider
    )
import Agent.CLI.Auth.OpenAI
    ( loadOpenAi
    , openAiAuthStateChanged
    , preferredOpenAiTokenProvider
    )
import qualified Agent.CLI.Auth.OpenAI as OpenAIAuth
import Agent.CLI.Auth.Types
    ( GrokAuthState(..)
    , LoadedAuth(..)
    , credentialAccountLabel
    , credentialAccountLabelWith
    , externalAuthSelectionId
    , applyGrokAuthTokens
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokCredentialFromAuthJson
    , grokEmailFromAuthJson
    , grokOAuthOptionsFromAuthJson
    , gatewayAuthSelectionId
    , isGatewayLoadedAuth
    , managedAuthSelectionId
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , xaiOAuthClientId
    )
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    )
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.GatewayClient
    ( GatewayCredential (..)
    , loadGatewayCredential
    )
import Agent.Error (ApiError(..))
import Agent.OpenAI.WebSocketClient
    ( isGatewayWebSocketCredential
    , validateGatewayWebSocketUrl
    )
import qualified Agent.Claude.Auth as ClaudeCode
import Agent.Provider
    ( AccountFailure(..)
    , BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider
    , credentialsExhaustedForRateLimit
    , getNextToken
    , providerSlug
    , seedTokenProvider
    , tokenProvider
    , tokenProviderWithNextToken
    )
import Agent.OpenRouter.Credential (credentialFromApiKey)
import qualified Agent.Gemini.Auth as GeminiAuth
import qualified Agent.Gemini.Credential as GeminiCredential
import qualified Agent.XAI.Auth as XAIAuth
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT
    , runExceptT
    , throwE
    )
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath (unsafeEncodeUtf, (</>))

loadAuth :: Maybe Provider -> IO (Either Text LoadedAuth)
loadAuth requestedProvider =
    loadGatewayCredential >>= \case
        Left gatewayErr ->
            pure (Left ("cannot load gateway credential: " <> gatewayErr))
        Right (Just gateway) ->
            case requestedProvider of
                Nothing -> pure (gatewayLoadedAuth gateway)
                Just OpenAIProvider -> pure (gatewayLoadedAuth gateway)
                Just ClaudeCodeProvider ->
                    pure (Right (gatewayClaudeLoadedAuth gateway))
                Just _ ->
                    pure
                        (Left
                            "organization gateway is active; disconnect it before selecting another provider")
        Right Nothing ->
            case requestedProvider of
                Nothing -> loadDetectedProvider
                Just provider -> loadProvider provider

loadDetectedProvider :: IO (Either Text LoadedAuth)
loadDetectedProvider =
    runExceptT (detectProvider Nothing) >>= \case
        Left err -> pure (Left err)
        Right provider -> loadProvider provider

loadProvider :: Provider -> IO (Either Text LoadedAuth)
loadProvider provider = runExceptT do
    case provider of
        XAIProvider -> loadXai Nothing
        OpenAIProvider -> loadOpenAi
        OpenRouterProvider -> loadOpenRouter Nothing
        GeminiProvider -> loadGemini Nothing
        ClaudeCodeProvider -> loadClaudeCode

-- | Load local OpenAI credentials without consulting the gateway.
--
-- Explicit local-account operations use this entry point so a saved gateway
-- cannot shadow the requested ChatGPT account.
loadDirectOpenAiAuth :: IO (Either Text LoadedAuth)
loadDirectOpenAiAuth =
    runExceptT loadOpenAi

-- | Dictation must not send an organization gateway bearer to a direct
-- provider transcription endpoint.
loadOpenAiDictationAuth :: IO (Maybe LoadedAuth)
loadOpenAiDictationAuth =
    loadGatewayCredential >>= \case
        Right Nothing -> OpenAIAuth.loadOpenAiDictationAuth
        Right (Just _) -> pure Nothing
        Left _ -> pure Nothing

gatewayLoadedAuth :: GatewayCredential -> Either Text LoadedAuth
gatewayLoadedAuth gateway = do
    validateGatewayWebSocketUrl gateway.gatewayWebSocketUrl
    let credential = credentialForGateway gateway
    pure LoadedAuth
            { loadedProvider = OpenAIProvider
            , loadedTokenProvider =
                staticCredentialProvider SubscriptionBilled credential
            , loadedAccountLabel = const (pure gateway.gatewayBaseUrl)
            , loadedSelectionId = Just gatewayAuthSelectionId
            , loadedOpenAiPool = Nothing
            }

gatewayClaudeLoadedAuth :: GatewayCredential -> LoadedAuth
gatewayClaudeLoadedAuth gateway =
    let credential = Credential
            { accessToken = ""
            , accountId = "gateway-claude"
            , leaseId = Nothing
            , provider = ClaudeCodeProvider
            }
    in LoadedAuth
        { loadedProvider = ClaudeCodeProvider
        , loadedTokenProvider =
            staticCredentialProvider SubscriptionBilled credential
        , loadedAccountLabel =
            const (pure ("Claude via " <> gateway.gatewayBaseUrl))
        , loadedSelectionId = Just gatewayAuthSelectionId
        , loadedOpenAiPool = Nothing
        }

credentialForGateway :: GatewayCredential -> Credential
credentialForGateway gateway =
    Credential
        { accessToken = gateway.gatewayAccessToken
        , accountId = gateway.gatewayWebSocketUrl
        , leaseId = Nothing
        , provider = OpenAIProvider
        }

-- | Refuse any credential that could route an organization model outside the
-- connected gateway, even if another wrapper accidentally supplies one.
gatewayRouterTokenProvider :: TokenProvider -> TokenProvider
gatewayRouterTokenProvider provider =
    tokenProviderWithNextToken provider \failed ->
        getNextToken provider failed >>= \case
            Right credential
                | isGatewayWebSocketCredential credential ->
                    pure (Right credential)
                | otherwise ->
                    pure $ Left $ CredentialError
                        "the connected gateway is unavailable; refusing to \
                        \send an organization model with direct OpenAI credentials"
            Left err -> pure (Left err)

-- | Load one specific account for providers whose HTTP backends can swap
-- token sources without reconnecting a long-lived transport.
--
-- A connected gateway remains authoritative even if a caller asks for a local
-- account. Callers must disconnect the gateway before selecting a local
-- source.
loadAuthForAccount :: Provider -> Text -> IO (Either Text LoadedAuth)
loadAuthForAccount provider selectionId =
    loadGatewayCredential >>= \case
        Left err ->
            pure (Left ("cannot load gateway credential: " <> err))
        Right (Just gateway)
            | provider == OpenAIProvider
            , selectionId == gatewayAuthSelectionId ->
                pure (gatewayLoadedAuth gateway)
            | otherwise ->
                pure
                    (Left
                        "organization gateway is active; disconnect it before selecting another account")
        Right Nothing -> loadLocalAccount provider selectionId
  where
    loadLocalAccount OpenAIProvider accountId =
        loadDirectOpenAiAccountAuth accountId
    loadLocalAccount selectedProvider selectedSelectionId =
        runExceptT case selectedProvider of
            XAIProvider -> loadXai (Just selectedSelectionId)
            OpenRouterProvider -> loadOpenRouter (Just selectedSelectionId)
            GeminiProvider -> loadGemini (Just selectedSelectionId)
            ClaudeCodeProvider ->
                throwE "Claude Code accounts are managed by `claude auth login`"

loadDirectOpenAiAccountAuth :: Text -> IO (Either Text LoadedAuth)
loadDirectOpenAiAccountAuth accountId =
    loadDirectOpenAiAuth >>= \case
        Left err -> pure (Left err)
        Right loaded -> case loaded.loadedOpenAiPool of
            Nothing ->
                pure (Left
                    "OpenAI account selection requires a live account pool")
            Just pool -> do
                preferred <- newIORef (Just accountId)
                pure $ Right loaded
                    { loadedTokenProvider =
                        preferredOpenAiTokenProvider
                            preferred
                            pool
                            loaded.loadedTokenProvider
                    , loadedSelectionId = Just accountId
                    }

-- | Ask the token source whether it has a usable credential now without
-- making a model request, preserving a successful checkout for later use.
probeLoadedAuth :: LoadedAuth -> IO (Either ApiError LoadedAuth)
probeLoadedAuth loaded =
    fmap snd <$> probeLoadedAuthCredential loaded

-- | Validate the current token source and preserve the checked credential for
-- the next real request. Returning the credential lets the UI show the active
-- account before an HTTP backend performs its first checkout.
probeLoadedAuthCredential
    :: LoadedAuth
    -> IO (Either ApiError (Credential, LoadedAuth))
probeLoadedAuthCredential loaded = do
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
                pure $ Right
                    ( credential
                    , loaded { loadedTokenProvider = tokenProvider }
                    )

detectProvider :: Maybe Provider -> ExceptT Text IO Provider
detectProvider (Just provider) = pure provider
detectProvider Nothing = do
    grok <- lift hasGrokAuth
    openai <- lift hasOpenAiAuth
    openrouter <- lift hasOpenRouterAuth
    gemini <- lift hasGeminiAuth
    if openai
        then pure OpenAIProvider
        else if grok
            then pure XAIProvider
            else if openrouter
                then pure OpenRouterProvider
                else if gemini
                    then pure GeminiProvider
                    else throwE noAuthHint

loadXai :: Maybe Text -> ExceptT Text IO LoadedAuth
loadXai requestedSelectionId = do
    managed <- lift (loadManagedCredential XAIProvider requestedSelectionId)
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
                    , loadedSelectionId =
                        Just (managedAuthSelectionId metadata.managedId)
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
                , loadedSelectionId =
                    Just (managedAuthSelectionId metadata.managedId)
                , loadedOpenAiPool = Nothing
                }

        Nothing -> do
            selected <- lift $
                selectExternalGrok
                    requestedSelectionId
                    <$> loadExternalGrokCredentials
            case selected of
                Nothing ->
                    throwE $
                        maybe noAuthHint
                            (const
                                (accountNotFound
                                    XAIProvider requestedSelectionId))
                            requestedSelectionId
                Just loaded -> do
                    clientId <-
                        lift $
                            xaiOAuthClientId
                                <$> lookupNonEmpty "XAI_OAUTH_CLIENT_ID"
                    let refreshToken =
                            XAIAuth.refreshAccessToken
                                (maybe
                                    (XAIAuth.defaultOAuthOptions clientId)
                                    (grokOAuthOptionsFromAuthJson clientId)
                                    loaded.grokRawJson)
                    provider <- lift $
                        externalGrokTokenProvider loaded refreshToken
                    pure LoadedAuth
                        { loadedProvider = XAIProvider
                        , loadedTokenProvider = provider
                        , loadedAccountLabel = pure . credentialAccountLabel
                        , loadedSelectionId = Just loaded.grokSelectionId
                        , loadedOpenAiPool = Nothing
                        }

loadClaudeCode :: ExceptT Text IO LoadedAuth
loadClaudeCode = do
    auth <- lift ClaudeCode.loadClaudeCodeAuth >>= either throwE pure
    let label = auth.accountLabel
        credential = Credential
            { accessToken = ""
            , accountId = "claude-code"
            , leaseId = Nothing
            , provider = ClaudeCodeProvider
            }
    pure LoadedAuth
        { loadedProvider = ClaudeCodeProvider
        , loadedTokenProvider =
            staticCredentialProvider SubscriptionBilled credential
        , loadedAccountLabel = const (pure label)
        , loadedSelectionId = Just "claude-code"
        , loadedOpenAiPool = Nothing
        }

loadOpenRouterCredential
    :: Maybe Text
    -> IO (Maybe (Text, Credential, Text))
loadOpenRouterCredential requestedSelectionId = do
    managed <- loadManagedCredential OpenRouterProvider requestedSelectionId
    case managed of
        Just (metadata, secret) ->
            pure $ Just
                ( managedAuthSelectionId metadata.managedId
                , (credentialFromApiKey secret.secretPayload)
                    { accountId = metadata.managedAccountId }
                , metadata.managedLabel
                )
        Nothing -> do
            external <- fmap
                (\key ->
                    ( externalAuthSelectionId
                        OpenRouterProvider
                        "environment"
                    , (credentialFromApiKey key)
                        { accountId = "openrouter" }
                    , ""
                    ))
                <$> lookupNonEmpty "OPENROUTER_API_KEY"
            pure $ external >>= \candidate@(selectionId, credential, _) ->
                if matchesSelection
                    requestedSelectionId
                    selectionId
                    credential
                    then Just candidate
                    else Nothing

loadOpenRouter :: Maybe Text -> ExceptT Text IO LoadedAuth
loadOpenRouter requestedSelectionId = do
    loadedCredential <- lift (loadOpenRouterCredential requestedSelectionId)
    case loadedCredential of
        Nothing ->
            throwE $
                maybe noAuthHint
                    (const
                        (accountNotFound
                            OpenRouterProvider requestedSelectionId))
                    requestedSelectionId
        Just (selectionId, initial, initialLabel) -> do
            provider <- lift $ reloadableFileCredentialProvider
                OpenRouterProvider
                ApiBilled
                initial
                (fmap (\(_, credential, _) -> credential)
                    <$> loadOpenRouterCredential (Just selectionId))
            pure LoadedAuth
                { loadedProvider = OpenRouterProvider
                , loadedTokenProvider = provider
                , loadedAccountLabel = \credential -> do
                    current <- loadOpenRouterCredential (Just selectionId)
                    let label = case current of
                            Just (_, currentCredential, currentLabel)
                                | currentCredential.accountId
                                    == credential.accountId ->
                                        currentLabel
                            _ -> initialLabel
                    pure (credentialAccountLabelWith label credential)
                , loadedSelectionId = Just selectionId
                , loadedOpenAiPool = Nothing
                }

loadGeminiCredential
    :: Maybe Text
    -> IO (Maybe (Text, Credential, Text))
loadGeminiCredential requestedSelectionId = do
    managed <- loadManagedCredential GeminiProvider requestedSelectionId
    case managed of
        Just (metadata, secret)
            | metadata.managedAuthKind == ManagedBearerToken ->
                pure $ Just
                    ( managedAuthSelectionId metadata.managedId
                    , (GeminiCredential.credentialFromApiKey secret.secretPayload)
                        { accountId = metadata.managedAccountId }
                    , metadata.managedLabel
                    )
        Just _ -> pure Nothing
        Nothing -> do
            googleKey <- lookupNonEmpty "GOOGLE_API_KEY"
            geminiKey <- lookupNonEmpty "GEMINI_API_KEY"
            let external = fmap
                    (\key ->
                        ( externalAuthSelectionId
                            GeminiProvider
                            "environment"
                        , (GeminiCredential.credentialFromApiKey key)
                            { accountId = "gemini" }
                        , ""
                        ))
                    (googleKey <|> geminiKey)
            pure $ external >>= \candidate@(selectionId, credential, _) ->
                if matchesSelection
                    requestedSelectionId
                    selectionId
                    credential
                    then Just candidate
                    else Nothing

loadGemini :: Maybe Text -> ExceptT Text IO LoadedAuth
loadGemini requestedSelectionId = do
    managed <- lift
        (loadManagedCredential GeminiProvider requestedSelectionId)
    case managed of
        Just (metadata, secret)
            | metadata.managedAuthKind == ManagedGeminiAuthJson -> do
                state <- maybe
                    (throwE
                        "managed Gemini OAuth credential contains invalid auth JSON; reconnect the account")
                    pure
                    (geminiAuthStateFromJson secret.secretPayload)
                case state.refreshToken of
                    Nothing ->
                        throwE
                            "managed Gemini OAuth credential has no refresh token; reconnect the Google account"
                    Just _ -> pure ()
                options <- lift GeminiAuth.oauthOptionsFromEnv
                provider <- lift $ managedGeminiTokenProvider
                    metadata
                    secret
                    state
                    (GeminiAuth.refreshAccessToken options)
                pure LoadedAuth
                    { loadedProvider = GeminiProvider
                    , loadedTokenProvider = provider
                    , loadedAccountLabel =
                        pure
                            . credentialAccountLabelWith
                                metadata.managedLabel
                    , loadedSelectionId =
                        Just (managedAuthSelectionId metadata.managedId)
                    , loadedOpenAiPool = Nothing
                    }
            | metadata.managedAuthKind /= ManagedBearerToken ->
                throwE "managed Gemini credential has an unsupported auth kind"
        _ -> loadGeminiApiKey requestedSelectionId

loadGeminiApiKey :: Maybe Text -> ExceptT Text IO LoadedAuth
loadGeminiApiKey requestedSelectionId = do
    loadedCredential <- lift (loadGeminiCredential requestedSelectionId)
    case loadedCredential of
        Nothing ->
            throwE $
                maybe noAuthHint
                    (const
                        (accountNotFound
                            GeminiProvider requestedSelectionId))
                    requestedSelectionId
        Just (selectionId, initial, initialLabel) -> do
            provider <- lift $ reloadableFileCredentialProvider
                GeminiProvider
                ApiBilled
                initial
                (fmap (\(_, credential, _) -> credential)
                    <$> loadGeminiCredential (Just selectionId))
            pure LoadedAuth
                { loadedProvider = GeminiProvider
                , loadedTokenProvider = provider
                , loadedAccountLabel = \credential -> do
                    current <- loadGeminiCredential (Just selectionId)
                    let label = case current of
                            Just (_, currentCredential, currentLabel)
                                | currentCredential.accountId
                                    == credential.accountId ->
                                        currentLabel
                            _ -> initialLabel
                    pure (credentialAccountLabelWith label credential)
                , loadedSelectionId = Just selectionId
                , loadedOpenAiPool = Nothing
                }

loadManagedCredential
    :: Provider
    -> Maybe Text
    -> IO (Maybe (ManagedCredential, ManagedSecret))
loadManagedCredential provider requestedSelectionId =
    loadManagedCredentials >>= \case
        Left _ -> pure Nothing
        Right credentials ->
            pure $ listToMaybe
                [ (metadata, secret)
                | (metadata, secret) <- credentials
                , metadata.managedEnabled
                , metadata.managedProvider == provider
                , matchesManagedSelection requestedSelectionId metadata
                ]

matchesManagedSelection :: Maybe Text -> ManagedCredential -> Bool
matchesManagedSelection requested metadata =
    case requested of
        Nothing -> True
        Just selectionId ->
            case Text.stripPrefix "managed:" selectionId of
                Just managedId -> metadata.managedId == managedId
                Nothing -> metadata.managedAccountId == selectionId

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
    configuredCodexHome <- lookupNonEmpty "CODEX_HOME"
    let codexDirectory =
            maybe
                (home </> unsafeEncodeUtf ".codex")
                (unsafeEncodeUtf . Text.unpack)
                configuredCodexHome
    file <- doesFileExist
        (codexDirectory </> unsafeEncodeUtf "auth.json")
    managed <- hasManagedProvider OpenAIProvider
    pure (isJust envJson || isJust envToken || file || managed)

hasOpenRouterAuth :: IO Bool
hasOpenRouterAuth = do
    environment <- isJust <$> lookupNonEmpty "OPENROUTER_API_KEY"
    managed <- hasManagedProvider OpenRouterProvider
    pure (environment || managed)

hasGeminiAuth :: IO Bool
hasGeminiAuth = do
    google <- isJust <$> lookupNonEmpty "GOOGLE_API_KEY"
    gemini <- isJust <$> lookupNonEmpty "GEMINI_API_KEY"
    managed <- hasManagedProvider GeminiProvider
    pure (google || gemini || managed)

hasManagedProvider :: Provider -> IO Bool
hasManagedProvider provider =
    isJust <$> loadManagedCredential provider Nothing

matchesSelection :: Maybe Text -> Text -> Credential -> Bool
matchesSelection requested selectionId credential =
    maybe True
        (\requestedId ->
            requestedId == selectionId
                || requestedId == credential.accountId)
        requested

selectExternalGrok
    :: Maybe Text
    -> [ExternalGrokLoaded]
    -> Maybe ExternalGrokLoaded
selectExternalGrok requested =
    find \loaded ->
        matchesSelection
            requested
            loaded.grokSelectionId
            (Credential
                { accessToken = loaded.grokState.grokAccessToken
                , accountId =
                    fromMaybe "grok"
                        (XAIAuth.accountIdFromAccessToken
                            loaded.grokState.grokAccessToken)
                , leaseId = Nothing
                , provider = XAIProvider
                })

accountNotFound :: Provider -> Maybe Text -> Text
accountNotFound provider requested =
    "no enabled "
        <> providerSlug provider
        <> " credential found for account "
        <> fromMaybe "(unknown)" requested

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
                            "reloaded credential is unchanged; refresh the configured credential source and retry"
                    | otherwise -> do
                        writeIORef cache (Just credential)
                        pure (Right credential)
    pure $ tokenProvider billing \failed -> case failed of
            Just reported -> credentialsExhaustedForRateLimit reported >>= \case
                Just err -> pure (Left err)
                Nothing -> case reported of
                    FailedCredential
                        { credential = rejected
                        , failure = AccountAuthenticationRejected
                        } -> do
                            writeIORef cache Nothing
                            loadFresh (Just rejected.accessToken)
                    _ -> pure $ Left $ CredentialError
                        "unsupported credential failure"
            Nothing ->
                readIORef cache >>= \case
                    Just credential -> pure (Right credential)
                    Nothing -> loadFresh Nothing

staticCredentialProvider :: BillingMode -> Credential -> TokenProvider
staticCredentialProvider billing credential =
    tokenProvider billing \failed -> case failed of
        Nothing -> pure (Right credential)
        Just reported -> credentialsExhaustedForRateLimit reported >>= \case
            Just err -> pure (Left err)
            Nothing -> pure $ Left $ CredentialError
                "static credential was rejected"

noAuthHint :: Text
noAuthHint =
    "no credentials found. Set GROK_ACCESS_TOKEN, CODEX_ACCESS_TOKEN, \
    \OPENROUTER_API_KEY, GOOGLE_API_KEY, or GEMINI_API_KEY, \
    \place auth at ~/.grok/auth.json / ~/.codex/auth.json, \
    \connect a Google account from /model or /account, \
    \or use --provider claude-code after `claude auth login`."

authErrorNeedsOnboarding :: Text -> Bool
authErrorNeedsOnboarding message =
    "no credentials found." `Text.isPrefixOf` message
        || "no valid OpenAI credentials found:" `Text.isPrefixOf` message

-- | Decide whether selecting a Gemini model should offer Google sign-in.
-- Besides a first connection, this covers malformed or rejected managed OAuth
-- state. Network and rate-limit failures remain ordinary switch errors rather
-- than opening a fresh browser flow.
geminiAuthErrorNeedsReconnect :: Text -> Bool
geminiAuthErrorNeedsReconnect message =
    authErrorNeedsOnboarding message
        || any (`Text.isInfixOf` message)
            [ "managed Gemini OAuth credential contains invalid auth JSON"
            , "managed Gemini OAuth credential has no refresh token"
            , "managed Gemini credential has an unsupported auth kind"
            , "Google OAuth token request failed with HTTP 400"
            , "Google OAuth token request failed with HTTP 401"
            ]

geminiStartupAuthNeedsReconnect :: Bool -> Text -> Bool
geminiStartupAuthNeedsReconnect targetsGemini message =
    (geminiAuthErrorNeedsReconnect message
        && not (authErrorNeedsOnboarding message))
        || (targetsGemini
            && ( authErrorNeedsOnboarding message
                || "no enabled gemini credential"
                    `Text.isInfixOf` Text.toLower message
               ))
