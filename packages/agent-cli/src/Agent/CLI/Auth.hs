-- | Load provider credentials or validate a local subscription-backed CLI.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , authErrorNeedsOnboarding
    , GrokAuthState(..)
    , credentialAccountLabel
    , grokCredentialFromAuthJson
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokEmailFromAuthJson
    , externalAuthSelectionId
    , loadAuth
    , loadAuthForAccount
    , managedAuthSelectionId
    , managedGrokTokenProvider
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
    ( loadExternalGrokCredentials
    , managedGrokTokenProvider
    )
import Agent.CLI.Auth.OpenAI
    ( loadOpenAi
    , openAiAuthStateChanged
    , preferredOpenAiTokenProvider
    )
import Agent.CLI.Auth.Types
    ( GrokAuthState(..)
    , LoadedAuth(..)
    , credentialAccountLabel
    , credentialAccountLabelWith
    , externalAuthSelectionId
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokCredentialFromAuthJson
    , grokEmailFromAuthJson
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
import Agent.OpenAI.WebSocketClient (validateGatewayWebSocketUrl)
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
    , tokenProviderBillingMode
    )
import Agent.OpenRouter.Credential (credentialFromApiKey)
import qualified Agent.XAI.Auth as XAIAuth
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
loadAuth (Just OpenAIProvider) = loadOpenAiWithGateway
loadAuth (Just provider) = loadProvider provider
loadAuth Nothing =
    loadGatewayCredential >>= \case
        Left gatewayErr ->
            loadSubscriptionFallback gatewayErr loadDetectedProvider
        Right (Just gateway) -> loadGatewayPreferredAuth gateway
        Right Nothing -> loadDetectedProvider

loadOpenAiWithGateway :: IO (Either Text LoadedAuth)
loadOpenAiWithGateway =
    loadGatewayCredential >>= \case
        Left gatewayErr ->
            loadSubscriptionFallback
                gatewayErr
                (loadProvider OpenAIProvider)
        Right (Just gateway) -> loadGatewayPreferredAuth gateway
        Right Nothing -> loadProvider OpenAIProvider

loadSubscriptionFallback
    :: Text
    -> IO (Either Text LoadedAuth)
    -> IO (Either Text LoadedAuth)
loadSubscriptionFallback gatewayErr loadFallback =
    loadFallback >>= \case
        Right loaded
            | tokenProviderBillingMode
                loaded.loadedTokenProvider
                == SubscriptionBilled ->
                    pure (Right loaded)
        Right _ ->
            pure $ Left $
                "cannot load gateway credential: "
                    <> gatewayErr
                    <> "; refusing automatic fallback to "
                    <> "API-credit billing"
        Left providerErr ->
            pure $ Left $
                "cannot load gateway credential: "
                    <> gatewayErr
                    <> "; "
                    <> providerErr

loadDetectedProvider :: IO (Either Text LoadedAuth)
loadDetectedProvider =
    runExceptT (detectProvider Nothing) >>= \case
        Left err -> pure (Left err)
        Right provider -> loadProvider provider

loadProvider :: Provider -> IO (Either Text LoadedAuth)
loadProvider provider = runExceptT case provider of
    XAIProvider -> loadXai Nothing
    OpenAIProvider -> loadOpenAi
    OpenRouterProvider -> loadOpenRouter Nothing
    ClaudeCodeProvider -> loadClaudeCode

-- | Load local OpenAI credentials without consulting the gateway.
--
-- Explicit account selection and gateway failover use this entry point so a
-- saved gateway can never shadow the user's local ChatGPT account pool.
loadDirectOpenAiAuth :: IO (Either Text LoadedAuth)
loadDirectOpenAiAuth =
    runExceptT loadOpenAi

loadGatewayPreferredAuth
    :: GatewayCredential
    -> IO (Either Text LoadedAuth)
loadGatewayPreferredAuth =
    pure . gatewayLoadedAuth

gatewayLoadedAuth :: GatewayCredential -> Either Text LoadedAuth
gatewayLoadedAuth gateway = do
    validateGatewayWebSocketUrl gateway.gatewayWebSocketUrl
    let credential = credentialForGateway gateway
    pure LoadedAuth
            { loadedProvider = OpenAIProvider
            , loadedTokenProvider =
                staticCredentialProvider SubscriptionBilled credential
            , loadedAccountLabel = const (pure gateway.gatewayBaseUrl)
            , loadedSelectionId = Just "gateway"
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

-- | Load one specific account for providers whose HTTP backends can swap
-- token sources without reconnecting a long-lived transport.
loadAuthForAccount :: Provider -> Text -> IO (Either Text LoadedAuth)
loadAuthForAccount provider selectionId =
    if provider == OpenAIProvider
        then
            if selectionId == "gateway"
                then loadGatewayCredential >>= \case
                    Left err ->
                        pure (Left
                            ("cannot load gateway credential: " <> err))
                    Right (Just gateway) ->
                        loadGatewayPreferredAuth gateway
                    Right Nothing ->
                        pure (Left "no gateway credential is connected")
                else loadDirectOpenAiAccountAuth selectionId
        else runExceptT case provider of
            XAIProvider -> loadXai (Just selectionId)
            OpenRouterProvider -> loadOpenRouter (Just selectionId)
            OpenAIProvider ->
                throwE "OpenAI account selection is handled by the live account pool"
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
    if openai
        then pure OpenAIProvider
        else if grok
            then pure XAIProvider
            else if openrouter
                then pure OpenRouterProvider
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
                selectExternalCredential
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
                Just (selectionId, loaded) -> do
                    provider <- lift $ reloadableFileCredentialProvider
                        XAIProvider
                        SubscriptionBilled
                        loaded
                        (fmap snd
                            . selectExternalCredential (Just selectionId)
                            <$> loadExternalGrokCredentials)
                    pure LoadedAuth
                        { loadedProvider = XAIProvider
                        , loadedTokenProvider = provider
                        , loadedAccountLabel = pure . credentialAccountLabel
                        , loadedSelectionId = Just selectionId
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
    isJust <$> loadManagedCredential provider Nothing

matchesSelection :: Maybe Text -> Text -> Credential -> Bool
matchesSelection requested selectionId credential =
    maybe True
        (\requestedId ->
            requestedId == selectionId
                || requestedId == credential.accountId)
        requested

selectExternalCredential
    :: Maybe Text
    -> [(Text, Credential)]
    -> Maybe (Text, Credential)
selectExternalCredential requested =
    find (\(selectionId, credential) ->
        matchesSelection requested selectionId credential)

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
                            "reloaded credential is unchanged; refresh ~/.grok/auth.json or OPENROUTER_API_KEY and retry"
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
    \or OPENROUTER_API_KEY, place auth at ~/.grok/auth.json / ~/.codex/auth.json, \
    \or use --provider claude-code after `claude auth login`."

authErrorNeedsOnboarding :: Text -> Bool
authErrorNeedsOnboarding message =
    "no credentials found." `Text.isPrefixOf` message
        || "no valid OpenAI credentials found:" `Text.isPrefixOf` message
