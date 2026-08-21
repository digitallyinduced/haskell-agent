-- | Load ChatGPT, Grok, OpenRouter, or broker credentials for the CLI process.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , grokCredentialFromAuthJson
    , grokEmailFromAuthJson
    , loadAuth
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , probeLoadedAuth
    , reloadableFileCredentialProvider
    , xaiOAuthClientId
    ) where

import Agent.Broker (BrokerOptions(..), newBrokerTokenProviderFor)
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , loadManagedCredentials
    , upsertManagedCredential
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.FileRetry (retryOnFileBusy)
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider(..)
    , getNextToken
    , seedTokenProvider
    )
import Agent.OpenRouter.Credential (credentialFromApiKey)
import Agent.XAI.Auth (accountIdFromAccessToken, emailFromToken)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Directory.OsPath (doesFileExist, getHomeDirectory)
import System.OsPath ((</>))
import qualified System.OsPath as OsPath
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
    -- | Live OpenAI OAuth pool, when authentication uses one.
    , loadedOpenAiPool :: !(Maybe OpenAI.Pool)
    }

loadAuth :: Maybe Provider -> IO (Either Text LoadedAuth)
loadAuth requested = runExceptT do
    brokerUrl <- lift (lookupNonEmpty "AGENT_BROKER_URL")
    case brokerUrl of
        Just url -> loadBroker url requested
        Nothing -> do
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
                pure $ Left $ ProviderError AuthenticationError
                    "credential provider does not match loaded auth"
                    Nothing
            | otherwise -> do
                tokenProvider <-
                    seedTokenProvider loaded.loadedTokenProvider credential
                pure $ Right loaded { loadedTokenProvider = tokenProvider }

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

loadBroker :: Text -> Maybe Provider -> ExceptT Text IO LoadedAuth
loadBroker url requested = do
    serviceToken <- lift (lookupNonEmpty "AGENT_BROKER_TOKEN")
        >>= maybe
            (throwE "AGENT_BROKER_URL is set; also set AGENT_BROKER_TOKEN")
            pure
    let supportedProviders = case requested of
            Just selected -> [selected]
            Nothing -> [OpenAIProvider, XAIProvider, OpenRouterProvider]
    provider <- lift $ newBrokerTokenProviderFor
        BrokerOptions
            { baseUrl = Text.unpack url
            , serviceToken
            }
        supportedProviders
    credential <- withExceptT
        (\err -> "broker: " <> Text.pack (show err))
        (ExceptT (getNextToken provider Nothing))
    let actual = credential.provider
    when (maybe False (/= actual) requested) $
        throwE $
            "broker returned " <> Text.pack (show actual)
                <> " but --provider asked for a different vendor"
    seeded <- lift (seedTokenProvider provider credential)
    pure LoadedAuth
        { loadedProvider = actual
        , loadedTokenProvider = seeded
        , loadedOpenAiPool = Nothing
        }

loadXai :: ExceptT Text IO LoadedAuth
loadXai = do
    credential <- lift loadXaiCredential
    case credential of
        Nothing -> throwE noAuthHint
        Just loaded -> do
            provider <- lift $ reloadableFileCredentialProvider
                XAIProvider loaded loadXaiCredential
            pure LoadedAuth
                { loadedProvider = XAIProvider
                , loadedTokenProvider = provider
                , loadedOpenAiPool = Nothing
                }

loadXaiCredential :: IO (Maybe Credential)
loadXaiCredential = do
    managed <- loadManagedCredential XAIProvider
    case managed of
        Just (metadata, secret) ->
            pure $ managedBearerCredential metadata secret
        Nothing -> loadGrokCredential

loadOpenRouterKey :: IO (Maybe Text)
loadOpenRouterKey = do
    managed <- loadManagedCredential OpenRouterProvider
    case managed of
        Just (_, secret) -> pure (Just secret.secretPayload)
        Nothing -> lookupNonEmpty "OPENROUTER_API_KEY"

loadOpenRouter :: ExceptT Text IO LoadedAuth
loadOpenRouter = do
    key <- lift loadOpenRouterKey
    case key of
        Nothing -> throwE noAuthHint
        Just apiKey -> do
            let initial = credentialFromApiKey apiKey
            provider <- lift $ reloadableFileCredentialProvider OpenRouterProvider initial
                (fmap (fmap credentialFromApiKey) loadOpenRouterKey)
            pure LoadedAuth
                { loadedProvider = OpenRouterProvider
                , loadedTokenProvider = provider
                , loadedOpenAiPool = Nothing
                }

loadOpenAi :: ExceptT Text IO LoadedAuth
loadOpenAi = do
    managed <- lift (loadManagedCredential OpenAIProvider)
    fromEnvToken <- lift (lookupNonEmpty "CODEX_ACCESS_TOKEN")
    fromEnvJson <- lift (lookupNonEmpty "CODEX_AUTH_JSON")
    home <- lift getHomeDirectory
    let filePath =
            home </> fromFilePath ".codex" </> fromFilePath "auth.json"
    fileExists <- lift (doesFileExist filePath)
    fileBytes <- if fileExists
        then lift (Just <$> retryOnFileBusy (LBS.readFile (toFilePath filePath)))
        else pure Nothing
    now <- lift getCurrentTime
    case managed of
        Just (metadata, secret) ->
            loadManagedOpenAI now metadata secret
        Nothing ->
            case fromEnvToken of
                Just token -> do
                    accountId <- lift (openaiAccountIdForToken token)
                    pure LoadedAuth
                        { loadedProvider = OpenAIProvider
                        , loadedTokenProvider = staticCredentialProvider Credential
                            { accessToken = token
                            , accountId
                            , leaseId = Nothing
                            , provider = OpenAIProvider
                            }
                        , loadedOpenAiPool = Nothing
                        }
                Nothing -> case envOrFileState now fromEnvJson fileBytes of
                    Nothing -> throwE noAuthHint
                    Just state ->
                        openAiPoolAuth fileExists filePath state

loadManagedOpenAI
    :: UTCTime
    -> ManagedCredential
    -> ManagedSecret
    -> ExceptT Text IO LoadedAuth
loadManagedOpenAI now metadata secret =
    case metadata.managedAuthKind of
        ManagedOpenAIAuthJson ->
            case openaiAuthStateFromJson now
                (LBS.fromStrict (TextEncoding.encodeUtf8 secret.secretPayload)) of
                Nothing ->
                    throwE "managed OpenAI OAuth credential contains invalid auth JSON"
                Just state -> do
                    clientId <-
                        lift $
                            openAIOAuthClientId
                                <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
                    let refresh stale =
                            OpenAI.refreshAccessTokenHTTP clientId stale >>= \case
                                Left err -> pure (Left err)
                                Right newState -> do
                                    stamped <- authStateToJson newState <$> getCurrentTime
                                    let payload =
                                            TextEncoding.decodeUtf8
                                                (LBS.toStrict (Aeson.encode stamped))
                                    upsertManagedCredential
                                        metadata
                                        secret { secretPayload = payload }
                                        >>= \case
                                            Left err ->
                                                pure $ Left $ ConnectionError err
                                            Right () -> pure (Right newState)
                    pool <- lift (OpenAI.newPool [state] refresh)
                    tokenProvider <- lift (OpenAICredential.poolTokenProvider pool)
                    pure LoadedAuth
                        { loadedProvider = OpenAIProvider
                        , loadedTokenProvider = tokenProvider
                        , loadedOpenAiPool = Just pool
                        }
        _ ->
            pure LoadedAuth
                { loadedProvider = OpenAIProvider
                , loadedTokenProvider =
                    staticCredentialProvider Credential
                        { accessToken = secret.secretPayload
                        , accountId = metadata.managedAccountId
                        , leaseId = Nothing
                        , provider = OpenAIProvider
                        }
                , loadedOpenAiPool = Nothing
                }

openAiPoolAuth
    :: Bool
    -> OsPath
    -> OpenAI.AuthState
    -> ExceptT Text IO LoadedAuth
openAiPoolAuth persistRefresh filePath state = do
    clientId <-
        lift $
            openAIOAuthClientId <$> lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
    let refresh stale =
            OpenAI.refreshAccessTokenHTTP clientId stale >>= \case
                Left err -> pure (Left err)
                Right newState
                    | persistRefresh -> do
                        stamped <- authStateToJson newState <$> getCurrentTime
                        OpenAILogin.writeAuthFile filePath stamped
                        pure (Right newState)
                    | otherwise -> pure (Right newState)
    pool <- lift (OpenAI.newPool [state] refresh)
    tokenProvider <- lift (OpenAICredential.poolTokenProvider pool)
    pure LoadedAuth
        { loadedProvider = OpenAIProvider
        , loadedTokenProvider = tokenProvider
        , loadedOpenAiPool = Just pool
        }

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

managedBearerCredential
    :: ManagedCredential
    -> ManagedSecret
    -> Maybe Credential
managedBearerCredential metadata secret =
    let token = case metadata.managedAuthKind of
            ManagedGrokAuthJson ->
                grokCredentialFromAuthJson secret.secretPayload
            _ -> Just secret.secretPayload
    in (\accessToken -> Credential
        { accessToken
        , accountId = metadata.managedAccountId
        , leaseId = Nothing
        , provider = metadata.managedProvider
        }) <$> token

envOrFileState
    :: UTCTime
    -> Maybe Text
    -> Maybe LBS.ByteString
    -> Maybe OpenAI.AuthState
envOrFileState now fromEnvJson fileBytes =
    (fromEnvJson >>= openaiAuthStateFromJson now . LBS.fromStrict . TextEncoding.encodeUtf8)
        <|> (fileBytes >>= openaiAuthStateFromJson now)

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

loadGrokCredential :: IO (Maybe Credential)
loadGrokCredential = do
    fromJson <- lookupNonEmpty "GROK_AUTH_JSON"
    fromToken <- lookupNonEmpty "GROK_ACCESS_TOKEN"
    home <- getHomeDirectory
    let filePath =
            home </> fromFilePath ".grok" </> fromFilePath "auth.json"
    fileExists <- doesFileExist filePath
    fileJson <- if fileExists
        then Just . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> retryOnFileBusy (LBS.readFile (toFilePath filePath))
        else pure Nothing
    let token =
            (fromJson >>= grokCredentialFromAuthJson)
                <|> fromToken
                <|> (fileJson >>= grokCredentialFromAuthJson)
    pure (fmap grokCredential token)

grokCredentialFromAuthJson :: Text -> Maybe Text
grokCredentialFromAuthJson raw = do
    value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    entryToken value <|> firstNestedToken value
  where
    entryToken (Aeson.Object object) =
        textField "key" object <|> textField "access_token" object
    entryToken _ = Nothing

    firstNestedToken (Aeson.Object object) =
        listToMaybe
            [ token
            | nested <- KeyMap.elems object
            , Just token <- [entryToken nested]
            ]
    firstNestedToken _ = Nothing

grokEmailFromAuthJson :: Text -> Maybe Text
grokEmailFromAuthJson raw = do
    value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    entryEmail value <|> firstNestedEmail value
  where
    entryEmail (Aeson.Object object) =
        textField "email" object
            <|> (textField "id_token" object >>= emailFromToken)
            <|> (textField "access_token" object >>= emailFromToken)
            <|> (textField "key" object >>= emailFromToken)
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
    , accountId = fromMaybe "grok" (accountIdFromAccessToken token)
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
        (home </> fromFilePath ".grok" </> fromFilePath "auth.json")
    managed <- hasManagedProvider XAIProvider
    pure (isJust envJson || isJust envToken || file || managed)

hasOpenAiAuth :: IO Bool
hasOpenAiAuth = do
    envJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    envToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    home <- getHomeDirectory
    file <- doesFileExist
        (home </> fromFilePath ".codex" </> fromFilePath "auth.json")
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
    -> Credential
    -> IO (Maybe Credential)
    -> IO TokenProvider
reloadableFileCredentialProvider expectedProvider initial reload = do
    cache <- newIORef (Just initial)
    let loadFresh rejectedToken =
            reload >>= \case
                Nothing ->
                    pure $ Left $ ProviderError AuthenticationError
                        "no credentials found while reloading auth"
                        Nothing
                Just credential
                    | credential.provider /= expectedProvider ->
                        pure $ Left $ ProviderError AuthenticationError
                            ("reloaded auth resolved "
                                <> Text.pack (show credential.provider)
                                <> " but this session expects "
                                <> Text.pack (show expectedProvider))
                            Nothing
                    | rejectedToken == Just credential.accessToken ->
                        pure $ Left $ ProviderError AuthenticationError
                            "reloaded credential is unchanged; refresh ~/.grok/auth.json or OPENROUTER_API_KEY and retry"
                            Nothing
                    | otherwise -> do
                        writeIORef cache (Just credential)
                        pure (Right credential)
    pure $ TokenProvider \failed -> case failed of
        Just FailedCredential { failure = AccountRateLimited { retryAfterSeconds } } -> do
            now <- getCurrentTime
            let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
            pure $ Left $ CredentialsExhausted
                (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential
            { credential = rejected
            , failure = AccountAuthenticationRejected
            } -> do
            writeIORef cache Nothing
            loadFresh (Just rejected.accessToken)
        Nothing ->
            readIORef cache >>= \case
                Just credential -> pure (Right credential)
                Nothing -> loadFresh Nothing

staticCredentialProvider :: Credential -> TokenProvider
staticCredentialProvider credential = TokenProvider \failed ->
    case failed of
        Nothing -> pure (Right credential)
        Just _ -> pure $ Left $ ProviderError AuthenticationError
            "static credential was rejected"
            Nothing

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
