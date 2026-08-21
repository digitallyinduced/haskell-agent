-- | Load ChatGPT, Grok, OpenRouter, or broker credentials for the CLI process.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , grokCredentialFromAuthJson
    , loadAuth
    , openAIOAuthClientId
    , openaiAuthStateFromJson
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
import Agent.XAI.Auth (accountIdFromAccessToken)
import Control.Applicative ((<|>))
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
import System.Environment (lookupEnv)
import System.OsPath ((</>))

openAIOAuthClientId :: Maybe Text -> Text
openAIOAuthClientId =
    fromMaybe "app_EMoamEEZ73f0CkXaXp7hrann"

xaiOAuthClientId :: Maybe Text -> Text
xaiOAuthClientId =
    fromMaybe "b1a00492-073a-47ea-816f-4c329264a828"

data LoadedAuth = LoadedAuth
    { loadedProvider :: !Provider
    , loadedTokenProvider :: !TokenProvider
    }

loadAuth :: Maybe Provider -> IO (Either String LoadedAuth)
loadAuth requested = do
    brokerUrl <- lookupNonEmpty "AGENT_BROKER_URL"
    case brokerUrl of
        Just url -> loadBroker url requested
        Nothing -> do
            provider <- detectProvider requested
            case provider of
                Left err -> pure (Left err)
                Right XAIProvider -> loadXai
                Right OpenAIProvider -> loadOpenAi
                Right OpenRouterProvider -> loadOpenRouter

detectProvider :: Maybe Provider -> IO (Either String Provider)
detectProvider (Just provider) = pure (Right provider)
detectProvider Nothing = do
    grok <- hasGrokAuth
    openai <- hasOpenAiAuth
    openrouter <- hasOpenRouterAuth
    pure $ if grok
        then Right XAIProvider
        else if openai
            then Right OpenAIProvider
            else if openrouter
                then Right OpenRouterProvider
                else Left noAuthHint

loadBroker :: Text -> Maybe Provider -> IO (Either String LoadedAuth)
loadBroker url requested = do
    token <- lookupNonEmpty "AGENT_BROKER_TOKEN"
    case token of
        Nothing -> pure (Left "AGENT_BROKER_URL is set; also set AGENT_BROKER_TOKEN")
        Just serviceToken -> do
            let supportedProviders = case requested of
                    Just selected -> [selected]
                    Nothing -> [OpenAIProvider, XAIProvider, OpenRouterProvider]
            provider <- newBrokerTokenProviderFor
                BrokerOptions
                    { baseUrl = Text.unpack url
                    , serviceToken
                    }
                supportedProviders
            first <- getNextToken provider Nothing
            case first of
                Left err -> pure (Left ("broker: " <> show err))
                Right credential ->
                    let actual = credential.provider
                    in if maybe False (/= actual) requested
                        then pure $ Left $
                            "broker returned " <> show actual
                                <> " but --provider asked for a different vendor"
                        else do
                            seeded <- seedTokenProvider provider credential
                            pure $ Right LoadedAuth
                                { loadedProvider = actual
                                , loadedTokenProvider = seeded
                                }

loadXai :: IO (Either String LoadedAuth)
loadXai = do
    credential <- loadXaiCredential
    case credential of
        Nothing -> pure (Left noAuthHint)
        Just loaded -> do
            provider <- reloadableFileCredentialProvider
                XAIProvider loaded loadXaiCredential
            pure $ Right LoadedAuth
                { loadedProvider = XAIProvider
                , loadedTokenProvider = provider
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

loadOpenRouter :: IO (Either String LoadedAuth)
loadOpenRouter = do
    key <- loadOpenRouterKey
    case key of
        Nothing -> pure (Left noAuthHint)
        Just apiKey -> do
            let initial = credentialFromApiKey apiKey
            provider <- reloadableFileCredentialProvider OpenRouterProvider initial
                (fmap (fmap credentialFromApiKey) loadOpenRouterKey)
            pure $ Right LoadedAuth
                { loadedProvider = OpenRouterProvider
                , loadedTokenProvider = provider
                }

loadOpenAi :: IO (Either String LoadedAuth)
loadOpenAi = do
    managed <- loadManagedCredential OpenAIProvider
    fromEnvToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    fromEnvJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    home <- getHomeDirectory
    let filePath =
            home </> fromFilePath ".codex" </> fromFilePath "auth.json"
    fileExists <- doesFileExist filePath
    fileBytes <- if fileExists
        then Just <$> LBS.readFile (toFilePath filePath)
        else pure Nothing
    now <- getCurrentTime
    case managed of
        Just (metadata, secret) ->
            loadManagedOpenAI now metadata secret
        Nothing ->
            case fromEnvToken of
                Just token -> do
                    accountId <- openaiAccountIdForToken token
                    pure $ Right LoadedAuth
                        { loadedProvider = OpenAIProvider
                        , loadedTokenProvider = staticCredentialProvider Credential
                            { accessToken = token
                            , accountId
                            , leaseId = Nothing
                            , provider = OpenAIProvider
                            }
                        }
                Nothing -> case envOrFileState now fromEnvJson fileBytes of
                    Nothing -> pure (Left noAuthHint)
                    Just state ->
                        openAiPoolAuth fileExists filePath state

loadManagedOpenAI
    :: UTCTime
    -> ManagedCredential
    -> ManagedSecret
    -> IO (Either String LoadedAuth)
loadManagedOpenAI now metadata secret =
    case metadata.managedAuthKind of
        ManagedOpenAIAuthJson ->
            case openaiAuthStateFromJson now
                (LBS.fromStrict (TextEncoding.encodeUtf8 secret.secretPayload)) of
                Nothing ->
                    pure $ Left
                        "managed OpenAI OAuth credential contains invalid auth JSON"
                Just state -> do
                    clientId <-
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
                    pool <- OpenAI.newPool [state] refresh
                    tokenProvider <- OpenAICredential.poolTokenProvider pool
                    pure $ Right LoadedAuth
                        { loadedProvider = OpenAIProvider
                        , loadedTokenProvider = tokenProvider
                        }
        _ ->
            pure $ Right LoadedAuth
                { loadedProvider = OpenAIProvider
                , loadedTokenProvider =
                    staticCredentialProvider Credential
                        { accessToken = secret.secretPayload
                        , accountId = metadata.managedAccountId
                        , leaseId = Nothing
                        , provider = OpenAIProvider
                        }
                }

openAiPoolAuth
    :: Bool
    -> OsPath
    -> OpenAI.AuthState
    -> IO (Either String LoadedAuth)
openAiPoolAuth persistRefresh filePath state = do
    clientId <-
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
    pool <- OpenAI.newPool [state] refresh
    tokenProvider <- OpenAICredential.poolTokenProvider pool
    pure $ Right LoadedAuth
        { loadedProvider = OpenAIProvider
        , loadedTokenProvider = tokenProvider
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
            <$> LBS.readFile (toFilePath filePath)
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
    value <- lookupEnv name
    pure $ case value of
        Just text | not (null text) -> Just (Text.pack text)
        _ -> Nothing

textField :: Text -> Aeson.Object -> Maybe Text
textField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) | not (Text.null value) -> Just value
    _ -> Nothing

noAuthHint :: String
noAuthHint =
    "no credentials found. Set GROK_ACCESS_TOKEN, CODEX_ACCESS_TOKEN, \
    \or OPENROUTER_API_KEY, or place auth at ~/.grok/auth.json / ~/.codex/auth.json."
