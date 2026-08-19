-- | Load ChatGPT, Grok, OpenRouter, or broker credentials for the CLI process.
module Agent.CLI.Auth
    ( LoadedAuth(..)
    , grokCredentialFromAuthJson
    , loadAuth
    , openaiAuthStateFromJson
    ) where

import Agent.Broker (BrokerOptions(..), newBrokerTokenProvider)
import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
import qualified Agent.OpenAI.Login as OpenAILogin
import Agent.Provider
    ( Credential(..)
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
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

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
            provider <- newBrokerTokenProvider BrokerOptions
                { baseUrl = Text.unpack url
                , serviceToken
                }
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
    credential <- loadGrokCredential
    case credential of
        Nothing -> pure (Left noAuthHint)
        Just loaded -> pure $ Right LoadedAuth
            { loadedProvider = XAIProvider
            , loadedTokenProvider = staticCredentialProvider loaded
            }

loadOpenRouter :: IO (Either String LoadedAuth)
loadOpenRouter = do
    key <- lookupNonEmpty "OPENROUTER_API_KEY"
    case key of
        Nothing -> pure (Left noAuthHint)
        Just apiKey -> pure $ Right LoadedAuth
            { loadedProvider = OpenRouterProvider
            , loadedTokenProvider = staticCredentialProvider (credentialFromApiKey apiKey)
            }

loadOpenAi :: IO (Either String LoadedAuth)
loadOpenAi = do
    fromEnvToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    fromEnvJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    home <- getHomeDirectory
    let filePath = home </> ".codex" </> "auth.json"
    fileExists <- doesFileExist filePath
    fileBytes <- if fileExists then Just <$> LBS.readFile filePath else pure Nothing
    now <- getCurrentTime
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
            Just state -> do
                clientId <- lookupNonEmpty "OPENAI_OAUTH_CLIENT_ID"
                let refresh stale = case clientId of
                        Nothing -> pure $ Left $ ProviderError AuthenticationError
                            "OPENAI_OAUTH_CLIENT_ID is required to refresh ~/.codex/auth.json"
                            Nothing
                        Just oauthClientId ->
                            OpenAI.refreshAccessTokenHTTP oauthClientId stale >>= \case
                                Left err -> pure (Left err)
                                Right newState
                                    | fileExists -> do
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
    let filePath = home </> ".grok" </> "auth.json"
    fileExists <- doesFileExist filePath
    fileJson <- if fileExists
        then Just . TextEncoding.decodeUtf8 . LBS.toStrict <$> LBS.readFile filePath
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
    file <- doesFileExist (home </> ".grok" </> "auth.json")
    pure (isJust envJson || isJust envToken || file)

hasOpenAiAuth :: IO Bool
hasOpenAiAuth = do
    envJson <- lookupNonEmpty "CODEX_AUTH_JSON"
    envToken <- lookupNonEmpty "CODEX_ACCESS_TOKEN"
    home <- getHomeDirectory
    file <- doesFileExist (home </> ".codex" </> "auth.json")
    pure (isJust envJson || isJust envToken || file)

hasOpenRouterAuth :: IO Bool
hasOpenRouterAuth = isJust <$> lookupNonEmpty "OPENROUTER_API_KEY"

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
