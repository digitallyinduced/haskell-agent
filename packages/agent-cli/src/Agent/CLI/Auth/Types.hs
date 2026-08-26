module Agent.CLI.Auth.Types
    ( GrokAuthState(..)
    , LoadedAuth(..)
    , authStateToJson
    , credentialAccountLabel
    , credentialAccountLabelWith
    , externalAuthSelectionId
    , applyGrokAuthTokens
    , grokAuthStateFromJson
    , grokAuthStateToJson
    , grokCredentialFromAuthJson
    , grokEmailFromAuthJson
    , grokOAuthOptionsFromAuthJson
    , managedAuthSelectionId
    , nonEmptyText
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , textField
    , xaiOAuthClientId
    ) where

import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , providerSlug
    )
import qualified Agent.XAI.Auth as XAIAuth
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
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

openAIOAuthClientId :: Maybe Text -> Text
openAIOAuthClientId =
    fromMaybe "app_EMoamEEZ73f0CkXaXp7hrann"

xaiOAuthClientId :: Maybe Text -> Text
xaiOAuthClientId =
    fromMaybe "b1a00492-073a-47ea-816f-4c329264a828"

data LoadedAuth = LoadedAuth
    { loadedProvider :: !Provider
    , loadedTokenProvider :: !TokenProvider
    , loadedAccountLabel :: !(Credential -> IO Text)
    -- | Stable credential-source key used by the account picker.
    , loadedSelectionId :: !(Maybe Text)
    -- | Live OpenAI OAuth pool, when authentication uses one.
    , loadedOpenAiPool :: !(Maybe OpenAI.Pool)
    }

managedAuthSelectionId :: Text -> Text
managedAuthSelectionId managedId = "managed:" <> managedId

externalAuthSelectionId :: Provider -> Text -> Text
externalAuthSelectionId provider source =
    "external:" <> providerSlug provider <> ":" <> source

-- | Human-readable identity for the credential most recently selected by a
-- provider. Prefer an email claim, then fall back to a compact account id.
credentialAccountLabel :: Credential -> Text
credentialAccountLabel credential = case credential.provider of
    OpenAIProvider ->
        fromMaybe (fallback "ChatGPT") $
            OpenAI.deriveEmail credential.accessToken
    XAIProvider ->
        fromMaybe (fallback "Grok") $
            XAIAuth.emailFromToken credential.accessToken
    OpenRouterProvider ->
        fallback "OpenRouter"
    ClaudeCodeProvider ->
        fallback "Claude"
  where
    accountId = Text.strip credential.accountId
    fallback providerName
        | Text.null accountId = providerName
        | Text.length accountId <= 12 = accountId
        | otherwise = Text.take 8 accountId <> "…"

credentialAccountLabelWith :: Text -> Credential -> Text
credentialAccountLabelWith preferred credential =
    fromMaybe (credentialAccountLabel credential) $
        credentialEmail credential <|> nonEmptyText preferred

credentialEmail :: Credential -> Maybe Text
credentialEmail credential = case credential.provider of
    OpenAIProvider -> OpenAI.deriveEmail credential.accessToken
    XAIProvider -> XAIAuth.emailFromToken credential.accessToken
    OpenRouterProvider -> Nothing
    ClaudeCodeProvider -> Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null trimmed = Nothing
    | otherwise = Just trimmed
  where
    trimmed = Text.strip value

data GrokAuthState = GrokAuthState
    { grokAccessToken :: !Text, grokRefreshToken :: !(Maybe Text)
    , grokIdToken :: !(Maybe Text), grokExpiresAt :: !(Maybe UTCTime)
    }
    deriving (Eq)

instance Show GrokAuthState where
    show state =
        "GrokAuthState { grokAccessToken = <redacted>, grokRefreshToken = "
            <> maybe "Nothing" (const "Just <redacted>") state.grokRefreshToken
            <> ", grokIdToken = "
            <> maybe "Nothing" (const "Just <redacted>") state.grokIdToken
            <> ", grokExpiresAt = " <> show state.grokExpiresAt <> " }"

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

grokCredentialFromAuthJson :: Text -> Maybe Text
grokCredentialFromAuthJson raw =
    (.grokAccessToken) <$> grokAuthStateFromJson epoch raw
  where
    epoch = posixSecondsToUTCTime 0

grokAuthStateFromJson :: UTCTime -> Text -> Maybe GrokAuthState
grokAuthStateFromJson now raw = do
    value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    object <- authObject value
    grokAccessToken <-
        textField "key" object <|> textField "access_token" object
    let grokRefreshToken = textField "refresh_token" object
        grokIdToken = textField "id_token" object
        grokExpiresAt =
            utcTimeField "expires_at" object
                <|> ((`addUTCTime` now) . fromIntegral
                    <$> intField "expires_in" object)
                <|> OpenAI.parseJwtExp grokAccessToken
    pure GrokAuthState{..}

grokAuthStateToJson :: GrokAuthState -> Aeson.Value
grokAuthStateToJson state = Aeson.object
    [ "access_token" .= state.grokAccessToken
    , "refresh_token" .= state.grokRefreshToken
    , "id_token" .= state.grokIdToken
    , "expires_at" .= state.grokExpiresAt
    ]

-- | Patch rotated Grok tokens into an existing auth document, preserving the
-- grok CLI's nested map and any profile fields around the token object.
applyGrokAuthTokens :: GrokAuthState -> Aeson.Value -> Maybe Aeson.Value
applyGrokAuthTokens state = \case
    Aeson.Object object
        | hasGrokAccessToken object ->
            Just (Aeson.Object (updateGrokAuthObject state object))
        | otherwise ->
            let updated = KeyMap.map updateNested object
            in if updated == object
                then Nothing
                else Just (Aeson.Object updated)
    _ -> Nothing
  where
    updateNested = \case
        Aeson.Object nestedObject | hasGrokAccessToken nestedObject ->
            Aeson.Object (updateGrokAuthObject state nestedObject)
        nested -> nested

updateGrokAuthObject :: GrokAuthState -> Aeson.Object -> Aeson.Object
updateGrokAuthObject state object =
    insertOptional "expires_at" (Aeson.toJSON <$> state.grokExpiresAt)
        . insertOptional "id_token" (Aeson.String <$> state.grokIdToken)
        . insertOptional "refresh_token" (Aeson.String <$> state.grokRefreshToken)
        . insertAccess
        $ object
  where
    insertAccess current
        | KeyMap.member keyKey current =
            KeyMap.insert keyKey (Aeson.String state.grokAccessToken)
                (if KeyMap.member accessKey current
                    then KeyMap.insert accessKey
                        (Aeson.String state.grokAccessToken) current
                    else current)
        | otherwise =
            KeyMap.insert accessKey (Aeson.String state.grokAccessToken) current
    insertOptional _ Nothing current = current
    insertOptional name (Just value) current =
        KeyMap.insert (Key.fromText name) value current
    keyKey = Key.fromText "key"
    accessKey = Key.fromText "access_token"

-- | Build the xAI OAuth client used to refresh grok CLI / managed JSON.
-- Nested grok CLI documents carry @oidc_client_id@ and optional team
-- principal fields that must be echoed on refresh.
grokOAuthOptionsFromAuthJson :: Text -> Text -> XAIAuth.OAuthOptions
grokOAuthOptionsFromAuthJson defaultClientId raw =
    case Aeson.decodeStrict (TextEncoding.encodeUtf8 raw) >>= authObject of
        Nothing -> XAIAuth.defaultOAuthOptions defaultClientId
        Just object ->
            let clientId =
                    fromMaybe defaultClientId
                        (textField "oidc_client_id" object)
                options = XAIAuth.defaultOAuthOptions clientId
            in options
                { XAIAuth.principalType = textField "principal_type" object
                , XAIAuth.principalId = textField "principal_id" object
                }

authObject :: Aeson.Value -> Maybe Aeson.Object
authObject = \case
    Aeson.Object object
        | hasGrokAccessToken object -> Just object
        | otherwise ->
            listToMaybe
                [ nestedObject
                | Aeson.Object nestedObject <- KeyMap.elems object
                , hasGrokAccessToken nestedObject
                ]
    _ -> Nothing

hasGrokAccessToken :: Aeson.Object -> Bool
hasGrokAccessToken object =
    isJust (textField "key" object <|> textField "access_token" object)

utcTimeField :: Text -> Aeson.Object -> Maybe UTCTime
utcTimeField name object =
    KeyMap.lookup (Key.fromText name) object >>= \value ->
        case Aeson.fromJSON value of
            Aeson.Success time -> Just time
            Aeson.Error _ -> case value of
                Aeson.Number seconds ->
                    Just (posixSecondsToUTCTime (realToFrac seconds))
                _ -> Nothing

intField :: Text -> Aeson.Object -> Maybe Int
intField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Number value) -> Just (floor value)
    _ -> Nothing

grokEmailFromAuthJson :: Text -> Maybe Text
grokEmailFromAuthJson raw = do
    value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    entryEmail value <|> firstNestedEmail value
  where
    entryEmail (Aeson.Object object) =
        textField "email" object
            <|> (textField "id_token" object >>= XAIAuth.emailFromToken)
            <|> (textField "access_token" object >>= XAIAuth.emailFromToken)
            <|> (textField "key" object >>= XAIAuth.emailFromToken)
    entryEmail _ = Nothing

    firstNestedEmail (Aeson.Object object) =
        listToMaybe
            [ email
            | nested <- KeyMap.elems object
            , Just email <- [entryEmail nested]
            ]
    firstNestedEmail _ = Nothing

textField :: Text -> Aeson.Object -> Maybe Text
textField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) | not (Text.null value) -> Just value
    _ -> Nothing
