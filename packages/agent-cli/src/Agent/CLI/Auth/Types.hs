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
    , grokAuthStateToJsonWithKnownFields
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
import Agent.Json.Decode (optionalKey)
import Agent.Json.Decode qualified as Hermes
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isJust)
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

data GrokFields = GrokFields
    { grokFieldKey :: !(Maybe Text)
    , grokFieldAccessToken :: !(Maybe Text)
    , grokFieldRefreshToken :: !(Maybe Text)
    , grokFieldIdToken :: !(Maybe Text)
    , grokFieldExpiresAt :: !(Maybe UTCTime)
    , grokFieldExpiresIn :: !(Maybe Int)
    , grokFieldClientId :: !(Maybe Text)
    , grokFieldPrincipalType :: !(Maybe Text)
    , grokFieldPrincipalId :: !(Maybe Text)
    , grokFieldEmail :: !(Maybe Text)
    }

grokFieldsDecoder :: Hermes.Decoder GrokFields
grokFieldsDecoder = Hermes.object grokFieldsFieldsDecoder

grokFieldsFieldsDecoder :: Hermes.FieldsDecoder GrokFields
grokFieldsFieldsDecoder =
    GrokFields
        <$> optionalKey "key" Hermes.text
        <*> optionalKey "access_token" Hermes.text
        <*> optionalKey "refresh_token" Hermes.text
        <*> optionalKey "id_token" Hermes.text
        <*> optionalKey "expires_at" timestampDecoder
        <*> optionalKey "expires_in" Hermes.int
        <*> optionalKey "oidc_client_id" Hermes.text
        <*> optionalKey "principal_type" Hermes.text
        <*> optionalKey "principal_id" Hermes.text
        <*> optionalKey "email" Hermes.text

grokDocumentDecoder :: Hermes.Decoder GrokFields
grokDocumentDecoder = Hermes.object do
    direct <- grokFieldsFieldsDecoder
    nested <- Hermes.liftObjectDecoder $
        Hermes.objectFold Nothing \_ found ->
            Hermes.withOwnedRawJson \raw ->
                pure $ found <|> validGrokFields
                    (Hermes.decodeEither grokFieldsDecoder raw)
    case validFields direct <|> nested of
        Just fields -> pure fields
        Nothing -> fail "authentication object has no access token"
  where
    validGrokFields = either (const Nothing) validFields

validFields :: GrokFields -> Maybe GrokFields
validFields fields
    | isJust (fields.grokFieldKey <|> fields.grokFieldAccessToken) =
        Just fields
    | otherwise = Nothing

timestampDecoder :: Hermes.Decoder UTCTime
timestampDecoder =
    Hermes.withOwnedRawJson \raw ->
        case Hermes.decodeEither Hermes.utcTime raw of
            Right value -> pure value
            Left _ -> case Hermes.decodeEither Hermes.scientific raw of
                Right seconds ->
                    pure (posixSecondsToUTCTime (realToFrac seconds))
                Left _ -> fail "expected an ISO timestamp or epoch seconds"

openaiAuthStateFromJson :: UTCTime -> LBS.ByteString -> Maybe OpenAI.AuthState
openaiAuthStateFromJson now bytes = do
    OpenAiTokens{..} <- either (const Nothing) Just $
        Hermes.decodeEither openAiTokensDocumentDecoder (LBS.toStrict bytes)
    Just OpenAI.AuthState
        { accessToken
        , refreshToken = fromMaybe "" refreshToken
        , accountId
        , idToken
        , lastRefresh = now
        }

data OpenAiTokens = OpenAiTokens
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , accountId :: !Text
    , idToken :: !(Maybe Text)
    }

openAiTokensDecoder :: Hermes.Decoder OpenAiTokens
openAiTokensDecoder = Hermes.object $
    OpenAiTokens
        <$> Hermes.atKey "access_token" Hermes.text
        <*> optionalKey "refresh_token" Hermes.text
        <*> Hermes.atKey "account_id" Hermes.text
        <*> optionalKey "id_token" Hermes.text

openAiTokensDocumentDecoder :: Hermes.Decoder OpenAiTokens
openAiTokensDocumentDecoder = Hermes.withType \case
    Hermes.VObject -> Hermes.object do
        nested <- optionalKey "tokens" openAiTokensDecoder
        case nested of
            Just tokens -> pure tokens
            Nothing -> Hermes.liftObjectDecoder openAiTokensDecoder
    Hermes.VArray -> do
        values <- Hermes.list openAiTokensDocumentDecoder
        case values of
            first : _ -> pure first
            [] -> fail "authentication array is empty"
    _ -> fail "expected an authentication object or array"

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
    fields <- either (const Nothing) Just $
        Hermes.decodeEither grokDocumentDecoder (TextEncoding.encodeUtf8 raw)
    grokAccessToken <- fields.grokFieldKey <|> fields.grokFieldAccessToken
    let grokRefreshToken = fields.grokFieldRefreshToken
        grokIdToken = fields.grokFieldIdToken
        grokExpiresAt =
            fields.grokFieldExpiresAt
                <|> ((`addUTCTime` now) . fromIntegral <$> fields.grokFieldExpiresIn)
                <|> OpenAI.parseJwtExp grokAccessToken
    pure GrokAuthState{..}

grokAuthStateToJson :: GrokAuthState -> Aeson.Value
grokAuthStateToJson state = Aeson.object
    [ "access_token" .= state.grokAccessToken
    , "refresh_token" .= state.grokRefreshToken
    , "id_token" .= state.grokIdToken
    , "expires_at" .= state.grokExpiresAt
    ]

-- | Normalize a supported flat or one-level nested Grok auth document while
-- retaining the known non-token profile fields used during refresh.
grokAuthStateToJsonWithKnownFields :: Text -> GrokAuthState -> Aeson.Value
grokAuthStateToJsonWithKnownFields original state =
    Aeson.object $
        [ "access_token" .= state.grokAccessToken
        , "refresh_token" .= state.grokRefreshToken
        , "id_token" .= state.grokIdToken
        , "expires_at" .= state.grokExpiresAt
        ]
        <> maybe [] (\value -> ["email" .= value]) (fields >>= (.grokFieldEmail))
        <> maybe [] (\value -> ["oidc_client_id" .= value])
            (fields >>= (.grokFieldClientId))
        <> maybe [] (\value -> ["principal_type" .= value])
            (fields >>= (.grokFieldPrincipalType))
        <> maybe [] (\value -> ["principal_id" .= value])
            (fields >>= (.grokFieldPrincipalId))
  where
    fields = either (const Nothing) Just $
        Hermes.decodeEither grokDocumentDecoder (TextEncoding.encodeUtf8 original)

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
    case Hermes.decodeEither grokDocumentDecoder (TextEncoding.encodeUtf8 raw) of
        Left _ -> XAIAuth.defaultOAuthOptions defaultClientId
        Right fields ->
            let clientId =
                    fromMaybe defaultClientId
                        fields.grokFieldClientId
                options = XAIAuth.defaultOAuthOptions clientId
            in options
                { XAIAuth.principalType = fields.grokFieldPrincipalType
                , XAIAuth.principalId = fields.grokFieldPrincipalId
                }

hasGrokAccessToken :: Aeson.Object -> Bool
hasGrokAccessToken object =
    isJust (textField "key" object <|> textField "access_token" object)

grokEmailFromAuthJson :: Text -> Maybe Text
grokEmailFromAuthJson raw =
    either (const Nothing) id $
        Hermes.decodeEither grokEmailDocumentDecoder
            (TextEncoding.encodeUtf8 raw)

-- Email lookup deliberately does not require the same object to contain an
-- access token: grok's auth map may keep profile data in a sibling entry.
grokEmailDocumentDecoder :: Hermes.Decoder (Maybe Text)
grokEmailDocumentDecoder =
    Hermes.object do
        fields <- grokFieldsFieldsDecoder
        nested <- Hermes.liftObjectDecoder $
            Hermes.objectFold Nothing \_ found ->
                Hermes.withOwnedRawJson \raw ->
                    pure $ found <|>
                        either (const Nothing) id
                            (Hermes.decodeEither
                                grokEmailDocumentDecoder
                                raw)
        pure $
            fields.grokFieldEmail
                <|> (fields.grokFieldIdToken >>= XAIAuth.emailFromToken)
                <|> (fields.grokFieldAccessToken >>= XAIAuth.emailFromToken)
                <|> (fields.grokFieldKey >>= XAIAuth.emailFromToken)
                <|> nested

textField :: Text -> Aeson.Object -> Maybe Text
textField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) | not (Text.null value) -> Just value
    _ -> Nothing
