module Agent.OpenAI.Auth.JWT
    ( deriveAccountId
    , deriveEmail
    , needsRefresh
    , parseJwtExp
    , refreshMarginSeconds
    ) where

import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Auth.Types (AuthState(..))
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime)

-- | Refresh when the access-token JWT has less than this many seconds left.
refreshMarginSeconds :: Int
refreshMarginSeconds = 10 * 60

-- | Refresh only when the access-token JWT is about to expire.
needsRefresh :: AuthState -> UTCTime -> Bool
needsRefresh state now =
    if Text.null state.refreshToken
        then False
        else case parseJwtExp state.accessToken of
            Nothing    -> True
            Just expAt ->
                diffUTCTime expAt now < fromIntegral refreshMarginSeconds

-- | Parse the @exp@ claim from a JWT without signature verification.
parseJwtExp :: Text -> Maybe UTCTime
parseJwtExp token = do
    expAt <- decodeJwtClaims (Json.object (Json.atKey "exp" Json.int)) token
    let epoch = UTCTime (fromGregorian 1970 1 1) 0
    pure $ addUTCTime (fromIntegral expAt) epoch

-- | Derive the ChatGPT account id from an @id_token@ JWT.
deriveAccountId :: Text -> Maybe Text
deriveAccountId idTok = do
    decodeJwtClaims accountIdDecoder idTok

deriveEmail :: Text -> Maybe Text
deriveEmail token = do
    email <- decodeJwtClaims
        (Json.object (Json.atKey "email" Json.text))
        token
    if Text.null (Text.strip email)
        then Nothing
        else Just email

accountIdDecoder :: Json.Decoder Text
accountIdDecoder = Json.object $
    Json.atKey "https://api.openai.com/auth" $
        Json.object $
            Json.atKey "chatgpt_account_id" Json.text

decodeJwtClaims :: Json.Decoder value -> Text -> Maybe value
decodeJwtClaims decoder token = do
    payload <- case Text.splitOn "." token of
        (_header : encodedPayload : _) -> Just encodedPayload
        _ -> Nothing
    bytes <- either (const Nothing) Just $
        Base64.decode (Text.encodeUtf8 (base64UrlToBase64 payload))
    either (const Nothing) Just (Json.decodeEither decoder bytes)

base64UrlToBase64 :: Text -> Text
base64UrlToBase64 input =
    replaced <> Text.replicate paddingLength "="
  where
    replaced = Text.map replace input
    replace '-' = '+'
    replace '_' = '/'
    replace character = character
    paddingLength = (4 - Text.length replaced `mod` 4) `mod` 4
