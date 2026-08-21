module Agent.OpenAI.Auth.JWT
    ( deriveAccountId
    , deriveEmail
    , needsRefresh
    , parseJwtExp
    , refreshMarginSeconds
    ) where

import Agent.OpenAI.Auth.Types (AuthState(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
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
    case parseJwtExp state.accessToken of
        Nothing    -> True
        Just expAt -> diffUTCTime expAt now < fromIntegral refreshMarginSeconds

-- | Parse the @exp@ claim from a JWT without signature verification.
parseJwtExp :: Text -> Maybe UTCTime
parseJwtExp token = do
    payloadB64 <- Text.splitOn "." token !!? 1
    payloadBytes <- either (const Nothing) Just
        (Base64.decode (Text.encodeUtf8 (base64UrlToBase64 payloadB64)))
    obj <- Aeson.decode (LBS.fromStrict payloadBytes)
    expAt <- jsonIntMaybe obj "exp"
    let epoch = UTCTime (fromGregorian 1970 1 1) 0
    pure $ addUTCTime (fromIntegral expAt) epoch

-- | Derive the ChatGPT account id from an @id_token@ JWT.
deriveAccountId :: Text -> Maybe Text
deriveAccountId idTok = do
    km <- jwtClaims idTok
    Aeson.Object authKm <- KeyMap.lookup "https://api.openai.com/auth" km
    Aeson.String accId <- KeyMap.lookup "chatgpt_account_id" authKm
    pure accId

deriveEmail :: Text -> Maybe Text
deriveEmail token = do
    claims <- jwtClaims token
    Aeson.String email <- KeyMap.lookup "email" claims
    if Text.null (Text.strip email)
        then Nothing
        else Just email

jwtClaims :: Text -> Maybe Aeson.Object
jwtClaims token = do
    payloadB64 <- Text.splitOn "." token !!? 1
    payloadBytes <- either (const Nothing) Just
        (Base64.decode (Text.encodeUtf8 (base64UrlToBase64 payloadB64)))
    Aeson.Object claims <- Aeson.decode (LBS.fromStrict payloadBytes)
    pure claims

base64UrlToBase64 :: Text -> Text
base64UrlToBase64 text =
    let replaced = Text.replace "-" "+" (Text.replace "_" "/" text)
        padLen = (4 - Text.length replaced `mod` 4) `mod` 4
    in replaced <> Text.replicate padLen "="

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs index
    | index < 0 = Nothing
    | otherwise = go xs index
  where
    go [] _ = Nothing
    go (x:_) 0 = Just x
    go (_:rest) n = go rest (n - 1)

jsonIntMaybe :: Aeson.Value -> Text -> Maybe Int
jsonIntMaybe (Aeson.Object object) key =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.Number n) -> Just (round n)
        _ -> Nothing
jsonIntMaybe _ _ = Nothing
