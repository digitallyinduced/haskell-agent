module Agent.OpenAI.Auth.JWT
    ( deriveAccountId
    , deriveEmail
    , needsRefresh
    , parseJwtExp
    , refreshMarginSeconds
    ) where

import Agent.Auth.JWT (decodeJwtPayload)
import Agent.OpenAI.Auth.Types (AuthState(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
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
    obj <- decodeJwtPayload token
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
    Aeson.Object claims <- decodeJwtPayload token
    pure claims

jsonIntMaybe :: Aeson.Value -> Text -> Maybe Int
jsonIntMaybe (Aeson.Object object) key =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.Number n) -> Just (round n)
        _ -> Nothing
jsonIntMaybe _ _ = Nothing
