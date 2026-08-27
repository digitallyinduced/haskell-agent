module Agent.OpenAI.Auth.JWT
    ( deriveAccountId
    , deriveEmail
    , needsRefresh
    , parseJwtExp
    , refreshMarginSeconds
    ) where

import Agent.Auth.JWT (decodeJwtPayload)
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Auth.Types (AuthState(..))
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
    expAt <- decodeJwtPayload (Json.object (Json.atKey "exp" Json.int)) token
    let epoch = UTCTime (fromGregorian 1970 1 1) 0
    pure $ addUTCTime (fromIntegral expAt) epoch

-- | Derive the ChatGPT account id from an @id_token@ JWT.
deriveAccountId :: Text -> Maybe Text
deriveAccountId idTok = do
    decodeJwtPayload accountIdDecoder idTok

deriveEmail :: Text -> Maybe Text
deriveEmail token = do
    email <- decodeJwtPayload
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

