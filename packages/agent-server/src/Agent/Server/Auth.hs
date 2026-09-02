-- | Host, bearer-token, and explicit-origin checks for the HTTP boundary.
module Agent.Server.Auth
    ( AuthMode(..)
    , AuthConfig(..)
    , AuthFailure(..)
    , authorizeRequest
    , corsResponseHeaders
    , isCorsPreflight
    ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (constEq)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Network.HTTP.Types
    ( Header
    , hAuthorization
    )
import Network.Wai
    ( Request
    , requestHeaders
    , requestMethod
    )

data AuthMode
    = LoopbackHostAuth !(Set ByteString)
    | BearerTokenAuth !ByteString

-- Deliberately omit Show: a remote-mode value contains the bearer secret.
data AuthConfig = AuthConfig
    { authMode :: !AuthMode
    , authCorsOrigins :: !(Set ByteString)
    }

data AuthFailure = AuthFailure
    { authFailureStatus :: !Int
    , authFailureCode :: !Text
    , authFailureMessage :: !Text
    }
    deriving (Eq, Show)

authorizeRequest :: AuthConfig -> Request -> Either AuthFailure [Header]
authorizeRequest config request = do
    authorizeOrigin config request
    authorizeMode config.authMode request
    pure (corsResponseHeaders config request)

authorizeMode :: AuthMode -> Request -> Either AuthFailure ()
authorizeMode mode request = case mode of
    LoopbackHostAuth allowedHosts ->
        case lookup "Host" request.requestHeaders of
            Just host
                | asciiLower host `Set.member` allowedHosts -> Right ()
            _ -> Left AuthFailure
                { authFailureStatus = 403
                , authFailureCode = "invalid_host"
                , authFailureMessage =
                    "the Host header is not valid for this loopback server"
                }
    BearerTokenAuth expected ->
        case lookup hAuthorization request.requestHeaders of
            Just header
                | Just supplied <- BS.stripPrefix "Bearer " header
                , secureTokenEqual supplied expected ->
                    Right ()
            _ -> Left AuthFailure
                { authFailureStatus = 401
                , authFailureCode = "unauthorized"
                , authFailureMessage =
                    "a valid bearer token is required"
                }

authorizeOrigin :: AuthConfig -> Request -> Either AuthFailure ()
authorizeOrigin config request =
    case lookup "Origin" request.requestHeaders of
        Nothing -> Right ()
        Just origin
            | origin `Set.member` config.authCorsOrigins -> Right ()
            | otherwise -> Left AuthFailure
                { authFailureStatus = 403
                , authFailureCode = "origin_not_allowed"
                , authFailureMessage =
                    "the request Origin is not allowed"
                }

corsResponseHeaders :: AuthConfig -> Request -> [Header]
corsResponseHeaders config request =
    case lookup "Origin" request.requestHeaders of
        Just origin
            | origin `Set.member` config.authCorsOrigins ->
                [ ("Access-Control-Allow-Origin", origin)
                , ("Vary", "Origin")
                , ("Access-Control-Allow-Methods",
                    "GET, POST, PATCH, DELETE, OPTIONS")
                , ("Access-Control-Allow-Headers",
                    "Authorization, Content-Type, Last-Event-ID")
                , ("Access-Control-Max-Age", "600")
                ]
        _ -> []

isCorsPreflight :: Request -> Bool
isCorsPreflight request =
    request.requestMethod == "OPTIONS"
        && lookup "Origin" request.requestHeaders /= Nothing

secureTokenEqual :: ByteString -> ByteString -> Bool
secureTokenEqual left right =
    let leftDigest = hash left :: Digest SHA256
        rightDigest = hash right :: Digest SHA256
    in constEq leftDigest rightDigest

asciiLower :: ByteString -> ByteString
asciiLower = BS8.map \char ->
    if char >= 'A' && char <= 'Z'
        then toEnum (fromEnum char + 32)
        else char
