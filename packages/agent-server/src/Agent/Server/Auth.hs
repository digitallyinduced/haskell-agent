-- | Host, bearer-token, tenant, and explicit-origin checks.
module Agent.Server.Auth
    ( AuthMode(..)
    , TenantCredential
    , tenantCredential
    , AuthConfig(..)
    , AuthenticatedRequest(..)
    , AuthFailure(..)
    , authorizeRequest
    , authorizePreflight
    , corsResponseHeaders
    , isCorsPreflight
    ) where

import Agent.Server.Types
    ( Principal
    , localPrincipal
    )
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (constEq, convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
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
    | TenantBearerAuth ![TenantCredential]

-- Deliberately omit Show: this carries a digest derived from a secret.
data TenantCredential = TenantCredential
    { credentialPrincipal :: !Principal
    , credentialDigest :: !ByteString
    }

tenantCredential :: Principal -> ByteString -> TenantCredential
tenantCredential principal token = TenantCredential
    { credentialPrincipal = principal
    , credentialDigest = tokenDigest token
    }

-- Deliberately omit Show: a local remote-mode value contains a bearer secret.
data AuthConfig = AuthConfig
    { authMode :: !AuthMode
    , authCorsOrigins :: !(Set ByteString)
    }

data AuthenticatedRequest = AuthenticatedRequest
    { authenticatedPrincipal :: !Principal
    , authenticatedCorsHeaders :: ![Header]
    }
    deriving (Eq, Show)

data AuthFailure = AuthFailure
    { authFailureStatus :: !Int
    , authFailureCode :: !Text
    , authFailureMessage :: !Text
    }
    deriving (Eq, Show)

authorizeRequest
    :: AuthConfig
    -> Request
    -> Either AuthFailure AuthenticatedRequest
authorizeRequest config request = do
    authorizeOrigin config request
    principal <- authorizeMode config.authMode request
    pure AuthenticatedRequest
        { authenticatedPrincipal = principal
        , authenticatedCorsHeaders = corsResponseHeaders config request
        }

-- | CORS preflights carry no bearer. They reveal no application data, but
-- still require an explicitly allowed Origin and a valid loopback Host when
-- loopback Host authentication is selected.
authorizePreflight
    :: AuthConfig
    -> Request
    -> Either AuthFailure [Header]
authorizePreflight config request = do
    authorizeOrigin config request
    case lookup "Origin" request.requestHeaders of
        Nothing -> Left AuthFailure
            { authFailureStatus = 403
            , authFailureCode = "origin_not_allowed"
            , authFailureMessage = "a CORS preflight requires an Origin"
            }
        Just origin
            | origin `Set.member` config.authCorsOrigins -> Right ()
            | otherwise -> Left AuthFailure
                { authFailureStatus = 403
                , authFailureCode = "origin_not_allowed"
                , authFailureMessage = "the request Origin is not allowed"
                }
    case config.authMode of
        LoopbackHostAuth allowedHosts ->
            () <$ authorizeMode (LoopbackHostAuth allowedHosts) request
        BearerTokenAuth _ -> Right ()
        TenantBearerAuth _ -> Right ()
    pure (corsResponseHeaders config request)

authorizeMode :: AuthMode -> Request -> Either AuthFailure Principal
authorizeMode mode request = case mode of
    LoopbackHostAuth allowedHosts ->
        case lookup "Host" request.requestHeaders of
            Just host
                | asciiLower host `Set.member` allowedHosts ->
                    Right localPrincipal
            _ -> Left AuthFailure
                { authFailureStatus = 403
                , authFailureCode = "invalid_host"
                , authFailureMessage =
                    "the Host header is not valid for this loopback server"
                }
    BearerTokenAuth expected ->
        case bearerToken request of
            Just supplied
                | secureTokenEqual supplied expected ->
                    Right localPrincipal
            _ -> unauthorized
    TenantBearerAuth credentials ->
        case bearerToken request >>= matchingPrincipal credentials of
            Just principal -> Right principal
            Nothing -> unauthorized
  where
    unauthorized = Left AuthFailure
        { authFailureStatus = 401
        , authFailureCode = "unauthorized"
        , authFailureMessage = "a valid bearer token is required"
        }

bearerToken :: Request -> Maybe ByteString
bearerToken request =
    lookup hAuthorization request.requestHeaders
        >>= ByteString.stripPrefix "Bearer "

matchingPrincipal
    :: [TenantCredential]
    -> ByteString
    -> Maybe Principal
matchingPrincipal credentials supplied =
    snd $
        foldl'
            select
            (tokenDigest supplied, Nothing)
            credentials
  where
    -- Perform every digest comparison even after a match. Registry validation
    -- separately rejects duplicate credential material.
    select (suppliedDigest, matched) credential =
        ( suppliedDigest
        , if constEq suppliedDigest credential.credentialDigest
            then Just credential.credentialPrincipal
            else matched
        )

authorizeOrigin :: AuthConfig -> Request -> Either AuthFailure ()
authorizeOrigin config request =
    case lookup "Origin" request.requestHeaders of
        Nothing -> Right ()
        Just origin
            | origin `Set.member` config.authCorsOrigins -> Right ()
            | otherwise -> Left AuthFailure
                { authFailureStatus = 403
                , authFailureCode = "origin_not_allowed"
                , authFailureMessage = "the request Origin is not allowed"
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
    constEq (tokenDigest left) (tokenDigest right)

tokenDigest :: ByteString -> ByteString
tokenDigest value =
    convert (hash value :: Digest SHA256)

asciiLower :: ByteString -> ByteString
asciiLower = ByteString8.map \character ->
    if character >= 'A' && character <= 'Z'
        then toEnum (fromEnum character + 32)
        else character
