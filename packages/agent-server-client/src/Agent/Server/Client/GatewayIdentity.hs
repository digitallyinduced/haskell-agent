-- | Stable gateway credential identity shared by runtimes and API clients.
module Agent.Server.Client.GatewayIdentity
    ( GatewayCredential (..)
    , gatewayCredentialIdentity
    )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (String)
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Bits ((.|.), shiftL)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.URI qualified as URI
import Text.Read (readMaybe)

{- | Private gateway connection material.

The custom 'Show' instance never renders the bearer.
-}
data GatewayCredential = GatewayCredential
    { gatewayBaseUrl :: !Text
    , gatewayWebSocketUrl :: !Text
    , gatewayAccessToken :: !Text
    }
    deriving (Eq)

instance Show GatewayCredential where
    show credential =
        "GatewayCredential { gatewayBaseUrl = "
            <> show credential.gatewayBaseUrl
            <> ", gatewayWebSocketUrl = "
            <> show credential.gatewayWebSocketUrl
            <> ", gatewayAccessToken = <redacted> }"

instance FromJSON GatewayCredential where
    parseJSON = withObject "GatewayCredential" \value ->
        GatewayCredential
            <$> value .: "base_url"
            <*> value .: "websocket_url"
            <*> value .: "access_token"

instance ToJSON GatewayCredential where
    toJSON credential =
        object
            [ "version" .= (1 :: Int)
            , "base_url" .= credential.gatewayBaseUrl
            , "websocket_url" .= credential.gatewayWebSocketUrl
            , "access_token" .= credential.gatewayAccessToken
            ]

{- | Stable, non-secret binding for one exact gateway credential.

Reauthorization deliberately produces a different identity: without a
gateway-issued organization identifier, treating a replacement bearer as
the same routing boundary could send prior context to another organization.
-}
gatewayCredentialIdentity :: GatewayCredential -> Text
gatewayCredentialIdentity credential =
    "gateway-sha256:"
        <> TextEncoding.decodeUtf8
            ( Base64Url.encodeUnpadded
                (digestBytes (hash material :: Digest SHA256))
            )
  where
    fields =
        [ canonicalGatewayIdentityUrl
            ""
            credential.gatewayBaseUrl
        , canonicalGatewayIdentityUrl
            "/v1/responses"
            credential.gatewayWebSocketUrl
        , credential.gatewayAccessToken
        ]
    material
        -- Preserve identities produced by existing installations for valid
        -- credentials. Invalid NUL-bearing material uses explicit JSON
        -- framing so distinct fields cannot alias.
        | any (Text.any (== '\NUL')) fields =
            LazyByteString.toStrict . encode $
                String "haskell-agent gateway session binding v2"
                    : map String fields
        | otherwise =
            TextEncoding.encodeUtf8 $
                Text.intercalate
                    "\NUL"
                    ( "haskell-agent gateway session binding v1"
                        : fields
                    )

digestBytes :: Digest SHA256 -> ByteString.ByteString
digestBytes = ByteString.pack . decodeHex . show
  where
    decodeHex (high : low : rest) =
        fromIntegral
            (digitToInt high `shiftL` 4 .|. digitToInt low)
            : decodeHex rest
    decodeHex [] = []
    decodeHex _ = error "SHA-256 digest rendered an odd number of hex digits"

canonicalGatewayIdentityUrl :: Text -> Text -> Text
canonicalGatewayIdentityUrl defaultPath raw =
    case URI.parseURI (Text.unpack (Text.strip raw)) of
        Just uri
            | Just authority <- URI.uriAuthority uri
            , let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
            , Right port <-
                gatewayOriginPort scheme (URI.uriPort authority) ->
                let host =
                        Text.toLower (Text.pack (URI.uriRegName authority))
                    userInfo = Text.pack (URI.uriUserInfo authority)
                    rawPath = Text.pack (URI.uriPath uri)
                    path
                        | Text.null rawPath = defaultPath
                        | Text.null defaultPath =
                            Text.dropWhileEnd (== '/') rawPath
                        | otherwise = rawPath
                 in scheme
                        <> "//"
                        <> userInfo
                        <> host
                        <> ":"
                        <> Text.pack (show port)
                        <> path
                        <> Text.pack (URI.uriQuery uri)
                        <> Text.pack (URI.uriFragment uri)
        _ -> Text.strip raw

gatewayOriginPort :: Text -> String -> Either Text Int
gatewayOriginPort "https:" "" = Right 443
gatewayOriginPort "http:" "" = Right 80
gatewayOriginPort "wss:" "" = Right 443
gatewayOriginPort "ws:" "" = Right 80
gatewayOriginPort scheme (':' : digits)
    | scheme `elem` ["https:", "http:", "wss:", "ws:"]
    , not (null digits)
    , all isDigit digits
    , Just port <- readMaybe digits
    , port > 0
    , port <= (65535 :: Int) =
        Right port
gatewayOriginPort _ _ = Left "invalid gateway identity URL"
