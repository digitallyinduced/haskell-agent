-- | Shared gateway URL and same-origin validation.
module Agent.CLI.Gateway.Origin
    ( parseGatewayOrigin
    , parseGatewayResourceOrigin
    , validateBaseUrl
    , whenEither
    ) where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI qualified as URI
import Text.Read (readMaybe)

parseGatewayOrigin
    :: Text
    -> Text
    -> Either Text (Text, Text, Int)
parseGatewayOrigin errorMessage raw = do
    uri <-
        maybe (Left errorMessage) Right $
            URI.parseURI (Text.unpack (Text.strip raw))
    authority <-
        maybe (Left errorMessage) Right (URI.uriAuthority uri)
    let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
        host = Text.toLower (Text.pack (URI.uriRegName authority))
    whenEither
        ( not (null (URI.uriUserInfo authority))
            || Text.null host
            || not (null (URI.uriQuery uri))
            || not (null (URI.uriFragment uri))
        )
        errorMessage
    port <- gatewayOriginPort errorMessage scheme (URI.uriPort authority)
    pure (scheme, host, port)

parseGatewayResourceOrigin
    :: Text
    -> Text
    -> Either Text (Text, Text, Int)
parseGatewayResourceOrigin errorMessage raw = do
    uri <-
        maybe (Left errorMessage) Right $
            URI.parseURI (Text.unpack (Text.strip raw))
    authority <-
        maybe (Left errorMessage) Right (URI.uriAuthority uri)
    let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
        host = Text.toLower (Text.pack (URI.uriRegName authority))
    whenEither
        ( not (null (URI.uriUserInfo authority))
            || Text.null host
            || not (null (URI.uriFragment uri))
        )
        errorMessage
    port <- gatewayOriginPort errorMessage scheme (URI.uriPort authority)
    pure (scheme, host, port)

gatewayOriginPort :: Text -> Text -> String -> Either Text Int
gatewayOriginPort _ "https:" "" = Right 443
gatewayOriginPort _ "http:" "" = Right 80
gatewayOriginPort _ "wss:" "" = Right 443
gatewayOriginPort _ "ws:" "" = Right 80
gatewayOriginPort errorMessage scheme (':' : digits)
    | scheme `elem` ["https:", "http:", "wss:", "ws:"]
    , not (null digits)
    , all isDigit digits
    , Just port <- readMaybe digits
    , port > 0
    , port <= (65535 :: Int) =
        Right port
    | otherwise = Left errorMessage
gatewayOriginPort errorMessage _ _ = Left errorMessage

whenEither :: Bool -> Text -> Either Text ()
whenEither condition message
    | condition = Left message
    | otherwise = Right ()

validateBaseUrl :: Text -> Either Text Text
validateBaseUrl raw
    | Text.null base = Left "Gateway URL cannot be empty."
    | otherwise = do
        uri <- maybe
            (Left "Gateway URL is invalid.")
            Right
            (URI.parseURI (Text.unpack base))
        authority <- maybe
            (Left "Gateway URL must include a host.")
            Right
            (URI.uriAuthority uri)
        whenEither
            (null (URI.uriRegName authority))
            "Gateway URL must include a host."
        whenEither
            (not (null (URI.uriUserInfo authority))
                || not (null (URI.uriQuery uri))
                || not (null (URI.uriFragment uri)))
            "Gateway URL cannot contain user info, a query, or a fragment."
        case Text.toLower (Text.pack (URI.uriScheme uri)) of
            "https:" -> Right base
            "http:"
                | localHost (URI.uriRegName authority) -> Right base
            _ ->
                Left
                    "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
  where
    base = Text.dropWhileEnd (== '/') (Text.strip raw)
    localHost rawHost =
        Text.toLower (Text.pack rawHost)
            `elem` ["localhost", "127.0.0.1", "::1", "[::1]"]
    whenEither condition message
        | condition = Left message
        | otherwise = Right ()
