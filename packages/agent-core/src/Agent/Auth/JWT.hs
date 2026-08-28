-- | Provider-neutral helpers for reading unverified JWT payloads.
--
-- These helpers decode claims for metadata such as account identity and token
-- expiry. They do not verify signatures and must not be used for authorization.
module Agent.Auth.JWT
    ( decodeJwtPayload
    ) where

import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Decode the JSON payload (the second dot-separated segment) of a JWT
-- without verifying its signature.
decodeJwtPayload :: Decoder value -> Text -> Maybe value
decodeJwtPayload decoder token = do
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
