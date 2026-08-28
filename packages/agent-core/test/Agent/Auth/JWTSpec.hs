module Agent.Auth.JWTSpec (spec) where

import Agent.Auth.JWT (decodeJwtPayload)
import qualified Agent.Json.Decode as Json
import qualified Data.Aeson as Aeson
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "decodeJwtPayload" do
    it "decodes an unpadded base64url JSON payload" do
        decodeJwtPayload
            (Json.object (Json.atKey "account_id" Json.text))
            (unsignedJwt (Aeson.object
            [ "account_id" Aeson..= ("account-123" :: Text) ]))
            `shouldBe` Just "account-123"

    it "supports typed payload decoding" do
        decodeJwtPayload Json.text (unsignedJwt (Aeson.String "claims")) `shouldBe`
            Just ("claims" :: Text)

    it "rejects missing, malformed, or non-JSON payloads" do
        decodeJwtPayload Json.text "" `shouldBe` Nothing
        decodeJwtPayload Json.text "one-segment" `shouldBe` Nothing
        decodeJwtPayload Json.text "header.%%%.signature"
            `shouldBe` Nothing
        decodeJwtPayload Json.text "header.bm90LWpzb24.signature"
            `shouldBe` Nothing

unsignedJwt :: Aeson.Value -> Text
unsignedJwt payload =
    "header." <> encodePayload payload <> ".signature"

encodePayload :: Aeson.Value -> Text
encodePayload =
    Text.decodeUtf8
        . BS.filter (/= padding)
        . BS.map urlSafe
        . Base64.encode
        . LBS.toStrict
        . Aeson.encode
  where
    padding = 0x3d
    urlSafe 0x2b = 0x2d
    urlSafe 0x2f = 0x5f
    urlSafe byte = byte
