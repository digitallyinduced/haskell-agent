module Agent.Auth.JWTSpec (spec) where

import Agent.Auth.JWT (decodeJwtPayload)
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
        decodeJwtPayload (unsignedJwt (Aeson.object
            [ "account_id" Aeson..= ("account-123" :: Text) ]))
            `shouldBe`
                Just (Aeson.object
                    [ "account_id" Aeson..= ("account-123" :: Text) ])

    it "supports typed payload decoding" do
        decodeJwtPayload (unsignedJwt (Aeson.String "claims")) `shouldBe`
            Just ("claims" :: Text)

    it "rejects missing, malformed, or non-JSON payloads" do
        (decodeJwtPayload "" :: Maybe Aeson.Value) `shouldBe` Nothing
        (decodeJwtPayload "one-segment" :: Maybe Aeson.Value) `shouldBe` Nothing
        (decodeJwtPayload "header.%%%.signature" :: Maybe Aeson.Value)
            `shouldBe` Nothing
        (decodeJwtPayload "header.bm90LWpzb24.signature" :: Maybe Aeson.Value)
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
