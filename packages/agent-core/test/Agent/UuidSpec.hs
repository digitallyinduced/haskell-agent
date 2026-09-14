module Agent.UuidSpec (spec) where

import Agent.Uuid (generateUuidV7, uuidV7FromParts)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit, isUpper)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Uuid" do
    describe "uuidV7FromParts" do
        it "lays out the timestamp, version, and variant bits" do
            uuidV7FromParts
                0x0123456789ab
                (BS.pack [0xff, 0x11, 0xff, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88])
                `shouldBe` "01234567-89ab-7f11-bf22-334455667788"

        it "zero pads a short random tail" do
            uuidV7FromParts 1 BS.empty
                `shouldBe` "00000000-0001-7000-8000-000000000000"

        it "uses only the first ten random bytes" do
            uuidV7FromParts 1 (BS.replicate 32 0x01)
                `shouldBe` "00000000-0001-7101-8101-010101010101"

    describe "generateUuidV7" do
        it "produces canonical lowercase hexadecimal groups" do
            value <- generateUuidV7
            map Text.length (Text.splitOn "-" value) `shouldBe` [8, 4, 4, 4, 12]
            value `shouldSatisfy`
                Text.all (\c -> c == '-' || (isHexDigit c && not (isUpper c)))
            Text.index value 14 `shouldBe` '7'
            Text.index value 19 `shouldSatisfy` (`elem` ("89ab" :: String))

        it "produces distinct values" do
            first <- generateUuidV7
            second <- generateUuidV7
            first `shouldNotBe` second
