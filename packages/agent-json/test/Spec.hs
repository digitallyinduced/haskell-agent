module Main (main) where

import Agent.Json
import qualified Agent.Json.Decode as Json
import Control.Concurrent.Async (concurrently)
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Test.Hspec

data Person = Person
    { personName :: !Text
    , personAge :: !Int
    }
    deriving (Eq, Show)

personDecoder :: Json.Decoder Person
personDecoder =
    Json.object $
        Person
            <$> Json.atKey "name" Json.text
            <*> Json.atKey "age" Json.int

optionalPersonDecoder :: Json.Decoder (Maybe Text, Int)
optionalPersonDecoder =
    Json.object $
        (,)
            <$> Json.optionalKey "nickname" Json.text
            <*> Json.defaultKey 0 "score" Json.int

main :: IO ()
main = hspec do
    describe "Hermes JSON boundary" do
        it "decodes objects" do
            Json.decodeEither personDecoder
                "{\"name\":\"Ada\",\"age\":37}"
                `shouldBe` Right (Person "Ada" 37)

        it "decodes arrays" do
            Json.decodeEither (Json.list Json.int) "[1,2,3]"
                `shouldBe` Right [1, 2, 3]

        it "decodes scalar documents through Hermes" do
            Json.decodeEither Json.text "\"hello\""
                `shouldBe` Right "hello"
            Json.decodeEither Json.int "42"
                `shouldBe` Right 42
            Json.decodeEither Json.bool "true"
                `shouldBe` Right True

        it "treats missing and null optional fields alike" do
            Json.decodeEither optionalPersonDecoder "{}"
                `shouldBe` Right (Nothing, 0)
            Json.decodeEither optionalPersonDecoder
                "{\"nickname\":null,\"score\":null}"
                `shouldBe` Right (Nothing, 0)

        it "keeps owned raw JSON valid after the parse buffer is released" do
            -- Hermes exposes a view into simdjson's padded input, which is
            -- freed when the parse returns. Decode lazily, churn the
            -- allocator with same-sized documents, then force the result.
            let document = "{\"k\":\"" <> BS.replicate 300 0x41 <> "\"}"
                churn = "{\"k\":\"" <> BS.replicate 300 0x42 <> "\"}"
                lazyRaw =
                    Json.withOwnedRawJson (pure . Text.decodeUtf8)
                        :: Json.Decoder Text
                result = Json.decodeEither lazyRaw document
            either (expectationFailure . show) (const (pure ())) result
            mapM_
                (\_ ->
                    Json.decodeEither
                        (Json.withOwnedRawJson (pure . BS.length))
                        churn
                        `shouldBe` Right (BS.length churn))
                [1 .. 20 :: Int]
            fmap Text.encodeUtf8 result `shouldBe` Right document

        it "re-decodes owned raw JSON captured inside a decoder" do
            let nested = Json.object $
                    Json.atKey "items" $ Json.withOwnedRawJson \raw ->
                        pure (Json.decodeEither (Json.list Json.int) raw)
            Json.decodeEither nested "{\"items\":[1,2,3]}"
                `shouldBe` Right (Right [1, 2, 3])

        it "rejects malformed and trailing input" do
            Json.decodeEither personDecoder
                "{\"name\":\"Ada\",\"age\":}"
                `shouldSatisfy` isLeft
            Json.decodeEither Json.int "42 false"
                `shouldSatisfy` isLeft

        it "owns raw bytes after the decoder session closes" do
            result <-
                Json.withDecoderSession \session ->
                    Json.decodeIO session rawJsonDecoder
                        "{\"nested\":[1,true,null]}"
            fmap rawJsonBytes result
                `shouldBe` Right "{\"nested\":[1,true,null]}"

        it "validates complete opaque values" do
            fmap rawJsonBytes
                (Json.validateRawJson "{\"nested\":[1,true,null]}")
                `shouldBe` Right "{\"nested\":[1,true,null]}"
            Json.validateRawJson "{\"nested\":[1,]}"
                `shouldSatisfy` isLeft
            Json.validateRawJson "true false"
                `shouldSatisfy` isLeft

        it "injects raw bytes into Aeson encoding without reparsing" do
            let raw =
                    rawJsonFromEncoding $
                        Aeson.pairs (Aeson.pair "answer" (Aeson.int 42))
            LBS.toStrict
                (Aeson.encodingToLazyByteString
                    (Aeson.pairs
                        (Aeson.pair "raw" (rawJsonEncoding raw))))
                `shouldBe` "{\"raw\":{\"answer\":42}}"

        it "materialises Aeson encoder values through Hermes" do
            fmap Aeson.toJSON (Json.validateRawJson "true")
                `shouldBe` Right (Aeson.Bool True)
            fmap Aeson.toJSON
                (Json.validateRawJson "{\"answer\":42}")
                `shouldBe`
                    Right
                        (Aeson.object
                            ["answer" Aeson..= (42 :: Int)])

        it "uses independent environments for concurrent streams" do
            let decodeMany label =
                    Json.withDecoderSession \session ->
                        mapM
                            (Json.decodeIO session personDecoder)
                            [ "{\"name\":\"" <> label
                                <> "\",\"age\":" <> age <> "}"
                            | age <- ["1", "2", "3"]
                            ]
            (left, right) <- concurrently (decodeMany "left") (decodeMany "right")
            left `shouldBe`
                [ Right (Person "left" 1)
                , Right (Person "left" 2)
                , Right (Person "left" 3)
                ]
            right `shouldBe`
                [ Right (Person "right" 1)
                , Right (Person "right" 2)
                , Right (Person "right" 3)
                ]

isLeft :: Either error value -> Bool
isLeft = \case
    Left{} -> True
    Right{} -> False
