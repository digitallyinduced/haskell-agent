{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Agent.Json
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

data Person = Person
    { name :: !Text
    , age :: !Int
    , active :: !(Maybe Bool)
    , tags :: ![Text]
    , extensions :: !Extensions
    }
    deriving stock (Eq, Show)

data PersonState = PersonState
    { personName :: !(Maybe Text)
    , personAge :: !(Maybe Int)
    , personActive :: !(Maybe Bool)
    , personTags :: !(Maybe [Text])
    , personExtensions :: !Extensions
    }

emptyPersonState :: PersonState
emptyPersonState = PersonState
    { personName = Nothing
    , personAge = Nothing
    , personActive = Nothing
    , personTags = Nothing
    , personExtensions = emptyExtensions
    }

personDecoder :: Decoder.Decoder Person
personDecoder =
    Decoder.object
        emptyPersonState
        fields
        (Decoder.unknownField Decoder.rawJson
            \key value state ->
                Right state
                    { personExtensions =
                        insertExtension
                            key
                            value
                            state.personExtensions
                    })
        finish
  where
    fields =
        [ Decoder.field "name" Decoder.text \value state ->
            Right state { personName = Just value }
        , Decoder.field "age" Decoder.int \value state ->
            Right state { personAge = Just value }
        , Decoder.field "active"
            (Decoder.nullable Decoder.bool) \value state ->
            Right state { personActive = value }
        , Decoder.field "tags"
            (Decoder.array Decoder.text) \value state ->
            Right state { personTags = Just value }
        ]

    finish PersonState {..} = do
        person <- Person
            <$> required "name" personName
            <*> required "age" personAge
            <*> Right personActive
            <*> required "tags" personTags
            <*> Right personExtensions
        pure person

    required fieldLabel =
        maybe (Left ("missing required field " <> fieldLabel)) Right

personEncoder :: Encoder.Encoder Person
personEncoder = Encoder.object
    [ Encoder.field "name" Encoder.text (.name)
    , Encoder.field "age" Encoder.int (.age)
    , Encoder.nullableField "active" Encoder.bool (.active)
    , Encoder.field "tags" (Encoder.list Encoder.text) (.tags)
    , Encoder.extensionsField (.extensions)
    ]

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Left err -> expectationFailure (show err) >> error "unreachable"
    Right value -> pure value

data Planned = Planned
    { plannedName :: !Text
    , plannedCount :: !Int
    , plannedEnabled :: !(Maybe Bool)
    , plannedExtensions :: !Extensions
    }

plannedDecoder :: Decoder.Decoder Planned
plannedDecoder =
    Decoder.objectFields $
        Planned
            <$> Decoder.requiredField "name" Decoder.text
            <*> Decoder.defaultField 0 "count" Decoder.int
            <*> Decoder.optionalField "enabled" Decoder.bool
            <*> Decoder.extensionFields

taggedDecoder :: Decoder.Decoder Text
taggedDecoder =
    Decoder.discriminatedObject "type" \case
        "text" ->
            Decoder.objectFields $
                Decoder.requiredField "value" Decoder.text
                    <* Decoder.defaultField () "type" (() <$ Decoder.text)
        tag ->
            Decoder.mapEither
                (const (Left ("unknown tag " <> tag)))
                Decoder.skip

plannedNestedDecoder :: Decoder.Decoder ()
plannedNestedDecoder =
    Decoder.byType \case
        Decoder.JsonNull -> Decoder.nullValue ()
        Decoder.JsonObject ->
            Decoder.objectFields $
                Decoder.defaultField
                    ()
                    "x"
                    plannedNestedDecoder
        _ -> () <$ Decoder.skip

main :: IO ()
main = hspec do
    describe "portable direct decoder" do
        it "decodes typed fields and retains unknown values opaquely" do
            let input =
                    "{\"name\":\"Ada\",\"age\":37,\"active\":true,"
                        <> "\"tags\":[\"math\",\"code\"],"
                        <> "\"vendor\":{\"nested\":[1,true,null]}}"
            person <- expectRight (Decoder.decode personDecoder input)
            person.name `shouldBe` "Ada"
            person.age `shouldBe` 37
            person.active `shouldBe` Just True
            person.tags `shouldBe` ["math", "code"]
            rawJsonBytes
                <$> lookupExtension "vendor" person.extensions
                `shouldBe` Just "{\"nested\":[1,true,null]}"

        it "uses last-key-wins semantics" do
            person <- expectRight $
                Decoder.decode personDecoder
                    ( "{\"name\":\"first\",\"name\":\"last\","
                        <> "\"age\":1,\"tags\":[]}"
                    )
            person.name `shouldBe` "last"

        it "reports nested paths for type errors" do
            case Decoder.decode personDecoder
                "{\"name\":\"Ada\",\"age\":1,\"tags\":[\"ok\",false]}" of
                Left err ->
                    err.path
                        `shouldBe`
                            [ Decoder.PathKey "tags"
                            , Decoder.PathIndex 1
                            ]
                Right _ -> expectationFailure "expected decoding to fail"

        it "rejects malformed JSON and trailing bytes" do
            mapM_
                (\input ->
                    Decoder.decode Decoder.rawJson input
                        `shouldSatisfy` isLeft)
                [ ""
                , "01"
                , "1."
                , "1e"
                , "\"\\uD800\""
                , "\"\x01\""
                , "[1,]"
                , "{\"a\":1,}"
                , "{\"a\":[1,2}"
                , "true false"
                , "+1"
                , ".1"
                , "00"
                , "1e+"
                , "\"\\x20\""
                , "{\"a\" 1}"
                ]

        it "rejects invalid UTF-8" do
            Decoder.validateRawJson (BS.pack [0x22, 0xc0, 0xaf, 0x22])
                `shouldSatisfy` isLeft

        it "enforces the nesting-depth limit" do
            let nesting = 1_025
                input =
                    BS.replicate nesting 0x5b
                        <> "0"
                        <> BS.replicate nesting 0x5d
            Decoder.validateRawJson input `shouldSatisfy` isLeft

        it "enforces the depth limit for applicative objects" do
            let nesting = 1_025
                input =
                    BS.concat
                        (replicate nesting "{\"x\":")
                        <> "null"
                        <> BS.replicate nesting 0x7d
            Decoder.decode plannedNestedDecoder input
                `shouldSatisfy` isLeft

        it "accepts scalar roots" do
            Decoder.decode Decoder.bool "true" `shouldBe` Right True
            Decoder.decode Decoder.text "\"hello\"" `shouldBe` Right "hello"
            Decoder.decode Decoder.scientific "-12.5e2"
                `shouldBe` Right (-1250)
            Decoder.decode (Decoder.nullValue ()) "null" `shouldBe` Right ()

        it "decodes heterogeneous object fields applicatively in one pass" do
            planned <- expectRight $
                Decoder.decode plannedDecoder
                    ( "{\"name\":\"first\",\"count\":1,\"name\":\"last\","
                        <> "\"enabled\":null,\"future\":{\"x\":1}}"
                    )
            planned.plannedName `shouldBe` "last"
            planned.plannedCount `shouldBe` 1
            planned.plannedEnabled `shouldBe` Nothing
            rawJsonBytes
                <$> lookupExtension
                    "future"
                    planned.plannedExtensions
                `shouldBe` Just "{\"x\":1}"

        it "tracks known field presence and retains explicit null" do
            decoded <- expectRight $
                Decoder.decode plannedDecoder
                    "{\"name\":\"Ada\",\"active\":null}"
            extensionFieldWasPresent
                "active"
                decoded.plannedExtensions
                `shouldBe` True
            rawJsonBytes
                <$> lookupExtension
                    "active"
                    decoded.plannedExtensions
                `shouldBe` Just "null"

        it "selects tagged object codecs without a generic object" do
            Decoder.decode taggedDecoder
                "{\"value\":\"first\",\"type\":\"text\"}"
                `shouldBe` Right "first"
            Decoder.decode taggedDecoder
                "{\"type\":\"ignored\",\"type\":\"text\",\"value\":\"last\"}"
                `shouldBe` Right "last"

    describe "direct encoder" do
        it "round-trips domain values without a JSON tree" do
            extension <- expectRight $
                Decoder.validateRawJson "{\"enabled\":true}"
            let expected = Person
                    { name = "Ada"
                    , age = 37
                    , active = Nothing
                    , tags = ["math", "code"]
                    , extensions =
                        insertExtension "vendor" extension emptyExtensions
                    }
                encoded = Encoder.encode personEncoder expected
            Decoder.decode personDecoder encoded `shouldBe` Right expected

        it "does not let extensions override typed fields" do
            staleName <- expectRight $
                Decoder.validateRawJson "\"stale\""
            let value = Person
                    { name = "current"
                    , age = 1
                    , active = Just False
                    , tags = []
                    , extensions =
                        insertExtension "name" staleName emptyExtensions
                    }
                encoded = Encoder.encode personEncoder value
            BS8.count '"' encoded `shouldSatisfy` (> 0)
            Decoder.decode personDecoder encoded
                `shouldBe` Right value { extensions = emptyExtensions }

        it "retains an extension for an absent optional field" do
            explicitNull <- expectRight $
                Decoder.validateRawJson "null"
            let value = Person
                    { name = "Ada"
                    , age = 1
                    , active = Nothing
                    , tags = []
                    , extensions =
                        insertExtension
                            "active"
                            explicitNull
                            emptyExtensions
                    }
                encoded = Encoder.encode personEncoder value
            encoded `shouldSatisfy` BS8.isInfixOf "\"active\":null"

        it "escapes control characters and unicode directly" do
            let value = "quote: \" slash: \\ newline:\n snowman: ☃"
                encoded = Encoder.encode Encoder.text value
            Decoder.decode Decoder.text encoded `shouldBe` Right value

        it "encodes non-finite doubles as null" do
            Encoder.encode Encoder.double (0 / 0) `shouldBe` "null"
            Encoder.encode Encoder.double (1 / 0) `shouldBe` "null"

    describe "numeric safety" do
        it "rejects pathological Integer exponents without exponentiating" do
            Decoder.decode Decoder.integer "1e-1000000000"
                `shouldSatisfy` isLeft

    describe "opaque raw JSON" do
        it "copies validated bytes so they survive source reuse" do
            let source = "{\"a\":[1,2,3]}"
            raw <- expectRight (Decoder.validateRawJson source)
            rawJsonBytes raw `shouldBe` source

    describe "differential conformance" do
        prop "validates generated JSON accepted by Aeson" $
            withMaxSuccess 1_000 \(tree :: JsonTree) ->
                let bytes = renderJsonTree tree
                in counterexample (BS8.unpack bytes) $
                    isRight (Decoder.validateRawJson bytes)
                        .&&. isRight
                            (Aeson.eitherDecodeStrict bytes
                                :: Either String Aeson.Value)

data JsonTree
    = JsonNull
    | JsonBool !Bool
    | JsonNumber !Int
    | JsonText !Text
    | JsonArray ![JsonTree]
    | JsonObject ![(Text, JsonTree)]
    deriving stock (Eq, Show)

instance Arbitrary JsonTree where
    arbitrary = sized go
      where
        go size
            | size <= 0 = oneof scalars
            | otherwise = frequency
                [ (5, oneof scalars)
                , (2, JsonArray
                    <$> resize (size `div` 2) arbitrary)
                , (2, JsonObject
                    <$> resize (size `div` 2)
                        (listOf ((,) <$> jsonText <*> arbitrary)))
                ]

        scalars =
            [ pure JsonNull
            , JsonBool <$> arbitrary
            , JsonNumber <$> chooseInt (-1_000_000, 1_000_000)
            , JsonText <$> jsonText
            ]

        jsonText =
            Text.pack <$> listOf
                (arbitraryUnicodeChar `suchThat` \character ->
                    character < '\xD800' || character > '\xDFFF')

    shrink = \case
        JsonArray values ->
            map JsonArray (shrink values) <> values
        JsonObject fields ->
            [ JsonObject (take index fields <> drop (index + 1) fields)
            | index <- [0 .. length fields - 1]
            ] <> map snd fields
        JsonText value ->
            JsonText . Text.pack <$> shrink (Text.unpack value)
        JsonNumber value -> JsonNumber <$> shrink value
        JsonBool _ -> [JsonNull]
        JsonNull -> []

renderJsonTree :: JsonTree -> BS.ByteString
renderJsonTree = \case
    JsonNull -> "null"
    JsonBool value -> Encoder.encode Encoder.bool value
    JsonNumber value -> Encoder.encode Encoder.int value
    JsonText value -> Encoder.encode Encoder.text value
    JsonArray values ->
        BS.concat
            [ "["
            , BS.intercalate "," (map renderJsonTree values)
            , "]"
            ]
    JsonObject fields ->
        BS.concat
            [ "{"
            , BS.intercalate ","
                [ BS.concat
                    [ Encoder.encode Encoder.text key
                    , ":"
                    , renderJsonTree value
                    ]
                | (key, value) <- fields
                ]
            , "}"
            ]

