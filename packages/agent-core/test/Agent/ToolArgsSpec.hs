module Agent.ToolArgsSpec (spec) where

import Agent.ToolArgs
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    , typedTool
    )
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "ToolArgs" do
    it "extracts required text with model-facing errors" do
        extractText (Aeson.object []) "query"
            `shouldBe` Left "Missing parameter: query"

    it "accepts a bare string for string-list parameters" do
        extractTextList (Aeson.object ["ids" .= ("abc" :: Text)]) "ids"
            `shouldBe` Right ["abc"]

    it "accepts string booleans" do
        extractMaybeBool (Aeson.object ["flag" .= ("true" :: Text)]) "flag"
            `shouldBe` Just True

    it "backs typed FromJSON records" do
        Aeson.eitherDecode "{\"query\":\"invoice\",\"limit\":5,\"dry_run\":\"false\"}"
            `shouldBe`
                Right (SearchArgs "invoice" 5 (Just False))

    it "accepts only exact bounded JSON integers" do
        decodeSearch "{\"query\":\"invoice\",\"limit\":5}"
            `shouldBe` Right (SearchArgs "invoice" 5 Nothing)
        decodeSearch "{\"query\":\"invoice\",\"limit\":5.0}"
            `shouldBe` Right (SearchArgs "invoice" 5 Nothing)
        decodeSearch "{\"query\":\"invoice\",\"limit\":1.9}"
            `shouldBe` Left "Expected integer for key: limit"
        decodeSearch "{\"query\":\"invoice\",\"limit\":1e100}"
            `shouldBe` Left "Expected integer for key: limit"
        decodeSearch "{\"query\":\"invoice\",\"limit\":-1e100}"
            `shouldBe` Left "Expected integer for key: limit"
        decodeSearch "{\"query\":\"invoice\",\"limit\":\"5\"}"
            `shouldBe` Left "Expected integer for key: limit"

    it "parses required exact integers" do
        parseRequiredInt (Aeson.object ["session_id" .= (42 :: Int)])
            `shouldBe` Right 42
        parseRequiredInt (Aeson.object [])
            `shouldBe` Left "Missing parameter: session_id"
        parseRequiredInt (Aeson.object ["session_id" .= (1.5 :: Double)])
            `shouldBe` Left "Expected integer for key: session_id"

    it "defaults integers only when absent or null" do
        decodeSearch "{\"query\":\"invoice\"}"
            `shouldBe` Right (SearchArgs "invoice" 10 Nothing)
        decodeSearch "{\"query\":\"invoice\",\"limit\":null}"
            `shouldBe` Right (SearchArgs "invoice" 10 Nothing)

    it "keeps optional integers absent but rejects malformed values" do
        parseOptionalInt (Aeson.object [])
            `shouldBe` Right Nothing
        parseOptionalInt (Aeson.object ["timeout" .= Aeson.Null])
            `shouldBe` Right Nothing
        parseOptionalInt (Aeson.object ["timeout" .= (250 :: Int)])
            `shouldBe` Right (Just 250)
        parseOptionalInt (Aeson.object ["timeout" .= (2.5 :: Double)])
            `shouldBe` Left "Expected integer for key: timeout"
        parseOptionalInt (Aeson.object ["timeout" .= ("250" :: Text)])
            `shouldBe` Left "Expected integer for key: timeout"

    it "accepts bounded integer strings only through the compatibility helper" do
        parseOptionalIntString (Aeson.object ["timeout" .= (" 250 " :: Text)])
            `shouldBe` Right (Just 250)
        parseOptionalIntString (Aeson.object ["timeout" .= ("1.9" :: Text)])
            `shouldBe` Left "Expected integer for key: timeout"
        parseOptionalIntString (Aeson.object ["timeout" .= ("0x10" :: Text)])
            `shouldBe` Left "Expected integer for key: timeout"
        parseOptionalIntString
            (Aeson.object ["timeout" .= ("999999999999999999999999999" :: Text)])
            `shouldBe` Left "Expected integer for key: timeout"

    it "keeps legacy pure extractors total on malformed integers" do
        let fractional = jsonValue "{\"limit\":1.9}"
            overflowing = jsonValue "{\"limit\":1e100}"
        extractIntOr fractional "limit" 20 `shouldBe` 20
        extractMaybeInt fractional "limit" `shouldBe` Nothing
        extractIntOr overflowing "limit" 20 `shouldBe` 20
        extractMaybeInt overflowing "limit" `shouldBe` Nothing

    it "does not execute a handler when an integer argument is invalid" do
        called <- newIORef False
        result <- dispatchToolCall defaultLoopDispatch
            [ typedTool "search" \(args :: SearchArgs) -> do
                writeIORef called True
                pure (Right args.query)
            ]
            (functionToolCall "call-1" "search"
                "{\"query\":\"invoice\",\"limit\":1.9}")
        result.output `shouldBe` "Error: Expected integer for key: limit"
        readIORef called `shouldReturn` False

    it "keeps optList absent distinct from present empty" do
        parseEither (objectArgs $ \o -> optList @Text o "items" "items must be an array") (Aeson.object [])
            `shouldBe` Right Nothing
        parseEither (objectArgs $ \o -> optList @Text o "items" "items must be an array") (Aeson.object ["items" .= ([] :: [Text])])
            `shouldBe` Right (Just [])

data SearchArgs = SearchArgs
    { query :: Text
    , limit :: Int
    , dryRun :: Maybe Bool
    } deriving (Eq, Show)

instance Aeson.FromJSON SearchArgs where
    parseJSON = objectArgs $ \o -> SearchArgs
        <$> reqText o "query"
        <*> intOr o "limit" 10
        <*> optBool o "dry_run"

decodeSearch :: ByteString -> Either Text SearchArgs
decodeSearch bytes =
    case Aeson.eitherDecodeStrict bytes of
        Right value -> Right value
        Left err -> Left (stripAesonPrefix (toText err))

parseOptionalInt :: Aeson.Value -> Either Text (Maybe Int)
parseOptionalInt =
    firstText . parseEither (objectArgs $ \o -> optInt o "timeout")

parseOptionalIntString :: Aeson.Value -> Either Text (Maybe Int)
parseOptionalIntString =
    firstText . parseEither (objectArgs $ \o -> optIntOrString o "timeout")

parseRequiredInt :: Aeson.Value -> Either Text Int
parseRequiredInt =
    firstText . parseEither (objectArgs $ \o -> reqInt o "session_id")

firstText :: Either String a -> Either Text a
firstText = either (Left . stripAesonPrefix . toText) Right

toText :: String -> Text
toText = Text.pack

jsonValue :: ByteString -> Aeson.Value
jsonValue bytes =
    case Aeson.eitherDecodeStrict bytes of
        Right value -> value
        Left err -> error err
