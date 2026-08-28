module Agent.ToolArgsSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolArgs
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    , typedTool
    )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "ToolArgs" do
    it "decodes typed records with Hermes" do
        decodeSearch "{\"query\":\"invoice\",\"limit\":5,\"dry_run\":\"false\"}"
            `shouldBe` Right (SearchArgs "invoice" 5 (Just False))

    it "accepts a bare string for string-list parameters" do
        Json.decodeText
            (objectArgs \object -> reqTextList object "ids")
            "{\"ids\":\"abc\"}"
            `shouldBe` Right ["abc"]

    it "uses defaults only for absent and null integers" do
        decodeSearch "{\"query\":\"invoice\"}"
            `shouldBe` Right (SearchArgs "invoice" 10 Nothing)
        decodeSearch "{\"query\":\"invoice\",\"limit\":null}"
            `shouldBe` Right (SearchArgs "invoice" 10 Nothing)

    it "rejects wrongly typed optional integers" do
        decodeSearch "{\"query\":\"invoice\",\"limit\":\"5\"}"
            `shouldSatisfy` isLeft

    it "accepts stringy booleans only through the compatibility helper" do
        decodeSearch "{\"query\":\"invoice\",\"dry_run\":\" FALSE \"}"
            `shouldBe` Right (SearchArgs "invoice" 10 (Just False))

    it "does not execute a handler when an argument is invalid" do
        called <- newIORef False
        result <- dispatchToolCall defaultLoopDispatch
            [ typedTool "search" searchArgsDecoder \(args :: SearchArgs) -> do
                writeIORef called True
                pure (Right args.query)
            ]
            (functionToolCall "call-1" "search"
                "{\"query\":\"invoice\",\"limit\":1.9}")
        result.output `shouldSatisfy` Text.isInfixOf "Error:"
        readIORef called `shouldReturn` False

    it "keeps an absent list distinct from an empty list" do
        let decoder = objectArgs \object ->
                optList Json.text object "items" "items must be an array"
        Json.decodeText decoder "{}" `shouldBe` Right Nothing
        Json.decodeText decoder "{\"items\":[]}" `shouldBe` Right (Just [])

data SearchArgs = SearchArgs
    { query :: Text
    , limit :: Int
    , dryRun :: Maybe Bool
    } deriving (Eq, Show)

searchArgsDecoder :: Json.Decoder SearchArgs
searchArgsDecoder = objectArgs \object ->
    SearchArgs
        <$> reqText object "query"
        <*> intOr object "limit" 10
        <*> optBool object "dry_run"

decodeSearch :: Text -> Either Json.JsonError SearchArgs
decodeSearch = Json.decodeText searchArgsDecoder

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
