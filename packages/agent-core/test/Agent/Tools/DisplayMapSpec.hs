module Agent.Tools.DisplayMapSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..), ToolDispatchConfig(..), dispatchToolCall
    , functionToolCall )
import Agent.Tools.DisplayMap
import Agent.Tools.OutputArtifact (finalizeToolOutput)
import Agent.Tools.Types (AppTool(..), ToolEnv(..), ToolExecutionPolicy(..), defaultToolEnv)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "display_map" do
    it "is parallel-safe and returns a versioned map with readable fallback" do
        displayMapTool.appToolExecution `shouldBe` ParallelSafe
        output <- runTool (arguments [location "one" 52.52 13.405])
        fmap (.title) (decodeMapResult output) `shouldBe` Just "Places"
        output `shouldSatisfy` Text.isInfixOf "\"version\":1"
        output `shouldSatisfy` Text.isInfixOf "supplied coordinates"

    it "rejects empty lists and duplicate identifiers" do
        runTool (arguments []) >>= (`shouldSatisfy` Text.isPrefixOf "Error:")
        runTool (arguments [location "one" 0 0, location "one" 1 1])
            >>= (`shouldSatisfy` Text.isPrefixOf "Error:")

    it "accepts 100 places and rejects 101" do
        let places count = [location (Text.pack (show index)) 0 0 | index <- [1..count :: Int]]
        runTool (arguments (places 100))
            >>= (`shouldSatisfy` (isJust . decodeMapResult))
        runTool (arguments (places 101))
            >>= (`shouldSatisfy` Text.isPrefixOf "Error:")

    it "accepts coordinate boundaries and rejects out-of-range values" do
        runTool (arguments [location "north" 90 180, location "south" (-90) (-180)])
            >>= (`shouldSatisfy` (isJust . decodeMapResult))
        mapM_ (\place -> runTool (arguments [place])
            >>= (`shouldSatisfy` Text.isPrefixOf "Error:"))
            [location "one" 90.01 0, location "one" 0 (-180.01)]

    it "rejects nonfinite, fractional-version, unknown-version and malformed results" do
        let document = "{\"type\":\"map\",\"version\":1,\"title\":\"Places\",\"locations\":[{\"id\":\"one\",\"name\":\"Place\",\"latitude\":1e999,\"longitude\":0}]}"
        decodeMapResult document `shouldBe` Nothing
        decodeMapResult (Text.replace "1e999" "NaN" document) `shouldBe` Nothing
        output <- runTool (arguments [location "one" 0 0])
        decodeMapResult (Text.replace "\"version\":1" "\"version\":1.5" output) `shouldBe` Nothing
        decodeMapResult (Text.replace "\"version\":1" "\"version\":2" output) `shouldBe` Nothing
        decodeMapResult (Text.take 20 output) `shouldBe` Nothing

    it "enforces UTF-8 byte bounds and nonblank identifiers" do
        runTool (arguments [location (Text.replicate 33 "é") 0 0])
            >>= (`shouldSatisfy` Text.isPrefixOf "Error:")
        runTool (arguments [location " \n" 0 0])
            >>= (`shouldSatisfy` Text.isPrefixOf "Error:")

    it "round trips complete maps larger than ordinary transcript previews" do
        output <- runTool (arguments
            [location (Text.pack (show index)) 0 0 | index <- [1..100 :: Int]])
        Text.length output `shouldSatisfy` (>8192)
        fmap renderMapResult (decodeMapResult output) `shouldBe` Just (Right output)

    it "does not replace valid map documents with oversized-output artifacts" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        output <- runTool (arguments [location "one" 0 0])
        finalizeToolOutput (env { toolOutputInlineCap = 1 })
            (functionToolCall "map-1" "display_map" "{}") output
            `shouldReturn` output

    it "rejects an encoded result beyond the document limit" do
        let result = MapResult "Places"
                [MapLocation (Text.pack (show index)) "Place" 0 0
                    (Just (Text.replicate 500 "\""))
                    (Just (Text.replicate 1000 "\""))
                | index <- [1..100 :: Int]]
        renderMapResult result `shouldSatisfy` isLeft
        decodeMapResult (Text.replicate (maximumMapResultBytes + 1) " ")
            `shouldBe` Nothing

arguments :: [Aeson.Value] -> Text
arguments places = Text.decodeUtf8 . LBS.toStrict . Aeson.encode $
    Aeson.object ["title" .= ("Places" :: Text), "locations" .= places]

location :: Text -> Double -> Double -> Aeson.Value
location identifier latitude longitude = Aeson.object
    [ "id" .= identifier, "name" .= ("Place" :: Text)
    , "latitude" .= latitude, "longitude" .= longitude ]

runTool :: Text -> IO Text
runTool arguments = do
    result <- dispatchToolCall ToolDispatchConfig
        { toolDispatchUnknownTool = ("Unknown:" <>)
        , toolDispatchFormatResult = either ("Error: " <>) id
        , toolDispatchFormatException = \name _ -> "Exception: " <> name
        , toolDispatchOnException = \_ _ -> pure ()
        , toolDispatchOnOutput = \_ _ -> pure ()
        , toolDispatchFinalizeOutput = \_ output -> pure output
        } [displayMapTool.appToolHandler]
        (functionToolCall "map-1" "display_map" arguments)
    pure result.output
