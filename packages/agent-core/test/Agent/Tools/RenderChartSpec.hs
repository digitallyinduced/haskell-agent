module Agent.Tools.RenderChartSpec (spec) where

import Agent.Tools.RenderChart
import Agent.Tools.Types (AppTool(..), ApprovalRule(..), ToolExecutionPolicy(..))
import Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.RenderChart" do
    it "is a read-only parallel-safe presentation tool" do
        renderChartTool.appToolName `shouldBe` "render_chart"
        renderChartTool.appToolExecution `shouldBe` ParallelSafe
        case renderChartTool.appToolApproval of
            AlwaysReadOnly -> pure ()
            _ -> expectationFailure "Expected read-only approval"

    it "retains the complete versioned document in its result" do
        let input = document "bar" "category" [point (String "April") 12]
        case renderChartResult input of
            Left err -> expectationFailure (Text.unpack err)
            Right output -> do
                chartResultDocument output `shouldBe` Just input
                chartResultSummary output `shouldBe`
                    Just "Rendered Revenue — bar chart; 1 series, 1 point."

    it "accepts every chart kind, negative numbers and single points" do
        mapM_ (\kind ->
            renderChartResult (document kind "number" [point (Number 1) (-4)])
                `shouldSatisfy` isRight) ["line", "bar", "area", "scatter"]

    it "keeps every Unicode category in the accessible chart fallback" do
        let labels = ["地域" <> Text.pack (show index) | index <- [1 :: Int .. 12]]
            input = document "bar" "category" [point (String label) 2 | label <- labels]
        case renderChartResult input >>= maybe (Left "Missing fallback") Right . chartResultFallback of
            Left err -> expectationFailure (Text.unpack err)
            Right fallback ->
                mapM_ (\label -> fallback `shouldSatisfy` Text.isInfixOf label) labels

    it "rejects invalid chart envelopes instead of trusting stored fallback text" do
        chartResultFallback "{\"type\":\"chart\",\"summary\":\"Forged\"}" `shouldBe` Nothing
        chartResultFallback "Error: invalid chart" `shouldBe` Nothing

    it "removes terminal controls from every chart fallback label" do
        let input = "{\"version\":1,\"kind\":\"bar\",\"title\":\"売上\",\"subtitle\":\"地域\\u0007別\",\"x_axis\":{\"type\":\"category\",\"label\":\"都\\u001b市\",\"unit\":\"区\\u009b域\"},\"y_axis\":{\"label\":\"収\\u000d益\",\"unit\":\"円\\u0008\"},\"series\":[{\"name\":\"実\\u0000績\",\"points\":[{\"x\":\"東\\u001b京\",\"y\":2}]}]}"
        case renderChartResult input >>= maybe (Left "Missing fallback") Right . chartResultFallback of
            Left err -> expectationFailure (Text.unpack err)
            Right fallback -> do
                mapM_ (\label -> fallback `shouldSatisfy` Text.isInfixOf label)
                    ["地域 別", "都 市", "区 域", "収 益", "円", "実 績", "東 京"]
                mapM_ (\control -> fallback `shouldNotSatisfy` Text.isInfixOf control)
                    ["\ESC", "\NUL", "\BEL", "\r", "\BS", "\x9b"]

    it "rejects unsupported versions, kinds and fields" do
        let input = document "bar" "number" [point (Number 1) 2]
        renderChartResult (Text.replace "\"version\":1" "\"version\":2" input) `shouldSatisfy` isLeft
        renderChartResult (Text.replace "\"kind\":\"bar\"" "\"kind\":\"pie\"" input) `shouldSatisfy` isLeft
        renderChartResult (Text.replace "\"y\":2" "\"y\":2,\"source\":\"code\"" input) `shouldSatisfy` isLeft

    it "requires finite numbers and consistent x types" do
        renderChartResult (document "bar" "number" [point (String "one") 2]) `shouldSatisfy` isLeft
        renderChartResult (Text.replace "\"y\":2" "\"y\":1e999" $
            document "bar" "number" [point (Number 1) 2]) `shouldSatisfy` isLeft
        renderChartResult (Text.replace "\"y\":2" "\"y\":null" $
            document "bar" "number" [point (Number 1) 2]) `shouldSatisfy` isLeft

    it "requires numeric and timestamp line/area observations to be increasing" do
        let points = [point (Number 2) 1, point (Number 1) 2]
        mapM_ (\kind -> renderChartResult (document kind "number" points) `shouldSatisfy` isLeft)
            ["line", "area"]
        renderChartResult (document "scatter" "number" points) `shouldSatisfy` isRight
        renderChartResult (document "line" "number" [point (Number 1) 1, point (Number 1) 2])
            `shouldSatisfy` isLeft

    it "rejects timestamps more precise than milliseconds for every kind" do
        let points = map (\timestamp -> point (String timestamp) 2)
                ["2026-09-07T12:30:00.000000001Z", "2026-09-07T12:30:00.000000002Z"]
        mapM_ (\kind -> renderChartResult (document kind "timestamp" points) `shouldSatisfy` isLeft)
            ["line", "area", "bar", "scatter"]

    it "validates timestamp syntax and calendar values" do
        mapM_ (\timestamp -> renderChartResult
            (document "line" "timestamp" [point (String timestamp) 2]) `shouldSatisfy` isRight)
            ["2024-02-29T12:30:00Z", "2026-09-07T12:30:00.123Z"]
        mapM_ (\timestamp -> renderChartResult
            (document "line" "timestamp" [point (String timestamp) 2]) `shouldSatisfy` isLeft)
            [ "2025-02-29T12:30:00Z", "2026-09-07", "2026-09-07T12:30:00+02:00"
            , "2026-09-07T12:30:60Z", "0000-01-01T00:00:00Z", "2026-09-07T12:30:00.Z"
            , "2026-09-07T12:30:00.1234Z"
            ]

    it "bounds points, text and serialized input" do
        renderChartResult (document "bar" "number" []) `shouldSatisfy` isLeft
        renderChartResult (document "bar" "number" (replicate 2001 (point (Number 1) 2)))
            `shouldSatisfy` isLeft
        renderChartResult (Text.replace "Revenue" (Text.replicate 201 "x")
            (document "bar" "number" [point (Number 1) 2])) `shouldSatisfy` isLeft
        renderChartResult (Text.replicate (256 * 1024 + 1) " ") `shouldSatisfy` isLeft

    it "does not interpret failed, truncated or unknown results as charts" do
        chartResultDocument "Error: invalid chart" `shouldBe` Nothing
        chartResultDocument "{\"type\":\"chart\",\"chart\":" `shouldBe` Nothing
        chartResultDocument "{\"type\":\"image\",\"chart\":{}}" `shouldBe` Nothing

    it "keeps error-related chart titles distinct from tool failures" do
        let input = Text.replace "Revenue" "Error: request rates"
                (document "bar" "number" [point (Number 1) 2])
        (renderChartResult input >>= maybe (Left "Missing summary") Right . chartResultSummary)
            `shouldBe` Right "Rendered Error: request rates — bar chart; 1 series, 1 point."

    it "requires unique series names and bounds the series count" do
        let series (name :: Text) = object ["name" .= name, "points" .= [point (Number 1) 2]]
            input values = encodeText (object
                [ "version" .= (1 :: Int), "kind" .= ("bar" :: Text), "title" .= ("Revenue" :: Text)
                , "x_axis" .= object ["type" .= ("number" :: Text)]
                , "y_axis" .= object [], "series" .= values
                ])
        renderChartResult (input [series ("Sales" :: Text), series "Sales"]) `shouldSatisfy` isLeft
        renderChartResult (input [series (Text.pack (show index)) | index <- [1 :: Int .. 9]])
            `shouldSatisfy` isLeft

    it "accepts nullable optional metadata and counts Unicode scalars" do
        let input = document "bar" "number" [point (Number 1) 2]
        renderChartResult (Text.replace "\"title\":\"Revenue\""
            "\"title\":\"Revenue\",\"subtitle\":null" input) `shouldSatisfy` isRight
        renderChartResult (Text.replace "Revenue" (Text.replicate 200 "😀") input) `shouldSatisfy` isRight
        renderChartResult (Text.replace "Revenue" (Text.replicate 201 "😀") input) `shouldSatisfy` isLeft

document :: Text -> Text -> [Value] -> Text
document kind axisType points = encodeText (object
    [ "version" .= (1 :: Int), "kind" .= kind, "title" .= ("Revenue" :: Text)
    , "x_axis" .= object ["type" .= axisType]
    , "y_axis" .= object ["label" .= ("Revenue" :: Text), "unit" .= ("EUR" :: Text)]
    , "series" .= [object ["name" .= ("Sales" :: Text), "points" .= points]]
    ])

point :: Value -> Double -> Value
point coordinate value = object ["x" .= coordinate, "y" .= value]

encodeText :: Value -> Text
encodeText = TextEncoding.decodeUtf8 . LBS.toStrict . encode
