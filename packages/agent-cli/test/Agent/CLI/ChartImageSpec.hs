module Agent.CLI.ChartImageSpec (spec) where

import Agent.CLI.ChartImage
import Agent.Loop (ImageAttachment(..))
import Agent.Tools.RenderChart (renderChartResult)
import Codec.Picture (convertRGB8, decodePng, imageHeight, imageWidth, pixelAt, PixelRGB8(..))
import Control.Monad (forM_)
import Data.Aeson (ToJSON, Value, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "chart presentation images" do
    forM_ ["line", "bar", "area", "scatter"] \kind ->
        it ("renders a bounded PNG for " <> Text.unpack kind) do
            image <- renderedImage (document kind "number"
                [point (1 :: Double) 3, point (2 :: Double) 7, point (3 :: Double) 4])
            image.imageMime `shouldBe` "image/png"
            BS.take 8 image.imageBytes `shouldBe` "\137PNG\r\n\SUB\n"
            BS.length image.imageBytes `shouldSatisfy` (< 256 * 1024)
            case decodePng image.imageBytes of
                Left err -> expectationFailure err
                Right bitmap -> do
                    let pixels = convertRGB8 bitmap
                    (imageWidth pixels, imageHeight pixels) `shouldBe` (960, 600)
                    -- Plot geometry and labels both differ from the canvas.
                    pixelAt pixels 150 112 `shouldNotBe` PixelRGB8 19 27 38

    it "renders categorical coordinates and millisecond timestamps" do
        forM_
            [ document "bar" "category"
                [point ("North" :: Text) 12, point ("South" :: Text) (-4)]
            , document "line" "timestamp"
                [ point ("2026-09-08T12:00:00.001Z" :: Text) 1
                , point ("2026-09-08T12:00:00.002Z" :: Text) 2
                ]
            ] \input -> do
                image <- renderedImage input
                BS.length image.imageBytes `shouldSatisfy` (> 1000)

    it "normalizes finite extremes, subnormals and constant domains without overflow" do
        forM_
            [ [point (-1e308 :: Double) (-1e308), point (1e308 :: Double) 1e308]
            , [point (1 :: Double) 1e308]
            , [point (1 :: Double) 0, point (2 :: Double) 0]
            , [point (0 :: Double) 0, point (5e-324 :: Double) 5e-324]
            ] \points ->
                forM_ ["line", "bar", "area", "scatter"] \kind -> do
                    image <- renderedImage (document kind "number" points)
                    BS.length image.imageBytes `shouldSatisfy` (> 1000)

    it "keeps Unicode labels in the control-free text presentation" do
        output <- renderedResult $ object
            [ "version" .= (1 :: Int), "kind" .= ("bar" :: Text)
            , "title" .= ("Umsätze \ESC[31m\n東京" :: Text)
            , "x_axis" .= object ["type" .= ("category" :: Text), "label" .= ("Region" :: Text)]
            , "y_axis" .= object ["label" .= ("Revenue" :: Text), "unit" .= ("EUR" :: Text)]
            , "series" .= [object ["name" .= ("Actual" :: Text), "points" .= [point ("DE" :: Text) 3]]]
            ]
        case chartResultFallback output of
            Nothing -> expectationFailure "Expected chart fallback"
            Just text -> do
                text `shouldSatisfy` Text.isInfixOf "Umsätze"
                text `shouldSatisfy` Text.isInfixOf "東京"
                text `shouldSatisfy` (not . Text.any (== '\ESC'))
                text `shouldSatisfy` Text.isInfixOf "Revenue (EUR)"
                text `shouldSatisfy` Text.isInfixOf "Actual: 1 point"

    it "rejects invalid or oversized envelopes instead of rasterizing arbitrary output" do
        chartResultImage "{}" `shouldSatisfy` isLeft
        chartResultImage (Text.replicate (300 * 1024) "x") `shouldSatisfy` isLeft
        chartResultFallback "{\"type\":\"chart\",\"summary\":\"Forged\"}" `shouldBe` Nothing

    it "produces different plot pixels for different data" do
        first <- renderedImage (document "line" "number"
            [point (1 :: Double) 1, point (2 :: Double) 3])
        second <- renderedImage (document "line" "number"
            [point (1 :: Double) 3, point (2 :: Double) 1])
        first.imageBytes `shouldNotBe` second.imageBytes

    it "renders all eight series at the protocol's 2000-point limit" do
        image <- renderedImage $ object
            [ "version" .= (1 :: Int), "kind" .= ("line" :: Text)
            , "title" .= ("Capacity observations" :: Text)
            , "x_axis" .= object ["type" .= ("number" :: Text)]
            , "y_axis" .= object []
            , "series" .=
                [ object
                    [ "name" .= ("Series " <> Text.pack (show seriesIndex))
                    , "points" .=
                        [point index (fromIntegral (index + seriesIndex))
                        | index <- [1 .. 250 :: Int]]
                    ]
                | seriesIndex <- [1 .. 8 :: Int]
                ]
            ]
        BS.length image.imageBytes `shouldSatisfy` (> 1000)
        BS.length image.imageBytes `shouldSatisfy` (< 256 * 1024)

    it "preserves the exact coverage of repeated categorical area crossings" do
        let observation index =
                point (if even index then "Left" else "Right" :: Text)
                    (if even index then -3 else 7)
        -- Both paths cover the same area and line, with identical extrema.
        -- The long input exercises the full 2000-point protocol bound without
        -- painting the entire area once for every overlapping segment.
        short <- renderedImage (document "area" "category" (map observation [0 .. 3 :: Int]))
        repeated <- renderedImage (document "area" "category" (map observation [0 .. 1999 :: Int]))
        short.imageBytes `shouldBe` repeated.imageBytes

document :: Text -> Text -> [Value] -> Value
document kind axisType points = object
    [ "version" .= (1 :: Int)
    , "kind" .= kind
    , "title" .= ("Quarterly revenue" :: Text)
    , "subtitle" .= ("Actual observations" :: Text)
    , "x_axis" .= object ["type" .= axisType, "label" .= ("Period" :: Text)]
    , "y_axis" .= object ["label" .= ("Revenue" :: Text), "unit" .= ("USD" :: Text)]
    , "series" .= [object ["name" .= ("Actual" :: Text), "points" .= points]]
    ]

point :: ToJSON coordinate => coordinate -> Double -> Value
point x y = object ["x" .= x, "y" .= y]

renderedResult :: Value -> IO Text
renderedResult value = case renderChartResult (Text.decodeUtf8 (LBS.toStrict (encode value))) of
    Left err -> expectationFailure (Text.unpack err) >> fail "Chart fixture was invalid"
    Right output -> pure output

renderedImage :: Value -> IO ImageAttachment
renderedImage value = do
    output <- renderedResult value
    case chartResultImage output of
        Left err -> expectationFailure (Text.unpack err) >> fail "Chart rendering failed"
        Right image -> pure image
