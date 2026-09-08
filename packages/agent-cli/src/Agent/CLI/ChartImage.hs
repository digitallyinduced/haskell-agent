-- | Bounded, process-free rasterization of the shared chart presentation
-- protocol. The original document remains authoritative; images are disposable
-- view data and never become model context.
module Agent.CLI.ChartImage
    ( chartResultImage
    , chartResultFallback
    , terminalChartTool
    ) where

import Agent.Loop (ImageAttachment(..))
import Agent.Tools.RenderChart (chartResultDocument, chartResultFallback, renderChartTool)
import Agent.Tools.ShowImage (ImageDisplayHooks(..), ImageDisplayRequest(..))
import Agent.Tools.Types (AppTool(..))
import Agent.ToolDispatch (ToolCall(..), ToolHandlerResult(..), wrapToolHandler)
import Codec.Picture (Image, PixelRGB8(..), encodePng)
import Codec.Picture.Types (MutableImage, createMutableImage, freezeImage, writePixel)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Aeson (Value, eitherDecodeStrict', withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, toUpper)
import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Numeric (showGFloat)

data Chart = Chart
    { kind :: !Text
    , title :: !Text
    , subtitle :: !Text
    , xAxis :: !Axis
    , yAxis :: !Axis
    , series :: ![Series]
    }

-- | Keep the durable chart document unchanged; only the host sees the PNG.
terminalChartTool :: ImageDisplayHooks -> AppTool
terminalChartTool hooks =
    renderChartTool
        { appToolDescription =
            Text.replace "interactive native chart" "static chart"
                renderChartTool.appToolDescription
        , appToolHandler =
            wrapToolHandler
                (\call execute -> execute >>= \case
                    Left err -> pure (Left err)
                    Right result ->
                        case chartResultImage result.resultText of
                            Left err -> pure (Left err)
                            Right image -> do
                                displayed <- hooks.showImage ImageDisplayRequest
                                    { displayCallId = call.callId
                                    , displayPath = "render_chart"
                                    , displayCaption = chartResultFallback result.resultText
                                    , displayImage = image
                                    }
                                pure (result <$ displayed))
                renderChartTool.appToolHandler
        }

data Axis = Axis
    { axisType :: !Text
    , label :: !Text
    , unit :: !Text
    }

data Series = Series
    { name :: !Text
    , points :: ![(Coordinate, Double)]
    }

data Coordinate = Category !Text | Number !Double | Timestamp !UTCTime
    deriving (Eq, Ord)

-- | Accept only the complete validated tool-result envelope, not arbitrary
-- JSON that merely happens to contain a chart-shaped field.
chartResultImage :: Text -> Either Text ImageAttachment
chartResultImage output = do
    chart <- parseResult output
    pure ImageAttachment
        { imageMime = "image/png"
        , imageBytes = LBS.toStrict (encodePng (drawChart chart))
        }

parseResult :: Text -> Either Text Chart
parseResult output = do
    document <- maybe (Left "Invalid chart presentation document.") Right
        (chartResultDocument output)
    value <- either (Left . Text.pack) Right (eitherDecodeStrict' (Text.encodeUtf8 document))
    either (Left . Text.pack) Right (parseEither parseChart value)

parseChart :: Value -> Parser Chart
parseChart = withObject "chart" \fields -> do
    kind <- fields .: "kind"
    title <- fields .: "title"
    subtitle <- fields .:? "subtitle" .!= ""
    xAxis <- fields .: "x_axis" >>= parseAxis
    yAxis <- fields .: "y_axis" >>= parseAxis
    series <- fields .: "series" >>= traverse (parseSeries xAxis.axisType)
    pure Chart{..}
  where
    parseAxis = withObject "axis" \fields ->
        Axis <$> (fields .:? "type" .!= "number")
            <*> (fields .:? "label" .!= "")
            <*> (fields .:? "unit" .!= "")
    parseSeries axisType = withObject "series" \fields -> do
        name <- fields .: "name"
        points <- fields .: "points" >>= traverse (parsePoint axisType)
        pure Series{..}
    parsePoint axisType = withObject "point" \fields -> do
        coordinate <- case axisType of
            "category" -> Category <$> fields .: "x"
            "timestamp" -> do
                text <- fields .: "x"
                Timestamp <$> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" text
            _ -> Number <$> fields .: "x"
        ordinate <- fields .: "y"
        pure (coordinate, ordinate)

safeText :: Text -> Text
safeText = Text.unwords . Text.words . Text.map \character ->
    if isControl character then ' ' else character

axisDescription :: Axis -> Text
axisDescription axis =
    safeText axis.label
        <> if Text.null axis.unit then "" else " (" <> safeText axis.unit <> ")"

imageWidth, imageHeight, plotLeft, plotRight, plotTop, plotBottom :: Int
imageWidth = 960
imageHeight = 600
plotLeft = 150
plotRight = 918
plotTop = 112
plotBottom = 434

background, foreground, muted, grid :: PixelRGB8
background = PixelRGB8 19 27 38
foreground = PixelRGB8 232 237 243
muted = PixelRGB8 165 180 195
grid = PixelRGB8 49 64 79

seriesColors :: [PixelRGB8]
seriesColors =
    [ PixelRGB8 81 174 245, PixelRGB8 245 174 71
    , PixelRGB8 87 207 162, PixelRGB8 231 116 146
    , PixelRGB8 179 153 247, PixelRGB8 95 210 220
    , PixelRGB8 226 214 102, PixelRGB8 203 164 132
    ]

-- Rational domain arithmetic prevents overflow for finite extremes and
-- preserves narrow ranges (including subnormals). Conversion to floating
-- point happens only after normalization into [0,1].
data Scale = Scale !Rational !Rational

makeScale :: Bool -> [Rational] -> Scale
makeScale includeZero values =
    let bounds = if includeZero then 0 : values else values
    in Scale (minimum bounds) (maximum bounds)

fractionAt :: Scale -> Rational -> Double
fractionAt (Scale lower upper) value
    | lower == upper = 0.5
    | otherwise = fromRational ((value - lower) / (upper - lower))

valueAt :: Scale -> Rational -> Rational
valueAt (Scale lower upper) fraction =
    lower * (1 - fraction) + upper * fraction

drawChart :: Chart -> Image PixelRGB8
drawChart chart = runST do
    image <- createMutableImage imageWidth imageHeight background
    labelText image foreground 3 24 20 50 chart.title
    labelText image muted 2 24 54 75 chart.subtitle
    labelText image muted 2 plotLeft 84 64 (axisDescription chart.yAxis)
    forM_ yTickFractions \fraction -> do
        let position = plotBottom - round (fromIntegral (plotBottom - plotTop) * fraction)
        line image grid 1 (plotLeft, position) (plotRight, position)
        labelText image muted 2 8 (position - 6) 11
            (numberText (fromRational (valueAt yScale fraction)))
    forM_ xTicks \(position, text) -> do
        line image grid 1 (position, plotTop) (position, plotBottom)
        labelText image muted 2 (max 8 (min (imageWidth - 145) (position - 45)))
            (plotBottom + 16) 12 text
    line image muted 1 (plotLeft, plotTop) (plotLeft, plotBottom)
    line image muted 1 (plotLeft, plotBottom) (plotRight, plotBottom)
    forM_ (zip3 [0 ..] seriesColors chart.series) \(seriesIndex, color, series) -> do
        let positions = [(xPosition x, yPosition y) | (x, y) <- series.points]
        case chart.kind of
            "bar" -> forM_ positions \(x, y) -> do
                let totalWidth = max 1 (min 64 (barSpacing * 3 `div` 4))
                    width = max 1 (totalWidth `div` length chart.series)
                    left = x - totalWidth `div` 2 + seriesIndex * width
                rectangle image color left (min y zeroY)
                    (left + width - 1) (max y zeroY)
            "scatter" -> pure ()
            _ -> do
                when (chart.kind == "area") $
                    fillArea image (areaColor color) zeroY positions
                forM_ (zip positions (drop 1 positions)) \(start, end) ->
                    line image color 2 start end
        when (chart.kind /= "bar") $
            forM_ positions \(x, y) ->
                rectangle image color (x - 2) (y - 2) (x + 2) (y + 2)
        let legendX = 24 + (seriesIndex `mod` 4) * 234
            legendY = 520 + (seriesIndex `div` 4) * 32
        rectangle image color legendX legendY (legendX + 13) (legendY + 13)
        labelText image foreground 2 (legendX + 22) legendY 16 series.name
    labelText image muted 2 plotLeft 477 64 (axisDescription chart.xAxis)
    freezeImage image
  where
    coordinates = nub [x | series <- chart.series, (x, _) <- series.points]
    coordinateIndices = Map.fromList (zip coordinates [0 :: Int ..])
    coordinateValue = \case
        Number value -> toRational value
        Timestamp value -> toRational (utcTimeToPOSIXSeconds value)
        category -> fromIntegral (Map.findWithDefault 0 category coordinateIndices)
    rawXScale = makeScale False (map coordinateValue coordinates)
    xScale = case rawXScale of
        Scale lower upper
            | chart.xAxis.axisType == "category" || chart.kind == "bar" ->
                let padding = if lower == upper then 1 / 2 else (upper - lower) / 20
                in Scale (lower - padding) (upper + padding)
        _ -> rawXScale
    yScale = makeScale (chart.kind `elem` ["bar", "area"])
        [toRational y | series <- chart.series, (_, y) <- series.points]
    yTickFractions = case yScale of
        Scale lower upper | lower == upper -> [1 / 2]
        _ -> [0, 1 / 4, 1 / 2, 3 / 4, 1]
    xPosition coordinate =
        plotLeft + round (fromIntegral (plotRight - plotLeft) * fractionAt xScale (coordinateValue coordinate))
    yPosition :: Double -> Int
    yPosition value =
        plotBottom - round (fromIntegral (plotBottom - plotTop) * fractionAt yScale (toRational value))
    zeroY = max plotTop (min plotBottom (yPosition 0))
    coordinatePositions = Map.keys (Map.fromList [(xPosition x, ()) | x <- coordinates])
    barSpacing = minimum (64 : zipWith (-) (drop 1 coordinatePositions) coordinatePositions)
    xTicks
        | chart.xAxis.axisType == "category" || chart.xAxis.axisType == "timestamp" =
            [(xPosition coordinate, tickText coordinate) | coordinate <- sampleLabels coordinates]
        | otherwise =
            [ (plotLeft + round (fromIntegral (plotRight - plotLeft) * fractionAt xScale value)
              , numberText (fromRational value))
            | index <- [0 .. 4 :: Int]
            , let value = valueAt rawXScale (fromIntegral index / 4)
            ]
    tickText (Timestamp value) =
        let Scale lower upper = rawXScale
            format
                | upper - lower < 60 = "%H:%M:%S%Q"
                | upper - lower < 86400 = "%H:%M:%S"
                | otherwise = "%m-%d %H:%M"
        in Text.pack (formatTime defaultTimeLocale format value)
    tickText coordinate = coordinateText coordinate

sampleLabels :: [a] -> [a]
sampleLabels values
    | length values <= 5 = values
    | otherwise = [values !! (index * (length values - 1) `div` 4) | index <- [0 .. 4]]

coordinateText :: Coordinate -> Text
coordinateText = \case
    Category text -> text
    Number value -> numberText value
    Timestamp value -> Text.pack (formatTime defaultTimeLocale "%m-%d %H:%M" value)

numberText :: Double -> Text
numberText value
    | value == 0 = "0"
    | otherwise = Text.pack (showGFloat (Just 3) value "")

areaColor :: PixelRGB8 -> PixelRGB8
areaColor (PixelRGB8 red green blue) =
    PixelRGB8 (red `div` 3 + 13) (green `div` 3 + 18) (blue `div` 3 + 25)

rectangle :: MutableImage s PixelRGB8 -> PixelRGB8 -> Int -> Int -> Int -> Int -> ST s ()
rectangle image color left top right bottom =
    forM_ [max 0 top .. min (imageHeight - 1) bottom] \y ->
        forM_ [max 0 left .. min (imageWidth - 1) right] \x ->
            writePixel image x y color

line :: MutableImage s PixelRGB8 -> PixelRGB8 -> Int -> (Int, Int) -> (Int, Int) -> ST s ()
line image color thickness (startX, startY) (endX, endY) =
    forM_ [0 .. steps] \index -> do
        let fraction = fromIntegral index / fromIntegral (max 1 steps) :: Double
            x = startX + round (fromIntegral (endX - startX) * fraction)
            y = startY + round (fromIntegral (endY - startY) * fraction)
        rectangle image color x y (x + thickness - 1) (y + thickness - 1)
  where
    steps = max (abs (endX - startX)) (abs (endY - startY))

-- | Every segment's vertical interval contains the same baseline, so their
-- union is exactly their minimum/maximum extent. Resolve that union before
-- touching the bitmap: even categorical paths that repeatedly cross the
-- canvas paint each pixel at most once per series.
fillArea :: MutableImage s PixelRGB8 -> PixelRGB8 -> Int -> [(Int, Int)] -> ST s ()
fillArea image color baseline positions =
    forM_ (IntMap.toList extents) \(x, (top, bottom)) ->
        rectangle image color x top x bottom
  where
    extents = foldl' addSegment IntMap.empty (zip positions (drop 1 positions))
    addSegment accumulated ((startX, startY), (endX, endY)) =
        foldl' addColumn accumulated [min startX endX .. max startX endX]
      where
        addColumn columns x =
            let fraction = if startX == endX then 0 else
                    fromIntegral (x - startX) / fromIntegral (endX - startX) :: Double
                y = startY + round (fromIntegral (endY - startY) * fraction)
            in IntMap.insertWith mergeExtents x (min y baseline, max y baseline) columns
    mergeExtents (firstTop, firstBottom) (secondTop, secondBottom) =
        (min firstTop secondTop, max firstBottom secondBottom)

-- | Small built-in specification lettering avoids font discovery, filesystem
-- reads and platform-specific graphics libraries. Unsupported glyphs are
-- visibly marked rather than silently omitted; original labels accompany the
-- image in the text presentation.
labelText :: MutableImage s PixelRGB8 -> PixelRGB8 -> Int -> Int -> Int -> Int -> Text -> ST s ()
labelText image color scale left top maximumCharacters text =
    forM_ (zip [0 ..] (Text.unpack clipped)) \(index, character) ->
        forM_ (zip [0 ..] (glyph (toUpper character))) \(row, columns) ->
            forM_ (zip [0 ..] columns) \(column, pixel) ->
                when (pixel == '#') $
                    rectangle image color
                        (left + index * 6 * scale + column * scale)
                        (top + row * scale)
                        (left + index * 6 * scale + column * scale + scale - 1)
                        (top + row * scale + scale - 1)
  where
    cleaned = safeText text
    clipped
        | Text.length cleaned <= maximumCharacters = cleaned
        | otherwise = Text.take (max 0 (maximumCharacters - 3)) cleaned <> "..."

glyph :: Char -> [String]
glyph character = fromMaybe ["#####","#...#","...#.","..#..",".....","..#..","....."] $
    Map.lookup character lettering

lettering :: Map.Map Char [String]
lettering = Map.fromList
    [ (' ', [".....",".....",".....",".....",".....",".....","....."])
    , ('A', [".###.","#...#","#...#","#####","#...#","#...#","#...#"])
    , ('B', ["####.","#...#","#...#","####.","#...#","#...#","####."])
    , ('C', [".####","#....","#....","#....","#....","#....",".####"])
    , ('D', ["####.","#...#","#...#","#...#","#...#","#...#","####."])
    , ('E', ["#####","#....","#....","####.","#....","#....","#####"])
    , ('F', ["#####","#....","#....","####.","#....","#....","#...."])
    , ('G', [".####","#....","#....","#.###","#...#","#...#",".###."])
    , ('H', ["#...#","#...#","#...#","#####","#...#","#...#","#...#"])
    , ('I', ["#####","..#..","..#..","..#..","..#..","..#..","#####"])
    , ('J', ["..###","...#.","...#.","...#.","...#.","#..#.",".##.."])
    , ('K', ["#...#","#..#.","#.#..","##...","#.#..","#..#.","#...#"])
    , ('L', ["#....","#....","#....","#....","#....","#....","#####"])
    , ('M', ["#...#","##.##","#.#.#","#.#.#","#...#","#...#","#...#"])
    , ('N', ["#...#","##..#","##..#","#.#.#","#..##","#..##","#...#"])
    , ('O', [".###.","#...#","#...#","#...#","#...#","#...#",".###."])
    , ('P', ["####.","#...#","#...#","####.","#....","#....","#...."])
    , ('Q', [".###.","#...#","#...#","#...#","#.#.#","#..#.",".##.#"])
    , ('R', ["####.","#...#","#...#","####.","#.#..","#..#.","#...#"])
    , ('S', [".####","#....","#....",".###.","....#","....#","####."])
    , ('T', ["#####","..#..","..#..","..#..","..#..","..#..","..#.."])
    , ('U', ["#...#","#...#","#...#","#...#","#...#","#...#",".###."])
    , ('V', ["#...#","#...#","#...#","#...#","#...#",".#.#.","..#.."])
    , ('W', ["#...#","#...#","#...#","#.#.#","#.#.#","##.##","#...#"])
    , ('X', ["#...#","#...#",".#.#.","..#..",".#.#.","#...#","#...#"])
    , ('Y', ["#...#","#...#",".#.#.","..#..","..#..","..#..","..#.."])
    , ('Z', ["#####","....#","...#.","..#..",".#...","#....","#####"])
    , ('0', [".###.","#...#","#..##","#.#.#","##..#","#...#",".###."])
    , ('1', ["..#..",".##..","..#..","..#..","..#..","..#..",".###."])
    , ('2', [".###.","#...#","....#","...#.","..#..",".#...","#####"])
    , ('3', ["####.","....#","....#",".###.","....#","....#","####."])
    , ('4', ["...#.","..##.",".#.#.","#..#.","#####","...#.","...#."])
    , ('5', ["#####","#....","#....","####.","....#","....#","####."])
    , ('6', [".###.","#....","#....","####.","#...#","#...#",".###."])
    , ('7', ["#####","....#","...#.","..#..",".#...",".#...",".#..."])
    , ('8', [".###.","#...#","#...#",".###.","#...#","#...#",".###."])
    , ('9', [".###.","#...#","#...#",".####","....#","....#",".###."])
    , ('.', [".....",".....",".....",".....",".....","..#..","..#.."])
    , (',', [".....",".....",".....",".....","..#..","..#..",".#..."])
    , ('-', [".....",".....",".....","#####",".....",".....","....."])
    , ('+', [".....","..#..","..#..","#####","..#..","..#..","....."])
    , ('/', ["....#","....#","...#.","..#..",".#...","#....","#...."])
    , (':', [".....","..#..","..#..",".....","..#..","..#..","....."])
    , ('(', ["...#.","..#..",".#...",".#...",".#...","..#..","...#."])
    , (')', [".#...","..#..","...#.","...#.","...#.","..#..",".#..."])
    , ('%', ["##..#","##..#","...#.","..#..",".#...","#..##","#..##"])
    , ('_', [".....",".....",".....",".....",".....",".....","#####"])
    , ('=', [".....",".....","#####",".....","#####",".....","....."])
    , ('$', ["..#..",".####","#.#..",".###.","..#.#","####.","..#.."])
    ]
