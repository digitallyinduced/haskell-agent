-- | Validated, versioned presentation documents. No chart input is executed.
module Agent.Tools.RenderChart
    ( renderChartTool
    , renderChartToolName
    , renderChartResult
    , chartResultDocument
    , chartResultSummary
    , chartResultFallback
    ) where

import Agent.ToolDispatch (textTool)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..), jsonTool)
import Control.Monad (unless, when)
import Data.Aeson (Value(..), Object, eitherDecodeStrict', encode, object, (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Scientific (toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Numeric (showGFloat)

renderChartToolName :: Text
renderChartToolName = "render_chart"

renderChartTool :: AppTool
renderChartTool =
    jsonTool renderChartToolName description properties True ParallelSafe
        (textTool renderChartToolName (pure . renderChartResult))
  where
    description =
        "Present an interactive native chart in the conversation. Use line/area for trends, \
        \bar for comparisons, scatter for relationships. Supply actual data, clear axis \
        \labels and units; never invent missing values. This tool does not execute Swift. \
        \Use version 1; at most 8 series, 2000 total points, 256 KiB JSON, and 200 characters \
        \per text field. Numeric/time x values in each line/area series must be strictly \
        \increasing. Timestamps are UTC YYYY-MM-DDTHH:mm:ss[.fraction]Z with at most \
        \3 fractional digits (milliseconds). Aggregate large \
        \datasets first and describe the aggregation in subtitle. Missing values are \
        \not supported: omit missing observations and disclose gaps in subtitle."
    properties =
        [ PropertySchema "version" PropertyInteger True (Just "Document version; must be 1.")
        , PropertySchema "kind" (PropertyEnum ["line", "bar", "area", "scatter"]) True Nothing
        , PropertySchema "title" PropertyString True (Just "Nonempty descriptive chart title.")
        , PropertySchema "subtitle" PropertyString False Nothing
        , PropertySchema "x_axis" (PropertyObject
            (PropertySchema "type" (PropertyEnum ["category", "number", "timestamp"]) True Nothing : axisLabels))
            True Nothing
        , PropertySchema "y_axis" (PropertyObject
            (PropertySchema "type" (PropertyEnum ["number"]) False Nothing : axisLabels))
            True (Just "Numeric y-axis; type may be omitted.")
        , PropertySchema "series" (PropertyArray (PropertyObject
            [ PropertySchema "name" PropertyString True (Just "Unique nonempty series name.")
            , PropertySchema "points" (PropertyArray (PropertyObject
                [ PropertySchema "x" (PropertyRaw (object
                    [ "anyOf" .=
                        [ object ["type" .= ("string" :: Text)]
                        , object ["type" .= ("number" :: Text)]
                        ]
                    ])) True (Just "Category string, finite number, or UTC timestamp, matching x_axis.type.")
                , PropertySchema "y" PropertyNumber True (Just "Finite numeric value.")
                ])) True Nothing
            ])) True Nothing
        ]
    axisLabels =
        [ PropertySchema "label" PropertyString False Nothing
        , PropertySchema "unit" PropertyString False Nothing
        ]

maximumDocumentBytes :: Int
maximumDocumentBytes = 256 * 1024

-- | The result itself is the authoritative persisted protocol document. Native
-- projections extract its chart before any display-preview truncation.
renderChartResult :: Text -> Either Text Text
renderChartResult input = do
    let bytes = TextEncoding.encodeUtf8 input
    when (BS.length bytes > maximumDocumentBytes) $
        Left "render_chart: document exceeds 256 KiB; aggregate the data first."
    value <- either (Left . Text.pack) Right (eitherDecodeStrict' bytes)
    chart <- either (Left . ("render_chart: " <>) . Text.pack) Right
        (parseEither validateDocument value)
    pure (encodeText (object
        [ "type" .= ("chart" :: Text), "chart" .= value, "summary" .= describeSummary chart ]))

chartResultDocument :: Text -> Maybe Text
chartResultDocument = fmap (encodeText . fst) . parseChartResult

chartResultSummary :: Text -> Maybe Text
chartResultSummary = fmap (describeSummary . snd) . parseChartResult

-- | Accessible plain-text presentation shared by terminal views. Derive every
-- label from the validated document, never from stored summary prose. Preserve
-- Unicode even when a client's bitmap font cannot display it.
chartResultFallback :: Text -> Maybe Text
chartResultFallback = fmap (describeChart . snd) . parseChartResult

describeChart :: Chart -> Text
describeChart chart =
    Text.intercalate "\n" $
        filter (not . Text.null)
            [ safeLabel chart.title <> " [" <> kindText chart.kind <> " chart]"
            , safeLabel chart.subtitle
            , "X: " <> describeAxis chart.xAxis <> "; Y: " <> describeAxis chart.yAxis
            , if null categories then "" else
                "Categories: " <> Text.intercalate ", " categories
            ] <> map describeSeries (NonEmpty.toList chart.series)
  where
    categories = nub
        [ safeLabel category
        | series <- NonEmpty.toList chart.series
        , point <- NonEmpty.toList series.points
        , CategoryCoordinate category <- [point.x]
        ]
    describeAxis :: ChartAxis -> Text
    describeAxis axis =
        safeLabel axis.label <> if Text.null axis.unit then "" else " (" <> safeLabel axis.unit <> ")"
    describeSeries :: ChartSeries -> Text
    describeSeries series =
        let first :| rest = fmap (.y) series.points
        in safeLabel series.name <> ": " <> Text.pack (show (length series.points))
            <> (if length series.points == 1 then " point; range " else " points; range ")
            <> numberText (foldl min first rest) <> " to " <> numberText (foldl max first rest)
    safeLabel = Text.unwords . Text.words . Text.map \character ->
        if isControl character then ' ' else character
    numberText value
        | value == 0 = "0"
        | otherwise = Text.pack (showGFloat (Just 3) value "")

parseChartResult :: Text -> Maybe (Value, Chart)
parseChartResult input = do
    -- Envelope text adds only a bounded generated summary to the document.
    let bytes = TextEncoding.encodeUtf8 input
    unless (BS.length bytes <= maximumDocumentBytes + 4096) Nothing
    value <- either (const Nothing) Just (eitherDecodeStrict' bytes)
    parseMaybe (withObject "chart result" \fields -> do
        exactFields ["type", "chart", "summary"] fields
        resultType <- fields .: "type"
        unless (resultType == ("chart" :: Text)) (fail "Not a chart result")
        chart <- fields .: "chart"
        validated <- validateDocument chart
        -- Recompute the fallback from validated data, not untrusted stored prose.
        pure (chart, validated)) value

encodeText :: Value -> Text
encodeText = TextEncoding.decodeUtf8 . LBS.toStrict . encode

-- Constructors stay private: rendering only consumes data accepted by validation.
-- Keep the original Value separately for native consumers, preserving omitted/null
-- metadata and exact JSON numbers rather than round-tripping through Double.
data Chart = Chart
    { kind :: ChartKind
    , title :: Text
    , subtitle :: Text
    , xAxis :: ChartAxis
    , yAxis :: ChartAxis
    , series :: NonEmpty ChartSeries
    }

data ChartKind = Line | Bar | Area | Scatter deriving (Eq)
data AxisType = CategoryAxis | NumberAxis | TimestampAxis deriving (Eq)
data ChartAxis = ChartAxis { axisType :: AxisType, label :: Text, unit :: Text }
data ChartSeries = ChartSeries { name :: Text, points :: NonEmpty ChartPoint }
data ChartPoint = ChartPoint { x :: ChartCoordinate, y :: Double }

kindText :: ChartKind -> Text
kindText Line = "line"
kindText Bar = "bar"
kindText Area = "area"
kindText Scatter = "scatter"

describeSummary :: Chart -> Text
describeSummary chart =
    let pointCount = sum (fmap (length . (.points)) chart.series)
    in "Rendered " <> chart.title <> " — " <> kindText chart.kind <> " chart; "
        <> Text.pack (show (length chart.series)) <> " series, "
        <> Text.pack (show pointCount)
        <> (if pointCount == 1 then " point." else " points.")

validateDocument :: Value -> Parser Chart
validateDocument value = do
    unless (LBS.length (encode value) <= fromIntegral maximumDocumentBytes) $
        fail "Document exceeds 256 KiB"
    withObject "chart document" (\fields -> do
        exactFields ["version", "kind", "title", "subtitle", "x_axis", "y_axis", "series"] fields
        version <- fields .: "version" :: Parser Int
        unless (version == 1) (fail "Unsupported chart version; use version 1")
        kind <- fields .: "kind" >>= \case
            ("line" :: Text) -> pure Line
            "bar" -> pure Bar
            "area" -> pure Area
            "scatter" -> pure Scatter
            _ -> fail "kind must be line, bar, area, or scatter"
        title <- requiredText fields "title"
        subtitle <- optionalText fields "subtitle"
        xAxis <- fields .: "x_axis" >>= validateAxis False
        yAxis <- fields .: "y_axis" >>= validateAxis True
        series <- fields .: "series" :: Parser [Value]
        nonemptySeries <- boundedNonEmpty 8 "Supply between 1 and 8 series" series
        seriesInfo <- traverse (validateSeries kind xAxis.axisType) nonemptySeries
        let names = fmap (.name) seriesInfo
            pointCount = sum (fmap (length . (.points)) seriesInfo)
        unless (Set.size (Set.fromList (NonEmpty.toList names)) == length names) $
            fail "Series names must be unique"
        unless (pointCount <= 2000) (fail "At most 2000 total points; aggregate the data first")
        pure (Chart kind title subtitle xAxis yAxis seriesInfo)
        ) value

validateAxis :: Bool -> Value -> Parser ChartAxis
validateAxis numericOnly = withObject "axis" \fields -> do
    exactFields ["type", "label", "unit"] fields
    axisType <- if numericOnly
        then maybe "number" id <$> fields .:? "type"
        else fields .: "type" :: Parser Text
    typedAxis <- case axisType of
        "number" -> pure NumberAxis
        "category" -> pure CategoryAxis
        "timestamp" -> pure TimestampAxis
        _ -> fail "Axis type is unsupported; y_axis must be numeric"
    when (numericOnly && typedAxis /= NumberAxis) $
        fail "Axis type is unsupported; y_axis must be numeric"
    label <- optionalText fields "label"
    unit <- optionalText fields "unit"
    pure (ChartAxis typedAxis label unit)

data ChartCoordinate = NumericCoordinate Double | TimestampCoordinate UTCTime | CategoryCoordinate Text
    deriving (Eq, Ord)

boundedNonEmpty :: Int -> String -> [a] -> Parser (NonEmpty a)
boundedNonEmpty limit message values = case NonEmpty.nonEmpty values of
    Just nonempty | length nonempty <= limit -> pure nonempty
    _ -> fail message

validateSeries :: ChartKind -> AxisType -> Value -> Parser ChartSeries
validateSeries kind axisType = withObject "series" \fields -> do
    exactFields ["name", "points"] fields
    name <- requiredText fields "name"
    points <- fields .: "points" :: Parser [Value]
    nonemptyPoints <- boundedNonEmpty 2000 "Each series must contain between 1 and 2000 points" points
    validatedPoints <- traverse (validatePoint axisType) nonemptyPoints
    let coordinates = map (.x) (NonEmpty.toList validatedPoints)
    when (kind `elem` [Line, Area] && axisType /= CategoryAxis) $
        unless (and (zipWith coordinateIncreasing coordinates (drop 1 coordinates))) $
            fail "Line/area x values must be strictly increasing"
    pure (ChartSeries name validatedPoints)

coordinateIncreasing :: ChartCoordinate -> ChartCoordinate -> Bool
coordinateIncreasing left right = left < right

validatePoint :: AxisType -> Value -> Parser ChartPoint
validatePoint axisType = withObject "point" \fields -> do
    exactFields ["x", "y"] fields
    y <- fields .: "y" >>= finiteNumber
    x <- case axisType of
        NumberAxis -> NumericCoordinate <$> (fields .: "x" >>= finiteNumber)
        TimestampAxis -> do
            text <- requiredText fields "x"
            maybe (fail "Timestamp must be a valid UTC YYYY-MM-DDTHH:mm:ss[.fraction]Z")
                (pure . TimestampCoordinate) (parseTimestamp text)
        CategoryAxis -> CategoryCoordinate <$> requiredText fields "x"
    pure (ChartPoint x y)

finiteNumber :: Value -> Parser Double
finiteNumber (Number number) =
    let value = toRealFloat number
    in if isInfinite value || isNaN value
        then fail "Chart numbers must be finite"
        else pure value
finiteNumber _ = fail "Expected a finite number"

parseTimestamp :: Text -> Maybe UTCTime
parseTimestamp text = do
    let source = Text.unpack text
        fixed = take 19 source
        suffix = drop 19 source
        digits = [character | (index, character) <- zip [0 :: Int ..] fixed,
            index `notElem` [4, 7, 10, 13, 16]]
        asciiDigit character = character >= '0' && character <= '9'
        fractional = case suffix of
            "Z" -> True
            '.' : rest -> let fraction = takeWhile asciiDigit rest
                in not (null fraction) && length fraction <= 3
                    && drop (length fraction) rest == "Z"
            _ -> False
    unless (length fixed == 19 && all asciiDigit digits && fractional
        && take 4 fixed /= "0000"
        && [fixed !! index | index <- [4, 7, 10, 13, 16]] == "--T::"
        && take 2 (drop 17 fixed) < "60") Nothing
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" source

requiredText :: Object -> Key.Key -> Parser Text
requiredText fields key = do
    text <- fields .: key
    validateText text
    unless (not (Text.null (Text.strip text))) $
        fail (Text.unpack (Key.toText key) <> " must not be blank")
    pure text

optionalText :: Object -> Key.Key -> Parser Text
optionalText fields key = do
    text <- fields .:? key .!= ""
    validateText text
    pure text

validateText :: Text -> Parser ()
validateText text = unless (Text.length text <= 200) $
    fail "Text fields must not exceed 200 Unicode scalar values"

exactFields :: [Key.Key] -> Object -> Parser ()
exactFields allowed fields =
    unless (all (`elem` allowed) (KeyMap.keys fields)) $
        fail "Unknown chart field"
