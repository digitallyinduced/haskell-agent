module Agent.Json.Decoder.Portable
    ( DecodeError(..)
    , PathElement(..)
    , decode
    , renderDecodeError
    ) where

import Agent.Json.Decoder.Backend
import Agent.Json.Internal (RawJson(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (chr)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Text.Read (readMaybe)

data PathElement
    = PathKey !Text
    | PathIndex !Int
    deriving stock (Eq, Show)

data DecodeError = DecodeError
    { offset :: !Int
    , path :: ![PathElement]
    , message :: !Text
    }
    deriving stock (Eq, Show)

data Cursor = Cursor
    { input :: !BS.ByteString
    , position :: !Int
    , pathRev :: ![PathElement]
    , depth :: !Int
    }

decode :: Decoder a -> BS.ByteString -> Either DecodeError a
decode decoder bytes = do
    (value, cursor) <- runDecoder decoder (Cursor bytes 0 [] 0)
    let finished = skipWhitespace cursor
    if finished.position == BS.length bytes
        then Right value
        else failure finished "expected end of input"

renderDecodeError :: DecodeError -> Text
renderDecodeError DecodeError { path, offset, message } =
    "JSON decode error at byte "
        <> Text.pack (show offset)
        <> renderPath path
        <> ": "
        <> message
  where
    renderPath [] = ""
    renderPath values = " at " <> Text.pack (concatMap renderSegment values)
    renderSegment segment =
        case segment of
            PathKey key -> "/" <> Text.unpack (Text.replace "/" "~1" (Text.replace "~" "~0" key))
            PathIndex index -> "/" <> show index

runDecoder :: Decoder a -> Cursor -> Either DecodeError (a, Cursor)
runDecoder decoder initial = case decoder of
    NullDecoder value -> do
        cursor <- consumeLiteral "null" initial
        pure (value, cursor)
    BoolDecoder -> parseBool initial
    TextDecoder -> parseString (skipWhitespace initial)
    ScientificDecoder -> parseScientific (skipWhitespace initial)
    ArrayDecoder elementDecoder ->
        parseArray elementDecoder initial
    ObjectDecoder state fields unknown finish ->
        parseObject state fields unknown finish initial
    PlannedObjectDecoder plan ->
        parsePlannedObject plan initial
    NullableDecoder inner ->
        let cursor = skipWhitespace initial
        in if literalAt "null" cursor
            then (Nothing,) <$> consumeLiteral "null" cursor
            else do
                (value, next) <- runDecoder inner cursor
                pure (Just value, next)
    ByTypeDecoder select -> do
        valueType <- jsonTypeAt initial
        runDecoder (select valueType) initial
    RawJsonDecoder -> do
        let start = (skipWhitespace initial).position
        next <- skipJsonValue initial
        let bytes =
                BS.copy $
                    BS.take (next.position - start) $
                        BS.drop start initial.input
        pure (RawJson bytes, next)
    SkipDecoder ->
        ((),) <$> skipJsonValue initial
    MapDecoder transform inner -> do
        (value, next) <- runDecoder inner initial
        case transform value of
            Left err -> failure next err
            Right transformed -> pure (transformed, next)

jsonTypeAt :: Cursor -> Either DecodeError JsonType
jsonTypeAt initial =
    let cursor = skipWhitespace initial
    in case peekByte cursor of
        Just byte
            | byte == 0x6e -> Right JsonNull
            | byte == 0x74 || byte == 0x66 -> Right JsonBoolean
            | byte == quote -> Right JsonString
            | byte == openBracket -> Right JsonArray
            | byte == openBrace -> Right JsonObject
            | byte == minus || isDigit byte -> Right JsonNumber
        _ -> failure cursor "expected a JSON value"

parseBool :: Cursor -> Either DecodeError (Bool, Cursor)
parseBool initial
    | literalAt "true" cursor =
        (True,) <$> consumeLiteral "true" cursor
    | literalAt "false" cursor =
        (False,) <$> consumeLiteral "false" cursor
    | otherwise = failure cursor "expected a boolean"
  where
    cursor = skipWhitespace initial

parseArray
    :: Decoder a
    -> Cursor
    -> Either DecodeError ([a], Cursor)
parseArray elementDecoder initial = do
    opened <- descend =<< consumeByte openBracket (skipWhitespace initial)
    let cursor = skipWhitespace opened
    if peekByte cursor == Just closeBracket
        then ([],) <$> (leaveDepth <$> consumeByte closeBracket cursor)
        else go 0 [] cursor
  where
    go index reversed cursor = do
        nested <- pure (enter (PathIndex index) cursor)
        (value, afterValue) <- runDecoder elementDecoder nested
        let afterNested = leave afterValue
            next = skipWhitespace afterNested
        case peekByte next of
            Just byte | byte == comma ->
                consumeByte comma next
                    >>= go (index + 1) (value : reversed)
                    . skipWhitespace
            Just byte | byte == closeBracket -> do
                finished <- consumeByte closeBracket next
                pure (reverse (value : reversed), leaveDepth finished)
            _ -> failure next "expected ',' or ']'"

parseObject
    :: state
    -> [NamedField state]
    -> UnknownField state
    -> (state -> Either Text a)
    -> Cursor
    -> Either DecodeError (a, Cursor)
parseObject initialState fields unknown finish initial = do
    opened <- descend =<< consumeByte openBrace (skipWhitespace initial)
    let cursor = skipWhitespace opened
    if peekByte cursor == Just closeBrace
        then do
            finished <- consumeByte closeBrace cursor
            finishObject initialState (leaveDepth finished)
        else go initialState cursor
  where
    go state cursor = do
        (key, afterKey) <- parseString cursor
        afterColon <-
            consumeByte colon (skipWhitespace afterKey)
        nested <- pure (enter (PathKey key) afterColon)
        (updatedState, afterValue) <-
            decodeObjectField key state nested fields unknown
        let next = skipWhitespace afterValue
        case peekByte next of
            Just byte | byte == comma ->
                consumeByte comma next
                    >>= go updatedState . skipWhitespace
            Just byte | byte == closeBrace -> do
                finished <- consumeByte closeBrace next
                finishObject updatedState (leaveDepth finished)
            _ -> failure next "expected ',' or '}'"

    finishObject state cursor =
        case finish state of
            Left err -> failure cursor err
            Right value -> pure (value, cursor)

decodeObjectField
    :: Text
    -> state
    -> Cursor
    -> [NamedField state]
    -> UnknownField state
    -> Either DecodeError (state, Cursor)
decodeObjectField key state cursor fields unknown =
    case fields of
        [] -> case unknown of
            UnknownField valueDecoder update -> do
                (value, decodedCursor) <- runDecoder valueDecoder cursor
                finishUpdate decodedCursor (update key value state)
        NamedField name valueDecoder update : rest
            | name == key -> do
                (value, decodedCursor) <- runDecoder valueDecoder cursor
                finishUpdate decodedCursor (update value state)
            | otherwise ->
                decodeObjectField key state cursor rest unknown
  where
    finishUpdate decodedCursor = \case
        Left err -> failure decodedCursor err
        Right updated -> pure (updated, leave decodedCursor)

parsePlannedObject
    :: ObjectPlan a
    -> Cursor
    -> Either DecodeError (a, Cursor)
parsePlannedObject initialPlan initial = do
    opened <- consumeByte openBrace (skipWhitespace initial)
    let cursor = skipWhitespace opened
    if peekByte cursor == Just closeBrace
        then do
            finished <- consumeByte closeBrace cursor
            finishPlan initialPlan finished
        else go initialPlan cursor
  where
    go plan cursor = do
        (key, afterKey) <- parseString cursor
        afterColon <- consumeByte colon (skipWhitespace afterKey)
        let nested = enter (PathKey key) afterColon
        (updatedPlan, afterValue) <-
            case matchPlannedField key plan of
                Just (PlannedFieldMatch decoder rebuild) -> do
                    (value, decodedCursor) <- runDecoder decoder nested
                    pure (rebuild value, leave decodedCursor)
                Nothing
                    | objectPlanCapturesExtensions plan -> do
                        (value, decodedCursor) <-
                            runDecoder RawJsonDecoder nested
                        pure
                            ( capturePlannedExtension key value plan
                            , leave decodedCursor
                            )
                    | otherwise -> do
                        ((), decodedCursor) <-
                            runDecoder SkipDecoder nested
                        pure (plan, leave decodedCursor)
        let next = skipWhitespace afterValue
        case peekByte next of
            Just byte | byte == comma ->
                consumeByte comma next >>= go updatedPlan . skipWhitespace
            Just byte | byte == closeBrace -> do
                finished <- consumeByte closeBrace next
                finishPlan updatedPlan finished
            _ -> failure next "expected ',' or '}'"

    finishPlan plan cursor =
        case finishObjectPlan plan of
            Left err -> failure cursor err
            Right value -> pure (value, cursor)

parseString :: Cursor -> Either DecodeError (Text, Cursor)
parseString initial = do
    afterQuote <- consumeByte quote (skipWhitespace initial)
    go [] afterQuote.position afterQuote
  where
    go reversedChunks chunkStart cursor =
        case peekByte cursor of
            Nothing -> failure cursor "unterminated string"
            Just byte
                | byte == quote -> do
                    chunk <- decodeUtf8Chunk cursor chunkStart cursor.position
                    finished <- consumeByte quote cursor
                    pure
                        (Text.concat (reverse (chunk : reversedChunks)), finished)
                | byte == backslash -> do
                    chunk <- decodeUtf8Chunk cursor chunkStart cursor.position
                    (escaped, afterEscape) <- parseEscape (advance 1 cursor)
                    go
                        (escaped : chunk : reversedChunks)
                        afterEscape.position
                        afterEscape
                | byte < 0x20 ->
                    failure cursor "unescaped control character in string"
                | otherwise ->
                    go reversedChunks chunkStart (advance 1 cursor)

decodeUtf8Chunk :: Cursor -> Int -> Int -> Either DecodeError Text
decodeUtf8Chunk cursor start end =
    case TextEncoding.decodeUtf8' (BS.take (end - start) (BS.drop start cursor.input)) of
        Left err ->
            failure cursor
                ("invalid UTF-8 in string: " <> Text.pack (show err))
        Right value -> Right value

parseEscape :: Cursor -> Either DecodeError (Text, Cursor)
parseEscape cursor =
    case peekByte cursor of
        Nothing -> failure cursor "unterminated string escape"
        Just byte -> case byte of
            0x22 -> simple "\""
            0x5c -> simple "\\"
            0x2f -> simple "/"
            0x62 -> simple "\b"
            0x66 -> simple "\f"
            0x6e -> simple "\n"
            0x72 -> simple "\r"
            0x74 -> simple "\t"
            0x75 -> parseUnicodeEscape (advance 1 cursor)
            _ -> failure cursor "invalid string escape"
  where
    simple value = Right (value, advance 1 cursor)

parseUnicodeEscape :: Cursor -> Either DecodeError (Text, Cursor)
parseUnicodeEscape cursor = do
    (first, afterFirst) <- parseHex4 cursor
    if isHighSurrogate first
        then do
            afterSlash <- consumeByte backslash afterFirst
            afterU <- consumeByte 0x75 afterSlash
            (second, afterSecond) <- parseHex4 afterU
            if isLowSurrogate second
                then pure
                    ( Text.singleton
                        (chr
                            ( 0x10000
                                + (first - 0xd800) * 0x400
                                + second
                                - 0xdc00
                            ))
                    , afterSecond
                    )
                else failure afterU "expected a low surrogate"
        else if isLowSurrogate first
            then failure cursor "unexpected low surrogate"
            else pure (Text.singleton (chr first), afterFirst)

parseHex4 :: Cursor -> Either DecodeError (Int, Cursor)
parseHex4 initial =
    go 4 0 initial
  where
    go :: Int -> Int -> Cursor -> Either DecodeError (Int, Cursor)
    go 0 value cursor = Right (value, cursor)
    go remaining value cursor = case peekByte cursor >>= hexValue of
        Nothing -> failure cursor "expected four hexadecimal digits"
        Just digit ->
            go (remaining - 1) (value * 16 + digit) (advance 1 cursor)

hexValue :: Word8 -> Maybe Int
hexValue byte
    | byte >= 0x30 && byte <= 0x39 =
        Just (fromIntegral byte - 0x30)
    | byte >= 0x41 && byte <= 0x46 =
        Just (fromIntegral byte - 0x41 + 10)
    | byte >= 0x61 && byte <= 0x66 =
        Just (fromIntegral byte - 0x61 + 10)
    | otherwise = Nothing

parseScientific :: Cursor -> Either DecodeError (Scientific, Cursor)
parseScientific cursor = do
    end <- scanNumber cursor
    let bytes =
            BS.take (end.position - cursor.position) $
                BS.drop cursor.position cursor.input
    case readMaybe (BS8.unpack bytes) of
        Just value -> Right (value, end)
        Nothing -> failure cursor "invalid JSON number"

scanNumber :: Cursor -> Either DecodeError Cursor
scanNumber initial = do
    let afterMinus =
            if peekByte initial == Just minus
                then advance 1 initial
                else initial
    afterInteger <- case peekByte afterMinus of
        Just 0x30 ->
            let next = advance 1 afterMinus
            in case peekByte next of
                Just byte | isDigit byte ->
                    failure next "leading zero in number"
                _ -> Right next
        Just byte | byte >= 0x31 && byte <= 0x39 ->
            Right (takeWhileByte isDigit afterMinus)
        _ -> failure afterMinus "expected a number"
    afterFraction <- if peekByte afterInteger == Just dot
        then do
            let digits = takeWhileByte isDigit (advance 1 afterInteger)
            if digits.position == afterInteger.position + 1
                then failure digits "expected digits after decimal point"
                else Right digits
        else Right afterInteger
    case peekByte afterFraction of
        Just byte | byte == exponentLower || byte == exponentUpper -> do
            let afterExponent = advance 1 afterFraction
                afterSign = case peekByte afterExponent of
                    Just sign | sign == plus || sign == minus ->
                        advance 1 afterExponent
                    _ -> afterExponent
                digits = takeWhileByte isDigit afterSign
            if digits.position == afterSign.position
                then failure digits "expected exponent digits"
                else Right digits
        _ -> Right afterFraction

skipJsonValue :: Cursor -> Either DecodeError Cursor
skipJsonValue initial =
    let cursor = skipWhitespace initial
    in case peekByte cursor of
        Just byte
            | byte == quote -> snd <$> parseString cursor
            | byte == openBrace -> skipObject cursor
            | byte == openBracket -> skipArray cursor
            | byte == 0x74 -> consumeLiteral "true" cursor
            | byte == 0x66 -> consumeLiteral "false" cursor
            | byte == 0x6e -> consumeLiteral "null" cursor
            | byte == minus || isDigit byte -> scanNumber cursor
        _ -> failure cursor "expected a JSON value"

skipObject :: Cursor -> Either DecodeError Cursor
skipObject initial = do
    cursor <- descend initial >>= consumeByte openBrace
    let next = skipWhitespace cursor
    if peekByte next == Just closeBrace
        then leaveDepth <$> consumeByte closeBrace next
        else go next
  where
    go cursor = do
        (_, afterKey) <- parseString cursor
        afterColon <- consumeByte colon (skipWhitespace afterKey)
        afterValue <- skipJsonValue afterColon
        let next = skipWhitespace afterValue
        case peekByte next of
            Just byte | byte == comma ->
                consumeByte comma next >>= go . skipWhitespace
            Just byte | byte == closeBrace ->
                leaveDepth <$> consumeByte closeBrace next
            _ -> failure next "expected ',' or '}'"

skipArray :: Cursor -> Either DecodeError Cursor
skipArray initial = do
    cursor <- descend initial >>= consumeByte openBracket
    let next = skipWhitespace cursor
    if peekByte next == Just closeBracket
        then leaveDepth <$> consumeByte closeBracket next
        else go next
  where
    go cursor = do
        afterValue <- skipJsonValue cursor
        let next = skipWhitespace afterValue
        case peekByte next of
            Just byte | byte == comma ->
                consumeByte comma next >>= go . skipWhitespace
            Just byte | byte == closeBracket ->
                leaveDepth <$> consumeByte closeBracket next
            _ -> failure next "expected ',' or ']'"

skipWhitespace :: Cursor -> Cursor
skipWhitespace =
    takeWhileByte isWhitespace

takeWhileByte :: (Word8 -> Bool) -> Cursor -> Cursor
takeWhileByte predicate cursor =
    cursor
        { position =
            cursor.position
                + BS.length
                    (BS.takeWhile predicate (BS.drop cursor.position cursor.input))
        }

consumeLiteral :: BS.ByteString -> Cursor -> Either DecodeError Cursor
consumeLiteral literal initial =
    let cursor = skipWhitespace initial
    in if literalAt literal cursor
        then Right (advance (BS.length literal) cursor)
        else failure cursor ("expected " <> TextEncoding.decodeUtf8 literal)

literalAt :: BS.ByteString -> Cursor -> Bool
literalAt literal cursor =
    literal `BS.isPrefixOf` BS.drop cursor.position cursor.input

consumeByte :: Word8 -> Cursor -> Either DecodeError Cursor
consumeByte expected cursor =
    case peekByte cursor of
        Just actual | actual == expected -> Right (advance 1 cursor)
        _ ->
            failure cursor
                ("expected '" <> Text.singleton (chr (fromIntegral expected)) <> "'")

peekByte :: Cursor -> Maybe Word8
peekByte cursor =
    BS.indexMaybe cursor.input cursor.position

advance :: Int -> Cursor -> Cursor
advance amount cursor =
    cursor { position = cursor.position + amount }

enter :: PathElement -> Cursor -> Cursor
enter element cursor =
    cursor
        { pathRev = element : cursor.pathRev }

descend :: Cursor -> Either DecodeError Cursor
descend cursor
    | cursor.depth >= maximumDepth =
        failure cursor "maximum JSON nesting depth exceeded"
    | otherwise =
        Right cursor { depth = cursor.depth + 1 }

leave :: Cursor -> Cursor
leave cursor =
    cursor
        { pathRev = drop 1 cursor.pathRev }

leaveDepth :: Cursor -> Cursor
leaveDepth cursor =
    cursor { depth = max 0 (cursor.depth - 1) }

failure :: Cursor -> Text -> Either DecodeError a
failure cursor reason =
    Left DecodeError
        { offset = cursor.position
        , path = reverse cursor.pathRev
        , message = reason
        }

isWhitespace :: Word8 -> Bool
isWhitespace byte =
    byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d

isDigit :: Word8 -> Bool
isDigit byte =
    byte >= 0x30 && byte <= 0x39

isHighSurrogate, isLowSurrogate :: Int -> Bool
isHighSurrogate value = value >= 0xd800 && value <= 0xdbff
isLowSurrogate value = value >= 0xdc00 && value <= 0xdfff

maximumDepth :: Int
maximumDepth = 1024

quote, backslash, openBrace, closeBrace, openBracket, closeBracket :: Word8
quote = 0x22
backslash = 0x5c
openBrace = 0x7b
closeBrace = 0x7d
openBracket = 0x5b
closeBracket = 0x5d

comma, colon, minus, plus, dot, exponentLower, exponentUpper :: Word8
comma = 0x2c
colon = 0x3a
minus = 0x2d
plus = 0x2b
dot = 0x2e
exponentLower = 0x65
exponentUpper = 0x45
