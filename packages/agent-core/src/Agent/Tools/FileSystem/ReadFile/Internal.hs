{-# LANGUAGE BangPatterns #-}

module Agent.Tools.FileSystem.ReadFile.Internal
    ( ReadFileArgs(..)
    , FileWindow(..)
    , readFileArgsDecoder
    , runReadFile
    , streamReadFile
    , readFileWindowForArgs
    , fileWindowCoversArgs
    , formatFileWindow
    , formatReadFileContent
    ) where

import Agent.FileRetry (retryOnFileBusy)
import Agent.Json.Decode (Decoder)
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optInt, optText, reqText)
import Agent.Tools.IO (displayPathInWorkspace, resolveForRead)
import Agent.Tools.Types (ToolEnv)
import Control.Exception.Safe (SomeException, try)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import System.Directory.OsPath (doesFileExist)
import System.IO
    ( BufferMode(..)
    , Handle
    , IOMode(..)
    , SeekMode(..)
    , hIsEOF
    , hSeek
    , hSetBuffering
    , withBinaryFile
    )
import System.OsPath (OsPath)

data ReadFileArgs = ReadFileArgs
    { targetFile :: Text
    , offset :: Maybe Int
    , limit :: Maybe Int
    , pages :: Maybe Text
    , format :: Maybe Text
    } deriving (Eq, Show)

readFileArgsDecoder :: Decoder ReadFileArgs
readFileArgsDecoder = objectArgs \object -> ReadFileArgs
    <$> reqText object "target_file"
    <*> optInt object "offset"
    <*> optInt object "limit"
    <*> optText object "pages"
    <*> optText object "format"

maxReadLines :: Int
maxReadLines = 1000

maxReadTokens :: Int
maxReadTokens = 25000

runReadFile :: ToolEnv -> ReadFileArgs -> IO (Either Text Text)
runReadFile env args =
    resolveForRead env (fromText args.targetFile) >>= \case
        Left err -> pure (Left err)
        Right path
            | ".pdf" `Text.isSuffixOf` Text.toLower args.targetFile ->
                pure $ Left
                    "PDF rendering is not available. Use an explicit terminal conversion tool if available, or convert the file to text first."
            | otherwise -> doesFileExist path >>= \case
                False -> do
                    display <- displayPathInWorkspace env path
                    pure $ Left $ "File not found: " <> display
                True -> do
                    _ <- pure (args.pages, args.format)
                    try @_ @SomeException (streamReadFile path args) >>= \case
                        Left err ->
                            pure $ Left $
                                "Failed to read file: " <> Text.pack (show err)
                        Right result -> pure result

-- | A raw leading window can serve later streamed range arguments when it
-- covers them. An exact window stores the already-formatted result of the
-- bounded two-pass reader and is reusable only for identical arguments.
data FileWindow
    = RawFileWindow
        { fileWindowText :: !Text
        , fileWindowComplete :: !Bool
        }
    | ExactFileWindow
        { fileWindowText :: !Text
        , fileWindowArguments :: !ReadFileArgs
        }

-- | Build a bounded speculative result. Leading windows stay raw so later
-- offset/limit fields can be applied; non-leading ranges use the normal
-- bounded reader and cache only the exact argument set.
readFileWindowForArgs :: OsPath -> ReadFileArgs -> IO (Either Text FileWindow)
readFileWindowForArgs path args
    | ".pdf" `Text.isSuffixOf` Text.toLower args.targetFile =
        pure $ Left
            "PDF rendering is not available. Use an explicit terminal conversion tool if available, or convert the file to text first."
    | Just n <- args.limit
    , n <= 0 =
        pure (Left "limit must be a positive integer")
    | otherwise = doesFileExist path >>= \case
        False -> pure $ Left $ "File not found: " <> args.targetFile
        True ->
            case leadingWindowLineCount args of
                Just lineCount -> readFirstLines path lineCount
                Nothing ->
                    try @_ @SomeException (streamReadFile path args) >>= \case
                        Left err ->
                            pure $ Left $
                                "Failed to read file: " <> Text.pack (show err)
                        Right (Left err) -> pure (Left err)
                        Right (Right output) ->
                            pure $ Right ExactFileWindow
                                { fileWindowText = output
                                , fileWindowArguments = args
                                }

leadingWindowLineCount :: ReadFileArgs -> Maybe Int
leadingWindowLineCount args =
    case requestedReadRange args of
        Just (1, count) -> Just count
        _ -> Nothing

fileWindowCoversArgs :: FileWindow -> ReadFileArgs -> Bool
fileWindowCoversArgs window args =
    case window of
        ExactFileWindow
            { fileWindowArguments = prepared } ->
                prepared == args
        RawFileWindow
            { fileWindowComplete = True } ->
                True
        RawFileWindow
            { fileWindowText = content } ->
                case requestedReadRange args of
                    Just (start, count) ->
                        rangeFits
                            (length (readFileLines content))
                            start
                            count
                    Nothing -> False

requestedReadRange :: ReadFileArgs -> Maybe (Int, Int)
requestedReadRange args = do
    start <-
        case args.offset of
            Nothing -> Just 1
            Just 0 -> Just 1
            Just value
                | value > 0 -> Just value
                | otherwise -> Nothing
    count <-
        case args.limit of
            Nothing -> Just maxReadLines
            Just value
                | value > 0 -> Just (min maxReadLines value)
                | otherwise -> Nothing
    pure (start, count)

rangeFits :: Int -> Int -> Int -> Bool
rangeFits available start count =
    start <= available
        && count <= available - start + 1

formatFileWindow :: FileWindow -> ReadFileArgs -> Either Text Text
formatFileWindow window args =
    case window of
        ExactFileWindow
            { fileWindowText = output
            , fileWindowArguments = prepared
            }
                | prepared == args -> Right output
                | otherwise -> Left "Prefetched read arguments changed"
        RawFileWindow
            { fileWindowText = content } ->
                formatReadFileContent content args

readFirstLines :: OsPath -> Int -> IO (Either Text FileWindow)
readFirstLines path maxLines = do
    result <-
        try @_ @SomeException $
            retryOnFileBusy $
                withBinaryFile (unsafeToFilePath path) ReadMode \handle -> do
                    hSetBuffering handle (BlockBuffering (Just chunkSize))
                    first <- BS.hGet handle 8192
                    if BS.elem 0 first
                        then pure $ Left "Cannot read binary file"
                        else Right <$> collectLines handle maxLines first
    pure $ case result of
        Left err ->
            Left ("Failed to read file: " <> Text.pack (show err))
        Right inner -> inner

collectLines
    :: Handle
    -> Int
    -> BS.ByteString
    -> IO FileWindow
collectLines handle maxLines firstChunk =
    go [firstChunk] (BS.count 10 firstChunk) (BS.length firstChunk)
  where
    go chunks newlines retainedBytes
        | newlines >= maxLines = do
            let gathered = BS.concat (reverse chunks)
                bytes = takeNthLines maxLines gathered
            if BS.length bytes > maxSelectedLineBytes
                then fail "Selected read_file window exceeds the token limit"
                else do
                    eof <-
                        if BS.length bytes == BS.length gathered
                            then hIsEOF handle
                            else pure False
                    decodeWindow
                        (BS.length bytes == BS.length gathered && eof)
                        bytes
        | retainedBytes > maxSelectedLineBytes =
            fail "Selected read_file window exceeds the token limit"
        | otherwise = do
            eof <- hIsEOF handle
            if eof
                then decodeWindow True (BS.concat (reverse chunks))
                else do
                    chunk <- BS.hGetSome handle chunkSize
                    if BS.null chunk
                        then decodeWindow True (BS.concat (reverse chunks))
                        else
                            go
                                (chunk : chunks)
                                (newlines + BS.count 10 chunk)
                                (retainedBytes + BS.length chunk)

    decodeWindow complete bytes =
        pure RawFileWindow
            { fileWindowText = decodeUtf8With lenientDecode bytes
            , fileWindowComplete = complete
            }

takeNthLines :: Int -> BS.ByteString -> BS.ByteString
takeNthLines n bytes =
    case nthNewlineEnd n bytes of
        Nothing -> bytes
        Just end -> BS.take end bytes

nthNewlineEnd :: Int -> BS.ByteString -> Maybe Int
nthNewlineEnd n bytes = go n 0
  where
    go 0 idx = Just idx
    go left idx =
        case BS.elemIndex 10 (BS.drop idx bytes) of
            Nothing -> Nothing
            Just found -> go (left - 1) (idx + found + 1)

-- | Bounded, incremental implementation used by normal and exact speculative
-- reads. The first pass counts lines for negative offsets and stable
-- out-of-range errors; the second retains only the requested window.
streamReadFile :: OsPath -> ReadFileArgs -> IO (Either Text Text)
streamReadFile path args =
    case args.limit of
        Just n | n <= 0 -> pure (Left "limit must be a positive integer")
        _ ->
            retryOnFileBusy $
                withBinaryFile (unsafeToFilePath path) ReadMode \handle -> do
                    countResult <- countFileLines handle
                    case countResult of
                        Left err -> pure (Left err)
                        Right total -> do
                            let start = resolveReadStartLine total args.offset
                                takeCount =
                                    min maxReadLines
                                        (fromMaybe maxReadLines args.limit)
                                rangeSpecified =
                                    args.offset /= Nothing
                                        || args.limit /= Nothing
                            if start > total && total > 0
                                then pure $ Right $
                                    "Offset " <> Text.pack (show start)
                                        <> " is beyond the end of the file ("
                                        <> Text.pack (show total) <> " lines)."
                                else do
                                    hSeek handle AbsoluteSeek 0
                                    collectWindow
                                        handle
                                        start
                                        takeCount
                                        rangeSpecified
                                        args

countFileLines :: Handle -> IO (Either Text Int)
countFileLines handle =
    go handle 0 False (0 :: Word8) 0
  where
    go handle !newlines !seen !lastByte !checked = do
        chunk <- BS.hGetSome handle chunkSize
        let prefix = BS.take (max 0 (8192 - checked)) chunk
            checked' = checked + BS.length prefix
        if BS.elem 0 prefix
            then pure (Left "Cannot read binary file")
            else if BS.null chunk
                then pure $ Right $
                    if not seen
                        then 1
                        else if lastByte == 10
                            then newlines
                            else newlines + 1
                else
                    go
                        handle
                        (newlines + BS.count 10 chunk)
                        True
                        (BS.last chunk)
                        checked'

chunkSize :: Int
chunkSize = 64 * 1024

-- Conservative UTF-8 bound for a selected pathological single line.
maxSelectedLineBytes :: Int
maxSelectedLineBytes = maxReadTokens * 8

collectWindow
    :: Handle
    -> Int
    -> Int
    -> Bool
    -> ReadFileArgs
    -> IO (Either Text Text)
collectWindow handle start takeCount rangeSpecified args =
    go 1 [] [] 0 False
  where
    go !lineNo !currentChunks !selected !outChars !done = do
        chunk <- BS.hGetSome handle chunkSize
        if BS.null chunk
            then
                if null currentChunks
                    then finish selected outChars
                    else finishLine lineNo currentChunks selected outChars
            else consume chunk lineNo currentChunks selected outChars done

    consume bytes lineNo chunks selected outChars done
        | BS.null bytes = go lineNo chunks selected outChars done
        | otherwise =
            let (before, after) = BS.break (== 10) bytes
                chunks'
                    | lineNo < start = []
                    | BS.null before = chunks
                    | otherwise = before : chunks
                bytesInLine = sum (map BS.length chunks')
            in if bytesInLine > maxSelectedLineBytes
                && lineNo >= start
                && lineNo < start + takeCount
                then
                    pure $ Left $
                        tokenLimitMessage (maxReadTokens + 1) rangeSpecified args
                else if BS.null after
                    then go lineNo chunks' selected outChars done
                    else
                        let (selected', chars', done') =
                                addLine lineNo chunks' selected outChars done
                        in if done'
                            then finish selected' chars'
                            else
                                consume
                                    (BS.drop 1 after)
                                    (lineNo + 1)
                                    []
                                    selected'
                                    chars'
                                    done'

    finishLine lineNo chunks selected outChars =
        let (selected', chars', _) =
                addLine lineNo chunks selected outChars False
        in finish selected' chars'

    addLine lineNo chunks selected outChars done
        | lineNo < start = (selected, outChars, done)
        | lineNo >= start + takeCount = (selected, outChars, True)
        | otherwise =
            let raw = BS.concat (reverse chunks)
                text = decodeUtf8With lenientDecode raw
                rendered = formatSelectedLine start lineNo text
                extra = Text.length rendered + if null selected then 0 else 1
                chars' = outChars + extra
            in if chars' `div` 4 > maxReadTokens
                then (selected, chars', True)
                else
                    ( rendered : selected
                    , chars'
                    , length selected + 1 >= takeCount
                    )

    finish selected outChars
        | outChars `div` 4 > maxReadTokens =
            pure $ Left $
                tokenLimitMessage
                    (max 1 (outChars `div` 4))
                    rangeSpecified
                    args
        | null selected && outChars == 0 && start == 1 =
            pure $ Right (formatNumbered 1 [""])
        | otherwise =
            pure $ Right $ Text.intercalate "\n" (reverse selected)

formatReadFileContent :: Text -> ReadFileArgs -> Either Text Text
formatReadFileContent content args =
    case args.limit of
        Just n | n <= 0 ->
            Left "limit must be a positive integer"
        _ ->
            let allLines = readFileLines content
                total = length allLines
                start = resolveReadStartLine total args.offset
                window = drop (start - 1) allLines
                takeCount =
                    min maxReadLines (fromMaybe maxReadLines args.limit)
                taken = take takeCount window
                numbered = formatNumbered start taken
                tokens = estimateTokens numbered
                rangeSpecified =
                    args.offset /= Nothing || args.limit /= Nothing
            in if start > total && total > 0
                then Right $
                    "Offset " <> Text.pack (show start)
                        <> " is beyond the end of the file ("
                        <> Text.pack (show total) <> " lines)."
                else if tokens > maxReadTokens
                    then Left (tokenLimitMessage tokens rangeSpecified args)
                    else Right numbered

-- | Split into display lines without treating a POSIX trailing newline as an
-- extra empty line.
readFileLines :: Text -> [Text]
readFileLines content =
    let fields = Text.splitOn "\n" content
    in if Text.isSuffixOf "\n" content
        then case reverse fields of
            "" : rest -> reverse rest
            _ -> fields
        else fields

resolveReadStartLine :: Int -> Maybe Int -> Int
resolveReadStartLine totalLines offset =
    case fromMaybe 1 offset of
        0 -> 1
        n | n > 0 -> n
        n -> max 1 (totalLines + n + 1)

estimateTokens :: Text -> Int
estimateTokens text = max 1 (Text.length text `div` 4)

tokenLimitMessage :: Int -> Bool -> ReadFileArgs -> Text
tokenLimitMessage tokens rangeSpecified args
    | rangeSpecified =
        "The requested line range (offset=" <> off <> ", limit=" <> lim
            <> ") contains " <> Text.pack (show tokens)
            <> " tokens, which exceeds the maximum allowed tokens ("
            <> Text.pack (show maxReadTokens)
            <> " tokens).\nTry a smaller `limit`, a different starting `offset`, \
               \or use the 'grep' tool to search for specific content."
    | otherwise =
        "File content (" <> Text.pack (show tokens)
            <> " tokens) exceeds maximum allowed tokens ("
            <> Text.pack (show maxReadTokens)
            <> " tokens).\nTry a smaller `limit`, a different starting `offset`, \
               \or use the 'grep' tool to search for specific content."
  where
    off = maybe "1" (Text.pack . show) args.offset
    lim = maybe "to end" (Text.pack . show) args.limit

formatNumbered :: Int -> [Text] -> Text
formatNumbered start lines_ =
    Text.intercalate "\n" (zipWith formatLine [start ..] lines_)
  where
    formatLine n line
        | n == start || n `mod` 10 == 0 =
            Text.pack (show n) <> "\8594" <> line
        | otherwise = line

formatSelectedLine :: Int -> Int -> Text -> Text
formatSelectedLine start n line
    | n == start || n `mod` 10 == 0 =
        Text.pack (show n) <> "\8594" <> line
    | otherwise = line
