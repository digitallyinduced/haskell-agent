module Agent.Tools.FileSystem.ReadFile.Internal
    ( ReadFileArgs(..)
    , FileWindow(..)
    , runReadFile
    , runReadFileResolved
    , readFileResolvedContent
    , readFileWindowForArgs
    , fileWindowCoversArgs
    , formatReadFileContent
    ) where

import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optInt, optText, reqText)
import Agent.Tools.IO (readTextFile, resolveForRead)
import Agent.Tools.Types (ToolEnv)
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (FromJSON(..))
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory.OsPath (doesFileExist)
import System.IO
    ( BufferMode(..)
    , Handle
    , IOMode(..)
    , hIsEOF
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

instance FromJSON ReadFileArgs where
    parseJSON = objectArgs \object -> ReadFileArgs
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
        Right path -> runReadFileResolved path args

data FileWindow = FileWindow
    { fileWindowText :: !Text
    , fileWindowComplete :: !Bool
    }

runReadFileResolved
    :: OsPath
    -> ReadFileArgs
    -> IO (Either Text Text)
runReadFileResolved path args =
    readFileWindowForArgs path args
        >>= pure . (>>= (`formatReadFileContent` args) . (.fileWindowText))

readFileResolvedContent
    :: OsPath
    -> ReadFileArgs
    -> IO (Either Text Text)
readFileResolvedContent path args
    | ".pdf" `Text.isSuffixOf` Text.toLower args.targetFile =
        pure $ Left
            "PDF rendering is not available. Use an explicit terminal conversion tool if available, or convert the file to text first."
    | otherwise = doesFileExist path >>= \case
        False -> pure $ Left $ "File not found: " <> args.targetFile
        True -> readTextFile path >>= \case
            Left err -> pure (Left err)
            Right content -> do
                _ <- pure (args.pages, args.format)
                pure (Right content)

-- | Read only the line window 'read_file' will return when the offset is a
-- non-negative start. Negative offsets still slurp the whole file so the last
-- line can be found.
readFileWindowForArgs :: OsPath -> ReadFileArgs -> IO (Either Text FileWindow)
readFileWindowForArgs path args
    | ".pdf" `Text.isSuffixOf` Text.toLower args.targetFile =
        pure $ Left
            "PDF rendering is not available. Use an explicit terminal conversion tool if available, or convert the file to text first."
    | otherwise = doesFileExist path >>= \case
        False -> pure $ Left $ "File not found: " <> args.targetFile
        True -> case boundedLineCount args of
            Nothing ->
                readFileResolvedContent path args >>= \case
                    Left err -> pure (Left err)
                    Right content ->
                        pure (Right (FileWindow content True))
            Just lineCount ->
                readFirstLines path lineCount

boundedLineCount :: ReadFileArgs -> Maybe Int
boundedLineCount args =
    case args.offset of
        Just n | n <= 0 -> Nothing
        Just n | n > 1 -> Nothing
        _ ->
            Just (min maxReadLines (fromMaybe maxReadLines args.limit))

fileWindowCoversArgs :: FileWindow -> ReadFileArgs -> Bool
fileWindowCoversArgs window args
    | window.fileWindowComplete = True
    | otherwise =
        case args.offset of
            Just n | n <= 0 -> False
            Just n | n > 1 -> False
            _ ->
                let have = length (readFileLines window.fileWindowText)
                    need = min maxReadLines (fromMaybe maxReadLines args.limit)
                in have >= need

readFirstLines :: OsPath -> Int -> IO (Either Text FileWindow)
readFirstLines path maxLines = do
    result <-
        try @_ @SomeException $
            withBinaryFile (unsafeToFilePath path) ReadMode \handle -> do
                hSetBuffering handle (BlockBuffering (Just 65536))
                first <- BS.hGetSome handle 8192
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
    go [firstChunk] (BS.count 10 firstChunk)
  where
    go chunks newlines
        | newlines >= maxLines = do
            let gathered = BS.concat (reverse chunks)
                bytes = takeNthLines maxLines gathered
            eof <-
                if BS.length bytes == BS.length gathered
                    then hIsEOF handle
                    else pure False
            decodeWindow (BS.length bytes == BS.length gathered && eof) bytes
        | otherwise = do
            eof <- hIsEOF handle
            if eof
                then decodeWindow True (BS.concat (reverse chunks))
                else do
                    chunk <- BS.hGetSome handle 65536
                    if BS.null chunk
                        then decodeWindow True (BS.concat (reverse chunks))
                    else if BS.elem 0 chunk
                        then fail "Cannot read binary file"
                    else
                        go (chunk : chunks) (newlines + BS.count 10 chunk)

    decodeWindow complete bytes =
        pure FileWindow
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
            Just offset -> go (left - 1) (idx + offset + 1)

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
                takeCount = min maxReadLines (fromMaybe maxReadLines args.limit)
                taken = take takeCount window
                numbered = formatNumbered start taken
                tokens = estimateTokens numbered
                rangeSpecified = args.offset /= Nothing || args.limit /= Nothing
            in if start > total && total > 0
                then Right $ "Offset " <> Text.pack (show start)
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
resolveReadStartLine totalLines offset = case fromMaybe 1 offset of
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
    Text.intercalate "\n" (zipWith fmt [start..] lines_)
  where
    fmt n line
        | n == start || n `mod` 10 == 0 =
            Text.pack (show n) <> "\8594" <> line
        | otherwise = line
