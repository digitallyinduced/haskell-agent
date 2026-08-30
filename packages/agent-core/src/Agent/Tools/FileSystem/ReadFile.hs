{-# LANGUAGE BangPatterns #-}

module Agent.Tools.FileSystem.ReadFile
    ( readFileTool
    , ReadFileArgs(..)
    , formatReadFile
    , streamReadFile
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optInt, optText, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedTool
    )
import Agent.Tools.IO (displayPathInWorkspace, resolveForRead)
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv
    , ToolExecutionPolicy(..)
    , jsonTool
    , withToolResourceClaims
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Exception.Safe (SomeException, try)
import System.IO (Handle, IOMode(ReadMode), withBinaryFile)
import System.OsPath (OsPath)
import System.Directory.OsPath (doesFileExist)

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

readFileTool :: ToolEnv -> AppTool
readFileTool env = withToolResourceClaims (readFileClaims env) $
    jsonTool "read_file" readFileDescription
    [ PropertySchema "target_file" PropertyString True $ Just
        "The path of the file to read. Relative paths use the workspace; absolute paths may resolve within the workspace or session temp directory."
    , PropertySchema "offset" PropertyInteger False $ Just
        "The 1-based line number to start reading from. Negative values count from the end of the file (-1 is the last line). Only provide if the file is too large to read at once."
    , PropertySchema "limit" PropertyInteger False $ Just
        "The number of lines to read, starting at offset. Must be a positive integer. Only provide if the file is too large to read at once."
    ]
    True
    ParallelSafe
    (typedTool "read_file" readFileArgsDecoder (runReadFile env))

readFileClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
readFileClaims env call =
    case
        decodeToolArguments readFileArgsDecoder (toolArgumentsValue call.arguments)
            :: Either Text ReadFileArgs
    of
        Left err -> pure (Left err)
        Right args ->
            resolveForRead env (fromText args.targetFile)
                >>= pure . fmap
                    (\path ->
                        [ToolResourceClaim ToolRead (ToolPath path)])

readFileDescription :: Text
readFileDescription =
    "Read a file.\n\
    \\n\
    \- The target_file parameter can be relative to the workspace or an absolute path in an allowed filesystem root\n\
    \- By default, it reads up to 1000 lines starting from the beginning of the file\n\
    \- offset is 1-based. Negative offsets count from the end of the file (-1 is the last line).\n\
    \- Line numbers (1-based) appear as anchors in the format LINE_NUMBER\8594LINE_CONTENT on the first returned line and on every 10th line of the file; the lines in between show content only. Count from the nearest anchor when referring to a specific line"

maxReadLines :: Int
maxReadLines = 1000

maxReadTokens :: Int
maxReadTokens = 25000

runReadFile :: ToolEnv -> ReadFileArgs -> IO (Either Text Text)
runReadFile env args = resolveForRead env (fromText args.targetFile) >>= \case
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
                    Left err -> pure $ Left $ "Failed to read file: " <> Text.pack (show err)
                    Right result -> pure result

-- | Bounded, incremental implementation used by the tool.  The first pass
-- counts lines (needed for negative offsets and stable out-of-range errors);
-- the second pass retains only the requested window.
streamReadFile :: OsPath -> ReadFileArgs -> IO (Either Text Text)
streamReadFile path args =
    case args.limit of
        Just n | n <= 0 -> pure (Left "limit must be a positive integer")
        _ -> do
            countResult <- countFileLines path
            case countResult of
                Left err -> pure (Left err)
                Right total -> do
                    let start = resolveReadStartLine total args.offset
                        takeCount = min maxReadLines (fromMaybe maxReadLines args.limit)
                        rangeSpecified = args.offset /= Nothing || args.limit /= Nothing
                    if start > total && total > 0
                        then pure $ Right $ "Offset " <> Text.pack (show start)
                            <> " is beyond the end of the file ("
                            <> Text.pack (show total) <> " lines)."
                        else withBinaryFile (unsafeToFilePath path) ReadMode $ \h ->
                            collectWindow h start takeCount rangeSpecified args

countFileLines :: OsPath -> IO (Either Text Int)
countFileLines path =
    withBinaryFile (unsafeToFilePath path) ReadMode $ \h ->
        go h 0 False 0 0
  where
    go h !newlines !seen !lastByte !checked = do
        chunk <- BS.hGetSome h chunkSize
        let prefix = BS.take (max 0 (8192 - checked)) chunk
            checked' = checked + BS.length prefix
        if BS.elem 0 prefix
            then pure (Left "Cannot read binary file")
            else if BS.null chunk
                then pure $ Right
                    (if not seen then 1 else if lastByte == 10 then newlines else newlines + 1)
                else
                    let n = BS.count 10 chunk
                        lb = BS.last chunk
                    in go h (newlines + n) True (fromIntegral lb) checked'

chunkSize :: Int
chunkSize = 64 * 1024

-- Keep a selected line bounded even when the source contains a pathological
-- single line.  This is deliberately conservative for UTF-8 (worst case
-- replacement expansion is still below this bound).
maxSelectedLineBytes :: Int
maxSelectedLineBytes = maxReadTokens * 8

collectWindow
    :: Handle
    -> Int
    -> Int
    -> Bool
    -> ReadFileArgs
    -> IO (Either Text Text)
collectWindow h start takeCount rangeSpecified args =
    go 1 [] [] 0 False
  where
    go !lineNo !currentChunks !selected !outChars !done = do
        chunk <- BS.hGetSome h chunkSize
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
                chunks' =
                    if lineNo < start
                        then []
                        else if BS.null before then chunks else before : chunks
                bytesInLine = sum (map BS.length chunks')
            in if bytesInLine > maxSelectedLineBytes
                && lineNo >= start && lineNo < start + takeCount
                then pure $ Left $ tokenLimitMessage (maxReadTokens + 1) rangeSpecified args
                else if BS.null after
                    then go lineNo chunks' selected outChars done
                    else
                        let (selected', chars', done') =
                                addLine lineNo chunks' selected outChars done
                        in if done'
                            then finish selected' chars'
                            else consume (BS.drop 1 after) (lineNo + 1) [] selected' chars' done'

    finishLine lineNo chunks selected outChars =
        let (selected', chars', _) = addLine lineNo chunks selected outChars False
        in finish selected' chars'

    addLine lineNo chunks selected outChars done
        | lineNo < start = (selected, outChars, done)
        | lineNo >= start + takeCount = (selected, outChars, True)
        | otherwise =
            let raw = BS.concat (reverse chunks)
                txt = decodeUtf8With lenientDecode raw
                rendered = formatSelectedLine start lineNo txt
                extra = Text.length rendered + if null selected then 0 else 1
                chars' = outChars + extra
            in if chars' `div` 4 > maxReadTokens
                then (selected, chars', True)
                else (selected <> [rendered], chars', length selected + 1 >= takeCount)

    finish selected outChars
        | outChars `div` 4 > maxReadTokens =
            pure $ Left $ tokenLimitMessage (max 1 (outChars `div` 4))
                rangeSpecified args
        | null selected && outChars == 0 && start == 1 =
            pure $ Right (formatNumbered 1 [""])
        | otherwise = pure $ Right $ Text.intercalate "\n" selected

formatReadFile :: Text -> ReadFileArgs -> Either Text Text
formatReadFile content args =
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

formatSelectedLine :: Int -> Int -> Text -> Text
formatSelectedLine start n line
    | n == start || n `mod` 10 == 0 =
        Text.pack (show n) <> "\8594" <> line
    | otherwise = line
