module Agent.Tools.FileSystem.ReadFile.Internal
    ( ReadFileArgs(..)
    , runReadFile
    , runReadFileResolved
    , readFileResolvedContent
    , formatReadFileContent
    ) where

import Agent.OsPath (fromText)
import Agent.ToolArgs (objectArgs, optInt, optText, reqText)
import Agent.Tools.IO (readTextFile, resolveForRead)
import Agent.Tools.Types (ToolEnv)
import Data.Aeson (FromJSON(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist)
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

runReadFileResolved
    :: OsPath
    -> ReadFileArgs
    -> IO (Either Text Text)
runReadFileResolved path args =
    readFileResolvedContent path args
        >>= pure . (>>= (`formatReadFileContent` args))

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

formatReadFileContent :: Text -> ReadFileArgs -> Either Text Text
formatReadFileContent content args =
    let allLines = Text.splitOn "\n" content
        total = length allLines
        start = resolveReadStartLine content args.offset
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

resolveReadStartLine :: Text -> Maybe Int -> Int
resolveReadStartLine content offset = case fromMaybe 1 offset of
    0 -> 1
    n | n > 0 -> n
    n ->
        let totalFields = length (Text.splitOn "\n" content)
            extra
                | not (Text.null content)
                    && not (Text.isSuffixOf "\n" content) = 1
                | otherwise = 0
        in max 1 (totalFields + extra + n + 1)

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
