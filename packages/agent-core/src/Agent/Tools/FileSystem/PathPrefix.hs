-- | Shared helpers for streamed path arguments: unique git-index prefixes,
-- top-level JSON string fields, and filesystem fingerprints.
module Agent.Tools.FileSystem.PathPrefix
    ( PathProgress(..)
    , FileFingerprint(..)
    , jsonStringFieldProgress
    , uniqueWorkspaceCandidate
    , workspaceFileIndex
    , workspaceDirectoryIndex
    , fileFingerprint
    , directoryFingerprint
    , fingerprintsMatch
    , cancelAndJoin
    , minimumPredictionPrefix
    , maxSpeculativeReadBytes
    , maximumConcurrentSpeculativeTasks
    ) where

import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent.Async (Async, cancel, waitCatch)
import Control.Exception.Safe (tryAny)
import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isSpace)
import Data.List (inits)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath, isAbsolute)
import System.Posix.Files
    ( FileStatus
    , deviceID
    , fileID
    , fileSize
    , getFileStatus
    , isDirectory
    , isRegularFile
    , modificationTimeHiRes
    , statusChangeTimeHiRes
    )
import System.Process (readProcessWithExitCode)

data PathProgress
    = PathPrefix !Text
    | PathComplete !Text
    deriving (Eq, Show)

data FileFingerprint = FileFingerprint
    { fingerprintDevice :: !Integer
    , fingerprintFile :: !Integer
    , fingerprintSize :: !Integer
    , fingerprintModified :: !Rational
    , fingerprintChanged :: !Rational
    }
    deriving (Eq, Show)

jsonStringFieldProgress :: Text -> Text -> Maybe PathProgress
jsonStringFieldProgress fieldName arguments =
    completeField arguments <|> partialField arguments
  where
    completeField input = do
        Object object <- Aeson.decodeStrict' (Text.encodeUtf8 input)
        String target <- KeyMap.lookup (Key.fromText fieldName) object
        pure (PathComplete target)

    partialField = findTopLevelStringField fieldName

uniqueWorkspaceCandidate :: Text -> Set.Set Text -> Maybe Text
uniqueWorkspaceCandidate prefix paths
    | isAbsolute (fromText prefix) = Nothing
    | otherwise = do
        candidate <- Set.lookupGE normalizedPrefix paths
        guard (normalizedPrefix `Text.isPrefixOf` candidate)
        case Set.lookupGT candidate paths of
            Just next
                | normalizedPrefix `Text.isPrefixOf` next -> Nothing
            _ -> Just (decorate candidate)
  where
    (normalizedPrefix, decorate)
        | Just rest <- Text.stripPrefix "./" prefix =
            (rest, ("./" <>))
        | otherwise = (prefix, id)

workspaceFileIndex :: ToolEnv -> IO (Set.Set Text)
workspaceFileIndex environment = do
    result <- tryAny $
        readProcessWithExitCode
            "git"
            [ "-C"
            , unsafeToFilePath environment.toolCwd
            , "ls-files"
            , "--cached"
            , "--others"
            , "--exclude-standard"
            , "-z"
            ]
            ""
    pure $ Set.fromList $ case result of
        Right (ExitSuccess, output, _) ->
            filter (not . Text.null) (Text.splitOn "\0" (Text.pack output))
        _ -> []

workspaceDirectoryIndex :: ToolEnv -> IO (Set.Set Text)
workspaceDirectoryIndex environment = do
    files <- workspaceFileIndex environment
    pure $ Set.fromList $
        "." : concatMap ancestorDirectories (Set.toList files)

ancestorDirectories :: Text -> [Text]
ancestorDirectories path =
    [ Text.intercalate "/" prefix
    | let segments = filter (not . Text.null) (Text.splitOn "/" path)
    , prefix <- drop 1 (inits (dropTail segments))
    , not (null prefix)
    ]
  where
    dropTail [] = []
    dropTail xs = init xs

fileFingerprint :: OsPath -> IO (Maybe FileFingerprint)
fileFingerprint path = fingerprintWhere isRegularFile path

directoryFingerprint :: OsPath -> IO (Maybe FileFingerprint)
directoryFingerprint path = fingerprintWhere isDirectory path

fingerprintWhere
    :: (FileStatus -> Bool)
    -> OsPath
    -> IO (Maybe FileFingerprint)
fingerprintWhere predicate path = do
    result <- tryAny (getFileStatus (unsafeToFilePath path))
    pure $ case result of
        Right status
            | predicate status -> Just FileFingerprint
                { fingerprintDevice = fromIntegral (deviceID status)
                , fingerprintFile = fromIntegral (fileID status)
                , fingerprintSize = fromIntegral (fileSize status)
                , fingerprintModified = toRational (modificationTimeHiRes status)
                , fingerprintChanged = toRational (statusChangeTimeHiRes status)
                }
        _ -> Nothing

fingerprintsMatch :: Maybe FileFingerprint -> Maybe FileFingerprint -> Bool
fingerprintsMatch left right = left == right && isJust left
  where
    isJust = maybe False (const True)

cancelAndJoin :: Async a -> IO ()
cancelAndJoin worker = do
    cancel worker
    void (waitCatch worker)

minimumPredictionPrefix :: Int
minimumPredictionPrefix = 4

maximumConcurrentSpeculativeTasks :: Int
maximumConcurrentSpeculativeTasks = 4

maxSpeculativeReadBytes :: Integer
maxSpeculativeReadBytes = 16 * 1024 * 1024

findTopLevelStringField :: Text -> Text -> Maybe PathProgress
findTopLevelStringField fieldName =
    scan 0 . Text.unpack
  where
    scan :: Int -> String -> Maybe PathProgress
    scan _ [] = Nothing
    scan depth ('{' : rest) = scan (depth + 1) rest
    scan depth ('[' : rest) = scan (depth + 1) rest
    scan depth ('}' : rest) = scan (max 0 (depth - 1)) rest
    scan depth (']' : rest) = scan (max 0 (depth - 1)) rest
    scan depth ('"' : rest) =
        case scanJsonStringToken [] rest of
            Nothing -> Nothing
            Just (JsonStringIncomplete _) -> Nothing
            Just (JsonStringComplete value afterString)
                | depth == 1
                , value == fieldName
                , Just afterColon <- consumeColon afterString ->
                    parseFieldValue afterColon
                | otherwise ->
                    scan depth afterString
    scan depth (_ : rest) = scan depth rest

    consumeColon input =
        case dropWhile isSpace input of
            ':' : rest -> Just (dropWhile isSpace rest)
            _ -> Nothing

    parseFieldValue = \case
        '"' : rest ->
            case scanJsonStringToken [] rest of
                Just (JsonStringIncomplete value) ->
                    Just (PathPrefix value)
                Just (JsonStringComplete value _) ->
                    Just (PathComplete value)
                Nothing -> Nothing
        _ -> Nothing

data JsonStringToken
    = JsonStringIncomplete !Text
    | JsonStringComplete !Text ![Char]

scanJsonStringToken :: [Char] -> [Char] -> Maybe JsonStringToken
scanJsonStringToken reversed = \case
    [] ->
        JsonStringIncomplete <$> decodeJsonString (reverse reversed)
    '"' : rest ->
        (`JsonStringComplete` rest)
            <$> decodeJsonString (reverse reversed)
    '\\' : escaped : rest ->
        scanJsonStringToken (escaped : '\\' : reversed) rest
    ['\\'] -> Nothing
    character : rest ->
        scanJsonStringToken (character : reversed) rest

decodeJsonString :: String -> Maybe Text
decodeJsonString raw =
    Aeson.decodeStrict' $
        Text.encodeUtf8 $
            "\"" <> Text.pack raw <> "\""
