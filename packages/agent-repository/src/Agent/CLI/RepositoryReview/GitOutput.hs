-- | Pure decoding of Git status and diff output.
module Agent.CLI.RepositoryReview.GitOutput
    ( RepositoryFile(..)
    , DiffHunk(..)
    , parsePorcelain
    , parseDiffHunks
    , decodePath
    ) where

import Agent.CLI.RepositoryReview.Error
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)

data RepositoryFile = RepositoryFile
    { repositoryFilePath :: !FilePath
    , repositoryFileOriginalPath :: !(Maybe FilePath)
    , repositoryFileIndexStatus :: !Char
    , repositoryFileWorktreeStatus :: !Char
    } deriving (Eq, Show)

data DiffHunk = DiffHunk
    { hunkOldStart :: !Int
    , hunkOldCount :: !Int
    , hunkNewStart :: !Int
    , hunkNewCount :: !Int
    , hunkHeader :: !Text
    } deriving (Eq, Show)

parsePorcelain :: BS.ByteString -> Either RepositoryError [RepositoryFile]
parsePorcelain bytes = go (filter (not . BS.null) (BS.split 0 bytes)) []
  where
    go [] acc = Right (reverse acc)
    go (entry:rest) acc
        | BS.length entry < 3 =
            Left (InvalidRepositoryRequest "git returned malformed status")
        | otherwise =
            let indexStatus = toStatus (BS.index entry 0)
                worktreeStatus = toStatus (BS.index entry 1)
                path = decodePath (BS.drop 3 entry)
                renamed = indexStatus `elem` ['R', 'C']
                    || worktreeStatus `elem` ['R', 'C']
            in if renamed
                then case rest of
                    [] ->
                        Left
                            (InvalidRepositoryRequest
                                "git returned an incomplete rename status")
                    original:remaining ->
                        go remaining
                            (RepositoryFile
                                { repositoryFilePath = path
                                , repositoryFileOriginalPath =
                                    Just (decodePath original)
                                , repositoryFileIndexStatus = indexStatus
                                , repositoryFileWorktreeStatus = worktreeStatus
                                }
                                : acc)
                else
                    go rest
                        (RepositoryFile
                            { repositoryFilePath = path
                            , repositoryFileOriginalPath = Nothing
                            , repositoryFileIndexStatus = indexStatus
                            , repositoryFileWorktreeStatus = worktreeStatus
                            }
                            : acc)
    toStatus byte
        | byte == 32 = ' '
        | otherwise = toEnum (fromIntegral byte)

parseDiffHunks :: BS.ByteString -> [DiffHunk]
parseDiffHunks =
    reverse
        . foldl' collect []
        . Text.lines
        . TextEncoding.decodeUtf8With lenientDecode
  where
    collect acc line =
        maybe acc (: acc) (parseHunkHeader line)

parseHunkHeader :: Text -> Maybe DiffHunk
parseHunkHeader line = do
    body <- Text.stripPrefix "@@ -" line
    let (oldRange, afterOld) = Text.breakOn " +" body
    newAndHeader <- Text.stripPrefix " +" afterOld
    let (newRange, afterNew) = Text.breakOn " @@" newAndHeader
    header <- Text.stripPrefix " @@" afterNew
    (oldStart, oldCount) <- parseRange oldRange
    (newStart, newCount) <- parseRange newRange
    pure DiffHunk
        { hunkOldStart = oldStart
        , hunkOldCount = oldCount
        , hunkNewStart = newStart
        , hunkNewCount = newCount
        , hunkHeader = Text.strip header
        }

parseRange :: Text -> Maybe (Int, Int)
parseRange text = case Text.splitOn "," text of
    [start] -> (, 1) <$> decimal start
    [start, count] -> (,) <$> decimal start <*> decimal count
    _ -> Nothing
  where
    decimal value
        | Text.null value || Text.any (not . isDigit) value = Nothing
        | otherwise = case reads (Text.unpack value) of
            [(number, "")] -> Just number
            _ -> Nothing

decodePath :: BS.ByteString -> FilePath
decodePath = Text.unpack . TextEncoding.decodeUtf8With lenientDecode
