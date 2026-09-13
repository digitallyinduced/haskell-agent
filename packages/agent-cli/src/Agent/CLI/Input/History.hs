-- | Persistent REPL history helpers.
module Agent.CLI.Input.History
    ( replHistoryPath
    , readReplHistory
    , appendReplHistory
    , readReplHistoryAt
    , appendReplHistoryAt
    , ensureHistoryParent
    , trySetMode
    ) where

import Agent.PrivateFileLock (withPrivateFileLock)
import Control.DeepSeq (force)
import Control.Exception.Safe (catchIO)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO
    ( IOMode(ReadMode, WriteMode)
    , hSetEncoding
    , hSetNewlineMode
    , mkTextEncoding
    , noNewlineTranslation
    , utf8
    , withFile
    )
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)
import System.OsPath (unsafeEncodeUtf)

replHistoryPath :: FilePath -> FilePath
replHistoryPath home = home </> ".haskell-agent" </> "history"

readReplHistory :: IO [Text]
readReplHistory = do
    home <- getHomeDirectory
    readReplHistoryAt (replHistoryPath home)

-- | Fully evaluated, newest-first entries. Haskeline's on-disk representation
-- is UTF-8 lines without newline translation or escaping; embedded newlines
-- therefore become separate entries on the next read.
readReplHistoryAt :: FilePath -> IO [Text]
readReplHistoryAt path = do
    ensureHistoryParent path
    withHistoryLock path (readHistoryEntries path)

readHistoryEntries :: FilePath -> IO [Text]
readHistoryEntries path =
    (withFile path ReadMode \handle -> do
        encoding <- mkTextEncoding "UTF-8//TRANSLIT"
        hSetEncoding handle encoding
        hSetNewlineMode handle noNewlineTranslation
        contents <- Text.hGetContents handle
        pure $! force (Text.lines contents))
        `catchIO` \_ -> pure []

appendReplHistory :: Text -> IO ()
appendReplHistory text
    | Text.all isSpace text = pure ()
    | otherwise = do
        home <- getHomeDirectory
        appendReplHistoryAt (replHistoryPath home) text

-- | Read the current file under the write lock, rather than saving an editor's
-- stale snapshot and discarding entries submitted by another process.
-- The caller controls which submissions belong in history; this helper does
-- not filter whitespace (inline and fullscreen have different policies).
appendReplHistoryAt :: FilePath -> Text -> IO ()
appendReplHistoryAt path text = do
    ensureHistoryParent path
    withHistoryLock path do
        entries <- readHistoryEntries path
        (withFile path WriteMode \handle -> do
            hSetEncoding handle utf8
            hSetNewlineMode handle noNewlineTranslation
            Text.hPutStr handle (Text.unlines (text : entries)))
            `catchIO` \_ -> pure ()
        trySetMode path 0o600

-- History is rewritten in place. Fullscreen input publication and
-- command handling run concurrently, so serialize reads with that rewrite to
-- avoid observing the temporary truncated file. The lock also coordinates
-- independent harness processes sharing the same history.
withHistoryLock :: FilePath -> IO a -> IO a
withHistoryLock path =
    withPrivateFileLock (unsafeEncodeUtf (path <> ".lock"))

ensureHistoryParent :: FilePath -> IO ()
ensureHistoryParent path = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    trySetMode dir 0o700

trySetMode :: FilePath -> FileMode -> IO ()
trySetMode path mode =
    setFileMode path mode `catchIO` \_ -> pure ()
