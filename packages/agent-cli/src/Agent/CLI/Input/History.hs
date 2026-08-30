-- | Persistent REPL history helpers.
module Agent.CLI.Input.History
    ( replHistoryPath
    , readReplHistory
    , appendReplHistory
    , ensureHistoryParent
    , trySetMode
    ) where

import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Control.Exception.Safe (catchIO)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.Haskeline.History
    ( addHistory
    , emptyHistory
    , historyLines
    , readHistory
    , writeHistory
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
    let path = replHistoryPath home
    ensureHistoryParent path
    withHistoryLock path do
        history <- readHistory path `catchIO` \_ -> pure emptyHistory
        pure (map Text.pack (historyLines history))

appendReplHistory :: Text -> IO ()
appendReplHistory text
    | Text.all isSpace text = pure ()
    | otherwise = do
        home <- getHomeDirectory
        let path = replHistoryPath home
        ensureHistoryParent path
        withHistoryLock path do
            history <- readHistory path `catchIO` \_ -> pure emptyHistory
            writeHistory path (addHistory (Text.unpack text) history)
                `catchIO` \_ -> pure ()
            trySetMode path 0o600

-- Haskeline rewrites history in place. Fullscreen input publication and
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
