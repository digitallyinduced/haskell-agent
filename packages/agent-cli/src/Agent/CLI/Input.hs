-- | Line editing for interactive prompts. TTY sessions use haskeline so
-- arrow keys move the cursor / recall history instead of echoing escape
-- sequences; non-TTY falls back to plain 'getLine'.
module Agent.CLI.Input
    ( readReplLine
    , readApprovalLine
    , replHistoryPath
    ) where

import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (catchIO, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.Haskeline
    ( Settings(..)
    , defaultSettings
    , getInputLine
    , handleInterrupt
    , runInputT
    , withInterrupt
    )
import System.Directory
    ( createDirectoryIfMissing
    , getHomeDirectory
    )
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hIsTerminalDevice, isEOF, stdin, stdout)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

-- | @~/.haskell-agent/history@ given the user's home directory.
replHistoryPath :: FilePath -> FilePath
replHistoryPath home = home </> ".haskell-agent" </> "history"

-- | Read a REPL prompt line. Persists history under 'replHistoryPath' when
-- stdin is a TTY. 'Nothing' means EOF.
readReplLine :: Text -> IO (Maybe Text)
readReplLine prompt = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then do
            home <- getHomeDirectory
            let path = replHistoryPath home
            ensureHistoryParent path
            readEditedLine
                defaultSettings
                    { historyFile = Just path
                    , autoAddHistory = True
                    }
                prompt
        else readPlainLine prompt

-- | Read a one-shot approval answer without touching REPL history.
readApprovalLine :: Text -> IO (Maybe Text)
readApprovalLine prompt = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then readEditedLine
            defaultSettings
                { historyFile = Nothing
                , autoAddHistory = False
                }
            prompt
        else readPlainLine prompt

readEditedLine :: Settings IO -> Text -> IO (Maybe Text)
readEditedLine settings prompt =
    -- Haskeline turns Ctrl-C into 'Interrupt'; rethrow as 'UserInterrupt'
    -- so the outer resume-hint handler still runs.
    handleInterrupt (throwIO UserInterrupt) $
        runInputT settings $
            withInterrupt $
                fmap (fmap Text.pack) (getInputLine (Text.unpack prompt))

readPlainLine :: Text -> IO (Maybe Text)
readPlainLine prompt = do
    Text.putStr prompt
    hFlush stdout
    done <- isEOF
    if done
        then pure Nothing
        else Just . Text.strip <$> Text.getLine

ensureHistoryParent :: FilePath -> IO ()
ensureHistoryParent path = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    -- Best-effort private mode; ignore failures on filesystems that refuse.
    trySetMode dir 0o700

trySetMode :: FilePath -> FileMode -> IO ()
trySetMode path mode =
    setFileMode path mode `catchIO` \_ -> pure ()
