-- | Line editing for interactive prompts. TTY sessions use haskeline so
-- arrow keys move the cursor / recall history instead of echoing escape
-- sequences; non-TTY falls back to plain 'getLine'.
module Agent.CLI.Input
    ( ReplLine(..)
    , readReplLine
    , readApprovalLine
    , approvalKeyText
    , replHistoryPath
    ) where

import Agent.CLI.Interrupt
    ( IdleCtrlCResult(..)
    , InterruptState
    , noteIdleCtrlC
    )
import Control.Exception.Safe (bracket, catchIO, throwIO, tryIO)
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
import System.IO
    ( BufferMode(..)
    , hFlush
    , hGetBuffering
    , hGetChar
    , hIsTerminalDevice
    , hSetBuffering
    , isEOF
    , stderr
    , stdin
    , stdout
    )
import System.IO.Error (isEOFError)
import System.Posix.Files (setFileMode)
import System.Posix.IO (stdInput)
import System.Posix.Terminal
    ( TerminalMode(..)
    , TerminalState(..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withMinInput
    , withTime
    , withoutMode
    )
import System.Posix.Types (FileMode)

-- | Outcome of an interactive REPL read.
data ReplLine
    = ReplEof
    | ReplText Text
    | ReplQuitInterrupt
    deriving (Eq, Show)

-- | @~/.haskell-agent/history@ given the user's home directory.
replHistoryPath :: FilePath -> FilePath
replHistoryPath home = home </> ".haskell-agent" </> "history"

-- | Read a REPL prompt line. Persists history under 'replHistoryPath' when
-- stdin is a TTY. 'ReplEof' means EOF; 'ReplQuitInterrupt' means a confirmed
-- double Ctrl-C. The prompt is drawn on stdout.
--
-- Haskeline installs its own SIGINT handler while reading, so idle Ctrl-C
-- goes through 'noteIdleCtrlC' rather than the outer signal handler.
readReplLine :: InterruptState -> Text -> IO ReplLine
readReplLine interrupt prompt = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then do
            home <- getHomeDirectory
            let path = replHistoryPath home
            ensureHistoryParent path
            readEditedLine interrupt
                defaultSettings
                    { historyFile = Just path
                    , autoAddHistory = True
                    }
                prompt
        else do
            Text.hPutStr stdout prompt
            hFlush stdout
            fmap (maybe ReplEof ReplText) readAnswerOnly
-- | Read a one-shot approval answer. The question is always written to
-- stderr (matching the pre-haskeline behavior) so redirected stdout does
-- not swallow the prompt. Does not touch REPL history.
--
-- On a TTY, a single keypress submits immediately (@y@ / @n@ / @a@, or
-- Enter for the default deny) so the user does not need a trailing Enter.
-- Non-TTY keeps cooked 'getLine' for scripts that pipe a full line.
readApprovalLine :: Text -> IO (Maybe Text)
readApprovalLine prompt = do
    Text.hPutStr stderr prompt
    hFlush stderr
    isTty <- hIsTerminalDevice stdin
    if isTty
        then readApprovalKey
        else readAnswerOnly

-- | Map one approval keypress to the text 'parseApprovalAnswer' expects.
-- Enter / Return become empty (deny by default).
approvalKeyText :: Char -> Text
approvalKeyText c
    | c == '\n' || c == '\r' = ""
    | otherwise = Text.singleton c

-- | Single-key approval on a TTY: disable canonical input, read one byte,
-- echo it (except bare Enter), then restore the previous terminal state.
readApprovalKey :: IO (Maybe Text)
readApprovalKey = do
    oldTerm <- getTerminalAttributes stdInput
    oldBuf <- hGetBuffering stdin
    let enter = do
            -- Keep ProcessInput so Ctrl-C still becomes SIGINT while waiting.
            let raw =
                    flip withMinInput 1
                        . flip withTime 0
                        . flip withoutMode EnableEcho
                        $ oldTerm
            setTerminalAttributes stdInput raw Immediately
            hSetBuffering stdin NoBuffering
        restore = do
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> do
        result <- tryIO (hGetChar stdin)
        case result of
            Left err
                | isEOFError err -> pure Nothing
                | otherwise -> throwIO err
            Right c -> do
                let answer = approvalKeyText c
                Text.hPutStrLn stderr answer
                pure (Just answer)

readEditedLine :: InterruptState -> Settings IO -> Text -> IO ReplLine
readEditedLine interrupt settings prompt = go
  where
    go =
        -- Haskeline turns Ctrl-C into 'Interrupt' and replaces our SIGINT
        -- handler for the duration of the prompt. Soft-warn / double-quit
        -- are applied here via 'noteIdleCtrlC'.
        handleInterrupt onInterrupt $
            runInputT settings $
                withInterrupt $
                    fmap (maybe ReplEof (ReplText . Text.pack))
                        (getInputLine (Text.unpack prompt))
    onInterrupt = do
        noteIdleCtrlC interrupt >>= \case
            ContinuePrompt -> go
            QuitProcess -> pure ReplQuitInterrupt
readAnswerOnly :: IO (Maybe Text)
readAnswerOnly = do
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
