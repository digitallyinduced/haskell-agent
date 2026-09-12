-- | Text capture for clipboard commands, without an intermediate String.
module Agent.CLI.Clipboard.Process
    ( readClipboardProcessText
    ) where

import Control.Concurrent.Async (concurrently)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Exit (ExitCode)
import System.IO (hClose)
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe)
    , proc
    , waitForProcess
    , withCreateProcess
    )

-- | Run a command with empty stdin and capture both streams as strict Text.
-- Leave the handles' locale encoding and newline translation unchanged, just
-- like readProcessWithExitCode. Both streams must be drained concurrently to
-- avoid blocking a child that fills its stderr pipe while producing stdout.
-- The process and reader threads are scoped so failures and cancellation clean
-- up the handles and child rather than leaving background readers behind.
readClipboardProcessText :: FilePath -> [String] -> IO (ExitCode, Text, Text)
readClipboardProcessText cmd args =
    withCreateProcess
        (proc cmd args)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
        \input output errors process ->
            case (input, output, errors) of
                (Just stdinHandle, Just stdoutHandle, Just stderrHandle) -> do
                    hClose stdinHandle
                    (out, err) <- concurrently
                        (Text.hGetContents stdoutHandle)
                        (Text.hGetContents stderrHandle)
                    code <- waitForProcess process
                    pure (code, out, err)
                _ -> ioError (userError "clipboard process pipes unavailable")
