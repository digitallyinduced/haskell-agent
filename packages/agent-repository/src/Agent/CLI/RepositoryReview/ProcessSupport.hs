-- | Shared teardown and error reporting for review Git processes and checks.
module Agent.CLI.RepositoryReview.ProcessSupport
    ( closeQuietly
    , signalCheckProcessGroup
    , trySynchronous
    , renderCommand
    , processPipeTeardownMicros
    ) where

import Control.Exception.Safe (SomeException, isAsyncException, throwIO, tryAny)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle, hClose)
import System.Posix.Signals (Signal, signalProcessGroup)
import System.Posix.Types (ProcessID)
import System.Process (ProcessHandle, getPid, terminateProcess)

signalCheckProcessGroup
    :: Signal
    -> Maybe ProcessID
    -> ProcessHandle
    -> IO ()
signalCheckProcessGroup signal processGroup process = do
    processId <- maybe (getPid process) (pure . Just) processGroup
    case processId of
        Nothing -> do
            _ <- tryAny (terminateProcess process)
            pure ()
        Just pid -> do
            _ <- tryAny (signalProcessGroup signal pid)
            pure ()

closeQuietly :: Handle -> IO ()
closeQuietly handle = do
    _ <- tryAny (hClose handle)
    pure ()

renderCommand :: FilePath -> [String] -> Text
renderCommand executable arguments =
    Text.unwords (Text.pack executable : map (Text.pack . show) arguments)

processPipeTeardownMicros :: Int
processPipeTeardownMicros = 1_000_000

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action =
    tryAny action >>= \case
        Left exception
            | isAsyncException exception -> throwIO exception
            | otherwise -> pure (Left exception)
        Right value -> pure (Right value)
