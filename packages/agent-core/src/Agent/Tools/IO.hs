-- | Shared filesystem and process helpers for coding tools.
module Agent.Tools.IO
    ( CommandResult(..)
    , resolveUnderCwd
    , readTextFile
    , writeTextFile
    , deleteTextFile
    , renameTextFile
    , listDirectoryEntries
    , runShellCommand
    , truncateText
    ) where

import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , listDirectory
    , removeFile
    , renameFile
    )
import System.Exit (ExitCode(..))
import System.FilePath
    ( addTrailingPathSeparator
    , isAbsolute
    , joinPath
    , splitDirectories
    , takeDirectory
    , takeFileName
    , (</>)
    )
import System.IO (hClose)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , createProcess
    , interruptProcessGroupOf
    , shell
    , waitForProcess
    )

-- | Resolve a model-supplied path against the tool cwd and reject anything
-- that canonicalizes outside that tree (including via '..' or symlinks).
resolveUnderCwd :: ToolEnv -> FilePath -> IO (Either Text FilePath)
resolveUnderCwd env requested = do
    absCwd <- canonicalizePath env.toolCwd
    let combined
            | isAbsolute requested = requested
            | otherwise = absCwd </> requested
    exists <- doesPathExist combined
    resolved <- if exists
        then canonicalizePath combined
        else resolveMissing combined
    if isInside absCwd resolved
        then pure (Right resolved)
        else pure $ Left $
            "Path escapes the working directory: " <> Text.pack requested

resolveMissing :: FilePath -> IO FilePath
resolveMissing combined = do
    let parent = takeDirectory combined
    parentExists <- doesDirectoryExist parent
    if parentExists
        then (</> takeFileName combined) <$> canonicalizePath parent
        else pure (collapseDots combined)

-- | Collapse @.@ / @..@ without requiring the path to exist, so a missing
-- @cwd/../outside@ cannot sneak past the prefix check.
collapseDots :: FilePath -> FilePath
collapseDots path = joinPath (go [] (splitDirectories path))
  where
    go acc [] = reverse acc
    go acc ("." : xs) = go acc xs
    go acc (".." : xs) = case acc of
        [] -> go acc xs
        ("/" : _) -> go acc xs
        (_ : rest) -> go rest xs
    go acc (x : xs) = go (x : acc) xs

isInside :: FilePath -> FilePath -> Bool
isInside root path =
    let root' = addTrailingPathSeparator root
    in path == root || root' `isPrefixOf` path

readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = try @SomeException (BS.readFile path) >>= \case
    Left err -> pure $ Left $ "Failed to read file: " <> Text.pack (show err)
    Right bytes
        | BS.elem 0 (BS.take 8192 bytes) ->
            pure $ Left "Cannot read binary file"
        | otherwise ->
            pure $ Right $ decodeUtf8With lenientDecode bytes

writeTextFile :: FilePath -> Text -> IO (Either Text ())
writeTextFile path content = do
    createDirectoryIfMissing True (takeDirectory path)
    try @SomeException (Text.writeFile path content) >>= \case
        Left err -> pure $ Left $ "Failed to write file: " <> Text.pack (show err)
        Right () -> pure (Right ())

deleteTextFile :: FilePath -> IO (Either Text ())
deleteTextFile path =
    try @SomeException (removeFile path) >>= \case
        Left err -> pure $ Left $ "Failed to delete file: " <> Text.pack (show err)
        Right () -> pure (Right ())

renameTextFile :: FilePath -> FilePath -> IO (Either Text ())
renameTextFile from to = do
    createDirectoryIfMissing True (takeDirectory to)
    try @SomeException (renameFile from to) >>= \case
        Left err -> pure $ Left $ "Failed to move file: " <> Text.pack (show err)
        Right () -> pure (Right ())

listDirectoryEntries :: FilePath -> IO (Either Text [(FilePath, Bool)])
listDirectoryEntries path = try @SomeException (listDirectory path) >>= \case
    Left err -> pure $ Left $ "Failed to list directory: " <> Text.pack (show err)
    Right names -> Right <$> mapM (classify path) names
  where
    classify root name = do
        isDir <- doesDirectoryExist (root </> name)
        pure (name, isDir)

data CommandResult = CommandResult
    { commandExitCode :: !(Maybe Int)
    , commandStdout :: !Text
    , commandStderr :: !Text
    , commandTimedOut :: !Bool
    } deriving (Eq, Show)

-- | Run a shell command in @workdir@, killing the process group on timeout.
runShellCommand
    :: ToolEnv
    -> FilePath
    -> String
    -> Int
    -> IO CommandResult
runShellCommand env workdir command timeoutMs = do
    let spec = (shell command)
            { cwd = Just workdir
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @SomeException (createProcess spec) >>= \case
        Left err -> pure CommandResult
            { commandExitCode = Just 127
            , commandStdout = ""
            , commandStderr = "Failed to start command: " <> Text.pack (show err)
            , commandTimedOut = False
            }
        Right (Just hin, Just hout, Just herr, processHandle) -> do
            hClose hin
            let collect = do
                    outBytes <- BS.hGetContents hout
                    errBytes <- BS.hGetContents herr
                    -- Force both pipes before waitForProcess so a full pipe
                    -- cannot deadlock against the child.
                    _ <- evaluate (BS.length outBytes + BS.length errBytes)
                    code <- waitForProcess processHandle
                    pure (outBytes, errBytes, code)
            raced <- race (threadDelay (max 1 timeoutMs * 1000)) collect
            case raced of
                Left () -> do
                    _ <- try @SomeException (interruptProcessGroupOf processHandle)
                    pure CommandResult
                        { commandExitCode = Nothing
                        , commandStdout = ""
                        , commandStderr = ""
                        , commandTimedOut = True
                        }
                Right (outBytes, errBytes, code) ->
                    pure CommandResult
                        { commandExitCode = Just (exitCodeInt code)
                        , commandStdout = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode outBytes)
                        , commandStderr = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode errBytes)
                        , commandTimedOut = False
                        }
        Right _ ->
            pure CommandResult
                { commandExitCode = Just 127
                , commandStdout = ""
                , commandStderr = "Failed to capture command output"
                , commandTimedOut = False
                }

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
    ExitSuccess -> 0
    ExitFailure n -> n

truncateText :: Int -> Text -> Text
truncateText cap text
    | cap <= 0 = text
    | Text.length text <= cap = text
    | otherwise =
        Text.take cap text
            <> "\n...[truncated "
            <> Text.pack (show (Text.length text - cap))
            <> " chars]"
