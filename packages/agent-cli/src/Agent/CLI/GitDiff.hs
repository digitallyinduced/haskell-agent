-- | Safe, informational Git inspection used by @/diff@ and @/review@.
--
-- Git repositories can configure executable diff, text-conversion, filter,
-- hook, and fsmonitor helpers.  Slash commands that only inspect a repository
-- must not execute those helpers as a side effect.
module Agent.CLI.GitDiff
    ( GitCommandOutput(..)
    , GitDiffResult(..)
    , getGitDiff
    , gitOutputText
    , runSafeGit
    ) where

import Agent.OsPath (unsafeToFilePath)
import Agent.Process (terminateProcessGroup)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (displayException, tryIO)
import Control.Monad (void)
import Data.List (intercalate, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetContents)
import System.OsPath (OsPath)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , getPid
    , proc
    , waitForProcess
    , withCreateProcess
    )
import System.Timeout (timeout)

data GitCommandOutput = GitCommandOutput
    { gitCommandExitCode :: !ExitCode
    , gitCommandStdout :: !String
    , gitCommandStderr :: !String
    }
    deriving (Eq, Show)

data GitDiffResult
    = GitDiffNotRepository
    | GitDiffOutput !Text
    deriving (Eq, Show)

-- | Return the working-tree diff, including untracked files.
--
-- An empty 'GitDiffOutput' means the directory is a repository with no
-- working-tree changes.
getGitDiff :: OsPath -> IO (Either Text GitDiffResult)
getGitDiff cwd = do
    inside <- insideGitWorkTree cwd
    case inside of
        Left err -> pure (Left err)
        Right False -> pure (Right GitDiffNotRepository)
        Right True -> do
            filterOverrides <- executableFilterOverrides cwd
            case filterOverrides of
                Left err -> pure (Left err)
                Right overrides -> do
                    (tracked, untracked) <-
                        concurrently
                            (runDiff cwd overrides trackedDiffArguments)
                            (runGitSuccess cwd [] untrackedListArguments)
                    case (tracked, untracked) of
                        (Left err, _) -> pure (Left err)
                        (_, Left err) -> pure (Left err)
                        (Right trackedText, Right untrackedOutput) -> do
                            untrackedDiffs <-
                                traverse
                                    (runUntrackedDiff cwd overrides)
                                    (nulSeparatedPaths
                                        untrackedOutput.gitCommandStdout)
                            pure do
                                pieces <- sequence untrackedDiffs
                                Right
                                    (GitDiffOutput
                                        (trackedText <> Text.concat pieces))

insideGitWorkTree :: OsPath -> IO (Either Text Bool)
insideGitWorkTree cwd =
    runSafeGit cwd [] ["rev-parse", "--is-inside-work-tree"] >>= \case
        Left err -> pure (Left err)
        Right output ->
            pure $
                Right
                    ( output.gitCommandExitCode == ExitSuccess
                        && Text.strip (gitOutputText output) == "true"
                    )

executableFilterOverrides
    :: OsPath
    -> IO (Either Text [(Text, Text)])
executableFilterOverrides cwd =
    runSafeGit
        cwd
        []
        [ "config"
        , "--null"
        , "--name-only"
        , "--get-regexp"
        , "^filter\\..*\\.(clean|process)$"
        ] >>= \case
            Left err -> pure (Left err)
            Right output
                | output.gitCommandExitCode == ExitSuccess
                    || output.gitCommandExitCode == ExitFailure 1 ->
                        pure $
                            Right
                                (concatMap disableDriver
                                    (configuredFilterDrivers
                                        output.gitCommandStdout))
                | otherwise ->
                    pure (Left (gitFailure "git config" output))
  where
    disableDriver driver =
        [ (driver <> ".clean", "")
        , (driver <> ".process", "")
        , (driver <> ".required", "false")
        ]

configuredFilterDrivers :: String -> [Text]
configuredFilterDrivers raw =
    deduplicate . sort $
        foldMap driverForKey
            (filter (not . Text.null) (Text.splitOn "\0" (Text.pack raw)))
  where
    driverForKey key =
        case Text.stripSuffix ".clean" key of
            Just driver -> [driver]
            Nothing -> maybe [] pure (Text.stripSuffix ".process" key)

    deduplicate [] = []
    deduplicate (first : rest) =
        first : deduplicate (dropWhile (== first) rest)

runUntrackedDiff
    :: OsPath
    -> [(Text, Text)]
    -> String
    -> IO (Either Text Text)
runUntrackedDiff cwd overrides path =
    runDiff cwd overrides
        (commonDiffArguments
            <> ["--no-index", "--", "/dev/null", path])

runDiff
    :: OsPath
    -> [(Text, Text)]
    -> [String]
    -> IO (Either Text Text)
runDiff cwd overrides arguments =
    runSafeGit cwd overrides arguments >>= \case
        Left err -> pure (Left err)
        Right output
            | output.gitCommandExitCode == ExitSuccess
                || output.gitCommandExitCode == ExitFailure 1 ->
                    pure (Right (gitOutputText output))
            | otherwise ->
                pure (Left (gitFailure (renderGitCommand arguments) output))

runGitSuccess
    :: OsPath
    -> [(Text, Text)]
    -> [String]
    -> IO (Either Text GitCommandOutput)
runGitSuccess cwd overrides arguments =
    runSafeGit cwd overrides arguments >>= \case
        Left err -> pure (Left err)
        Right output
            | output.gitCommandExitCode == ExitSuccess ->
                pure (Right output)
            | otherwise ->
                pure (Left (gitFailure (renderGitCommand arguments) output))

-- | Run one bounded Git command with repository-configured hooks and
-- fsmonitor disabled. Additional config values are passed as individual
-- argv entries, never through a shell.
runSafeGit
    :: OsPath
    -> [(Text, Text)]
    -> [String]
    -> IO (Either Text GitCommandOutput)
runSafeGit cwd overrides arguments = do
    let allArguments =
            baseConfigArguments
                <> foldMap configArgument overrides
                <> arguments
        process =
            (proc "git" allArguments)
                { cwd = Just (unsafeToFilePath cwd)
                , close_fds = True
                , create_group = True
                , new_session = True
                , std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    result <-
        tryIO $
            withCreateProcess process \_ stdoutHandle stderrHandle processHandle ->
                case (stdoutHandle, stderrHandle) of
                    (Just stdout, Just stderr) -> do
                        groupId <- getPid processHandle
                        completed <-
                            timeout gitCommandTimeoutMicros $
                                concurrently
                                    (concurrently
                                        (readHandleStrict stdout)
                                        (readHandleStrict stderr))
                                    (waitForProcess processHandle)
                        case completed of
                            Nothing -> do
                                terminateProcessGroup groupId processHandle
                                void (timeout processCleanupTimeoutMicros
                                    (waitForProcess processHandle))
                                pure Nothing
                            Just ((stdoutText, stderrText), exitCode) ->
                                pure $
                                    Just GitCommandOutput
                                        { gitCommandExitCode = exitCode
                                        , gitCommandStdout = stdoutText
                                        , gitCommandStderr = stderrText
                                        }
                    _ -> fail "git did not provide stdout and stderr pipes"
    pure case result of
        Left err ->
            Left
                (renderGitCommand arguments
                    <> " failed to start: "
                    <> Text.pack (displayException err))
        Right Nothing ->
            Left
                (renderGitCommand arguments
                    <> " timed out after 30 seconds")
        Right (Just output) -> Right output
  where
    configArgument (key, value) =
        ["-c", Text.unpack key <> "=" <> Text.unpack value]

baseConfigArguments :: [String]
baseConfigArguments =
    [ "-c"
    , "safe.bareRepository=explicit"
    , "-c"
    , "core.fsmonitor=false"
    , "-c"
    , "core.hooksPath=/dev/null"
    ]

trackedDiffArguments :: [String]
trackedDiffArguments = commonDiffArguments

commonDiffArguments :: [String]
commonDiffArguments =
    [ "diff"
    , "--no-textconv"
    , "--no-ext-diff"
    , "--submodule=short"
    , "--ignore-submodules=dirty"
    , "--color"
    ]

untrackedListArguments :: [String]
untrackedListArguments =
    [ "ls-files"
    , "--others"
    , "--exclude-standard"
    , "-z"
    ]

nulSeparatedPaths :: String -> [String]
nulSeparatedPaths =
    filter (not . null) . splitOnNul
  where
    splitOnNul [] = []
    splitOnNul value =
        let (path, rest) = break (== '\0') value
        in path : case rest of
            [] -> []
            _ : remaining -> splitOnNul remaining

gitOutputText :: GitCommandOutput -> Text
gitOutputText = Text.pack . (.gitCommandStdout)

gitFailure :: Text -> GitCommandOutput -> Text
gitFailure command output =
    command
        <> " failed with "
        <> Text.pack (show output.gitCommandExitCode)
        <> case Text.strip (Text.pack output.gitCommandStderr) of
            "" -> ""
            stderr -> ": " <> stderr

renderGitCommand :: [String] -> Text
renderGitCommand arguments =
    Text.pack (intercalate " " ("git" : arguments))

gitCommandTimeoutMicros :: Int
gitCommandTimeoutMicros = 30 * 1_000_000

processCleanupTimeoutMicros :: Int
processCleanupTimeoutMicros = 2 * 1_000_000

readHandleStrict :: Handle -> IO String
readHandleStrict handle = do
    contents <- hGetContents handle
    let size = length contents
    size `seq` pure contents
