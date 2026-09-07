-- | Safe, informational Git inspection used by @/diff@ and @/review@.
--
-- Git repositories can configure executable diff, text-conversion, filter,
-- hook, and fsmonitor helpers.  Slash commands that only inspect a repository
-- must not execute those helpers as a side effect.
module Agent.CLI.GitDiff
    ( GitCommandOutput(..)
    , GitDiffResult(..)
    , colorizeGitDiff
    , getGitDiff
    , gitOutputText
    , runSafeGit
    ) where

import Agent.OsPath (unsafeToFilePath)
import Agent.Process (terminateProcessGroup)
import Agent.TUI.TextWidth (displayTerminalText)
import Agent.CLI.Style (roleError, roleMuted, roleSuccess)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (displayException, tryIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT, throwE)
import qualified Data.ByteString as ByteString
import Data.List (intercalate, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))
import System.IO (Handle)
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
getGitDiff cwd = runExceptT do
    inside <- insideGitWorkTree cwd
    if not inside
        then pure GitDiffNotRepository
        else do
            overrides <- executableFilterOverrides cwd
            -- Finish both inspections before selecting the tracked error first.
            (tracked, untracked) <- liftIO $
                concurrently
                    (runExceptT (runTrackedDiff cwd overrides))
                    (runExceptT (runGitSuccess cwd [] untrackedListArguments))
            trackedText <- except tracked
            untrackedOutput <- except untracked
            -- Preserve inspection of every path even if an earlier diff fails.
            untrackedDiffs <- liftIO $
                traverse
                    (runExceptT . runUntrackedDiff cwd overrides)
                    (nulSeparatedPaths untrackedOutput.gitCommandStdout)
            pieces <- except (sequence untrackedDiffs)
            pure (GitDiffOutput (trackedText <> Text.concat pieces))

insideGitWorkTree :: OsPath -> ExceptT Text IO Bool
insideGitWorkTree cwd = do
    output <- ExceptT $
        runSafeGit cwd [] ["rev-parse", "--is-inside-work-tree"]
    pure
        ( output.gitCommandExitCode == ExitSuccess
            && Text.strip (gitOutputText output) == "true"
        )

executableFilterOverrides
    :: OsPath
    -> ExceptT Text IO [(Text, Text)]
executableFilterOverrides cwd = do
    output <- ExceptT $ runSafeGit
        cwd
        []
        [ "config"
        , "--null"
        , "--name-only"
        , "--get-regexp"
        , "^filter\\..*\\.(clean|process)$"
        ]
    if output.gitCommandExitCode == ExitSuccess
        || output.gitCommandExitCode == ExitFailure 1
        then pure $
            concatMap disableDriver
                (configuredFilterDrivers output.gitCommandStdout)
        else throwE (gitFailure "git config" output)
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
    -> ExceptT Text IO Text
runUntrackedDiff cwd overrides path =
    runDiff cwd overrides
        (commonDiffArguments
            <> ["--no-index", "--", "/dev/null", path])

runTrackedDiff
    :: OsPath
    -> [(Text, Text)]
    -> ExceptT Text IO Text
runTrackedDiff cwd overrides = do
    output <- ExceptT $
        runSafeGit cwd [] ["rev-parse", "--verify", "--quiet", "HEAD"]
    case output.gitCommandExitCode of
        ExitSuccess ->
            runDiff cwd overrides (commonDiffArguments <> ["HEAD", "--"])
        ExitFailure 1 -> do
            -- In an unborn repository, staged files are compared with the
            -- empty index while unstaged edits are compared with the index.
            (staged, unstaged) <- liftIO $
                concurrently
                    (runExceptT (runDiff cwd overrides cachedDiffArguments))
                    (runExceptT (runDiff cwd overrides trackedDiffArguments))
            except ((<>) <$> staged <*> unstaged)
        _ -> throwE (gitFailure "git rev-parse HEAD" output)

runDiff
    :: OsPath
    -> [(Text, Text)]
    -> [String]
    -> ExceptT Text IO Text
runDiff cwd overrides arguments = do
    output <- ExceptT (runSafeGit cwd overrides arguments)
    if output.gitCommandExitCode == ExitSuccess
        || output.gitCommandExitCode == ExitFailure 1
        then pure (displayTerminalText (gitOutputText output))
        else throwE (gitFailure (renderGitCommand arguments) output)

runGitSuccess
    :: OsPath
    -> [(Text, Text)]
    -> [String]
    -> ExceptT Text IO GitCommandOutput
runGitSuccess cwd overrides arguments = do
    output <- ExceptT (runSafeGit cwd overrides arguments)
    if output.gitCommandExitCode == ExitSuccess
        then pure output
        else throwE (gitFailure (renderGitCommand arguments) output)

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

cachedDiffArguments :: [String]
cachedDiffArguments =
    "diff" : "--cached" : drop 1 commonDiffArguments

commonDiffArguments :: [String]
commonDiffArguments =
    [ "diff"
    , "--no-textconv"
    , "--no-ext-diff"
    , "--submodule=short"
    , "--ignore-submodules=dirty"
    , "--no-color"
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
    displayTerminalText command
        <> " failed with "
        <> Text.pack (show output.gitCommandExitCode)
        <> case Text.strip (Text.pack output.gitCommandStderr) of
            "" -> ""
            stderr -> ": " <> displayTerminalText stderr

renderGitCommand :: [String] -> Text
renderGitCommand arguments =
    displayTerminalText (Text.pack (intercalate " " ("git" : arguments)))

-- | Add inert terminal color after Git output has been stripped of all
-- repository-controlled escape and control characters.
colorizeGitDiff :: Bool -> Text -> Text
colorizeGitDiff False = id
colorizeGitDiff True =
    Text.intercalate "\n" . map colorLine . Text.splitOn "\n"
  where
    colorLine line
        | "+++ " `Text.isPrefixOf` line
            || "--- " `Text.isPrefixOf` line =
                roleMuted True line
        | "+" `Text.isPrefixOf` line = roleSuccess True line
        | "-" `Text.isPrefixOf` line = roleError True line
        | "@@" `Text.isPrefixOf` line
            || "diff --git " `Text.isPrefixOf` line
            || "index " `Text.isPrefixOf` line =
                roleMuted True line
        | otherwise = line

gitCommandTimeoutMicros :: Int
gitCommandTimeoutMicros = 30 * 1_000_000

processCleanupTimeoutMicros :: Int
processCleanupTimeoutMicros = 2 * 1_000_000

readHandleStrict :: Handle -> IO String
readHandleStrict handle = do
    contents <- ByteString.hGetContents handle
    pure
        (Text.unpack
            (Text.decodeUtf8With lenientDecode contents))
