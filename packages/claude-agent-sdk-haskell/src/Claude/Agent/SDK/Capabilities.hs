{-# LANGUAGE OverloadedStrings #-}

-- | Bounded capability discovery for the Claude Code executable.
--
-- Probing is deliberately kept separate from transport construction.  Callers
-- may use the pure parsers in tests, while production code can opt in to the
-- bounded process probe before starting a session.
module Claude.Agent.SDK.Capabilities
    ( ClaudeAgentCapabilities(..)
    , claudeAgentSDKVersion
    , parseClaudeVersion
    , parseClaudeHelp
    , probeClaudeCapabilities
    , probeClaudeCapabilitiesIn
    , validateSubprocessArguments
    , capabilitySupportsFlag
    , capabilitySupportsPermissionMode
    ) where

import Agent.Process (terminateProcessGroup)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , mask
    , tryAny
    )
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Version
import qualified Paths_claude_agent_sdk_haskell as Paths
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hSetBinaryMode, hSetBuffering, BufferMode(NoBuffering))
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , proc
    , waitForProcess
    )
import System.Timeout (timeout)
import System.IO.Unsafe (unsafePerformIO)

data ClaudeAgentCapabilities = ClaudeAgentCapabilities
    { capabilityVersion :: !(Maybe Text)
    , supportedFlags :: !(Set Text)
    , permissionModes :: !(Set Text)
    , supportsStreaming :: !Bool
    , supportsSafeMode :: !Bool
    , warnings :: ![Text]
    }
    deriving (Eq, Show)

claudeAgentSDKVersion :: Text
claudeAgentSDKVersion = Text.pack (showVersion Paths.version)
  where
    showVersion = Data.Version.showVersion

parseClaudeVersion :: Text -> Maybe Text
parseClaudeVersion input =
    case Text.words input of
        first : _ ->
            let candidate = Text.takeWhile (\c -> c == '.' || c >= '0' && c <= '9') first
            in if Text.any (\c -> c >= '0' && c <= '9') candidate
                then Just candidate
                else Nothing
        [] -> Nothing

parseClaudeHelp :: Text -> ClaudeAgentCapabilities
parseClaudeHelp help =
    let ls = Text.lines help
        flagLines = Set.fromList
            [ Text.takeWhile (\c -> c /= ' ' && c /= '\t' && c /= ',') (Text.strip line)
            | line <- ls
            , let stripped = Text.strip line
            , "--" `Text.isPrefixOf` stripped || "-p " `Text.isPrefixOf` stripped
            ]
        allFlags = Set.fromList
            [ token
            | line <- ls
            , token <- Text.words line
            , let token' = Text.takeWhile (\c -> c /= ',' && c /= '>' && c /= '=') token
            , let token = token'
            , "--" `Text.isPrefixOf` token || token == "-p"
            ]
        permissions = Set.fromList
            [ value
            | value <- knownPermissionModes
            , value `Text.isInfixOf` help
            , any ("permission-mode" `Text.isInfixOf`) ls
            ]
        flags = Set.union flagLines allFlags
        streaming = Set.member "--input-format" flags
            && Set.member "--output-format" flags
            && "stream-json" `Text.isInfixOf` help
        safe = Set.member "--safe-mode" flags
    in ClaudeAgentCapabilities
        { capabilityVersion = Nothing
        , supportedFlags = flags
        , permissionModes = permissions
        , supportsStreaming = streaming
        , supportsSafeMode = safe
        , warnings =
            [ "Claude Code help does not advertise stream-json input/output."
            | not streaming
            ] <> [ "Claude Code help does not advertise --safe-mode."
                 | not safe
                 ]
        }
  where
    knownPermissionModes =
        [ "acceptEdits", "auto", "bypassPermissions", "manual"
        , "dontAsk", "plan", "default"
        ]

capabilitySupportsFlag :: ClaudeAgentCapabilities -> Text -> Bool
capabilitySupportsFlag capabilities flag =
    Set.member flag capabilities.supportedFlags

capabilitySupportsPermissionMode :: ClaudeAgentCapabilities -> Text -> Bool
capabilitySupportsPermissionMode capabilities mode =
    Set.member mode capabilities.permissionModes

validateSubprocessArguments
    :: ClaudeAgentCapabilities
    -> [String]
    -> Either Text ()
validateSubprocessArguments capabilities args
    | not capabilities.supportsStreaming =
        Left "Claude Code does not support the required stream-json protocol."
    | otherwise =
        case [ flag
             | flag <- requiredFlags
             , flag `notElem` args
             ] of
            missing : _ ->
                Left ("Claude Code invocation is missing required flag " <> Text.pack missing)
            [] ->
                case [ flag
                     | flag <- securityCriticalFlags
                     , flag `elem` args
                     , not (capabilitySupportsFlag capabilities (Text.pack flag))
                     ] of
                    unsupported : _ ->
                        Left ("Refusing to use unsupported security flag " <> Text.pack unsupported)
                    [] -> Right ()
  where
    requiredFlags = ["-p", "--input-format", "--output-format", "--verbose"]
    securityCriticalFlags =
        [ "--safe-mode"
        , "--strict-mcp-config"
        , "--permission-mode"
        , "--allowedTools"
        , "--disallowedTools"
        ]

probeClaudeCapabilities :: FilePath -> IO (Either Text ClaudeAgentCapabilities)
probeClaudeCapabilities executable = getCurrentDirectory >>= probeClaudeCapabilitiesIn executable

probeClaudeCapabilitiesIn :: FilePath -> FilePath -> IO (Either Text ClaudeAgentCapabilities)
probeClaudeCapabilitiesIn executable cwd = do
    versionResult <- runProbe executable cwd ["--version"]
    case versionResult of
        Left err -> pure (Left err)
        Right (exitCode, versionOutput, _versionErr) ->
            case parseClaudeVersion versionOutput of
                Nothing -> pure (Left "Unable to parse Claude Code version output.")
                Just version -> do
                    let key = executable <> "\0" <> Text.unpack version
                    cached <- readIORef capabilityCache
                    case Map.lookup key cached of
                        Just result -> pure (Right result)
                        Nothing -> do
                            helpResult <- runProbe executable cwd ["--help"]
                            let result = case helpResult of
                                    Left err -> ClaudeAgentCapabilities
                                        { capabilityVersion = Just version
                                        , supportedFlags = Set.empty
                                        , permissionModes = Set.empty
                                        , supportsStreaming = False
                                        , supportsSafeMode = False
                                        , warnings = [err]
                                        }
                                    Right (helpExit, helpOutput, helpErr) ->
                                        let parsed = parseClaudeHelp helpOutput
                                            failures =
                                                [ "Claude Code --version exited unsuccessfully."
                                                | exitCode /= ExitSuccess
                                                ] <> [ "Claude Code --help exited unsuccessfully."
                                                     | helpExit /= ExitSuccess
                                                     ] <> [ "Claude Code probe stderr: " <> helpErr
                                                           | not (Text.null (Text.strip helpErr))
                                                           ]
                                        in parsed
                                            { capabilityVersion = Just version
                                            , warnings = failures <> parsed.warnings
                                            }
                            atomicModifyIORef' capabilityCache \m -> (Map.insert key result m, ())
                            pure (Right result)
capabilityCache :: IORef (Map.Map String ClaudeAgentCapabilities)
capabilityCache = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE capabilityCache #-}

runProbe :: FilePath -> FilePath -> [String] -> IO (Either Text (ExitCode, Text, Text))
runProbe executable cwd args =
    mask \restore -> do
        created <- tryAny $ createProcess
            (proc executable args)
                { cwd = Just cwd
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = True
                , create_group = True
                , new_session = True
                }
        case created of
            Left exception -> pure (Left (Text.pack (show (exception :: SomeException))))
            Right (Nothing, Nothing, Nothing, _) ->
                pure (Left "Claude Code probe did not provide output handles.")
            Right (Nothing, Just output, Just error, processHandle) -> do
                outputReader <- async (readBounded output)
                errorReader <- async (readBounded error)
                groupId <- getPid processHandle
                outcome <- restore (timeout probeTimeoutMicros (waitForProcess processHandle))
                    `catchAny` \_ -> pure Nothing
                when (outcome == Nothing) do
                    terminateProcessGroup groupId processHandle
                outputText <- waitBounded outputReader
                errorText <- waitBounded errorReader
                case outcome of
                    Nothing -> pure (Left "Claude Code capability probe timed out.")
                    Just exitCode -> pure (Right (exitCode, outputText, errorText))
            Right (_, _, _, processHandle) -> do
                terminateProcessGroup Nothing processHandle
                pure (Left "Claude Code probe returned incomplete output handles.")
  where
    probeTimeoutMicros = 3_000_000
    waitBounded reader = do
        result <- timeout 1_000_000 (waitCatch reader)
        case result of
            Just (Right text) -> pure text
            _ -> cancel reader >> pure ""

readBounded :: Handle -> IO Text
readBounded handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering
    go ByteString.empty
  where
    limit = 256 * 1024
    go acc = do
        chunk <- ByteString.hGetSome handle 8192
        if ByteString.null chunk
            then do
                voidClose handle
                pure (decode acc)
            else
                let next = ByteString.take limit (acc <> chunk)
                in if ByteString.length next >= limit
                    then do
                        voidClose handle
                        pure (decode next)
                    else go next
    decode = TextEncoding.decodeUtf8With lenientDecode

voidClose :: Handle -> IO ()
voidClose handle = hClose handle `catchAny` \_ -> pure ()
