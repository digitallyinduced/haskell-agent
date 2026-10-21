-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI.Runtime.Internal
    ( BuildInfo(..)
    , DevResult(..)
    , agentBuildInfo
    , afterDev
    , accountSwitchTarget
    , applyReplMode
    , buildPromptState
    , cycleReplInteraction
    , devArgs
    , devMain
    , devMainResume
    , formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    , formatBuildInfo
    , formatBuildInfoCompact
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , formatTokensPerSecond
    , formatUsageWithRate
    , learnAboutUserOnboardingPrompt
    , run
    , withRestoredCurrentDirectory
    ) where

import Agent.Runtime.GatewayClient (runGatewayCommand)
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.McpCatalog (runMcpCommand)
import Agent.CLI.WorktreeAdmin (runWorktreeAdmin)
import Agent.CLI.McpStatus
    ( formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    )
import Agent.CLI.Options
    ( CliOptions
    , StorageCommand(..)
    , LocalServerCommand(..)
    , Command(..)
    , parseArgs
    , usage
    )
import Agent.CLI.Provider.Switch ( accountSwitchTarget )
import Agent.CLI.Runtime.Orchestration
    ( withRestoredCurrentDirectory )
import Agent.CLI.Runtime.Types ( DevResult(..) )
import Agent.CLI.Remote.Session (runRemoteSession)
import Agent.CLI.Remote.Transport (RemoteFailure(..))
import Control.Exception.Safe (catch)
import Agent.CLI.Remote.Service (runLocalServerCommand)
import Agent.CLI.Remote.Development (configureDevelopmentService, stopDevelopmentService)
import Agent.CLI.Remote.Admin (runRemoteList, runRemoteShow, runRemoteWait)
import Agent.CLI.Session.Interaction ( buildPromptState )
import Agent.CLI.SessionAdmin
    ( runImportSession
    , runStorageAdmin
    )
import Agent.CLI.Startup.Auth ( learnAboutUserOnboardingPrompt )
import Agent.CLI.Startup.Format
    ( BuildInfo(..)
    , agentBuildInfo
    , formatBuildInfo
    , formatBuildInfoCompact
    , formatRepositoryPath
    , formatStartupTimings
    )
import Agent.CLI.Status
    ( applyReplMode
    , cycleReplInteraction
    , formatReplStatusLine
    , formatTokenUsage
    , formatTokensPerSecond
    , formatUsageWithRate
    )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Worktree ( isUnderWorktreeRoot, worktreeRoot )
import Data.Text ( Text )
import System.Directory.OsPath
    ( getCurrentDirectory, getHomeDirectory, makeAbsolute )
import System.Environment ( getArgs, setEnv )
import System.Exit ( die )
import System.IO ( stderr )

import qualified Data.Text as Text

-- | GHCi @:cmd@ helper: on 'DevReload', reload modules and resume that exact
-- session. Keeping the id in the generated GHCi command avoids a shared
-- cross-process resume pointer when several development REPLs are open.
afterDev :: DevResult -> IO String
afterDev = \case
    DevQuit -> pure ""
    DevReload sessionId -> stopDevelopmentService >> (pure $ unlines
        [ ":reload"
        , ":module +Agent.CLI"
        , ":cmd afterDev =<< devMainResume (Just "
            <> show (Text.unpack sessionId)
            <> ")"
        ])

-- | Arguments used by the development @repl@ launcher.
--
-- Fresh sessions use OpenAI's frontier model in yolo mode. Reloaded sessions
-- keep their persisted provider and model while reapplying the yolo default.
devArgs :: Maybe Text -> Bool -> [String]
devArgs resumeId underWorktree = case resumeId of
    Just sessionId ->
        [ "--yolo"
        , "--resume", Text.unpack sessionId
        ]
    Nothing ->
        [ "--provider", "openai"
        , "--model", "gpt-6-sol"
        , "--yolo"
        ]
            <> ["--worktree" | not underWorktree]

-- | Start a fresh agent from GHCi (@repl@).
devMain :: IO DevResult
devMain = devMainResume Nothing

-- | Start or resume the GHCi-driven agent. 'afterDev' embeds the session id in
-- the next @:cmd@ invocation, so concurrent REPLs cannot consume each other's
-- reload state.
devMainResume :: Maybe Text -> IO DevResult
devMainResume resumeId = do
    configureClientIdentity
    configureDevelopmentService
    home <- getHomeDirectory
    underWorktree <- case resumeId of
        Just _ -> pure True
        Nothing -> do
            cwd <- makeAbsolute =<< getCurrentDirectory
            root <- makeAbsolute (worktreeRoot home)
            pure (isUnderWorktreeRoot root cwd)
    let args = devArgs resumeId underWorktree
    case parseArgs args of
        Left err -> die err
        Right ShowHelp -> putStr usage >> pure DevQuit
        Right ShowVersion ->
            putStrLn (Text.unpack (formatBuildInfo agentBuildInfo)) >> pure DevQuit
        Right Login -> do
            color <- resolveColor stderr
            runLoginManager color
            pure DevQuit
        Right (Gateway command) -> runGatewayCommand command >> pure DevQuit
        Right (Mcp command) -> runMcpCommand command >> pure DevQuit
        Right (ListSessions outputFormat) ->
            runRemoteList outputFormat >> pure DevQuit
        Right (ShowSession sessionId outputFormat pageRequest) ->
            runRemoteShow sessionId outputFormat pageRequest >> pure DevQuit
        Right (WaitSession sessionId) -> runRemoteWait sessionId >> pure DevQuit
        Right (ImportSession cwd) -> runImportSession cwd >> pure DevQuit
        Right (Storage command) ->
            runStorageAdminSafely command >> pure DevQuit
        Right (Worktree command) ->
            runWorktreeAdmin command >> pure DevQuit
        Right (LocalServer command) -> runLocalServerCommand command >> pure DevQuit
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure DevQuit
                DevReload sessionId -> pure (DevReload sessionId)

run :: IO ()
run = runCommand `catch` \(RemoteFailure message) -> die (Text.unpack message)

runCommand :: IO ()
runCommand = do
    configureClientIdentity
    args <- getArgs
    case parseArgs args of
        Left err -> die err
        Right ShowHelp -> putStr usage
        Right ShowVersion ->
            putStrLn (Text.unpack (formatBuildInfo agentBuildInfo))
        Right Login -> do
            color <- resolveColor stderr
            runLoginManager color
        Right (Gateway command) -> runGatewayCommand command
        Right (Mcp command) -> runMcpCommand command
        Right (ListSessions outputFormat) -> runRemoteList outputFormat
        Right (ShowSession sessionId outputFormat pageRequest) ->
            runRemoteShow sessionId outputFormat pageRequest
        Right (WaitSession sessionId) -> runRemoteWait sessionId
        Right (ImportSession cwd) -> runImportSession cwd
        Right (Storage command) -> runStorageAdminSafely command
        Right (Worktree command) -> runWorktreeAdmin command
        Right (LocalServer command) -> runLocalServerCommand command
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure ()
                DevReload _ ->
                    die ":reload is only available under `repl` (nix develop)"

configureClientIdentity :: IO ()
configureClientIdentity =
    setEnv "AGENT_BUILD_COMMIT" (Text.unpack agentBuildInfo.buildCommit)

runStorageAdminSafely :: StorageCommand -> IO ()
runStorageAdminSafely command = do
    case command of
        StorageStop -> runLocalServerCommand ServerStop
        _ -> pure ()
    runStorageAdmin command

-- | The frontend leaves process resources and teardown with the local service.
runAgentWithRestarts :: CliOptions -> IO DevResult
runAgentWithRestarts = runRemoteSession

