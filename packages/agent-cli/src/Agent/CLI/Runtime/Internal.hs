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

import Agent.CLI.AgentSessions
    ( closeSessionThreadManager, newSessionThreadManager )
import Agent.CLI.GatewayClient (runGatewayCommand)
import Agent.CLI.Interrupt ( catchUserInterrupt )
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.McpOAuth
    ( LoginOptions(..)
    , defaultLoginOptions
    , loginMcpWith
    , logoutMcp
    )
import Agent.CLI.McpStatus
    ( formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    )
import Agent.CLI.Options
    ( CliOptions
    , Command(..)
    , McpCommand(..)
    , parseArgs
    , usage
    )
import Agent.CLI.Provider.Switch ( accountSwitchTarget )
import Agent.CLI.Runtime.Orchestration
    ( runAgentWithRuntime, withRestoredCurrentDirectory )
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..), foregroundRunMode )
import Agent.CLI.Runtime.Types ( DevResult(..) )
import Agent.CLI.Session ( sessionsRoot )
import Agent.CLI.Session.Interaction ( buildPromptState )
import Agent.CLI.SessionAdmin
    ( runImportSession
    , runListSessions
    , runShowSession
    , runStorageAdmin
    , runWaitSession
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
import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception.Safe ( finally, mask_, onException )
import Data.Text ( Text )
import System.Directory.OsPath
    ( getCurrentDirectory, getHomeDirectory, makeAbsolute )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.IO ( stderr )

import qualified Agent.MCP as MCP
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as Text

-- | GHCi @:cmd@ helper: on 'DevReload', reload modules and resume that exact
-- session. Keeping the id in the generated GHCi command avoids a shared
-- cross-process resume pointer when several development REPLs are open.
afterDev :: DevResult -> IO String
afterDev = \case
    DevQuit -> pure ""
    DevReload sessionId -> pure $ unlines
        [ ":reload"
        , ":module +Agent.CLI"
        , ":cmd afterDev =<< devMainResume (Just "
            <> show (Text.unpack sessionId)
            <> ")"
        ]

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
        , "--model", "gpt-5.6-sol"
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
        Right (Mcp (McpLogin url scopes)) -> loginMcpWithScopes scopes url >> pure DevQuit
        Right (Mcp (McpLogout url)) -> logoutMcp url >> pure DevQuit
        Right (ListSessions outputFormat) ->
            runListSessions outputFormat >> pure DevQuit
        Right (ShowSession sessionId outputFormat pageRequest) ->
            runShowSession sessionId outputFormat pageRequest >> pure DevQuit
        Right (WaitSession sessionId) -> runWaitSession sessionId >> pure DevQuit
        Right (ImportSession cwd) -> runImportSession cwd >> pure DevQuit
        Right (Storage command) ->
            runStorageAdmin command >> pure DevQuit
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure DevQuit
                DevReload sessionId -> pure (DevReload sessionId)

run :: IO ()
run = do
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
        Right (Mcp (McpLogin url scopes)) -> loginMcpWithScopes scopes url
        Right (Mcp (McpLogout url)) -> logoutMcp url
        Right (ListSessions outputFormat) -> runListSessions outputFormat
        Right (ShowSession sessionId outputFormat pageRequest) ->
            runShowSession sessionId outputFormat pageRequest
        Right (WaitSession sessionId) -> runWaitSession sessionId
        Right (ImportSession cwd) -> runImportSession cwd
        Right (Storage command) -> runStorageAdmin command
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure ()
                DevReload _ ->
                    die ":reload is only available under `repl` (nix develop)"

-- | Tear down and rebuild provider-specific auth, tools, prompt, and transport.
-- Automatic transitions carry the exact failed turn in memory and commit
-- persisted provider metadata only after the replacement backend succeeds.
runAgentWithRestarts :: CliOptions -> IO DevResult
runAgentWithRestarts options =
    catchUserInterrupt
        (do
            home <- getHomeDirectory
            let root = sessionsRoot home
            elicitationRef <- newIORef Nothing
            cleanupStarted <- newIORef False
            cleanupRequest <- newEmptyMVar
            -- Cleanup is intentionally process-scoped rather than
            -- session-scoped: provider restarts must not rescan every
            -- worktree. 'withAsync' owns and joins the worker on shutdown.
            withAsync (takeMVar cleanupRequest >>= id) \_ -> do
                mcpSupervisor <-
                    MCP.newMcpSupervisorWith
                        MCP.defaultMcpHostHooks
                            { MCP.mcpHostElicit = readIORef elicitationRef }
                sessionThreads <-
                    newSessionThreadManager root
                        `onException` MCP.closeMcpSupervisor mcpSupervisor
                let startCleanup action = mask_ do
                        shouldStart <- atomicModifyIORef'
                            cleanupStarted
                            (\started -> (True, not started))
                        if shouldStart
                            then putMVar cleanupRequest action
                            else pure ()
                    processRuntime = AgentProcessRuntime
                        { processMcpSupervisor = mcpSupervisor
                        , processSessionThreads = sessionThreads
                        , processStartCleanup = startCleanup
                        , processMcpElicitation = elicitationRef
                        }
                withRestoredCurrentDirectory
                    (runAgentWithRuntime processRuntime foregroundRunMode options)
                    `finally`
                        (closeSessionThreadManager sessionThreads
                            `finally` MCP.closeMcpSupervisor mcpSupervisor))
        (pure DevQuit)

loginMcpWithScopes :: [Text] -> Text -> IO ()
loginMcpWithScopes scopes =
    loginMcpWith defaultLoginOptions { loginAdditionalScopes = scopes }
