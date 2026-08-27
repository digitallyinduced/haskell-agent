-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI.Runtime.Internal
    ( DevResult(..)
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
import Agent.CLI.Interrupt ( catchUserInterrupt )
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.McpStatus
    ( formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    )
import Agent.CLI.Options
    ( CliOptions
    , Command(..)
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
    ( formatRepositoryPath, formatStartupTimings )
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
import Control.Exception.Safe ( finally, onException )
import Data.Text ( Text )
import System.Directory.OsPath
    ( getCurrentDirectory, getHomeDirectory, makeAbsolute )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.IO ( stderr )

import qualified Agent.MCP as MCP
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
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0" >> pure DevQuit
        Right Login -> do
            color <- resolveColor stderr
            runLoginManager color
            pure DevQuit
        Right ListSessions -> runListSessions >> pure DevQuit
        Right (ShowSession sessionId) -> runShowSession sessionId >> pure DevQuit
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
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0"
        Right Login -> do
            color <- resolveColor stderr
            runLoginManager color
        Right ListSessions -> runListSessions
        Right (ShowSession sessionId) -> runShowSession sessionId
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
            mcpSupervisor <- MCP.newMcpSupervisor
            sessionThreads <-
                newSessionThreadManager root
                    `onException` MCP.closeMcpSupervisor mcpSupervisor
            let processRuntime = AgentProcessRuntime
                    { processMcpSupervisor = mcpSupervisor
                    , processSessionThreads = sessionThreads
                    }
            withRestoredCurrentDirectory
                (runAgentWithRuntime processRuntime foregroundRunMode options)
                `finally`
                    (closeSessionThreadManager sessionThreads
                        `finally` MCP.closeMcpSupervisor mcpSupervisor))
        (pure DevQuit)
