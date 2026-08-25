module Agent.CLI.Runtime.Orchestration.Background
    ( applyBackgroundApproval
    , runInProcessSessionTurn
    ) where

import Agent.CLI.Options ( ApprovalPolicy(..), CliOptions(..), ScreenMode(..) )
import Agent.CLI.Runtime.Orchestration.Types ( AgentRunMode, backgroundRunMode )
import Agent.CLI.Runtime.Types ( DevResult(..) )
import Agent.CLI.Session
    ( SessionHandle(..), SessionMeta(..) )
import Agent.OsPath ( unsafeToFilePath )
import Data.Text ( Text )
import System.IO ( IOMode(AppendMode), withFile )
import System.OsPath ( (</>), unsafeEncodeUtf )
import System.Posix.Files ( setFileMode )

runInProcessSessionTurn
    :: (AgentRunMode -> CliOptions -> IO DevResult)
    -> CliOptions
    -> ApprovalPolicy
    -> Bool
    -> Bool
    -> SessionHandle
    -> Text
    -> IO (Either Text ())
runInProcessSessionTurn runAgent parentOptions policy ghciEnabled bashEnabled
        handle message =
    withFile logPath AppendMode \logHandle -> do
        setFileMode logPath 0o600
        runAgent
            (backgroundRunMode logHandle handle.sessionMeta.metaCwd)
            childOptions >>= \case
                DevQuit -> pure (Right ())
                DevReload _ ->
                    pure (Left "background agent session requested a reload")
  where
    logPath =
        unsafeToFilePath
            (handle.sessionDir </> unsafeEncodeUtf "agent.log")
    childOptions =
        applyBackgroundApproval policy $
            parentOptions
                { optProvider = Nothing
                , optModel = Nothing
                , optCwd = Nothing
                , optWorktree = False
                , optEffort = Nothing
                , optPrompt = Just message
                , optPromptFile = Nothing
                , optManagedTurnFile = Nothing
                , optResume = Just handle.sessionMeta.metaId
                , optSaveSession = True
                , optGhci = ghciEnabled
                , optBash = bashEnabled
                , optScreenMode = ScreenMinimal
                }

applyBackgroundApproval :: ApprovalPolicy -> CliOptions -> CliOptions
applyBackgroundApproval policy options =
    case policy of
        ApproveAll ->
            options
                { optYolo = True
                , optNoYolo = False
                , optManagedDenyMutations = False
                }
        DenyMutating ->
            options
                { optYolo = False
                , optNoYolo = True
                , optManagedDenyMutations = True
                }
        PromptMutating ->
            -- Background sessions cannot safely borrow the caller's stdin.
            -- Keep the prompt policy marker; non-TTY one-shot resolution
            -- conservatively denies mutating calls.
            options
                { optYolo = False
                , optNoYolo = True
                , optManagedDenyMutations = False
                }
