-- | Subagent tools for a host that runs 'Agent.Loop' itself.
--
-- The command-line app builds the same collaboration tools around a
-- provider-specific child runtime. A library host has no such runtime: it
-- already owns the parent backend, tool registry, and approval policy. This
-- module installs the shared registry and runs each child through another
-- 'runLoopInputsDetailed' using a loop the host prepares.
--
-- Give these tools only to the root loop. A child that receives them spawns
-- as a sibling, because the context installed here is the root context.
-- Depth, concurrency, and the per-turn admission budget are still enforced
-- by the registry when a child is spawned from a nested context.
module Agent.Tools.SubagentLoop
    ( LibrarySubagentConfig(..)
    , LibraryChildRequest(..)
    , PreparedChildLoop(..)
    , LibrarySubagents
    , openLibrarySubagents
    , closeLibrarySubagents
    , librarySubagentTools
    , libraryParentTools
    , runLoopWithSubagents
    , plainLibraryChildInputs
    ) where

import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , renderInterAgentMessage
    )
import Agent.Loop
    ( LoopConfig(..)
    , LoopError(..)
    , LoopEvent
    , LoopExecution(..)
    , LoopProgress(..)
    , LoopResult
    , TurnInput(..)
    , runLoopInputsDetailed
    )
import Agent.Subagents
    ( RootTurnId
    , SubagentConfig
    , SubagentId
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , abortRootTurn
    , beginRootTurn
    , closeSubagentRegistry
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents
    ( CollaborationSpawnOptions
    , MultiAgentContext(..)
    , multiAgentTools
    )
import Agent.Tools.Types (AppTool, ToolRegistry, mkToolRegistry, toolRegistryTools)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Exception.Safe (finally)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.OsPath (OsPath)

data LibrarySubagentConfig = LibrarySubagentConfig
    { librarySubagentCwd :: !OsPath
    , librarySubagentLimits :: !SubagentConfig
    -- | Build the child loop for one admitted prompt. The registry supplies
    -- the previous response id for a follow-up to the same child. Honor
    -- 'libraryChildOptions' when the host can change model, effort, or
    -- forked history; otherwise reject an override here.
    , libraryPrepareChild :: !(LibraryChildRequest -> IO (Either Text PreparedChildLoop))
    , libraryOnChildEvent :: !(SubagentId -> LoopEvent -> IO ())
    , librarySendToRoot :: !(Maybe (InterAgentMessage -> IO (Either Text Text)))
    , libraryAllowedChildModels :: !(Maybe [Text])
    , librarySpawnModelGuidance :: !(Maybe Text)
    }

data LibraryChildRequest = LibraryChildRequest
    { libraryChildEnv :: !SubagentSpawnEnv
    , libraryChildPreviousResponseId :: !(Maybe Text)
    , libraryChildMessage :: !InterAgentMessage
    , libraryChildOptions :: !CollaborationSpawnOptions
    }

data PreparedChildLoop = PreparedChildLoop
    { preparedChildConfig :: !LoopConfig
    , preparedChildInputs :: ![TurnInput]
    , preparedChildPrevious :: !(Maybe Text)
    }

data LibrarySubagents = LibrarySubagents
    { sessionRegistry :: !SubagentRegistry
    , sessionTools :: ![AppTool]
    , sessionSpawnOptions :: !(IORef (Map SubagentId CollaborationSpawnOptions))
    , sessionRootTurn :: !(IORef (Maybe RootTurnId))
    , sessionTurnLock :: !(MVar ())
    }

openLibrarySubagents :: LibrarySubagentConfig -> IO LibrarySubagents
openLibrarySubagents config = do
    spawnOptions <- newIORef Map.empty
    rootTurn <- newIORef Nothing
    turnLock <- newMVar ()
    registry <- newSubagentRegistry
        config.librarySubagentLimits
        config.librarySubagentCwd
        (runPreparedChild config spawnOptions)
        config.libraryOnChildEvent
    let context = MultiAgentContext
            { multiRegistry = registry
            , multiCwd = config.librarySubagentCwd
            , multiSelfId = Nothing
            , multiDepth = 0
            , multiTaskPath = taskPathRoot
            , multiRootTurnId = readIORef rootTurn
            , multiResumeFromDisk = Nothing
            , multiCreateWorktree = Nothing
            , multiPrepareSpawn = Just \agentId options ->
                atomicModifyIORef' spawnOptions \known ->
                    (Map.insert agentId options known, ())
            , multiSendToRoot = config.librarySendToRoot
            , multiSpawnModelGuidance = config.librarySpawnModelGuidance
            , multiAllowedChildModels = config.libraryAllowedChildModels
            , multiResolveChildModel = Nothing
            , multiChildModelAllowed = Nothing
            }
        session = LibrarySubagents
            { sessionRegistry = registry
            , sessionTools = multiAgentTools context
            , sessionSpawnOptions = spawnOptions
            , sessionRootTurn = rootTurn
            , sessionTurnLock = turnLock
            }
    pure session

closeLibrarySubagents :: LibrarySubagents -> IO ()
closeLibrarySubagents session =
    closeSubagentRegistry session.sessionRegistry

librarySubagentTools :: LibrarySubagents -> [AppTool]
librarySubagentTools = (.sessionTools)

-- | Append the collaboration tools to a host registry. Fails when a host
-- tool already uses one of those names.
libraryParentTools
    :: LibrarySubagents
    -> ToolRegistry
    -> Either Text ToolRegistry
libraryParentTools session registry =
    mkToolRegistry (toolRegistryTools registry <> session.sessionTools)

-- | Run one root loop with collaboration tools installed. The admission
-- budget belongs to this call. A second overlapping call is rejected.
-- Children are interrupted when the root loop returns.
runLoopWithSubagents
    :: LibrarySubagents
    -> LoopConfig
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopExecution
runLoopWithSubagents session config previous inputs =
    case libraryParentTools session config.loopTools of
        Left err -> pure (rejectedExecution err)
        Right tools ->
            withRootTurn session (run tools) >>= \case
                Left err -> pure (rejectedExecution err)
                Right execution -> pure execution
  where
    run tools =
        runLoopInputsDetailed
            config
                { loopTools = tools
                , loopReadTools = reread tools
                }
            previous
            inputs
    reread merged = case config.loopReadTools of
        Nothing -> Nothing
        Just readBase -> Just do
            base <- readBase
            pure case libraryParentTools session base of
                Right refreshed -> refreshed
                Left _ ->
                    if null (toolRegistryTools base)
                        then merged
                        else base

-- | Plain-text child prompt. Encrypted collaboration payloads stay rejected
-- until a host can pass them through its provider's native message item.
plainLibraryChildInputs :: InterAgentMessage -> Either Text [TurnInput]
plainLibraryChildInputs message =
    case message.messageContent of
        EncryptedInterAgentContent _ ->
            Left "encrypted child messages are not supported by this host"
        PlainInterAgentContent _ ->
            Right [UserMessage (renderInterAgentMessage message)]

runPreparedChild
    :: LibrarySubagentConfig
    -> IORef (Map SubagentId CollaborationSpawnOptions)
    -> SubagentSpawnEnv
    -> Maybe Text
    -> InterAgentMessage
    -> (LoopEvent -> IO ())
    -> IO (Either LoopError LoopResult)
runPreparedChild config spawnOptions env previous message onEvent = do
    known <- readIORef spawnOptions
    case Map.lookup env.subId known of
        Nothing ->
            pure (Left (LoopUnexpected "subagent spawn options were not prepared"))
        Just options -> do
            prepared <- config.libraryPrepareChild LibraryChildRequest
                { libraryChildEnv = env
                , libraryChildPreviousResponseId = previous
                , libraryChildMessage = message
                , libraryChildOptions = options
                }
            case prepared of
                Left err -> pure (Left (LoopUnexpected err))
                Right child ->
                    (.executionResult) <$> runLoopInputsDetailed
                        child.preparedChildConfig
                            { loopCancel = env.subCancel
                            , loopOnEvent = \event -> do
                                child.preparedChildConfig.loopOnEvent event
                                onEvent event
                            }
                        child.preparedChildPrevious
                        child.preparedChildInputs

withRootTurn :: LibrarySubagents -> IO a -> IO (Either Text a)
withRootTurn session action =
    tryTakeMVar session.sessionTurnLock >>= \case
        Nothing -> pure (Left "a subagent root turn is already running")
        Just () -> do
            value <-
                (do
                    turnId <- beginRootTurn session.sessionRegistry
                    writeIORef session.sessionRootTurn (Just turnId)
                    action `finally` do
                        abortRootTurn session.sessionRegistry turnId
                        writeIORef session.sessionRootTurn Nothing
                ) `finally` putMVar session.sessionTurnLock ()
            pure (Right value)

rejectedExecution :: Text -> LoopExecution
rejectedExecution message = LoopExecution
    { executionState = []
    , executionPendingInputs = []
    , executionProgress = NoResponseCommitted
    , executionUncommittedAssistantText = Nothing
    , executionUncommittedDisplayEvents = []
    , executionProviderTelemetry = []
    , executionResult = Left (LoopUnexpected message)
    }
