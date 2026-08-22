-- | CLI-owned runtime state and provider adapters for child agents.
module Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(..)
    , SubagentSession(..)
    , SubagentStoreRoot
    , flushAllSubagentSnapshots
    , freshOpenAiBackend
    , persistSubagentSnapshotWithStatus
    , prepareCollaborationSpawn
    , restoreAgentFromDisk
    , runCodexSubagent
    , runHttpSubagent
    ) where

import Agent.CLI.Approval (childApprove)
import Agent.CLI.Btw (trimDanglingToolSuffix)
import Agent.CLI.Compaction (autoCompactOpenAiBackendWithThreshold)
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.CLI.Options
    ( ApprovalPolicy
    , CliOptions(..)
    , defaultEffortFor
    )
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.Request (requestParams)
import Agent.CLI.SubagentStore
    ( SubagentDiskMeta(..)
    , forkSubagentTranscript
    , loadSubagentState
    , saveSubagentState
    )
import Agent.CLI.Tools (requireToolRegistry, schemasFromAppTools)
import Agent.InterAgentMessage
    ( InterAgentMessage
    , interAgentMessagePayload
    )
import Agent.Loop
    ( Backend(..)
    , LoopConfig(..)
    , LoopError
    , LoopEvent
    , LoopResult(..)
    , TurnInput(..)
    , defaultLoopDispatch
    , runLoop
    , runLoopInputs
    )
import qualified Agent.OpenAI.Client as OpenAI
import Agent.OpenAI.LoopBackend
    ( openAiBackend
    , openAiBackendWithTransportFallback
    , statelessResponsesBackend
    )
import Agent.OpenAI.WebSocketClient (withCodexWsRetrying)
import System.OsPath (OsPath)
import Agent.Provider (Provider(..), TokenProvider)
import Agent.Responses.Types
    ( ReasoningConfig(..)
    , ResponseCreateParams(..)
    , ResponseItem
    )
import Agent.Subagents
    ( RunSubagent
    , SubagentId(..)
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , getPreviousResponseId
    , getStatus
    , getSubagentCwd
    , getSubagentIdentity
    , getTaskPath
    , restoreSubagent
    , restoreSubagentAtStatus
    , restoreSubagentAtWithCwdStatus
    , restoreSubagentWithCwd
    , setPreviousResponseId
    )
import Agent.Subagents.TaskPath
    ( parseTaskPath
    , taskPathRoot
    )
import Agent.Tools
    ( CodingTools(..)
    , codingToolsFor
    , filterChildGrokTools
    )
import Agent.Tools.Grok.Task
    ( GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , defaultSubagentType
    , lookupAgentModel
    , lookupAgentReasoningEffort
    , lookupAgentType
    , recordAgentSpec
    )
import Agent.Tools.MultiAgents
    ( CollaborationSpawnOptions(..)
    , MultiAgentContext(..)
    )
import Agent.Tools.PlanMode (PlanModeEnv(..), PlanModeHooks)
import Agent.Tools.Types
    ( ToolEnv(..)
    , ToolRegistry
    , defaultToolEnv
    )
import Control.Exception.Safe (finally)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, utctDay)

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef [ResponseItem])
    , subSessionContextTokens :: !(IORef (Maybe (Int, Int)))
    }

-- | Optional on-disk root for child transcripts (@sessionDir/agents/<id>@).
type SubagentStoreRoot = IORef (Maybe OsPath)

-- | Provider-neutral dependencies shared by all child-agent backends.
data SubagentRuntime = SubagentRuntime
    { subagentOptions :: !CliOptions
    , subagentPolicy :: !ApprovalPolicy
    , subagentPlanHooks :: !PlanModeHooks
    , subagentParams :: !(IORef ResponseCreateParams)
    , subagentRegistry :: !SubagentRegistry
    , subagentSessions :: !(IORef (Map SubagentId SubagentSession))
    , subagentStoreRoot :: !SubagentStoreRoot
    , subagentTypes :: !GrokSubagentSpecs
    }

data PreparedChild = PreparedChild
    { preparedParentParams :: !ResponseCreateParams
    , preparedSession :: !SubagentSession
    , preparedToolEnv :: !ToolEnv
    , preparedMultiContext :: !MultiAgentContext
    }

-- | Prefer an explicit store root; otherwise fall back to planMode's session dir.
syncStoreRootFromPlan :: SubagentStoreRoot -> PlanModeEnv -> IO ()
syncStoreRootFromPlan storeRootRef planMode = do
    mroot <- readIORef storeRootRef
    case mroot of
        Just _ -> pure ()
        Nothing -> do
            sessionDir <- readIORef planMode.planSessionDir
            case sessionDir of
                Just dir -> writeIORef storeRootRef (Just dir)
                Nothing -> pure ()

prepareCollaborationSpawn
    :: IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> GrokSubagentSpecs
    -> IORef (Maybe (IORef [ResponseItem]))
    -> SubagentId
    -> CollaborationSpawnOptions
    -> IO ()
prepareCollaborationSpawn
        sessionsRef storeRootRef typesRef sourceRef agentId spawnOptions = do
    recordAgentSpec typesRef agentId GrokSubagentSpec
        { agentType = defaultSubagentType
        , modelOverride = spawnOptions.collaborationModel
        , reasoningEffortOverride =
            spawnOptions.collaborationReasoningEffort
        }
    session <-
        lookupOrCreateSubagentSession
            sessionsRef storeRootRef typesRef agentId
    source <- readIORef sourceRef
    sourceItems <- maybe (pure []) readIORef source
    writeIORef session.subSessionTranscript
        (forkSubagentTranscript spawnOptions.collaborationForkTurns sourceItems)

persistSubagentSnapshot
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> IORef [ResponseItem]
    -> IO ()
persistSubagentSnapshot storeRootRef registry typesRef agentId transcriptRef = do
    status <- getStatus registry agentId
    persistSubagentSnapshotWithStatus
        storeRootRef registry typesRef agentId status transcriptRef

persistSubagentSnapshotWithStatus
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> IORef [ResponseItem]
    -> IO ()
persistSubagentSnapshotWithStatus
        storeRootRef registry typesRef agentId status transcriptRef = do
    mroot <- readIORef storeRootRef
    case mroot of
        Nothing -> pure ()
        Just sessionDir -> do
            items <- trimDanglingToolSuffix <$> readIORef transcriptRef
            previous <- getPreviousResponseId registry agentId
            agentType <- lookupAgentType typesRef agentId
            agentModel <- lookupAgentModel typesRef agentId
            reasoningEffort <- lookupAgentReasoningEffort typesRef agentId
            agentCwd <- getSubagentCwd registry agentId
            identity <- getSubagentIdentity registry agentId
            _ <- saveSubagentState
                sessionDir agentId items previous status agentType agentModel
                reasoningEffort agentCwd identity
            pure ()

flushAllSubagentSnapshots
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> IO ()
flushAllSubagentSnapshots storeRootRef registry sessionsRef typesRef = do
    sessions <- readIORef sessionsRef
    mapM_
        (\(agentId, session) ->
            persistSubagentSnapshot storeRootRef registry typesRef agentId
                session.subSessionTranscript)
        (Map.toList sessions)

-- | Rehydrate a closed/missing agent from @sessionDir/agents/<id>@ so
-- 'resume_agent' / 'resume_from' can continue the prior transcript.
restoreAgentFromDisk
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> SubagentId
    -> IO (Either Text ())
restoreAgentFromDisk storeRootRef registry sessionsRef typesRef agentId = do
    status <- getStatus registry agentId
    case status of
        NotFound -> restore
        Closed -> restore
        _ -> pure (Right ())
  where
    restore = do
        mroot <- readIORef storeRootRef
        case mroot of
            Nothing ->
                pure (Left "no session directory; cannot restore subagent from disk")
            Just sessionDir ->
                loadSubagentState sessionDir agentId >>= \case
                    Left err -> pure (Left err)
                    Right Nothing ->
                        -- Same-process close with no disk yet: still reopen.
                        reopenInMemory Nothing Nothing
                    Right (Just (items, meta)) -> do
                        result <- reopenPersisted meta
                        case result of
                            Left err -> pure (Left err)
                            Right () -> do
                                case meta.diskAgentType of
                                    Just agentType ->
                                        recordAgentSpec typesRef agentId GrokSubagentSpec
                                            { agentType
                                            , modelOverride = meta.diskAgentModel
                                            , reasoningEffortOverride =
                                                meta.diskReasoningEffort
                                            }
                                    Nothing -> pure ()
                                transcript <- newIORef items
                                contextTokens <- newIORef Nothing
                                let session =
                                        SubagentSession
                                            { subSessionTranscript = transcript
                                            , subSessionContextTokens = contextTokens
                                            }
                                atomicModifyIORef' sessionsRef \m ->
                                    (Map.insert agentId session m, ())
                                pure (Right ())
    reopenPersisted meta =
        case meta.diskTaskPath of
            Nothing -> restoreAt taskPathRoot
            Just pathText ->
                case parseTaskPath pathText of
                    Left err -> pure (Left err)
                    Right taskPath -> restoreAt taskPath
      where
        restoredStatus = fromMaybe (Completed Nothing) meta.diskStatus
        restoreAt taskPath = do
            let restore =
                    case meta.diskCwd of
                        Just childCwd ->
                            restoreSubagentAtWithCwdStatus
                                registry childCwd
                        Nothing ->
                            restoreSubagentAtStatus registry
            restore
                agentId meta.diskParentId taskPath
                (fromMaybe 1 meta.diskDepth)
                Nothing meta.diskPreviousResponseId restoredStatus
                >>= pure . fmap (const ())
    reopenInMemory previous requestedCwd = do
        restored <- case requestedCwd of
            Just childCwd ->
                restoreSubagentWithCwd
                    registry childCwd agentId Nothing 1 Nothing previous
            Nothing ->
                restoreSubagent registry agentId Nothing 1 Nothing previous
        pure $ case restored of
            Left err -> Left err
            Right _ -> Right ()

-- | Use a disposable WebSocket for side questions so cancellation cannot
-- leave abandoned response frames queued on the main conversation connection.
freshOpenAiBackend
    :: TokenProvider
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
freshOpenAiBackend provider getParams transcript = Backend \previous inputs onEvent ->
    withCodexWsRetrying provider \conn _credential ->
        let Backend submit = openAiBackend conn getParams transcript
        in submit previous inputs onEvent

-- | Child Codex agent: per-agent transcript retained across follow-ups,
-- independently scoped WebSocket requests, and nested multi-agent tools.
runCodexSubagent
    :: SubagentRuntime
    -> TokenProvider
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> RunSubagent
runCodexSubagent runtime tokenProvider sendToRoot =
    \env previous prompt onEvent -> do
        prepared <- prepareChild runtime env sendToRoot
        childModel <- lookupAgentModel runtime.subagentTypes env.subId
        childEffort <- lookupAgentReasoningEffort runtime.subagentTypes env.subId
        coding <-
            codingToolsFor
                OpenAIProvider
                prepared.preparedToolEnv
                (Just runtime.subagentPlanHooks)
                (Just prepared.preparedMultiContext)
        syncStoreRootFromPlan runtime.subagentStoreRoot coding.codingPlanMode
        flip finally coding.codingClose do
            today <- utctDay <$> getCurrentTime
            let (model, effort) =
                    resolveChildModelAndEffort
                        OpenAIProvider
                        prepared.preparedParentParams
                        childModel
                        childEffort
                baseInstructions =
                    fromMaybe
                        (systemPrompt OpenAIProvider env.subCwd today True)
                        prepared.preparedParentParams.instructions
                instructions =
                    baseInstructions
                        <> "\n\nYou are a Codex subagent. Complete the assigned task and "
                        <> "report results clearly. Your agent id is "
                        <> env.subId.unSubagentId
                        <> "."
                tools = coding.codingAppTools
                childParams = requestParams model instructions
                    (schemasFromAppTools OpenAIProvider tools) effort
            toolRegistry <- requireToolRegistry tools
            childParamsRef <- newIORef childParams
            httpFallbackActive <- newIORef False
            let websocketBackend =
                    freshOpenAiBackend tokenProvider
                        (readIORef childParamsRef)
                        prepared.preparedSession.subSessionTranscript
                httpBackend =
                    statelessResponsesBackend
                        (\request _onEvent ->
                            OpenAI.createCodexMessageWithProvider tokenProvider request)
                        (readIORef childParamsRef)
                        prepared.preparedSession.subSessionTranscript
                baseBackend =
                    openAiBackendWithTransportFallback
                        httpFallbackActive
                        websocketBackend
                        httpBackend
                backend =
                    withConnectionRecovery $
                        autoCompactOpenAiBackendWithThreshold
                            runtime.subagentOptions.optCompactThreshold
                            tokenProvider
                            (readIORef childParamsRef)
                            prepared.preparedSession.subSessionTranscript
                            prepared.preparedSession.subSessionContextTokens
                            baseBackend
            runPreparedChild
                runtime env prepared.preparedSession toolRegistry backend onEvent
                (\config ->
                    runLoopInputs config previous [AgentMessage prompt])

-- | Child XAI/OpenRouter agent: HTTP backend, filtered tools by subagent_type.
runHttpSubagent
    :: SubagentRuntime
    -> Provider
    -> (IORef ResponseCreateParams -> IORef [ResponseItem] -> Backend)
    -> RunSubagent
runHttpSubagent runtime provider mkBackend =
    \env previous prompt onEvent -> do
        prepared <- prepareChild runtime env Nothing
        agentType <-
            fromMaybe defaultSubagentType
                <$> lookupAgentType runtime.subagentTypes env.subId
        childModel <- lookupAgentModel runtime.subagentTypes env.subId
        childEffort <-
            lookupAgentReasoningEffort runtime.subagentTypes env.subId
        coding <-
            codingToolsFor
                provider
                prepared.preparedToolEnv
                (Just runtime.subagentPlanHooks)
                (Just prepared.preparedMultiContext)
        flip finally coding.codingClose do
            today <- utctDay <$> getCurrentTime
            let (model, effort) =
                    resolveChildModelAndEffort
                        provider
                        prepared.preparedParentParams
                        childModel
                        childEffort
                baseInstructions = systemPrompt provider env.subCwd today True
                instructions =
                    baseInstructions
                        <> "\n\n"
                        <> grokSubagentSuffix agentType env.subId
                tools = filterChildGrokTools agentType coding.codingAppTools
                childParams = requestParams model instructions
                    (schemasFromAppTools provider tools) effort
            toolRegistry <- requireToolRegistry tools
            childParamsRef <- newIORef childParams
            let backend =
                    withConnectionRecovery $
                        mkBackend
                            childParamsRef
                            prepared.preparedSession.subSessionTranscript
            runPreparedChild
                runtime env prepared.preparedSession toolRegistry backend onEvent
                (\config ->
                    runLoop config previous (interAgentMessagePayload prompt))

prepareChild
    :: SubagentRuntime
    -> SubagentSpawnEnv
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> IO PreparedChild
prepareChild runtime env sendToRoot = do
    parentParams <- readIORef runtime.subagentParams
    childEnv <- defaultToolEnv env.subCwd
    childPath <-
        fromMaybe taskPathRoot
            <$> getTaskPath runtime.subagentRegistry env.subId
    session <-
        lookupOrCreateSubagentSession
            runtime.subagentSessions
            runtime.subagentStoreRoot
            runtime.subagentTypes
            env.subId
    nestedForkSource <- newIORef (Just session.subSessionTranscript)
    let childToolEnv = childEnv { toolCancel = env.subCancel }
        childCtx = MultiAgentContext
            { multiRegistry = runtime.subagentRegistry
            , multiSelfId = Just env.subId
            , multiDepth = env.subDepth
            , multiTaskPath = childPath
            , multiRootTurnId = pure env.subRootTurnId
            , multiResumeFromDisk = Nothing
            , multiCreateWorktree = Nothing
            , multiPrepareSpawn = Just
                (prepareCollaborationSpawn
                    runtime.subagentSessions
                    runtime.subagentStoreRoot
                    runtime.subagentTypes
                    nestedForkSource)
            , multiSendToRoot = sendToRoot
            }
    pure PreparedChild
        { preparedParentParams = parentParams
        , preparedSession = session
        , preparedToolEnv = childToolEnv
        , preparedMultiContext = childCtx
        }

resolveChildModelAndEffort
    :: Provider
    -> ResponseCreateParams
    -> Maybe Text
    -> Maybe Text
    -> (Text, Text)
resolveChildModelAndEffort provider parentParams childModel childEffort =
    ( model
    , fromMaybe inheritedEffort childEffort
    )
  where
    model = fromMaybe
        (fromMaybe (defaultModelFor provider) parentParams.model)
        childModel
    inheritedEffort = case parentParams.reasoning of
        Just cfg -> fromMaybe (defaultEffortFor provider) cfg.effort
        Nothing -> defaultEffortFor provider

runPreparedChild
    :: SubagentRuntime
    -> SubagentSpawnEnv
    -> SubagentSession
    -> ToolRegistry
    -> Backend
    -> (LoopEvent -> IO ())
    -> (LoopConfig -> IO (Either LoopError LoopResult))
    -> IO (Either LoopError LoopResult)
runPreparedChild runtime env session toolRegistry backend onEvent runChild = do
    let config = LoopConfig
            { loopBackend = backend
            , loopTools = toolRegistry
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = runtime.subagentOptions.optMaxTurns
            , loopOnEvent = onEvent
            , loopApprove =
                \call ->
                    childApprove runtime.subagentPolicy toolRegistry call
            , loopCancel = env.subCancel
            }
    result <- runChild config
    case result of
        Right loopResult ->
            setPreviousResponseId
                runtime.subagentRegistry
                env.subId
                loopResult.finalResponseId
        Left _ ->
            modifyIORef' session.subSessionTranscript trimDanglingToolSuffix
    persistSubagentSnapshot
        runtime.subagentStoreRoot
        runtime.subagentRegistry
        runtime.subagentTypes
        env.subId
        session.subSessionTranscript
    pure result

grokSubagentSuffix :: Text -> SubagentId -> Text
grokSubagentSuffix agentType agentId =
    "You are a Grok Build subagent — a focused worker delegated a specific task.\n\
    \Complete the assigned task directly and efficiently. Do not broaden scope beyond what was asked. \
    \Report blocked or unverified work explicitly.\n\n\
    \Subagent type: "
        <> agentType
        <> "\nAgent id: "
        <> agentId.unSubagentId
        <> case agentType of
            "explore" ->
                "\n\n=== READ-ONLY MODE ===\n\
                \You have no file editing or command execution tools. Search broadly, narrow down, \
                \and return absolute file paths and relevant findings."
            "plan" ->
                "\n\n=== READ-ONLY MODE ===\n\
                \Do not create, modify, or delete implementation files. Explore the codebase, \
                \consider trade-offs, and produce a concrete implementation strategy. End with \
                \a Critical Files for Implementation section listing 3-5 files."
            _ ->
                "\n\nStart broad and narrow down. Check multiple locations and naming conventions. \
                \Never create documentation files unless explicitly requested."

lookupOrCreateSubagentSession
    :: IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> GrokSubagentSpecs
    -> SubagentId
    -> IO SubagentSession
lookupOrCreateSubagentSession sessionsRef storeRootRef typesRef agentId = do
    sessions <- readIORef sessionsRef
    case Map.lookup agentId sessions of
        Just session -> pure session
        Nothing -> do
            mroot <- readIORef storeRootRef
            loaded <- case mroot of
                Just sessionDir -> loadSubagentState sessionDir agentId
                Nothing -> pure (Right Nothing)
            let (items, meta) = case loaded of
                    Right (Just (xs, m)) -> (xs, Just m)
                    _ -> ([], Nothing)
            transcript <- newIORef items
            contextTokens <- newIORef Nothing
            case meta >>= (.diskAgentType) of
                Just agentType ->
                    recordAgentSpec typesRef agentId GrokSubagentSpec
                        { agentType
                        , modelOverride = meta >>= (.diskAgentModel)
                        , reasoningEffortOverride =
                            meta >>= (.diskReasoningEffort)
                        }
                Nothing -> pure ()
            let session = SubagentSession
                    { subSessionTranscript = transcript
                    , subSessionContextTokens = contextTokens
                    }
            atomicModifyIORef' sessionsRef \m -> (Map.insert agentId session m, ())
            pure session
