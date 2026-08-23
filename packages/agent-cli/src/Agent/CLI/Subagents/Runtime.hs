-- | CLI-owned runtime state and provider adapters for child agents.
module Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(..)
    , SubagentSession(..)
    , SubagentStoreRoot
    , flushAllSubagentSnapshots
    , freshOpenAiBackend
    , lookupOrCreateSubagentSession
    , persistAndEvictSubagentSessionWithStatus
    , persistSubagentSnapshotWithStatus
    , prepareCollaborationSpawn
    , restoreAgentFromDisk
    , runCodexSubagent
    , runHttpSubagent
    , validatePersistedSubagentTarget
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
import Agent.CLI.Prompt
    ( defaultModelFor
    , sessionTempGuidance
    , systemPrompt
    , systemPromptForTools
    )
import Agent.CLI.Request (requestParams)
import Agent.CLI.Session (LegacySubagentTarget(..))
import Agent.CLI.SubagentStore
    ( SubagentDiskMeta(..)
    , forkSubagentTranscript
    , loadSubagentState
    , saveSubagentState
    )
import Agent.CLI.Tools (requireToolRegistry, schemasFromAppTools)
import Agent.Dialect
    ( ChildAgentProtocol(..)
    , Dialect
    , DialectId
    , codexDialect
    , dialectChildAgentProtocol
    , dialectForId
    , dialectId
    , dialectIdForModel
    , dialectSlug
    , providerSupportsDialect
    )
import Agent.InterAgentMessage
    ( InterAgentMessage
    , interAgentMessagePayload
    )
import Agent.Loop
    ( Backend(..)
    , LoopConfig(..)
    , LoopError(..)
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
import Agent.Provider (Provider(..), TokenProvider, providerSlug)
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
import Agent.Tools.Grok.Prompt
    ( codingGrokPromptTools
    , grokSubagentSystemPrompt
    )
import Agent.Tools.MultiAgents
    ( CollaborationSpawnOptions(..)
    , MultiAgentContext(..)
    )
import Agent.Tools.PlanMode (PlanModeEnv(..), PlanModeHooks)
import Agent.Tools.Types
    ( AppTool(..)
    , ToolEnv(..)
    , ToolRegistry
    , defaultToolEnv
    , setToolSessionTmp
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Exception.Safe (finally, throwIO)
import Control.Monad (unless, void, when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Environment (lookupEnv)
import qualified System.Info as SystemInfo

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef [ResponseItem])
    , subSessionContextTokens :: !(IORef (Maybe (Int, Int)))
    , subSessionProvider :: !Provider
    , subSessionEffectiveModel :: !Text
    , subSessionDialect :: !DialectId
    , subSessionPinned :: !(IORef Bool)
    , subSessionHydrated :: !(MVar Bool)
    }

-- | Optional on-disk root for child transcripts (@sessionDir/agents/<id>@).
type SubagentStoreRoot = IORef (Maybe OsPath)

-- | Provider-neutral dependencies shared by all child-agent backends.
data SubagentRuntime = SubagentRuntime
    { subagentOptions :: !CliOptions
    , subagentPolicy :: !ApprovalPolicy
    , subagentPlanHooks :: !PlanModeHooks
    , subagentSessionTmp :: !(IORef (Maybe OsPath))
    , subagentParams :: !(IORef ResponseCreateParams)
    , subagentRegistry :: !SubagentRegistry
    , subagentSessions :: !(IORef (Map SubagentId SubagentSession))
    , subagentStoreRoot :: !SubagentStoreRoot
    , subagentTypes :: !GrokSubagentSpecs
    , subagentLegacyTarget :: !(Maybe LegacySubagentTarget)
    , subagentMapModel :: !(Text -> Text)
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
    :: Provider
    -> (Text -> Text)
    -> Text
    -> DialectId
    -> Maybe LegacySubagentTarget
    -> IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> GrokSubagentSpecs
    -> IORef (Maybe (IORef [ResponseItem]))
    -> SubagentId
    -> CollaborationSpawnOptions
    -> IO ()
prepareCollaborationSpawn
        provider
        mapModel
        currentEffectiveModel
        currentDialect
        legacyTarget
        sessionsRef storeRootRef typesRef sourceRef agentId spawnOptions = do
    recordAgentSpec typesRef agentId GrokSubagentSpec
        { agentType = defaultSubagentType
        , modelOverride = spawnOptions.collaborationModel
        , reasoningEffortOverride =
            spawnOptions.collaborationReasoningEffort
        }
    let effectiveModel =
            maybe currentEffectiveModel mapModel spawnOptions.collaborationModel
        childDialect =
            maybe
                currentDialect
                (dialectIdForModel provider . mapModel)
                spawnOptions.collaborationModel
    session <-
        lookupOrCreateSubagentSession
            sessionsRef
            storeRootRef
            typesRef
            provider
            legacyTarget
            effectiveModel
            childDialect
            agentId
    source <- readIORef sourceRef
    sourceItems <- maybe (pure []) readIORef source
    writeIORef session.subSessionTranscript
        (forkSubagentTranscript spawnOptions.collaborationForkTurns sourceItems)

persistSubagentSnapshot
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentSession
    -> IO ()
persistSubagentSnapshot
        storeRootRef registry typesRef agentId session = do
    status <- getStatus registry agentId
    persistSubagentSnapshotWithStatus
        storeRootRef registry typesRef agentId status session

persistSubagentSnapshotWithStatus
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> SubagentSession
    -> IO ()
persistSubagentSnapshotWithStatus
        storeRootRef registry typesRef agentId status session = do
    mroot <- readIORef storeRootRef
    case mroot of
        Nothing -> pure ()
        Just sessionDir ->
            void $
                saveSubagentSnapshotWithStatus
                    sessionDir registry typesRef agentId status session

-- | Persist a final snapshot, then release its parsed transcript payload.
--
-- The stable 'SubagentSession' object stays installed so a concurrent follow-up
-- cannot split history across two session objects. A failed or disabled save
-- leaves the resident transcript untouched.
persistAndEvictSubagentSessionWithStatus
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> SubagentSession
    -> IO (Either Text Bool)
persistAndEvictSubagentSessionWithStatus
        storeRootRef registry typesRef agentId status session =
    modifyMVar session.subSessionHydrated \hydrated ->
        if not hydrated || not (evictableStatus status)
            then pure (hydrated, Right False)
            else readIORef storeRootRef >>= \case
                Nothing -> pure (True, Right False)
                Just sessionDir ->
                    saveSubagentSnapshotWithStatus
                        sessionDir registry typesRef agentId status session >>= \case
                            Left err -> pure (True, Left err)
                            Right () -> do
                                pinned <- readIORef session.subSessionPinned
                                if pinned
                                    then pure (True, Right False)
                                    else do
                                        writeIORef session.subSessionTranscript []
                                        writeIORef
                                            session.subSessionContextTokens
                                            Nothing
                                        pure (False, Right True)
  where
    evictableStatus = \case
        Completed{} -> True
        Errored{} -> True
        Interrupted -> True
        Closed -> True
        Pending -> False
        Running -> False
        NotFound -> False

saveSubagentSnapshotWithStatus
    :: OsPath
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> SubagentSession
    -> IO (Either Text ())
saveSubagentSnapshotWithStatus
        sessionDir registry typesRef agentId status session = do
    items <-
        trimDanglingToolSuffix
            <$> readIORef session.subSessionTranscript
    previous <- getPreviousResponseId registry agentId
    agentType <- lookupAgentType typesRef agentId
    agentModel <- lookupAgentModel typesRef agentId
    reasoningEffort <- lookupAgentReasoningEffort typesRef agentId
    agentCwd <- getSubagentCwd registry agentId
    identity <- getSubagentIdentity registry agentId
    saveSubagentState
        sessionDir agentId items previous status
        session.subSessionProvider session.subSessionEffectiveModel
        session.subSessionDialect
        agentType agentModel
        reasoningEffort agentCwd identity

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
            withMVar session.subSessionHydrated \hydrated ->
                when hydrated $
                    persistSubagentSnapshot
                        storeRootRef registry typesRef agentId session)
        (Map.toList sessions)

-- | Rehydrate a closed/missing agent from @sessionDir/agents/<id>@ so
-- 'resume_agent' / 'resume_from' can continue the prior transcript.
restoreAgentFromDisk
    :: Provider
    -> (Text -> Text)
    -> Text
    -> DialectId
    -> Maybe LegacySubagentTarget
    -> SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> SubagentId
    -> IO (Either Text ())
restoreAgentFromDisk
        provider mapModel parentEffectiveModel parentDialect legacyTarget
        storeRootRef registry sessionsRef typesRef agentId = do
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
                    Right Nothing -> do
                        session <-
                            getOrInstallSubagentSession
                                sessionsRef
                                provider
                                parentEffectiveModel
                                parentDialect
                                agentId
                        -- Same-process close with no disk yet: still reopen.
                        restoreSession session Nothing \_ ->
                            reopenInMemory Nothing Nothing
                    Right (Just (items, meta)) -> do
                        let expectedEffectiveModel =
                                maybe
                                    parentEffectiveModel
                                    mapModel
                                    meta.diskAgentModel
                            expectedDialect =
                                maybe
                                    parentDialect
                                    (dialectIdForModel provider . mapModel)
                                    meta.diskAgentModel
                        case validatePersistedSubagentTarget
                                provider
                                expectedEffectiveModel
                                expectedDialect
                                legacyTarget
                                meta of
                            Left err -> pure (Left err)
                            Right (_, storedDialect)
                                | not
                                    (providerSupportsDialect
                                        provider storedDialect) ->
                                    pure $ Left $
                                        unsupportedDialectMessage
                                            provider agentId storedDialect
                            Right (storedEffectiveModel, storedDialect) -> do
                                session <-
                                    getOrInstallSubagentSession
                                        sessionsRef
                                        provider
                                        storedEffectiveModel
                                        storedDialect
                                        agentId
                                restoreSession session (Just (items, meta)) \_ ->
                                    reopenPersisted meta >>= \case
                                        Left err -> pure (Left err)
                                        Right () -> pure (Right ())
    restoreSession session loaded reopen =
        modifyMVar session.subSessionHydrated \hydrated -> do
            currentStatus <- getStatus registry agentId
            case currentStatus of
                NotFound -> finish hydrated
                Closed -> finish hydrated
                _ -> do
                    hydrated' <-
                        ensureSubagentSessionHydratedLocked
                            storeRootRef typesRef legacyTarget
                            agentId session hydrated
                    pure (hydrated', Right ())
      where
        finish hydrated =
            reopen hydrated >>= \case
                Left err -> pure (hydrated, Left err)
                Right () -> do
                    case loaded of
                        Nothing -> pure ()
                        Just (items, meta) -> do
                            recordPersistedAgentSpec typesRef agentId meta
                            unless hydrated $
                                writeIORef session.subSessionTranscript items
                    pure (True, Right ())
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
        childModel <- lookupAgentModel runtime.subagentTypes env.subId
        childEffort <- lookupAgentReasoningEffort runtime.subagentTypes env.subId
        parentParams <- readIORef runtime.subagentParams
        let (model, effort) =
                resolveChildModelAndEffort
                    OpenAIProvider
                    parentParams
                    childModel
                    childEffort
        prepared <-
            prepareChild
                runtime
                OpenAIProvider
                model
                (dialectId codexDialect)
                env
                sendToRoot
        sessionTmp <- readIORef runtime.subagentSessionTmp
        case activeSubagentTargetError
                OpenAIProvider model prepared.preparedSession of
            Just err -> pure (Left (LoopUnexpected err))
            Nothing -> do
                coding <-
                    codingToolsFor
                        codexDialect
                        prepared.preparedToolEnv
                        (Just runtime.subagentPlanHooks)
                        (Just prepared.preparedMultiContext)
                syncStoreRootFromPlan
                    runtime.subagentStoreRoot
                    coding.codingPlanMode
                flip finally coding.codingClose do
                    today <- utctDay <$> getCurrentTime
                    let baseInstructions =
                            fromMaybe
                                (systemPrompt
                                    codexDialect
                                    env.subCwd
                                    sessionTmp
                                    today
                                    True)
                                prepared.preparedParentParams.instructions
                        instructions =
                            baseInstructions
                                <> "\n\nYou are a Codex subagent. Complete the assigned task and "
                                <> "report results clearly. Your agent id is "
                                <> env.subId.unSubagentId
                                <> "."
                        tools = coding.codingAppTools
                        childParams = requestParams model instructions
                            (schemasFromAppTools codexDialect tools) effort
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
                                    OpenAI.createCodexMessageWithProvider
                                        tokenProvider request)
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
                        runtime env prepared.preparedSession toolRegistry
                        backend onEvent
                        (\config ->
                            runLoopInputs config previous [AgentMessage prompt])

-- | Child XAI/OpenRouter agent: HTTP backend, filtered tools by subagent_type.
runHttpSubagent
    :: SubagentRuntime
    -> Dialect
    -> Provider
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> (IORef ResponseCreateParams -> IORef [ResponseItem] -> Backend)
    -> RunSubagent
runHttpSubagent runtime dialect provider sendToRoot mkBackend =
    \env previous prompt onEvent -> do
        agentType <-
            fromMaybe defaultSubagentType
                <$> lookupAgentType runtime.subagentTypes env.subId
        childModel <- lookupAgentModel runtime.subagentTypes env.subId
        childEffort <-
            lookupAgentReasoningEffort runtime.subagentTypes env.subId
        parentParams <- readIORef runtime.subagentParams
        let (model, effort) =
                resolveChildModelAndEffort
                    provider
                    parentParams
                    childModel
                    childEffort
            effectiveModel = runtime.subagentMapModel model
        prepared <-
            prepareChild
                runtime
                provider
                effectiveModel
                (maybe
                    (dialectId dialect)
                    (dialectIdForModel provider . runtime.subagentMapModel)
                    childModel)
                env
                sendToRoot
        sessionTmp <- readIORef runtime.subagentSessionTmp
        case activeSubagentTargetError
                provider effectiveModel prepared.preparedSession of
            Just err -> pure (Left (LoopUnexpected err))
            Nothing -> do
                let childDialect =
                        dialectForId
                            prepared.preparedSession.subSessionDialect
                coding <-
                    codingToolsFor
                        childDialect
                        prepared.preparedToolEnv
                        (Just runtime.subagentPlanHooks)
                        (Just prepared.preparedMultiContext)
                flip finally coding.codingClose do
                    today <- utctDay <$> getCurrentTime
                    shellPath <-
                        Text.pack . fromMaybe defaultShell <$> lookupEnv "SHELL"
                    let tools = case
                                dialectChildAgentProtocol childDialect of
                            CodexCollaborationProtocol ->
                                coding.codingAppTools
                            GrokTaskProtocol ->
                                filterChildGrokTools
                                    agentType coding.codingAppTools
                            GenericTaskProtocol ->
                                filterChildGrokTools
                                    agentType coding.codingAppTools
                            NoHostChildAgentProtocol ->
                                []
                        baseInstructions =
                            case dialectChildAgentProtocol childDialect of
                                CodexCollaborationProtocol ->
                                    systemPrompt
                                        childDialect
                                        env.subCwd
                                        sessionTmp
                                        today
                                        True
                                GrokTaskProtocol ->
                                    Text.intercalate "\n\n" $
                                        filter (not . Text.null)
                                            [ grokSubagentSystemPrompt
                                                codingGrokPromptTools
                                                ("web_search" : map (.appToolName) tools)
                                                env.subCwd
                                                today
                                                (Text.pack SystemInfo.os)
                                                shellPath
                                                agentType
                                                env.subId.unSubagentId
                                            , sessionTempGuidance sessionTmp
                                            ]
                                GenericTaskProtocol ->
                                    systemPromptForTools
                                        childDialect
                                        (map (.appToolName) tools)
                                        env.subCwd
                                        sessionTmp
                                        today
                                        True
                                NoHostChildAgentProtocol ->
                                    systemPrompt
                                        childDialect env.subCwd today True
                        instructions =
                            baseInstructions
                                <> "\n\n"
                                <> case
                                    dialectChildAgentProtocol childDialect of
                                    CodexCollaborationProtocol ->
                                        codexSubagentSuffix env.subId
                                    GrokTaskProtocol ->
                                        ""
                                    GenericTaskProtocol ->
                                        genericSubagentSuffix agentType env.subId
                                    NoHostChildAgentProtocol ->
                                        ""
                        childParams = requestParams model instructions
                            (schemasFromAppTools childDialect tools) effort
                    toolRegistry <- requireToolRegistry tools
                    childParamsRef <- newIORef childParams
                    let backend =
                            withConnectionRecovery $
                                mkBackend
                                    childParamsRef
                                    prepared.preparedSession.subSessionTranscript
                    runPreparedChild
                        runtime env prepared.preparedSession toolRegistry
                        backend onEvent
                        (\config ->
                            runLoop
                                config
                                previous
                                (interAgentMessagePayload prompt))

defaultShell :: String
defaultShell
    | SystemInfo.os == "mingw32" = "cmd.exe"
    | otherwise = "/bin/sh"

prepareChild
    :: SubagentRuntime
    -> Provider
    -> Text
    -> DialectId
    -> SubagentSpawnEnv
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> IO PreparedChild
prepareChild runtime provider currentEffectiveModel currentDialect env sendToRoot = do
    parentParams <- readIORef runtime.subagentParams
    childEnv <- defaultToolEnv env.subCwd
    sessionTmp <- readIORef runtime.subagentSessionTmp
    setToolSessionTmp childEnv sessionTmp
    childPath <-
        fromMaybe taskPathRoot
            <$> getTaskPath runtime.subagentRegistry env.subId
    session <-
        lookupOrCreateSubagentSession
            runtime.subagentSessions
            runtime.subagentStoreRoot
            runtime.subagentTypes
            provider
            runtime.subagentLegacyTarget
            currentEffectiveModel
            currentDialect
            env.subId
    nestedForkSource <- newIORef (Just session.subSessionTranscript)
    let sessionDialect = dialectForId session.subSessionDialect
        childToolEnv = childEnv { toolCancel = env.subCancel }
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
                    provider
                    runtime.subagentMapModel
                    session.subSessionEffectiveModel
                    session.subSessionDialect
                    (Just LegacySubagentTarget
                        { legacyTargetProvider =
                            session.subSessionProvider
                        , legacyTargetEffectiveModel =
                            session.subSessionEffectiveModel
                        , legacyTargetDialect =
                            session.subSessionDialect
                        })
                    runtime.subagentSessions
                    runtime.subagentStoreRoot
                    runtime.subagentTypes
                    nestedForkSource)
            , multiSendToRoot =
                case dialectChildAgentProtocol sessionDialect of
                    CodexCollaborationProtocol -> sendToRoot
                    GrokTaskProtocol -> Nothing
                    GenericTaskProtocol -> Nothing
                    NoHostChildAgentProtocol -> Nothing
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
        session
    pure result

codexSubagentSuffix :: SubagentId -> Text
codexSubagentSuffix agentId =
    "You are a Codex subagent. Complete the assigned task and report results clearly. \
    \Your agent id is "
        <> agentId.unSubagentId
        <> "."

genericSubagentSuffix :: Text -> SubagentId -> Text
genericSubagentSuffix agentType agentId =
    "You are a focused subagent delegated a specific task.\n\
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
    -> Provider
    -> Maybe LegacySubagentTarget
    -> Text
    -> DialectId
    -> SubagentId
    -> IO SubagentSession
lookupOrCreateSubagentSession
        sessionsRef storeRootRef typesRef provider legacyTarget
        currentEffectiveModel currentDialect agentId = do
    session <-
        getOrInstallSubagentSession
            sessionsRef provider currentEffectiveModel currentDialect agentId
    modifyMVar_ session.subSessionHydrated $
        ensureSubagentSessionHydratedLocked
            storeRootRef typesRef legacyTarget agentId session
    pure session

getOrInstallSubagentSession
    :: IORef (Map SubagentId SubagentSession)
    -> Provider
    -> Text
    -> DialectId
    -> SubagentId
    -> IO SubagentSession
getOrInstallSubagentSession
        sessionsRef provider effectiveModel dialect agentId = do
    transcript <- newIORef []
    contextTokens <- newIORef Nothing
    pinned <- newIORef False
    hydrated <- newMVar False
    let candidate = SubagentSession
            { subSessionTranscript = transcript
            , subSessionContextTokens = contextTokens
            , subSessionProvider = provider
            , subSessionEffectiveModel = effectiveModel
            , subSessionDialect = dialect
            , subSessionPinned = pinned
            , subSessionHydrated = hydrated
            }
    atomicModifyIORef' sessionsRef \sessions ->
        case Map.lookup agentId sessions of
            Just existing -> (sessions, existing)
            Nothing -> (Map.insert agentId candidate sessions, candidate)

ensureSubagentSessionHydratedLocked
    :: SubagentStoreRoot
    -> GrokSubagentSpecs
    -> Maybe LegacySubagentTarget
    -> SubagentId
    -> SubagentSession
    -> Bool
    -> IO Bool
ensureSubagentSessionHydratedLocked
        storeRootRef typesRef legacyTarget agentId session hydrated
    | hydrated = pure True
    | otherwise = do
        mroot <- readIORef storeRootRef
        loaded <- case mroot of
            Just sessionDir -> loadSubagentState sessionDir agentId
            Nothing -> pure (Right Nothing)
        case loaded of
            Left err ->
                throwIO (userError (Text.unpack err))
            Right Nothing ->
                pure ()
            Right (Just (items, meta)) ->
                case validatePersistedSubagentTarget
                        session.subSessionProvider
                        session.subSessionEffectiveModel
                        session.subSessionDialect
                        legacyTarget
                        meta of
                    Left err ->
                        throwIO (userError (Text.unpack err))
                    Right (_, storedDialect)
                        | not
                            (providerSupportsDialect
                                session.subSessionProvider storedDialect) ->
                            throwIO $
                                userError $
                                    Text.unpack $
                                        unsupportedDialectMessage
                                            session.subSessionProvider
                                            agentId
                                            storedDialect
                    Right _ -> do
                        writeIORef session.subSessionTranscript items
                        writeIORef session.subSessionContextTokens Nothing
                        recordPersistedAgentSpec typesRef agentId meta
        pure True

recordPersistedAgentSpec
    :: GrokSubagentSpecs
    -> SubagentId
    -> SubagentDiskMeta
    -> IO ()
recordPersistedAgentSpec typesRef agentId meta =
    case meta.diskAgentType of
        Just agentType ->
            recordAgentSpec typesRef agentId GrokSubagentSpec
                { agentType
                , modelOverride = meta.diskAgentModel
                , reasoningEffortOverride = meta.diskReasoningEffort
                }
        Nothing -> pure ()

validatePersistedSubagentTarget
    :: Provider
    -> Text
    -> DialectId
    -> Maybe LegacySubagentTarget
    -> SubagentDiskMeta
    -> Either Text (Text, DialectId)
validatePersistedSubagentTarget
        provider expectedEffectiveModel expectedDialect legacyTarget meta = do
    let legacyDialect =
            legacyDialectForTarget
                legacyTarget
                provider
                expectedEffectiveModel
                expectedDialect
    storedProvider <- case meta.diskProvider of
        Just storedProvider -> Right storedProvider
        Nothing
            | Nothing <- legacyDialect ->
                Left
                    "cannot restore a legacy subagent without provider metadata \
                    \after changing the session target; reopen the parent \
                    \session under its original target first"
        Nothing -> Right provider
    storedEffectiveModel <- case meta.diskEffectiveModel of
        Just stored -> Right stored
        Nothing
            | Just _ <- legacyDialect -> Right expectedEffectiveModel
            | otherwise ->
                Left
                    "cannot restore a legacy subagent without effective model \
                    \metadata after changing the session target; reopen the \
                    \parent session under its original target first"
    case subagentTargetError
            provider expectedEffectiveModel storedProvider storedEffectiveModel of
        Just err -> Left err
        Nothing -> Right ()
    storedDialect <- case meta.diskDialect of
        Just stored -> Right stored
        Nothing -> case legacyDialect of
            Just legacy -> Right legacy
            Nothing ->
                Left
                    "cannot restore a legacy subagent without dialect metadata \
                    \after changing the session target; reopen the parent \
                    \session under its original target first"
    Right (storedEffectiveModel, storedDialect)

legacyDialectForTarget
    :: Maybe LegacySubagentTarget
    -> Provider
    -> Text
    -> DialectId
    -> Maybe DialectId
legacyDialectForTarget target provider effectiveModel dialect = do
    legacy <- target
    if legacy.legacyTargetProvider == provider
        && legacy.legacyTargetEffectiveModel == effectiveModel
        && legacy.legacyTargetDialect == dialect
        then Just dialect
        else Nothing

activeSubagentTargetError
    :: Provider
    -> Text
    -> SubagentSession
    -> Maybe Text
activeSubagentTargetError provider effectiveModel session =
    subagentTargetError
        provider
        effectiveModel
        session.subSessionProvider
        session.subSessionEffectiveModel

subagentTargetError
    :: Provider
    -> Text
    -> Provider
    -> Text
    -> Maybe Text
subagentTargetError provider effectiveModel storedProvider storedEffectiveModel
    | storedProvider /= provider =
        Just
            ( "cannot continue subagent created for the "
                <> providerSlug storedProvider
                <> " transport under "
                <> providerSlug provider
            )
    | storedEffectiveModel /= effectiveModel =
        Just
            ( "cannot continue subagent after its effective model changed \
                \from "
                <> storedEffectiveModel
                <> " to "
                <> effectiveModel
            )
    | otherwise = Nothing

unsupportedDialectMessage :: Provider -> SubagentId -> DialectId -> Text
unsupportedDialectMessage provider agentId storedDialect =
    "cannot restore subagent "
        <> agentId.unSubagentId
        <> ": dialect "
        <> dialectSlug storedDialect
        <> " is not supported by the current "
        <> providerSlug provider
        <> " transport"
