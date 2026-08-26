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
    , resolveChildModelAndEffort
    , runCodexSubagent
    , runHttpSubagent
    , runXaiParentSubagent
    , grokSpawnedChildIdentity
    , usesOpenAiChildTransport
    , validatePersistedSubagentTarget
    ) where

import Agent.CLI.Approval (childApprove)
import Agent.CLI.Btw (trimDanglingToolSuffix)
import Agent.CLI.Compaction
    ( OccupancySnapshot
    , autoCompactOpenAiBackendWithSender
    )
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.CLI.Options
    ( ApprovalPolicy
    , CliOptions(..)
    , defaultEffortFor
    )
import Agent.CLI.Prompt
    ( sessionTempGuidance
    , systemPrompt
    , systemPromptForTools
    )
import Agent.CLI.Request (requestParams)
import Agent.CLI.Session (LegacySubagentTarget(..))
import Agent.CLI.SubagentStore
    ( LegacySubagentTargetFields(..)
    , SubagentDiskFields(..)
    , SubagentDiskMeta(..)
    , SubagentStateSnapshot(..)
    , SubagentTarget(..)
    , forkSubagentTranscript
    , loadSubagentState
    , saveSubagentState
    , subagentDiskFields
    )
import Agent.CLI.Tools
    ( hostedSearchToolNames
    , requireToolRegistry
    , schemasFromAppTools
    )
import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , filterBashTools
    , filterChildGrokTools
    , filterGhciTools
    )
import Agent.Codex.Dialect.Subagent (codexSubagentSuffix)
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
    , BackendStateStore(..)
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
    ( openAiBackendWithRawReasoning
    , openAiBackendWithReasoningVisibility
    , openAiBackendWithTransportFallback
    , withCodexTurnStateScope
    )
import Agent.OpenAI.WebSocketClient
    ( CodexTurnState
    , newCodexTurnState
    , sendWsRequestWithEvents
    , withCodexWsRetrying
    , withCodexWsRetryingUsingTurnState
    )
import System.OsPath (OsPath)
import Agent.Provider (Provider(..), TokenProvider, providerSlug)
import Agent.Responses.LoopBackend
    ( statelessResponsesBackendWithRawReasoning
    )
import Agent.Responses.Types
    ( ReasoningConfig(..)
    , ResponseCreateParams(..)
    , ResponseItem
    )
import Agent.Subagents
    ( RunSubagent
    , SubagentId(..)
    , SubagentIdentity(..)
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
import Agent.GrokBuild.Dialect.Prompt
    ( codingGrokPromptTools
    , grokSubagentSystemPrompt
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , canonicalizeGrokChildModel
    , defaultSubagentType
    , isLunaSubagentModel
    , lookupAgentModel
    , lookupAgentReasoningEffort
    , lookupAgentType
    , lunaSubagentModel
    , recordAgentSpec
    )
import Agent.Tools.MultiAgents
    ( CollaborationSpawnOptions(..)
    , MultiAgentContext(..)
    , SubagentWorktree
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
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Environment (lookupEnv)
import qualified System.Info as SystemInfo

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef [ResponseItem])
    , subSessionContextTokens :: !(IORef (Maybe OccupancySnapshot))
    , subSessionProvider :: !Provider
    , subSessionConnection :: !Text
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
    , subagentGhciEnabled :: !(IORef Bool)
    , subagentBashEnabled :: !(IORef Bool)
    , subagentPolicy :: !ApprovalPolicy
    , subagentPlanHooks :: !PlanModeHooks
    , subagentSkillRoots :: !(IORef [OsPath])
    , subagentSessionTmp :: !(IORef (Maybe OsPath))
    , subagentMcpTools :: ![AppTool]
    , subagentParams :: !(IORef ResponseCreateParams)
    , subagentRegistry :: !SubagentRegistry
    , subagentSessions :: !(IORef (Map SubagentId SubagentSession))
    , subagentStoreRoot :: !SubagentStoreRoot
    , subagentTypes :: !GrokSubagentSpecs
    , subagentLegacyTarget :: !(Maybe LegacySubagentTarget)
    , subagentConnection :: !Text
    , subagentMapModel :: !(Text -> Text)
    , subagentCreateWorktree
        :: !(Maybe (OsPath -> IO (Either Text SubagentWorktree)))
    , subagentSpawnModelGuidance :: !(Maybe Text)
    , subagentAllowedChildModels :: !(Maybe [Text])
    , subagentOpenAiChild :: !(Maybe TokenProvider)
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
    -> Text
    -> (Text -> Text)
    -> Text
    -> DialectId
    -> Maybe LegacySubagentTarget
    -> IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> GrokSubagentSpecs
    -> IORef (Maybe (IO [ResponseItem]))
    -> SubagentId
    -> CollaborationSpawnOptions
    -> IO ()
prepareCollaborationSpawn
        provider
        connection
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
            connection
            legacyTarget
            effectiveModel
            childDialect
            agentId
    source <- readIORef sourceRef
    sourceItems <- maybe (pure []) id source
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
        sessionDir
        agentId
        SubagentStateSnapshot
            { snapshotItems = items
            , snapshotPreviousResponseId = previous
            , snapshotStatus = status
            , snapshotTarget = SubagentTarget
                { targetProvider = session.subSessionProvider
                , targetConnection = session.subSessionConnection
                , targetEffectiveModel = session.subSessionEffectiveModel
                , targetDialect = session.subSessionDialect
                }
            , snapshotAgentType = agentType
            , snapshotAgentModel = agentModel
            , snapshotReasoningEffort = reasoningEffort
            , snapshotCwd = agentCwd
            , snapshotIdentity = identity
            }

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
    -> Text
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
        provider connection mapModel parentEffectiveModel parentDialect legacyTarget
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
                                connection
                                parentEffectiveModel
                                parentDialect
                                agentId
                        -- Same-process close with no disk yet: still reopen.
                        restoreSession session Nothing \_ ->
                            reopenInMemory Nothing Nothing
                    Right (Just (items, meta)) -> do
                        let fields = subagentDiskFields meta
                            derivedIdentity =
                                grokSpawnedChildIdentity
                                    provider
                                    connection
                                    mapModel
                                    parentEffectiveModel
                                    parentDialect
                                    fields.diskAgentModel
                            ( expectedProvider
                                , expectedConnection
                                , expectedEffectiveModel
                                , expectedDialect
                                ) =
                                    if provider == XAIProvider
                                        then case meta of
                                            CurrentSubagentDiskMeta _ stored
                                                | stored.targetProvider
                                                    == OpenAIProvider ->
                                                    ( stored.targetProvider
                                                    , stored.targetConnection
                                                    , stored.targetEffectiveModel
                                                    , stored.targetDialect
                                                    )
                                            _ ->
                                                derivedIdentity
                                        else
                                            ( provider
                                            , connection
                                            , maybe
                                                parentEffectiveModel
                                                mapModel
                                                fields.diskAgentModel
                                            , maybe
                                                parentDialect
                                                (dialectIdForModel provider
                                                    . mapModel)
                                                fields.diskAgentModel
                                            )
                        case validatePersistedSubagentTarget
                                expectedProvider
                                expectedConnection
                                expectedEffectiveModel
                                expectedDialect
                                legacyTarget
                                meta of
                            Left err -> pure (Left err)
                            Right storedTarget
                                | not
                                    (providerSupportsDialect
                                        storedTarget.targetProvider
                                        storedTarget.targetDialect) ->
                                    pure $ Left $
                                        unsupportedDialectMessage
                                            storedTarget.targetProvider
                                            agentId
                                            storedTarget.targetDialect
                            Right storedTarget -> do
                                session <-
                                    getOrInstallSubagentSession
                                        sessionsRef
                                        storedTarget.targetProvider
                                        storedTarget.targetConnection
                                        storedTarget.targetEffectiveModel
                                        storedTarget.targetDialect
                                        agentId
                                restoreSession session (Just (items, fields)) \_ ->
                                    reopenPersisted fields >>= \case
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
                        Just (items, fields) -> do
                            recordPersistedAgentSpec typesRef agentId fields
                            unless hydrated $
                                writeIORef session.subSessionTranscript items
                    pure (True, Right ())
    reopenPersisted fields =
        case fields.diskTaskPath of
            Nothing -> restoreAt taskPathRoot
            Just pathText ->
                case parseTaskPath pathText of
                    Left err -> pure (Left err)
                    Right taskPath -> restoreAt taskPath
      where
        restoredStatus = fromMaybe (Completed Nothing) fields.diskStatus
        restoreAt taskPath = do
            let restore =
                    case fields.diskCwd of
                        Just childCwd ->
                            restoreSubagentAtWithCwdStatus
                                registry childCwd
                        Nothing ->
                            restoreSubagentAtStatus registry
            restore
                agentId fields.diskParentId taskPath
                (fromMaybe 1 fields.diskDepth)
                Nothing fields.diskPreviousResponseId restoredStatus
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
    :: Bool
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
freshOpenAiBackend showRawReasoning provider getParams =
    Backend \state previous inputs onEvent ->
        withCodexWsRetrying provider \conn _credential ->
            let Backend submit =
                    openAiBackendWithRawReasoning
                        showRawReasoning
                        conn
                        getParams
            in submit state previous inputs onEvent

-- | Cancellation-safe Codex backend whose disposable sockets all participate
-- in one logical turn's sticky-routing scope.
freshOpenAiBackendWithTurnState
    :: Bool
    -> CodexTurnState
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
freshOpenAiBackendWithTurnState showRawReasoning turnState provider getParams =
    Backend \state previous inputs onEvent ->
        withCodexWsRetryingUsingTurnState provider turnState
            \conn _credential ->
                let Backend submit =
                        openAiBackendWithReasoningVisibility
                            showRawReasoning
                            (\request previousResponseId onStreamEvent ->
                                sendWsRequestWithEvents
                                    conn
                                    request
                                    previousResponseId
                                    onStreamEvent)
                            getParams
                in submit state previous inputs onEvent

-- | Identity for a Grok-root child, including OpenAI Luna when requested.
grokSpawnedChildIdentity
    :: Provider
    -> Text
    -> (Text -> Text)
    -> Text
    -> DialectId
    -> Maybe Text
    -> (Provider, Text, Text, DialectId)
grokSpawnedChildIdentity
        parentProvider parentConnection mapModel parentModel parentDialect childModel =
    case childModel >>= canonicalizeGrokChildModel of
        Just model
            | isLunaSubagentModel model ->
                ( OpenAIProvider
                , providerSlug OpenAIProvider
                , lunaSubagentModel
                , dialectId codexDialect
                )
            | otherwise ->
                (parentProvider, parentConnection, model, parentDialect)
        Nothing ->
            ( parentProvider
            , parentConnection
            , maybe parentModel mapModel childModel
            , maybe
                parentDialect
                (dialectIdForModel parentProvider . mapModel)
                childModel
            )

inheritedGrokChildModel :: SubagentRuntime -> Text -> Text
inheritedGrokChildModel runtime parentModel =
    case runtime.subagentAllowedChildModels of
        Nothing -> parentModel
        Just allowed ->
            case canonicalizeGrokChildModel parentModel of
                Just slug | slug `elem` allowed -> slug
                _ -> fromMaybe parentModel (listToMaybe allowed)

-- | Luna requests and already-OpenAI descendants stay on Codex/OpenAI.
-- An omitted or non-Luna override from a Luna child must not fall through
-- to the xAI HTTP runner.
usesOpenAiChildTransport
    :: Maybe Provider
    -> Maybe Provider
    -> Maybe Text
    -> Bool
usesOpenAiChildTransport childSessionProvider parentSessionProvider childModel =
    childSessionProvider == Just OpenAIProvider
        || parentSessionProvider == Just OpenAIProvider
        || maybe False isLunaSubagentModel
            (childModel >>= canonicalizeGrokChildModel)

-- | XAI parent runner: Grok children stay on xAI; Luna and its descendants
-- use Codex/OpenAI.
runXaiParentSubagent
    :: SubagentRuntime
    -> Dialect
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> (ResponseCreateParams -> Backend)
    -> RunSubagent
runXaiParentSubagent runtime dialect sendToRoot mkBackend =
    \env previous prompt onEvent -> do
        childModel <- lookupAgentModel runtime.subagentTypes env.subId
        sessions <- readIORef runtime.subagentSessions
        identity <- getSubagentIdentity runtime.subagentRegistry env.subId
        let childSessionProvider =
                (.subSessionProvider) <$> Map.lookup env.subId sessions
            parentSessionProvider = do
                ident <- identity
                parentId <- ident.identityParent
                session <- Map.lookup parentId sessions
                pure session.subSessionProvider
        if usesOpenAiChildTransport
                childSessionProvider parentSessionProvider childModel
            then case runtime.subagentOpenAiChild of
                Nothing ->
                    pure $ Left $ LoopUnexpected $
                        "OpenAI is not signed in; cannot run this subagent on "
                            <> lunaSubagentModel
                            <> "."
                Just openaiToken ->
                    runCodexSubagent
                        runtime
                            { subagentConnection =
                                providerSlug OpenAIProvider
                            , subagentMapModel = id
                            }
                        openaiToken
                        sendToRoot
                        env
                        previous
                        prompt
                        onEvent
            else
                runHttpSubagent
                    runtime
                    dialect
                    XAIProvider
                    sendToRoot
                    mkBackend
                    env
                    previous
                    prompt
                    onEvent

-- | Child Codex agent: per-agent transcript retained across follow-ups,
-- independently scoped WebSocket requests, and nested multi-agent tools.
runCodexSubagent
    :: SubagentRuntime
    -> TokenProvider
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> RunSubagent
runCodexSubagent runtime tokenProvider sendToRoot =
    \env previous prompt onEvent -> do
        agentType <-
            fromMaybe defaultSubagentType
                <$> lookupAgentType runtime.subagentTypes env.subId
        childModel <- lookupAgentModel runtime.subagentTypes env.subId
        childEffort <- lookupAgentReasoningEffort runtime.subagentTypes env.subId
        parentParams <- readIORef runtime.subagentParams
        let (provisionalModel, _) =
                resolveChildModelAndEffort
                    OpenAIProvider
                    parentParams
                    (fromMaybe "" parentParams.model)
                    childModel
                    childEffort
        prepared <-
            prepareChild
                runtime
                OpenAIProvider
                provisionalModel
                (dialectId codexDialect)
                env
                sendToRoot
        let (model, effort) =
                resolveChildModelAndEffort
                    OpenAIProvider
                    parentParams
                    prepared.preparedSession.subSessionEffectiveModel
                    childModel
                    childEffort
        sessionTmp <- readIORef runtime.subagentSessionTmp
        case activeSubagentTargetError
                OpenAIProvider runtime.subagentConnection
                model prepared.preparedSession of
            Just err -> pure (Left (LoopUnexpected err))
            Nothing -> do
                coding <-
                    codingToolsFor
                        codexDialect
                        prepared.preparedToolEnv
                        (Just runtime.subagentPlanHooks)
                        Nothing
                        (Just prepared.preparedMultiContext)
                syncStoreRootFromPlan
                    runtime.subagentStoreRoot
                    coding.codingPlanMode
                flip finally coding.codingClose do
                    today <- utctDay <$> getCurrentTime
                    ghciEnabled <- readIORef runtime.subagentGhciEnabled
                    bashEnabled <- readIORef runtime.subagentBashEnabled
                    let codingTools =
                            filterGhciTools ghciEnabled $
                                filterBashTools bashEnabled $
                                    filterChildGrokTools
                                        agentType coding.codingAppTools
                        tools =
                            codingTools <> runtime.subagentMcpTools
                        generatedInstructions =
                            systemPromptForTools
                                codexDialect
                                (map (.appToolName) tools)
                                env.subCwd
                                sessionTmp
                                today
                                True
                        inheritParentPrompt =
                            case prepared.preparedParentParams.model of
                                Just parentModel ->
                                    not
                                        ( "grok"
                                            `Text.isPrefixOf`
                                                Text.toLower parentModel
                                        )
                                Nothing -> True
                        baseInstructions =
                            if inheritParentPrompt
                                then
                                    fromMaybe
                                        generatedInstructions
                                        prepared.preparedParentParams.instructions
                                else generatedInstructions
                        instructions =
                            baseInstructions
                                <> "\n\nYou are a Codex subagent. Complete the assigned task and "
                                <> "report results clearly. Your agent id is "
                                <> env.subId.unSubagentId
                                <> "."
                                <> case agentType of
                                    "explore" ->
                                        " Operate read-only: search and report, do not edit files."
                                    "plan" ->
                                        " Produce an implementation plan; do not edit implementation files."
                                    _ ->
                                        ""
                        childParams = requestParams OpenAIProvider model instructions
                            (schemasFromAppTools codexDialect tools) effort
                    toolRegistry <- requireToolRegistry tools
                    httpFallbackActive <- newIORef False
                    turnState <- newCodexTurnState
                    let websocketBackend =
                            freshOpenAiBackendWithTurnState
                                runtime.subagentOptions.optShowRawReasoning
                                turnState
                                tokenProvider
                                (pure childParams)
                        httpBackend =
                            statelessResponsesBackendWithRawReasoning
                                runtime.subagentOptions.optShowRawReasoning
                                (\request _onEvent ->
                                    OpenAI.createCodexMessageWithProviderWithTurnState
                                        turnState tokenProvider request)
                                (pure childParams)
                        baseBackend =
                            openAiBackendWithTransportFallback
                                httpFallbackActive
                                websocketBackend
                                httpBackend
                        compactSender request =
                            OpenAI.createCodexMessageWithProviderWithOptionsAndTurnState
                                OpenAI.remoteCompactionV2RequestOptions
                                turnState
                                tokenProvider
                                request
                        compactingBackend =
                            autoCompactOpenAiBackendWithSender
                                runtime.subagentOptions.optCompactThreshold
                                compactSender
                                (const (pure ()))
                                (pure childParams)
                                prepared.preparedSession.subSessionContextTokens
                                baseBackend
                        backend =
                            withCodexTurnStateScope (pure turnState) $
                                withConnectionRecovery compactingBackend
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
    -> (ResponseCreateParams -> Backend)
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
        let inheritedParentModel =
                inheritedGrokChildModel
                    runtime
                    (fromMaybe "" parentParams.model)
            (provisionalModel, _) =
                resolveChildModelAndEffort
                    provider
                    parentParams
                    inheritedParentModel
                    childModel
                    childEffort
            provisionalEffectiveModel =
                runtime.subagentMapModel provisionalModel
        prepared <-
            prepareChild
                runtime
                provider
                provisionalEffectiveModel
                (maybe
                    (dialectId dialect)
                    (dialectIdForModel provider . runtime.subagentMapModel)
                    childModel)
                env
                sendToRoot
        let (model, effort) =
                resolveChildModelAndEffort
                    provider
                    parentParams
                    prepared.preparedSession.subSessionEffectiveModel
                    childModel
                    childEffort
            effectiveModel =
                maybe
                    prepared.preparedSession.subSessionEffectiveModel
                    runtime.subagentMapModel
                    childModel
        sessionTmp <- readIORef runtime.subagentSessionTmp
        case activeSubagentTargetError
                provider runtime.subagentConnection
                effectiveModel prepared.preparedSession of
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
                        Nothing
                        (Just prepared.preparedMultiContext)
                flip finally coding.codingClose do
                    today <- utctDay <$> getCurrentTime
                    shellPath <-
                        Text.pack . fromMaybe defaultShell <$> lookupEnv "SHELL"
                    let childTools = case
                                dialectChildAgentProtocol childDialect of
                            CodexCollaborationProtocol -> coding.codingAppTools
                            GrokTaskProtocol ->
                                filterChildGrokTools agentType coding.codingAppTools
                            GenericTaskProtocol ->
                                filterChildGrokTools
                                    agentType coding.codingAppTools
                            NoHostChildAgentProtocol ->
                                []
                    ghciEnabled <- readIORef runtime.subagentGhciEnabled
                    bashEnabled <- readIORef runtime.subagentBashEnabled
                    let codingTools =
                            filterGhciTools ghciEnabled $
                                filterBashTools bashEnabled childTools
                        tools =
                            codingTools <> runtime.subagentMcpTools
                        baseInstructions =
                            case dialectChildAgentProtocol childDialect of
                                CodexCollaborationProtocol ->
                                    systemPromptForTools
                                        childDialect
                                        (map (.appToolName) tools)
                                        env.subCwd
                                        sessionTmp
                                        today
                                        True
                                GrokTaskProtocol ->
                                    Text.intercalate "\n\n" $
                                        filter (not . Text.null)
                                            [ grokSubagentSystemPrompt
                                                codingGrokPromptTools
                                                (hostedSearchToolNames childDialect
                                                    ++ map (.appToolName) tools)
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
                                        childDialect
                                        env.subCwd
                                        sessionTmp
                                        today
                                        True
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
                        childParams = requestParams provider model instructions
                            (schemasFromAppTools childDialect tools) effort
                    toolRegistry <- requireToolRegistry tools
                    let backend =
                            withConnectionRecovery $
                                mkBackend childParams
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
    skillRoots <- readIORef runtime.subagentSkillRoots
    writeIORef childEnv.toolSkillRoots skillRoots
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
            runtime.subagentConnection
            runtime.subagentLegacyTarget
            currentEffectiveModel
            currentDialect
            env.subId
    nestedForkSource <- newIORef (Just (readIORef session.subSessionTranscript))
    let sessionDialect = dialectForId session.subSessionDialect
        childToolEnv = childEnv { toolCancel = env.subCancel }
        childCtx = MultiAgentContext
            { multiRegistry = runtime.subagentRegistry
            , multiCwd = env.subCwd
            , multiSelfId = Just env.subId
            , multiDepth = env.subDepth
            , multiTaskPath = childPath
            , multiRootTurnId = pure env.subRootTurnId
            , multiResumeFromDisk = Nothing
            , multiCreateWorktree = runtime.subagentCreateWorktree
            , multiPrepareSpawn = Just
                (prepareCollaborationSpawn
                    provider
                    session.subSessionConnection
                    runtime.subagentMapModel
                    session.subSessionEffectiveModel
                    session.subSessionDialect
                    (Just LegacySubagentTarget
                        { legacyTargetProvider =
                            session.subSessionProvider
                        , legacyTargetConnection =
                            session.subSessionConnection
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
            , multiSpawnModelGuidance = runtime.subagentSpawnModelGuidance
            , multiAllowedChildModels = runtime.subagentAllowedChildModels
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
    -> Text
    -> Maybe Text
    -> Maybe Text
    -> (Text, Text)
resolveChildModelAndEffort
        provider parentParams inheritedModel childModel childEffort =
    ( model
    , fromMaybe defaultChildEffort childEffort
    )
  where
    model = fromMaybe inheritedModel childModel
    inheritedEffort = case parentParams.reasoning of
        Just cfg -> fromMaybe (defaultEffortFor provider) cfg.effort
        Nothing -> defaultEffortFor provider
    defaultChildEffort
        | provider == OpenAIProvider
        , model == "gpt-5.6-luna"
        , inheritedEffort `notElem` ["xhigh", "max"] = "high"
        | otherwise = inheritedEffort

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
            , loopBackendState = BackendStateStore
                { readBackendState = readIORef session.subSessionTranscript
                , commitBackendState = writeIORef session.subSessionTranscript
                }
            , loopTools = toolRegistry
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = runtime.subagentOptions.optMaxTurns
            , loopOnEvent = onEvent
            , loopApprove =
                \call ->
                    childApprove runtime.subagentPolicy toolRegistry call
            , loopReadSteering = pure []
            , loopCommitSteering = \_ -> pure ()
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
    -> Text
    -> Maybe LegacySubagentTarget
    -> Text
    -> DialectId
    -> SubagentId
    -> IO SubagentSession
lookupOrCreateSubagentSession
        sessionsRef storeRootRef typesRef provider connection legacyTarget
        currentEffectiveModel currentDialect agentId = do
    session <-
        getOrInstallSubagentSession
            sessionsRef provider connection currentEffectiveModel currentDialect agentId
    modifyMVar_ session.subSessionHydrated $
        ensureSubagentSessionHydratedLocked
            storeRootRef typesRef legacyTarget agentId session
    pure session

getOrInstallSubagentSession
    :: IORef (Map SubagentId SubagentSession)
    -> Provider
    -> Text
    -> Text
    -> DialectId
    -> SubagentId
    -> IO SubagentSession
getOrInstallSubagentSession
        sessionsRef provider connection effectiveModel dialect agentId = do
    transcript <- newIORef []
    contextTokens <- newIORef Nothing
    pinned <- newIORef False
    hydrated <- newMVar False
    let candidate = SubagentSession
            { subSessionTranscript = transcript
            , subSessionContextTokens = contextTokens
            , subSessionProvider = provider
            , subSessionConnection = connection
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
                        session.subSessionConnection
                        session.subSessionEffectiveModel
                        session.subSessionDialect
                        legacyTarget
                        meta of
                    Left err ->
                        throwIO (userError (Text.unpack err))
                    Right storedTarget
                        | not
                            (providerSupportsDialect
                                session.subSessionProvider
                                storedTarget.targetDialect) ->
                            throwIO $
                                userError $
                                    Text.unpack $
                                        unsupportedDialectMessage
                                            session.subSessionProvider
                                            agentId
                                            storedTarget.targetDialect
                    Right _ -> do
                        writeIORef session.subSessionTranscript items
                        writeIORef session.subSessionContextTokens Nothing
                        recordPersistedAgentSpec
                            typesRef
                            agentId
                            (subagentDiskFields meta)
        pure True

recordPersistedAgentSpec
    :: GrokSubagentSpecs
    -> SubagentId
    -> SubagentDiskFields
    -> IO ()
recordPersistedAgentSpec typesRef agentId fields =
    case fields.diskAgentType of
        Just agentType ->
            recordAgentSpec typesRef agentId GrokSubagentSpec
                { agentType
                , modelOverride = fields.diskAgentModel
                , reasoningEffortOverride = fields.diskReasoningEffort
                }
        Nothing -> pure ()

validatePersistedSubagentTarget
    :: Provider
    -> Text
    -> Text
    -> DialectId
    -> Maybe LegacySubagentTarget
    -> SubagentDiskMeta
    -> Either Text SubagentTarget
validatePersistedSubagentTarget
        provider connection expectedEffectiveModel expectedDialect legacyTarget meta = do
    let expectedTarget = SubagentTarget
            { targetProvider = provider
            , targetConnection = connection
            , targetEffectiveModel = expectedEffectiveModel
            , targetDialect = expectedDialect
            }
    storedTarget <- normalizePersistedSubagentTarget
        legacyTarget
        expectedTarget
        meta
    case subagentTargetError expectedTarget storedTarget of
        Just err -> Left err
        Nothing -> Right ()
    Right storedTarget

normalizePersistedSubagentTarget
    :: Maybe LegacySubagentTarget
    -> SubagentTarget
    -> SubagentDiskMeta
    -> Either Text SubagentTarget
normalizePersistedSubagentTarget legacyTarget expectedTarget = \case
    CurrentSubagentDiskMeta _ storedTarget ->
        Right storedTarget
    LegacySubagentDiskMeta _ legacyFields -> do
        legacyDialect <- case
                legacyDialectForTarget legacyTarget expectedTarget
                of
            Just dialect -> Right dialect
            Nothing ->
                Left
                    "cannot restore a legacy subagent with incomplete target \
                    \metadata after changing the session target; reopen the \
                    \parent session under its original target first"
        Right SubagentTarget
            { targetProvider =
                fromMaybe
                    expectedTarget.targetProvider
                    legacyFields.legacyDiskProvider
            , targetConnection =
                fromMaybe
                    expectedTarget.targetConnection
                    legacyFields.legacyDiskConnection
            , targetEffectiveModel =
                fromMaybe
                    expectedTarget.targetEffectiveModel
                    legacyFields.legacyDiskEffectiveModel
            , targetDialect =
                fromMaybe legacyDialect legacyFields.legacyDiskDialect
            }

legacyDialectForTarget
    :: Maybe LegacySubagentTarget
    -> SubagentTarget
    -> Maybe DialectId
legacyDialectForTarget target expectedTarget = do
    legacy <- target
    if legacy.legacyTargetProvider == expectedTarget.targetProvider
        && legacy.legacyTargetConnection == expectedTarget.targetConnection
        && legacy.legacyTargetEffectiveModel
            == expectedTarget.targetEffectiveModel
        && legacy.legacyTargetDialect == expectedTarget.targetDialect
        then Just expectedTarget.targetDialect
        else Nothing

activeSubagentTargetError
    :: Provider
    -> Text
    -> Text
    -> SubagentSession
    -> Maybe Text
activeSubagentTargetError provider connection effectiveModel session =
    subagentTargetError
        SubagentTarget
            { targetProvider = provider
            , targetConnection = connection
            , targetEffectiveModel = effectiveModel
            , targetDialect = session.subSessionDialect
            }
        SubagentTarget
            { targetProvider = session.subSessionProvider
            , targetConnection = session.subSessionConnection
            , targetEffectiveModel = session.subSessionEffectiveModel
            , targetDialect = session.subSessionDialect
            }

subagentTargetError
    :: SubagentTarget
    -> SubagentTarget
    -> Maybe Text
subagentTargetError expected stored
    | stored.targetProvider /= expected.targetProvider =
        Just
            ( "cannot continue subagent created for the "
                <> providerSlug stored.targetProvider
                <> " transport under "
                <> providerSlug expected.targetProvider
            )
    | stored.targetConnection /= expected.targetConnection =
        Just
            ( "cannot continue subagent created for connection "
                <> stored.targetConnection
                <> " under connection "
                <> expected.targetConnection
            )
    | stored.targetEffectiveModel /= expected.targetEffectiveModel =
        Just
            ( "cannot continue subagent after its effective model changed \
                \from "
                <> stored.targetEffectiveModel
                <> " to "
                <> expected.targetEffectiveModel
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
