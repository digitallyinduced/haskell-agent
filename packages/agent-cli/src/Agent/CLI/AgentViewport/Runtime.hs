-- | State and lifecycle for a live agent viewport.
module Agent.CLI.AgentViewport.Runtime
    ( AgentStepCache(..)
    , AgentChildListing(..)
    , AgentChildSource(..)
    , AgentViewportRuntime
    , AgentViewportRuntimeConfig(..)
    , agentViewportEnvironment
    , loadAgentSnapshot
    , newAgentViewportRuntime
    , recordAgentViewportEvent
    , resetAgentViewport
    , selectAgentViewport
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , agentStepsForStatusRelative
    , formatAgentStatus
    , responseItemPreviewLines
    , responseItemStepPreviewsRelative
    , responseItemsToUiStateRelative
    )
import Agent.CLI.NativeAgents
    ( NativeAgentView(..)
    , applyNativeAgentEvent
    , nativeAgentEntries
    , restoreNativeAgents
    )
import qualified Agent.CLI.TUI.Bridge as TuiBridge
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.Loop (LoopEvent)
import Agent.Responses.Types (ResponseItem)
import Agent.Subagents
    ( SubagentId
    , SubagentStatus(..)
    )
import Agent.TUI.Model
    ( BlockState(..)
    , UiEvent(..)
    , UiState(..)
    , initialUiState
    , reduceUi
    )
import Control.Monad (when)
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Mem.StableName (StableName, makeStableName)

data AgentStepCache = AgentStepCache
    { cachedTranscript :: !(StableName [ResponseItem])
    , cachedVariant :: !(Maybe SubagentStatus)
    , cachedSteps :: ![AgentStep]
    }

-- | Registry metadata needed to render one host-managed child agent.
data AgentChildListing = AgentChildListing
    { childListingPath :: !Text
    , childListingId :: !SubagentId
    , childListingStatus :: !SubagentStatus
    }

-- | Resident session data for a child agent.
--
-- The transcript remains an action so snapshots can read child transcripts
-- concurrently after taking one coherent snapshot of the resident-session map.
data AgentChildSource = AgentChildSource
    { childSourceModel :: !Text
    , childSourceTranscript :: !(IO [ResponseItem])
    }

-- | External capabilities used by the viewport runtime.
--
-- Child selection and release stay injected because they own persistence and
-- hydration concerns outside of the viewport itself.
data AgentViewportRuntimeConfig = AgentViewportRuntimeConfig
    { viewportConfigShowRawReasoning :: !Bool
    , viewportConfigWorkspace :: !Text
    , viewportConfigReadRootTranscript :: !(IO [ResponseItem])
    , viewportConfigListChildren :: !(IO [AgentChildListing])
    , viewportConfigReadChildSources
        :: !(IO (Map.Map SubagentId AgentChildSource))
    , viewportConfigSelectChild :: !(SubagentId -> IO ())
    , viewportConfigReleaseChild :: !(SubagentId -> IO ())
    }

data AgentViewportRuntime = AgentViewportRuntime
    { runtimeEnvironment :: !AgentViewportEnv
    , runtimeLoadSnapshot :: !(Bool -> IO (AgentTarget, [AgentEntry]))
    , runtimeRecordEvent :: !(LoopEvent -> IO ())
    , runtimeReset :: !(IO ())
    }

newAgentViewportRuntime
    :: AgentViewportRuntimeConfig
    -> IO AgentViewportRuntime
newAgentViewportRuntime config = do
    selectedAgent <- newIORef AgentRoot
    nativeAgents <- newIORef (Map.empty :: Map.Map Text NativeAgentView)
    stepCache <-
        newIORef (Map.empty :: Map.Map AgentTarget AgentStepCache)
    let cachedAgentSteps target variant items build = do
            transcriptName <- makeStableName items
            cache <- readIORef stepCache
            case Map.lookup target cache of
                Just cached
                    | cached.cachedTranscript == transcriptName
                    , cached.cachedVariant == variant ->
                        pure cached.cachedSteps
                _ -> do
                    let steps = build items
                    atomicModifyIORef' stepCache \current ->
                        ( Map.insert target (AgentStepCache
                            { cachedTranscript = transcriptName
                            , cachedVariant = variant
                            , cachedSteps = steps
                            })
                            current
                        , ()
                        )
                    pure steps
        loadSnapshot includeSummaries = do
            rootItems <- config.viewportConfigReadRootTranscript
            native <-
                atomicModifyIORef' nativeAgents \current ->
                    let restored = restoreNativeAgents rootItems current
                    in (restored, restored)
            children <- config.viewportConfigListChildren
            let availableTargets =
                    AgentRoot
                        : map
                            (AgentChild . (.childListingId))
                            children
                        <> map
                            (AgentNative . (.nativeAgentId))
                            (Map.elems native)
            selected <-
                atomicModifyIORef' selectedAgent \current ->
                    let reconciled =
                            TuiBridge.reconcileAgentSelection
                                availableTargets
                                current
                    in (reconciled, reconciled)
            childSources <- config.viewportConfigReadChildSources
            let transcriptLines target items
                    | null children = []
                    | target == selected = case target of
                        AgentRoot ->
                            responseItemPreviewLines 12 items
                        AgentChild _
                            | includeSummaries ->
                                responseItemPreviewLines 12 items
                            | otherwise ->
                                []
                        AgentNative nativeId ->
                            maybe [] (.nativeAgentTranscript)
                                (Map.lookup nativeId native)
                    | includeSummaries =
                        responseItemPreviewLines 0 items
                    | otherwise = []
                conversationFor target status items
                    | includeSummaries = initialUiState
                    | target /= selected = initialUiState
                    | target == AgentRoot = initialUiState
                    | AgentNative nativeId <- target =
                        maybe initialUiState (.nativeAgentConversation)
                            (Map.lookup nativeId native)
                    | otherwise =
                        settleConversation items status $
                            responseItemsToUiStateRelative
                                config.viewportConfigShowRawReasoning
                                config.viewportConfigWorkspace
                                items
            rootSteps <-
                if null children
                    then pure []
                    else cachedAgentSteps
                        AgentRoot
                        Nothing
                        rootItems
                        (responseItemStepPreviewsRelative
                            config.viewportConfigWorkspace
                            2)
            let rootEntry = AgentEntry
                    { agentTarget = AgentRoot
                    , agentPath = "/root"
                    , agentStatus = "active"
                    , agentModel = Nothing
                    , agentSteps = rootSteps
                    , agentTranscript =
                        transcriptLines AgentRoot rootItems
                    , agentConversation = initialUiState
                    }
            childEntries <- mapConcurrentlyBounded 8
                (materializeChild
                    config.viewportConfigWorkspace
                    cachedAgentSteps
                    transcriptLines
                    conversationFor
                    childSources)
                children
            pure
                ( selected
                , rootEntry : childEntries <> nativeAgentEntries native
                )
        selectAgent target = do
            previous <- readIORef selectedAgent
            when (previous /= target) $
                releaseAgent config previous
            case target of
                AgentRoot -> pure ()
                AgentNative _ -> pure ()
                AgentChild agentId ->
                    config.viewportConfigSelectChild agentId
            writeIORef selectedAgent target
        environment = AgentViewportEnv
            { viewportSelected = selectedAgent
            , viewportSelect = selectAgent
            , viewportEntries = snd <$> loadSnapshot True
            }
        recordEvent event =
            atomicModifyIORef' nativeAgents \current ->
                (applyNativeAgentEvent event current, ())
        reset = do
            writeIORef selectedAgent AgentRoot
            writeIORef stepCache Map.empty
    pure AgentViewportRuntime
        { runtimeEnvironment = environment
        , runtimeLoadSnapshot = loadSnapshot
        , runtimeRecordEvent = recordEvent
        , runtimeReset = reset
        }

agentViewportEnvironment :: AgentViewportRuntime -> AgentViewportEnv
agentViewportEnvironment = (.runtimeEnvironment)

loadAgentSnapshot
    :: AgentViewportRuntime
    -> Bool
    -> IO (AgentTarget, [AgentEntry])
loadAgentSnapshot runtime = runtime.runtimeLoadSnapshot

recordAgentViewportEvent :: AgentViewportRuntime -> LoopEvent -> IO ()
recordAgentViewportEvent runtime = runtime.runtimeRecordEvent

selectAgentViewport
    :: AgentViewportRuntime
    -> AgentTarget
    -> IO ()
selectAgentViewport runtime =
    runtime.runtimeEnvironment.viewportSelect

-- | Reset selection and derived step previews for a fresh conversation.
--
-- Native-agent views remain event-owned, matching the existing session reset
-- semantics.
resetAgentViewport :: AgentViewportRuntime -> IO ()
resetAgentViewport = (.runtimeReset)

releaseAgent :: AgentViewportRuntimeConfig -> AgentTarget -> IO ()
releaseAgent config = \case
    AgentRoot -> pure ()
    AgentNative _ -> pure ()
    AgentChild agentId ->
        config.viewportConfigReleaseChild agentId

materializeChild
    :: Text
    -> ( AgentTarget
        -> Maybe SubagentStatus
        -> [ResponseItem]
        -> ([ResponseItem] -> [AgentStep])
        -> IO [AgentStep]
       )
    -> (AgentTarget -> [ResponseItem] -> [Text])
    -> (AgentTarget -> SubagentStatus -> [ResponseItem] -> UiState)
    -> Map.Map SubagentId AgentChildSource
    -> AgentChildListing
    -> IO AgentEntry
materializeChild
        workspace
        cachedAgentSteps
        transcriptLines
        conversationFor
        childSources
        child = do
    let agentId = child.childListingId
        target = AgentChild agentId
        maybeSource = Map.lookup agentId childSources
    items <- maybe (pure []) (.childSourceTranscript) maybeSource
    steps <- cachedAgentSteps
        target
        (Just child.childListingStatus)
        items
        (agentStepsForStatusRelative
            workspace
            2
            child.childListingStatus)
    let transcript =
            transcriptLines target items
                <> case child.childListingStatus of
                    Completed (Just result)
                        | null items
                        , not (Text.null (Text.strip result)) ->
                            ["assistant: " <> Text.strip result]
                    Errored message ->
                        ["error: " <> Text.strip message]
                    _ -> []
    pure AgentEntry
        { agentTarget = target
        , agentPath = child.childListingPath
        , agentStatus = formatAgentStatus child.childListingStatus
        , agentModel = (.childSourceModel) <$> maybeSource
        , agentSteps = steps
        , agentTranscript = transcript
        , agentConversation =
            conversationFor target child.childListingStatus items
        }

settleConversation
    :: [ResponseItem]
    -> SubagentStatus
    -> UiState
    -> UiState
settleConversation items status conversation =
    case status of
        Pending -> conversation
        Running -> conversation
        Completed result ->
            let settled = finalizeAll BlockComplete conversation
            in case result of
                Just text
                    | null items
                    , not (Text.null (Text.strip text)) ->
                        reduceUi
                            (UiAssistantHistory (Text.strip text))
                            settled
                _ -> settled
        Errored message ->
            reduceUi
                (UiErrorMessage
                    (if Text.null (Text.strip message)
                        then "Agent failed."
                        else Text.strip message))
                (finalizeAll BlockFailed conversation)
        Interrupted ->
            finalizeAll BlockCancelled conversation
        Closed ->
            finalizeAll BlockComplete conversation
        NotFound ->
            reduceUi
                (UiErrorMessage "Agent transcript is unavailable.")
                (finalizeAll BlockFailed conversation)
  where
    finalizeAll terminal ui =
        reduceUi
            (UiTurnEnded terminal)
            ui { uiTurnStartBlock = 0 }
