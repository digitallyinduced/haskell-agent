-- | Execute one admitted turn. Worker lifetime and gateway leases are owned by
-- the supervisor; this module owns native hooks and per-turn output collection.
module Agent.CLI.MacOS.TurnExecution
    ( composeNativeTools
    , runNativeTurn
    , nativeExceptionMessage
    ) where

import Agent.CLI.MacOS.EngineEvents
import Agent.CLI.MacOS.EngineMailbox (EngineMailbox, acceptEngineCommand)
import Agent.CLI.MacOS.EngineState (EngineCommand(..))
import Agent.CLI.MacOS.InteractionState (InteractionRuntime(..))
import Agent.CLI.MacOS.NativeInteraction
    ( nativePlanModeHooks, requestApproval, requestFreshApproval
    , requestRootAccessFromClient )
import Agent.CLI.MacOS.NativeLoopEvent (encodeNativeLoopEventWithChartCalls)
import Agent.CLI.MacOS.NativeRequest (TurnStart(..))
import Agent.CLI.MacOS.TurnEvents (nativeLoopEvent)
import Agent.CLI.MacOS.TurnInputs
import Agent.CLI.MacOS.TurnState
import Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeRunHooks(..)
    , NativeWorkspaceDiscovery(..)
    , StartupFailure(..)
    , fullNativeRunCapabilities
    , runNativeAgent
    )
import Agent.Loop
    ( ImageAttachment, LoopEvent(..), TurnOutput(..), emptyTokenUsage )
import Agent.Runtime.StartupPolicy (hostNativeStartupPolicy)
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools.Types (AppTool(..), AppToolGroup(..), appToolsFromGroups)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception.Safe (SomeException, fromException, tryAny, finally)
import Control.Concurrent.MVar (modifyMVar_)
import qualified Data.Map.Strict as Map
import Control.Monad (forM_, void)
import qualified Data.Aeson as Aeson
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Foreign.Ptr (FunPtr, Ptr)
import System.IO (IOMode(WriteMode), withFile)
import System.OsPath (unsafeEncodeUtf)

-- | Replace, rather than append, the generic desktop backend. This keeps the
-- command-line computer-use flag authoritative and guarantees a single
-- model-visible @computer@ tool.
composeNativeTools :: Maybe AppTool -> [AppToolGroup] -> [AppTool]
composeNativeTools Nothing = appToolsFromGroups
composeNativeTools (Just nativeComputer) =
    replaceFirst False . appToolsFromGroups
  where
    replaceFirst _ [] = []
    replaceFirst found (tool : tools)
        | tool.appToolName /= "computer" =
            tool : replaceFirst found tools
        | found =
            replaceFirst True tools
        | otherwise =
            nativeComputer : replaceFirst True tools

runNativeTurn
    :: FunPtr EventCallback
    -> Ptr ()
    -> EngineMailbox EngineCommand
    -> NativeProcessRuntime
    -> TurnControl
    -> [AppTool]
    -> Maybe (AppTool, IO (), IO ())
    -> TurnStart
    -> [ImageAttachment]
    -> NativeTurnOptions
    -> InteractionRuntime
    -> IO TurnOutcome
runNativeTurn
        callback context commands processRuntime control nativeHostTools
        nativeComputerSession start images turnOptions interactions = do
    sessionIdRef <- newIORef start.turnStartSessionId
    completedRef <- newIORef False
    usageRef <- newIORef emptyTokenUsage
    chartCallsRef <- newIORef Set.empty
    let hooks = NativeRunHooks
            { nativeOnLoopEvent = \event -> do
                case event of
                    ToolStarted call | call.name == "render_chart" ->
                        atomicModifyIORef' chartCallsRef \calls ->
                            (Set.insert call.callId calls, ())
                    TurnFinished output -> do
                        writeIORef completedRef True
                        modifyIORef' usageRef (<> output.tokenUsage)
                    ModelContextReset ->
                        forM_ nativeComputerSession \(_, reset, _) -> reset
                    _ -> pure ()
                chartCalls <- readIORef chartCallsRef
                case encodeNativeLoopEventWithChartCalls chartCalls control.turnControlId event of
                    Just bytes -> sendBinaryEvent callback context bytes
                    Nothing ->
                        forM_ (nativeLoopEvent control.turnControlId event)
                            (sendEvent callback context)
            , nativeInitialTurnInputs = Nothing
            , nativeOnSessionId = \sessionId -> do
                writeIORef sessionIdRef (Just sessionId)
                atomically do
                    writeTVar
                        control.turnControlSessionId
                        (Just sessionId)
                    void $ acceptEngineCommand
                        commands
                        (EngineTaskSession control.turnControlId sessionId)
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.session" :: Text)
                        , "turnId" Aeson..= control.turnControlId
                        , "sessionId" Aeson..= sessionId
                        ]
            , nativeRegisterCancel =
                atomically . writeTVar control.turnControlCancel
            , nativeRegisterAgentSnapshot =
                atomically . writeTVar control.turnControlAgentSnapshot
            , nativeRequestApproval =
                requestApproval callback context control
            , nativeRequestFreshApproval =
                requestFreshApproval callback context control
            , nativeRequestRootAccess =
                requestRootAccessFromClient callback context control
            , nativeToolGroups = [HostToolGroup nativeHostTools]
            , nativeComposeTools =
                composeNativeTools
                    ((\(tool, _, _) -> tool) <$> nativeComputerSession)
            , nativePlanHooks =
                nativePlanModeHooks control interactions
            , nativeInteractionMode =
                turnOptions.nativeTurnInteractionMode
            , nativeRegisterInteractionMode = Just $ \setter ->
                modifyMVar_ interactions.interactionModeSetters $
                    pure . Map.insert control.turnControlId setter
            , nativeShellMode = turnOptions.nativeTurnShellMode
            , nativeHome = Nothing
            , nativeDatabaseStore = Nothing
            , nativeDatabaseScopeNamespace = Nothing
            , nativeExposeHarnessCatalog = True
            , nativeWorkspaceDiscovery = DiscoverHostWorkspace
            , nativeCapabilities = fullNativeRunCapabilities
            , nativeStartupPolicy = hostNativeStartupPolicy
            }
        args = nativeTurnArguments start
    result <- tryAny $ flip finally
        (modifyMVar_ interactions.interactionModeSetters $
            pure . Map.delete control.turnControlId) $
        withTurnImages start.turnStartPrompt images \managedFile ->
            withFile "/dev/null" WriteMode \output ->
                runNativeAgent
                    processRuntime
                    output
                    (unsafeEncodeUtf start.turnStartCwd)
                    hooks
                    (args <> maybe ["--prompt", Text.unpack start.turnStartPrompt]
                        (\path -> ["--managed-turn-file", path])
                        managedFile)
    completed <- readIORef completedRef
    sessionId <- readIORef sessionIdRef
    usage <- readIORef usageRef
    pure TurnOutcome
        { turnOutcomeSessionId = sessionId
        , turnOutcomeError =
            case result of
                Left exception -> Just (nativeExceptionMessage exception)
                Right (Left err) -> Just err
                Right (Right ())
                    | completed -> Nothing
                    | otherwise ->
                        Just
                            "turn ended without a completion event"
        , turnOutcomeUsage = usage
        , turnOutcomeProviderCostUSD = Nothing
        }

nativeExceptionMessage :: SomeException -> Text
nativeExceptionMessage exception =
    case fromException exception of
        Just (StartupFailure message) -> message
        Nothing -> Text.pack (show exception)
