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
import Agent.CLI.MacOS.InteractionState (InteractionRuntime)
import Agent.CLI.MacOS.NativeInteraction
    ( nativePlanModeHooks, requestApproval, requestRootAccessFromClient )
import Agent.CLI.MacOS.NativeLoopEvent (encodeNativeLoopEvent)
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
import Agent.Tools.Types (AppTool(..), AppToolGroup(..), appToolsFromGroups)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception.Safe (SomeException, fromException, tryAny)
import Control.Monad (forM_, void)
import qualified Data.Aeson as Aeson
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
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
    -> Maybe AppTool
    -> TurnStart
    -> [ImageAttachment]
    -> NativeTurnOptions
    -> InteractionRuntime
    -> IO TurnOutcome
runNativeTurn
        callback context commands processRuntime control nativeBrowserTools
        nativeComputerTool start images turnOptions interactions = do
    sessionIdRef <- newIORef start.turnStartSessionId
    completedRef <- newIORef False
    usageRef <- newIORef emptyTokenUsage
    let hooks = NativeRunHooks
            { nativeOnLoopEvent = \event -> do
                case event of
                    TurnFinished output -> do
                        writeIORef completedRef True
                        modifyIORef' usageRef (<> output.tokenUsage)
                    _ -> pure ()
                case encodeNativeLoopEvent control.turnControlId event of
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
            , nativeRequestRootAccess =
                requestRootAccessFromClient callback context control
            , nativeToolGroups = [HostToolGroup nativeBrowserTools]
            , nativeComposeTools =
                composeNativeTools nativeComputerTool
            , nativePlanHooks =
                nativePlanModeHooks control interactions
            , nativeInteractionMode =
                turnOptions.nativeTurnInteractionMode
            , nativeShellMode = turnOptions.nativeTurnShellMode
            , nativeHome = Nothing
            , nativeDatabaseStore = Nothing
            , nativeDatabaseScopeNamespace = Nothing
            , nativeWorkspaceDiscovery = DiscoverHostWorkspace
            , nativeCapabilities = fullNativeRunCapabilities
            , nativePrepareOptions = Right
            }
        args = nativeTurnArguments start
    result <- tryAny $
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
