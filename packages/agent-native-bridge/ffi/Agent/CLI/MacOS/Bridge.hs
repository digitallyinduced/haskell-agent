{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.Bridge
    ( BrowserCallback
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , composeNativeTools
    , invokeBrowserCommand
    , RepositoryCheckHandle(..)
    , ha_repository_check_destroy
    , TurnStart(..)
    , nativeExceptionMessage
    , nativeRequestRequiresGatewayLock
    , nativeSessionRouteMatchesBoundary
    , nativeTurnRouteMatchesBoundary
    , nativeTurnArguments
    , NativeInteractionResolution(..)
    , PendingInteraction(..)
    , cancelPendingInteractions
    , discardStagedTurn
    , discardStagedTurnById
    , emitBoundaryChecked
    , invokeGatewayCallbackOnce
    , resolvePendingInteraction
    , turnStartCleanupId
    ) where

import Agent.CLI.MacOS.ResourceAdmin ()
import Agent.CLI.MacOS.Marshalling
import Agent.CLI.MacOS.NativeRequest
import Agent.CLI.MacOS.NativeGatewayBoundary
import Agent.CLI.MacOS.NativeModelCatalog
import Agent.CLI.MacOS.BrowserBridge
import Agent.CLI.MacOS.GatewayBridge (invokeGatewayCallbackOnce)
import Agent.CLI.MacOS.AccountBridge ()
import Agent.CLI.MacOS.RepositoryChecks
import Agent.CLI.MacOS.RepositoryDeliveryBridge ()
import Agent.CLI.MacOS.RepositoryReviewBridge ()
import Agent.CLI.MacOS.ComputerBridge
    ( ComputerCallback
    , ComputerHost(..)
    , ComputerRegistration(..)
    , computerToolWhenEnabled
    , newComputerHost
    )
import qualified Agent.CLI.AgentViewport as Viewport
import Agent.CLI.Render (summarizeToolCall)
import Agent.CLI.NativeRuntime
    ( NativeInteractionMode(..)
    , NativeProcessRuntime
    , NativeRunHooks(..)
    , NativeShellMode(..)
    , NativeWorkspaceDiscovery(..)
    , StartupFailure(..)
    , closeNativeProcessRuntime
    , fullNativeRunCapabilities
    , newNativeProcessRuntime
    , restartNativeMcpRuntime
    , runNativeAgent
    )
import Agent.CLI.McpAdmin
    ( McpAdminError
    , McpAdminSnapshot (..)
    , restartMcpAdminServer
    )
import Agent.CLI.MacOS.McpAdminBridge
    ( McpResultCallback
    , invokeMcpResultCallback
    , decodeMcpInput
    , maxMcpTextBytes
    , emitMcpResult
    , mcpAdminTry
    )
import Agent.Store.Postgres.Session
    ( NativeConversationSearchResult(..)
    , searchNativeConversationsForBoundary
    )
import Agent.CLI.ManagedTurn
    ( ManagedTurnMedia(..)
    , managedTurnRequestWithImages
    , renderManagedTurnPrompt
    )
import Agent.CLI.MacOS.DatabaseBrowseBridge ()
import Agent.CLI.MacOS.LearnedSkillsBridge ()
import Agent.CLI.MacOS.NativeLoopEvent
    ( encodeNativeLoopEvent
    , encodeNativeUsageEvent
    )
import Agent.CLI.MacOS.EngineMailbox
    ( EngineMailbox
    , acceptEngineCommand
    , closeEngineMailbox
    , drainEngineCommands
    , newEngineMailboxIO
    , readEngineCommand
    )
import Agent.Runtime.Daemon.TaskScheduler
    ( TaskIdentity(..)
    , selectRunnableTasks
    )
import qualified Agent.CLI.GatewayBoundary as GatewayBoundary
import Agent.CLI.GatewayClient
    ( withGatewayCredentialLease
    , withGatewayCredentialTurnLease
    )
import Agent.CLI.ModelConfig
    ( organizationGatewayConnectionId
    )
import Agent.CLI.Models
    ( validateResumedGatewayBoundary
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , SessionTransfer(..)
    , SessionTransferEnvelope(..)
    , TranscriptEffect(..)
    , deleteSession
    , forkSessionAtTurn
    , importSessionTransferRemapped
    , listArchivedSessionIds
    , listSessions
    , loadSessionHistoryTurnsAround
    , renameSession
    , setSessionArchived
    , sessionsRoot
    , streamSessionTransfer
    )
import Agent.CLI.SessionAdmin
    ( loadSessionPageJSON
    , managedPostgresConfigForHome
    , sessionSummaryWithStatusJSON
    )
import Agent.Loop
    ( ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , emptyTokenUsage
    )
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , closeStore
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallMode(..)
    , toolCallMode
    , ToolCallKind(..)
    , isComputerToolCallKind
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    )
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolGroup(..)
    , appToolsFromGroups
    )
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , withMVar
    , takeMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTVarIO
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTVar
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeException
    , bracket
    , finally
    , fromException
    , mask
    , onException
    , throwString
    , tryAny
    )
import Control.Monad
    ( foldM
    , filterM
    , forM_
    , void
    , when
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson
    ( (.:?)
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Int (Int64)
import Data.Foldable (toList)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , StablePtr
    , Storable(..)
    , allocaArray
    , castPtr
    , castPtrToStablePtr
    , castStablePtrToPtr
    , deRefStablePtr
    , freeStablePtr
    , newStablePtr
    , nullFunPtr
    , nullPtr
    , peek
    , poke
    , plusPtr
    , peekByteOff
    , pokeByteOff
    , pokeElemOff
    , sizeOf
    )
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CLLong(..), CSize(..))
import System.Directory
    ( getTemporaryDirectory
    , removeFile
    )
import System.Directory.OsPath (getHomeDirectory)
import System.IO
    ( IOMode(WriteMode)
    , hClose
    , openBinaryTempFile
    , withFile
    )
import System.OsPath
    ( OsPath
    , decodeFS
    , unsafeEncodeUtf
    )

type EventCallback = Ptr () -> Ptr Word8 -> CSize -> IO ()

data CInteractionOption = CInteractionOption
    { cInteractionOptionLabel :: !(Ptr Word8)
    , cInteractionOptionLabelLength :: !CSize
    }

instance Storable CInteractionOption where
    sizeOf _ = sizeOf (nullPtr :: Ptr Word8) + sizeOf (undefined :: CSize)
    alignment _ =
        max
            (alignment (nullPtr :: Ptr Word8))
            (alignment (undefined :: CSize))
    peek pointer =
        CInteractionOption
            <$> peekByteOff pointer 0
            <*> peekByteOff pointer (sizeOf (nullPtr :: Ptr Word8))
    poke pointer option = do
        pokeByteOff pointer 0 option.cInteractionOptionLabel
        pokeByteOff
            pointer
            (sizeOf (nullPtr :: Ptr Word8))
            option.cInteractionOptionLabelLength

type InteractionCallback =
    Ptr ()
    -> Ptr Word8 -> CSize -- turn id
    -> Ptr Word8 -> CSize -- interaction id
    -> CInt -- kind
    -> Ptr Word8 -> CSize -- prompt/body
    -> Ptr CInteractionOption -> CSize
    -> IO ()

type SessionResultCallback =
    Ptr () -> CInt -> CString -> CSize -> IO ()

-- Status is 0 for a result, 1 for completion, and -1 for failure. Every
-- pointer is callback-scoped UTF-8. A turn index of -1 denotes a metadata hit;
-- role is 0 (metadata), 1 (user), or 2 (assistant).
type SearchCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize -- session id
    -> Ptr Word8 -> CSize -- title
    -> Ptr Word8 -> CSize -- cwd
    -> Ptr Word8 -> CSize -- provider
    -> Ptr Word8 -> CSize -- model
    -> Int64 -> CInt -> Int64 -> Int64 -> CInt
    -> Ptr Word8 -> CSize -- user
    -> Ptr Word8 -> CSize -- assistant
    -> CDouble
    -> Ptr Word8 -> CSize -- error
    -> IO ()

-- Status is 0 for an active task, 1 for completion, and -1 for failure.
-- State is 0 for queued and 1 for running. Every pointer is callback-scoped.
type TaskSnapshotCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize -- task id
    -> Ptr Word8 -> CSize -- session id, optional
    -> CInt
    -> Ptr Word8 -> CSize -- error
    -> IO ()

-- Session page status is 0 for a turn, 1 for completion, and -1 for failure.
-- Text buffers are callback-scoped UTF-8.
type SessionTurnCallback =
    Ptr () -> CInt -> Int64
    -> CString -> CSize -- occurred at
    -> CString -> CSize -- user
    -> CString -> CSize -- assistant
    -> CString -> CSize -- turn error
    -> CString -> CSize -- response id
    -> CString -> CSize -- transcript effect
    -> CString -> CSize -- provider-extensible response items JSON
    -> CLLong -> CLLong -> CLLong -- usage; -1 means absent
    -> CInt -> CInt -- has older/newer on completion
    -> CString -> CSize -- error
    -> IO ()

-- Transfer result status is 0 for success and -1 for failure.
type SessionTransferResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

-- Export status is 0 for a chunk, 1 for completion, and -1 for failure.
type SessionExportCallback =
    Ptr () -> CInt -> Ptr Word8 -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeEventCallback :: FunPtr EventCallback -> EventCallback

foreign import ccall "dynamic"
    invokeInteractionCallback
        :: FunPtr InteractionCallback -> InteractionCallback

foreign import ccall "dynamic"
    invokeSessionTransferResultCallback
        :: FunPtr SessionTransferResultCallback -> SessionTransferResultCallback

foreign import ccall "dynamic"
    invokeSearchCallback :: FunPtr SearchCallback -> SearchCallback

foreign import ccall "dynamic"
    invokeTaskSnapshotCallback
        :: FunPtr TaskSnapshotCallback -> TaskSnapshotCallback

foreign import ccall "dynamic"
    invokeSessionTurnCallback
        :: FunPtr SessionTurnCallback -> SessionTurnCallback

foreign import ccall "dynamic"
    invokeSessionResultCallback
        :: FunPtr SessionResultCallback -> SessionResultCallback

foreign import ccall "dynamic"
    invokeSessionExportCallback
        :: FunPtr SessionExportCallback -> SessionExportCallback

discardStagedTurn
    :: Text
    -> Aeson.Value
    -> TVar (Map Text a)
    -> TVar (Map Text b)
    -> STM ()
discardStagedTurn requestId params stagedImages stagedOptions = do
    discardStagedTurnById
        (turnStartCleanupId requestId params)
        stagedImages
        stagedOptions

discardStagedTurnById
    :: Text
    -> TVar (Map Text a)
    -> TVar (Map Text b)
    -> STM ()
discardStagedTurnById turnId stagedImages stagedOptions = do
    modifyTVar' stagedImages (Map.delete turnId)
    modifyTVar' stagedOptions (Map.delete turnId)

data NativeTurnOptions = NativeTurnOptions
    { nativeTurnInteractionMode :: !NativeInteractionMode
    , nativeTurnShellMode :: !NativeShellMode
    } deriving (Eq, Show)

defaultNativeTurnOptions :: NativeTurnOptions
defaultNativeTurnOptions = NativeTurnOptions
    { nativeTurnInteractionMode = NativeAsk
    , nativeTurnShellMode = NativeShellBash
    }

data NativeInteractionResolution = NativeInteractionResolution
    { interactionSelectedIndex :: !Int
    , interactionCustomText :: !(Maybe Text)
    } deriving (Eq, Show)

data PendingInteraction = PendingInteraction
    { pendingInteractionOptionCount :: !Int
    , pendingInteractionWaiter :: !(TMVar NativeInteractionResolution)
    }

resolvePendingInteraction
    :: TVar (Map (Text, Text) PendingInteraction)
    -> (Text, Text)
    -> NativeInteractionResolution
    -> STM Bool
resolvePendingInteraction pendingRef key resolution = do
    pending <- readTVar pendingRef
    case Map.lookup key pending of
        Nothing -> pure False
        Just interaction@PendingInteraction
            { pendingInteractionOptionCount = optionCount
            }
            | resolution.interactionSelectedIndex < (-1)
                || resolution.interactionSelectedIndex >= optionCount ->
                pure False
            | otherwise -> do
                published <- tryPutTMVar
                    interaction.pendingInteractionWaiter
                    resolution
                when published $
                    writeTVar pendingRef (Map.delete key pending)
                pure published

cancelPendingInteractions
    :: TVar (Map (Text, Text) PendingInteraction)
    -> STM ()
cancelPendingInteractions pendingRef = do
    pending <- readTVar pendingRef
    writeTVar pendingRef Map.empty
    forM_ (Map.elems pending) \interaction ->
        void $ tryPutTMVar
            interaction.pendingInteractionWaiter
            cancelledInteractionResolution

cancelledInteractionResolution :: NativeInteractionResolution
cancelledInteractionResolution = NativeInteractionResolution
    { interactionSelectedIndex = -1
    , interactionCustomText = Nothing
    }

data InteractionCallbackTarget = InteractionCallbackTarget
    { interactionTargetCallback :: !(FunPtr InteractionCallback)
    , interactionTargetContext :: !(Ptr ())
    }

data InteractionRuntime = InteractionRuntime
    { interactionCallbackTarget :: !(TVar (Maybe InteractionCallbackTarget))
    , interactionCallbackLock :: !(MVar ())
    , interactionPending
        :: !(TVar (Map (Text, Text) PendingInteraction))
    }

data EngineCommand
    = EngineRequest !BridgeRequest
    | EngineSearch !Text !Int !(FunPtr SearchCallback) !(Ptr ())
    | EngineSessionMutation
        !SessionMutation !(FunPtr SessionResultCallback) !(Ptr ())
    | EngineMcpRestart !Word64 !Text !(FunPtr McpResultCallback) !(Ptr ())
    | EngineCancelTask !Text
    | EngineTaskSnapshot !(FunPtr TaskSnapshotCallback) !(Ptr ())
    | EngineSetTaskLimit !Int
    | EngineTaskSession !Text !Text
    | EngineTaskFinished !Text !TaskResult
    | EngineStop

data SessionMutation
    = SessionRename !Text !Text
    | SessionDelete !Text
    | SessionArchive !Text !Bool

data Engine = Engine
    { engineCommands :: !(EngineMailbox EngineCommand)
    , engineWorker :: !(Async ())
    , engineStagedImages :: !(TVar (Map Text [ImageAttachment]))
    , engineBrowser :: !BrowserHost
    , engineComputer :: !ComputerHost
    , engineStagedTurnOptions :: !(TVar (Map Text NativeTurnOptions))
    , engineInteractions :: !InteractionRuntime
    }

data TurnControl = TurnControl
    { turnControlId :: !Text
    , turnControlGatewayIdentity :: !(Maybe Text)
    , turnControlSessionId :: !(TVar (Maybe Text))
    , turnControlCancelled :: !(TVar Bool)
    , turnControlCancel :: !(TVar (IO ()))
    , turnControlApprovals
        :: !(TVar (Map Text (TMVar PermissionChoice)))
    , turnControlApprovalCounter :: !(TVar Int)
    , turnControlInteractionCounter :: !(TVar Int)
    , turnControlAllowedTools :: !(TVar (Set.Set Text))
    , turnControlAgentSnapshot :: !(TVar (IO [Viewport.AgentEntry]))
    , turnControlInteractions :: !InteractionRuntime
    }

data TurnOutcome = TurnOutcome
    { turnOutcomeSessionId :: !(Maybe Text)
    , turnOutcomeError :: !(Maybe Text)
    , turnOutcomeUsage :: !TokenUsage
    , turnOutcomeProviderCostUSD :: !(Maybe Double)
    }

data TaskResult
    = TaskOutcome !TurnOutcome
    | TaskFailure !Text

data PendingTurn = PendingTurn
    { pendingTurnStart :: !TurnStart
    , pendingTurnGatewayIdentity :: !(Maybe Text)
    , pendingTurnImages :: ![ImageAttachment]
    , pendingTurnOptions :: !NativeTurnOptions
    }

data RunningTurn = RunningTurn
    { runningTurnControl :: !TurnControl
    , runningTurnWorker :: !(Async ())
    }

data TaskSupervisor = TaskSupervisor
    { supervisorLimit :: !Int
    , supervisorPending :: !(Seq PendingTurn)
    , supervisorRunning :: !(Map Text RunningTurn)
    , supervisorKnownTaskIds :: !(Set.Set Text)
    }

defaultTaskLimit :: Int
defaultTaskLimit = 3

foreign export ccall ha_engine_create
    :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())

foreign export ccall ha_engine_send_json
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt

foreign export ccall ha_engine_set_browser_callback
    :: Ptr () -> FunPtr BrowserCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_set_computer_callback
    :: Ptr () -> FunPtr ComputerCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_cancel_task
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_list_tasks
    :: Ptr () -> FunPtr TaskSnapshotCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_set_task_limit
    :: Ptr () -> CSize -> IO CInt

foreign export ccall ha_engine_stage_turn_options
    :: Ptr () -> Ptr Word8 -> CSize -> CInt -> CInt -> IO CInt

foreign export ccall ha_engine_discard_turn_staging
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_set_interaction_callback
    :: Ptr () -> FunPtr InteractionCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_resolve_interaction
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_destroy
    :: Ptr () -> IO ()

foreign export ccall ha_engine_session_rename
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_session_delete
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_session_archive
    :: Ptr () -> Ptr Word8 -> CSize -> CInt
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_load_around
    :: Ptr Word8 -> CSize -> Int64 -> CInt
    -> FunPtr SessionTurnCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_fork
    :: Ptr Word8 -> CSize -> Int64
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_export
    :: Ptr Word8 -> CSize
    -> FunPtr SessionExportCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_import
    :: Ptr Word8 -> CSize
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_mcp_server_restart
    :: Ptr () -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

ha_engine_mcp_server_restart
    :: Ptr () -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_engine_mcp_server_restart pointer expected nameBytes (CSize nameLength)
        callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                accepted <- tryAny do
                    let stable =
                            castPtrToStablePtr pointer :: StablePtr Engine
                    engine <- deRefStablePtr stable
                    atomically $ acceptEngineCommand engine.engineCommands
                        (EngineMcpRestart expected name callback context)
                pure case accepted of
                    Left _ -> 3
                    Right False -> 3
                    Right True -> 0

ha_session_load_around
    :: Ptr Word8 -> CSize -> Int64 -> CInt
    -> FunPtr SessionTurnCallback -> Ptr () -> IO CInt
ha_session_load_around sessionBytes (CSize sessionLength) center radius
        callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | center < 0 || radius < 0 = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        withNativeGatewayBoundary \gatewayIdentity ->
                            validateNativeSessionBoundary
                                pool root gatewayIdentity sessionId >>= \case
                                    Left err -> pure (Left err)
                                    Right _ ->
                                        loadSessionHistoryTurnsAround
                                            pool
                                            root
                                            sessionId
                                            center
                                            (fromIntegral radius) >>= \case
                                                Left err -> pure (Left err)
                                                Right page ->
                                                    pure
                                                        (Right
                                                            ( gatewayIdentity
                                                            , page
                                                            ))
                    case result of
                        Left exception ->
                            sessionTurnFailure callback context
                                (Text.pack (show exception))
                        Right (Left err) ->
                            sessionTurnFailure callback context err
                        Right (Right (gatewayIdentity, page)) ->
                            emitSessionPageForBoundary
                                gatewayIdentity callback context page >>= \case
                                    Left err ->
                                        sessionTurnFailure
                                            callback context err
                                    Right () -> pure ()
                pure 0

ha_session_fork
    :: Ptr Word8 -> CSize -> Int64
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt
ha_session_fork sessionBytes (CSize sessionLength) throughIndex callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 || throughIndex < 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        withNativeSessionBoundary
                            pool root sessionId \gatewayIdentity _ ->
                                forkSessionAtTurn
                                    pool root sessionId throughIndex >>= \case
                                        Left err -> pure (Left err)
                                        Right forkedSessionId ->
                                            pure
                                                (Right
                                                    ( gatewayIdentity
                                                    , forkedSessionId
                                                    ))
                    completeBoundarySessionResult callback context result
                pure 0

ha_session_export
    :: Ptr Word8 -> CSize
    -> FunPtr SessionExportCallback -> Ptr () -> IO CInt
ha_session_export sessionBytes (CSize sessionLength) callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        withNativeGatewayBoundary \gatewayIdentity ->
                            validateNativeSessionBoundary
                                pool root gatewayIdentity sessionId >>= \case
                                    Left err -> pure (Left err)
                                    Right _ ->
                                        streamSessionTransfer
                                            pool
                                            root
                                            sessionId
                                            (emitSessionExportChunkForBoundary
                                                gatewayIdentity
                                                callback
                                                context) >>= \case
                                                    Left err ->
                                                        pure (Left err)
                                                    Right () ->
                                                        pure
                                                            (Right
                                                                gatewayIdentity)
                    case result of
                        Left exception ->
                            sessionExportFailure callback context
                                (Text.pack (show exception))
                        Right (Left err) ->
                            sessionExportFailure callback context err
                        Right (Right gatewayIdentity) ->
                            emitSessionExportTerminalForBoundary
                                gatewayIdentity callback context >>= \case
                                    Left err ->
                                        sessionExportFailure
                                            callback context err
                                    Right () -> pure ()
                pure 0

ha_session_import
    :: Ptr Word8 -> CSize
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt
ha_session_import bytes (CSize length) callback context
    | callback == nullFunPtr = pure 2
    | bytes == nullPtr || length == 0 = pure 2
    | toInteger length > 512 * 1024 * 1024 = pure 2
    | otherwise = do
        payload <- BS.packCStringLen (castPtr bytes, fromIntegral length)
        _ <- forkIO do
            result <- tryAny $
                case TextEncoding.decodeUtf8' payload of
                    Left _ ->
                        pure
                            (Left
                                "invalid session transfer: invalid UTF-8")
                    Right _ ->
                        case
                            (Aeson.eitherDecodeStrict' payload
                                :: Either String SessionTransferEnvelope)
                        of
                            Left err ->
                                pure
                                    (Left
                                        ("invalid session transfer: "
                                            <> Text.pack err))
                            Right envelope -> do
                                let meta =
                                        envelope.transferSession.transferMeta
                                withNativeSessionStore \pool root ->
                                    withNativeGatewayBoundary
                                        \gatewayIdentity ->
                                            case
                                                validateResumedGatewayBoundary
                                                    gatewayIdentity
                                                    meta.metaConnection
                                                    meta.metaGatewayIdentity
                                            of
                                                Left err -> pure (Left err)
                                                Right () ->
                                                    importSessionTransferRemapped
                                                        pool
                                                        root
                                                        Nothing
                                                        envelope >>= \case
                                                            Left err ->
                                                                pure
                                                                    (Left
                                                                        err)
                                                            Right
                                                                importedSessionId ->
                                                                    pure
                                                                        (Right
                                                                            ( gatewayIdentity
                                                                            , importedSessionId
                                                                            ))
            completeBoundarySessionResult callback context result
        pure 0

withNativeSessionStore
    :: (StorePool -> OsPath -> IO (Either Text a))
    -> IO (Either Text a)
withNativeSessionStore action = do
    home <- getHomeDirectory
    config <- managedPostgresConfigForHome home
    openStore config >>= \case
        Left err -> pure (Left (renderStoreError err))
        Right opened ->
            bracket (pure opened) closeStore \store ->
                action (trustedPool store) (sessionsRoot home)

emitSessionTurn
    :: FunPtr SessionTurnCallback
    -> Ptr ()
    -> Int64
    -> SessionTurn
    -> IO ()
emitSessionTurn callback context turnIndex turn =
    withText (Text.pack (show turn.turnAt)) \occurred occurredLength ->
    withText turn.turnUserText \user userLength ->
    withOptionalText turn.turnAssistantText \assistant assistantLength ->
    withOptionalText turn.turnError \turnError turnErrorLength ->
    withOptionalText turn.turnResponseId \response responseLength ->
    withText (transcriptEffectName turn.turnEffect) \effect effectLength ->
    BS.useAsCStringLen
        (LBS.toStrict (Aeson.encode turn.turnItems))
        \(items, itemsLength) -> do
            let (inputTokens', outputTokens', cachedTokens') =
                    maybe (-1, -1, -1)
                        (\usage ->
                            ( fromIntegral usage.inputTokens
                            , fromIntegral usage.outputTokens
                            , fromIntegral usage.cachedTokens
                            ))
                        turn.turnUsage
            invokeSessionTurnCallback callback context 0 turnIndex
                occurred occurredLength
                user userLength
                assistant assistantLength
                turnError turnErrorLength
                response responseLength
                effect effectLength
                items (fromIntegral itemsLength)
                inputTokens' outputTokens' cachedTokens'
                0 0 nullPtr 0

emitSessionPageForBoundary
    :: Maybe Text
    -> FunPtr SessionTurnCallback
    -> Ptr ()
    -> SessionTurnPage
    -> IO (Either Text ())
emitSessionPageForBoundary gatewayIdentity callback context page =
    emitBoundaryChecked
        withGatewayCredentialLease
        (ensureNativeGatewayIdentity gatewayIdentity)
        (\(turnIndex, turn) ->
            emitSessionTurn callback context turnIndex turn)
        (sessionTurnTerminal callback context
            page.pageHasOlder page.pageHasNewer)
        page.pageTurns

sessionTurnTerminal
    :: FunPtr SessionTurnCallback -> Ptr () -> Bool -> Bool -> IO ()
sessionTurnTerminal callback context hasOlder hasNewer =
    invokeSessionTurnCallback callback context 1 (-1)
        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
        (-1) (-1) (-1)
        (if hasOlder then 1 else 0)
        (if hasNewer then 1 else 0)
        nullPtr 0

sessionTurnFailure
    :: FunPtr SessionTurnCallback -> Ptr () -> Text -> IO ()
sessionTurnFailure callback context err =
    withText err \errorPointer errorLength ->
        invokeSessionTurnCallback callback context (-1) (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            nullPtr 0 (-1) (-1) (-1) 0 0 errorPointer errorLength

transcriptEffectName :: TranscriptEffect -> Text
transcriptEffectName = \case
    TranscriptAppend -> "append"
    TranscriptReplace -> "replace"
    TranscriptReset -> "reset"

completeBoundarySessionResult
    :: FunPtr SessionTransferResultCallback
    -> Ptr ()
    -> Either SomeException (Either Text (Maybe Text, Text))
    -> IO ()
completeBoundarySessionResult callback context = \case
    Left exception ->
        emitSessionResult callback context (-1) Nothing
            (Just (Text.pack (show exception)))
    Right (Left err) ->
        emitSessionResult callback context (-1) Nothing (Just err)
    Right (Right (gatewayIdentity, sessionId)) ->
        emitForNativeGatewayBoundary
            gatewayIdentity
            (emitSessionResult callback context 0 (Just sessionId) Nothing)
            >>= \case
                Left err ->
                    emitSessionResult
                        callback context (-1) Nothing (Just err)
                Right () -> pure ()

emitSessionResult
    :: FunPtr SessionTransferResultCallback
    -> Ptr ()
    -> CInt
    -> Maybe Text
    -> Maybe Text
    -> IO ()
emitSessionResult callback context status sessionId err =
    withOptionalText sessionId \sessionPointer sessionLength ->
    withOptionalText err \errorPointer errorLength ->
        invokeSessionTransferResultCallback callback context status
            sessionPointer sessionLength errorPointer errorLength

emitSessionExportChunk
    :: FunPtr SessionExportCallback -> Ptr () -> BS.ByteString -> IO ()
emitSessionExportChunk callback context chunk =
    BS.useAsCStringLen chunk \(pointer, length) ->
        invokeSessionExportCallback callback context 0
            (castPtr pointer) (fromIntegral length) nullPtr 0

emitSessionExportChunkForBoundary
    :: Maybe Text
    -> FunPtr SessionExportCallback
    -> Ptr ()
    -> BS.ByteString
    -> IO ()
emitSessionExportChunkForBoundary
        gatewayIdentity callback context chunk =
    emitForNativeGatewayBoundary
        gatewayIdentity
        (emitSessionExportChunk callback context chunk) >>= \case
            Left _ ->
                throwString
                    "Gateway credentials changed during session export."
            Right () -> pure ()

emitSessionExportTerminalForBoundary
    :: Maybe Text
    -> FunPtr SessionExportCallback
    -> Ptr ()
    -> IO (Either Text ())
emitSessionExportTerminalForBoundary gatewayIdentity callback context =
    emitForNativeGatewayBoundary
        gatewayIdentity
        (invokeSessionExportCallback callback
            context 1 nullPtr 0 nullPtr 0) >>= \case
                Left _ ->
                    pure
                        (Left
                            "Gateway credentials changed during session export.")
                Right () -> pure (Right ())

sessionExportFailure
    :: FunPtr SessionExportCallback -> Ptr () -> Text -> IO ()
sessionExportFailure callback context err =
    withText err \pointer length ->
        invokeSessionExportCallback callback context (-1)
            nullPtr 0 pointer length

foreign export ccall ha_engine_search_conversations
    :: Ptr () -> Ptr Word8 -> CSize -> CSize
    -> FunPtr SearchCallback -> Ptr () -> IO CInt

ha_engine_create :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())
ha_engine_create callback context
    | callback == nullFunPtr = pure nullPtr
    | otherwise = do
        created <- tryAny do
            home <- getHomeDirectory
            config <- managedPostgresConfigForHome home
            commands <- newEngineMailboxIO
            stagedImages <- newTVarIO Map.empty
            browser <- BrowserHost <$> newMVar Nothing
            computer <- newComputerHost
            stagedTurnOptions <- newTVarIO Map.empty
            interactionTarget <- newTVarIO Nothing
            interactionLock <- newMVar ()
            pendingInteractions <- newTVarIO Map.empty
            let interactions = InteractionRuntime
                    { interactionCallbackTarget = interactionTarget
                    , interactionCallbackLock = interactionLock
                    , interactionPending = pendingInteractions
                    }
            mask \_ -> do
                -- Keep worker creation and stable-pointer publication in one
                -- masked region so publication failure cannot orphan it.
                worker <- asyncWithUnmask \unmask ->
                    unmask
                        (workerLifecycle
                            callback
                            context
                            config
                            (sessionsRoot home)
                            commands
                            stagedImages
                            browser
                            computer
                            stagedTurnOptions
                            interactions)
                let engine = Engine
                        { engineCommands = commands
                        , engineWorker = worker
                        , engineStagedImages = stagedImages
                        , engineBrowser = browser
                        , engineComputer = computer
                        , engineStagedTurnOptions = stagedTurnOptions
                        , engineInteractions = interactions
                        }
                stable <- newStablePtr engine `onException` cancel worker
                pure (castStablePtrToPtr stable)
        case created of
            Left exception -> do
                sendEvent callback context $
                    failureEvent "_engine" (Text.pack (show exception))
                pure nullPtr
            Right pointer -> pure pointer

ha_engine_send_json :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_send_json pointer bytes (CSize length)
    | pointer == nullPtr = pure 1
    | bytes == nullPtr && length > 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen
                (castPtr bytes, fromIntegral length)
            case (Aeson.eitherDecodeStrict' payload
                :: Either String BridgeRequest) of
                Left _ -> do
                    atomically do
                        writeTVar engine.engineStagedImages Map.empty
                        writeTVar engine.engineStagedTurnOptions Map.empty
                    pure Nothing
                Right request -> Just <$> atomically
                    (acceptEngineCommand
                        engine.engineCommands
                        (EngineRequest request))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 4
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_set_browser_callback
    :: Ptr () -> FunPtr BrowserCallback -> Ptr () -> IO CInt
ha_engine_set_browser_callback pointer callback context
    | pointer == nullPtr = pure 1
    | otherwise = do
        updated <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            modifyMVar_ engine.engineBrowser.browserRegistration $ \_ ->
                pure
                    if callback == nullFunPtr
                        then Nothing
                        else Just BrowserRegistration
                            { browserCallback = callback
                            , browserContext = context
                            }
        pure $ case updated of
            Left _ -> 2
            Right () -> 0

ha_engine_set_computer_callback
    :: Ptr () -> FunPtr ComputerCallback -> Ptr () -> IO CInt
ha_engine_set_computer_callback pointer callback context
    | pointer == nullPtr = pure 1
    | otherwise = do
        updated <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            modifyMVar_ engine.engineComputer.computerRegistration $ \_ ->
                pure
                    if callback == nullFunPtr
                        then Nothing
                        else Just ComputerRegistration
                            { computerCallback = callback
                            , computerContext = context
                            }
        pure $ case updated of
            Left _ -> 2
            Right () -> 0

ha_engine_search_conversations
    :: Ptr () -> Ptr Word8 -> CSize -> CSize
    -> FunPtr SearchCallback -> Ptr () -> IO CInt
ha_engine_search_conversations pointer bytes (CSize length) rawLimit callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | bytes == nullPtr || length == 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen (castPtr bytes, fromIntegral length)
            case TextEncoding.decodeUtf8' payload of
                Left _ -> pure Nothing
                Right query
                    | Text.null (Text.strip query) -> pure Nothing
                    | otherwise -> do
                        let requested = fromIntegral rawLimit :: Integer
                            limit = fromInteger (max 1 (min 100 requested))
                        Just <$> atomically
                            (acceptEngineCommand engine.engineCommands
                                (EngineSearch
                                    query limit callback context))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 2
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt
ha_engine_stage_turn_images pointer turnID turnIDLength imagePointer imageCount
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | imagePointer == nullPtr && imageCount > 0 = pure 4
    | toInteger imageCount > toInteger (maxBound :: Int) = pure 4
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            turnIDBytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            let turnIDText = TextEncoding.decodeUtf8' turnIDBytes
            imageResults <- mapM peekImage
                [0 .. fromIntegral imageCount - 1]
            case (turnIDText, sequence imageResults) of
                (Right turnIDValue, Just images) -> do
                    atomically $ modifyTVar' engine.engineStagedImages $
                        if null images
                            then Map.delete turnIDValue
                            else Map.insert turnIDValue images
                    pure True
                _ -> pure False
        pure $ case accepted of
            Left _ -> 3
            Right False -> 4
            Right True -> 0
  where
    pointerSize = sizeOf (nullPtr :: Ptr ())
    sizeSize = sizeOf (undefined :: CSize)
    imageSize = pointerSize + sizeSize + pointerSize + sizeSize

    peekImage index = do
        let base = castPtr imagePointer `plusPtr` (index * imageSize)
            readPointer offset =
                peekByteOff base offset :: IO (Ptr Word8)
            readLength offset =
                peekByteOff base offset :: IO CSize
        mimePointer <- readPointer 0
        mimeLength <- readLength pointerSize
        bytesPointer <- readPointer (pointerSize + sizeSize)
        bytesLength <- readLength (pointerSize + sizeSize + pointerSize)
        if
            (mimePointer == nullPtr && mimeLength > 0)
                || (bytesPointer == nullPtr && bytesLength > 0)
                || mimeLength == 0
                || bytesLength == 0
        then pure Nothing
        else do
            mimeBytes <- BS.packCStringLen
                (castPtr mimePointer, fromIntegral mimeLength)
            let mime = TextEncoding.decodeUtf8' mimeBytes
            bytes <- BS.packCStringLen
                (castPtr bytesPointer, fromIntegral bytesLength)
            pure $ case mime of
                Left _ -> Nothing
                Right mimeValue -> Just ImageAttachment
                    { imageMime = mimeValue
                    , imageBytes = bytes
                    }

ha_engine_session_rename
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_rename engine idBytes (CSize idLength)
    titleBytes (CSize titleLength) callback context
    | anyNonEmptyNull
        [ (idBytes, idLength), (titleBytes, titleLength) ] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        title <- decodeInput titleBytes titleLength
        enqueueSessionMutation engine (SessionRename sessionId title) callback context

ha_engine_session_delete
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_delete engine idBytes (CSize idLength) callback context
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        enqueueSessionMutation engine (SessionDelete sessionId) callback context

ha_engine_session_archive
    :: Ptr () -> Ptr Word8 -> CSize -> CInt
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_archive engine idBytes (CSize idLength)
    archived callback context
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        enqueueSessionMutation
            engine (SessionArchive sessionId (archived /= 0)) callback context

enqueueSessionMutation
    :: Ptr ()
    -> SessionMutation
    -> FunPtr SessionResultCallback
    -> Ptr ()
    -> IO CInt
enqueueSessionMutation pointer mutation callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand engine.engineCommands
                (EngineSessionMutation mutation callback context)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0

ha_engine_cancel_task
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_cancel_task pointer taskID (CSize taskIDLength)
    | pointer == nullPtr = pure 1
    | taskID == nullPtr || taskIDLength == 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            taskIDBytes <- BS.packCStringLen
                (castPtr taskID, fromIntegral taskIDLength)
            case TextEncoding.decodeUtf8' taskIDBytes of
                Left _ -> pure Nothing
                Right taskIDText
                    | Text.null taskIDText -> pure Nothing
                    | otherwise -> Just <$> atomically
                        (acceptEngineCommand
                            engine.engineCommands
                            (EngineCancelTask taskIDText))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 2
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_list_tasks
    :: Ptr () -> FunPtr TaskSnapshotCallback -> Ptr () -> IO CInt
ha_engine_list_tasks pointer callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand
                engine.engineCommands
                (EngineTaskSnapshot callback context)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0

ha_engine_set_task_limit :: Ptr () -> CSize -> IO CInt
ha_engine_set_task_limit pointer rawLimit
    | pointer == nullPtr = pure 1
    | limit < 1 || limit > 32 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand
                engine.engineCommands
                (EngineSetTaskLimit limit)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0
  where
    limit = fromIntegral rawLimit

ha_engine_stage_turn_options
    :: Ptr () -> Ptr Word8 -> CSize -> CInt -> CInt -> IO CInt
ha_engine_stage_turn_options pointer turnID turnIDLength rawMode rawShell
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | otherwise =
        case (interactionModeFromCode rawMode, shellModeFromCode rawShell) of
            (Just interactionMode, Just shellMode) -> do
                accepted <- tryAny do
                    let stable =
                            castPtrToStablePtr pointer :: StablePtr Engine
                    engine <- deRefStablePtr stable
                    bytes <- BS.packCStringLen
                        (castPtr turnID, fromIntegral turnIDLength)
                    case TextEncoding.decodeUtf8' bytes of
                        Left _ -> pure False
                        Right turnIDText
                            | Text.null turnIDText -> pure False
                            | otherwise -> do
                                atomically $ modifyTVar'
                                    engine.engineStagedTurnOptions
                                    (Map.insert
                                        turnIDText
                                        NativeTurnOptions
                                            { nativeTurnInteractionMode =
                                                interactionMode
                                            , nativeTurnShellMode = shellMode
                                            })
                                pure True
                pure $ case accepted of
                    Left _ -> 3
                    Right False -> 2
                    Right True -> 0
            _ -> pure 4

ha_engine_discard_turn_staging
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_discard_turn_staging pointer turnID turnIDLength
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | otherwise = do
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            bytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            case TextEncoding.decodeUtf8' bytes of
                Left _ -> pure False
                Right turnIDText
                    | Text.null turnIDText -> pure False
                    | otherwise -> do
                        atomically $ discardStagedTurnById
                            turnIDText
                            engine.engineStagedImages
                            engine.engineStagedTurnOptions
                        pure True
        pure $ case result of
            Left _ -> 3
            Right False -> 2
            Right True -> 0

maxNativeTurnIDBytes :: Integer
maxNativeTurnIDBytes = 1_024

validNativeTurnIDLength :: CSize -> Bool
validNativeTurnIDLength length =
    let integerLength = toInteger length
    in integerLength > 0
        && integerLength <= toInteger (maxBound :: Int)
        && integerLength <= maxNativeTurnIDBytes

ha_engine_set_interaction_callback
    :: Ptr () -> FunPtr InteractionCallback -> Ptr () -> IO CInt
ha_engine_set_interaction_callback pointer callback callbackContext
    | pointer == nullPtr = pure 1
    | otherwise = do
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            withMVar
                engine.engineInteractions.interactionCallbackLock
                \_ -> atomically do
                    writeTVar
                        engine.engineInteractions.interactionCallbackTarget
                        (if callback == nullFunPtr
                            then Nothing
                            else Just InteractionCallbackTarget
                                { interactionTargetCallback = callback
                                , interactionTargetContext = callbackContext
                                })
                    cancelPendingInteractions
                        engine.engineInteractions.interactionPending
        pure $ either (const 3) (const 0) result

ha_engine_resolve_interaction
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> Ptr Word8 -> CSize -> IO CInt
ha_engine_resolve_interaction
        pointer
        turnID
        (CSize turnIDLength)
        interactionID
        (CSize interactionIDLength)
        (CInt selectedIndex)
        customText
        (CSize customTextLength)
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || turnIDLength == 0 = pure 2
    | interactionID == nullPtr || interactionIDLength == 0 = pure 2
    | customText == nullPtr && customTextLength > 0 = pure 2
    | otherwise = do
        let selectedIndexValue = fromIntegral selectedIndex :: Int
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            let pendingRef =
                    engine.engineInteractions.interactionPending
            turnBytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            interactionBytes <- BS.packCStringLen
                (castPtr interactionID, fromIntegral interactionIDLength)
            customBytes <-
                if customTextLength == 0
                    then pure (Right Nothing)
                    else fmap (fmap Just . TextEncoding.decodeUtf8')
                        (BS.packCStringLen
                            (castPtr customText, fromIntegral customTextLength))
            case
                ( TextEncoding.decodeUtf8' turnBytes
                , TextEncoding.decodeUtf8' interactionBytes
                , customBytes
                )
              of
                (Right turnIDText, Right interactionIDText, Right custom) ->
                    fmap (\published -> if published then 0 else 4) $
                        atomically $
                            resolvePendingInteraction
                                pendingRef
                                (turnIDText, interactionIDText)
                                NativeInteractionResolution
                                    { interactionSelectedIndex =
                                        selectedIndexValue
                                    , interactionCustomText = custom
                                    }
                _ -> pure 2
        pure $ case result of
            Left _ -> 3
            Right status -> status

interactionModeFromCode :: CInt -> Maybe NativeInteractionMode
interactionModeFromCode = \case
    0 -> Just NativeAsk
    1 -> Just NativePlan
    2 -> Just NativeYolo
    _ -> Nothing

shellModeFromCode :: CInt -> Maybe NativeShellMode
shellModeFromCode = \case
    0 -> Just NativeShellNone
    1 -> Just NativeShellBash
    2 -> Just NativeShellGhci
    3 -> Just NativeShellBoth
    _ -> Nothing

ha_engine_destroy :: Ptr () -> IO ()
ha_engine_destroy pointer
    | pointer == nullPtr = pure ()
    | otherwise = void $ tryAny do
        let stable = castPtrToStablePtr pointer :: StablePtr Engine
        (do
            engine <- deRefStablePtr stable
            modifyMVar_
                engine.engineComputer.computerRegistration
                (const (pure Nothing))
            _ <- atomically
                (closeEngineMailbox engine.engineCommands EngineStop)
            void (waitCatch engine.engineWorker))
            `finally` freeStablePtr stable

workerLifecycle
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> OsPath
    -> EngineMailbox EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> BrowserHost
    -> ComputerHost
    -> TVar (Map Text NativeTurnOptions)
    -> InteractionRuntime
    -> IO ()
workerLifecycle
        callback context config root commands stagedImages browser computer
        stagedTurnOptions interactions =
    (do
        store <- newMVar Nothing
        processRuntime <- newNativeProcessRuntime root
        workerRegistry <- newTVarIO Map.empty
        let cleanup =
                shutdownRunningTurns workerRegistry
                    `finally` closeNativeProcessRuntime processRuntime
                    `finally` closeEngineStore store
        supervisorLoop
            callback
            context
            config
            store
            root
            processRuntime
            commands
            stagedImages
            browser
            computer
            stagedTurnOptions
            interactions
            workerRegistry
            TaskSupervisor
                { supervisorLimit = defaultTaskLimit
                , supervisorPending = Seq.empty
                , supervisorRunning = Map.empty
                , supervisorKnownTaskIds = Set.empty
                }
            `finally` cleanup)
        `finally`
            (atomically $
                cancelPendingInteractions interactions.interactionPending)
        `finally` cancelPendingMcpRestarts commands

cancelPendingMcpRestarts :: EngineMailbox EngineCommand -> IO ()
cancelPendingMcpRestarts commands = do
    pending <- atomically do
        _ <- closeEngineMailbox commands EngineStop
        drainEngineCommands commands
    forM_ pending \case
        EngineMcpRestart expected _ callback context ->
            void $ tryAny $
                withText "engine stopped before MCP restart completed" $
                    invokeMcpResultCallback callback context (-1) expected
        _ -> pure ()

supervisorLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> NativeProcessRuntime
    -> EngineMailbox EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> BrowserHost
    -> ComputerHost
    -> TVar (Map Text NativeTurnOptions)
    -> InteractionRuntime
    -> TVar (Map Text RunningTurn)
    -> TaskSupervisor
    -> IO ()
supervisorLoop
        callback context config store root processRuntime commands stagedImages
        browser computer
        stagedTurnOptions interactions workerRegistry =
    go
  where
    go supervisor0 = do
        supervisor <- startRunnableTasks supervisor0
        atomically (readEngineCommand commands) >>= handleCommand supervisor

    handleCommand supervisor = \case
        EngineStop ->
            shutdownSupervisor supervisor
        EngineSearch query limit searchCallback searchContext -> do
            GatewayBoundary.withCurrentGatewayBoundary
                (\boundary ->
                    runConversationSearch
                        config
                        store
                        boundary.gatewayBoundaryIdentity
                        query
                        limit
                        searchCallback
                        searchContext) >>= \case
                Left err ->
                    sendSearchFailure searchCallback searchContext
                        (GatewayBoundary.renderGatewayBoundaryError err)
                Right () -> pure ()
            go supervisor
        EngineSessionMutation mutation resultCallback resultContext -> do
            runSessionMutation
                config store root mutation resultCallback resultContext
            go supervisor
        EngineMcpRestart expected name resultCallback resultContext -> do
            if Map.null supervisor.supervisorRunning
                then do
                    restarted <- tryAny do
                        home <- getHomeDirectory
                        mcpAdminTry
                            (restartMcpAdminServer home expected name
                                (restartNativeMcpRuntime processRuntime))
                    case restarted of
                        Left exception ->
                            withText (Text.pack (show exception)) $
                                invokeMcpResultCallback resultCallback
                                    resultContext (-1) expected
                        Right (Left err) ->
                            emitMcpResult resultCallback resultContext
                                (Left err
                                    :: Either
                                        McpAdminError
                                        (McpAdminSnapshot ()))
                        Right (Right snapshot) ->
                            invokeMcpResultCallback
                                resultCallback
                                resultContext
                                0
                                snapshot.mcpAdminRevision
                                nullPtr
                                0
                else
                    withText "cannot restart MCP while tasks are active" $
                        invokeMcpResultCallback resultCallback resultContext
                            (-1) expected
            go supervisor
        EngineCancelTask taskId -> do
            next <- cancelTaskById supervisor taskId
            go next
        EngineTaskSnapshot snapshotCallback snapshotContext -> do
            sendTaskSnapshot snapshotCallback snapshotContext supervisor
            go supervisor
        EngineSetTaskLimit limit ->
            go supervisor { supervisorLimit = limit }
        EngineTaskSession taskId sessionId -> do
            case Map.lookup taskId supervisor.supervisorRunning of
                Nothing -> pure ()
                Just running -> atomically $
                    writeTVar
                        running.runningTurnControl.turnControlSessionId
                        (Just sessionId)
            go supervisor
        EngineTaskFinished taskId outcome -> do
            case Map.lookup taskId supervisor.supervisorRunning of
                Nothing -> pure ()
                Just running -> do
                    _ <- waitCatch running.runningTurnWorker
                    sessionId <- readTVarIO
                        running.runningTurnControl.turnControlSessionId
                    cancelled <- readTVarIO
                        running.runningTurnControl.turnControlCancelled
                    _ <- emitForNativeGatewayBoundary
                        running.runningTurnControl.turnControlGatewayIdentity $
                            if cancelled
                                then do
                                    sendTaskState
                                        taskId sessionId "cancelled"
                                    sendEvent callback context $
                                        turnFailedEvent
                                            taskId
                                            "turn cancelled"
                                else do
                                    sendTaskState
                                        taskId
                                        (taskResultSessionId
                                            sessionId
                                            outcome)
                                        (taskResultState outcome)
                                    finishTurnEvent
                                        callback context taskId outcome
                    pure ()
            atomically $ modifyTVar' workerRegistry (Map.delete taskId)
            go supervisor
                { supervisorRunning =
                    Map.delete taskId supervisor.supervisorRunning
                }
        EngineRequest request ->
            handleEngineRequest supervisor request >>= go

    handleEngineRequest supervisor request
        | request.requestMethod == "turn.start" =
            enqueueTurn supervisor request
        | request.requestMethod == "turn.cancel" =
            case (parseParams request :: Either Text TurnReference) of
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right reference -> do
                    let active =
                            Map.member
                                reference.turnReferenceId
                                supervisor.supervisorRunning
                        queued = any
                            ((== reference.turnReferenceId)
                                . (.turnStartId)
                                . (.pendingTurnStart))
                            supervisor.supervisorPending
                    next <- cancelTaskById
                        supervisor
                        reference.turnReferenceId
                    sendEvent callback context $
                        if active || queued
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "turn id is not active"
                    pure next
        | request.requestMethod == "approval.resolve" =
            case (parseParams request :: Either Text ApprovalResolution) of
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right resolution -> do
                    matching <- filterM
                        (approvalIsActive resolution.approvalResolutionId
                            . (.runningTurnControl))
                        (Map.elems supervisor.supervisorRunning)
                    case matching of
                        [running] ->
                            resolveApproval running.runningTurnControl request
                                >>= sendEvent callback context
                        _ ->
                            sendEvent callback context $
                                failureEvent
                                    request.requestId
                                    "approval request is no longer active"
                    pure supervisor
        | request.requestMethod == "turn.agents" =
            selectRunningTurn request.requestParams supervisor >>= \case
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right Nothing -> do
                    sendEvent callback context $
                        successEvent request.requestId ([] :: [Aeson.Value])
                    pure supervisor
                Right (Just running) ->
                    do
                        _ <- emitForNativeGatewayBoundary
                            running.runningTurnControl.turnControlGatewayIdentity
                            (activeAgentSnapshot
                                running.runningTurnControl
                                request
                                >>= sendEvent callback context)
                        pure supervisor
        | otherwise = do
            let respond = do
                    event <- handleRequest config store root request
                    sendEvent callback context event
            if nativeRequestRequiresGatewayLock request.requestMethod
                then withGatewayCredentialLease respond
                else respond
            pure supervisor

    selectRunningTurn params supervisor =
        case Aeson.parseEither
            (Aeson.withObject "turn reference" (.:? "turnId"))
            params of
            Left err -> pure (Left (Text.pack err))
            Right (Just taskId) ->
                pure $ maybe
                    (Left "turn id is not active")
                    (Right . Just)
                    (Map.lookup taskId supervisor.supervisorRunning)
            Right Nothing ->
                pure $ case Map.elems supervisor.supervisorRunning of
                    [running] -> Right (Just running)
                    [] -> Right Nothing
                    _ -> Left "turnId is required while multiple turns run"

    approvalIsActive approvalId control =
        Map.member approvalId
            <$> readTVarIO control.turnControlApprovals

    enqueueTurn supervisor request =
        case (parseParams request :: Either Text TurnStart) of
            Left err -> do
                atomically $ discardStagedTurn
                    request.requestId
                    request.requestParams
                    stagedImages
                    stagedTurnOptions
                sendEvent callback context
                    (failureEvent request.requestId err)
                pure supervisor
            Right start
                | taskExists start.turnStartId supervisor -> do
                    atomically $ discardStagedTurnById
                        start.turnStartId
                        stagedImages
                        stagedTurnOptions
                    sendEvent callback context $
                        failureEvent request.requestId "turn id already exists"
                    pure supervisor
                | otherwise ->
                    withGatewayCredentialLease $
                        loadNativeGatewayIdentity >>= \case
                            Left err -> do
                                atomically $ discardStagedTurnById
                                    start.turnStartId
                                    stagedImages
                                    stagedTurnOptions
                                sendEvent callback context $
                                    failureEvent request.requestId err
                                pure supervisor
                            Right gatewayIdentity -> do
                                (images, turnOptions) <- atomically $ do
                                    staged <- readTVar stagedImages
                                    writeTVar stagedImages
                                        (Map.delete start.turnStartId staged)
                                    options <- readTVar stagedTurnOptions
                                    writeTVar stagedTurnOptions
                                        (Map.delete start.turnStartId options)
                                    pure
                                        ( Map.findWithDefault
                                            []
                                            start.turnStartId
                                            staged
                                        , Map.findWithDefault
                                            defaultNativeTurnOptions
                                            start.turnStartId
                                            options
                                        )
                                sendEvent callback context $
                                    successEvent request.requestId $
                                        Aeson.object
                                            [ "turnId" Aeson..=
                                                start.turnStartId
                                            , "state" Aeson..=
                                                ("queued" :: Text)
                                            ]
                                sendTaskState
                                    start.turnStartId
                                    start.turnStartSessionId
                                    "queued"
                                pure supervisor
                                    { supervisorPending =
                                        supervisor.supervisorPending
                                            Seq.|> PendingTurn
                                                { pendingTurnStart = start
                                                , pendingTurnGatewayIdentity =
                                                    gatewayIdentity
                                                , pendingTurnImages = images
                                                , pendingTurnOptions =
                                                    turnOptions
                                                }
                                    , supervisorKnownTaskIds =
                                        Set.insert
                                            start.turnStartId
                                            supervisor.supervisorKnownTaskIds
                                    }

    startRunnableTasks supervisor = do
        sessionIds <- activeSessionIds supervisor
        let available =
                supervisor.supervisorLimit
                    - Map.size supervisor.supervisorRunning
            pending = toList supervisor.supervisorPending
            candidates =
                [ ( TaskIdentity
                        pending.pendingTurnStart.turnStartId
                        pending.pendingTurnStart.turnStartSessionId
                  , pending
                  )
                | pending <- pending
                ]
            (selected, remaining) =
                selectRunnableTasks available sessionIds candidates
        running <- foldM
            startTask
            supervisor.supervisorRunning
            (map snd selected)
        pure supervisor
            { supervisorPending = Seq.fromList (map snd remaining)
            , supervisorRunning = running
            }

    startTask
        :: Map Text RunningTurn
        -> PendingTurn
        -> IO (Map Text RunningTurn)
    startTask running pending = do
        let start = pending.pendingTurnStart
        control <- newTurnControl
            start.turnStartId
            pending.pendingTurnGatewayIdentity
            start.turnStartSessionId
            interactions
        nativeBrowserTools <- browserToolsWhenEnabled browser
        nativeComputerTool <- computerToolWhenEnabled computer
        worker <- launchTrackedWorker start.turnStartId do
            withGatewayCredentialTurnLease $
                ensureNativeGatewayIdentity
                    pending.pendingTurnGatewayIdentity >>= \case
                        Left err ->
                            pure
                                TurnOutcome
                                    { turnOutcomeSessionId =
                                        start.turnStartSessionId
                                    , turnOutcomeError = Just err
                                    , turnOutcomeUsage = emptyTokenUsage
                                    , turnOutcomeProviderCostUSD = Nothing
                                    }
                        Right () ->
                            do
                                sendTaskState
                                    start.turnStartId
                                    start.turnStartSessionId
                                    "running"
                                sendTurnStatus
                                    callback
                                    context
                                    start.turnStartId
                                    (if start.turnStartWorktree
                                        then "Creating worktree…"
                                        else "Starting…")
                                runNativeTurn
                                    callback
                                    context
                                    commands
                                    processRuntime
                                    control
                                    nativeBrowserTools
                                    nativeComputerTool
                                    start
                                    pending.pendingTurnImages
                                    pending.pendingTurnOptions
                                    interactions
        let runningTurn =
                RunningTurn
                    { runningTurnControl = control
                    , runningTurnWorker = worker
                    }
        atomically $ modifyTVar' workerRegistry $
            Map.insert start.turnStartId runningTurn
        pure $ Map.insert
            start.turnStartId
            runningTurn
            running

    launchTrackedWorker taskId action =
        mask \_ -> do
            gate <- newEmptyMVar
            worker <- asyncWithUnmask \unmask -> do
                takeMVar gate
                outcome <- newIORef (TaskFailure "turn cancelled")
                (tryAny (unmask action) >>= \case
                    Left exception ->
                        writeIORef outcome
                            (TaskFailure (Text.pack (show exception)))
                    Right value ->
                        writeIORef outcome (TaskOutcome value))
                    `finally` do
                        result <- readIORef outcome
                        void $ atomically $ acceptEngineCommand
                            commands
                            (EngineTaskFinished taskId result)
            putMVar gate ()
            pure worker

    activeSessionIds supervisor = do
        sessions <- mapM
            (readTVarIO . (.turnControlSessionId) . (.runningTurnControl))
            (Map.elems supervisor.supervisorRunning)
        pure (Set.fromList [session | Just session <- sessions])

    cancelTaskById supervisor taskId =
        case Map.lookup taskId supervisor.supervisorRunning of
            Just running -> do
                cancelTurn running.runningTurnControl
                cancel running.runningTurnWorker
                pure supervisor
            Nothing -> do
                let (cancelled, retained) = partition
                        ((== taskId)
                            . (.turnStartId)
                            . (.pendingTurnStart))
                        (toList supervisor.supervisorPending)
                forM_ cancelled \pending ->
                    let start = pending.pendingTurnStart
                    in do
                        _ <- emitForNativeGatewayBoundary
                            pending.pendingTurnGatewayIdentity do
                                sendTaskState
                                    start.turnStartId
                                    start.turnStartSessionId
                                    "cancelled"
                                sendEvent callback context $
                                    turnFailedEvent
                                        start.turnStartId
                                        "turn cancelled"
                        pure ()
                pure supervisor
                    { supervisorPending = Seq.fromList retained }

    taskExists taskId supervisor =
        Set.member taskId supervisor.supervisorKnownTaskIds

    shutdownSupervisor _ =
        shutdownRunningTurns workerRegistry

    sendTaskState :: Text -> Maybe Text -> Text -> IO ()
    sendTaskState taskId sessionId state =
        sendEvent callback context $
            Aeson.object
                [ "event" Aeson..= ("task.state" :: Text)
                , "taskId" Aeson..= taskId
                , "sessionId" Aeson..= sessionId
                , "state" Aeson..= state
                ]

    sendTaskSnapshot snapshotCallback snapshotContext supervisor =
        withGatewayCredentialLease $
            loadNativeGatewayIdentity >>= \case
                Left err ->
                    withTextBytes err \errorPointer errorLength ->
                        invokeTaskSnapshotCallback
                            snapshotCallback
                            snapshotContext
                            (-1)
                            nullPtr 0 nullPtr 0 0
                            errorPointer errorLength
                Right gatewayIdentity -> do
                    forM_ supervisor.supervisorPending \pending ->
                        when
                            (nativeTurnRouteMatchesBoundary
                                pending.pendingTurnGatewayIdentity
                                gatewayIdentity) $
                                sendSnapshotItem
                                    snapshotCallback
                                    snapshotContext
                                    pending.pendingTurnStart.turnStartId
                                    pending.pendingTurnStart.turnStartSessionId
                                    0
                    forM_
                        (filter
                            (\running ->
                                let control = running.runningTurnControl
                                in nativeTurnRouteMatchesBoundary
                                    control.turnControlGatewayIdentity
                                    gatewayIdentity)
                            (Map.elems supervisor.supervisorRunning))
                        \running -> do
                            sessionId <- readTVarIO
                                running.runningTurnControl.turnControlSessionId
                            sendSnapshotItem
                                snapshotCallback
                                snapshotContext
                                running.runningTurnControl.turnControlId
                                sessionId
                                1
                    invokeTaskSnapshotCallback
                        snapshotCallback snapshotContext
                        1 nullPtr 0 nullPtr 0 0 nullPtr 0

    sendSnapshotItem snapshotCallback snapshotContext taskId sessionId state =
        withTextBytes taskId \taskPointer taskLength ->
        withMaybeTextBytes sessionId \sessionPointer sessionLength ->
            invokeTaskSnapshotCallback snapshotCallback snapshotContext
                0 taskPointer taskLength sessionPointer sessionLength
                state nullPtr 0

shutdownRunningTurns :: TVar (Map Text RunningTurn) -> IO ()
shutdownRunningTurns workerRegistry = do
    running <- atomically do
        current <- readTVar workerRegistry
        writeTVar workerRegistry Map.empty
        pure (Map.elems current)
    forM_ running (cancelTurn . (.runningTurnControl))
    mapM_ (cancel . (.runningTurnWorker)) running
    mapM_ (waitCatch . (.runningTurnWorker)) running

runSessionMutation
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> SessionMutation
    -> FunPtr SessionResultCallback
    -> Ptr ()
    -> IO ()
runSessionMutation config store root mutation callback context = do
    outcome <- tryAny do
        activeStore <- acquireStore config store
        let pool = trustedPool activeStore
            sessionId = case mutation of
                SessionRename identifier _ -> identifier
                SessionDelete identifier -> identifier
                SessionArchive identifier _ -> identifier
        withNativeSessionBoundary
            pool root sessionId \gatewayIdentity _ ->
                case mutation of
                    SessionRename _ title -> do
                        result <- renameSession pool root sessionId title
                        pure (gatewayIdentity <$ result)
                    SessionDelete _ -> do
                        result <- deleteSession pool root sessionId
                        pure (gatewayIdentity <$ result)
                    SessionArchive _ archived -> do
                        result <-
                            setSessionArchived
                                pool root sessionId archived
                        pure (gatewayIdentity <$ result)
    case outcome of
        Left exception ->
            sendSessionMutationFailure
                callback context (Text.pack (show exception))
        Right (Left err) ->
            sendSessionMutationFailure callback context err
        Right (Right gatewayIdentity) ->
            emitForNativeGatewayBoundary gatewayIdentity
                (invokeSessionResultCallback callback context 0 nullPtr 0)
                >>= \case
                    Left err ->
                        sendSessionMutationFailure callback context err
                    Right () -> pure ()

sendSessionMutationFailure
    :: FunPtr SessionResultCallback -> Ptr () -> Text -> IO ()
sendSessionMutationFailure callback context message =
    withText message \errorPtr errorLength ->
        invokeSessionResultCallback callback context (-1) errorPtr errorLength

runConversationSearch
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> Maybe Text
    -> Text
    -> Int
    -> FunPtr SearchCallback
    -> Ptr ()
    -> IO ()
runConversationSearch
        config store gatewayIdentity query limit callback context = do
    outcome <- tryAny do
        activeStore <- acquireStore config store
        searchNativeConversationsForBoundary
            (trustedPool activeStore)
            organizationGatewayConnectionId
            gatewayIdentity
            query
            limit
    case outcome of
        Left exception ->
            sendSearchFailure callback context (Text.pack (show exception))
        Right (Left err) ->
            sendSearchFailure callback context (renderStoreError err)
        Right (Right results) ->
            emitSearchResultsForBoundary
                gatewayIdentity callback context results >>= \case
                    Left err -> sendSearchFailure callback context err
                    Right () -> pure ()

emitSearchResultsForBoundary
    :: Maybe Text
    -> FunPtr SearchCallback
    -> Ptr ()
    -> [NativeConversationSearchResult]
    -> IO (Either Text ())
emitSearchResultsForBoundary gatewayIdentity callback context =
    emitBoundaryChecked
        withGatewayCredentialLease
        (ensureNativeGatewayIdentity gatewayIdentity)
        (sendSearchResult callback context)
        (invokeSearchCallback callback context
            1 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 (-1) 0 0 nullPtr 0 nullPtr 0 0 nullPtr 0)

sendSearchFailure :: FunPtr SearchCallback -> Ptr () -> Text -> IO ()
sendSearchFailure callback context message =
    withTextBytes message \errorPointer errorLength ->
        invokeSearchCallback callback context
            (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 (-1) 0 0 nullPtr 0 nullPtr 0 0
            errorPointer errorLength

sendSearchResult
    :: FunPtr SearchCallback
    -> Ptr ()
    -> NativeConversationSearchResult
    -> IO ()
sendSearchResult callback context result =
    withTextBytes result.nativeSearchSessionId \sessionPointer sessionLength ->
    withTextBytes result.nativeSearchTitle \titlePointer titleLength ->
    withTextBytes result.nativeSearchCwd \cwdPointer cwdLength ->
    withTextBytes result.nativeSearchProvider \providerPointer providerLength ->
    withTextBytes result.nativeSearchModel \modelPointer modelLength ->
    withMaybeTextBytes result.nativeSearchUserText \userPointer userLength ->
    withMaybeTextBytes result.nativeSearchAssistantText
        \assistantPointer assistantLength ->
            invokeSearchCallback callback context
                0
                sessionPointer sessionLength
                titlePointer titleLength
                cwdPointer cwdLength
                providerPointer providerLength
                modelPointer modelLength
                (epochMilliseconds result.nativeSearchUpdatedAt)
                (if result.nativeSearchArchived then 1 else 0)
                (fromMaybe (-1) result.nativeSearchTurnIndex)
                (maybe 0 epochMilliseconds result.nativeSearchOccurredAt)
                (searchRoleCode result.nativeSearchRole)
                userPointer userLength
                assistantPointer assistantLength
                (realToFrac result.nativeSearchRank)
                nullPtr 0

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes value action =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
        action (castPtr pointer) (fromIntegral length)

withMaybeTextBytes
    :: Maybe Text
    -> (Ptr Word8 -> CSize -> IO a)
    -> IO a
withMaybeTextBytes Nothing action = action nullPtr 0
withMaybeTextBytes (Just value) action = withTextBytes value action

epochMilliseconds :: UTCTime -> Int64
epochMilliseconds =
    floor . (* 1000) . utcTimeToPOSIXSeconds

searchRoleCode :: Maybe Text -> CInt
searchRoleCode = \case
    Just "user" -> 1
    Just "assistant" -> 2
    _ -> 0

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

nativeTurnArguments :: TurnStart -> [String]
nativeTurnArguments start =
    [ "--minimal"
    , "--motion", "off"
    , "--save-session"
    , "--no-yolo"
    ]
        <> maybe
            []
            (\sessionId -> ["--resume", Text.unpack sessionId])
            start.turnStartSessionId
        <> (if start.turnStartWorktree then ["--worktree"] else [])
        <> (if start.turnStartComputerUse
            then ["--computer-use"]
            else ["--no-computer-use"])
        <> modelArgs
        <> maybe
            []
            (\effort -> ["--effort", Text.unpack effort])
            start.turnStartEffort
  where
    modelArgs = case
        (start.turnStartProvider, start.turnStartModel) of
            (Just provider, Just model) ->
                [ "--provider", Text.unpack provider
                , "--model", Text.unpack model
                ]
            _ -> []

withTurnImages
    :: Text
    -> [ImageAttachment]
    -> (Maybe FilePath -> IO a)
    -> IO a
withTurnImages _ [] action = action Nothing
withTurnImages prompt images action = do
    temporaryDirectory <- getTemporaryDirectory
    withImageFiles temporaryDirectory images \paths -> do
        let request = managedTurnRequestWithImages prompt
                [ ManagedTurnMedia
                    { managedTurnMediaPath = path
                    , managedTurnMediaMime = image.imageMime
                    , managedTurnMediaName = Nothing
                    }
                | (path, image) <- zip paths images
                ]
        bracket
            (openBinaryTempFile temporaryDirectory "ha-native-turn-")
            (\(path, handle) -> do
                hClose handle
                void (tryAny (removeFile path)))
            \(path, handle) -> do
                hClose handle
                BS.writeFile path
                    (TextEncoding.encodeUtf8 (renderManagedTurnPrompt request))
                action (Just path)
  where
    withImageFiles _ [] action = action []
    withImageFiles directory (image : rest) action =
        bracket
            (openBinaryTempFile directory "ha-native-image-")
            (\(path, handle) -> do
                hClose handle
                void (tryAny (removeFile path)))
            \(path, handle) -> do
                hClose handle
                BS.writeFile path image.imageBytes
                withImageFiles directory rest (action . (path :))

newTurnControl
    :: Text
    -> Maybe Text
    -> Maybe Text
    -> InteractionRuntime
    -> IO TurnControl
newTurnControl turnId gatewayIdentity sessionId interactions = do
    sessionIdRef <- newTVarIO sessionId
    cancelled <- newTVarIO False
    cancelAction <- newTVarIO (pure ())
    approvals <- newTVarIO Map.empty
    approvalCounter <- newTVarIO 0
    interactionCounter <- newTVarIO 0
    allowedTools <- newTVarIO Set.empty
    agentSnapshot <- newTVarIO (pure [])
    pure TurnControl
        { turnControlId = turnId
        , turnControlGatewayIdentity = gatewayIdentity
        , turnControlSessionId = sessionIdRef
        , turnControlCancelled = cancelled
        , turnControlCancel = cancelAction
        , turnControlApprovals = approvals
        , turnControlApprovalCounter = approvalCounter
        , turnControlInteractionCounter = interactionCounter
        , turnControlAllowedTools = allowedTools
        , turnControlAgentSnapshot = agentSnapshot
        , turnControlInteractions = interactions
        }

nativePlanModeHooks
    :: TurnControl
    -> InteractionRuntime
    -> PlanModeHooks
nativePlanModeHooks control interactions = PlanModeHooks
    { planConfirmEnter = \reason ->
        requestNativeInteraction
            control interactions 1 reason
            [ "Enter plan mode"
            , "Stay in normal mode"
            ] >>= \case
                Just resolution ->
                    pure (resolution.interactionSelectedIndex == 0)
                Nothing -> pure False
    , planDecideExit = \planBody ->
        requestNativeInteraction
            control interactions 2 planBody
            [ "Approve and implement"
            , "Request changes"
            , "Cancel plan"
            ] >>= \case
                Just resolution ->
                    pure $ case resolution.interactionSelectedIndex of
                        0 -> PlanApprove
                        1 ->
                            PlanRequestChanges
                                (fromMaybe
                                    "(no notes)"
                                    (nonBlank
                                        resolution.interactionCustomText))
                        _ -> PlanCancel
                Nothing -> pure PlanCancel
    , planAskQuestion = \question options ->
        requestNativeInteraction
            control interactions 3 question options >>= \case
                Nothing -> pure Nothing
                Just resolution
                    | resolution.interactionSelectedIndex >= 0 ->
                        pure $
                            atMay
                                resolution.interactionSelectedIndex
                                options
                    | otherwise ->
                        pure (nonBlank resolution.interactionCustomText)
    }
  where
    nonBlank = (>>= \text ->
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped)

requestNativeInteraction
    :: TurnControl
    -> InteractionRuntime
    -> CInt
    -> Text
    -> [Text]
    -> IO (Maybe NativeInteractionResolution)
requestNativeInteraction control interactions kind prompt options = do
    waiter <- newEmptyTMVarIO
    registration <-
        withMVar interactions.interactionCallbackLock \_ -> do
            registered <- atomically do
                target <- readTVar interactions.interactionCallbackTarget
                case target of
                    Nothing -> pure Nothing
                    Just callbackTarget -> do
                        interactionID <- register waiter
                        pure (Just (callbackTarget, interactionID))
            forM_ registered \(callbackTarget, interactionID) ->
                sendNativeInteraction
                    callbackTarget
                    control.turnControlId
                    interactionID
                    kind
                    prompt
                    options
                    `onException`
                        atomically
                            (modifyTVar'
                                interactions.interactionPending
                                (Map.delete
                                    (control.turnControlId, interactionID)))
            pure registered
    case registration of
        Nothing -> pure Nothing
        Just (_, interactionID) -> do
            let cleanup =
                    atomically $ modifyTVar'
                        interactions.interactionPending
                        (Map.delete
                            (control.turnControlId, interactionID))
            (Just <$> atomically (takeTMVar waiter))
                `finally` cleanup
  where
    register waiter = do
        current <- readTVar control.turnControlInteractionCounter
        let next = current + 1
            interactionID =
                control.turnControlId
                    <> "-interaction-"
                    <> Text.pack (show next)
        writeTVar control.turnControlInteractionCounter next
        modifyTVar'
            interactions.interactionPending
            (Map.insert
                (control.turnControlId, interactionID)
                PendingInteraction
                    { pendingInteractionOptionCount = length options
                    , pendingInteractionWaiter = waiter
                    })
        pure interactionID

sendNativeInteraction
    :: InteractionCallbackTarget
    -> Text
    -> Text
    -> CInt
    -> Text
    -> [Text]
    -> IO ()
sendNativeInteraction target turnID interactionID kind prompt options =
    withTextBytes turnID \turnPointer turnLength ->
    withTextBytes interactionID
        \interactionPointer interactionLength ->
    withTextBytes prompt \promptPointer promptLength ->
    withInteractionOptions options \optionPointer optionCount ->
        invokeInteractionCallback
            target.interactionTargetCallback
            target.interactionTargetContext
            turnPointer
            turnLength
            interactionPointer
            interactionLength
            kind
            promptPointer
            promptLength
            optionPointer
            optionCount

withInteractionOptions
    :: [Text]
    -> (Ptr CInteractionOption -> CSize -> IO a)
    -> IO a
withInteractionOptions [] action = action nullPtr 0
withInteractionOptions options action =
    withEncodedOptions options \encoded ->
        allocaArray (length encoded) \pointer -> do
            forM_ (zip [0..] encoded) \(index, (label, labelLength)) ->
                pokeElemOff pointer index CInteractionOption
                    { cInteractionOptionLabel = label
                    , cInteractionOptionLabelLength = labelLength
                    }
            action pointer (fromIntegral (length encoded))

withEncodedOptions
    :: [Text]
    -> ([(Ptr Word8, CSize)] -> IO a)
    -> IO a
withEncodedOptions [] action = action []
withEncodedOptions (option : rest) action =
    withTextBytes option \pointer length ->
        withEncodedOptions rest
            (action . ((pointer, length) :))

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

activeAgentSnapshot :: TurnControl -> BridgeRequest -> IO Aeson.Value
activeAgentSnapshot control request = do
    loadSnapshot <- readTVarIO control.turnControlAgentSnapshot
    tryAny loadSnapshot >>= \case
        Left exception -> pure $
            failureEvent request.requestId (Text.pack (show exception))
        Right agents -> pure $
            successEvent request.requestId (map agentEntryJSON agents)

agentEntryJSON :: Viewport.AgentEntry -> Aeson.Value
agentEntryJSON entry =
    Aeson.object
        [ "path" Aeson..= entry.agentPath
        , "status" Aeson..= entry.agentStatus
        , "model" Aeson..= entry.agentModel
        , "steps" Aeson..= map agentStepJSON entry.agentSteps
        ]

agentStepJSON :: Viewport.AgentStep -> Aeson.Value
agentStepJSON step =
    Aeson.object
        [ "state" Aeson..= agentStepStateText step.agentStepState
        , "title" Aeson..= step.agentStepTitle
        , "detail" Aeson..= step.agentStepDetail
        ]

agentStepStateText :: Viewport.AgentStepState -> Text
agentStepStateText = \case
    Viewport.AgentStepRunning -> "running"
    Viewport.AgentStepCompleted -> "completed"
    Viewport.AgentStepFailed -> "failed"
    Viewport.AgentStepInfo -> "info"

cancelTurn :: TurnControl -> IO ()
cancelTurn control = do
    atomically $ writeTVar control.turnControlCancelled True
    cancelAction <- readTVarIO control.turnControlCancel
    cancelAction
    waiters <- atomically do
        current <- readTVar control.turnControlApprovals
        writeTVar control.turnControlApprovals Map.empty
        pure (Map.elems current)
    atomically $
        forM_ waiters \waiter ->
            void (tryPutTMVar waiter PermissionDeny)
    interactionWaiters <- atomically do
        current <- readTVar
            control.turnControlInteractions.interactionPending
        let (owned, remaining) =
                Map.partitionWithKey
                    (\(turnID, _) _ ->
                        turnID == control.turnControlId)
                    current
        writeTVar
            control.turnControlInteractions.interactionPending
            remaining
        pure (map (.pendingInteractionWaiter) (Map.elems owned))
    atomically $
        forM_ interactionWaiters \waiter ->
            void $ tryPutTMVar waiter cancelledInteractionResolution

requestApproval
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApproval callback context control call = do
    alreadyAllowed <- Set.member call.name
        <$> readTVarIO control.turnControlAllowedTools
    if alreadyAllowed && not (isComputerToolCallKind call.callKind)
      then pure (Just PermissionAllowOnce)
      else requestApprovalFromClient callback context control call

requestApprovalFromClient
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApprovalFromClient callback context control call = do
    waiter <- newEmptyTMVarIO
    approvalId <- atomically do
        current <- readTVar control.turnControlApprovalCounter
        let next = current + 1
            approvalId =
                control.turnControlId
                    <> "-approval-"
                    <> Text.pack (show next)
        writeTVar control.turnControlApprovalCounter next
        modifyTVar'
            control.turnControlApprovals
            (Map.insert approvalId waiter)
        pure approvalId
    let (arguments, truncated) =
            boundedEventText
                (if call.argumentsEncrypted then "" else call.arguments)
    sendEvent callback context $
        Aeson.object
            [ "event" Aeson..= ("approval.requested" :: Text)
            , "turnId" Aeson..= control.turnControlId
            , "approval" Aeson..= Aeson.object
                [ "id" Aeson..= approvalId
                , "callId" Aeson..= call.callId
                , "name" Aeson..= call.name
                , "summary" Aeson..= summarizeToolCall call
                , "arguments" Aeson..= arguments
                , "argumentsEncrypted" Aeson..= call.argumentsEncrypted
                , "async" Aeson..= (toolCallMode call == AsyncToolCall)
                , "truncated" Aeson..= truncated
                ]
            ]
    choice <- atomically (takeTMVar waiter)
    atomically $
        modifyTVar'
            control.turnControlApprovals
            (Map.delete approvalId)
    case choice of
        PermissionAllowTool
            | not (isComputerToolCallKind call.callKind) ->
            atomically $
                modifyTVar'
                    control.turnControlAllowedTools
                    (Set.insert call.name)
        _ -> pure ()
    pure (Just choice)

requestRootAccessFromClient
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> OsPath
    -> IO Bool
requestRootAccessFromClient callback context control root = do
    path <- Text.pack <$> decodeFS root
    choice <- requestApprovalFromClient callback context control ToolCall
        { callId = control.turnControlId <> "-filesystem-root-access"
        , name = "filesystem_root_access"
        , arguments =
            TextEncoding.decodeUtf8
                . LBS.toStrict
                . Aeson.encode
                $ Aeson.object ["path" Aeson..= path]
        , callKind = FunctionCallKind
        , argumentsEncrypted = False
        }
    -- Root grants are maintained by the filesystem permission store, not by
    -- the per-turn "allow this tool" shortcut used for ordinary tool calls.
    atomically $
        modifyTVar'
            control.turnControlAllowedTools
            (Set.delete "filesystem_root_access")
    pure $ case choice of
        Nothing -> False
        Just PermissionDeny -> False
        Just _ -> True

resolveApproval :: TurnControl -> BridgeRequest -> IO Aeson.Value
resolveApproval control request =
    case (parseParams request
        :: Either Text ApprovalResolution) of
        Left err -> pure (failureEvent request.requestId err)
        Right resolution ->
            case permissionChoice resolution.approvalResolutionDecision of
                Nothing ->
                    pure $ failureEvent
                        request.requestId
                        "unknown approval decision"
                Just choice -> do
                    accepted <- atomically do
                        current <- readTVar control.turnControlApprovals
                        case Map.lookup
                            resolution.approvalResolutionId
                            current of
                                Nothing -> pure False
                                Just waiter -> do
                                    published <- tryPutTMVar waiter choice
                                    if published
                                        then writeTVar
                                            control.turnControlApprovals
                                            (Map.delete
                                                resolution.approvalResolutionId
                                                current)
                                        else pure ()
                                    pure published
                    pure $
                        if accepted
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "approval request is no longer active"

permissionChoice :: Text -> Maybe PermissionChoice
permissionChoice = \case
    "allow_once" -> Just PermissionAllowOnce
    "allow_tool" -> Just PermissionAllowTool
    "deny" -> Just PermissionDeny
    _ -> Nothing

finishTurnEvent
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> TaskResult
    -> IO ()
finishTurnEvent callback context turnId = \case
    TaskFailure message ->
        sendEvent callback context $
            turnFailedEvent turnId message
    TaskOutcome outcome -> do
        forM_
            (encodeNativeUsageEvent
                True
                turnId
                outcome.turnOutcomeUsage
                outcome.turnOutcomeProviderCostUSD)
            (sendBinaryEvent callback context)
        case outcome.turnOutcomeError of
            Just err ->
                sendEvent callback context (turnFailedEvent turnId err)
            Nothing ->
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.completed" :: Text)
                        , "turnId" Aeson..= turnId
                        , "sessionId" Aeson..=
                            outcome.turnOutcomeSessionId
                        ]

taskResultSessionId :: Maybe Text -> TaskResult -> Maybe Text
taskResultSessionId fallback = \case
    TaskFailure _ -> fallback
    TaskOutcome outcome -> outcome.turnOutcomeSessionId <|> fallback

taskResultState :: TaskResult -> Text
taskResultState = \case
    TaskFailure _ -> "failed"
    TaskOutcome outcome ->
        case outcome.turnOutcomeError of
            Just _ -> "failed"
            Nothing -> "succeeded"

nativeLoopEvent :: Text -> LoopEvent -> Maybe Aeson.Value
nativeLoopEvent turnId = \case
    ActivityUpdated status -> Just $ turnStatusEvent turnId status
    WarningRaised warning -> Just $ turnStatusEvent turnId warning
    ResponseRestarted message -> Just $ turnStatusEvent turnId message
    _ -> Nothing

turnStatusEvent :: Text -> Text -> Aeson.Value
turnStatusEvent turnId status =
    Aeson.object
        [ "event" Aeson..= ("turn.status" :: Text)
        , "turnId" Aeson..= turnId
        , "status" Aeson..= status
        ]

sendTurnStatus
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> Text
    -> IO ()
sendTurnStatus callback context turnId =
    sendEvent callback context . turnStatusEvent turnId

turnFailedEvent :: Text -> Text -> Aeson.Value
turnFailedEvent turnId message =
    Aeson.object
        [ "event" Aeson..= ("turn.failed" :: Text)
        , "turnId" Aeson..= turnId
        , "error" Aeson..= message
        ]

-- These methods return gateway-scoped session or model data in one event.
-- Keep the final event callback in the same credential critical section as
-- the snapshot query so a completed connect/disconnect cannot overtake it.
nativeRequestRequiresGatewayLock :: Text -> Bool
nativeRequestRequiresGatewayLock = \case
    "sessions.list" -> True
    "sessions.show" -> True
    "models.list" -> True
    _ -> False

handleRequest
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> BridgeRequest
    -> IO Aeson.Value
handleRequest config store root request = do
    result <- tryAny (handleMethod request)
    pure $ either
        (failureEvent request.requestId . Text.pack . show)
        id
        result
  where
    handleMethod current =
        case current.requestMethod of
            "ping" ->
                pure $ successEvent current.requestId $
                    Aeson.object
                        [ "runtime" Aeson..= ("haskell" :: Text)
                        , "protocol" Aeson..= (4 :: Int)
                        ]
            "sessions.list" -> do
                activeStore <- acquireStore config store
                let pool = trustedPool activeStore
                visible <- withNativeGatewayBoundary \gatewayIdentity -> do
                    (sessions, _warnings) <- listSessions pool root
                    archivedIds <- listArchivedSessionIds pool
                    case archivedIds of
                        Left err -> pure (Left err)
                        Right identifiers -> do
                            let archived = Set.fromList identifiers
                                allowed =
                                    filter
                                        (nativeSessionMatchesBoundary
                                            gatewayIdentity)
                                        sessions
                            summaries <- mapM
                                (\session -> sessionSummaryWithStatusJSON
                                    root
                                    (Set.member session.metaId archived)
                                    session)
                                allowed
                            pure (Right summaries)
                pure $ either
                    (failureEvent current.requestId)
                    (successEvent current.requestId)
                    visible
            "sessions.show" ->
                case (parseParams current
                    :: Either Text SessionPageRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right page -> do
                        activeStore <- acquireStore config store
                        let pool = trustedPool activeStore
                        snapshot <- withNativeGatewayBoundary
                            \gatewayIdentity ->
                                validateNativeSessionBoundary
                                    pool
                                    root
                                    gatewayIdentity
                                    page.sessionPageId >>= \case
                                        Left err -> pure (Left err)
                                        Right _ ->
                                            loadSessionPageJSON
                                                pool
                                                root
                                                page.sessionPageId
                                                page.sessionPageBefore
                                                (max 1
                                                    (min 200
                                                        (maybe
                                                            50
                                                            id
                                                            page.sessionPageLimit)))
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            snapshot
            "turn.agents" ->
                pure $ successEvent current.requestId ([] :: [Aeson.Value])
            "models.list" ->
                case (parseParams current
                    :: Either Text ModelsListRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right parameters -> do
                        activeStore <- acquireStore config store
                        catalogResult <-
                            withNativeGatewayCredentialBoundary
                            \credential gatewayIdentity ->
                                loadNativeModelCatalog
                                    activeStore
                                    root
                                    credential
                                    gatewayIdentity
                                    parameters
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            catalogResult
            method ->
                pure $ failureEvent current.requestId
                    ("unknown method: " <> method)

acquireStore :: ManagedPostgresConfig -> MVar (Maybe Store) -> IO Store
acquireStore config state =
    modifyMVar state \case
        Just store -> pure (Just store, store)
        Nothing ->
            openStore config >>= \case
                Left err -> fail (Text.unpack (renderStoreError err))
                Right store -> pure (Just store, store)

closeEngineStore :: MVar (Maybe Store) -> IO ()
closeEngineStore state =
    modifyMVar state \case
        Nothing -> pure (Nothing, ())
        Just store -> closeStore store >> pure (Nothing, ())

boundedEventText :: Text -> (Text, Bool)
boundedEventText value =
    let (visible, remainder) = Text.splitAt 8192 value
    in (visible, not (Text.null remainder))

successEvent :: Aeson.ToJSON value => Text -> value -> Aeson.Value
successEvent requestId result =
    Aeson.object
        [ "id" Aeson..= requestId
        , "ok" Aeson..= True
        , "result" Aeson..= result
        ]

failureEvent :: Text -> Text -> Aeson.Value
failureEvent requestId message =
    Aeson.object
        [ "id" Aeson..= requestId
        , "ok" Aeson..= False
        , "error" Aeson..= message
        ]

sendEvent :: FunPtr EventCallback -> Ptr () -> Aeson.Value -> IO ()
sendEvent callback context event =
    void $ tryAny $
        BS.useAsCStringLen (LBS.toStrict (Aeson.encode event)) \(bytes, length) ->
            sendCallbackBytes callback context (castPtr bytes) length

sendBinaryEvent :: FunPtr EventCallback -> Ptr () -> BS.ByteString -> IO ()
sendBinaryEvent callback context bytes =
    void $ tryAny $
        BS.useAsCStringLen bytes \ (pointer, length) ->
            sendCallbackBytes callback context (castPtr pointer) length

sendCallbackBytes
    :: FunPtr EventCallback
    -> Ptr ()
    -> Ptr Word8
    -> Int
    -> IO ()
sendCallbackBytes callback context bytes length =
    invokeEventCallback callback
        context
        (castPtr bytes)
        (fromIntegral length)
