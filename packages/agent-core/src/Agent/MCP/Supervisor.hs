module Agent.MCP.Supervisor where


import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Concurrent (forConcurrentlyBounded_)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , mapConcurrently
    , waitCatch
    )
import Control.Concurrent.QSem
    ( newQSem
    , signalQSem
    , waitQSem
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , displayException
    , bracket_
    , finally
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (forM, unless, void, when)
import Data.Aeson
    ( FromJSON(..)
    , Value(..)
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, isJust)
import Data.Ord (Down(..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import System.Environment (getEnvironment)
import System.Directory (getCurrentDirectory)
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , proc
    )
import System.Timeout (timeout)
import Agent.MCP.Types
import Agent.MCP.Client (exceptionSummary, mcpClientStatus)
import Agent.MCP.Fleet
    ( closeMcpFleet
    , mcpFleetStatuses
    , resolveEffectiveCwds
    , sameServerConfigs
    , startMcpFleetProgressive
    , startMcpFleetWithProgress
    )
newMcpSupervisor :: IO McpSupervisor
newMcpSupervisor =
    McpSupervisor <$> newMVar McpSupervisorState
        { supervisorClosed = False
        , supervisorNextLeaseId = 1
        , supervisorEntries = []
        , supervisorPending = []
        }

acquireMcpFleet
    :: McpSupervisor
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleet supervisor configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor False
        (startMcpFleetWithProgress (const (pure ())))

acquireMcpFleetWithProgress
    :: McpSupervisor
    -> ([Text] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetWithProgress supervisor report configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor False
        (startMcpFleetWithProgress report)

acquireMcpFleetProgressive
    :: McpSupervisor
    -> ([McpServerStatus] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetProgressive supervisor report configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor True
        (startMcpFleetProgressive report)

acquireMcpFleetWith
    :: McpSupervisor
    -> Bool
    -> ([McpServerConfig] -> IO McpFleet)
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetWith supervisor progressive start configs =
  mask \restore -> do
    (decision, obsolete) <-
        modifyMVar supervisor.supervisorState \state ->
            if state.supervisorClosed
                then ioError (userError "MCP supervisor closed")
                else do
                    (healthyEntries, failedIdle) <-
                        partitionReusable state.supervisorEntries
                    matching <- findMatching healthyEntries
                    case matching of
                        Just entry -> do
                            let retained = entry
                                    { supervisorEntryLeases =
                                        entry.supervisorEntryLeases + 1
                                    }
                                otherIdle =
                                    idleExcept entry.supervisorEntryId
                                        healthyEntries
                                removed = failedIdle <> otherIdle
                            pure
                                ( state
                                    { supervisorEntries =
                                        replaceEntry retained
                                            (withoutEntries removed
                                                healthyEntries)
                                    }
                                , ( UseReady
                                        ( entry.supervisorEntryId
                                        , entry.supervisorEntryFleet
                                        )
                                  , map (.supervisorEntryFleet) removed
                                  )
                                )
                        Nothing ->
                            case findPending state.supervisorPending of
                                Just pending -> do
                                    let retained = pending
                                            { supervisorPendingLeases =
                                                pending.supervisorPendingLeases + 1
                                            }
                                    pure
                                        ( state
                                            { supervisorEntries = healthyEntries
                                            , supervisorPending =
                                                replacePending retained
                                                    state.supervisorPending
                                            }
                                        , ( WaitPending
                                                pending.supervisorPendingId
                                                pending.supervisorPendingResult
                                          , map (.supervisorEntryFleet) failedIdle
                                          )
                                        )
                                Nothing -> do
                                    completion <- newEmptyTMVarIO
                                    workerSlot <- newEmptyTMVarIO
                                    let pending = McpSupervisorPending
                                            { supervisorPendingId =
                                                state.supervisorNextLeaseId
                                            , supervisorPendingProgressive =
                                                progressive
                                            , supervisorPendingConfigs = configs
                                            , supervisorPendingResult = completion
                                            , supervisorPendingWorker = workerSlot
                                            , supervisorPendingLeases = 1
                                            }
                                        healthyIdle = filter isIdle healthyEntries
                                        removed = failedIdle <> healthyIdle
                                    pure
                                        ( state
                                            { supervisorNextLeaseId =
                                                state.supervisorNextLeaseId + 1
                                            , supervisorEntries =
                                                filter (not . isIdle) healthyEntries
                                            , supervisorPending =
                                                pending : state.supervisorPending
                                            }
                                        , ( StartPending
                                                pending.supervisorPendingId
                                                completion
                                                workerSlot
                                          , map (.supervisorEntryFleet) removed
                                          )
                                        )
    case decision of
        UseReady acquired -> do
            mapM_ closeMcpFleet obsolete
            makeLease acquired
        WaitPending entryId completion -> do
            mapM_ closeMcpFleet obsolete
            result <-
                restore (atomically (readTMVar completion))
                    `onException` releaseSupervisorEntry supervisor entryId
            either (ioError . userError . Text.unpack)
                (\fleet -> makeLease (entryId, fleet))
                result
        StartPending entryId completion workerSlot -> do
            worker <-
                asyncWithUnmask \unmask ->
                    tryAny (unmask (start configs)) >>= \case
                        Left exception ->
                            pure (Left (exceptionSummary exception))
                        Right fleet -> pure (Right fleet)
            atomically $ void (tryPutTMVar workerSlot worker)
            mapM_ closeMcpFleet obsolete
            outcome <-
                restore (waitCatch worker)
                    `onException` do
                        cancel worker
                        void (waitCatch worker)
                        failPending entryId completion
                            "MCP fleet startup cancelled"
            case outcome of
                Left exception -> do
                    let err = exceptionSummary exception
                    failPending entryId completion err
                    ioError (userError (Text.unpack err))
                Right (Left err) -> do
                    failPending entryId completion err
                    ioError (userError (Text.unpack err))
                Right (Right fleet) -> do
                    accepted <-
                        publishPending entryId completion fleet
                            `onException` closeMcpFleet fleet
                    if accepted
                        then makeLease (entryId, fleet)
                        else do
                            closeMcpFleet fleet
                            ioError (userError "MCP supervisor closed")
  where
    makeLease :: (Int, McpFleet) -> IO McpFleetLease
    makeLease (entryId, fleet) = do
        released <- newIORef False
        let release =
                atomicModifyIORef' released (\done -> (True, done))
                    >>= \alreadyReleased ->
                        unless alreadyReleased $
                            releaseSupervisorEntry supervisor entryId
        pure McpFleetLease
            { mcpLeaseFleet = fleet
            , mcpLeaseRelease = release
            }

    findPending :: [McpSupervisorPending] -> Maybe McpSupervisorPending
    findPending = go
      where
        go
            :: [McpSupervisorPending]
            -> Maybe McpSupervisorPending
        go [] = Nothing
        go (pending : rest)
            | pending.supervisorPendingProgressive == progressive
            , sameServerConfigs pending.supervisorPendingConfigs configs =
                Just pending
            | otherwise = go rest

    replacePending
        :: McpSupervisorPending
        -> [McpSupervisorPending]
        -> [McpSupervisorPending]
    replacePending replacement =
        map \pending ->
            if pending.supervisorPendingId == replacement.supervisorPendingId
                then replacement
                else pending

    failPending
        :: Int
        -> TMVar (Either Text McpFleet)
        -> Text
        -> IO ()
    failPending entryId completion err =
        modifyMVar_ supervisor.supervisorState \state -> do
            atomically $ void (tryPutTMVar completion (Left err))
            pure state
                { supervisorPending =
                    filter
                        ((/= entryId) . (.supervisorPendingId))
                        state.supervisorPending
                }

    publishPending
        :: Int
        -> TMVar (Either Text McpFleet)
        -> McpFleet
        -> IO Bool
    publishPending entryId completion fleet =
        modifyMVar supervisor.supervisorState \state ->
            case find
                ((== entryId) . (.supervisorPendingId))
                state.supervisorPending of
                Nothing -> do
                    atomically $
                        void (tryPutTMVar completion
                            (Left "MCP supervisor closed"))
                    pure (state, False)
                Just pending
                    | state.supervisorClosed -> do
                        atomically $
                            void (tryPutTMVar completion
                                (Left "MCP supervisor closed"))
                        pure
                            ( state
                                { supervisorPending =
                                    filter
                                        ((/= entryId) . (.supervisorPendingId))
                                        state.supervisorPending
                                }
                            , False
                            )
                    | otherwise -> do
                        let entry = McpSupervisorEntry
                                { supervisorEntryId = entryId
                                , supervisorEntryProgressive = progressive
                                , supervisorEntryConfigs = configs
                                , supervisorEntryFleet = fleet
                                , supervisorEntryLeases =
                                    pending.supervisorPendingLeases
                                }
                        atomically $
                            void (tryPutTMVar completion (Right fleet))
                        pure
                            ( state
                                { supervisorEntries =
                                    entry : state.supervisorEntries
                                , supervisorPending =
                                    filter
                                        ((/= entryId) . (.supervisorPendingId))
                                        state.supervisorPending
                                }
                            , True
                            )

    findMatching
        :: [McpSupervisorEntry]
        -> IO (Maybe McpSupervisorEntry)
    findMatching = go
      where
        go :: [McpSupervisorEntry] -> IO (Maybe McpSupervisorEntry)
        go [] = pure Nothing
        go (entry : rest)
            | entry.supervisorEntryProgressive /= progressive
                || not
                    (sameServerConfigs
                        entry.supervisorEntryConfigs configs) =
                    go rest
            | otherwise = do
                statuses <- mcpFleetStatuses entry.supervisorEntryFleet
                if all statusReusable statuses
                    then pure (Just entry)
                    else go rest

    replaceEntry
        :: McpSupervisorEntry
        -> [McpSupervisorEntry]
        -> [McpSupervisorEntry]
    replaceEntry replacement =
        map \entry ->
            if entry.supervisorEntryId == replacement.supervisorEntryId
                then replacement
                else entry

    isIdle :: McpSupervisorEntry -> Bool
    isIdle entry = entry.supervisorEntryLeases == 0

    idleExcept :: Int -> [McpSupervisorEntry] -> [McpSupervisorEntry]
    idleExcept retainedId =
        filter \entry ->
            isIdle entry && entry.supervisorEntryId /= retainedId

    withoutEntries
        :: [McpSupervisorEntry]
        -> [McpSupervisorEntry]
        -> [McpSupervisorEntry]
    withoutEntries removed =
        filter \entry ->
            all
                ((/= entry.supervisorEntryId) . (.supervisorEntryId))
                removed

    partitionReusable
        :: [McpSupervisorEntry]
        -> IO ([McpSupervisorEntry], [McpSupervisorEntry])
    partitionReusable = go [] []
      where
        go
            :: [McpSupervisorEntry]
            -> [McpSupervisorEntry]
            -> [McpSupervisorEntry]
            -> IO ([McpSupervisorEntry], [McpSupervisorEntry])
        go healthy failed [] =
            pure (reverse healthy, reverse failed)
        go healthy failed (entry : rest) = do
            statuses <- mcpFleetStatuses entry.supervisorEntryFleet
            let reusable =
                    all
                        (\status -> case status.mcpStatusState of
                            McpFailed _ -> False
                            McpClosed -> False
                            _ -> True)
                        statuses
            if reusable || entry.supervisorEntryLeases > 0
                then go (entry : healthy) failed rest
                else go healthy (entry : failed) rest

    statusReusable :: McpServerStatus -> Bool
    statusReusable status = case status.mcpStatusState of
        McpFailed _ -> False
        McpClosed -> False
        _ -> True

releaseMcpFleetLease :: McpFleetLease -> IO ()
releaseMcpFleetLease = (.mcpLeaseRelease)

releaseSupervisorEntry :: McpSupervisor -> Int -> IO ()
releaseSupervisorEntry supervisor entryId = do
    modifyMVar_ supervisor.supervisorState \state ->
        pure state
            { supervisorEntries =
                map releaseOne state.supervisorEntries
            , supervisorPending =
                map releasePending state.supervisorPending
            }
  where
    releaseOne :: McpSupervisorEntry -> McpSupervisorEntry
    releaseOne entry
        | entry.supervisorEntryId == entryId =
            entry
                { supervisorEntryLeases =
                    max 0 (entry.supervisorEntryLeases - 1)
                }
        | otherwise = entry

    releasePending :: McpSupervisorPending -> McpSupervisorPending
    releasePending pending
        | pending.supervisorPendingId == entryId =
            pending
                { supervisorPendingLeases =
                    max 0 (pending.supervisorPendingLeases - 1)
                }
        | otherwise = pending

closeMcpSupervisor :: McpSupervisor -> IO ()
closeMcpSupervisor supervisor = do
    (fleets, pending) <-
        modifyMVar supervisor.supervisorState \state ->
            if state.supervisorClosed
                then pure (state, ([], []))
                else
                    pure
                        ( state
                            { supervisorClosed = True
                            , supervisorEntries = []
                            , supervisorPending = []
                            }
                        , ( map (.supervisorEntryFleet)
                                state.supervisorEntries
                          , state.supervisorPending
                          )
                        )
    atomically $
        mapM_
            (\entry ->
                void (tryPutTMVar entry.supervisorPendingResult
                    (Left "MCP supervisor closed")))
            pending
    forConcurrentlyBounded_ 8
        (\entry -> do
            worker <- atomically
                (readTMVar entry.supervisorPendingWorker)
            cancel worker
            void (waitCatch worker))
        pending
    forConcurrentlyBounded_ 4 closeMcpFleet fleets
