-- | Local frontend ownership, independent of event-stream connections.
module Agent.Server.ClientRegistry
    ( ClientRegistry
    , ClientSession(..)
    , ClientLeaseError(..)
    , newClientRegistry
    , registerClient
    , renewClient
    , releaseClient
    , bindClientSession
    , unbindClientSession
    , withClientMutation
    , clientRegistryCounts
    , sessionClientOwner
    , maintainClientRegistry
    , closeClientRegistry
    , withServiceMutation
    , beginServiceShutdown
    ) where

import Agent.Server.Types (AccessBoundary)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket, finally, tryAny)
import Control.Monad (forM_, unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64)

data ClientSession = ClientSession
    { clientSessionBoundary :: !AccessBoundary
    , clientSessionId :: !Text
    } deriving (Eq, Show)

data ClientLeaseError
    = ClientNotRegistered
    | ClientLeaseExpired
    | ClientLimitReached
    | ClientSessionLimitReached
    | ClientSessionOwned
    | ClientSessionNotOwned
    | ClientRegistryClosed
    deriving (Eq, Show)

data ClientLease = ClientLease
    { leaseDeadline :: !Word64
    , leaseClosing :: !Bool
    , leaseMutations :: !Int
    , leaseSessions :: !(Map Text ClientSession)
    , leaseCleanupRequested :: !Bool
    }

data RegistryState = RegistryState
    { registryClosed :: !Bool
    , registryClients :: !(Map Text ClientLease)
    , registryMutations :: !Int
    }

data ClientRegistry = ClientRegistry
    { registryState :: !(TVar RegistryState)
    , registryClock :: !(IO Word64)
    , registryLifetime :: !Word64
    }

-- | The clock and lifetime use monotonic nanoseconds. Injection permits expiry
-- tests without sleeping or depending on wall-clock adjustments.
newClientRegistry :: IO Word64 -> Word64 -> IO ClientRegistry
newClientRegistry clock lifetime = do
    state <- newTVarIO RegistryState
        { registryClosed = False, registryClients = Map.empty, registryMutations = 0 }
    pure ClientRegistry
        { registryState = state
        , registryClock = clock
        , registryLifetime = lifetime
        }

registerClient :: ClientRegistry -> Text -> IO (Either ClientLeaseError ())
registerClient registry identifier = do
    now <- registry.registryClock
    atomically do
        state <- readTVar registry.registryState
        if state.registryClosed
            then pure (Left ClientRegistryClosed)
            else if Map.member identifier state.registryClients
                then pure (Left ClientSessionOwned)
                else if Map.size state.registryClients >= 256
                    then pure (Left ClientLimitReached)
                    else do
                        let lease = ClientLease
                                { leaseDeadline = now + registry.registryLifetime
                                , leaseClosing = False
                                , leaseMutations = 0
                                , leaseSessions = Map.empty
                                , leaseCleanupRequested = False
                                }
                        writeTVar registry.registryState state
                            { registryClients = Map.insert identifier lease state.registryClients }
                        pure (Right ())

renewClient :: ClientRegistry -> Text -> IO (Either ClientLeaseError ())
renewClient registry identifier = do
    now <- registry.registryClock
    atomically $ updateLiveClient registry now identifier \lease ->
        Right lease { leaseDeadline = now + registry.registryLifetime }

-- | Release is idempotent and never waits for provider or process cleanup.
releaseClient :: ClientRegistry -> Text -> IO ()
releaseClient registry identifier = atomically $
    modifyTVar' registry.registryState \state -> state
        { registryClients = Map.adjust
            (\lease -> lease { leaseClosing = True }) identifier state.registryClients }

bindClientSession
    :: ClientRegistry -> Text -> ClientSession -> IO (Either ClientLeaseError ())
bindClientSession registry identifier session = do
    now <- registry.registryClock
    atomically do
        state <- readTVar registry.registryState
        let otherOwner = any
                (\(owner, lease) -> owner /= identifier
                    && Map.member session.clientSessionId lease.leaseSessions)
                (Map.toList state.registryClients)
        if otherOwner
            then pure (Left ClientSessionOwned)
            else updateLiveClient registry now identifier \lease ->
                if Map.size lease.leaseSessions >= 64
                    && Map.notMember session.clientSessionId lease.leaseSessions
                    then Left ClientSessionLimitReached
                    else Right lease
                        { leaseSessions = Map.insert session.clientSessionId session lease.leaseSessions }

unbindClientSession :: ClientRegistry -> Text -> Text -> IO (Either ClientLeaseError ())
unbindClientSession registry identifier sessionId = do
    now <- registry.registryClock
    atomically $ updateLiveClient registry now identifier \lease ->
        if lease.leaseMutations /= 0 then Left ClientSessionOwned else
            Right lease{leaseSessions = Map.delete sessionId lease.leaseSessions}

-- | Count accepted mutations before invoking the HTTP application. Release
-- closes admission first, then waits for these invocations to publish their
-- turns before requesting cancellation. A delayed submit cannot escape cleanup.
withClientMutation
    :: ClientRegistry
    -> Text
    -> Maybe Text
    -> IO value
    -> IO (Either ClientLeaseError value)
withClientMutation registry identifier sessionId action =
    bracket begin finish \case
        Left err -> pure (Left err)
        Right () -> Right <$> action
  where
    begin = do
        now <- registry.registryClock
        atomically $ updateLiveClient registry now identifier \lease ->
            if maybe False (`Map.notMember` lease.leaseSessions) sessionId
                then Left ClientSessionNotOwned
                else Right lease { leaseMutations = lease.leaseMutations + 1 }
    finish (Left _) = pure ()
    finish (Right ()) = atomically $
        modifyTVar' registry.registryState \state -> state
            { registryClients = Map.adjust
                (\lease -> lease { leaseMutations = max 0 (lease.leaseMutations - 1) })
                identifier state.registryClients }

updateLiveClient
    :: ClientRegistry
    -> Word64
    -> Text
    -> (ClientLease -> Either ClientLeaseError ClientLease)
    -> STM (Either ClientLeaseError ())
updateLiveClient registry now identifier update = do
    state <- readTVar registry.registryState
    case Map.lookup identifier state.registryClients of
        _ | state.registryClosed -> pure (Left ClientRegistryClosed)
        Nothing -> pure (Left ClientNotRegistered)
        Just lease
            | lease.leaseClosing || now >= lease.leaseDeadline -> pure (Left ClientLeaseExpired)
            | otherwise -> case update lease of
                Left err -> pure (Left err)
                Right next -> do
                    writeTVar registry.registryState state
                        { registryClients = Map.insert identifier next state.registryClients }
                    pure (Right ())

clientRegistryCounts :: ClientRegistry -> IO (Int, Int)
clientRegistryCounts registry = atomically do
    state <- readTVar registry.registryState
    pure (Map.size state.registryClients,
        sum (map (Map.size . (.leaseSessions)) (Map.elems state.registryClients)))

sessionClientOwner :: ClientRegistry -> Text -> IO (Maybe Text)
sessionClientOwner registry sessionId = atomically do
    state <- readTVar registry.registryState
    pure $ case
        [ owner | (owner, lease) <- Map.toList state.registryClients
        , Map.member sessionId lease.leaseSessions ] of
        owner : _ -> Just owner
        [] -> Nothing

-- | One maintenance pass. Cancellation is handed to the server supervisor,
-- whose tracked cancellation workers own termination and persistence. Session
-- claims remain held until that supervisor confirms quiescence.
maintainClientRegistry
    :: ClientRegistry
    -> (ClientSession -> IO ())
    -> (ClientSession -> IO Bool)
    -> IO ()
maintainClientRegistry registry requestCancellation sessionBusy = do
    now <- registry.registryClock
    pending <- atomically do
        state <- readTVar registry.registryState
        let expired = Map.map
                (\lease -> lease
                    { leaseClosing = lease.leaseClosing || now >= lease.leaseDeadline })
                state.registryClients
            ready = Map.filter
                (\lease -> lease.leaseClosing && lease.leaseMutations == 0) expired
            markRequested owner lease =
                if Map.member owner ready
                    then lease { leaseCleanupRequested = True }
                    else lease
        writeTVar registry.registryState state
            { registryClients = Map.mapWithKey markRequested expired }
        pure (Map.toList ready)
    forM_ pending \(identifier, lease) -> do
        result <- tryAny do
            unless lease.leaseCleanupRequested $
                mapM_ requestCancellation (Map.elems lease.leaseSessions)
            busy <- or <$> mapM sessionBusy (Map.elems lease.leaseSessions)
            unless busy $ atomically $
                modifyTVar' registry.registryState \state -> state
                    { registryClients = Map.delete identifier state.registryClients }
        case result of
            Right () -> pure ()
            Left _ -> atomically $
                modifyTVar' registry.registryState \state -> state
                    { registryClients = Map.adjust
                        (\current -> current { leaseCleanupRequested = False })
                        identifier state.registryClients }

-- | Gate every local mutation, including older clients without a lease header.
withServiceMutation :: ClientRegistry -> IO a -> IO (Either ClientLeaseError a)
withServiceMutation registry action = bracket begin finish \admitted ->
    if admitted then Right <$> action else pure (Left ClientRegistryClosed)
  where
    begin = atomically do
        state <- readTVar registry.registryState
        if state.registryClosed then pure False else do
            writeTVar registry.registryState state{registryMutations = state.registryMutations + 1}
            pure True
    finish False = pure ()
    finish True = atomically (modifyTVar' registry.registryState \state ->
        state{registryMutations = state.registryMutations - 1})

beginServiceShutdown :: ClientRegistry -> Bool -> IO Bool
beginServiceShutdown registry force = atomically do
    state <- readTVar registry.registryState
    if force || (Map.null state.registryClients && state.registryMutations == 0)
        then writeTVar registry.registryState state{registryClosed = True} >> pure True
        else pure False

closeClientRegistry :: ClientRegistry -> (ClientSession -> IO ()) -> IO ()
closeClientRegistry registry requestCancellation = do
    sessions <- atomically do
        state <- readTVar registry.registryState
        writeTVar registry.registryState state
            { registryClosed = True
            , registryClients = Map.map (\lease -> lease { leaseClosing = True }) state.registryClients }
        pure (concatMap (Map.elems . (.leaseSessions)) (Map.elems state.registryClients))
    mapM_ requestCancellation sessions `finally`
        atomically (modifyTVar' registry.registryState \state -> state { registryClients = Map.empty })
