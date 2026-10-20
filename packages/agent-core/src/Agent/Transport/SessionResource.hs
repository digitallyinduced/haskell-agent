{-# LANGUAGE NoImplicitPrelude, RankNTypes, NamedFieldPuns, OverloadedRecordDot, BlockArguments, LambdaCase, ScopedTypeVariables #-}
-- | A lazily acquired resource owned by one conversation session. The worker
-- holds its acquisition bracket across successful requests; failure, reset or
-- cancellation ends that bracket before acknowledging the request.
module Agent.Transport.SessionResource
    ( SessionResource
    , withSessionResource
    , runSessionResource
    , resetSessionResource
    , currentSessionCredential
    ) where

import Prelude
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import Data.IORef
import Control.Monad (forever, void)

data SessionResource resource credential failure = SessionResource
    { commands :: TQueue (Command resource credential failure)
    , currentCredential :: TVar (Maybe credential)
    , serial :: MVar ()
    , worker :: Async.Async ()
    }

data Command resource credential failure
    = Request credential (resource -> credential -> Bool -> IO Disposition)
        (TVar Bool) (Either Exception.SomeException failure -> IO ())
    | Reset (IO ())

data Disposition = Retain (IO ()) | Release (IO ())

withSessionResource
    :: Eq credential
    => (forall result. credential
        -> (resource -> credential -> IO (Either failure result))
        -> IO (Either failure result))
    -> (SessionResource resource credential failure -> IO result)
    -> IO result
withSessionResource acquire action = do
    commands <- newTQueueIO
    currentCredential <- newTVarIO Nothing
    serial <- newMVar ()
    Async.withAsync (serve acquire commands currentCredential) \worker ->
        action SessionResource { commands, currentCredential, serial, worker }

currentSessionCredential :: SessionResource resource credential failure -> IO (Maybe credential)
currentSessionCredential session = readTVarIO session.currentCredential

-- | Calls for a session are sequential. Cancellation wakes the worker, cancels
-- and joins the active callback, then waits for resource cleanup. Base throwIO
-- preserves the original exception identity, including System.Timeout.
runSessionResource
    :: SessionResource resource credential failure
    -> credential
    -> (resource -> credential -> Bool -> IO (Either failure result))
    -> IO (Either failure result)
runSessionResource session credential action = withMVar session.serial \_ -> do
    response <- newEmptyTMVarIO
    cancelled <- newTVarIO False
    let complete result = atomically (void (tryPutTMVar response result))
        execute resource active fresh = do
            outcome <- Async.race (awaitCancellation cancelled)
                (Exception.try (action resource active fresh))
            pure case outcome of
                Left () -> Release (complete (Left (Exception.toException Async.AsyncCancelled)))
                Right result -> case result of
                    Right (Right _) -> Retain (complete result)
                    _ -> Release (complete result)
        reject failure = complete case failure of
            Left exception -> Left exception
            Right err -> Right (Left err)
        request = Request credential execute cancelled reject
        cleanup = do
            atomically do
                pending <- isEmptyTMVar response
                if pending then writeTVar cancelled True else pure ()
            awaitResponse session response
            pure ()
    -- bracket covers the registration/wait boundary without hand-written
    -- masking. Its release is harmless after an acknowledged request.
    Exception.bracket
        (atomically (writeTQueue session.commands request))
        (const cleanup)
        (\_ -> awaitResponse session response >>= either Exception.throwIO pure)

resetSessionResource :: SessionResource resource credential failure -> IO ()
resetSessionResource session = withMVar session.serial \_ -> do
    complete <- newEmptyTMVarIO
    atomically (writeTQueue session.commands (Reset (atomically (putTMVar complete ()))))
    awaitResponse session complete

awaitResponse :: SessionResource resource credential failure -> TMVar result -> IO result
awaitResponse session response = atomically $
    readTMVar response `orElse` do
        outcome <- Async.waitCatchSTM session.worker
        either throwSTM (const (throwSTM (userError "SessionResource: worker stopped"))) outcome

awaitCancellation :: TVar Bool -> IO ()
awaitCancellation cancelled = atomically (readTVar cancelled >>= check)

serve
    :: Eq credential
    => (forall result. credential
        -> (resource -> credential -> IO (Either failure result))
        -> IO (Either failure result))
    -> TQueue (Command resource credential failure)
    -> TVar (Maybe credential)
    -> IO ()
serve acquire commands currentCredential = forever do
    command <- atomically (readTQueue commands)
    case command of
        Reset complete -> complete
        Request credential _ cancelled _ -> do
            active <- newIORef command
            let run = acquire credential \resource actual -> do
                    atomically (writeTVar currentCredential (Just actual))
                    Right <$> use active resource actual True command
                clear = atomically (writeTVar currentCredential Nothing)
            result <- Async.race (awaitCancellation cancelled)
                (Exception.try run) `Exception.finally` clear
            latest <- readIORef active
            case result of
                Left () -> reject latest (Left (Exception.toException Async.AsyncCancelled))
                Right (Left exception) -> reject latest (Left exception)
                Right (Right (Left failure)) -> reject latest (Right failure)
                Right (Right (Right complete)) -> complete
  where
    reject (Request _ _ _ complete) failure = complete failure
    reject (Reset complete) _ = complete

    use active resource credential fresh command = do
        writeIORef active command
        case command of
            Reset complete -> pure complete
            Request requested execute _ _
                -- Acquisition may refresh the credential. The first request
                -- accepts the credential actually supplied by that bracket;
                -- only later requests need to match the retained resource.
                | not fresh && requested /= credential -> do
                    atomically (unGetTQueue commands command)
                    pure (pure ())
                | otherwise -> execute resource credential fresh >>= \case
                    Release complete -> pure complete
                    Retain complete -> do
                        complete
                        next <- atomically (readTQueue commands)
                        use active resource credential False next
