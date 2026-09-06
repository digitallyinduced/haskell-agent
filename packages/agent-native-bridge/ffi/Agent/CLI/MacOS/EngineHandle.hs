{-# LANGUAGE ForeignFunctionInterface #-}

-- | Stable-pointer publication and destruction own the engine worker lifetime.
module Agent.CLI.MacOS.EngineHandle () where

import Agent.CLI.MacOS.BrowserBridge (BrowserHost(..))
import Agent.CLI.MacOS.ComputerBridge (ComputerHost(..), newComputerHost)
import Agent.CLI.MacOS.EngineEvents (EventCallback, sendEvent, failureEvent)
import Agent.CLI.MacOS.EngineLifecycle (workerLifecycle)
import Agent.CLI.MacOS.EngineMailbox (newEngineMailboxIO, closeEngineMailbox)
import Agent.CLI.MacOS.EngineState (Engine(..), EngineCommand(..))
import Agent.CLI.MacOS.InteractionState (InteractionRuntime(..))
import Agent.CLI.Session (sessionsRoot)
import Agent.CLI.SessionAdmin (managedPostgresConfigForHome)
import Control.Concurrent.Async (asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Concurrent.STM (newTVarIO, atomically)
import Control.Exception.Safe (tryAny, mask, onException, finally)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Foreign (FunPtr, Ptr, StablePtr, nullFunPtr, nullPtr,
    newStablePtr, castStablePtrToPtr, castPtrToStablePtr, deRefStablePtr, freeStablePtr)
import System.Directory.OsPath (getHomeDirectory)

foreign export ccall ha_engine_create
    :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())

foreign export ccall ha_engine_destroy
    :: Ptr () -> IO ()

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
