{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedRecordDot, BlockArguments #-}
-- | A retained resource with explicit scope ownership. Requests execute in
-- the caller's thread; failure or cancellation releases the resource before
-- returning. Successful requests retain it until reset or session closure.
module Agent.Transport.SessionResource
    ( SessionResource
    , withSessionResource
    , newSessionResource
    , runSessionResource
    , resetSessionResource
    , currentSessionCredential
    ) where

import Prelude
import Agent.ResourceScope
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Control.Exception as Exception
import Data.IORef

data SessionResource resource credential failure = SessionResource
    { scope :: !ResourceScope
    , current :: !(IORef (Maybe (credential, ResourceKey, resource)))
    , serial :: !(MVar ())
    , acquire :: ResourceScope -> credential -> IO (Either failure (resource, credential))
    }

withSessionResource
    :: (ResourceScope -> credential -> IO (Either failure (resource, credential)))
    -> (SessionResource resource credential failure -> IO result)
    -> IO result
withSessionResource acquire action = withResourceScope \scope ->
    newSessionResource scope acquire >>= action

-- | Attach a retained resource to an existing lexical scope. Acquisition gets
-- a separate child scope, released on failure, reset or credential change.
newSessionResource
    :: ResourceScope
    -> (ResourceScope -> credential -> IO (Either failure (resource, credential)))
    -> IO (SessionResource resource credential failure)
newSessionResource scope acquire = do
    current <- newIORef Nothing
    serial <- newMVar ()
    pure SessionResource { scope, current, serial, acquire }

currentSessionCredential :: SessionResource resource credential failure -> IO (Maybe credential)
currentSessionCredential session = fmap (\(credential, _, _) -> credential) <$> readIORef session.current

runSessionResource
    :: Eq credential
    => SessionResource resource credential failure
    -> credential
    -> (resource -> credential -> Bool -> IO (Either failure result))
    -> IO (Either failure result)
runSessionResource session credential action = withMVar session.serial \_ ->
    run `Exception.onException` releaseCurrent session
  where
    run = do
        retained <- readIORef session.current
        available <- case retained of
            Just (active, _, resource) | active == credential ->
                pure (Right (resource, active, False))
            _ -> do
                releaseCurrent session
                acquireCurrent session credential
        case available of
            Left err -> pure (Left err)
            Right (resource, active, fresh) -> do
                result <- action resource active fresh
                case result of
                    Left _ -> releaseCurrent session
                    Right _ -> pure ()
                pure result

acquireCurrent
    :: SessionResource resource credential failure
    -> credential
    -> IO (Either failure (resource, credential, Bool))
acquireCurrent session credential = Exception.bracketOnError
    (allocateResource session.scope newResourceScope closeResourceScopeChecked)
    (\(key, _) -> writeIORef session.current Nothing >> releaseResource key)
    \(key, scope) -> do
        acquired <- session.acquire scope credential
        case acquired of
            Left err -> releaseResource key >> pure (Left err)
            Right (resource, active) -> do
                writeIORef session.current (Just (active, key, resource))
                pure (Right (resource, active, True))

resetSessionResource :: SessionResource resource credential failure -> IO ()
resetSessionResource session = withMVar session.serial (const (releaseCurrent session))

releaseCurrent :: SessionResource resource credential failure -> IO ()
releaseCurrent session = do
    retained <- atomicModifyIORef' session.current (\current -> (Nothing, current))
    case retained of
        Nothing -> pure ()
        Just (_, key, _) -> releaseResource key
