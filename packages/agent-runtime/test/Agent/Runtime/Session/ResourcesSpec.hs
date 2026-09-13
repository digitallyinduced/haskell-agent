module Agent.Runtime.Session.ResourcesSpec (spec) where

import Agent.Runtime.Session (SessionCreate(..), Persistence(..))
import Agent.Runtime.Session.Preparation (PersistenceRequest(..))
import Agent.Runtime.Session.Resources
import Agent.Runtime.SessionSpec.Fixtures (testCreate, toFilePath, withTempSessionRoot)
import Agent.Tools.TaskPlan (readTaskPlan)
import Agent.Tools.Types
    ( ToolEnv(..), defaultToolEnv
    , insertMemoryOutputArtifact, lookupMemoryOutputArtifact )
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket, finally, throwIO, tryAny)
import Data.Either (isLeft)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified System.Directory as Directory
import System.OsPath (OsPath)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "session resource preparation" do
    it "owns scratch storage without enabling persistence and closes it once" $
        withRequest \request -> do
            released <- newIORef (0 :: Int)
            let hooks = emptyHooks
                    { resourceAcquireWorktreeLease =
                        pure (modifyIORef' released (+ 1))
                    }
            bracket (prepareSessionResources request hooks) (.resourceCleanup) \resources -> do
                case resources.resourcePersistence of
                    PersistenceDisabled -> pure ()
                    _ -> expectationFailure "expected disabled persistence"
                readTaskPlan resources.resourceTaskPlan `shouldReturn` Nothing
                readIORef request.resourceToolEnv.toolSessionTmp
                    `shouldReturn` Just resources.resourceSessionTmp
                Directory.doesDirectoryExist (toFilePath resources.resourceSessionTmp)
                    `shouldReturn` True
                resources.resourceCleanup
                resources.resourceCleanup
                readIORef released `shouldReturn` 1
                Directory.doesDirectoryExist (toFilePath resources.resourceSessionTmp)
                    `shouldReturn` False

    it "releases worktree ownership before clearing artifacts and deleting scratch" $
        withRequest \request -> do
            pathRef <- newIORef Nothing
            handle <- newIORef Nothing
            let hooks = emptyHooks
                    { resourcePrepareHost = \path -> do
                        writeIORef pathRef (Just path)
                        retainArtifact request.resourceToolEnv handle
                    , resourceAcquireWorktreeLease = pure do
                        readIORef pathRef >>= mapM_ \path ->
                            Directory.doesDirectoryExist (toFilePath path) `shouldReturn` True
                        artifactPresent request.resourceToolEnv handle `shouldReturn` True
                    }
            bracket (prepareSessionResources request hooks) (.resourceCleanup) \resources -> do
                resources.resourceCleanup
                artifactPresent request.resourceToolEnv handle `shouldReturn` False
                Directory.doesDirectoryExist (toFilePath resources.resourceSessionTmp)
                    `shouldReturn` False

    it "cleans completed acquisitions after a later host acquisition fails" $
        withRequest \request -> do
            pathRef <- newIORef Nothing
            handle <- newIORef Nothing
            let hooks = emptyHooks
                    { resourcePrepareHost = \path -> do
                        writeIORef pathRef (Just path)
                        retainArtifact request.resourceToolEnv handle
                    , resourceAcquireWorktreeLease = throwIO (userError "worktree lease failed")
                    }
            result <- tryAny (prepareSessionResources request hooks)
            fmap (const ()) result `shouldSatisfy` isLeft
            path <- readIORef pathRef
            path `shouldSatisfy` isJust
            mapM_ (\value -> Directory.doesDirectoryExist (toFilePath value) `shouldReturn` False) path
            artifactPresent request.resourceToolEnv handle `shouldReturn` False

    it "joins a cancelled acquisition before deleting its scratch directory" $
        withRequest \request -> do
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            stopped <- newIORef False
            handle <- newIORef Nothing
            let hooks = emptyHooks
                    { resourcePrepareHost = \path ->
                        (retainArtifact request.resourceToolEnv handle
                            >> putMVar started path >> takeMVar blocked)
                            `finally` do
                                Directory.doesDirectoryExist (toFilePath path) `shouldReturn` True
                                modifyIORef' stopped (const True)
                    }
            result <- timeout 2000000 $
                withAsync (prepareSessionResources request hooks) \worker -> do
                    path <- takeMVar started
                    cancel worker
                    readIORef stopped `shouldReturn` True
                    Directory.doesDirectoryExist (toFilePath path) `shouldReturn` False
            result `shouldBe` Just ()
            artifactPresent request.resourceToolEnv handle `shouldReturn` False

    it "continues scratch cleanup when a worktree finalizer fails" $
        withRequest \request -> do
            handle <- newIORef Nothing
            released <- newIORef False
            let hooks = emptyHooks
                    { resourcePrepareHost = const (retainArtifact request.resourceToolEnv handle)
                    , resourceAcquireWorktreeLease =
                        pure (writeIORef released True >> throwIO (userError "worktree release failed"))
                    }
            resources <- prepareSessionResources request hooks
            -- Preserve ResourceScope's best-effort cleanup contract.
            resources.resourceCleanup
            readIORef released `shouldReturn` True
            Directory.doesDirectoryExist (toFilePath resources.resourceSessionTmp)
                `shouldReturn` False
            artifactPresent request.resourceToolEnv handle `shouldReturn` False
            resources.resourceCleanup

emptyHooks :: SessionResourceHooks ()
emptyHooks = SessionResourceHooks
    { resourcePersistenceReady = const (pure ())
    , resourceTaskPlanReady = const (pure ())
    , resourcePrepareHost = const (pure ())
    , resourceAcquireWorktreeLease = pure (pure ())
    }

withRequest :: (SessionResourcesRequest -> IO a) -> IO a
withRequest action = withTempSessionRoot \root -> do
    env <- defaultToolEnv root
    action SessionResourcesRequest
        { resourcePersistenceRequest = disabledPersistence root
        , resourceToolEnv = env
        , resourceResumedTurns = []
        }

disabledPersistence :: OsPath -> PersistenceRequest
disabledPersistence root = PersistenceRequest
    { persistencePool = unusedPool
    , persistenceRoot = root
    , persistenceTarget = (testCreate unusedPool root).createTarget
    , persistenceGatewayIdentity = Nothing
    , persistenceRetargetResumed = False
    , persistenceCwd = root
    , persistenceEffort = "low"
    , persistencePrompt = Nothing
    , persistenceResumed = Nothing
    , persistenceEnabled = False
    }
  where
    unusedPool = error "disabled session resource preparation must not access the database"

retainArtifact :: ToolEnv -> IORef (Maybe Text) -> IO ()
retainArtifact env reference = do
    result <- insertMemoryOutputArtifact env.toolOutputMemoryStore 4096 "retained" 8
    case result of
        Just handle -> writeIORef reference (Just handle)
        Nothing -> expectationFailure "could not retain fixture artifact"

artifactPresent :: ToolEnv -> IORef (Maybe Text) -> IO Bool
artifactPresent env reference = readIORef reference >>= \case
    Nothing -> expectationFailure "artifact fixture was never retained" >> pure False
    Just handle -> isJust <$> lookupMemoryOutputArtifact env.toolOutputMemoryStore handle
