module Agent.Runtime.Session.PreparationSpec (spec) where

import Agent.Runtime.Models (ModelTarget(..))
import Agent.Runtime.Session
import Agent.Runtime.Session.Preparation
import Agent.Runtime.SessionSpec.Fixtures
    ( testCreate, testMeta, withTempSessionRoot, withTempStore, toFilePath )
import Agent.Store.Postgres (trustedPool)
import Agent.Store.Postgres.Connection (StorePool)
import Control.Exception.Safe (bracket)
import Data.IORef (readIORef)
import qualified System.Directory as Directory
import System.OsPath (OsPath)
import Test.Hspec

spec :: Spec
spec = describe "session persistence preparation" do
    it "keeps disabled sessions independent of a database" $
        withTempSessionRoot \root -> do
            result <- prepareSessionPersistence (request unusedPool root)
                { persistenceEnabled = False }
            case result of
                PersistenceDisabled -> pure ()
                _ -> expectationFailure "expected disabled persistence"

    it "reserves scratch but defers materialization for new sessions" $
        withTempStore \store root ->
            bracket
                (prepareSessionPersistence (request (trustedPool store) root))
                cleanupPendingPersistence
                \persistence -> do
                    state <- persistenceState persistence
                    case state of
                        PersistencePending create _ temp -> do
                            create.createTarget `shouldBe` (testCreate (trustedPool store) root).createTarget
                            create.createTitleHint `shouldBe` Just "first prompt"
                            create.createTitleIsManual `shouldBe` False
                            Directory.doesDirectoryExist (toFilePath temp) `shouldReturn` True
                        _ -> expectationFailure "new session materialized during preparation"

    it "retains an unchanged resumed session even when new persistence is disabled" $
        withTempStore \store root -> do
            let meta = (testMeta "unchanged") { metaLastResponseId = Just "response" }
            persistence <- prepareSessionPersistence (request (trustedPool store) root)
                { persistenceEnabled = False
                , persistenceResumed = Just meta
                }
            handle <- activeHandle persistence
            handle.sessionMeta `shouldBe` meta

    it "does not retarget a resumed session during a pending transition" $
        withTempStore \store root -> do
            let meta = (testMeta "transition") { metaLastResponseId = Just "response" }
                initial = request (trustedPool store) root
            persistence <- prepareSessionPersistence initial
                { persistenceResumed = Just meta
                , persistenceRetargetResumed = False
                , persistenceTarget = initial.persistenceTarget
                    { targetModelId = "replacement", targetWireModelId = "replacement-wire" }
                }
            handle <- activeHandle persistence
            handle.sessionMeta `shouldBe` meta

    it "clears response continuation for a changed wire target but retains legacy child routing" $
        withTempStore \store root -> do
            original <- createSession (testCreate (trustedPool store) root)
            let meta = original.sessionMeta { metaLastResponseId = Just "response" }
                initial = request (trustedPool store) root
            persistence <- prepareSessionPersistence initial
                { persistenceResumed = Just meta
                , persistenceTarget = initial.persistenceTarget { targetWireModelId = "replacement-wire" }
                }
            handle <- activeHandle persistence
            handle.sessionMeta.metaLastResponseId `shouldBe` Nothing
            handle.sessionMeta.metaTransportModel `shouldBe` Just "replacement-wire"
            handle.sessionMeta.metaLegacySubagentTarget
                `shouldBe` Just (sessionLegacySubagentTarget meta)
            handle.sessionMeta.metaId `shouldBe` meta.metaId

    it "backfills missing transport metadata without discarding response continuation" $
        withTempStore \store root -> do
            original <- createSession (testCreate (trustedPool store) root)
            let meta = original.sessionMeta
                    { metaLastResponseId = Just "response"
                    , metaTransportModel = Nothing
                    , metaLegacySubagentTarget = Nothing
                    }
            persistence <- prepareSessionPersistence (request (trustedPool store) root)
                { persistenceResumed = Just meta }
            handle <- activeHandle persistence
            handle.sessionMeta.metaLastResponseId `shouldBe` Just "response"
            handle.sessionMeta.metaTransportModel `shouldBe` Just "grok-4"
            handle.sessionMeta.metaLegacySubagentTarget
                `shouldBe` Just (sessionLegacySubagentTarget meta)

request :: StorePool -> OsPath -> PersistenceRequest
request pool root = PersistenceRequest
    { persistencePool = pool
    , persistenceRoot = root
    , persistenceTarget = (testCreate pool root).createTarget
    , persistenceGatewayIdentity = Nothing
    , persistenceRetargetResumed = True
    , persistenceCwd = root
    , persistenceEffort = "low"
    , persistencePrompt = Just "first prompt"
    , persistenceResumed = Nothing
    , persistenceEnabled = True
    }

unusedPool :: StorePool
unusedPool = error "preparation unexpectedly used database"

persistenceState :: Persistence -> IO PersistenceState
persistenceState (PersistenceEnabled slot) = readIORef slot
persistenceState PersistenceDisabled = fail "expected enabled persistence"

activeHandle :: Persistence -> IO SessionHandle
activeHandle persistence = persistenceState persistence >>= \case
    PersistenceActive handle -> pure handle
    _ -> fail "expected active persistence"
