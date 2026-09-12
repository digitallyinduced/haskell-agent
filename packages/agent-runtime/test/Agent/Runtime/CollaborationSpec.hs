module Agent.Runtime.CollaborationSpec (spec) where

import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.Runtime.Collaboration
import Agent.Runtime.Models (ModelOption(..), ModelTarget(..))
import Agent.Subagents
    ( SubagentId, SubagentRegistry, SubagentStatus(..), defaultMaxConcurrent
    , getStatus, setSubagentRunner, spawnSubagent )
import Agent.Tools.MultiAgents (CollaborationModelTarget(..))
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, putMVar, readMVar )
import Control.Exception.Safe (finally, throwIO, tryAny)
import qualified Data.Acquire as Acquire
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "collaboration startup" do
    it "prefers explicit concurrency over project, harness, and default settings" do
        resolveMaxConcurrentAgents (Just 2) (Just 3) (Just 4) `shouldBe` 2
        resolveMaxConcurrentAgents Nothing (Just 3) (Just 4) `shouldBe` 3
        resolveMaxConcurrentAgents Nothing Nothing (Just 4) `shouldBe` 4
        resolveMaxConcurrentAgents Nothing Nothing Nothing
            `shouldBe` defaultMaxConcurrent

    it "only loads supplemental OpenAI credentials for enabled local Grok collaboration" do
        shouldLoadOpenAiChild True Nothing XAIProvider `shouldBe` True
        shouldLoadOpenAiChild False Nothing XAIProvider `shouldBe` False
        shouldLoadOpenAiChild True (Just []) XAIProvider `shouldBe` False
        shouldLoadOpenAiChild True (Just ["admitted"]) XAIProvider `shouldBe` False
        shouldLoadOpenAiChild True Nothing OpenAIProvider `shouldBe` False

    it "keeps local non-Grok models unrestricted without consulting gateway state" do
        let policy = resolveCollaborationModels OpenAIProvider Nothing False
                (expectationFailure "local policy read gateway catalog" >> pure Nothing)
        policy.childAllowedModels `shouldBe` Nothing
        isNothing policy.childModelAllowed `shouldBe` True
        isNothing policy.childResolveModel `shouldBe` True
        isNothing policy.childGatewayModelOption `shouldBe` True

    it "preserves the Grok child list and only adds the supplemental model when available" do
        let policy available =
                resolveCollaborationModels XAIProvider Nothing available (pure Nothing)
        (policy False).childAllowedModels `shouldBe` Just ["grok-4.6", "grok-4.5"]
        (policy True).childAllowedModels
            `shouldBe` Just ["grok-4.6", "grok-4.5", "gpt-5.6-luna"]

    it "uses gateway child restrictions instead of extending the Grok list" do
        let policy = resolveCollaborationModels XAIProvider (Just ["admitted"]) True
                (pure (Just [model]))
        policy.childAllowedModels `shouldBe` Just ["admitted"]
        allowed <- requireCallback policy.childModelAllowed
        allowed "gpt-5.6-luna" `shouldReturn` False
        allowed "admitted" `shouldReturn` True

    it "fails closed when the live gateway catalog is unavailable or has no matching model" do
        catalog <- newIORef Nothing
        let policy = gatewayPolicy catalog
        allowed <- requireCallback policy.childModelAllowed
        resolve <- requireCallback policy.childResolveModel
        allowed "admitted" `shouldReturn` False
        resolve "admitted" `shouldReturn` Nothing
        writeIORef catalog (Just [])
        allowed "admitted" `shouldReturn` False
        resolve "admitted" `shouldReturn` Nothing

    it "resolves the exact admitted connection, wire model, and dialect after trimming input" do
        catalog <- newIORef (Just [model])
        let policy = gatewayPolicy catalog
        resolve <- requireCallback policy.childResolveModel
        option <- requireCallback policy.childGatewayModelOption
        resolve "  admitted\n" `shouldReturn` Just CollaborationModelTarget
            { collaborationTargetProvider = OpenRouterProvider
            , collaborationTargetConnection = "gateway-custom"
            , collaborationTargetEffectiveModel = "upstream/exact-wire"
            , collaborationTargetDialect = CodexDialect
            }
        option "admitted" `shouldReturn` Just model

    it "rechecks catalog changes instead of retaining a stale admission or wire target" do
        catalog <- newIORef (Just [model])
        let policy = gatewayPolicy catalog
        allowed <- requireCallback policy.childModelAllowed
        resolve <- requireCallback policy.childResolveModel
        allowed "admitted" `shouldReturn` True
        let changed = model
                { modelTarget = model.modelTarget
                    { targetWireModelId = "upstream/replaced-wire" }
                }
        writeIORef catalog (Just [changed])
        fmap (fmap (.collaborationTargetEffectiveModel)) (resolve "admitted")
            `shouldReturn` Just "upstream/replaced-wire"
        writeIORef catalog Nothing
        allowed "admitted" `shouldReturn` False
        resolve "admitted" `shouldReturn` Nothing

    it "interrupts and joins active children before snapshots, then closes the registry" do
        events <- newIORef []
        childSlot <- newEmptyMVar
        let snapshot registry = do
                child <- readMVar childSlot
                getStatus registry child `shouldReturn` Interrupted
                record events "snapshot"
        result <- timeout 2000000 $
            Acquire.with (acquireCollaborationRegistry 1 cwd snapshot) \registry -> do
                child <- startBlockedChild registry events
                putMVar childSlot child
                pure (registry, child)
        (registry, child) <- requireResult result
        readIORef events `shouldReturn` ["child stopped", "snapshot"]
        getStatus registry child `shouldReturn` Closed
        spawnSubagent registry Nothing 0 "late" Nothing
            `shouldReturn` Left "Subagent registry is closed."

    it "closes supervisors and rejects new work even when snapshot persistence fails" do
        events <- newIORef []
        registrySlot <- newEmptyMVar
        result <- timeout 2000000 $ tryAny $
            Acquire.with
                (acquireCollaborationRegistry 1 cwd \_ -> do
                    record events "snapshot"
                    throwIO (userError "snapshot failed"))
                \registry -> do
                    child <- startBlockedChild registry events
                    putMVar registrySlot (registry, child)
        -- Resource wrappers may suppress finalizer errors; the contract here
        -- is that failing persistence cannot leave child supervisors alive.
        result `shouldSatisfy` isJust
        (registry, child) <- readMVar registrySlot
        readIORef events `shouldReturn` ["child stopped", "snapshot"]
        getStatus registry child `shouldReturn` Closed
        spawnSubagent registry Nothing 0 "late" Nothing
            `shouldReturn` Left "Subagent registry is closed."

    it "releases collaboration ownership when its session owner is cancelled" do
        events <- newIORef []
        registrySlot <- newEmptyMVar
        blocked <- newEmptyMVar
        let owner = Acquire.with
                (acquireCollaborationRegistry 1 cwd (\_ -> record events "snapshot"))
                \registry -> do
                    child <- startBlockedChild registry events
                    putMVar registrySlot (registry, child)
                    readMVar blocked :: IO ()
        result <- timeout 2000000 $ withAsync owner \worker -> do
            (registry, child) <- readMVar registrySlot
            cancel worker
            getStatus registry child `shouldReturn` Closed
            spawnSubagent registry Nothing 0 "late" Nothing
                `shouldReturn` Left "Subagent registry is closed."
        result `shouldBe` Just ()
        readIORef events `shouldReturn` ["child stopped", "snapshot"]

    it "applies the configured concurrency limit to the acquired registry" do
        events <- newIORef []
        result <- timeout 2000000 $
            Acquire.with (acquireCollaborationRegistry 1 cwd (const (pure ()))) \registry -> do
                _ <- startBlockedChild registry events
                second <- spawnSubagent registry Nothing 0 "second" Nothing
                second `shouldSatisfy` isLeft
        result `shouldBe` Just ()
        readIORef events `shouldReturn` ["child stopped"]

model :: ModelOption
model = ModelOption
    { modelTarget = ModelTarget
        { targetProvider = OpenRouterProvider
        , targetConnectionId = "gateway-custom"
        , targetModelId = "admitted"
        , targetWireModelId = "upstream/exact-wire"
        , targetDialect = CodexDialect
        }
    , modelContextWindow = Nothing
    , modelLabel = Nothing
    , modelFallbackPriority = Nothing
    }

gatewayPolicy :: IORef (Maybe [ModelOption]) -> CollaborationModels
gatewayPolicy catalog =
    resolveCollaborationModels XAIProvider (Just ["admitted"]) False (readIORef catalog)

cwd :: OsPath
cwd = unsafeEncodeUtf "/workspace"

requireCallback :: Maybe a -> IO a
requireCallback = maybe (fail "expected gateway callback") pure

requireResult :: Maybe a -> IO a
requireResult = maybe (fail "collaboration lifecycle timed out") pure

record :: IORef [Text] -> Text -> IO ()
record ref event = atomicModifyIORef' ref (\events -> (events <> [event], ()))

startBlockedChild :: SubagentRegistry -> IORef [Text] -> IO SubagentId
startBlockedChild registry events = do
    started <- newEmptyMVar
    blocked <- newEmptyMVar :: IO (MVar ())
    setSubagentRunner registry \_ _ _ _ ->
        (putMVar started () >> readMVar blocked >> fail "unreachable child result")
            `finally` record events "child stopped"
    child <- spawnSubagent registry Nothing 0 "work" Nothing >>= either
        (fail . show) pure
    readMVar started
    pure child
