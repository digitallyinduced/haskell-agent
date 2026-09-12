module Agent.Runtime.Startup.ContextSpec (spec) where

import Agent.Runtime.Session
import Agent.Runtime.SessionSpec.Fixtures
    ( fixedTime, testMeta, testPromptSnapshot )
import Agent.Runtime.Startup.Context
import Agent.Loop (TurnInput(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Skills (SkillCatalog(..), SkillWarning(..))
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception.Safe (finally, throwIO, tryAny)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "startup context preparation" do
    it "requests generated context for a fresh session" do
        let context = resolveSessionInitialContext False False Nothing
        context.initialContextItems `shouldBe` []
        context.initialContextPrevious `shouldBe` Nothing
        context.initialContextNeeded `shouldBe` True
        context.initialContextMayRestoreSnapshot `shouldBe` False

    it "retains existing transcript and response continuation on a compatible resume" do
        let context = resolveSessionInitialContext False False
                (Just (continuedMeta, [sampleTurn]))
        context.initialContextItems `shouldBe` turnInputsToItems [UserMessage "hello"]
        context.initialContextPrevious `shouldBe` Just "response-1"
        context.initialContextNeeded `shouldBe` False
        context.initialContextMayRestoreSnapshot `shouldBe` False

    it "drops continuation but keeps transcript context on a target change" do
        let context = resolveSessionInitialContext False True
                (Just (continuedMeta, [sampleTurn]))
        context.initialContextItems `shouldBe` turnInputsToItems [UserMessage "hello"]
        context.initialContextPrevious `shouldBe` Nothing
        context.initialContextNeeded `shouldBe` False

    it "drops continuation on a pending transition and regenerates an empty session" do
        let context = resolveSessionInitialContext True False
                (Just (continuedMeta, []))
        context.initialContextPrevious `shouldBe` Nothing
        context.initialContextNeeded `shouldBe` True

    it "regenerates context after a transcript reset rather than restoring an old snapshot" do
        let context = resolveSessionInitialContext False False
                (Just (snapshotMeta, [sampleTurn { turnEffect = TranscriptReset }]))
        context.initialContextResumeNeedsFresh `shouldBe` True
        context.initialContextNeeded `shouldBe` True
        context.initialContextMayRestoreSnapshot `shouldBe` False

    it "permits snapshot restore only for an empty transcript without continuation" do
        let context = resolveSessionInitialContext False False (Just (snapshotMeta, []))
            continued = resolveSessionInitialContext False False
                (Just (snapshotMeta { metaLastResponseId = Just "response-1" }, []))
        context.initialContextNeeded `shouldBe` True
        context.initialContextMayRestoreSnapshot `shouldBe` True
        continued.initialContextNeeded `shouldBe` False
        continued.initialContextMayRestoreSnapshot `shouldBe` False

    it "overlaps instruction and learned-skill reads while preserving their results" do
        agentsStarted <- newEmptyMVar
        skillsStarted <- newEmptyMVar
        result <- timeout 2000000 $
            preloadInitialContext (ContextPreloadPolicy True False) freshContext
                (putMVar agentsStarted () >> takeMVar skillsStarted >> pure (Just "agents"))
                (putMVar skillsStarted () >> takeMVar agentsStarted >> pure (Just "skills"))
        result `shouldBe` Just (Just ("agents" :: Text), Just ("skills" :: Text))

    it "never discovers host instructions for restricted native hosts, even on dialect refresh" do
        preloadInitialContext (ContextPreloadPolicy False True) freshContext
            forbiddenRead (pure (Just "skills"))
            `shouldReturn` (Nothing :: Maybe Text, Just ("skills" :: Text))

    it "skips both quiet reads when compatible resumed context is reusable" do
        let context = resolveSessionInitialContext False False
                (Just (continuedMeta, [sampleTurn]))
        preloadInitialContext (ContextPreloadPolicy True False) context
            forbiddenRead forbiddenRead
            `shouldReturn` (Nothing :: Maybe Text, Nothing :: Maybe Text)

    it "defers instruction discovery for possible snapshot restore but still loads learned skills" do
        let context = resolveSessionInitialContext False False (Just (snapshotMeta, []))
        preloadInitialContext (ContextPreloadPolicy True False) context
            forbiddenRead (pure (Just "skills"))
            `shouldReturn` (Nothing :: Maybe Text, Just ("skills" :: Text))

    it "refreshes dialect instructions even when the old snapshot could otherwise be restored" do
        let context = resolveSessionInitialContext False False (Just (snapshotMeta, []))
        preloadInitialContext (ContextPreloadPolicy True True) context
            (pure (Just "new dialect")) (pure (Just "skills"))
            `shouldReturn` (Just ("new dialect" :: Text), Just ("skills" :: Text))

    it "refreshes dialect instructions without reloading learned skills for a retained transcript" do
        let context = resolveSessionInitialContext False False
                (Just (continuedMeta, [sampleTurn]))
        preloadInitialContext (ContextPreloadPolicy True True) context
            (pure (Just "new dialect")) forbiddenRead
            `shouldReturn` (Just ("new dialect" :: Text), Nothing :: Maybe Text)

    it "does not inspect discovery inputs when project instructions are disabled" do
        preloadAgentsContext False
            (error "disabled dialect evaluated")
            (error "disabled home evaluated")
            (error "disabled cwd evaluated")
            `shouldReturn` Nothing

    it "joins the instruction reader before propagating a learned-skill failure" do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        stopped <- newIORef False
        result <- timeout 2000000 $ tryAny $
            preloadInitialContext (ContextPreloadPolicy True False) freshContext
                ((putMVar started () >> takeMVar blocked :: IO (Maybe Text))
                    `finally` writeIORef stopped True)
                (takeMVar started >> throwIO (userError "learned skills failed") :: IO (Maybe Text))
        result `shouldSatisfy` maybe False isLeft
        readIORef stopped `shouldReturn` True

    it "joins both quiet readers when startup is cancelled" do
        agentsStarted <- newEmptyMVar
        skillsStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        agentsStopped <- newIORef False
        skillsStopped <- newIORef False
        let reader started stopped =
                (putMVar started () >> readMVar blocked :: IO (Maybe Text))
                    `finally` writeIORef stopped True
        result <- timeout 2000000 $
            withAsync
                (preloadInitialContext (ContextPreloadPolicy True False) freshContext
                    (reader agentsStarted agentsStopped)
                    (reader skillsStarted skillsStopped))
                \worker -> do
                    takeMVar agentsStarted
                    takeMVar skillsStarted
                    cancel worker
        result `shouldBe` Just ()
        readIORef agentsStopped `shouldReturn` True
        readIORef skillsStopped `shouldReturn` True

    it "retains local catalog warnings before remote warnings" do
        let localWarning = SkillWarning (unsafeEncodeUtf "local") "local warning"
            remoteWarning = SkillWarning (unsafeEncodeUtf "remote") "remote warning"
        assembleInitialSkills True
            (SkillCatalog [] [localWarning])
            (pure (SkillCatalog [] [remoteWarning]))
            `shouldReturn` SkillCatalog [] [localWarning, remoteWarning]

    it "does not query remote skills when skills are disabled" do
        let local = SkillCatalog [] [SkillWarning (unsafeEncodeUtf "local") "retained"]
        assembleInitialSkills False local forbiddenRead `shouldReturn` local

freshContext :: SessionInitialContext
freshContext = resolveSessionInitialContext False False Nothing

forbiddenRead :: IO a
forbiddenRead = throwIO (userError "unexpected context read")

continuedMeta :: SessionMeta
continuedMeta = (testMeta "context") { metaLastResponseId = Just "response-1" }

snapshotMeta :: SessionMeta
snapshotMeta = (testMeta "context")
    { metaPromptSnapshot = Just (testPromptSnapshot "context") }

sampleTurn :: SessionTurn
sampleTurn = SessionTurn
    { turnAt = fixedTime
    , turnUserText = "hello"
    , turnAssistantText = Nothing
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnItems = turnInputsToItems [UserMessage "hello"]
    , turnDisplayItems = []
    , turnUsage = Nothing
    , turnEffect = TranscriptAppend
    , turnProviderTelemetry = []
    }
