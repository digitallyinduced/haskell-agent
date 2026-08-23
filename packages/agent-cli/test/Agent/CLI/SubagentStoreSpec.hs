module Agent.CLI.SubagentStoreSpec (spec) where

import Agent.CLI.SubagentStore
import Agent.CLI.Session (LegacySubagentTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.CLI.Subagents.Runtime
    ( SubagentSession(..)
    , flushAllSubagentSnapshots
    , lookupOrCreateSubagentSession
    , persistAndEvictSubagentSessionWithStatus
    , restoreAgentFromDisk
    , validatePersistedSubagentTarget
    )
import Agent.Responses.Types
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Agent.Subagents
    ( SubagentId(..)
    , SubagentIdentity(..)
    , SubagentStatus(..)
    , closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (parseTaskPath)
import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_, replicateM_)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified System.Directory as Directory
import System.Directory.OsPath (doesFileExist)
import qualified System.FilePath as FilePath
import System.OsPath ((</>))
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.SubagentStore" do
    it "round-trips transcript items and meta" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-test-1"
                parentId = SubagentId "agent-parent-1"
                item = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentText "hello"
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
            Right taskPath <- pure (parseTaskPath "/root/research/worker")
            let identity = SubagentIdentity (Just parentId) 2 taskPath
            saveSubagentState dir agentId [item] (Just "resp-1")
                (Completed (Just "done")) XAIProvider "grok-4.5-mini"
                GrokBuildDialect (Just "explore")
                (Just "grok-4.5-mini") (Just "high")
                (Just (fromFilePath "/tmp/work"))
                (Just identity)
                `shouldReturn` Right ()
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (items, meta)) -> do
                    length items `shouldBe` 1
                    meta.diskPreviousResponseId `shouldBe` Just "resp-1"
                    meta.diskStatus `shouldBe` Just (Completed (Just "done"))
                    meta.diskProvider `shouldBe` Just XAIProvider
                    meta.diskEffectiveModel `shouldBe` Just "grok-4.5-mini"
                    meta.diskDialect `shouldBe` Just GrokBuildDialect
                    meta.diskAgentType `shouldBe` Just "explore"
                    meta.diskAgentModel `shouldBe` Just "grok-4.5-mini"
                    meta.diskReasoningEffort `shouldBe` Just "high"
                    meta.diskCwd `shouldBe` Just (fromFilePath "/tmp/work")
                    meta.diskTaskPath `shouldBe` Just "/root/research/worker"
                    meta.diskParentId `shouldBe` Just parentId
                    meta.diskDepth `shouldBe` Just 2
                other -> expectationFailure ("unexpected load: " <> show other)
            case subagentStoreDir dir agentId of
                Right path ->
                    doesFileExist (path </> fromFilePath "transcript.json")
                        `shouldReturn` True
                Left err -> expectationFailure (show err)

    it "rejects path-traversal agent ids" do
        isValidSubagentStoreId (SubagentId "../..") `shouldBe` False
        isValidSubagentStoreId (SubagentId "agent/../x") `shouldBe` False
        case subagentStoreDir (fromFilePath "/tmp/session") (SubagentId "../..") of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected invalid id"

    it "loads legacy metadata without task topology" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-legacy-1"
            saveSubagentState dir agentId [] (Just "legacy") Interrupted
                XAIProvider "grok-4.6" GrokBuildDialect
                Nothing Nothing Nothing Nothing Nothing
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "meta.json"))
                "{\"previousResponseId\":\"legacy\"}"
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (_, meta)) -> do
                    meta.diskTaskPath `shouldBe` Nothing
                    meta.diskParentId `shouldBe` Nothing
                    meta.diskDepth `shouldBe` Nothing
                    meta.diskStatus `shouldBe` Nothing
                    meta.diskProvider `shouldBe` Nothing
                    meta.diskEffectiveModel `shouldBe` Nothing
                    meta.diskDialect `shouldBe` Nothing
                other -> expectationFailure ("unexpected load: " <> show other)

    it "fails closed on corrupt transcript JSON" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-corrupt-1"
            saveSubagentState dir agentId [] (Just "r") Interrupted
                OpenAIProvider "gpt-5.6-luna" CodexDialect
                Nothing Nothing Nothing Nothing Nothing
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "transcript.json"))
                "{not-json"
            loadSubagentState dir agentId >>= \case
                Left err -> err `shouldSatisfy` (not . null . show)
                Right _ -> expectationFailure "expected decode failure"

    it "forks all, none, or the requested number of recent turns" do
        let items =
                [ messageItem RoleUser "one"
                , messageItem RoleAssistant "answer-one"
                , messageItem RoleUser "two"
                , messageItem RoleAssistant "answer-two"
                ]
        forkSubagentTranscript Nothing items `shouldBe` items
        forkSubagentTranscript (Just "none") items `shouldBe` []
        forkSubagentTranscript (Just "1") items
            `shouldBe` drop 2 items
        forkSubagentTranscript (Just "18446744073709551617") items
            `shouldBe` items

    describe "validatePersistedSubagentTarget" do
        it "accepts a matching provider, effective model, and dialect" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldBe` Right ("openai/gpt-5.1", CodexDialect)

        it "rejects an inherited child after the parent target changes" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "anthropic/claude-sonnet-4"
                GenericResponsesDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "rejects transport-specific child models after a provider change" do
            validatePersistedSubagentTarget
                OpenAIProvider
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "accepts missing target metadata only in legacy-compatible sessions" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openai/gpt-5.1"
                GrokBuildDialect
                (Just legacyTarget)
                legacyMeta
                `shouldBe` Right ("openai/gpt-5.1", GrokBuildDialect)
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openai/gpt-5.1"
                GrokBuildDialect
                Nothing
                legacyMeta
                `shouldSatisfy` isLeft

        it "rejects legacy metadata after a durable root target change" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "anthropic/claude-sonnet-4"
                GenericResponsesDialect
                (Just legacyTarget)
                legacyMeta
                `shouldSatisfy` isLeft
    it "shares one session across concurrent disk hydration" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-concurrent-hydration"
                persistedItems =
                    replicate 20000 (messageItem RoleUser "persisted")
                workerCount = 16
            saveSubagentState dir agentId persistedItems Nothing
                (Completed Nothing)
                OpenAIProvider "gpt-5.6-luna" CodexDialect
                Nothing Nothing Nothing Nothing Nothing
                `shouldReturn` Right ()
            sessionsRef <- newIORef Map.empty
            storeRootRef <- newIORef (Just dir)
            typesRef <- newIORef Map.empty
            ready <- newEmptyMVar
            start <- newEmptyMVar
            let hydrate _ = do
                    putMVar ready ()
                    readMVar start
                    lookupTestSession
                        sessionsRef storeRootRef typesRef agentId
            withAsync
                (mapConcurrently hydrate [1 .. workerCount])
                \workers -> do
                    replicateM_ workerCount (takeMVar ready)
                    putMVar start ()
                    sessions <- wait workers
                    case sessions of
                        [] -> expectationFailure "expected hydrated sessions"
                        first : _ -> do
                            let marker = [messageItem RoleAssistant "shared"]
                            writeIORef first.subSessionTranscript marker
                            forM_ sessions \session ->
                                readIORef session.subSessionTranscript
                                    `shouldReturn` marker

    it "keeps the installed session across concurrent registry restores" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-concurrent-restore"
                persisted = [messageItem RoleUser "persisted"]
                inMemory = [messageItem RoleAssistant "in-memory"]
                workerCount = 8
            saveSubagentState dir agentId persisted Nothing
                (Completed Nothing)
                OpenAIProvider "gpt-5.6-luna" CodexDialect
                Nothing Nothing Nothing Nothing Nothing
                `shouldReturn` Right ()
            sessionsRef <- newIORef Map.empty
            storeRootRef <- newIORef (Just dir)
            typesRef <- newIORef Map.empty
            installed <-
                lookupTestSession
                    sessionsRef storeRootRef typesRef agentId
            writeIORef installed.subSessionTranscript inMemory
            bracket
                (newSubagentRegistry defaultSubagentConfig dir
                    (\_ _ _ _ -> fail "unexpected subagent runner invocation")
                    (\_ _ -> pure ()))
                closeSubagentRegistry
                \registry -> do
                    ready <- newEmptyMVar
                    start <- newEmptyMVar
                    let restore _ = do
                            putMVar ready ()
                            readMVar start
                            restoreTestAgent
                                storeRootRef registry sessionsRef typesRef agentId
                    withAsync
                        (mapConcurrently restore [1 .. workerCount])
                        \workers -> do
                            replicateM_ workerCount (takeMVar ready)
                            putMVar start ()
                            wait workers
                                `shouldReturn` replicate workerCount (Right ())
                    sessions <- readIORef sessionsRef
                    case Map.lookup agentId sessions of
                        Nothing -> expectationFailure "expected restored session"
                        Just current -> do
                            readIORef current.subSessionTranscript
                                `shouldReturn` inMemory
                            let marker = [messageItem RoleAssistant "same-ref"]
                            writeIORef current.subSessionTranscript marker
                            readIORef installed.subSessionTranscript
                                `shouldReturn` marker

    it "evicts only after a successful save and rehydrates the same session" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-evict-rehydrate"
                items =
                    [ messageItem RoleUser "question"
                    , messageItem RoleAssistant "answer"
                    ]
            sessionsRef <- newIORef Map.empty
            storeRootRef <- newIORef (Just dir)
            typesRef <- newIORef Map.empty
            session <-
                lookupTestSession
                    sessionsRef storeRootRef typesRef agentId
            writeIORef session.subSessionTranscript items
            writeIORef session.subSessionContextTokens (Just (100, 200))
            bracket
                (newSubagentRegistry defaultSubagentConfig dir
                    (\_ _ _ _ -> fail "unexpected subagent runner invocation")
                    (\_ _ -> pure ()))
                closeSubagentRegistry
                \registry -> do
                    writeIORef session.subSessionPinned True
                    persistAndEvictSubagentSessionWithStatus
                        storeRootRef registry typesRef agentId
                        (Completed (Just "answer")) session
                        `shouldReturn` Right False
                    readIORef session.subSessionTranscript
                        `shouldReturn` items
                    readMVar session.subSessionHydrated `shouldReturn` True

                    writeIORef session.subSessionPinned False
                    persistAndEvictSubagentSessionWithStatus
                        storeRootRef registry typesRef agentId
                        (Completed (Just "answer")) session
                        `shouldReturn` Right True
                    readIORef session.subSessionTranscript `shouldReturn` []
                    readIORef session.subSessionContextTokens
                        `shouldReturn` Nothing
                    readMVar session.subSessionHydrated `shouldReturn` False

                    rehydrated <-
                        lookupTestSession
                            sessionsRef storeRootRef typesRef agentId
                    readIORef rehydrated.subSessionTranscript
                        `shouldReturn` items
                    readMVar rehydrated.subSessionHydrated
                        `shouldReturn` True
                    writeIORef rehydrated.subSessionTranscript
                        [messageItem RoleAssistant "poison"]
                    persistAndEvictSubagentSessionWithStatus
                        storeRootRef registry typesRef agentId
                        (Completed (Just "answer")) rehydrated
                        `shouldReturn` Right True
                    writeIORef rehydrated.subSessionTranscript
                        [messageItem RoleAssistant "must-not-flush"]
                    flushAllSubagentSnapshots
                        storeRootRef registry sessionsRef typesRef
                    loadSubagentState dir agentId >>= \case
                        Right (Just (stored, _)) ->
                            stored `shouldBe`
                                [messageItem RoleAssistant "poison"]
                        other ->
                            expectationFailure
                                ("unexpected stored snapshot: " <> show other)

    it "retains resident data when persistence is unavailable or fails" do
        withTempDir \dir -> do
            let validId = SubagentId "agent-no-store"
                invalidId = SubagentId "../agent-invalid"
                items = [messageItem RoleUser "keep-me"]
            sessionsRef <- newIORef Map.empty
            noStoreRef <- newIORef Nothing
            failingStoreRef <- newIORef Nothing
            typesRef <- newIORef Map.empty
            bracket
                (newSubagentRegistry defaultSubagentConfig dir
                    (\_ _ _ _ -> fail "unexpected subagent runner invocation")
                    (\_ _ -> pure ()))
                closeSubagentRegistry
                \registry -> do
                    noStore <-
                        lookupTestSession
                            sessionsRef noStoreRef typesRef validId
                    writeIORef noStore.subSessionTranscript items
                    persistAndEvictSubagentSessionWithStatus
                        noStoreRef registry typesRef validId Interrupted noStore
                        `shouldReturn` Right False
                    readIORef noStore.subSessionTranscript `shouldReturn` items
                    readMVar noStore.subSessionHydrated `shouldReturn` True

                    failed <-
                        lookupTestSession
                            sessionsRef failingStoreRef typesRef invalidId
                    writeIORef failed.subSessionTranscript items
                    writeIORef failingStoreRef (Just dir)
                    persistAndEvictSubagentSessionWithStatus
                        failingStoreRef registry typesRef invalidId Interrupted failed
                        >>= (`shouldSatisfy` \case
                            Left _ -> True
                            Right _ -> False)
                    readIORef failed.subSessionTranscript `shouldReturn` items
                    readMVar failed.subSessionHydrated `shouldReturn` True

lookupTestSession sessionsRef storeRootRef typesRef agentId =
    lookupOrCreateSubagentSession
        sessionsRef
        storeRootRef
        typesRef
        OpenAIProvider
        Nothing
        "gpt-5.6-luna"
        CodexDialect
        agentId

restoreTestAgent storeRootRef registry sessionsRef typesRef agentId =
    restoreAgentFromDisk
        OpenAIProvider
        id
        "gpt-5.6-luna"
        CodexDialect
        Nothing
        storeRootRef
        registry
        sessionsRef
        typesRef
        agentId

legacyTarget :: LegacySubagentTarget
legacyTarget = LegacySubagentTarget
    { legacyTargetProvider = OpenRouterProvider
    , legacyTargetEffectiveModel = "openai/gpt-5.1"
    , legacyTargetDialect = GrokBuildDialect
    }

persistedMeta :: SubagentDiskMeta
persistedMeta = legacyMeta
    { diskProvider = Just OpenRouterProvider
    , diskEffectiveModel = Just "openai/gpt-5.1"
    , diskDialect = Just CodexDialect
    }

legacyMeta :: SubagentDiskMeta
legacyMeta = SubagentDiskMeta
    { diskPreviousResponseId = Nothing
    , diskStatus = Nothing
    , diskProvider = Nothing
    , diskEffectiveModel = Nothing
    , diskDialect = Nothing
    , diskAgentType = Nothing
    , diskAgentModel = Nothing
    , diskReasoningEffort = Nothing
    , diskCwd = Nothing
    , diskTaskPath = Nothing
    , diskParentId = Nothing
    , diskDepth = Nothing
    }

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False

messageItem :: ResponseRole -> Text -> ResponseItem
messageItem role text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "subagent-store-"))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)
