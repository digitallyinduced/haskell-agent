module Agent.CLI.SubagentStoreSpec (spec) where

import Agent.CLI.Compaction (estimatedOccupancy)
import Agent.CLI.SubagentStore
import Agent.CLI.Session (LegacySubagentTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.CLI.Subagents.Runtime
    ( SubagentResidency(..)
    , SubagentSession(..)
    , flushAllSubagentSnapshots
    , lookupOrCreateSubagentSession
    , pinSubagentSession
    , persistAndEvictSubagentSessionWithStatus
    , prepareCollaborationSpawn
    , grokSpawnedChildIdentity
    , restoreAgentFromDisk
    , resolveChildModelAndEffort
    , unpinSubagentSession
    , usesOpenAiChildTransport
    , validatePersistedSubagentTarget
    )
import Agent.CLI.Request (requestParams)
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
import Agent.Tools.MultiAgents (CollaborationSpawnOptions(..))
import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_, replicateM_)
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
    describe "nested subagent model inheritance" do
        it "uses the immediate parent's effective model when no override is set" do
            sessionsRef <- newIORef Map.empty
            storeRootRef <- newIORef Nothing
            typesRef <- newIORef Map.empty
            forkSourceRef <- newIORef Nothing
            let agentId = SubagentId "agent-nested"
            prepareCollaborationSpawn
                OpenAIProvider
                "openai"
                id
                "gpt-5.6-luna"
                CodexDialect
                Nothing
                sessionsRef
                storeRootRef
                typesRef
                forkSourceRef
                agentId
                CollaborationSpawnOptions
                    { collaborationModel = Nothing
                    , collaborationReasoningEffort = Nothing
                    , collaborationForkTurns = Nothing
                    }
            Just session <- Map.lookup agentId <$> readIORef sessionsRef
            let rootParams =
                    requestParams OpenAIProvider "gpt-5.6-sol" "" [] "medium"
            resolveChildModelAndEffort
                OpenAIProvider
                rootParams
                session.subSessionEffectiveModel
                Nothing
                Nothing
                `shouldBe` ("gpt-5.6-luna", "high")

        it "keeps an explicit child model override" do
            let rootParams =
                    requestParams OpenAIProvider "gpt-5.6-sol" "" [] "medium"
            resolveChildModelAndEffort
                OpenAIProvider
                rootParams
                "gpt-5.6-luna"
                (Just "gpt-5.6-nano")
                (Just "high")
                `shouldBe` ("gpt-5.6-nano", "high")

        it "honors an explicit Luna effort below the default high floor" do
            let rootParams =
                    requestParams OpenAIProvider "gpt-5.6-sol" "" [] "medium"
            resolveChildModelAndEffort
                OpenAIProvider
                rootParams
                "gpt-5.6-sol"
                (Just "gpt-5.6-luna")
                (Just "medium")
                `shouldBe` ("gpt-5.6-luna", "medium")

        it "does not lower a higher inherited Luna effort" do
            let rootParams =
                    requestParams OpenAIProvider "gpt-5.6-sol" "" [] "xhigh"
            resolveChildModelAndEffort
                OpenAIProvider
                rootParams
                "gpt-5.6-luna"
                Nothing
                Nothing
                `shouldBe` ("gpt-5.6-luna", "xhigh")

        it "keeps inherited effort unchanged for other OpenAI models" do
            let rootParams =
                    requestParams OpenAIProvider "gpt-5.6-sol" "" [] "medium"
            resolveChildModelAndEffort
                OpenAIProvider
                rootParams
                "gpt-5.6-terra"
                Nothing
                Nothing
                `shouldBe` ("gpt-5.6-terra", "medium")

    describe "Grok-root child identity" do
        it "routes Luna to OpenAI Codex" do
            grokSpawnedChildIdentity
                XAIProvider
                "xai"
                id
                "grok-4.6"
                GrokBuildDialect
                (Just "luna")
                `shouldBe`
                    ( OpenAIProvider
                    , "openai"
                    , "gpt-5.6-luna"
                    , CodexDialect
                    )

        it "keeps grok-4.5 on the Grok parent" do
            grokSpawnedChildIdentity
                XAIProvider
                "xai"
                id
                "grok-4.6"
                GrokBuildDialect
                (Just "grok-4.5")
                `shouldBe`
                    (XAIProvider, "xai", "grok-4.5", GrokBuildDialect)

        it "inherits the Grok parent when model is omitted" do
            grokSpawnedChildIdentity
                XAIProvider
                "xai"
                id
                "grok-4.6"
                GrokBuildDialect
                Nothing
                `shouldBe`
                    (XAIProvider, "xai", "grok-4.6", GrokBuildDialect)

        it "keeps Luna descendants on OpenAI even without a model override" do
            usesOpenAiChildTransport
                (Just OpenAIProvider)
                Nothing
                Nothing
                `shouldBe` True
            usesOpenAiChildTransport
                Nothing
                (Just OpenAIProvider)
                (Just "gpt-5.6-sol")
                `shouldBe` True
            usesOpenAiChildTransport
                Nothing
                Nothing
                (Just "luna")
                `shouldBe` True
            usesOpenAiChildTransport
                Nothing
                Nothing
                (Just "grok-4.5")
                `shouldBe` False
            usesOpenAiChildTransport Nothing Nothing Nothing
                `shouldBe` False

        it "restores an inherited Luna descendant under a Grok parent" do
            withTempDir \dir -> do
                let agentId = SubagentId "agent-luna-descendant"
                saveSubagentState
                    dir
                    agentId
                    (testSnapshot
                        [messageItem RoleUser "inherited"]
                        (Completed (Just "OK"))
                        OpenAIProvider
                        "openai"
                        "gpt-5.6-luna"
                        CodexDialect)
                    `shouldReturn` Right ()
                sessionsRef <- newIORef Map.empty
                storeRootRef <- newIORef (Just dir)
                typesRef <- newIORef Map.empty
                bracket
                    (newSubagentRegistry defaultSubagentConfig dir
                        (\_ _ _ _ -> fail "unexpected subagent runner invocation")
                        (\_ _ -> pure ()))
                    closeSubagentRegistry
                    \registry -> do
                        restoreAgentFromDisk
                            XAIProvider
                            "xai"
                            id
                            "grok-4.6"
                            GrokBuildDialect
                            Nothing
                            storeRootRef
                            registry
                            sessionsRef
                            typesRef
                            agentId
                            `shouldReturn` Right ()
                        Just session <-
                            Map.lookup agentId <$> readIORef sessionsRef
                        session.subSessionProvider `shouldBe` OpenAIProvider
                        session.subSessionConnection `shouldBe` "openai"
                        session.subSessionEffectiveModel
                            `shouldBe` "gpt-5.6-luna"
                        session.subSessionDialect `shouldBe` CodexDialect

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
                    , passthrough = Nothing
                    }
            Right taskPath <- pure (parseTaskPath "/root/research/worker")
            let identity = SubagentIdentity (Just parentId) 2 taskPath
                snapshot =
                    (testSnapshot
                        [item]
                        (Completed (Just "done"))
                        XAIProvider
                        "xai"
                        "grok-4.5-mini"
                        GrokBuildDialect)
                        { snapshotPreviousResponseId = Just "resp-1"
                        , snapshotAgentType = Just "explore"
                        , snapshotAgentModel = Just "grok-4.5-mini"
                        , snapshotReasoningEffort = Just "high"
                        , snapshotCwd = Just (fromFilePath "/tmp/work")
                        , snapshotIdentity = Just identity
                        }
            saveSubagentState dir agentId snapshot
                `shouldReturn` Right ()
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (items, CurrentSubagentDiskMeta fields target)) -> do
                    length items `shouldBe` 1
                    fields.diskPreviousResponseId `shouldBe` Just "resp-1"
                    fields.diskStatus `shouldBe`
                        Just (Completed (Just "done"))
                    target.targetProvider `shouldBe` XAIProvider
                    target.targetConnection `shouldBe` "xai"
                    target.targetEffectiveModel `shouldBe` "grok-4.5-mini"
                    target.targetDialect `shouldBe` GrokBuildDialect
                    fields.diskAgentType `shouldBe` Just "explore"
                    fields.diskAgentModel `shouldBe` Just "grok-4.5-mini"
                    fields.diskReasoningEffort `shouldBe` Just "high"
                    fields.diskCwd `shouldBe` Just (fromFilePath "/tmp/work")
                    fields.diskTaskPath `shouldBe`
                        Just "/root/research/worker"
                    fields.diskParentId `shouldBe` Just parentId
                    fields.diskDepth `shouldBe` Just 2
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
                snapshot =
                    (testSnapshot
                        []
                        Interrupted
                        XAIProvider
                        "xai"
                        "grok-4.6"
                        GrokBuildDialect)
                        { snapshotPreviousResponseId = Just "legacy" }
            saveSubagentState dir agentId snapshot
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "meta.json"))
                "{\"previousResponseId\":\"legacy\"}"
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (_, LegacySubagentDiskMeta fields target)) -> do
                    fields.diskTaskPath `shouldBe` Nothing
                    fields.diskParentId `shouldBe` Nothing
                    fields.diskDepth `shouldBe` Nothing
                    fields.diskStatus `shouldBe` Nothing
                    target.legacyDiskProvider `shouldBe` Nothing
                    target.legacyDiskConnection `shouldBe` Nothing
                    target.legacyDiskEffectiveModel `shouldBe` Nothing
                    target.legacyDiskDialect `shouldBe` Nothing
                other -> expectationFailure ("unexpected load: " <> show other)

    it "normalizes a derivable connection into current target metadata" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-legacy-connection"
            saveSubagentState
                dir
                agentId
                (testSnapshot
                    []
                    Interrupted
                    XAIProvider
                    "xai"
                    "grok-4.6"
                    GrokBuildDialect)
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "meta.json"))
                "{\"provider\":\"xai\",\"effectiveModel\":\"grok-4.6\",\"dialect\":\"grok-build\"}"
            loadSubagentState dir agentId >>= \case
                Right (Just (_, CurrentSubagentDiskMeta _ target)) ->
                    target.targetConnection `shouldBe` "xai"
                other -> expectationFailure ("unexpected load: " <> show other)

    it "fails closed on corrupt transcript JSON" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-corrupt-1"
                snapshot =
                    (testSnapshot
                        []
                        Interrupted
                        OpenAIProvider
                        "openai"
                        "gpt-5.6-luna"
                        CodexDialect)
                        { snapshotPreviousResponseId = Just "r" }
            saveSubagentState dir agentId snapshot
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
                "openrouter"
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldBe`
                    Right SubagentTarget
                        { targetProvider = OpenRouterProvider
                        , targetConnection = "openrouter"
                        , targetEffectiveModel = "openai/gpt-5.1"
                        , targetDialect = CodexDialect
                        }

        it "rejects an inherited child after the parent target changes" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openrouter"
                "anthropic/claude-sonnet-4"
                GenericResponsesDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "rejects a matching model served by a different connection" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "local-responses"
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "rejects transport-specific child models after a provider change" do
            validatePersistedSubagentTarget
                OpenAIProvider
                "openai"
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "accepts missing target metadata only in legacy-compatible sessions" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openrouter"
                "openai/gpt-5.1"
                GrokBuildDialect
                (Just legacyTarget)
                legacyMeta
                `shouldBe`
                    Right SubagentTarget
                        { targetProvider = OpenRouterProvider
                        , targetConnection = "openrouter"
                        , targetEffectiveModel = "openai/gpt-5.1"
                        , targetDialect = GrokBuildDialect
                        }
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openrouter"
                "openai/gpt-5.1"
                GrokBuildDialect
                Nothing
                legacyMeta
                `shouldSatisfy` isLeft

        it "rejects legacy metadata after a durable root target change" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openrouter"
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
            saveSubagentState
                dir
                agentId
                (testSnapshot
                    persistedItems
                    (Completed Nothing)
                    OpenAIProvider
                    "openai"
                    "gpt-5.6-luna"
                    CodexDialect)
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

    it "keeps the installed pinned session across concurrent registry restores" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-concurrent-restore"
                persisted = [messageItem RoleUser "persisted"]
                inMemory = [messageItem RoleAssistant "in-memory"]
                workerCount = 8
            saveSubagentState
                dir
                agentId
                (testSnapshot
                    persisted
                    (Completed Nothing)
                    OpenAIProvider
                    "openai"
                    "gpt-5.6-luna"
                    CodexDialect)
                `shouldReturn` Right ()
            sessionsRef <- newIORef Map.empty
            storeRootRef <- newIORef (Just dir)
            typesRef <- newIORef Map.empty
            installed <-
                lookupTestSession
                    sessionsRef storeRootRef typesRef agentId
            writeIORef installed.subSessionTranscript inMemory
            pinSubagentSession
                storeRootRef typesRef Nothing agentId installed
            readMVar installed.subSessionResidency
                `shouldReturn` SessionPinned
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
                            readMVar current.subSessionResidency
                                `shouldReturn` SessionPinned
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
            writeIORef session.subSessionContextTokens
                (Just (estimatedOccupancy 100 200))
            bracket
                (newSubagentRegistry defaultSubagentConfig dir
                    (\_ _ _ _ -> fail "unexpected subagent runner invocation")
                    (\_ _ -> pure ()))
                closeSubagentRegistry
                \registry -> do
                    pinSubagentSession
                        storeRootRef typesRef Nothing agentId session
                    persistAndEvictSubagentSessionWithStatus
                        storeRootRef registry typesRef agentId
                        (Completed (Just "answer")) session
                        `shouldReturn` Right False
                    readIORef session.subSessionTranscript
                        `shouldReturn` items
                    readMVar session.subSessionResidency
                        `shouldReturn` SessionPinned

                    unpinSubagentSession session
                    persistAndEvictSubagentSessionWithStatus
                        storeRootRef registry typesRef agentId
                        (Completed (Just "answer")) session
                        `shouldReturn` Right True
                    readIORef session.subSessionTranscript `shouldReturn` []
                    readIORef session.subSessionContextTokens
                        `shouldReturn` Nothing
                    readMVar session.subSessionResidency
                        `shouldReturn` SessionEvicted

                    pinSubagentSession
                        storeRootRef typesRef Nothing agentId session
                    readMVar session.subSessionResidency
                        `shouldReturn` SessionPinned
                    rehydrated <-
                        lookupTestSession
                            sessionsRef storeRootRef typesRef agentId
                    readIORef rehydrated.subSessionTranscript
                        `shouldReturn` items
                    readMVar rehydrated.subSessionResidency
                        `shouldReturn` SessionPinned
                    writeIORef rehydrated.subSessionTranscript
                        [messageItem RoleAssistant "poison"]
                    unpinSubagentSession rehydrated
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
                    readMVar noStore.subSessionResidency
                        `shouldReturn` SessionResident

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
                    readMVar failed.subSessionResidency
                        `shouldReturn` SessionResident

lookupTestSession sessionsRef storeRootRef typesRef agentId =
    lookupOrCreateSubagentSession
        sessionsRef
        storeRootRef
        typesRef
        OpenAIProvider
        "openai"
        Nothing
        "gpt-5.6-luna"
        CodexDialect
        agentId

restoreTestAgent storeRootRef registry sessionsRef typesRef agentId =
    restoreAgentFromDisk
        OpenAIProvider
        "openai"
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
    , legacyTargetConnection = "openrouter"
    , legacyTargetEffectiveModel = "openai/gpt-5.1"
    , legacyTargetDialect = GrokBuildDialect
    }

persistedMeta :: SubagentDiskMeta
persistedMeta =
    CurrentSubagentDiskMeta emptyDiskFields SubagentTarget
        { targetProvider = OpenRouterProvider
        , targetConnection = "openrouter"
        , targetEffectiveModel = "openai/gpt-5.1"
        , targetDialect = CodexDialect
        }

legacyMeta :: SubagentDiskMeta
legacyMeta =
    LegacySubagentDiskMeta
        emptyDiskFields
        LegacySubagentTargetFields
            { legacyDiskProvider = Nothing
            , legacyDiskConnection = Nothing
            , legacyDiskEffectiveModel = Nothing
            , legacyDiskDialect = Nothing
            }

emptyDiskFields :: SubagentDiskFields
emptyDiskFields = SubagentDiskFields
    { diskPreviousResponseId = Nothing
    , diskStatus = Nothing
    , diskAgentType = Nothing
    , diskAgentModel = Nothing
    , diskReasoningEffort = Nothing
    , diskCwd = Nothing
    , diskTaskPath = Nothing
    , diskParentId = Nothing
    , diskDepth = Nothing
    }

testSnapshot
    :: [ResponseItem]
    -> SubagentStatus
    -> Provider
    -> Text
    -> Text
    -> DialectId
    -> SubagentStateSnapshot
testSnapshot items status provider connection effectiveModel dialect =
    SubagentStateSnapshot
        { snapshotItems = items
        , snapshotPreviousResponseId = Nothing
        , snapshotStatus = status
        , snapshotTarget = SubagentTarget
            { targetProvider = provider
            , targetConnection = connection
            , targetEffectiveModel = effectiveModel
            , targetDialect = dialect
            }
        , snapshotAgentType = Nothing
        , snapshotAgentModel = Nothing
        , snapshotReasoningEffort = Nothing
        , snapshotCwd = Nothing
        , snapshotIdentity = Nothing
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
    , passthrough = Nothing
    }

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "subagent-store-"))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)
