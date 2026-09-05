-- | PostgreSQL-backed session lifecycle and persistence behavior.
module Agent.CLI.SessionSpec.Persistence (spec) where

import Agent.CLI.Session
import Agent.CLI.SessionSpec.Fixtures
import Agent.CLI.SessionSpec.ResponseItems (asyncPersistenceItems)
import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Agent.Dialect (DialectId(..))
import Agent.Loop (TokenUsage(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Store.Postgres (trustedPool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Tools.TaskPlan
    ( CurrentTaskPlan(..), TaskPlan(..), TaskPlanHooks(..)
    , TaskPlanItem(..), TaskPlanStatus(..)
    )
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (cancelWith, waitCatch, withAsync)
import Control.Exception (AsyncException(UserInterrupt), fromException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as Text
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectory, doesDirectoryExist, doesFileExist, listDirectory
    , removePathForcibly
    )
import System.OsPath (unsafeEncodeUtf, (</>))
import Test.Hspec

spec :: Spec
spec = do
    describe "PostgreSQL session persistence" do
        it "materializes and round-trips task plans through persistence hooks" $
            withTempStore \store root -> do
                persistence <-
                    newPendingPersistence (testCreate (trustedPool store) root)
                hooks <- case taskPlanHooksForPersistence persistence of
                    Nothing -> expectationFailure "missing task-plan hooks" >> fail "hooks"
                    Just value -> pure value
                hooks.taskPlanPersistReplace sampleTaskPlan
                    `shouldReturn` Right 1
                loadCurrentTaskPlan persistence
                    `shouldReturn`
                        Right
                            (Just CurrentTaskPlan
                                { currentTaskPlanRevision = 1
                                , currentTaskPlanValue = sampleTaskPlan
                                })
                hooks.taskPlanPersistClear `shouldReturn` Right ()
                loadCurrentTaskPlan persistence `shouldReturn` Right Nothing

        it "does not materialize a pending session when clearing its task plan" $
            withTempStore \store root -> do
                PersistenceEnabled slot <-
                    newPendingPersistence (testCreate (trustedPool store) root)
                hooks <- case taskPlanHooksForPersistence
                    (PersistenceEnabled slot) of
                    Nothing ->
                        expectationFailure "missing task-plan hooks"
                            >> fail "hooks"
                    Just value -> pure value
                hooks.taskPlanPersistClear `shouldReturn` Right ()
                readIORef slot >>= \case
                    PersistencePending{} -> pure ()
                    PersistenceActive{} ->
                        expectationFailure
                            "clearing an absent plan materialized the session"

        it "rejects inconsistent gateway identities at every creation boundary" $
            withTempStore \store root -> do
                let pool = trustedPool store
                    directWithGatewayIdentity =
                        (testCreate pool root)
                            { createGatewayIdentity =
                                Just "gateway-sha256:test-tenant"
                            }
                    gatewayWithoutIdentity =
                        (testCreate pool root)
                            { createTarget = ModelTarget
                                { targetProvider = OpenAIProvider
                                , targetConnectionId =
                                    organizationGatewayConnectionId
                                , targetModelId = "company-coder"
                                , targetWireModelId = "company-coder"
                                , targetDialect = GenericResponsesDialect
                                }
                            }
                newPendingPersistence directWithGatewayIdentity
                    `shouldThrow` anyException
                createSession gatewayWithoutIdentity
                    `shouldThrow` anyException

        it "forks turns, metadata, and only allowlisted durable artifacts" $
            withTempStore \store root -> do
                let pool = trustedPool store
                    gatewayIdentity = "gateway-sha256:test-tenant"
                    gatewayCreate =
                        (testCreate pool root)
                            { createTarget = ModelTarget
                                { targetProvider = OpenAIProvider
                                , targetConnectionId =
                                    organizationGatewayConnectionId
                                , targetModelId = "company-coder"
                                , targetWireModelId = "company-coder"
                                , targetDialect = GenericResponsesDialect
                                }
                            , createGatewayIdentity = Just gatewayIdentity
                            }
                source0 <- createSession gatewayCreate
                source0.sessionMeta.metaGatewayIdentity
                    `shouldBe` Just gatewayIdentity
                let sourceTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "branch from here"
                        , turnAssistantText = Just "ready"
                        , turnError = Nothing
                        , turnResponseId = Just "response-parent"
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 8
                            , outputTokens = 3
                            , cachedTokens = 1
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                source <- appendTurnWithMetaUpdate source0 sourceTurn
                    \meta -> meta
                        { metaLastRecap = Just "recap"
                        , metaLastTurnSummary = Just "summary"
                        }
                Store.replaceSessionTaskPlan
                    pool
                    source.sessionMeta.metaId
                    sampleTaskPlan.taskPlanExplanation
                    sampleStoredTaskPlanItems
                    `shouldReturn` Right (Just 1)
                let planPath = source.sessionDir </> unsafeEncodeUtf "plan.md"
                    agentsDir = source.sessionDir </> unsafeEncodeUtf "agents"
                    childDir = agentsDir </> unsafeEncodeUtf "child"
                    childPath = childDir </> unsafeEncodeUtf "meta.json"
                    ignoredPath = source.sessionDir </> unsafeEncodeUtf "agent.log"
                createDirectory agentsDir
                createDirectory childDir
                LBS.writeFile (toFilePath planPath) "plan"
                LBS.writeFile (toFilePath childPath) "child"
                LBS.writeFile (toFilePath ignoredPath) "runtime log"

                let forkCwd = root </> unsafeEncodeUtf "fork-worktree"
                forkSessionAt
                    root
                    source
                    [sourceTurn]
                    (Just "Fork title")
                    forkCwd >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right forked -> do
                        forked.sessionMeta.metaId
                            `shouldNotBe` source.sessionMeta.metaId
                        forked.sessionMeta.metaCreatedAt
                            `shouldSatisfy` (>= source.sessionMeta.metaCreatedAt)
                        forked.sessionMeta.metaUpdatedAt
                            `shouldBe` forked.sessionMeta.metaCreatedAt
                        forked.sessionMeta.metaTitle `shouldBe` "Fork title"
                        forked.sessionMeta.metaTitleIsManual `shouldBe` True
                        forked.sessionMeta.metaCwd `shouldBe` forkCwd
                        forked.sessionMeta.metaGatewayIdentity
                            `shouldBe` Just gatewayIdentity
                        forked.sessionMeta.metaLastResponseId
                            `shouldBe` Just "response-parent"
                        forked.sessionMeta.metaLastRecap `shouldBe` Just "recap"
                        forked.sessionMeta.metaLastTurnSummary
                            `shouldBe` Just "summary"
                        loadSession pool root forked.sessionMeta.metaId
                            `shouldReturn`
                                Right (forked.sessionMeta, [sourceTurn])
                        Store.loadSessionTaskPlan
                            pool
                            forked.sessionMeta.metaId
                            `shouldReturn` Right (Just sampleStoredTaskPlan)
                        doesFileExist
                            (forked.sessionDir </> unsafeEncodeUtf "plan.md")
                            `shouldReturn` True
                        doesFileExist
                            (forked.sessionDir
                                </> unsafeEncodeUtf "agents"
                                </> unsafeEncodeUtf "child"
                                </> unsafeEncodeUtf "meta.json")
                            `shouldReturn` True
                        doesFileExist
                            (forked.sessionDir </> unsafeEncodeUtf "agent.log")
                            `shouldReturn` False
                        let forkOnlyTurn = sourceTurn
                                { turnUserText = "continue only on fork"
                                , turnAssistantText = Just "fork response"
                                , turnResponseId = Just "fork-response"
                                , turnUsage = Nothing
                                }
                        forkedFinal <- appendTurn forked forkOnlyTurn
                        loadSession pool root source.sessionMeta.metaId
                            `shouldReturn`
                                Right (source.sessionMeta, [sourceTurn])
                        loadSession pool root forkedFinal.sessionMeta.metaId
                            `shouldReturn`
                                Right
                                    ( forkedFinal.sessionMeta
                                    , [sourceTurn, forkOnlyTurn]
                                    )

        it "rejects symlinked fork artifacts and cleans reserved state" $
            withTempStore \store root -> do
                let pool = trustedPool store
                source0 <- createSession (testCreate pool root)
                let sourceTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "branch from here"
                        , turnAssistantText = Just "ready"
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                source <- appendTurn source0 sourceTurn
                let outside = source.sessionDir </> unsafeEncodeUtf "outside"
                    planPath = source.sessionDir </> unsafeEncodeUtf "plan.md"
                LBS.writeFile (toFilePath outside) "outside"
                Directory.createFileLink
                    (toFilePath outside)
                    (toFilePath planPath)
                before <- listDirectory root
                beforeTemps <- listDirectory (sessionTempsRoot root)
                forkSession root source [sourceTurn] Nothing >>= \case
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "refusing to copy symbolic link"
                    Right forked ->
                        expectationFailure
                            ("unexpected fork: " <> show forked.sessionMeta.metaId)
                after <- listDirectory root
                after `shouldMatchList` before
                afterTemps <- listDirectory (sessionTempsRoot root)
                afterTemps `shouldMatchList` beforeTemps

        it "requires a substantive persisted turn before forking" $
            withTempStore \store root -> do
                source <- createSession (testCreate (trustedPool store) root)
                forkSession root source [] Nothing >>= \case
                    Left err ->
                        err
                            `shouldBe`
                                "a session must contain at least one turn before it can be forked"
                    Right forked ->
                        expectationFailure
                            ("unexpected fork: " <> show forked.sessionMeta.metaId)

        it "requires a substantive turn after the latest reset before forking" $
            withTempStore \store root -> do
                let pool = trustedPool store
                source0 <- createSession (testCreate pool root)
                let sourceTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "old conversation"
                        , turnAssistantText = Just "old answer"
                        , turnError = Nothing
                        , turnResponseId = Just "old-response"
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                source <- appendTurn source0 sourceTurn
                let reset = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/clear"
                        , turnAssistantText = Just "Conversation cleared."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptReset
                        , turnProviderTelemetry = []
                        }
                resetSource <- appendTurnKeepTitle source reset

                forkSession
                    root resetSource
                    [ sourceTurn
                    , reset
                    ]
                    Nothing >>= \case
                        Left err ->
                            err `shouldBe`
                                "a session must contain at least one turn before it can be forked"
                        Right forked ->
                            expectationFailure
                                ("unexpected fork: "
                                    <> show forked.sessionMeta.metaId)

        it "round-trips and clears ephemeral session activity" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                persistence <- newActivePersistence handle
                setPersistenceActivity
                    persistence
                    "provider_cooldown"
                    "Waiting before retrying."
                    (Just fixedTime)

                activity <-
                    loadSessionActivity root handle.sessionMeta.metaId
                activity `shouldSatisfy` maybe False
                    (\current ->
                        current.activityKind == "provider_cooldown"
                            && current.activityMessage
                                == "Waiting before retrying."
                            && current.activityRetryAt == Just fixedTime)

                clearPersistenceActivity persistence
                loadSessionActivity root handle.sessionMeta.metaId
                    `shouldReturn` Nothing

        it "clears stale activity when a session is resumed" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                persistence <- newActivePersistence handle
                setPersistenceActivity
                    persistence
                    "provider_retry"
                    "Retrying."
                    Nothing
                _ <- newActivePersistence handle
                loadSessionActivity root handle.sessionMeta.metaId
                    `shouldReturn` Nothing

        it "round-trips metadata, provider items, usage, and compaction markers" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                doesDirectoryExist handle.sessionTempDir `shouldReturn` True
                doesFileExist handle.sessionMetaPath `shouldReturn` False
                handle.sessionMeta.metaTitle `shouldBe` "untitled"
                modeOf handle.sessionDir `shouldReturn` 0o700
                modeOf handle.sessionTempDir `shouldReturn` 0o700

                let item = MessageItem ResponseMessage
                        { messageId = Nothing
                        , content = MessageContentParts
                            [InputTextPart "hi" Nothing]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , passthrough = Nothing
                        }
                    asyncItems =
                        concatMap asyncPersistenceItems
                            [Nothing, Just False, Just True]
                    normalTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "hi there"
                        , turnAssistantText = Just "hello"
                        , turnError = Nothing
                        , turnResponseId = Just "resp-1"
                        , turnItems = item : asyncItems
                        , turnDisplayItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = [sampleTurnTelemetry]
                        }
                    compactTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText = Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptReplace
                        , turnProviderTelemetry = []
                        }
                withNormal <- appendTurn handle normalTurn
                final <- appendTurnWithMetaUpdate withNormal compactTurn
                    \meta -> meta { metaLastResponseId = Nothing }

                loadSession pool root final.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta.metaTitle `shouldBe` "hi there"
                        meta.metaLastResponseId `shouldBe` Nothing
                        turns `shouldBe` [normalTurn, compactTurn]
                        sessionUsageFromTurns meta turns `shouldBe` TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                loadSessions pool root
                    [final.sessionMeta.metaId, "missing", final.sessionMeta.metaId]
                    >>= \results ->
                        fmap (fmap (\(meta, turns) -> (meta.metaId, turns))) results
                            `shouldBe`
                                [ Right
                                    (final.sessionMeta.metaId, [normalTurn, compactTurn])
                                , Left "session not found: missing"
                                , Right
                                    (final.sessionMeta.metaId, [normalTurn, compactTurn])
                                ]

                (listed, warnings) <- listSessions pool root
                map (.metaId) listed `shouldBe` [handle.sessionMeta.metaId]
                warnings `shouldBe` []
                deleteSession pool root handle.sessionMeta.metaId
                    `shouldReturn` Right ()
                doesDirectoryExist handle.sessionDir `shouldReturn` False
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                deleteSession pool root "../outside"
                    `shouldReturn` Left "invalid session id"
                loadSession pool root handle.sessionMeta.metaId
                    `shouldReturn`
                        Left ("session not found: " <> handle.sessionMeta.metaId)

        it "publishes rewind branches while preserving checkpoints and usage" $
            withTempStore \store root -> do
                let
                    pool = trustedPool store
                    first = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "first prompt"
                        , turnAssistantText = Just "first answer"
                        , turnError = Nothing
                        , turnResponseId = Just "response-first"
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                    checkpoint = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText = Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptReplace
                        , turnProviderTelemetry = []
                        }
                    later = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "later prompt"
                        , turnAssistantText = Just "later answer"
                        , turnError = Nothing
                        , turnResponseId = Just "response-later"
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 7
                            , outputTokens = 3
                            , cachedTokens = 1
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                initial <- createSession (testCreate pool root)
                withFirst <- appendTurnWithMetaUpdate initial first
                    \meta -> meta { metaTitleUserTurns = 1 }
                withCheckpoint <-
                    appendTurnWithMetaUpdate withFirst checkpoint
                        \meta -> meta { metaLastResponseId = Nothing }
                final <- appendTurnWithMetaUpdate withCheckpoint later
                    \meta -> meta
                        { metaTitleRefreshIndex = 2
                        , metaTitleUserTurns = 2
                        , metaLastRecap = Just "stale recap"
                        , metaLastTurnSummary = Just "stale summary"
                        , metaLastRecapMainTurns = 2
                        }
                Store.replaceSessionTaskPlan
                    pool
                    final.sessionMeta.metaId
                    sampleTaskPlan.taskPlanExplanation
                    sampleStoredTaskPlanItems
                    `shouldReturn` Right (Just 1)

                rewound <- rewindSession final [first, checkpoint] >>= \case
                    Left err ->
                        expectationFailure (Text.unpack err)
                            >> fail "rewind failed"
                    Right handle -> pure handle

                rewound.sessionMeta.metaLastResponseId `shouldBe` Nothing
                rewound.sessionMeta.metaTitleRefreshIndex `shouldBe` 0
                rewound.sessionMeta.metaTitleUserTurns `shouldBe` 1
                rewound.sessionMeta.metaLastRecap `shouldBe` Nothing
                rewound.sessionMeta.metaLastTurnSummary `shouldBe` Nothing
                rewound.sessionMeta.metaLastRecapMainTurns `shouldBe` 0
                rewound.sessionMeta.metaInputTokens `shouldBe` 17
                rewound.sessionMeta.metaOutputTokens `shouldBe` 7
                rewound.sessionMeta.metaCachedTokens `shouldBe` 3
                Store.loadSessionTaskPlan pool rewound.sessionMeta.metaId
                    `shouldReturn` Right Nothing

                loadSession pool root rewound.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta `shouldBe` rewound.sessionMeta
                        case turns of
                            [ originalFirst
                                , originalCheckpoint
                                , originalLater
                                , marker
                                , replayedFirst
                                , replayedCheckpoint
                                ] -> do
                                    originalFirst `shouldBe` first
                                    originalCheckpoint `shouldBe` checkpoint
                                    originalLater `shouldBe` later
                                    marker.turnUserText `shouldBe` "/rewind"
                                    marker.turnAssistantText
                                        `shouldBe` Just "Conversation rewound."
                                    marker.turnResponseId `shouldBe` Nothing
                                    marker.turnEffect `shouldBe` TranscriptReset
                                    replayedFirst `shouldBe` first
                                    replayedCheckpoint `shouldBe` checkpoint
                            _ ->
                                expectationFailure
                                    ("unexpected rewind transcript: "
                                        <> show turns)

                loadActiveSession pool root rewound.sessionMeta.metaId
                    `shouldReturn`
                        Right (rewound.sessionMeta, [checkpoint])

        it "imports a legacy meta.json and JSONL transcript once" $
            withTempStore \store root -> do
                let
                    pool = trustedPool store
                    sessionId = "2026-08-19-legacy"
                    dir = root </> unsafeEncodeUtf (Text.unpack sessionId)
                    metaPath = dir </> unsafeEncodeUtf "meta.json"
                    transcriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
                    meta = testMeta sessionId
                    items =
                        concatMap asyncPersistenceItems
                            [Nothing, Just False, Just True]
                    turn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "from disk"
                        , turnAssistantText = Just "imported"
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = items
                        , turnDisplayItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                createDirectory dir
                LBS.writeFile (toFilePath metaPath) (Aeson.encode meta)
                LBS.writeFile
                    (toFilePath transcriptPath)
                    (Aeson.encode turn <> "\n")

                loadSession pool root sessionId
                    `shouldReturn` Right (meta, [turn])
                -- Removing the source proves the second load is PostgreSQL-only.
                Directory.removeDirectoryRecursive (toFilePath dir)
                loadSession pool root sessionId
                    `shouldReturn` Right (meta, [turn])

        it "forks immutable prefixes and remaps transfer imports" $
            withTempStore \store root -> do
                let pool = trustedPool store
                    turn prompt response effect usage = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = prompt
                        , turnAssistantText = Just ("answer " <> prompt)
                        , turnError = Nothing
                        , turnResponseId = response
                        , turnItems = []
                        , turnDisplayItems = []
                        , turnUsage = usage
                        , turnProviderTelemetry = []
                        , turnEffect = effect
                        }
                    first = turn "one" (Just "resp-1") TranscriptAppend
                        (Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 2
                            , cachedTokens = 1
                            })
                    compacted = turn "/compact" Nothing TranscriptReplace Nothing
                    third = turn "three" (Just "resp-3") TranscriptAppend
                        (Just TokenUsage
                            { inputTokens = 7
                            , outputTokens = 3
                            , cachedTokens = 0
                            })
                    cleared = turn "/clear" Nothing TranscriptReset Nothing
                    fifth = turn "five" (Just "resp-5") TranscriptAppend Nothing
                source0 <- createSession (testCreate pool root)
                source1 <- appendTurn source0 first
                source2 <- appendTurnWithMetaUpdate source1 compacted
                    \meta -> meta { metaLastResponseId = Nothing }
                source3 <- appendTurn source2 third
                source4 <- appendTurnWithMetaUpdate source3 cleared
                    \meta -> meta { metaLastResponseId = Nothing }
                _source5 <- appendTurn source4 fifth
                Store.replaceSessionTaskPlan
                    pool
                    source0.sessionMeta.metaId
                    sampleTaskPlan.taskPlanExplanation
                    sampleStoredTaskPlanItems
                    `shouldReturn` Right (Just 1)

                loadSessionHistoryTurnsAround
                    pool root source0.sessionMeta.metaId 1 1 >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right page -> do
                            map fst page.pageTurns `shouldBe` [0, 1, 2]
                            map snd page.pageTurns
                                `shouldBe` [first, compacted, third]
                            page.pageHasOlder `shouldBe` False
                            page.pageHasNewer `shouldBe` True

                forkSessionAtTurn pool root source0.sessionMeta.metaId 1
                    >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right forkId -> do
                            forkId `shouldNotBe` source0.sessionMeta.metaId
                            loadSession pool root forkId >>= \case
                                Left err -> expectationFailure (Text.unpack err)
                                Right (meta, turns) -> do
                                    turns `shouldBe` [first, compacted]
                                    meta.metaLastResponseId `shouldBe` Nothing
                                    meta.metaInputTokens `shouldBe` 10
                                    meta.metaOutputTokens `shouldBe` 2
                            Store.loadSessionTaskPlan pool forkId
                                `shouldReturn` Right Nothing
                            loadSession pool root source0.sessionMeta.metaId
                                >>= \case
                                    Left err ->
                                        expectationFailure (Text.unpack err)
                                    Right (sourceMeta, sourceTurns) -> do
                                        sourceMeta.metaId
                                            `shouldBe` source0.sessionMeta.metaId
                                        sourceTurns
                                            `shouldBe`
                                                [ first
                                                , compacted
                                                , third
                                                , cleared
                                                , fifth
                                                ]
                chunks <- newIORef []
                streamSessionTransfer
                    pool root source0.sessionMeta.metaId
                    (\chunk -> modifyIORef' chunks (chunk :))
                    `shouldReturn` Right ()
                payload <- BS.concat . reverse <$> readIORef chunks
                envelope <- case Aeson.eitherDecodeStrict' payload of
                    Left err -> expectationFailure err >> fail err
                    Right value -> pure value
                validateSessionTransferEnvelope envelope
                    `shouldBe` Right envelope
                envelope.transferSession.transferTurns
                    `shouldBe`
                        [first, compacted, third, cleared, fifth]
                envelope.transferSession.transferTaskPlan
                    `shouldBe` Just sampleTaskPlan
                importSessionTransferRemapped pool root Nothing envelope
                    >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right importedId -> do
                            importedId `shouldNotBe` source0.sessionMeta.metaId
                            loadSession pool root importedId >>= \case
                                Left err ->
                                    expectationFailure (Text.unpack err)
                                Right (importedMeta, importedTurns) -> do
                                    importedMeta.metaId `shouldBe` importedId
                                    importedTurns
                                        `shouldBe`
                                            [ first
                                            , compacted
                                            , third
                                            , cleared
                                            , fifth
                                            ]
                                    Store.loadSessionTaskPlan pool importedId
                                        `shouldReturn`
                                            Right (Just sampleStoredTaskPlan)

        it "rolls back failed task-plan imports so the transfer can be retried" $
            withTempStore \store root -> do
                let pool = trustedPool store
                    sessionId = "session-atomic-transfer"
                    invalidPlan = TaskPlan
                        { taskPlanExplanation = Nothing
                        , taskPlanItems =
                            [ TaskPlanItem
                                "first active step"
                                TaskPlanInProgress
                            , TaskPlanItem
                                "second active step"
                                TaskPlanInProgress
                            ]
                        }
                    transfer = SessionTransfer
                        { transferMeta = testMeta sessionId
                        , transferTaskPlan = Just invalidPlan
                        , transferTurns = []
                        }
                    sessionDir =
                        root </> unsafeEncodeUtf (Text.unpack sessionId)
                importSessionTransfer pool root Nothing transfer
                    >>= (`shouldSatisfy` either (const True) (const False))
                Store.loadSession pool sessionId
                    `shouldReturn` Right Nothing
                doesDirectoryExist sessionDir `shouldReturn` False

                importSessionTransfer
                    pool
                    root
                    Nothing
                    (transfer { transferTaskPlan = Just sampleTaskPlan })
                    `shouldReturn` Right sessionId
                Store.loadSessionTaskPlan pool sessionId
                    `shouldReturn` Right (Just sampleStoredTaskPlan)

        it "materializes pending persistence into a resumable session ID" $
            withTempStore \store root -> do
                let pool = trustedPool store
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence (testCreate pool root)
                listDirectory root `shouldReturn` []
                PersistencePending _ reservedId tempDir <- readIORef slot
                doesDirectoryExist tempDir `shouldReturn` True
                modeOf tempDir `shouldReturn` 0o700
                ensurePersistenceSessionId persist
                    `shouldReturn` Just reservedId
                PersistenceActive handle <- readIORef slot
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                handle.sessionMeta.metaId `shouldBe` reservedId
                handle.sessionTempDir `shouldBe` tempDir
                loadSession pool root reservedId
                    `shouldReturn` Right (handle.sessionMeta, [])

        it "keeps committed materialization retryable across interrupts" $
            withTempStore \store root -> do
                let pool = trustedPool store
                    gatewayIdentity = "gateway-sha256:interrupt-test"
                    create =
                        (testCreate pool root)
                            { createTarget = ModelTarget
                                { targetProvider = OpenAIProvider
                                , targetConnectionId =
                                    organizationGatewayConnectionId
                                , targetModelId = "company-coder"
                                , targetWireModelId = "company-coder"
                                , targetDialect = GenericResponsesDialect
                                }
                            , createGatewayIdentity = Just gatewayIdentity
                            }
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence create
                PersistencePending _ reservedId tempDir <- readIORef slot
                let
                    sessionDir =
                        root </> unsafeEncodeUtf (Text.unpack reservedId)
                stored <- newEmptyMVar
                release <- newEmptyMVar
                let afterStored = putMVar stored () >> takeMVar release
                    interruptMaterialization =
                        withAsync
                            (ensurePersistenceSessionIdWithMaterializationHook
                                afterStored
                                persist)
                            \worker -> do
                                takeMVar stored
                                cancelWith worker UserInterrupt
                                waitCatch worker >>= \case
                                    Left exception ->
                                        (fromException exception
                                            :: Maybe AsyncException)
                                            `shouldBe` Just UserInterrupt
                                    Right sessionId ->
                                        expectationFailure
                                            ("interrupted materialization returned: "
                                                <> show sessionId)
                interruptMaterialization
                interruptMaterialization
                readIORef slot >>= \case
                    PersistencePending _ actualId _ ->
                        actualId `shouldBe` reservedId
                    PersistenceActive handle ->
                        expectationFailure
                            ("interrupted session published before retry: "
                                <> Text.unpack handle.sessionMeta.metaId)
                let recoveryPath =
                        sessionTempsRoot root
                            </> unsafeEncodeUtf
                                (".materialization-"
                                    <> Text.unpack reservedId
                                    <> ".json")
                doesFileExist recoveryPath `shouldReturn` True
                ensurePersistenceSessionId persist
                    `shouldReturn` Just reservedId
                readIORef slot >>= \case
                    PersistenceActive handle -> do
                        handle.sessionMeta.metaId `shouldBe` reservedId
                        handle.sessionMeta.metaGatewayIdentity
                            `shouldBe` Just gatewayIdentity
                        handle.sessionTempDir `shouldBe` tempDir
                        loadSession pool root reservedId
                            `shouldReturn` Right (handle.sessionMeta, [])
                    PersistencePending _ actualId _ ->
                        expectationFailure
                            ("committed session remained pending: "
                                <> Text.unpack actualId)
                doesDirectoryExist sessionDir `shouldReturn` True
                doesDirectoryExist tempDir `shouldReturn` True
                doesFileExist recoveryPath `shouldReturn` False

        it "persists a prompt snapshot after interrupted materialization" $
            withTempStore \store root -> do
                let pool = trustedPool store
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence (testCreate pool root)
                PersistencePending _ reservedId _ <- readIORef slot
                stored <- newEmptyMVar
                release <- newEmptyMVar
                let afterStored = putMVar stored () >> takeMVar release
                withAsync
                    (ensurePersistenceSessionIdWithMaterializationHook
                        afterStored
                        persist)
                    \worker -> do
                        takeMVar stored
                        cancelWith worker UserInterrupt
                        waitCatch worker >>= \case
                            Left exception ->
                                (fromException exception :: Maybe AsyncException)
                                    `shouldBe` Just UserInterrupt
                            Right sessionId ->
                                expectationFailure
                                    ("interrupted materialization returned: "
                                        <> show sessionId)
                let candidate = testPromptSnapshot reservedId
                handle <- ensureSessionWithPromptSnapshot slot candidate
                handle.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just candidate
                promptEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) promptEpoch
                    `shouldBe` Right (Just 0)

        it "creates and advances immutable prompt epochs before first use" $
            withTempStore \store root -> do
                let pool = trustedPool store
                PersistenceEnabled slot <-
                    newPendingPersistence (testCreate pool root)
                PersistencePending _ reservedId _ <- readIORef slot
                let initial = testPromptSnapshot reservedId
                handle <-
                    ensureSessionWithPromptSnapshot slot initial
                handle.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just initial
                initialEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) initialEpoch
                    `shouldBe` Right (Just 0)

                -- Consuming the one-shot generated context does not create a
                -- new epoch; the original value remains available to repair
                -- a crash before the first transcript turn is durable.
                unchanged <-
                    ensureSessionWithPromptSnapshot
                        slot
                        initial
                            { promptSnapshotGeneratedContext = Nothing
                            , promptSnapshotGrokContext = Nothing
                            }
                unchanged.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just initial
                unchangedEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) unchangedEpoch
                    `shouldBe` Right (Just 0)

                let advanced = initial
                        { promptSnapshotInstructions =
                            "updated persisted instructions"
                        , promptSnapshotGeneratedContext = Nothing
                        , promptSnapshotGrokContext = Nothing
                        }
                latest <-
                    ensureSessionWithPromptSnapshot slot advanced
                latest.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just advanced
                advancedEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) advancedEpoch
                    `shouldBe` Right (Just 1)
                loadSession pool root reservedId >>= \case
                    Right (loadedMeta, []) ->
                        loadedMeta.metaPromptSnapshot
                            `shouldBe` Just advanced
                    result ->
                        expectationFailure
                            ("unexpected prompt session: " <> show result)

        it "cleans scratch space for a pending session that never persists" $
            withTempStore \store root -> do
                let pool = trustedPool store
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence (testCreate pool root)
                PersistencePending _ _ tempDir <- readIORef slot
                cleanupPendingPersistence persist
                doesDirectoryExist tempDir `shouldReturn` False
                listDirectory root `shouldReturn` []

        it "recreates missing scratch space when a session resumes" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                removePathForcibly handle.sessionTempDir
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                _ <- newActivePersistence handle
                doesDirectoryExist handle.sessionTempDir `shouldReturn` True
                modeOf handle.sessionTempDir `shouldReturn` 0o700
