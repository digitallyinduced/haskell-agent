module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.Session
import Agent.CLI.Session.StoreCodec
    ( fromStoredResponseItem
    , toStoredResponseItem
    )
import Agent.CLI.Models (ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Loop (TokenUsage(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Store.Postgres
    ( Store
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , storeConfig
    , trustedPool
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removePathForcibly
    )
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

toFilePath :: OsPath -> FilePath
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.Session" do
    describe "pure compatibility helpers" do
        it "keeps the legacy artifact root and safe ids" do
            sessionsRoot (fromFilePath "/home/marc")
                `shouldBe` fromFilePath "/home/marc/.haskell-agent/sessions"
            sessionTempsRoot
                (fromFilePath "/home/marc/.haskell-agent/sessions")
                `shouldBe`
                    fromFilePath "/home/marc/.haskell-agent/tmp/sessions"
            isValidSessionId "normal-id" `shouldBe` True
            isValidSessionId "../outside" `shouldBe` False
            isValidSessionId "nested/id" `shouldBe` False

        it "derives bounded titles and shell-safe resume hints" do
            sessionTitleFromPrompt
                "one two three four five six seven eight nine ten eleven"
                `shouldBe` "one two three four five six seven eight nine ten"
            resumeHint "it's" "id"
                `shouldBe` "Resume this session with: 'it'\\''s' --resume id"

        it "keeps response-item JSON codecs at the CLI storage boundary" do
            let items =
                    [ MessageItem ResponseMessage
                        { messageId = Just "message-1"
                        , content = MessageContentText "hello"
                        , role = RoleAssistant
                        , status = Just ItemCompleted
                        , phase = Nothing
                        , extraFields =
                            KeyMap.singleton
                                "provider_extension"
                                (Aeson.Bool True)
                        }
                    , MessageItem ResponseMessage
                        { messageId = Just "message-2"
                        , content = MessageContentParts
                            [ InputTextPart
                                { text = "input"
                                , promptCacheBreakpoint =
                                    Just
                                        (Aeson.object
                                            ["scope" Aeson..= ("turn" :: Text.Text)])
                                , extraFields = KeyMap.empty
                                }
                            , OutputTextPart
                                { text = "output"
                                , annotations =
                                    Just
                                        [ Aeson.object
                                            ["type" Aeson..= ("citation" :: Text.Text)]
                                        ]
                                , logprobs =
                                    Just
                                        [ Aeson.object
                                            ["token" Aeson..= ("output" :: Text.Text)]
                                        ]
                                , extraFields = KeyMap.empty
                                }
                            , UnknownContentPart TaggedObject
                                { tag = "provider_content"
                                , fields =
                                    KeyMap.singleton
                                        "opaque"
                                        (Aeson.Bool True)
                                }
                            ]
                        , role = RoleDeveloper
                        , status = Just ItemInProgress
                        , phase = Just "commentary"
                        , extraFields = KeyMap.empty
                        }
                    , FunctionCallItem FunctionCall
                        { itemId = Just "call-item"
                        , callId = "call-1"
                        , name = "shell"
                        , arguments = "{\"command\":\"pwd\"}"
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , FunctionCallOutputItem FunctionCallOutput
                        { itemId = Just "output-item"
                        , callId = "call-1"
                        , output =
                            Aeson.object
                                ["stdout" Aeson..= ("/tmp/project" :: Text.Text)]
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , CustomToolCallItem CustomToolCall
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = "apply_patch"
                        , input = "*** Begin Patch"
                        , status = Nothing
                        , extraFields = KeyMap.empty
                        }
                    , CustomToolCallOutputItem CustomToolCallOutput
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = Just "apply_patch"
                        , output = Aeson.String "Done"
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , ReasoningItemValue ReasoningItem
                        { itemId = Just "reasoning-1"
                        , summary =
                            [ ReasoningSummaryPart
                                { partType = "summary_text"
                                , text = Just "Checked the schema"
                                , extraFields = KeyMap.empty
                                }
                            ]
                        , content =
                            Just
                                [ ReasoningTextPart
                                    { text = "private placeholder"
                                    , extraFields = KeyMap.empty
                                    }
                                ]
                        , encryptedContent = Just "encrypted"
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , ItemReferenceValue ItemReference
                        { itemId = "call-item"
                        , extraFields = KeyMap.empty
                        }
                    , KnownResponseItem ItemCompactionTrigger TaggedObject
                        { tag = "compaction_trigger"
                        , fields = KeyMap.empty
                        }
                    , UnknownResponseItem TaggedObject
                        { tag = "provider_item"
                        , fields =
                            KeyMap.singleton "payload" (Aeson.String "opaque")
                        }
                    ]
            traverse fromStoredResponseItem (map toStoredResponseItem items)
                `shouldBe` Right items

    describe "PostgreSQL session persistence" do
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
                            [InputTextPart "hi" Nothing KeyMap.empty]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , extraFields = KeyMap.empty
                        }
                    normalTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "hi there"
                        , turnAssistantText = Just "hello"
                        , turnError = Nothing
                        , turnResponseId = Just "resp-1"
                        , turnItems = [item]
                        , turnUsage = Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                        }
                    compactTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText = Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
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

                listed <- listSessions pool root
                map (.metaId) listed `shouldBe` [handle.sessionMeta.metaId]
                deleteSession pool root handle.sessionMeta.metaId
                    `shouldReturn` Right ()
                doesDirectoryExist handle.sessionDir `shouldReturn` False
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                deleteSession pool root "../outside"
                    `shouldReturn` Left "invalid session id"
                loadSession pool root handle.sessionMeta.metaId
                    `shouldReturn`
                        Left ("session not found: " <> handle.sessionMeta.metaId)

        it "imports a legacy meta.json and JSONL transcript once" $
            withTempStore \store root -> do
                let
                    pool = trustedPool store
                    sessionId = "2026-08-19-legacy"
                    dir = root </> unsafeEncodeUtf (Text.unpack sessionId)
                    metaPath = dir </> unsafeEncodeUtf "meta.json"
                    transcriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
                    meta = testMeta sessionId
                    turn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "from disk"
                        , turnAssistantText = Just "imported"
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
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

        it "keeps pending persistence lazy" $
            withTempStore \store root -> do
                let pool = trustedPool store
                PersistenceEnabled slot <-
                    newPendingPersistence (testCreate pool root)
                listDirectory root `shouldReturn` []
                PersistencePending _ reservedId tempDir <- readIORef slot
                doesDirectoryExist tempDir `shouldReturn` True
                modeOf tempDir `shouldReturn` 0o700
                handle <- ensureSession slot
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                handle.sessionMeta.metaId `shouldBe` reservedId
                handle.sessionTempDir `shouldBe` tempDir
                PersistenceActive again <- readIORef slot
                again.sessionMeta.metaId `shouldBe` handle.sessionMeta.metaId

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

    describe "json codec" do
        it "encodes and decodes SessionTurn" do
            let turn = SessionTurn
                    { turnAt = fixedTime
                    , turnUserText = "q"
                    , turnAssistantText = Nothing
                    , turnError = Just "cancelled"
                    , turnResponseId = Nothing
                    , turnItems = []
                    , turnUsage = Nothing
                    }
            Aeson.eitherDecode (Aeson.encode turn) `shouldBe` Right turn

testCreate :: StorePool -> OsPath -> SessionCreate
testCreate pool root = SessionCreate
    { createPool = pool
    , createRoot = root
    , createTarget = ModelTarget
        { targetProvider = XAIProvider
        , targetConnectionId = "xai"
        , targetModelId = "grok-4"
        , targetWireModelId = "grok-4"
        , targetDialect = GrokBuildDialect
        }
    , createCwd = fromFilePath "/tmp/work"
    , createEffort = "low"
    , createTitleHint = Nothing
    , createTitleIsManual = False
    }

testMeta :: Text.Text -> SessionMeta
testMeta sessionId = SessionMeta
    { metaVersion = 1
    , metaId = sessionId
    , metaCreatedAt = fixedTime
    , metaUpdatedAt = fixedTime
    , metaProvider = XAIProvider
    , metaConnection = "xai"
    , metaModel = "grok-4"
    , metaTransportModel = Just "grok-4"
    , metaDialect = GrokBuildDialect
    , metaLegacySubagentTarget = Just LegacySubagentTarget
        { legacyTargetProvider = XAIProvider
        , legacyTargetConnection = "xai"
        , legacyTargetEffectiveModel = "grok-4"
        , legacyTargetDialect = GrokBuildDialect
        }
    , metaCwd = fromFilePath "/tmp/work"
    , metaEffort = "low"
    , metaTitle = "legacy"
    , metaTitleIsManual = False
    , metaTitleRefreshIndex = 0
    , metaTitleUserTurns = 0
    , metaLastResponseId = Nothing
    , metaInputTokens = 0
    , metaOutputTokens = 0
    , metaCachedTokens = 0
    }

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

modeOf :: OsPath -> IO Integer
modeOf path = do
    status <- getFileStatus (toFilePath path)
    pure (fromIntegral (fileMode status `mod` 0o1000))

withTempStore :: (Store -> OsPath -> IO a) -> IO a
withTempStore action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "hs"))
        Directory.removeDirectoryRecursive
        \basePath -> do
            let
                stateDirectory = basePath FilePath.</> ".haskell-agent"
                sessionsDirectory =
                    stateDirectory FilePath.</> "sessions"
                config = defaultManagedPostgresConfig stateDirectory ""
            Directory.createDirectoryIfMissing True sessionsDirectory
            bracket
                (openStore config >>= either
                    (fail . Text.unpack . renderStoreError)
                    pure)
                (\store -> do
                    closeStore store
                    _ <- stopManagedPostgres (storeConfig store)
                    pure ())
                (\store -> action store (fromFilePath sessionsDirectory))
