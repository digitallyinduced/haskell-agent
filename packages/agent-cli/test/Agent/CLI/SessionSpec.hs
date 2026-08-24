module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.Session
import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.SessionLock
import Agent.Dialect (DialectId(..))
import Agent.Loop (TokenUsage(..))
import Agent.Responses.Types
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import Agent.Provider (Provider(..))
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
    ( doesDirectoryExist
    , doesFileExist
    , createDirectoryIfMissing
    , listDirectory
    , removePathForcibly
    )
import qualified System.FilePath as FilePath
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.Session" do
    describe "sessionsRoot" do
        it "is ~/.haskell-agent/sessions" do
            sessionsRoot (fromFilePath "/home/marc")
                `shouldBe` fromFilePath "/home/marc/.haskell-agent/sessions"
            sessionTempsRoot
                (fromFilePath "/home/marc/.haskell-agent/sessions")
                `shouldBe`
                    fromFilePath "/home/marc/.haskell-agent/tmp/sessions"

    describe "sessionTitleFromPrompt" do
        it "collapses whitespace and keeps the first ten words" do
            sessionTitleFromPrompt "  hello   world  " `shouldBe` "hello world"
            sessionTitleFromPrompt "one two three four five six seven eight nine ten eleven"
                `shouldBe` "one two three four five six seven eight nine ten"
            sessionTitleFromPrompt "   " `shouldBe` "New session"
            Text.length (sessionTitleFromPrompt (Text.replicate 200 "x"))
                `shouldBe` 72

    describe "resumeHint" do
        it "prints a copy-pasteable --resume line with a quoted program name" do
            resumeHint "agent-cli" "2026-08-20-abcd1234"
                `shouldBe` "Resume this session with: 'agent-cli' --resume 2026-08-20-abcd1234"
            resumeHint "grok" "2026-08-20-abcd1234"
                `shouldBe` "Resume this session with: 'grok' --resume 2026-08-20-abcd1234"
            resumeHint "/path with spaces/agent-cli" "2026-08-20-abcd1234"
                `shouldBe`
                    "Resume this session with: '/path with spaces/agent-cli' --resume 2026-08-20-abcd1234"
            resumeHint "it's" "id"
                `shouldBe` "Resume this session with: 'it'\\''s' --resume id"

    describe "createSession/appendTurn/loadSession" do
        it "round-trips and clears ephemeral session activity" $
            withTempDir "agent-session-activity-" \root -> do
                handle <- createSession (testCreate root)
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
            withTempDir "agent-session-activity-resume-" \root -> do
                handle <- createSession (testCreate root)
                persistence <- newActivePersistence handle
                setPersistenceActivity
                    persistence
                    "provider_retry"
                    "Retrying."
                    Nothing
                _ <- newActivePersistence handle
                loadSessionActivity root handle.sessionMeta.metaId
                    `shouldReturn` Nothing

        it "round-trips meta and transcript items with private modes" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                doesDirectoryExist handle.sessionTempDir `shouldReturn` True
                doesFileExist handle.sessionMetaPath `shouldReturn` True
                handle.sessionMeta.metaTitle `shouldBe` "untitled"
                modeOf handle.sessionDir `shouldReturn` 0o700
                modeOf handle.sessionTempDir `shouldReturn` 0o700
                modeOf handle.sessionMetaPath `shouldReturn` 0o600

                let item = MessageItem ResponseMessage
                        { messageId = Nothing
                        , content = MessageContentParts
                            [InputTextPart "hi" Nothing KeyMap.empty]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , extraFields = KeyMap.empty
                        }
                    turn = SessionTurn
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
                handle' <- appendTurn handle turn
                handle'.sessionMeta.metaTitle `shouldBe` "hi there"
                handle'.sessionMeta.metaLastResponseId `shouldBe` Just "resp-1"
                handle'.sessionMeta.metaInputTokens `shouldBe` 10
                handle'.sessionMeta.metaOutputTokens `shouldBe` 4
                handle'.sessionMeta.metaCachedTokens `shouldBe` 2
                modeOf handle.sessionTranscriptPath `shouldReturn` 0o600

                loaded <- loadSession root handle.sessionMeta.metaId
                case loaded of
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta.metaId `shouldBe` handle.sessionMeta.metaId
                        meta.metaProvider `shouldBe` XAIProvider
                        meta.metaConnection `shouldBe` "xai"
                        meta.metaModel `shouldBe` "grok-4"
                        meta.metaDialect `shouldBe` GrokBuildDialect
                        meta.metaLegacySubagentTarget
                            `shouldBe` Just LegacySubagentTarget
                                { legacyTargetProvider = XAIProvider
                                , legacyTargetConnection = "xai"
                                , legacyTargetEffectiveModel = "grok-4"
                                , legacyTargetDialect = GrokBuildDialect
                                }
                        meta.metaCwd `shouldBe` fromFilePath "/tmp/work"
                        case turns of
                            [loadedTurn] -> do
                                loadedTurn.turnUserText `shouldBe` "hi there"
                                loadedTurn.turnItems `shouldBe` [item]
                                loadedTurn.turnUsage `shouldBe` Just TokenUsage
                                    { inputTokens = 10
                                    , outputTokens = 4
                                    , cachedTokens = 2
                                    }
                                sessionUsageFromTurns meta turns `shouldBe` TokenUsage
                                    { inputTokens = 10
                                    , outputTokens = 4
                                    , cachedTokens = 2
                                    }
                            other ->
                                expectationFailure
                                    ("expected one turn, got " <> show (length other))

                listed <- listSessions root
                map (.metaId) listed `shouldBe` [handle.sessionMeta.metaId]

                loadSessionHandle root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (loadedHandle, _) ->
                        loadedHandle.sessionTempDir
                            `shouldBe` handle.sessionTempDir

        it "combines append metadata and a caller transition in one result" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                let turn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "count this turn"
                        , turnAssistantText = Just "done"
                        , turnError = Nothing
                        , turnResponseId = Just "resp-counted"
                        , turnItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 7
                            , outputTokens = 3
                            , cachedTokens = 1
                            }
                        }
                handle' <- appendTurnWithMetaUpdate handle turn \meta ->
                    meta { metaTitleUserTurns = 1 }

                handle'.sessionMeta.metaTitle `shouldBe` "count this turn"
                handle'.sessionMeta.metaLastResponseId
                    `shouldBe` Just "resp-counted"
                handle'.sessionMeta.metaInputTokens `shouldBe` 7
                handle'.sessionMeta.metaOutputTokens `shouldBe` 3
                handle'.sessionMeta.metaCachedTokens `shouldBe` 1
                handle'.sessionMeta.metaTitleUserTurns `shouldBe` 1

                loaded <- loadSession root handle.sessionMeta.metaId
                case loaded of
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta `shouldBe` handle'.sessionMeta
                        turns `shouldBe` [turn]

        it "keeps synthetic turns out of title and usage metadata" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                let turn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/clear"
                        , turnAssistantText = Just "Started a new conversation."
                        , turnError = Nothing
                        , turnResponseId = Just "resp-marker"
                        , turnItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 11
                            , outputTokens = 5
                            , cachedTokens = 3
                            }
                        }
                handle' <- appendTurnKeepTitle handle turn

                handle'.sessionMeta.metaTitle `shouldBe` "untitled"
                handle'.sessionMeta.metaLastResponseId
                    `shouldBe` Just "resp-marker"
                handle'.sessionMeta.metaInputTokens `shouldBe` 0
                handle'.sessionMeta.metaOutputTokens `shouldBe` 0
                handle'.sessionMeta.metaCachedTokens `shouldBe` 0
                modeOf handle.sessionTranscriptPath `shouldReturn` 0o600

                loadSession root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta `shouldBe` handle'.sessionMeta
                        turns `shouldBe` [turn]

        it "commits a compact marker with a cleared response id" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                let completedTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "before compact"
                        , turnAssistantText = Just "done"
                        , turnError = Nothing
                        , turnResponseId = Just "resp-old"
                        , turnItems = []
                        , turnUsage = Nothing
                        }
                    compactTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText =
                            Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        }
                withResponse <- appendTurn handle completedTurn
                final <-
                    appendTurnWithMetaUpdate withResponse compactTurn
                        \meta -> meta { metaLastResponseId = Nothing }

                final.sessionMeta.metaLastResponseId `shouldBe` Nothing
                loadSession root final.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta.metaLastResponseId `shouldBe` Nothing
                        turns `shouldBe` [completedTurn, compactTurn]

        it "adds auxiliary usage without appending a transcript turn" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                updated <- addSessionUsage TokenUsage
                    { inputTokens = 90
                    , outputTokens = 7
                    , cachedTokens = 40
                    }
                    handle
                updated.sessionMeta.metaInputTokens `shouldBe` 90
                updated.sessionMeta.metaOutputTokens `shouldBe` 7
                updated.sessionMeta.metaCachedTokens `shouldBe` 40
                doesFileExist updated.sessionTranscriptPath
                    `shouldReturn` False

                loadSession root updated.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        turns `shouldBe` []
                        sessionUsageFromTurns meta turns `shouldBe` TokenUsage
                            { inputTokens = 90
                            , outputTokens = 7
                            , cachedTokens = 40
                            }

        it "combines auxiliary and turn usage exactly once on resume" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                withCompaction <- addSessionUsage TokenUsage
                    { inputTokens = 90
                    , outputTokens = 7
                    , cachedTokens = 40
                    }
                    handle
                let compactTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText = Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        }
                    normalTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "continue"
                        , turnAssistantText = Just "done"
                        , turnError = Nothing
                        , turnResponseId = Just "resp-next"
                        , turnItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                        }
                withMarker <- appendTurn withCompaction compactTurn
                final <- appendTurn withMarker normalTurn

                loadSession root final.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        turns `shouldBe` [compactTurn, normalTurn]
                        sessionUsageFromTurns meta turns `shouldBe` TokenUsage
                            { inputTokens = 100
                            , outputTokens = 11
                            , cachedTokens = 42
                            }

        it "round-trips an explicit OpenRouter dialect" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession $
                    (testCreate root)
                        { createTarget = ModelTarget
                            { targetProvider = OpenRouterProvider
                            , targetConnectionId = "openrouter"
                            , targetModelId = "openai/gpt-5.1"
                            , targetWireModelId = "openai/gpt-5.1"
                            , targetDialect = CodexDialect
                            }
                        }
                loadSession root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, _) -> do
                        meta.metaDialect `shouldBe` CodexDialect
                        meta.metaTransportModel
                            `shouldBe` Just "openai/gpt-5.1"

        it "decodes legacy OpenRouter metadata with the old Grok dialect" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession $
                    (testCreate root)
                        { createTarget = ModelTarget
                            { targetProvider = OpenRouterProvider
                            , targetConnectionId = "openrouter"
                            , targetModelId = "openai/gpt-5.1"
                            , targetWireModelId = "openai/gpt-5.1"
                            , targetDialect = CodexDialect
                            }
                        }
                rewriteMetaObject handle.sessionMetaPath $
                    KeyMap.delete "legacySubagentTarget"
                        . KeyMap.delete "connection"
                        . KeyMap.delete "transportModel"
                        . KeyMap.delete "dialect"
                loadSession root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, _) -> do
                        meta.metaDialect `shouldBe` GrokBuildDialect
                        meta.metaConnection `shouldBe` "openrouter"
                        meta.metaTransportModel `shouldBe` Nothing
                        meta.metaLegacySubagentTarget `shouldBe` Nothing
                        sessionLegacySubagentTarget meta
                            `shouldBe` LegacySubagentTarget
                                { legacyTargetProvider = OpenRouterProvider
                                , legacyTargetConnection = "openrouter"
                                , legacyTargetEffectiveModel =
                                    "openai/gpt-5.1"
                                , legacyTargetDialect = GrokBuildDialect
                                }

        it "keeps legacy child provenance after the root target changes" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession $
                    (testCreate root)
                        { createTarget = ModelTarget
                            { targetProvider = OpenRouterProvider
                            , targetConnectionId = "openrouter"
                            , targetModelId = "openai/gpt-5.1"
                            , targetWireModelId = "openai/gpt-5.1"
                            , targetDialect = CodexDialect
                            }
                        }
                let legacyTarget =
                        sessionLegacySubagentTarget handle.sessionMeta
                    retargeted = handle.sessionMeta
                        { metaModel = "x-ai/grok-4"
                        , metaTransportModel = Just "x-ai/grok-4"
                        , metaDialect = GrokBuildDialect
                        , metaLegacySubagentTarget = Just legacyTarget
                        }
                writeSessionMeta handle.sessionMetaPath retargeted
                loadSession root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, _) -> do
                        meta.metaDialect `shouldBe` GrokBuildDialect
                        sessionLegacySubagentTarget meta
                            `shouldBe` legacyTarget

        it "rejects an explicit unknown session dialect" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                rewriteMetaObject handle.sessionMetaPath $
                    KeyMap.insert "dialect" (Aeson.String "retired")
                loadSession root handle.sessionMeta.metaId >>= \case
                    Left err ->
                        err `shouldSatisfy` Text.isInfixOf "unknown dialect"
                    Right _ ->
                        expectationFailure "expected dialect decode failure"

        it "rejects a dialect incompatible with the persisted provider" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                rewriteMetaObject handle.sessionMetaPath $
                    KeyMap.insert "dialect" (Aeson.String "codex")
                loadSession root handle.sessionMeta.metaId >>= \case
                    Left err ->
                        err `shouldSatisfy` Text.isInfixOf "incompatible"
                    Right _ ->
                        expectationFailure
                            "expected provider/dialect compatibility failure"

        it "rejects unsupported schema versions" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                let bad = handle.sessionMeta { metaVersion = 99 }
                writeSessionMeta handle.sessionMetaPath bad
                loadSession root handle.sessionMeta.metaId
                    >>= \case
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf "unsupported session schema"
                        Right _ -> expectationFailure "expected schema failure"

        it "rejects session ids that escape the sessions root" $
            withTempDir "agent-sessions-" \root -> do
                isValidSessionId "normal-id" `shouldBe` True
                isValidSessionId "../outside" `shouldBe` False
                isValidSessionId "nested/id" `shouldBe` False
                loadSession root "../outside"
                    `shouldReturn` Left "invalid session id"

        it "reports a missing session with a Text error" $
            withTempDir "agent-sessions-" \root ->
                loadSession root "missing-session"
                    `shouldReturn` Left "session not found: missing-session"

        it "deletes a persisted session without escaping the session root" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                deleteSession root handle.sessionMeta.metaId
                    `shouldReturn` Right ()
                doesDirectoryExist handle.sessionDir `shouldReturn` False
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                deleteSession root "../outside"
                    `shouldReturn` Left "invalid session id"

        it "does not delete a session while another process owns its lock" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                acquireSessionLock
                    handle.sessionDir
                    handle.sessionMeta.metaId >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right lock -> do
                            deleteSession root handle.sessionMeta.metaId
                                `shouldReturn`
                                    Left "cannot delete a running session"
                            releaseSessionLock lock
                            deleteSession root handle.sessionMeta.metaId
                                `shouldReturn` Right ()

        it "rejects metadata whose id does not match its directory" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                writeSessionMeta handle.sessionMetaPath
                    handle.sessionMeta { metaId = "different-session" }
                loadSession root handle.sessionMeta.metaId
                    `shouldReturn` Left "session id does not match directory"

        it "reports malformed metadata and transcript lines as Text" $
            withTempDir "agent-sessions-" \root -> do
                badMeta <- createSession (testCreate root)
                writeFile (toFilePath badMeta.sessionMetaPath) "{not-json"
                loadSession root badMeta.sessionMeta.metaId >>= \case
                    Left err ->
                        err `shouldSatisfy` Text.isInfixOf "meta.json"
                    Right _ -> expectationFailure "expected metadata decode failure"

                badTranscript <- createSession (testCreate root)
                writeFile
                    (toFilePath badTranscript.sessionTranscriptPath)
                    "{not-json}\n"
                loadSession root badTranscript.sessionMeta.metaId >>= \case
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "invalid transcript line"
                    Right _ -> expectationFailure "expected transcript decode failure"

        it "creates a pending session only when ensureSession runs" $
            withTempDir "agent-sessions-" \root -> do
                PersistenceEnabled slot <- newPendingPersistence (testCreate root)
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
            withTempDir "agent-sessions-" \root -> do
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence (testCreate root)
                PersistencePending _ _ tempDir <- readIORef slot
                cleanupPendingPersistence persist
                doesDirectoryExist tempDir `shouldReturn` False
                listDirectory root `shouldReturn` []

        it "recreates missing scratch space when a session resumes" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
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

testCreate :: OsPath -> SessionCreate
testCreate root = SessionCreate
    { createRoot = root
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

rewriteMetaObject
    :: OsPath
    -> (KeyMap.KeyMap Aeson.Value -> KeyMap.KeyMap Aeson.Value)
    -> IO ()
rewriteMetaObject path update = do
    bytes <- LBS.readFile (toFilePath path)
    case Aeson.eitherDecode' bytes of
        Right (Aeson.Object object) ->
            LBS.writeFile
                (toFilePath path)
                (Aeson.encode (Aeson.Object (update object)))
        Right other ->
            expectationFailure ("expected metadata object, got " <> show other)
        Left err ->
            expectationFailure ("failed to decode metadata: " <> err)

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

modeOf :: OsPath -> IO Integer
modeOf path = do
    status <- getFileStatus (toFilePath path)
    pure (fromIntegral (fileMode status `mod` 0o1000))

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> prefix))
        Directory.removeDirectoryRecursive
        \basePath -> do
            let root =
                    fromFilePath basePath
                        </> fromFilePath ".haskell-agent"
                        </> fromFilePath "sessions"
            createDirectoryIfMissing True root
            action root
