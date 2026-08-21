module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.Session
import Agent.Loop (TokenUsage(..))
import Agent.Responses.Types
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.Provider (Provider(..))
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import qualified System.FilePath as FilePath
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Session" do
    describe "sessionsRoot" do
        it "is ~/.haskell-agent/sessions" do
            sessionsRoot (fromFilePath "/home/marc")
                `shouldBe` fromFilePath "/home/marc/.haskell-agent/sessions"

    describe "dev resume pointer" do
        it "round-trips and clears under ~/.haskell-agent/dev-resume" $
            withTempDir "agent-home-" \home -> do
                readDevResumePointer home `shouldReturn` Nothing
                writeDevResumePointer home "2026-08-20-abcd1234"
                modeOf (devResumePointerPath home) `shouldReturn` 0o600
                readDevResumePointer home
                    `shouldReturn` Just "2026-08-20-abcd1234"
                clearDevResumePointer home
                readDevResumePointer home `shouldReturn` Nothing

    describe "sessionTitleFromPrompt" do
        it "collapses whitespace and truncates long prompts" do
            sessionTitleFromPrompt "  hello   world  " `shouldBe` "hello world"
            let long = Text.replicate 100 "a"
            Text.length (sessionTitleFromPrompt long) `shouldBe` 72

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
        it "round-trips meta and transcript items with private modes" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession (testCreate root)
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                doesFileExist handle.sessionMetaPath `shouldReturn` True
                handle.sessionMeta.metaTitle `shouldBe` "untitled"
                modeOf handle.sessionDir `shouldReturn` 0o700
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
                        meta.metaModel `shouldBe` "grok-4"
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
                slot <- newIORef (Left (testCreate root))
                listDirectory root `shouldReturn` []
                handle <- ensureSession slot
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                Right again <- readIORef slot
                again.sessionMeta.metaId `shouldBe` handle.sessionMeta.metaId

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
    , createProvider = XAIProvider
    , createModel = "grok-4"
    , createCwd = fromFilePath "/tmp/work"
    , createEffort = "low"
    , createTitleHint = Nothing
    }

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
        (action . fromFilePath)
