module Agent.CLI.AgentSessionsSpec (spec) where

import Agent.CLI.AgentSessions
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Session
import Agent.Loop (defaultLoopDispatch)
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.Provider (Provider(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types (AppTool(..), appToolHandlers)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import qualified System.FilePath as FilePath
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.AgentSessions" do
    it "registers create/read/message tools with mutating flags" $
        withTempEnv \env _ -> do
            map (\tool -> (tool.appToolName, tool.appToolReadOnly))
                (agentSessionTools env)
                `shouldBe`
                    [ ("create_agent_session", False)
                    , ("read_agent_session", True)
                    , ("send_agent_session_message", False)
                    ]

    it "creates a persisted session and launches its first turn" $
        withTempEnv \env launched -> do
            result <- runTool env "create_agent_session"
                "{\"message\":\"investigate this\",\"title\":\"worker\",\"model\":\"model-2\",\"reasoning_effort\":\"high\"}"
            result `shouldSatisfy` Text.isInfixOf "\"status\":\"running\""
            [(handle, message)] <- readIORef launched
            message `shouldBe` "investigate this"
            handle.sessionMeta.metaTitle `shouldBe` "worker"
            handle.sessionMeta.metaModel `shouldBe` "model-2"
            handle.sessionMeta.metaEffort `shouldBe` "high"
            loadSession env.toolsRoot handle.sessionMeta.metaId
                `shouldReturn` Right (handle.sessionMeta, [])

    it "reads recent turns without exposing raw response items" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsRoot)
            _ <- appendTurn handle SessionTurn
                { turnAt = fixedTime
                , turnUserText = "question"
                , turnAssistantText = Just "answer"
                , turnError = Nothing
                , turnResponseId = Nothing
                , turnItems = []
                , turnUsage = Nothing
                }
            result <- runTool env "read_agent_session" $
                "{\"session_id\":\"" <> handle.sessionMeta.metaId <> "\"}"
            result `shouldSatisfy` Text.isInfixOf "\"user\":\"question\""
            result `shouldSatisfy` Text.isInfixOf "\"assistant\":\"answer\""
            result `shouldNotSatisfy` Text.isInfixOf "\"items\""

    it "starts a follow-up turn and rejects messaging the current session" $
        withTempEnv \env launched -> do
            handle <- createSession (testCreate env.toolsRoot)
            let target = handle.sessionMeta.metaId
                targetEnv = env { toolsCurrentSessionId = pure (Just "other") }
            result <- runTool targetEnv "send_agent_session_message" $
                "{\"session_id\":\"" <> target <> "\",\"message\":\"continue\"}"
            result `shouldSatisfy` Text.isInfixOf "\"status\":\"running\""
            [(launchedHandle, message)] <- readIORef launched
            launchedHandle.sessionMeta.metaId `shouldBe` target
            message `shouldBe` "continue"

            let selfEnv = env { toolsCurrentSessionId = pure (Just target) }
            selfResult <- runTool selfEnv "send_agent_session_message" $
                "{\"session_id\":\"" <> target <> "\",\"message\":\"loop\"}"
            selfResult `shouldSatisfy`
                Text.isInfixOf "cannot message the current agent session"

    it "rejects traversal session ids" $
        withTempEnv \env _ -> do
            result <- runTool env "read_agent_session"
                "{\"session_id\":\"../outside\"}"
            result `shouldSatisfy` Text.isInfixOf "invalid session id"

    it "serializes background turns with a cross-process session lock" $
        withTempDir "agent-session-runtime-" \root -> do
            script <- writeFakeAgent root
            withExecutableOverride script do
                handle <- createSession (testCreateAt root root)
                manager <- newSessionProcessManager root
                first <- launchSessionTurn manager True ApproveAll handle "one"
                first `shouldSatisfy` either (const False) (const True)
                second <- launchSessionTurn manager True ApproveAll handle "two"
                second `shouldSatisfy` \case
                    Left err -> "already running" `Text.isInfixOf` err
                    Right _ -> False
                threadDelay 500000
                sessionProcessStatus manager handle.sessionMeta.metaId
                    `shouldReturn` "completed"
                closeSessionProcessManager manager

runTool :: AgentSessionToolsEnv -> Text.Text -> Text.Text -> IO Text.Text
runTool env name arguments = do
    result <- dispatchToolCall defaultLoopDispatch
        (appToolHandlers (agentSessionTools env))
        (functionToolCall "call-1" name arguments)
    pure result.output

withTempEnv
    :: (AgentSessionToolsEnv -> IORef [(SessionHandle, Text.Text)] -> IO a)
    -> IO a
withTempEnv action =
    withTempDir "agent-session-tools-" \root -> do
        launched <- newIORef []
        let launch handle message = do
                modifyIORef' launched (<> [(handle, message)])
                pure (Right "started")
            env = AgentSessionToolsEnv
                { toolsRoot = root
                , toolsProvider = XAIProvider
                , toolsModel = "model-1"
                , toolsCwd = fromFilePath "/tmp/work"
                , toolsEffort = "low"
                , toolsCurrentSessionId = pure Nothing
                , toolsLaunchTurn = launch
                , toolsSessionStatus = const (pure "running")
                }
        action env launched

testCreate :: OsPath -> SessionCreate
testCreate root = SessionCreate
    { createRoot = root
    , createProvider = XAIProvider
    , createModel = "model-1"
    , createCwd = fromFilePath "/tmp/work"
    , createEffort = "low"
    , createTitleHint = Just "test"
    }

testCreateAt :: OsPath -> OsPath -> SessionCreate
testCreateAt root cwd = (testCreate root) { createCwd = cwd }

writeFakeAgent :: OsPath -> IO FilePath
writeFakeAgent root = do
    let path = toFilePath root FilePath.</> "fake-agent-cli"
    writeFile path "#!/bin/sh\nsleep 0.2\nexit 0\n"
    permissions <- Directory.getPermissions path
    Directory.setPermissions path permissions { Directory.executable = True }
    pure path

withExecutableOverride :: FilePath -> IO a -> IO a
withExecutableOverride executable action =
    bracket
        (do
            previous <- lookupEnv "HASKELL_AGENT_EXECUTABLE"
            setEnv "HASKELL_AGENT_EXECUTABLE" executable
            pure previous)
        (\previous -> case previous of
            Nothing -> unsetEnv "HASKELL_AGENT_EXECUTABLE"
            Just value -> setEnv "HASKELL_AGENT_EXECUTABLE" value)
        (const action)

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 21) (secondsToDiffTime 0)

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> prefix))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)
