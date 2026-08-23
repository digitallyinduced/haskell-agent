module Agent.CLI.AgentSessionsSpec (spec) where

import Agent.CLI.AgentSessions
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Session
import Agent.CLI.SessionLock
import Agent.Loop (defaultLoopDispatch)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , appToolHandlers
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, bracket, finally, try)
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import qualified System.FilePath as FilePath
import System.Posix.Temp (mkdtemp)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import Test.Hspec

isReadOnly :: ApprovalRule -> Bool
isReadOnly AlwaysReadOnly = True
isReadOnly _ = False

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.AgentSessions" do
    it "registers create/read/message tools with mutating flags" $
        withTempEnv \env _ -> do
            map (\tool -> (tool.appToolName, isReadOnly tool.appToolApproval))
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
            handle.sessionMeta.metaTitleIsManual `shouldBe` True
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
                waitForSessionStatus
                    manager
                    handle.sessionMeta.metaId
                    "completed"
                closeSessionProcessManager manager

    it "keeps an advisory lock until its owner releases it" $
        withTempDir "agent-session-lock-" \root -> do
            handle <- createSession (testCreateAt root root)
            acquireSessionLock
                handle.sessionDir
                handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right lock -> do
                        sessionLockIsActive (sessionLockPath handle.sessionDir)
                            `shouldReturn` True
                        threadDelay 5100000
                        acquireSessionLock
                            handle.sessionDir
                            handle.sessionMeta.metaId >>= \case
                                Left err ->
                                    err `shouldSatisfy`
                                        Text.isInfixOf "already running"
                                Right other -> do
                                    releaseSessionLock other
                                    expectationFailure
                                        "acquired an already-held session lock"
                        releaseSessionLock lock
                        Directory.doesFileExist
                            (sessionLockPath handle.sessionDir)
                            `shouldReturn` True
                        sessionLockIsActive (sessionLockPath handle.sessionDir)
                            `shouldReturn` False

    it "releases an advisory lock when its process crashes" $
        withTempDir "agent-session-lock-crash-" \root -> do
            handle <- createSession (testCreateAt root root)
            let marker = toFilePath root FilePath.</> "locked"
            pid <- forkProcess do
                acquireSessionLock
                    handle.sessionDir
                    handle.sessionMeta.metaId >>= \case
                        Left _ -> pure ()
                        Right _ -> do
                            writeFile marker "locked"
                            threadDelay 30000000
            let stopChild = do
                    _ <- try @_ @SomeException (signalProcess sigKILL pid)
                    pure ()
            flip finally stopChild do
                waitForFile marker
                acquireSessionLock
                    handle.sessionDir
                    handle.sessionMeta.metaId >>= \case
                        Left err ->
                            err `shouldSatisfy` Text.isInfixOf "already running"
                        Right lock -> do
                            releaseSessionLock lock
                            expectationFailure "acquired the child process lock"
                signalProcess sigKILL pid
                _ <- getProcessStatus True False pid
                reacquired <- acquireSessionLock
                    handle.sessionDir
                    handle.sessionMeta.metaId
                case reacquired of
                    Left err -> expectationFailure (Text.unpack err)
                    Right lock -> releaseSessionLock lock

    it "reports a managed child readiness failure" $
        withTempDir "agent-session-runtime-" \root -> do
            script <- writeFakeAgentError root "could not acquire lock"
            withExecutableOverride script do
                handle <- createSession (testCreateAt root root)
                manager <- newSessionProcessManager root
                launchSessionTurn manager True ApproveAll handle "one"
                    `shouldReturn` Left "could not acquire lock"
                closeSessionProcessManager manager

    it "does not terminate background sessions when the manager closes" $
        withTempDir "agent-session-runtime-" \root -> do
            let marker = toFilePath root FilePath.</> "finished"
            script <- writeFakeAgentBody root
                ("sleep 0.2\nprintf done > " <> shellQuote marker <> "\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt root root)
                manager <- newSessionProcessManager root
                _ <- launchSessionTurn manager True ApproveAll handle "one"
                closeSessionProcessManager manager
                waitForFile marker

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
    , createTitleIsManual = False
    }

testCreateAt :: OsPath -> OsPath -> SessionCreate
testCreateAt root cwd = (testCreate root) { createCwd = cwd }

writeFakeAgent :: OsPath -> IO FilePath
writeFakeAgent root = do
    writeFakeAgentBody root "sleep 0.2\nexit 0\n"

writeFakeAgentBody :: OsPath -> String -> IO FilePath
writeFakeAgentBody root body = do
    let path = toFilePath root FilePath.</> "fake-agent-cli"
    writeFile path $
        "#!/bin/sh\nprintf 'ready\\n' > \"$HASKELL_AGENT_MANAGED_SESSION_READY\"\n"
            <> body
    permissions <- Directory.getPermissions path
    Directory.setPermissions path permissions { Directory.executable = True }
    pure path

writeFakeAgentError :: OsPath -> String -> IO FilePath
writeFakeAgentError root message = do
    let path = toFilePath root FilePath.</> "fake-agent-cli-error"
    writeFile path $
        "#!/bin/sh\nprintf 'error\\n%s' "
            <> shellQuote message
            <> " > \"$HASKELL_AGENT_MANAGED_SESSION_READY\"\nexit 1\n"
    permissions <- Directory.getPermissions path
    Directory.setPermissions path permissions { Directory.executable = True }
    pure path

waitForFile :: FilePath -> IO ()
waitForFile path = go (50 :: Int)
  where
    go 0 = expectationFailure ("timed out waiting for " <> path)
    go attempts = do
        exists <- Directory.doesFileExist path
        if exists
            then pure ()
            else threadDelay 20000 >> go (attempts - 1)

waitForSessionStatus
    :: SessionProcessManager
    -> Text.Text
    -> Text.Text
    -> IO ()
waitForSessionStatus manager sessionId expected = go (100 :: Int)
  where
    go attempts
        | attempts <= 0 = do
            actual <- sessionProcessStatus manager sessionId
            actual `shouldBe` expected
        | otherwise = do
            actual <- sessionProcessStatus manager sessionId
            if actual == expected
                then pure ()
                else threadDelay 20000 >> go (attempts - 1)

shellQuote :: FilePath -> String
shellQuote path = "'" <> concatMap escape path <> "'"
  where
    escape '\'' = "'\\''"
    escape char = [char]

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
