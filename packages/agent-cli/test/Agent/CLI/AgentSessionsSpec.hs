module Agent.CLI.AgentSessionsSpec (spec) where

import Agent.CLI.AgentSessions
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Agent.CLI.ManagedTurn (managedTurnRequestFromText)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Session
import Agent.CLI.SessionLock
import Agent.Dialect (DialectId(..))
import Agent.Loop (defaultLoopDispatch)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import Agent.Provider (Provider(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolSchema(..)
    , appToolHandlers
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.Store.Postgres
    ( closeStore
    , defaultManagedPostgresConfig
    , openStore
    , storeConfig
    , trustedPool
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (SomeException, bracket, finally, try)
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import qualified System.FilePath as FilePath
import System.Posix.Temp (mkdtemp)
import System.Posix.Process (forkProcess, getProcessID, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import Test.Hspec

isReadOnly :: ApprovalRule -> Bool
isReadOnly AlwaysReadOnly = True
isReadOnly _ = False

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

toFilePath :: OsPath -> FilePath
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
            result `shouldSatisfy` Text.isInfixOf "Status: running"
            [(handle, message)] <- readIORef launched
            message `shouldBe` "investigate this"
            handle.sessionMeta.metaTitle `shouldBe` "worker"
            handle.sessionMeta.metaTitleIsManual `shouldBe` True
            handle.sessionMeta.metaModel `shouldBe` "model-2"
            handle.sessionMeta.metaDialect `shouldBe` GrokBuildDialect
            handle.sessionMeta.metaEffort `shouldBe` "high"
            loadSession env.toolsPool env.toolsRoot handle.sessionMeta.metaId
                `shouldReturn` Right (handle.sessionMeta, [])

    it "advertises and enforces organization-approved session models" $
        withTempEnv \env launched -> do
            let scopedEnv =
                    env { toolsAllowedModels = Just ["company-a", "company-b"] }
                createTool = head (agentSessionTools scopedEnv)
            case createTool.appToolSchema of
                JsonFunctionSchema properties ->
                    [ propertyKind
                    | PropertySchema propertyKey propertyKind _ _ <- properties
                    , propertyKey == "model"
                    ]
                        `shouldBe`
                            [PropertyEnum ["company-a", "company-b"]]
                other ->
                    expectationFailure
                        ("expected JSON function schema, got " <> show other)
            rejected <- runTool scopedEnv "create_agent_session"
                "{\"message\":\"try forbidden\",\"model\":\"public-model\"}"
            rejected `shouldSatisfy`
                Text.isInfixOf "not allowed by this organization"
            (null <$> readIORef launched) `shouldReturn` True

            accepted <- runTool scopedEnv "create_agent_session"
                "{\"message\":\"use approved\",\"model\":\" company-b \"}"
            accepted `shouldSatisfy` Text.isInfixOf "Status: running"
            [(handle, _)] <- readIORef launched
            handle.sessionMeta.metaModel `shouldBe` "company-b"

    it "uses refreshed organization models for persisted child sessions" $
        withTempEnv \env launched -> do
            let gatewayOption model dialect = ModelOption
                    { modelTarget = ModelTarget
                        { targetProvider = OpenAIProvider
                        , targetConnectionId =
                            organizationGatewayConnectionId
                        , targetModelId = model
                        , targetWireModelId = model
                        , targetDialect = dialect
                        }
                    , modelContextWindow = Nothing
                    , modelLabel = Nothing
                    , modelFallbackPriority = Nothing
                    }
            currentModels <-
                newIORef [gatewayOption "company-a" CodexDialect]
            let scopedEnv = env
                    { toolsAllowedModels = Just ["company-a"]
                    , toolsGatewayIdentity =
                        Just "gateway-sha256:test-tenant"
                    , toolsResolveModelOption =
                        Just \model ->
                            lookup model
                                . map
                                    (\option ->
                                        ( option.modelTarget.targetModelId
                                        , option
                                        ))
                                <$> readIORef currentModels
                    }
                createTool = head (agentSessionTools scopedEnv)
            case createTool.appToolSchema of
                JsonFunctionSchema properties ->
                    [ propertyKind
                    | PropertySchema propertyKey propertyKind _ _ <- properties
                    , propertyKey == "model"
                    ]
                        `shouldBe` [PropertyString]
                other ->
                    expectationFailure
                        ("expected JSON function schema, got " <> show other)
            rejected <- runTool scopedEnv "create_agent_session"
                "{\"message\":\"not yet\",\"model\":\"company-b\"}"
            rejected `shouldSatisfy`
                Text.isInfixOf "not allowed by this organization"
            writeIORef
                currentModels
                [gatewayOption "company-b" GenericResponsesDialect]
            accepted <- runTool scopedEnv "create_agent_session"
                "{\"message\":\"now approved\",\"model\":\"company-b\"}"
            accepted `shouldSatisfy` Text.isInfixOf "Status: running"
            [(handle, _)] <- readIORef launched
            handle.sessionMeta.metaModel `shouldBe` "company-b"
            handle.sessionMeta.metaConnection
                `shouldBe` organizationGatewayConnectionId
            handle.sessionMeta.metaGatewayIdentity
                `shouldBe` Just "gateway-sha256:test-tenant"
            handle.sessionMeta.metaDialect `shouldBe` GenericResponsesDialect

    it "inherits the active dialect and resolves explicit model overrides" $
        withTempEnv \env launched -> do
            let openRouterEnv = env
                    { toolsProvider = OpenRouterProvider
                    , toolsModel = "openai/gpt-5.1"
                    , toolsTransportModel = "openai/gpt-5.1"
                    , toolsDialect = GrokBuildDialect
                    }
            _ <- runTool openRouterEnv "create_agent_session"
                "{\"message\":\"inherit legacy dialect\"}"
            _ <- runTool openRouterEnv "create_agent_session"
                "{\"message\":\"use portable dialect\",\"model\":\"anthropic/claude-sonnet-4\"}"
            [(inherited, _), (overridden, _)] <- readIORef launched
            inherited.sessionMeta.metaModel `shouldBe` "openai/gpt-5.1"
            inherited.sessionMeta.metaDialect `shouldBe` GrokBuildDialect
            overridden.sessionMeta.metaModel
                `shouldBe` "anthropic/claude-sonnet-4"
            overridden.sessionMeta.metaDialect
                `shouldBe` GenericResponsesDialect

    it "reads recent turns without exposing raw response items" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            _ <- appendTurn handle SessionTurn
                { turnAt = fixedTime
                , turnUserText = "question"
                , turnAssistantText = Just "answer"
                , turnError = Nothing
                , turnResponseId = Nothing
                , turnItems = []
                , turnDisplayItems = []
                , turnUsage = Nothing
                , turnEffect = TranscriptAppend
                , turnProviderTelemetry = []
                }
            result <- runTool env "read_agent_session" $
                "{\"session_id\":\"" <> handle.sessionMeta.metaId <> "\"}"
            result `shouldSatisfy` Text.isInfixOf "User:\n  question"
            result `shouldSatisfy` Text.isInfixOf "Assistant:\n  answer"
            result `shouldNotSatisfy` Text.isInfixOf "items"

    it "includes ephemeral retry activity while a session is running" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            persistence <- newActivePersistence handle
            setPersistenceActivity
                persistence
                "provider_cooldown"
                "Waiting before retrying."
                (Just fixedTime)
            result <- runTool env "read_agent_session" $
                "{\"session_id\":\"" <> handle.sessionMeta.metaId <> "\"}"
            result `shouldSatisfy`
                Text.isInfixOf "Kind: provider_cooldown"
            result `shouldSatisfy`
                Text.isInfixOf "Message: Waiting before retrying."

    it "starts a follow-up turn and rejects messaging the current session" $
        withTempEnv \env launched -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            let target = handle.sessionMeta.metaId
                targetEnv = env { toolsCurrentSessionId = pure (Just "other") }
            result <- runTool targetEnv "send_agent_session_message" $
                "{\"session_id\":\"" <> target <> "\",\"message\":\"continue\"}"
            result `shouldSatisfy` Text.isInfixOf "Status: running"
            [(launchedHandle, message)] <- readIORef launched
            launchedHandle.sessionMeta.metaId `shouldBe` target
            message `shouldBe` "continue"

            let selfEnv = env { toolsCurrentSessionId = pure (Just target) }
            selfResult <- runTool selfEnv "send_agent_session_message" $
                "{\"session_id\":\"" <> target <> "\",\"message\":\"loop\"}"
            selfResult `shouldSatisfy`
                Text.isInfixOf "cannot message the current agent session"

    it "keeps gateway sessions private from direct mode but portable across gateways" $
        withTempEnv \env launched -> do
            let gatewayCreate =
                    (testCreate env.toolsPool env.toolsRoot)
                        { createTarget = ModelTarget
                            { targetProvider = OpenAIProvider
                            , targetConnectionId =
                                organizationGatewayConnectionId
                            , targetModelId = "company-private"
                            , targetWireModelId = "company-private"
                            , targetDialect = CodexDialect
                            }
                        , createGatewayIdentity =
                            Just "gateway-sha256:tenant-a"
                        }
            handle <- createSession gatewayCreate
            _ <- appendTurn handle SessionTurn
                { turnAt = fixedTime
                , turnUserText = "tenant A secret"
                , turnAssistantText = Just "private answer"
                , turnError = Nothing
                , turnResponseId = Nothing
                , turnItems = []
                , turnDisplayItems = []
                , turnUsage = Nothing
                , turnEffect = TranscriptAppend
                , turnProviderTelemetry = []
                }
            let payload =
                    "{\"session_id\":\""
                        <> handle.sessionMeta.metaId
                        <> "\"}"
                messagePayload =
                    "{\"session_id\":\""
                        <> handle.sessionMeta.metaId
                        <> "\",\"message\":\"continue\"}"
                otherGateway =
                    env
                        { toolsGatewayIdentity =
                            Just "gateway-sha256:tenant-b"
                        }
                sameGateway =
                    env
                        { toolsGatewayIdentity =
                            Just "gateway-sha256:tenant-a"
                        }
            directRead <- runTool env "read_agent_session" payload
            directRead `shouldSatisfy`
                Text.isInfixOf "Reconnect the same gateway"
            otherRead <- runTool otherGateway "read_agent_session" payload
            otherRead `shouldSatisfy`
                Text.isInfixOf "tenant A secret"
            otherSend <-
                runTool otherGateway "send_agent_session_message" messagePayload
            otherSend `shouldSatisfy`
                Text.isInfixOf "Status: running"
            [(launchedHandle, message)] <- readIORef launched
            launchedHandle.sessionMeta.metaId
                `shouldBe` handle.sessionMeta.metaId
            message `shouldBe` "continue"
            sameRead <- runTool sameGateway "read_agent_session" payload
            sameRead `shouldSatisfy` Text.isInfixOf "tenant A secret"

    it "rejects traversal session ids" $
        withTempEnv \env _ -> do
            result <- runTool env "read_agent_session"
                "{\"session_id\":\"../outside\"}"
            result `shouldSatisfy` Text.isInfixOf "invalid session id"

    it "runs CLI session turns inside the current process" $
        withTempSessionThreadManager ["same-process"] \_ manager -> do
            parentPid <- getProcessID
            observedPid <- newEmptyMVar
            launched <- launchSessionThread manager "same-process" do
                getProcessID >>= putMVar observedPid
                pure (Right ())
            launched `shouldBe` Right "started session same-process"
            takeMVar observedPid `shouldReturn` parentPid
            waitForThreadStatus manager "same-process" "completed"

    it "serializes and reports in-process session turns" $
        withTempSessionThreadManager ["blocked", "failed"] \_ manager -> do
            started <- newEmptyMVar
            release <- newEmptyMVar
            first <- launchSessionThread manager "blocked" do
                putMVar started ()
                takeMVar release
                pure (Right ())
            first `shouldBe` Right "started session blocked"
            takeMVar started
            second <- launchSessionThread
                manager
                "blocked"
                (pure (Right ()))
            second `shouldSatisfy` \case
                Left err -> "already running" `Text.isInfixOf` err
                Right _ -> False
            putMVar release ()
            waitForThreadStatus manager "blocked" "completed"

            failed <- launchSessionThread
                manager
                "failed"
                (pure (Left "boom"))
            failed `shouldBe` Right "started session failed"
            waitForThreadStatus manager "failed" "failed (boom)"

    it "cancels and joins in-process turns when the runtime closes" $ do
        withTempSessionThreadManager ["long-running"] \_ manager -> do
            started <- newEmptyMVar
            stopped <- newEmptyMVar
            launched <- launchSessionThread manager "long-running" $
                (do
                    putMVar started ()
                    threadDelay 30000000
                    pure (Right ()))
                `finally` putMVar stopped ()
            launched `shouldBe` Right "started session long-running"
            takeMVar started
            closeSessionThreadManager manager
            takeMVar stopped
            launchSessionThread manager "later" (pure (Right ()))
                `shouldReturn` Left "agent session manager is closed"

    it "retains terminal in-process status across reads and lock masking" $
        withTempSessionThreadManager ["terminal"] \root manager -> do
            launched <- launchSessionThread
                manager
                "terminal"
                (pure (Right ()))
            launched `shouldBe` Right "started session terminal"
            waitForThreadStatus manager "terminal" "completed"
            -- A still-held external session lock masks the outcome as
            -- "running" for this poll, but the terminal record is retained.
            acquired <-
                acquireSessionLock
                    (root </> unsafeEncodeUtf "terminal")
                    "terminal"
            lock <- either (fail . Text.unpack) pure acquired
            sessionThreadStatus manager "terminal"
                `shouldReturn` "running"
            releaseSessionLock lock
            -- Once the lock clears the real status is observable again, and
            -- repeated reads keep reporting it instead of decaying to "idle".
            sessionThreadStatus manager "terminal"
                `shouldReturn` "completed"
            sessionThreadStatus manager "terminal"
                `shouldReturn` "completed"

    it "serializes background turns with a cross-process session lock" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            script <- writeFakeAgent root
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                first <- launchSessionTurn manager True ApproveAll True False handle "one"
                first `shouldSatisfy` either (const False) (const True)
                second <- launchSessionTurn manager True ApproveAll True False handle "two"
                second `shouldSatisfy` \case
                    Left err -> "already running" `Text.isInfixOf` err
                    Right _ -> False
                waitForSessionStatus
                    manager
                    handle.sessionMeta.metaId
                    "completed"
                closeSessionProcessManager manager

    it "closes scoped children concurrently and rejects later launches" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let marker = toFilePath root FilePath.</> "stopped"
                started = toFilePath root FilePath.</> "started"
            script <- writeFakeAgentBody root
                ("trap 'printf stopped > " <> shellQuote marker
                    <> "; exit 0' TERM INT\nprintf started > "
                    <> shellQuote started
                    <> "\nsleep 30\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <-
                    newSessionProcessManagerWithLifetime
                        ScopedSessionProcesses
                        root
                launchSessionTurn
                    manager True ApproveAll True False handle "one"
                    `shouldReturn`
                        Right ("started session " <> handle.sessionMeta.metaId)
                waitForFile started
                closeSessionProcessManager manager
                waitForFile marker
                launchSessionTurn
                    manager True ApproveAll True False handle "two"
                    `shouldReturn`
                        Left
                            ("session " <> handle.sessionMeta.metaId
                                <> " is already running or its process manager is closed")

    it "forwards bash enablement to managed session turns" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let argsPath = toFilePath root FilePath.</> "agent-args"
            script <- writeFakeAgentBody root
                ("printf '%s\\n' \"$@\" > " <> shellQuote argsPath <> "\nexit 0\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                launched <-
                    launchSessionTurn manager True ApproveAll True True handle "one"
                launched `shouldSatisfy` either (const False) (const True)
                waitForSessionStatus
                    manager
                    handle.sessionMeta.metaId
                    "completed"
                args <- lines <$> readFile argsPath
                args `shouldContain` ["--bash"]
                closeSessionProcessManager manager

    it "forwards ghci disablement to managed session turns" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let argsPath = toFilePath root FilePath.</> "agent-args"
            script <- writeFakeAgentBody root
                ("printf '%s\\n' \"$@\" > " <> shellQuote argsPath <> "\nexit 0\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                launched <-
                    launchSessionTurn manager True ApproveAll False True handle "one"
                launched `shouldSatisfy` either (const False) (const True)
                waitForSessionStatus
                    manager
                    handle.sessionMeta.metaId
                    "completed"
                args <- lines <$> readFile argsPath
                args `shouldContain` ["--no-ghci", "--bash"]
                closeSessionProcessManager manager

    it "points managed child processes at their session temp directory" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let envPath = toFilePath root FilePath.</> "agent-temp-env"
            script <- writeFakeAgentBody root $
                "printf '%s\\n%s\\n%s\\n' "
                    <> "\"$TMPDIR\" "
                    <> "\"$HASKELL_AGENT_TMPDIR\" "
                    <> "\"${HASKELL_AGENT_HOST_TMPDIR-unset}\" > "
                    <> shellQuote envPath
                    <> "\nexit 0\n"
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                launchSessionTurn manager False ApproveAll True False handle "one"
                    `shouldReturn`
                        Right ("completed session " <> handle.sessionMeta.metaId)
                values <- lines <$> readFile envPath
                values `shouldBe`
                    [ toFilePath handle.sessionTempDir
                    , toFilePath handle.sessionTempDir
                    , "unset"
                    ]
                closeSessionProcessManager manager

    it "distinguishes managed deny from remote prompt approval" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let argsPath = toFilePath root FilePath.</> "agent-args"
            script <- writeFakeAgentBody root
                ("printf '%s\\n' \"$@\" > " <> shellQuote argsPath <> "\nexit 0\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                launchManagedTurnBounded
                    manager False DenyMutating True False Nothing handle
                    (managedTurnRequestFromText "one")
                    `shouldReturn`
                        Right ("completed session " <> handle.sessionMeta.metaId)
                args <- lines <$> readFile argsPath
                args `shouldContain` ["--managed-deny-mutations"]
                closeSessionProcessManager manager

    it "keeps an advisory lock until its owner releases it" $
        withTempStoreDir "agent-session-lock-" \pool root -> do
            handle <- createSession (testCreateAt pool root root)
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

    it "tracks active turns separately from idle session ownership" $
        withTempStoreDir "agent-session-activity-lock-" \pool root -> do
            handle <- createSession (testCreateAt pool root root)
            acquireSessionLock
                handle.sessionDir
                handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right sessionLock -> do
                        sessionLockIsActive
                            (sessionActivityLockPath handle.sessionDir)
                            `shouldReturn` False
                        acquireSessionActivityLock
                            handle.sessionDir
                            handle.sessionMeta.metaId >>= \case
                                Left err ->
                                    expectationFailure (Text.unpack err)
                                Right activityLock -> do
                                    sessionLockIsActive
                                        (sessionActivityLockPath
                                            handle.sessionDir)
                                        `shouldReturn` True
                                    releaseSessionLock activityLock
                        sessionLockIsActive
                            (sessionActivityLockPath handle.sessionDir)
                            `shouldReturn` False
                        releaseSessionLock sessionLock

    it "releases an advisory lock when its process crashes" $
        withTempStoreDir "agent-session-lock-crash-" \pool root -> do
            handle <- createSession (testCreateAt pool root root)
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
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            script <- writeFakeAgentError root "could not acquire lock"
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                launchSessionTurn manager True ApproveAll True False handle "one"
                    `shouldReturn` Left "could not acquire lock"
                closeSessionProcessManager manager

    it "does not expose gateway credentials to managed agent children" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let marker = toFilePath root FilePath.</> "leaked"
            script <- writeFakeAgentBody root
                ("if [ -n \"$TELEGRAM_BOT_TOKEN\" ] \
                \|| [ -n \"$TELEGRAM_ALLOWED_USERS\" ]; then \
                \printf leaked > " <> shellQuote marker <> "; fi\n")
            withExecutableOverride script $
                bracket
                    (do
                        oldToken <- lookupEnv "TELEGRAM_BOT_TOKEN"
                        oldUsers <- lookupEnv "TELEGRAM_ALLOWED_USERS"
                        setEnv "TELEGRAM_BOT_TOKEN" "secret"
                        setEnv "TELEGRAM_ALLOWED_USERS" "123"
                        pure (oldToken, oldUsers))
                    (\(oldToken, oldUsers) -> do
                        restoreEnv "TELEGRAM_BOT_TOKEN" oldToken
                        restoreEnv "TELEGRAM_ALLOWED_USERS" oldUsers)
                    \_ -> do
                        handle <- createSession (testCreateAt pool root root)
                        manager <- newSessionProcessManager root
                        launchSessionTurn
                            manager False ApproveAll True False handle "one"
                            `shouldReturn`
                                Right
                                    ("completed session "
                                        <> handle.sessionMeta.metaId)
                        Directory.doesFileExist marker `shouldReturn` False
                        closeSessionProcessManager manager

    it "does not terminate background sessions when the manager closes" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let marker = toFilePath root FilePath.</> "finished"
            script <- writeFakeAgentBody root
                ("sleep 0.2\nprintf done > " <> shellQuote marker <> "\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManager root
                _ <- launchSessionTurn manager True ApproveAll True False handle "one"
                closeSessionProcessManager manager
                waitForFile marker

    it "terminates scoped gateway children when the manager closes" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            let marker = toFilePath root FilePath.</> "finished"
            script <- writeFakeAgentBody root
                ("sleep 1\nprintf done > " <> shellQuote marker <> "\n")
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManagerWithLifetime
                    ScopedSessionProcesses
                    root
                _ <- launchSessionTurn
                    manager True ApproveAll True False handle "one"
                closeSessionProcessManager manager
                threadDelay 1_200_000
                Directory.doesFileExist marker `shouldReturn` False

    it "bounds a foreground managed gateway turn" $
        withTempStoreDir "agent-session-runtime-" \pool root -> do
            script <- writeFakeAgentBody root "sleep 1\n"
            withExecutableOverride script do
                handle <- createSession (testCreateAt pool root root)
                manager <- newSessionProcessManagerWithLifetime
                    ScopedSessionProcesses
                    root
                result <- launchManagedTurnBounded
                    manager
                    False
                    PromptMutating
                    True
                    False
                    (Just 50_000)
                    handle
                    (managedTurnRequestFromText "one")
                result `shouldBe` Left "agent session timed out"
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
    withTempStoreDir "agent-session-tools-" \pool root -> do
        launched <- newIORef []
        let launch handle message = do
                modifyIORef' launched (<> [(handle, message)])
                pure (Right "started")
            env = AgentSessionToolsEnv
                { toolsPool = pool
                , toolsRoot = root
                , toolsProvider = XAIProvider
                , toolsConnection = "xai"
                , toolsModel = "model-1"
                , toolsTransportModel = "model-1"
                , toolsDialect = GrokBuildDialect
                , toolsAllowedModels = Nothing
                , toolsResolveModelOption = Nothing
                , toolsGatewayIdentity = Nothing
                , toolsCwd = fromFilePath "/tmp/work"
                , toolsEffort = "low"
                , toolsCurrentSessionId = pure Nothing
                , toolsLaunchTurn = launch
                , toolsSessionStatus = const (pure "running")
                }
        action env launched

testCreate :: StorePool -> OsPath -> SessionCreate
testCreate pool root = SessionCreate
    { createPool = pool
    , createRoot = root
    , createTarget = ModelTarget
        { targetProvider = XAIProvider
        , targetConnectionId = "xai"
        , targetModelId = "model-1"
        , targetWireModelId = "model-1"
        , targetDialect = GrokBuildDialect
        }
    , createGatewayIdentity = Nothing
    , createCwd = fromFilePath "/tmp/work"
    , createEffort = "low"
    , createTitleHint = Just "test"
    , createTitleIsManual = False
    }

testCreateAt :: StorePool -> OsPath -> OsPath -> SessionCreate
testCreateAt pool root cwd = (testCreate pool root) { createCwd = cwd }

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

waitForThreadStatus
    :: SessionThreadManager
    -> Text.Text
    -> Text.Text
    -> IO ()
waitForThreadStatus manager sessionId expected = go (100 :: Int)
  where
    go attempts
        | attempts <= 0 = do
            actual <- sessionThreadStatus manager sessionId
            actual `shouldBe` expected
        | otherwise = do
            actual <- sessionThreadStatus manager sessionId
            if actual == expected
                then pure ()
                else threadDelay 20000 >> go (attempts - 1)

withTempSessionThreadManager
    :: [Text.Text]
    -> (OsPath -> SessionThreadManager -> IO a)
    -> IO a
withTempSessionThreadManager sessionIds action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "ha-threads"))
        Directory.removeDirectoryRecursive
        \basePath -> do
            mapM_
                (Directory.createDirectory
                    . (basePath FilePath.</>)
                    . Text.unpack)
                sessionIds
            let root = fromFilePath basePath
            bracket
                (newSessionThreadManager root)
                closeSessionThreadManager
                (action root)

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

restoreEnv :: String -> Maybe String -> IO ()
restoreEnv name = \case
    Nothing -> unsetEnv name
    Just value -> setEnv name value

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 21) (secondsToDiffTime 0)

withTempStoreDir :: String -> (StorePool -> OsPath -> IO a) -> IO a
withTempStoreDir _prefix action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "ha"))
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
                (\store ->
                    action
                        (trustedPool store)
                        (fromFilePath sessionsDirectory))
