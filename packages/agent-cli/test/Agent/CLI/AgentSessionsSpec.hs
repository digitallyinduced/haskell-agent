module Agent.CLI.AgentSessionsSpec (spec) where

import Agent.CLI.AgentSessions
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Agent.CLI.ManagedTurn (managedTurnRequestFromText)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Session
import Agent.CLI.SessionLock
import Agent.CLI.SteeringInputs
    ( awaitBackgroundCompletion, clearSteeringInputs, commitSteeringInputs
    , hasBackgroundCompletionWake, newSteeringInputs
    , prepareBackgroundCompletion, readSteeringInputs )
import Agent.Dialect (DialectId(..))
import Agent.Loop (TurnInput(..), defaultLoopDispatch)
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
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (SomeException, bracket, finally, try, throwIO)
import Data.IORef
import Control.Monad (void)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import qualified System.FilePath as FilePath
import System.Posix.Temp (mkdtemp)
import System.Posix.Process (forkProcess, getProcessID, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Timeout (timeout)
import Test.Hspec

isReadOnly :: ApprovalRule -> Bool
isReadOnly AlwaysReadOnly = True
isReadOnly _ = False

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

toFilePath :: OsPath -> FilePath
toFilePath path = either (error . show) id (decodeUtf path)

waitPayload :: SessionHandle -> Int -> Text.Text
waitPayload handle milliseconds =
    "{\"session_id\":\"" <> handle.sessionMeta.metaId
        <> "\",\"timeout_ms\":" <> Text.pack (show milliseconds) <> "}"

waitTestTurn :: Text.Text -> SessionTurn
waitTestTurn answer = SessionTurn
    { turnAt = fixedTime
    , turnUserText = "question"
    , turnAssistantText = Just answer
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnItems = []
    , turnDisplayItems = []
    , turnUsage = Nothing
    , turnEffect = TranscriptAppend
    , turnProviderTelemetry = []
    }

spec :: Spec
spec = describe "Agent.CLI.AgentSessions" do
    it "describes background session launches as explicit work, not completed results" $
        withTempEnv \env _ -> do
            let launchTools = filter
                    (\tool -> tool.appToolName `elem`
                        ["create_agent_session", "send_agent_session_message"])
                    (agentSessionTools env)
            length launchTools `shouldBe` 2
            mapM_ (\tool -> do
                tool.appToolDescription `shouldSatisfy` Text.isInfixOf "explicitly"
                tool.appToolDescription `shouldSatisfy` Text.isInfixOf "current task"
                tool.appToolDescription `shouldSatisfy` Text.isInfixOf "not a completed result"
                ) launchTools

    it "waits for idle sessions immediately and includes their last response" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            _ <- appendTurn handle (waitTestTurn "done")
            result <- runTool env "wait_agent_session" (waitPayload handle 1000)
            result `shouldSatisfy` Text.isInfixOf "Status: idle"
            result `shouldSatisfy` Text.isInfixOf "Assistant:\n  done"

    it "labels recent output separately when the target has already resumed" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            _ <- appendTurn handle (waitTestTurn "old")
            let waiting = env { toolsPrepareSessionWait = \_ -> pure do
                    next <- appendTurn handle (waitTestTurn "first result")
                    _ <- appendTurn next (waitTestTurn "later result")
                    pure "completed" }
            result <- runTool waiting "wait_agent_session" (waitPayload handle 1000)
            result `shouldSatisfy` Text.isInfixOf "later result"
            result `shouldSatisfy` Text.isInfixOf "may include a later resume"
            result `shouldNotSatisfy` Text.isInfixOf "Assistant:\n  old"

    it "rejects invalid wait deadlines and self waits before preparing a wait" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            let forbidden = env
                    { toolsPrepareSessionWait = \_ -> error "must not prepare"
                    , toolsCurrentSessionId = pure (Just handle.sessionMeta.metaId)
                    }
            mapM_ (\ms -> runTool forbidden "wait_agent_session" (waitPayload handle ms)
                >>= (`shouldSatisfy` Text.isInfixOf "timeout_ms must be")) [0, -1, 300001]
            result <- runTool forbidden "wait_agent_session" (waitPayload handle 1000)
            result `shouldSatisfy` Text.isInfixOf "cannot wait for the current"

    it "enforces the session boundary before preparing a wait" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            let forbidden = env
                    { toolsConnection = organizationGatewayConnectionId
                    , toolsGatewayIdentity = Just "different-organization"
                    , toolsPrepareSessionWait = \_ -> error "must not prepare"
                    }
            result <- runTool forbidden "wait_agent_session" (waitPayload handle 1000)
            result `shouldNotSatisfy` Text.isInfixOf "Status:"

    it "times out without cancelling the captured target and permits another wait" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            bracket (newSessionThreadManager env.toolsRoot) closeSessionThreadManager \manager -> do
                gate <- newEmptyMVar
                _ <- launchSessionThread manager handle.sessionMeta.metaId
                    (takeMVar gate >> pure (Right ()))
                let waiting = env { toolsPrepareSessionWait = prepareSessionThreadWait manager }
                result <- runTool waiting "wait_agent_session" (waitPayload handle 30)
                result `shouldSatisfy` Text.isInfixOf "Status: timed_out"
                sessionThreadStatus manager handle.sessionMeta.metaId `shouldReturn` "running"
                putMVar gate ()
                result2 <- runTool waiting "wait_agent_session" (waitPayload handle 1000)
                result2 `shouldSatisfy` Text.isInfixOf "Status: completed"

    it "keeps a captured wait attached to its run after a rapid resume" $
        withTempEnv \env _ ->
            bracket (newSessionThreadManager env.toolsRoot) closeSessionThreadManager \manager -> do
                gate <- newEmptyMVar
                _ <- launchSessionThread manager "target" (takeMVar gate >> pure (Left "original failure"))
                original <- prepareSessionThreadWait manager "target"
                putMVar gate ()
                original `shouldReturn` "failed (original failure)"
                nextGate <- newEmptyMVar
                _ <- launchSessionThread manager "target" (takeMVar nextGate >> pure (Right ()))
                timeout 100000 original `shouldReturn` Just "failed (original failure)"

    it "cancels only the waiter, not its target" $
        withTempEnv \env _ ->
            bracket (newSessionThreadManager env.toolsRoot) closeSessionThreadManager \manager -> do
                gate <- newEmptyMVar
                _ <- launchSessionThread manager "target" (takeMVar gate >> pure (Right ()))
                wait <- prepareSessionThreadWait manager "target"
                Async.withAsync wait \waiter -> do
                    Async.cancel waiter
                    sessionThreadStatus manager "target" `shouldReturn` "running"
                putMVar gate ()
                wait `shouldReturn` "completed"

    it "reports target cancellation to an existing waiter" $
        withTempEnv \env _ -> do
            manager <- newSessionThreadManager env.toolsRoot
            gate <- newEmptyMVar
            _ <- launchSessionThread manager "target" (takeMVar gate >> pure (Right ()))
            wait <- prepareSessionThreadWait manager "target"
            closeSessionThreadManager manager
            wait `shouldReturn` "cancelled"

    it "does not wait on an idle interactive session's lifetime lock" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            Right lock <- acquireSessionLock handle.sessionDir handle.sessionMeta.metaId
            flip finally (releaseSessionLock lock) $
                bracket (newSessionThreadManager env.toolsRoot) closeSessionThreadManager \manager -> do
                    wait <- prepareSessionThreadWait manager handle.sessionMeta.metaId
                    timeout 100000 wait `shouldReturn` Just "idle"

    it "stops waiting when an external session rapidly starts its next turn" $
        withTempEnv \env _ -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            bracket (newSessionThreadManager env.toolsRoot) closeSessionThreadManager \manager -> do
                Right first <- acquireSessionActivityLock handle.sessionDir handle.sessionMeta.metaId
                wait <- prepareSessionThreadWait manager handle.sessionMeta.metaId
                timeout 10000 wait `shouldReturn` Nothing
                releaseSessionLock first
                Right second <- acquireSessionActivityLock handle.sessionDir handle.sessionMeta.metaId
                flip finally (releaseSessionLock second) $ do
                    sessionActivityGeneration handle.sessionDir `shouldReturn` Just 2
                    timeout 100000 wait `shouldReturn` Just "idle"

    it "registers create/read/message/wait tools with mutating flags" $
        withTempEnv \env _ -> do
            map (\tool -> (tool.appToolName, isReadOnly tool.appToolApproval))
                (agentSessionTools env)
                `shouldBe`
                    [ ("create_agent_session", False)
                    , ("read_agent_session", True)
                    , ("send_agent_session_message", False)
                    , ("wait_agent_session", True)
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

    it "delivers to an open session owner instead of starting a competing turn" $
        withTempEnv \env launched -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            let target = handle.sessionMeta.metaId
                ownerEnv = env
                    { toolsDeliverToOwner = \_ _ -> pure (Just (Right ()))
                    , toolsSessionStatus = const (pure "running")
                    }
            result <- runTool ownerEnv "send_agent_session_message" $
                "{\"session_id\":\"" <> target <> "\",\"message\":\"continue\"}"
            result `shouldSatisfy` Text.isInfixOf "Status: running"
            launchedNow <- readIORef launched
            length launchedNow `shouldBe` 0

    it "does not start a competing turn when the target session is already open" $
        withTempEnv \env launched -> do
            handle <- createSession (testCreate env.toolsPool env.toolsRoot)
            Right lock <- acquireSessionLock
                handle.sessionDir handle.sessionMeta.metaId
            flip finally (releaseSessionLock lock) do
                result <- runTool env "send_agent_session_message" $
                    "{\"session_id\":\""
                        <> handle.sessionMeta.metaId
                        <> "\",\"message\":\"continue\"}"
                result `shouldSatisfy` Text.isInfixOf "already running"
                launchedNow <- readIORef launched
                length launchedNow `shouldBe` 0

    it "keeps persisted gateway sessions portable across direct and gateway modes" $
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
                Text.isInfixOf "tenant A secret"
            directSend <-
                runTool env "send_agent_session_message" messagePayload
            directSend `shouldSatisfy`
                Text.isInfixOf "Status: running"
            otherRead <- runTool otherGateway "read_agent_session" payload
            otherRead `shouldSatisfy`
                Text.isInfixOf "tenant A secret"
            otherSend <-
                runTool otherGateway "send_agent_session_message" messagePayload
            otherSend `shouldSatisfy`
                Text.isInfixOf "Status: running"
            [(directHandle, directMessage), (gatewayHandle, gatewayMessage)] <-
                readIORef launched
            directHandle.sessionMeta.metaId
                `shouldBe` handle.sessionMeta.metaId
            directMessage `shouldBe` "continue"
            gatewayHandle.sessionMeta.metaId
                `shouldBe` handle.sessionMeta.metaId
            gatewayMessage `shouldBe` "continue"
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

    it "notifies the launching parent once per completed session turn and permits follow-up" $
        withTempSessionThreadManager ["child"] \_ manager -> do
            notices <- newIORef []
            let notify status =
                    atomicModifyIORef' notices \previous ->
                        (previous <> [formatSessionCompletionNotice "child" status], ())
            launchSessionThreadNotifying manager "child" notify (pure (Right ()))
                `shouldReturn` Right "started session child"
            waitForThreadStatus manager "child" "completed"
            readIORef notices `shouldReturn` [formatSessionCompletionNotice "child" "completed"]
            -- A resumed turn gets a new notification; status reads do not.
            sessionThreadStatus manager "child" `shouldReturn` "completed"
            launchSessionThreadNotifying manager "child" notify (pure (Left "follow-up failed"))
                `shouldReturn` Right "started session child"
            waitForThreadStatus manager "child" "failed (follow-up failed)"
            readIORef notices `shouldReturn`
                [ formatSessionCompletionNotice "child" "completed"
                , formatSessionCompletionNotice "child" "failed (follow-up failed)"
                ]

    it "routes concurrent session turn completions only to their launching parents" $
        withTempSessionThreadManager ["first", "second"] \_ manager -> do
            firstNotices <- newIORef []
            secondNotices <- newIORef []
            let notify ref status =
                    atomicModifyIORef' ref \previous -> (previous <> [status], ())
            _ <- launchSessionThreadNotifying manager "first"
                (notify firstNotices) (pure (Right ()))
            _ <- launchSessionThreadNotifying manager "second"
                (notify secondNotices) (pure (Left "second failed"))
            waitForThreadStatus manager "first" "completed"
            waitForThreadStatus manager "second" "failed (second failed)"
            readIORef firstNotices `shouldReturn` ["completed"]
            readIORef secondNotices `shouldReturn` ["failed (second failed)"]

    it "notifies the parent when a session turn throws" $
        withTempSessionThreadManager ["child"] \_ manager -> do
            notice <- newEmptyMVar
            _ <- launchSessionThreadNotifying manager "child" (putMVar notice)
                (throwIO (userError "child exception"))
            observed <- timeout 1000000 (takeMVar notice)
            observed `shouldSatisfy` maybe False (Text.isInfixOf "child exception")
            sessionThreadStatus manager "child" `shouldReturn`
                maybe "missing notification" id observed

    it "does not notify for rejected launches or shutdown cancellation" $
        withTempSessionThreadManager ["child"] \_ manager -> do
            notices <- newIORef []
            gate <- newEmptyMVar
            let notify status =
                    atomicModifyIORef' notices \previous -> (previous <> [status], ())
            _ <- launchSessionThreadNotifying manager "child" notify
                (takeMVar gate >> pure (Right ()))
            launchSessionThreadNotifying manager "child" notify (pure (Right ()))
                `shouldReturn` Left "session child is already running"
            closeSessionThreadManager manager
            launchSessionThreadNotifying manager "child" notify (pure (Right ()))
                `shouldReturn` Left "agent session manager is closed"
            readIORef notices `shouldReturn` []

    it "preserves session outcome when a completion sink fails" $
        withTempSessionThreadManager ["child"] \_ manager -> do
            _ <- launchSessionThreadNotifying manager "child"
                (\_ -> throwIO (userError "notification failed"))
                (pure (Right ()))
            waitForThreadStatus manager "child" "completed"

    it "includes the session id and inspection/follow-up tools in completion notices" $ do
        let notice = formatSessionCompletionNotice "child-id" "completed"
        notice `shouldSatisfy` Text.isInfixOf "Session ID: child-id"
        notice `shouldSatisfy` Text.isInfixOf "Status: completed"
        notice `shouldSatisfy` Text.isInfixOf "read_agent_session"
        notice `shouldSatisfy` Text.isInfixOf "send_agent_session_message"

    it "wakes an idle parent and retains child completion until the provider commits it" $
        withTempSessionThreadManager ["child"] \_ manager -> do
            steering <- newSteeringInputs
            enqueue <- prepareBackgroundCompletion steering
            let notice = UserMessage (formatSessionCompletionNotice "child" "completed")
            _ <- launchSessionThreadNotifying manager "child"
                (\status -> void $ enqueue "child:turn-1" $
                    UserMessage (formatSessionCompletionNotice "child" status))
                (pure (Right ()))
            timeout 1000000 (atomically (awaitBackgroundCompletion steering))
                `shouldReturn` Just ()
            -- Consuming the idle-wake edge must not consume the model input.
            readSteeringInputs steering `shouldReturn` [notice]
            hasBackgroundCompletionWake steering `shouldReturn` False
            readSteeringInputs steering `shouldReturn` [notice]
            commitSteeringInputs steering 1
            readSteeringInputs steering `shouldReturn` []

    it "does not deliver a late child completion into a reset parent conversation" $
        withTempSessionThreadManager ["child"] \_ manager -> do
            steering <- newSteeringInputs
            enqueue <- prepareBackgroundCompletion steering
            gate <- newEmptyMVar
            _ <- launchSessionThreadNotifying manager "child"
                (\status -> void $ enqueue "child:turn-1" $
                    UserMessage (formatSessionCompletionNotice "child" status))
                (takeMVar gate >> pure (Right ()))
            clearSteeringInputs steering
            putMVar gate ()
            waitForThreadStatus manager "child" "completed"
            readSteeringInputs steering `shouldReturn` []
            hasBackgroundCompletionWake steering `shouldReturn` False
            -- New work belongs to the new conversation and can wake it.
            current <- prepareBackgroundCompletion steering
            current "child:turn-2" (UserMessage "new completion")
                `shouldReturn` Right True
            readSteeringInputs steering `shouldReturn` [UserMessage "new completion"]
            hasBackgroundCompletionWake steering `shouldReturn` True

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
            -- Start the child before publishing readiness and use the shell's
            -- interruptible wait builtin. A foreground sleep can defer the TERM
            -- trap until after the manager's escalation deadline.
            script <- writeFakeAgentBody root
                ("sleep 30 &\nchild=$!\n"
                    <> "trap 'kill \"$child\" 2>/dev/null; wait \"$child\" 2>/dev/null; printf stopped > "
                    <> shellQuote marker
                    <> "; exit 0' TERM INT\nprintf started > "
                    <> shellQuote started
                    <> "\nwait \"$child\"\n")
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

    it "keeps terminal-only capabilities out of managed session turns" $
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
                args `shouldContain` ["--no-computer-use"]
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
                , toolsPrepareSessionWait = const (pure (pure "idle"))
                , toolsDeliverToOwner = \_ _ -> pure Nothing
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
