module ServerSpec (spec) where

import Agent.Server.Client
import Agent.Telegram.Connector.Server
import Agent.Telegram.Connector.Session
import Data.Aeson (object, (.=))
import Data.Either (isLeft)
import Data.IORef
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Test.Hspec

spec :: Spec
spec = describe "agent-server connector backend" do
    it "implements the shared execution transition contract" do
        let transport = unusedTransport
                { transportGetTurn = \_ -> pure (Right completedResult.agentServerResultTurn)
                , transportGetResult = \_ -> pure (Right completedResult) }
        advanceSessionExecution (serverSessionBackendWith transport 16000)
            (SessionExecution "request" "session" "Original submitted text" (Just "turn"))
            `shouldReturn` ExecutionCompleted "turn" "Complete answer"

    it "recovers an accepted submission after its response was lost" do
        accepted <- newIORef False
        submissions <- newIORef (0 :: Int)
        let transport = unusedTransport
                { transportListTurns = \_ -> do
                    exists <- readIORef accepted
                    pure (Right (AgentServerTurnList [runningTurn | exists]))
                , transportCreateTurn = \session request -> do
                    session `shouldBe` "session"
                    request `shouldBe` submittedRequest
                    modifyIORef' submissions (+ 1)
                    writeIORef accepted True
                    pure (Left (AgentServerTransportError "response lost"))
                }
        reconcileServerTurnWith transport 16000 "session" submittedRequest Nothing
            >>= (`shouldSatisfy` isLeft)
        reconcileServerTurnWith transport 16000 "session" submittedRequest Nothing
            `shouldReturn` Right (ServerTurnSnapshot "turn" ServerTurnActive)
        readIORef submissions `shouldReturn` 1

    it "persists a recovered completed turn before publishing its result" do
        let transport = unusedTransport
                { transportListTurns = \_ -> pure (Right (AgentServerTurnList [completedResult.agentServerResultTurn]))
                , transportGetTurn = \_ -> pure (Right completedResult.agentServerResultTurn)
                , transportGetResult = \_ -> pure (Right completedResult)
                }
            backend = serverSessionBackendWith transport 16000
            execution = SessionExecution "request" "session" "Original submitted text" Nothing
        advanceSessionExecution backend execution `shouldReturn` ExecutionRunning "turn"
        advanceSessionExecution backend execution { executionTurnId = Just "turn" }
            `shouldReturn` ExecutionCompleted "turn" "Complete answer"

    it "retries transient faults without publishing remote diagnostics" do
        let execution = SessionExecution "request" "session" "Original submitted text" (Just "turn")
            advance problem = advanceSessionExecution
                (serverSessionBackendWith unusedTransport
                    { transportGetTurn = \_ -> pure (Left problem) } 16000) execution
        advance (AgentServerTransportError "Bearer secret") `shouldReturn` ExecutionRetry
        advance (AgentServerCredentialError "secret file content") `shouldReturn` ExecutionRetry
        advance (AgentServerProtocolError "private prompt")
            `shouldReturn` ExecutionFailed "Agent server response failed protocol validation"
        advance (AgentServerHttpError 409 (Just "session_busy") "private diagnostic")
            `shouldReturn` ExecutionRetry
        advance (AgentServerHttpError 409 (Just "request_conflict") "private diagnostic")
            `shouldReturn` ExecutionFailed "Agent server rejected the operation"

    it "inspects nonterminal state without requesting a terminal result" do
        let execution = SessionExecution "request" "session" "Original submitted text" (Just "turn")
            advance status = advanceSessionExecution (serverSessionBackendWith unusedTransport
                { transportGetTurn = \_ -> pure (Right runningTurn { agentServerTurnStatus = status })
                -- This deliberately throws if the terminal-only endpoint is used.
                , transportListRequests = \_ -> pure (Right (AgentServerRequestList [humanRequest]))
                } 16000) execution
        advance AgentServerTurnQueued `shouldReturn` ExecutionRunning "turn"
        advance AgentServerTurnRunning `shouldReturn` ExecutionRunning "turn"
        advance AgentServerTurnWaitingForInput `shouldReturn` ExecutionWaiting "turn"
            [HumanRequest "approval" "AgentServerToolApproval" "Allow this invocation?" ["allow_once", "deny"]]
        advance AgentServerTurnFailed `shouldReturn` ExecutionFailed "Agent execution failed"
        advance AgentServerTurnCancelled `shouldReturn` ExecutionCancelled

    it "rejects a mismatched inspected turn before fetching results or requests" do
        let check turn = reconcileServerTurnWith unusedTransport
                { transportGetTurn = \_ -> pure (Right turn) }
                16000 "session" submittedRequest (Just "turn")
        mapM_ (\turn -> check turn >>= (`shouldSatisfy` isLeft))
            [ runningTurn { agentServerTurnId = "other" }
            , waitingTurn { agentServerTurnSessionId = "other" }
            , completedResult.agentServerResultTurn { agentServerTurnClientRequestId = "other" }
            ]

    it "does not submit when the recovery listing failed" do
        let transport = unusedTransport
                { transportListTurns = \_ -> pure (Left (AgentServerTransportError "unavailable")) }
        reconcileServerTurnWith transport 16000 "session" submittedRequest Nothing
            >>= (`shouldSatisfy` isLeft)

    it "rejects recovered turns for a different session and duplicate request identifiers" do
        let recover turns = reconcileServerTurnWith
                unusedTransport { transportListTurns = \_ -> pure (Right (AgentServerTurnList turns)) }
                16000 "session" submittedRequest Nothing
        recover [runningTurn { agentServerTurnSessionId = "other" }] >>= (`shouldSatisfy` isLeft)
        recover [runningTurn, runningTurn] >>= (`shouldSatisfy` isLeft)

    it "rejects a different turn or client request in a result" do
        let check turn = reconcileServerTurnWith
                unusedTransport
                    { transportGetTurn = \_ -> pure (Right completedResult.agentServerResultTurn)
                    , transportGetResult = \_ -> pure (Right (completedResult { agentServerResultTurn = turn })) }
                16000 "session" submittedRequest (Just "turn")
        check (runningTurn { agentServerTurnId = "other" }) >>= (`shouldSatisfy` isLeft)
        check (runningTurn { agentServerTurnClientRequestId = "other" }) >>= (`shouldSatisfy` isLeft)

    it "publishes only complete bounded text and preserves content exactly" do
        completedServerResponse 16000 completedResult `shouldBe` Right "Complete answer"
        let withOutput output = completedResult { agentServerResultOutput = Just output }
        completedServerResponse 16000 (withOutput completeOutput { agentServerOutputAssistantTextTruncated = True })
            `shouldSatisfy` isLeft
        completedServerResponse 16000 (withOutput completeOutput { agentServerOutputCompletion = AgentServerTurnIncomplete "interrupted" Nothing })
            `shouldSatisfy` isLeft
        mapM_ (\content ->
            completedServerResponse 16000 (withOutput completeOutput { agentServerOutputAssistantText = Just content })
                `shouldSatisfy` isLeft)
            [" ", "\0", Text.replicate 16001 "a"]
        completedServerResponse 16000 (withOutput completeOutput { agentServerOutputAssistantText = Just "  preserved\n" })
            `shouldBe` Right "  preserved\n"

    it "rejects human requests that do not belong to the current turn" do
        let transport = unusedTransport
                { transportGetTurn = \_ -> pure (Right waitingTurn)
                , transportListRequests = \_ -> pure (Right (AgentServerRequestList
                    [humanRequest { agentServerRequestTurnId = "other" }]))
                }
        reconcileServerTurnWith transport 16000 "session" submittedRequest (Just "turn")
            >>= (`shouldSatisfy` isLeft)

    it "rejects NUL and oversized human request fields before persistence" do
        let check request = reconcileServerTurnWith unusedTransport
                { transportGetTurn = \_ -> pure (Right waitingTurn)
                , transportListRequests = \_ -> pure (Right (AgentServerRequestList [request]))
                } 16000 "session" submittedRequest (Just "turn")
        mapM_ (\text -> mapM_ (\request -> check request >>= (`shouldSatisfy` isLeft))
            [ humanRequest { agentServerRequestId = text }
            , humanRequest { agentServerRequestPrompt = text }
            , humanRequest { agentServerRequestOptions = [text] }
            ]) ["bad\0text", Text.replicate 16001 "a"]

    it "resolves only a currently offered decision and an exact request" do
        let transport = unusedTransport
                { transportGetTurn = \_ -> pure (Right waitingTurn)
                , transportListRequests = \_ -> pure (Right (AgentServerRequestList [humanRequest]))
                , transportResolveRequest = \identifier response -> do
                    identifier `shouldBe` "approval"
                    response `shouldBe` AgentServerResolveRequest "allow_once" Nothing
                    pure (Right humanRequest)
                }
        resolveServerRequestWith transport "session" "request" "turn" "approval"
            (AgentServerResolveRequest "allow_once" Nothing) `shouldReturn` Right ()
        resolveServerRequestWith transport "session" "request" "turn" "approval"
            (AgentServerResolveRequest "allow_tool" Nothing) >>= (`shouldSatisfy` isLeft)
        resolveServerRequestWith transport "session" "request" "turn" "missing"
            (AgentServerResolveRequest "allow_once" Nothing) >>= (`shouldSatisfy` isLeft)

    it "does not resolve stale requests or requests for another session" do
        let transport = unusedTransport { transportGetTurn = \_ -> pure (Right runningTurn) }
        resolveServerRequestWith transport "session" "request" "turn" "approval"
            (AgentServerResolveRequest "allow_once" Nothing) >>= (`shouldSatisfy` isLeft)
        resolveServerRequestWith transport "other" "request" "turn" "approval"
            (AgentServerResolveRequest "allow_once" Nothing) >>= (`shouldSatisfy` isLeft)

    it "rejects duplicate request identities and invalid response values before resolution" do
        let transport requests = unusedTransport
                { transportGetTurn = \_ -> pure (Right waitingTurn)
                , transportListRequests = \_ -> pure (Right (AgentServerRequestList requests))
                }
            resolve requests value = resolveServerRequestWith (transport requests)
                "session" "request" "turn" "approval" (AgentServerResolveRequest "allow_once" value)
        resolve [humanRequest, humanRequest] Nothing >>= (`shouldSatisfy` isLeft)
        resolve [humanRequest] (Just "\0") >>= (`shouldSatisfy` isLeft)
        resolve [humanRequest] (Just (Text.replicate 16001 "x")) >>= (`shouldSatisfy` isLeft)

    it "does not cancel an already terminal execution" do
        let transport = unusedTransport
                { transportGetTurn = \_ -> pure (Right completedResult.agentServerResultTurn) }
        cancelServerTurnWith transport "session" "request" "turn" `shouldReturn` Right ()

    it "rejects a request whose prompt changed after its approval was presented" do
        let transport = unusedTransport
                { transportGetTurn = \_ -> pure (Right waitingTurn)
                , transportListRequests = \_ -> pure (Right (AgentServerRequestList [humanRequest]))
                }
            backend = serverSessionBackendWith transport 16000
        backend.respondToRequest
            (SessionExecution "request" "session" "Original submitted text" (Just "turn"))
            "turn"
            (HumanRequest "approval" "AgentServerToolApproval" "A different invocation" ["allow_once", "deny"])
            (object ["decision" .= ("allow_once" :: Text.Text)])
            >>= (`shouldSatisfy` isLeft)

    it "checks the execution identity before cancellation and validates its receipt" do
        let transport = unusedTransport
                { transportGetTurn = \_ -> pure (Right runningTurn)
                , transportCancelTurn = \identifier -> do
                    identifier `shouldBe` "turn"
                    pure (Right (runningTurn { agentServerTurnStatus = AgentServerTurnCancelled }))
                }
        cancelServerTurnWith transport "session" "request" "turn" `shouldReturn` Right ()
        cancelServerTurnWith transport "other" "request" "turn" >>= (`shouldSatisfy` isLeft)
        cancelServerTurnWith transport
            { transportCancelTurn = \_ -> pure (Right (runningTurn { agentServerTurnId = "other" })) }
            "session" "request" "turn" >>= (`shouldSatisfy` isLeft)

unusedTransport :: ServerTransport
unusedTransport = ServerTransport
    { transportCreateTurn = \_ _ -> fail "unexpected submission"
    , transportListTurns = \_ -> fail "unexpected turn listing"
    , transportGetTurn = \_ -> fail "unexpected turn lookup"
    , transportGetResult = \_ -> fail "unexpected result lookup"
    , transportListRequests = \_ -> fail "unexpected request listing"
    , transportResolveRequest = \_ _ -> fail "unexpected request resolution"
    , transportCancelTurn = \_ -> fail "unexpected cancellation"
    }

submittedRequest :: AgentServerCreateTurnRequest
submittedRequest = AgentServerCreateTurnRequest "request" "Original submitted text" [] []

runningTurn :: AgentServerTurn
runningTurn = AgentServerTurn "turn" "session" "request" AgentServerTurnRunning
    (UTCTime (fromGregorian 2026 9 27) 0) Nothing Nothing Nothing "Original submitted text"

waitingTurn :: AgentServerTurn
waitingTurn = runningTurn { agentServerTurnStatus = AgentServerTurnWaitingForInput }

completeOutput :: AgentServerTurnOutput
completeOutput = AgentServerTurnOutput "response" (Just "Complete answer") False AgentServerTurnComplete

completedResult :: AgentServerTurnResult
completedResult = AgentServerTurnResult
    (runningTurn { agentServerTurnStatus = AgentServerTurnCompleted }) (Just completeOutput)

humanRequest :: AgentServerHumanRequest
humanRequest = AgentServerHumanRequest "approval" "turn" "session" AgentServerToolApproval
    "Allow this invocation?" ["allow_once", "deny"] (UTCTime (fromGregorian 2026 9 27) 0)
