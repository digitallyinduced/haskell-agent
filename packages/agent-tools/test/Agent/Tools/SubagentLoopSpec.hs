module Agent.Tools.SubagentLoopSpec (spec) where

import Agent.Cancel (CancelFlag, newCancelFlag)
import Agent.Loop
    ( Backend
    , BackendResult(..)
    , BackendSnapshot
    , BackendStateStore(..)
    , LoopConfig(..)
    , LoopExecution(..)
    , LoopResult(..)
    , TurnInput(..)
    , advanceBackendSnapshot
    , backendWithCallbacks
    , defaultLoopDispatch
    , defaultLoopMaxTurns
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Agent.Subagents (defaultSubagentConfig)
import Agent.ToolDispatch (ToolCallResult(..), functionToolCall)
import Agent.Tools.SubagentLoop
import Agent.Tools.Types (ToolApproval(..), ToolRegistry, mkToolRegistry)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.SubagentLoop" do
    it "runs a spawned child through the library loop and returns its answer" do
        childPrompts <- newIORef ([] :: [Text])
        parentStep <- newIORef (0 :: Int)
        let emptyTools = either (error . Text.unpack) id (mkToolRegistry [])
            childAnswer inputs = any (\case
                CompletedTool result ->
                    "child-answer" `Text.isInfixOf` result.output
                _ -> False) inputs
        cancel <- newCancelFlag
        parentState <- newIORef emptyBackendSnapshot
        let parentBackend = backendWithCallbacks \snapshot _ inputs _ -> do
                step <- atomicModifyIORef' parentStep \n -> (n + 1, n)
                let output
                        | childAnswer inputs =
                            emptyTurnOutput "parent-done" [] (Just "parent-done")
                        | step == 0 =
                            emptyTurnOutput "parent-spawn"
                                [ functionToolCall "spawn-1" "spawn_agent"
                                    "{\"task_name\":\"lookup\",\"message\":\"find the invoice\",\"fork_turns\":\"none\"}"
                                ]
                                Nothing
                        | step < 4 =
                            emptyTurnOutput ("parent-wait-" <> Text.pack (show step))
                                [ functionToolCall "wait-1" "wait_agent"
                                    "{\"targets\":[\"lookup\"]}"
                                ]
                                Nothing
                        | otherwise =
                            emptyTurnOutput "parent-gave-up" [] (Just "gave-up")
                pure $ Right BackendResult
                    { backendOutput = output
                    , backendState = advanceBackendSnapshot snapshot [] Nothing
                    }
            childBackend = backendWithCallbacks \snapshot _ inputs _ -> do
                atomicModifyIORef' childPrompts \known ->
                    ( [text | UserMessage text <- inputs] <> known
                    , ()
                    )
                pure $ Right BackendResult
                    { backendOutput = emptyTurnOutput "child-1" [] (Just "child-answer")
                    , backendState = advanceBackendSnapshot snapshot [] Nothing
                    }
        session <- openLibrarySubagents LibrarySubagentConfig
            { librarySubagentCwd = unsafeEncodeUtf "/tmp"
            , librarySubagentLimits = defaultSubagentConfig
            , libraryPrepareChild = \request -> do
                childState <- newIORef emptyBackendSnapshot
                case plainLibraryChildInputs request.libraryChildMessage of
                    Left err -> pure (Left err)
                    Right childInputs -> pure $ Right PreparedChildLoop
                        { preparedChildConfig = loopConfig cancel childBackend
                            (memoryStore childState) emptyTools
                        , preparedChildInputs = childInputs
                        , preparedChildPrevious =
                            request.libraryChildPreviousResponseId
                        }
            , libraryOnChildEvent = \_ _ -> pure ()
            , librarySendToRoot = Nothing
            , libraryAllowedChildModels = Nothing
            , librarySpawnModelGuidance = Nothing
            }
        execution <- runLoopWithSubagents session
            (loopConfig cancel parentBackend (memoryStore parentState) emptyTools)
            Nothing
            [UserMessage "start"]
        closeLibrarySubagents session
        case execution of
            LoopExecution{executionResult = Right LoopResult{finalText}} ->
                finalText `shouldBe` Just "parent-done"
            LoopExecution{executionResult = Left err} ->
                expectationFailure (show err)
        prompts <- readIORef childPrompts
        Text.intercalate "\n" prompts `shouldSatisfy`
            Text.isInfixOf "find the invoice"

loopConfig :: CancelFlag -> Backend -> BackendStateStore -> ToolRegistry -> LoopConfig
loopConfig cancel backend store tools = LoopConfig
    { loopBackend = backend
    , loopBackendState = store
    , loopTools = tools
    , loopReadTools = Nothing
    , loopDispatch = defaultLoopDispatch
    , loopMaxTurns = defaultLoopMaxTurns
    , loopOnEvent = \_ -> pure ()
    , loopApprove = \_ -> pure ToolApprovalGranted
    , loopReadSteering = pure []
    , loopCommitSteering = \_ -> pure ()
    , loopCloseSteering = pure []
    , loopInterrupt = pure ()
    , loopCancel = cancel
    }

memoryStore :: IORef BackendSnapshot -> BackendStateStore
memoryStore state = BackendStateStore
    { readBackendState = readIORef state
    , commitBackendState = \snapshot -> do
        writeIORef state snapshot
        pure snapshot
    }
